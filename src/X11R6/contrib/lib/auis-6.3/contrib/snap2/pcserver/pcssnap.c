/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pcserver/RCS/pcssnap.c,v 2.13 1992/12/15 21:06:36 rr2b R6tape $";
#endif

/*
 *      PC Server - SNAP Interface Component
 *      Access to the UNIX File System for IBM PC/XT/ATs
 *
 *      (C)Copyright IBM Corporation, 1985, 1986, 1987
 *      Program Property of IBM
 *
 *      Author - Larry K. Raper
 *      Version 2.5
 *      Developed for the Information Technology Center at
 *      Carnegie-Mellon University
 *
 *      Release history
 *      1.0 11/85 Initial release
 *      1.1 05/86 Misc fix package
 *      2.0 08/86 Modified to work with SNAP 2.0
 *      2.1 08/86 PCS_EXECUTE call added
 *      2.2 10/86 PCS_GETHOMEDIR call added
 *      2.3 03/87 Terminate function for VICE-specific pag invalidation
 *      2.4 03/87 Changed timeout interval to 8 hours
 *      2.5 09/87 Updated copyright notice
 *
 */

#include <andrewos.h>
#include <stdio.h>
#include <gasp.h>
#include <snap.h>
#include <pcserver.h>
#include <sys/types.h>
#include <sys/file.h>

int PCS_debuglevel;

long time();
#define TWELVEHOURS (12*60*60)
#define EIGHTHOURS   (8*60*60)

#ifdef DEBUG
PRIVATE char *ShowIntent (arg)
int arg;
{
    if (arg == PCS_INPUT)
	return "PCS_INPUT";
    if (arg == PCS_OUTPUT)
	return "PCS_OUTPUT";
    if (arg == PCS_UNSPECIFIED)
	return "PCS_UNSPECIFIED";
    return "Unknown";
}

PRIVATE char *ShowFilter (arg)
int arg;
{
    if (arg == PCS_NORMAL)
	return "PCS_NORMAL";
    if (arg == PCS_DIRECTORY)
	return "PCS_DIRECTORY";
    if (arg == PCS_HIDDEN)
	return "PCS_HIDDEN";
    if (arg == PCS_READONLY)
	return "PCS_READONLY";
    if (arg == (PCS_NORMAL+PCS_HIDDEN))
	return "PCS_NORMAL+PCS_HIDDEN";
    if (arg == (PCS_NORMAL+PCS_DIRECTORY))
	return "PCS_NORMAL+PCS_DIRECTORY";
    return "Unknown";
}

PRIVATE char *ShowRC(rc)
int rc;
{
    static char *codes[] = {"PCS_SUCCESS", "PCS_NOFILE", "PCS_NOPATH",
    "PCS_NOSPACE", "PCS_NOACCESS", "PCS_BADFILT",
    "PCS_BADINTEN", "PCS_NOHANDLE", "PCS_EOF",
    "PCS_NOMATCH", "PCS_NOTEMPTY", "PCS_BADMODE",
    "PCS_BADARG", "PCS_EXECFAIL"};
#define NCODES ((sizeof codes) / (sizeof (char *)))
    if ((rc >= NCODES) || (rc<0)) {
	static char line[50];
	sprintf(line, "Unknown error (%d)", rc);
	return line;
    }
    return codes[rc];
}

#endif /* DEBUG */

static long    quittime;
static long    maxquit;

#define DID_NOTHING_TIMEOUT (60)
#define DID_SOMETHING_TIMEOUT (61)
static int exit_timeout_code=DID_NOTHING_TIMEOUT;

main (argc, argv)
int argc;
char *argv[];
{
    SNAP_CPARMS connparms;
    char    *client;
    int     rc, auth, cid, msgtype, msglen, clientfd, scount, newscount;
    char    *InBuff;

    if (access("/debug.pcserver", F_OK)) /*print debugging info?*/
	PCS_debuglevel = 0;       /*no*/
    else
	PCS_debuglevel = 99;      /*yes*/
    quittime = time(NULL) + 20L*60L; /*initial quittime*/
    maxquit = time(NULL) + EIGHTHOURS;	/*max time to live*/
    scount = 0;

    connparms.maxtime = 20;
    connparms.timeout = 1;
    connparms.maxmsgsize  = LARGEST_MSG;
    connparms.encryptlevel = SNAP_NOENCRYPT;

    if ((rc = GASP_ServerInit (argc, argv, &connparms,
				&client, &clientfd, &auth)) != 0) {
	printf ("Could not initialize - rc: %d, terminating.\n",rc);
	Terminate (-1);
    }

    DBGMSG (3,("Back from Gasp_ServerInit  - client: %s",client));

    msgtype = 0; msglen = 1;
    while (1) {
	DBGMSG (3,(" "));
	msglen = SNAP_Accept (&InBuff, &msgtype, &cid, (4*60));
	DBGMSG (3,("Back from Accept - length: %d", msglen));
	CleanUpOrphans ();
	if (msglen < 0) {
	    if (time(NULL) > quittime) {
		DBGMSG (3,("Auto logoff due to timeout"));
		Terminate (exit_timeout_code);
	    }
	    GASP_Count (scount);
	    continue;
	}
	DBGMSG (3,("Msgtype: %d cid: %d", msgtype, cid));
	if (msgtype != SNAP_ENDCONV) {
	    newscount = SNAP_ConvCount ();
	    if (scount != newscount) {
		scount = newscount;
		GASP_Count (scount);
	    }
	    perform (InBuff, msglen, msgtype, cid);
	}
	else {
	    scount = SNAP_ConvCount ();
	    GASP_Count (scount);
	    if (scount == 0) {
		DBGMSG (3,("Terminating"));
		Terminate (0);
	    }
	}
    }
}

/*
  temporary check for null pointers sent by pcvenus
      */
char *PCS_ExtractStringFromMsg(ip,dst)
char *ip;
char **dst;
{register char *op=SNAP_ExtractStringFromMsg(ip,dst);
if((*dst)==0) {
    *dst = "";
    fprintf(stderr,"pcserver extacted bogus string\n");
    fflush(stderr);
}
return op;
}

PRIVATE perform (InBuff, msglen, msgtype, cid)
char *InBuff;
int msglen, msgtype, cid;
{
    char OutBuff[LARGEST_MSG];
    char *ip, *op;
    SNAP_integer opcode;
    int  rc, oplen;

    ip = SNAP_ExtractIntFromMsg (InBuff, &opcode);
    DBGMSG (3,("Msg length: %d, opcode: %d",msglen,(int) opcode));
    if (msglen == 0) {
	opcode = 0;
	oplen = 0;
    }

    /*if not just logging in use long timeout???*/
    if(opcode!=PCS_GETHOMEDIR) {
	quittime = maxquit;	/*possibly extend initial timeout*/
	exit_timeout_code=DID_SOMETHING_TIMEOUT;
    }

    switch ((int) opcode) {

	case PCS_OPEN: {
	    char *Pathname;
	    SNAP_integer Intent, Filter;
	    int  Handle, File_mode, File_date, File_time, File_size;
	    ip = PCS_ExtractStringFromMsg (ip, &Pathname);
	    ip = SNAP_ExtractIntFromMsg    (ip, &Intent);
	    ip = SNAP_ExtractIntFromMsg    (ip, &Filter);

	    DBGMSG (3,("Open - Pathname: \"%s\"",Pathname));
	    DBGMSG (3,("Open - Intent: %s, Filter: %s",
		       ShowIntent((int) Intent),ShowFilter((int) Filter)));

	    rc = PCS_Open (Pathname, (int) Intent, (int) Filter,
			   &Handle,&File_mode,&File_date,&File_time,&File_size);

	    DBGMSG (3,("Open - RC: %s, Handle: %d",ShowRC(rc),Handle));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) Handle);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) File_mode);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) File_date);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) File_time);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) File_size);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_CLOSE: {
	    SNAP_integer Handle;
	    ip = SNAP_ExtractIntFromMsg (ip, &Handle);

	    DBGMSG (3,("Close - Handle: %d", (int) Handle));

	    rc = PCS_Close ((int) Handle);

	    DBGMSG (3,("Close - RC: %s",ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_READ: {
	    SNAP_integer Handle, Offset, Length;
	    int  Return_length;
	    char *Data;
	    ip = SNAP_ExtractIntFromMsg (ip, &Handle);
	    ip = SNAP_ExtractIntFromMsg (ip, &Offset);
	    ip = SNAP_ExtractIntFromMsg (ip, &Length);

	    DBGMSG (3,("Read - Handle: %d, Offset: %d, Length: %d",
		       (int) Handle, (int) Offset, (int) Length));

	    rc = PCS_Read ((int) Handle, (int) Offset, (int) Length,
			   &Return_length, &Data);

	    DBGMSG (3,("Read - RC: %s, Return_length: %d",
		       ShowRC(rc),Return_length));

	    op = SNAP_AppendIntToMsg   (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendIntToMsg   (op, (SNAP_integer) Return_length);
	    op = SNAP_AppendBytesToMsg (op, Data, (int) Return_length);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_WRITE: {
	    SNAP_integer Handle, Offset, Length;
	    char *Data;
	    int  Return_length;
	    ip = SNAP_ExtractIntFromMsg   (ip, &Handle);
	    ip = SNAP_ExtractIntFromMsg   (ip, &Offset);
	    ip = SNAP_ExtractIntFromMsg   (ip, &Length);
	    ip = SNAP_ExtractBytesFromMsg (ip, &Data);

	    DBGMSG (3,("Write - Handle: %d, Offset: %d, Length: %d",
		       (int) Handle, (int) Offset, (int) Length));

	    rc = PCS_Write ((int) Handle, (int) Offset, (int) Length, Data,
			    &Return_length);

	    DBGMSG (3,("Write - RC: %s, Return_length: %d",
		       ShowRC(rc),Return_length));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) Return_length);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_DIRSEARCH: {
	    char *Generic_pathname, *Starting_from;
	    SNAP_integer Filter;
	    char *First_match;
	    int  File_mode, File_date, File_time, File_size;
	    ip = PCS_ExtractStringFromMsg (ip, &Generic_pathname);
	    ip = SNAP_ExtractIntFromMsg    (ip, &Filter);
	    ip = PCS_ExtractStringFromMsg (ip, &Starting_from);

	    DBGMSG (3,("DirSearch - Generic_pathname: \"%s\"",
		       Generic_pathname));
	    DBGMSG (3,("DirSearch - Filter: %s, Starting_from: \"%s\"",
		       ShowFilter((int) Filter),Starting_from));

	    rc = PCS_DirSearch (Generic_pathname, (int) Filter, Starting_from,
				&First_match, &File_mode, &File_date, &File_time, &File_size);

	    DBGMSG (3,("DirSearch - RC: %s, First_match: \"%s\"",
		       ShowRC(rc),First_match));

	    op = SNAP_AppendIntToMsg    (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendStringToMsg (op, First_match);
	    op = SNAP_AppendIntToMsg    (op, (SNAP_integer) File_mode);
	    op = SNAP_AppendIntToMsg    (op, (SNAP_integer) File_date);
	    op = SNAP_AppendIntToMsg    (op, (SNAP_integer) File_time);
	    op = SNAP_AppendIntToMsg    (op, (SNAP_integer) File_size);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_REMOVEFILES: {
	    char *Generic_pathname;
	    ip = PCS_ExtractStringFromMsg (ip, &Generic_pathname);

	    DBGMSG (3,("RemoveFiles - Generic_pathname: \"%s\"",
		       Generic_pathname));

	    rc = PCS_RemoveFiles (Generic_pathname);

	    DBGMSG (3,("RemoveFiles - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_RENAMEFILES: {
	    char *Old_generic_pathname, *New_generic_pathname;
	    ip = PCS_ExtractStringFromMsg (ip, &Old_generic_pathname);
	    ip = PCS_ExtractStringFromMsg (ip, &New_generic_pathname);

	    DBGMSG (3,("RenameFiles - New_generic_pathname: \"%s\"",
		       New_generic_pathname));
	    DBGMSG (3,("RenameFiles - Old_generic_pathname: \"%s\"",
		       Old_generic_pathname));

	    rc = PCS_RenameFiles (Old_generic_pathname, New_generic_pathname);

	    DBGMSG (3,("RenameFiles - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_MKDIR: {
	    char *Pathname;
	    ip = PCS_ExtractStringFromMsg (ip, &Pathname);

	    DBGMSG (3,("MkDir - Pathname: \"%s\"",Pathname));

	    rc = PCS_MkDir (Pathname);

	    DBGMSG (3,("MkDir - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_RMDIR: {
	    char *Pathname;
	    ip = PCS_ExtractStringFromMsg (ip, &Pathname);

	    DBGMSG (3,("RmDir - Pathname: \"%s\"",Pathname));

	    rc = PCS_RmDir (Pathname);

	    DBGMSG (3,("RmDir - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_CHMOD: {
	    char *Pathname;
	    SNAP_integer File_mode;
	    ip = PCS_ExtractStringFromMsg (ip, &Pathname);
	    ip = SNAP_ExtractIntFromMsg    (ip, &File_mode);

	    DBGMSG (3,("ChMod - Pathname: \"%s\", File_mode: %s",
		       Pathname, ShowFilter((int) File_mode)));

	    rc = PCS_ChMod (Pathname, (int) File_mode);

	    DBGMSG (3,("ChMod - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_SPACEQUERY: {
	    int  Total_AUs, Remaining_AUs, Sectors_per_AU, Bytes_per_sector;
	    rc = PCS_SpaceQuery (&Total_AUs, &Remaining_AUs, &Sectors_per_AU, &Bytes_per_sector);

	    DBGMSG (3,("SpaceQuery - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) Total_AUs);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) Remaining_AUs);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) Sectors_per_AU);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) Bytes_per_sector);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_TIMEOFDAY: {
	    int pcdate, pctime;
	    rc = PCS_TimeOfDay (&pcdate, &pctime);

	    DBGMSG (3,("TimeOfDay - date: %d, time: %d",pcdate, pctime));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) pcdate);
	    op = SNAP_AppendIntToMsg (op, (SNAP_integer) pctime);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_FORCETERM: {
	    DBGMSG (3,("Forcibly terminating"));
	    GASP_Count (0);
	    Terminate (0);
	    }

	case PCS_TIMESTAMP: {
	    SNAP_integer Handle, DosDate, DosTime;

	    ip = SNAP_ExtractIntFromMsg   (ip, &Handle);
	    ip = SNAP_ExtractIntFromMsg   (ip, &DosDate);
	    ip = SNAP_ExtractIntFromMsg   (ip, &DosTime);

	    DBGMSG (3,("TimeStamp - Handle: %d, DosDate: %x, DosTime: %x",
		       (int) Handle, (int) DosDate, (int) DosTime));

	    rc = PCS_TimeStamp ((int) Handle, (int) DosDate, (int) DosTime);

	    DBGMSG (3,("TimeStamp - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_EXECUTE: {
	    char *command;
	    char output[OUTPUT_LIMIT+1];
	    SNAP_integer waitopt;

	    ip = SNAP_ExtractIntFromMsg    (ip, &waitopt);
	    ip = PCS_ExtractStringFromMsg (ip, &command);

	    DBGMSG (3,("Execute - command: \"%s\"", command));
	    DBGMSG (3,("Execute - wait: %s", (waitopt ? "yes" : "no")));

	    rc = PCS_Execute (command, output, waitopt);

	    DBGMSG (3,("Execute - RC: %s", ShowRC(rc)));

	    op = SNAP_AppendIntToMsg    (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendStringToMsg (op, output);
	    oplen = op - OutBuff;
	    break;
	    }

	case PCS_GETHOMEDIR: {
	    char *relative_dir;
	    char *absolute_dir;

	    ip = PCS_ExtractStringFromMsg (ip, &relative_dir);

	    DBGMSG (3,("GetHomeDir - dir relative to home: \"%s\"",
		       relative_dir));

	    rc = PCS_GetHomeDir (relative_dir, &absolute_dir);

	    DBGMSG (3,("GetHomeDir - RC: %s, output: \"%s\"",
		       ShowRC(rc), absolute_dir));

	    op = SNAP_AppendIntToMsg    (OutBuff, (SNAP_integer) rc);
	    op = SNAP_AppendStringToMsg (op, absolute_dir);
	    oplen = op - OutBuff;
	    break;
	    }

	default: {
	    DBGMSG (2,("Unrecognized opcode: %d",(int) opcode));
	    rc = -1;
	    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) rc);
	    oplen = op - OutBuff;
	    }
    }
    if (msgtype == SNAP_SENDWITHREPLY)
	SNAP_Reply (OutBuff, oplen, cid);
}
