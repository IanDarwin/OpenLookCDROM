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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pcserver/RCS/pccsnap.c,v 2.6 1992/12/15 21:06:36 rr2b R6tape $";
#endif

/*
 *	PC Client - SNAP Interface Component
 *	Access to the VICE File System for IBM PC/XT/ATs
 *
 *	(C)Copyright IBM Corporation, 1984, 1985
 *	Program Property of IBM
 *
 *	Author - Larry K. Raper, 11/85
 *	Version 1.0
 *	Developed for the Information Technology Center at
 *	Carnegie-Mellon University
 *
 */

#include <snap.h>
#include <pcserver.h>

#define NULL	0

PRIVATE char OutBuff[LARGEST_MSG];
PRIVATE char InBuff[LARGEST_MSG];

int PCS_Open (cid, Pathname, Intent, Filter, Handle, File_mode, File_date,
	       File_time, File_size)
int cid;
char *Pathname;
SNAP_integer Intent, Filter;
SNAP_integer *Handle, *File_mode, *File_date, *File_time, *File_size;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_OPEN);
    op = SNAP_AppendStringToMsg (op, Pathname);
    op = SNAP_AppendIntToMsg	(op, (SNAP_integer) Intent);
    op = SNAP_AppendIntToMsg	(op, (SNAP_integer) Filter);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    ip = SNAP_ExtractIntFromMsg (ip, Handle);
    ip = SNAP_ExtractIntFromMsg (ip, File_mode);
    ip = SNAP_ExtractIntFromMsg (ip, File_date);
    ip = SNAP_ExtractIntFromMsg (ip, File_time);
    ip = SNAP_ExtractIntFromMsg (ip, File_size);
    return ((int) rc);
}

int PCS_Close (cid, Handle)
int cid;
SNAP_integer Handle;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_CLOSE);
    op = SNAP_AppendIntToMsg	(op, (SNAP_integer) Handle);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    return ((int) rc);
}

int PCS_Read (cid, Handle, Offset, Length, Return_length, Data)
int cid;
SNAP_integer Handle, Offset, Length;
SNAP_integer *Return_length;
char **Data;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) PCS_READ);
    op = SNAP_AppendIntToMsg (op, Handle);
    op = SNAP_AppendIntToMsg (op, Offset);
    op = SNAP_AppendIntToMsg (op, Length);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg   (InBuff, &rc);
    ip = SNAP_ExtractIntFromMsg   (ip, Return_length);
    ip = SNAP_ExtractBytesFromMsg (ip, Data);
    return ((int) rc);
}

int PCS_Write (cid, Handle, Offset, Length, Data, Return_length)
int cid;
SNAP_integer Handle, Offset, Length;
SNAP_integer *Return_length;
char *Data;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) PCS_WRITE);
    op = SNAP_AppendIntToMsg (op, Handle);
    op = SNAP_AppendIntToMsg (op, Offset);
    op = SNAP_AppendIntToMsg (op, Length);
    op = SNAP_AppendBytesToMsg (op, Data, (int) Length);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    ip = SNAP_ExtractIntFromMsg (ip, Return_length);
    return ((int) rc);
}

int PCS_DirSearch (cid, Generic_pathname, Filter, Starting_from,
		    First_match, File_mode, File_date, File_time, File_size)
int cid;
char *Generic_pathname;
SNAP_integer Filter;
char *Starting_from;
char **First_match;
SNAP_integer *File_mode, *File_date, *File_time, *File_size;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_DIRSEARCH);
    op = SNAP_AppendStringToMsg (op, Generic_pathname);
    op = SNAP_AppendIntToMsg	(op, Filter);
    op = SNAP_AppendStringToMsg (op, Starting_from);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg    (InBuff, &rc);
    ip = SNAP_ExtractStringFromMsg (ip, First_match);
    ip = SNAP_ExtractIntFromMsg    (ip, File_mode);
    ip = SNAP_ExtractIntFromMsg    (ip, File_date);
    ip = SNAP_ExtractIntFromMsg    (ip, File_time);
    ip = SNAP_ExtractIntFromMsg    (ip, File_size);
    return ((int) rc);
}

int PCS_RemoveFiles (cid, Generic_pathname)
int cid;
char *Generic_pathname;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_REMOVEFILES);
    op = SNAP_AppendStringToMsg (op, Generic_pathname);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    return ((int) rc);
}

int PCS_RenameFiles (cid, Old_generic_pathname, New_generic_pathname)
int cid;
char *Old_generic_pathname, *New_generic_pathname;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_RENAMEFILES);
    op = SNAP_AppendStringToMsg (op, Old_generic_pathname);
    op = SNAP_AppendStringToMsg (op, New_generic_pathname);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    return ((int) rc);
}

int PCS_MkDir (cid, Pathname)
int cid;
char *Pathname;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_MKDIR);
    op = SNAP_AppendStringToMsg (op, Pathname);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    return ((int) rc);
}

int PCS_RmDir (cid, Pathname)
int cid;
char *Pathname;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_RMDIR);
    op = SNAP_AppendStringToMsg (op, Pathname);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    return ((int) rc);
}

int PCS_ChMod (cid, Pathname, File_mode)
int cid;
char *Pathname;
SNAP_integer File_mode;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg	(OutBuff, (SNAP_integer) PCS_CHMOD);
    op = SNAP_AppendStringToMsg (op, Pathname);
    op = SNAP_AppendIntToMsg	(op, File_mode);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    return ((int) rc);
}

int PCS_SpaceQuery (cid, Total_AUs, Remaining_AUs,
		     Sectors_per_AU, Bytes_per_sector)
int cid;
SNAP_integer *Total_AUs, *Remaining_AUs, *Sectors_per_AU, *Bytes_per_sector;
{
    char *op, *ip;
    int  sc, oplen;
    SNAP_integer rc;
    op = SNAP_AppendIntToMsg (OutBuff, (SNAP_integer) PCS_SPACEQUERY);
    oplen = op - OutBuff;
    sc = SNAP_SendWithReply (cid, OutBuff, oplen, InBuff, sizeof(InBuff), NULL);
    if (sc < 0) return (sc);
    ip = SNAP_ExtractIntFromMsg (InBuff, &rc);
    ip = SNAP_ExtractIntFromMsg (ip, Total_AUs);
    ip = SNAP_ExtractIntFromMsg (ip, Remaining_AUs);
    ip = SNAP_ExtractIntFromMsg (ip, Sectors_per_AU);
    ip = SNAP_ExtractIntFromMsg (ip, Bytes_per_sector);
    return ((int) rc);
}
