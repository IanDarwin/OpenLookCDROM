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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/snap2/RCS/snapguar.c,v 2.12 1993/05/04 01:49:46 susan Exp $";
#endif

#define   SNAPSCRAM

#include <stdio.h>
#include <andrewos.h> /* sys/types.h sys/time.h */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>


#include <snap.h>
#include <snapcom.h>

#define MAXBUF		    (SNAP_HDRLENGTH+FRAG_HDRLENGTH+GUARDIAN_MAXFRAG)
#define MAXNOTIFY	    (MAXBUF*3)
#define FIXED_NOTIFYLENGTH  (SNAP_HDRLENGTH+FRAG_HDRLENGTH+sizeof(NETADDR)\
			    +SNAP_KEYLEN+7*sizeof(SNAP_integer))

long time();


/*
 *  (Null) Handler for notify packets
 *
 */

PRIVATE int NotifyHandler ()
{
    return SNAP_SUCCESS;
}

/*
  * Initialize the SNAP Guardian support layer
  *
  */

int SNAP_istcp()
{
  return FALSE;
}

int SNAP_GuardianInit ()
{
    DBGMSG (SUT,("GuardianInit"));
    return (SNAP_SUCCESS);
}

/*
  * Terminate the SNAP Guardian support layer
  *
  */

int SNAP_GuardianTerm ()
{
    DBGMSG (SUT,("GuardianTerm"));
    return (SNAP_SUCCESS);
}

/*
  * Handle a "Get authentication request" for the Guardian
      *
      */

int SNAP_GetAuthReq (socket, connparms, authenticator)
int socket;
SNAP_CPARMS *connparms;
int (*authenticator)();
{
    int recvlen, rc, msglen;
    SNAP_integer au_op, au_seqno, au_conv, plen, ptype;
    char *userid, *password, *server, *p, *mp;
    char sessionkey[SNAP_KEYLEN], fragstatus;
    char aubuf[MAXBUF], aureply[MAXBUF];
    char xmitbuf[MAXBUF], hdr[SNAP_HDRLENGTH];
    NETADDR source_na, server_na;
    long quittime;
    char errbuf[1000];

    DBGMSG (SUT+RCV,("GetAuthReq - socket: %d",socket));
    DBGMSG (SUT+RCV,("GetAuthReq - maxtime: %d, timeout: %d, encryptlevel: %d",
		      connparms->maxtime, connparms->timeout, connparms->encryptlevel));

    if ((connparms == NULL)	||
	 (connparms->maxtime <0) ||		/* Validate parameters */
	 (connparms->timeout <0) ||
	 (connparms->maxtime < connparms->timeout) ||
	 ((connparms->encryptlevel != SNAP_NOENCRYPT) &&
	  (connparms->encryptlevel != SNAP_ENCRYPT)) ||
	 (socket < 0)) {
	DBGMSG (SUT+RCV,("GetAuthReq - parm validation failure"));
	return (SNAP_BADPARMS);
    }

    while (TRUE) {

	quittime = time(NULL) + connparms->maxtime;

	if ((recvlen = SNAP_CommonReceive (socket, connparms->timeout, quittime,
					   (char *) NULL, aubuf, sizeof(aubuf), &source_na)) < 0) {
	    DBGMSG (SUT+RCV,("GetAuthReq - receive error: %d",recvlen));
	    return (recvlen);
	}

	DBGMSG (SUT+RCV,("GetAuthReq - successful receive"));

	/*
	 * Now we will examine the SNAP_AUTHENTICATE message.
	 * It has the following format:
	 *
	 *	SNAP_integer	checksum; <-- standard header
	   *	SNAP_integer	conv;	  <-- standard header
	     *	SNAP_integer	opcode;   <-- standard header
	       *	SNAP_integer	seqno;	  <-- standard header
		 *	SNAP_integer	fragword; <-- frag header
		   *	SNAP_integer	datalen;  <-- frag header
		     *	SNAP_string	servername;
		   *	SNAP_string	client_userid;
		   *	SNAP_bytestring client_password;
		   *	SNAP_integer	password_type;
		   */

	p = SNAP_ExtractIntFromMsg (aubuf+sizeof(SNAP_integer), &au_conv);
	DBGMSG (SUT+RCV,("GetAuthReq - conv is %ld",au_conv));

	p = SNAP_ExtractIntFromMsg (p, &au_op);
	DBGMSG (SUT+RCV,("GetAuthReq - received opcode is %ld",au_op));

	p = SNAP_ExtractIntFromMsg (p, &au_seqno);
	DBGMSG (SUT+RCV,("GetAuthReq - seqno is %ld",au_seqno));

	if (au_op != SNAP_AUTHENTICATE)
	    continue;

	DBGMSG (SUT+RCV,("GetAuthReq - msg len: %d",recvlen));

	break;
    }

    p = SNAP_ExtractStringFromMsg (p+(2*sizeof(SNAP_integer)), &server);
    p = SNAP_ExtractStringFromMsg (p, &userid);
    SNAP_ExtractIntFromMsg	  (p, &plen);
    p = SNAP_ExtractBytesFromMsg  (p, &password);
    scramble (password, (int) plen);
    SNAP_ExtractIntFromMsg	  (p, &ptype);

    if (plen == 0)
	password = "";

    DBGMSG (SUT+RCV,("GetAuthReq - server: %s, userid: %s",server,userid));
    DBGMSG (SUT+RCV,("GetAuthReq - ptype: %ld, plen: %ld", ptype, plen));

    rc = (*authenticator) (userid, password, (int) plen, (int) ptype,
			    server, sessionkey, &server_na, &source_na,
			   errbuf,"oldsnap",0);
    DBGMSG (SUT+RCV,("GetAuthReq - rc from authenticator is %d",rc));

    /*
      * Next we construct the SNAP_AUTHENTICATE reply.
      * It's body portion has the following format:
*
*	    SNAP_integer    returncode;
*	    SNAP_bytestring server_na;
*	    SNAP_bytestring sessionkey;
*/

    SNAP_BuildStandardHdr (hdr, SNAP_REPLY, (int) au_conv, au_seqno);
    mp = SNAP_AppendIntToMsg   (aureply, (SNAP_integer) rc);
    SNAP_NetaddrToNBO     (&server_na);
    mp = SNAP_AppendBytesToMsg (mp, &server_na, sizeof(NETADDR));
    mp = SNAP_AppendBytesToMsg (mp, sessionkey, SNAP_KEYLEN);
    msglen = mp - aureply;

    quittime = time(NULL) + connparms->maxtime;
    if ((rc = SNAP_SendMsg (socket, (FARCHAR *) aureply, msglen, &source_na,
			     connparms->timeout, quittime, SNAP_NOENCRYPT, sessionkey,
			     hdr, xmitbuf, sizeof (xmitbuf), 1, GUARDIAN_MAXFRAG, 1,
			     &fragstatus, FALSE, FALSE, NotifyHandler))
	 < 0) {
	DBGMSG (SUT+RCV,("GetAuthReq - unable to reply to client: %d",rc));
	rc = SNAP_XMITFAIL;
    }
    else {
	DBGMSG (SND,("GetAuthReq - reply sent to client, last frag: %d", rc));
	rc = SNAP_SUCCESS;
    }
    return (rc);
}
