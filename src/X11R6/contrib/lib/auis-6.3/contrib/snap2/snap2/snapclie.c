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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/snap2/RCS/snapclie.c,v 2.11 1994/03/31 06:02:03 rr2b Exp $";
#endif

#define SNAPSCRAM

#include <andrewos.h> /* sys/types.h sys/time.h */
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>


#include <snap.h>
#include <snapcom.h>

#define INADDR		    struct in_addr
#define BUFFER		    struct buffer
#define SET		    1
#define RESET		    0

#define MAXCONVS	    10
#define PointerFromCID(arg) (SNAP_anchor.ccbp[arg])

struct	ccb {			    /* Conversation Control Block   */
    NETADDR server_na;		    /* Network address of server    */
    NETADDR guardian_na;	    /* Network address of guardian  */
    int     serverdead; 	    /* TRUE if server has died	    */
    int     maxtime;		    /* Max elapsed time for request */
    int     timeout;		    /* Timeout interval 	    */
    int     encryptlevel;	    /* Type of encryption	    */
    int     maxfrag;		    /* Max data portion of fragment */
    int     serverwindow;	    /* frags/window for server	    */
    int     servminorversion;	    /* Server's minor version num   */
    char    sessionkey[SNAP_KEYLEN];/* Session key		    */
    char    rstatus[CLIENT_WINDOW]; /* Receive window status	    */
    char    sstatus[MAXWINDOW];     /* Send window status	    */
    SNAP_integer lastseqno;	    /* Last seqno sent with message */
    int     convid;		    /* Conversation id for session  */
};

PRIVATE struct {
    struct ccb *ccbp[MAXCONVS+1];   /* last element always NULL */
    int initflag;
} SNAP_anchor;

PRIVATE int SNAP_socket = -1;
PRIVATE int bufsize = 0;
PRIVATE char *xmitbuf = NULL;
PRIVATE long quittime;

#ifndef _IBMR2
char *malloc();
#endif /* _IBMR2 */
long time();


/*
  *  SetBuffer makes certain that the transmit buffer is big enough
  *  to hold the requested number of bytes.
  *
  */

PRIVATE int SetBuffer (size)
int size;
{
    int oldsize;

    size += SNAP_HDRLENGTH + FRAG_HDRLENGTH;
    if (size > bufsize) {
	if (xmitbuf != NULL)
	    free (xmitbuf);
	oldsize = bufsize;
	if ((xmitbuf = malloc (size)) != NULL) {
	    bufsize = size;
	    return (SNAP_SUCCESS);
	}
	if ((xmitbuf = malloc (oldsize)) != NULL) {
	    bufsize = oldsize;
	    return (SNAP_NOMEMORY);
	}

	/* This condition should not occur! */

	return (SNAP_NOMEMORY);
    }
    return (SNAP_SUCCESS);
}


/*
  *  Release the storage allocated for a CCB
      *  [Conversation Control Block]
      *
      */

PRIVATE ReleaseCCB (cid)
int cid;
{
    struct ccb *cp;
    cp = PointerFromCID(cid);
    if (cp != NULL) {
	DBGMSG (CTL,("Freeing CCB at %s, cid = %d", PTR(cp), cid));
	free (cp);
	SNAP_anchor.ccbp[cid] = NULL;
    }
}


/*
  *  This routine handles notify packets
  *
  */


PRIVATE int NotifyHandler (packet, curconv, source_na)
char *packet;
int curconv;
NETADDR *source_na;
{
    SNAP_integer type;
    NETADDR *server_na;
    char *p;
    int i;
    struct ccb *cp;

    p = SNAP_ExtractIntFromMsg (packet+SNAP_HDRLENGTH+FRAG_HDRLENGTH, &type);
    DBGMSG (RCV,("NotifyHandler - type: %ld", type));

    if (type == SNAP_SERVERGONE) {
	SNAP_ExtractBytesFromMsg (p, &server_na);
	SNAP_NetaddrFromNBO (server_na);
	for (i=0; ((cp = PointerFromCID(i)) != NULL); i++) {
	    if ((bcmp (&cp->guardian_na, source_na, sizeof(NETADDR)) == 0) &&
		(bcmp (&cp->server_na, server_na, sizeof(NETADDR)) == 0))
		cp->serverdead = TRUE;
	}
	cp = PointerFromCID (curconv);
	if (cp->serverdead)
	    return (SNAP_SERVERDIED);
	else
	    return (SNAP_SUCCESS);
    }

    /* Other types (like BROADCAST) not implemented yet */

    return (SNAP_SUCCESS);
}


/*
  *  Send a message and wait for a reply message.
      *  Reply message is placed directly into the user's reply buffer.
				     *  Returns the length of the body of the reply message.
				     *
				     */

PRIVATE SendReceive (cp, msg, msglen, msghdr, reply, rdatalen)
struct	ccb *cp;
FARCHAR *msg;
FARCHAR *reply;
char *msghdr;
int msglen, rdatalen;
{
    int replylen, f1, forceack;

    DBGMSG (SND+RCV,("SendReceive - CCB at %s, msglen: %d", PTR(cp), msglen));

    f1 = 0;
    replylen = 0;
    forceack = FALSE;

    while ((replylen != SNAP_TIMEOUT) && (replylen != SNAP_SERVERDIED)) {

	if ((f1 = SNAP_SendMsg (SNAP_socket, msg, msglen, &cp->server_na,
				cp->timeout, quittime, cp->encryptlevel, cp->sessionkey,
				msghdr, xmitbuf, bufsize, f1+1, cp->maxfrag, cp->serverwindow,
				cp->sstatus, forceack, FALSE, NotifyHandler))
	    < 0) {
	    DBGMSG (SND,("SendReceive - error from SNAP_SendMsg: %d",f1));
	    cp->lastseqno++;
	    return (f1);
	}

	DBGMSG (SND,("SendReceive - successful send"));

	replylen = SNAP_ReceiveMsg (SNAP_socket, cp->timeout, quittime,
				    reply, rdatalen, cp->sessionkey, cp->encryptlevel, cp->rstatus,
				    CLIENT_WINDOW, xmitbuf, bufsize, cp->maxfrag,
				    &cp->server_na, cp->convid, SNAP_REPLY, cp->lastseqno,
				    NotifyHandler);

	DBGMSG (RCV,("SendReceive - result from Receive was %d", replylen));

	if (replylen >= 0) {
	    cp->lastseqno++;
	    return (replylen);
	}

	forceack = TRUE;
    }

    if (replylen == SNAP_TIMEOUT) {
	DBGMSG (SND+RCV,("SendReceive - max time (%d secs) exceeded",
			 cp->maxtime));
	cp->lastseqno++;
    }
    if (replylen == SNAP_SERVERDIED)
	DBGMSG (SND+RCV,("SendReceive - server died"));

    return (replylen);
}


/*
  *  Swap session parameters
  *
  */

PRIVATE SwitchParms (cp, msgparms)
struct ccb *cp;
SNAP_CPARMS *msgparms;
{
    register int temp;

    temp = cp->maxtime;
    cp->maxtime = msgparms->maxtime;
    msgparms->maxtime = temp;
    temp = cp->timeout;
    cp->timeout = msgparms->timeout;
    msgparms->timeout = temp;
    temp = cp->encryptlevel;
    cp->encryptlevel = msgparms->encryptlevel;
    msgparms->encryptlevel = temp;
}


/*
  *  OverrideSessionParms will set or reset session parameters based
  *  on new values supplied for a particular SNAP call.
      *  (The reset operation restores the prior session parameter values);
  *
*/

PRIVATE int OverrideSessionParms (action, cp, msgparms)
int action;
struct ccb *cp;
SNAP_CPARMS *msgparms;
{
    if (msgparms == NULL)
	return (SNAP_SUCCESS);

    DBGMSG (CTL,("OverrideSessionParms"));
    switch (action) {
	case SET:
	    DBGMSG (CTL,
		    ("New values - maxtime: %d, timeout: %d, encrypt: %d",
		     msgparms->maxtime, msgparms->timeout,
		     msgparms->encryptlevel));
	    if ((msgparms->maxtime < 0) ||
		(msgparms->timeout < 0) ||
		(msgparms->maxtime < msgparms->timeout) ||
		((msgparms->encryptlevel != SNAP_NOENCRYPT) &&
		 (msgparms->encryptlevel != SNAP_ENCRYPT))) {
		DBGMSG (CTL,("OverrideSessionParms - parm reject"));
		return (SNAP_BADPARMS);
	    }
	    SwitchParms (cp, msgparms);
	    DBGMSG (CTL,
		    ("Old values - maxtime: %d, timeout: %d, encrypt: %d",
		     msgparms->maxtime, msgparms->timeout,
		     msgparms->encryptlevel));
	    break;
	case RESET:
	    DBGMSG (CTL,("Restoring session parms"));
	    SwitchParms (cp, msgparms);
	    DBGMSG (CTL,
		    ("Old values - maxtime: %d, timeout: %d, encrypt: %d",
		     cp->maxtime, cp->timeout, cp->encryptlevel));
    }
    return (SNAP_SUCCESS);
}


/*
  *  Initialize SNAP Client Services package
  *
  */

int SNAP_ClientInit ()
{
    DBGMSG (SUT,("ClientInit"));

    if (SNAP_socket == -1) {			/* Create a socket */
	SNAP_socket = socket (AF_INET, SOCK_DGRAM, PF_UNSPEC);
	DBGMSG (SUT,("ClientInit - socket handle is %d",SNAP_socket));
    }

    if (SNAP_socket == -1)
	return SNAP_SOCKETERROR;
    else {
	SNAP_anchor.initflag = YES;
	return SNAP_SUCCESS;
    }
}


/*
  *  Terminate the SNAPs use of it's socket
      *
      * On unix a socket is closed with close() but on the mac the library
	* that supplys socket() is different from the the runtime system close()
	* for files.  So there is the CLOSE_A_SOCKET macro which is only defined on
	    * the mac
	    */
#ifndef CLOSE_A_SOCKET
#define CLOSE_A_SOCKET close
#endif /* CLOSE_A_SOCKET */
void SNAP_SocketTerm()
{
    if(SNAP_socket != -1) {
	CLOSE_A_SOCKET(SNAP_socket);
	SNAP_socket= -1;
    }
}


/*
  *  Terminate the SNAP Client Services support
  *
  */

int SNAP_ClientTerm()
{
    int i;

    DBGMSG (SUT,("ClientTerm"));

    for (i=0; i<MAXCONVS; i++)
	if ((PointerFromCID(i)) != NULL) {
	    SNAP_EndConv (i, (FARCHAR *) NULL, 0, (SNAP_CPARMS *) NULL);
	    ReleaseCCB (i);
	}
    if (xmitbuf != NULL) {
	DBGMSG (SUT+CTL,("Freeing transmit buffer at %s", PTR(xmitbuf)));
	free (xmitbuf);
	bufsize = 0;
    }
    SNAP_anchor.initflag = NO;
    return SNAP_SUCCESS;
}


/*
  *  Returns the SNAP version numbers for the client side
      *
      */

int SNAP_ClientVersion (cid, versions)
int cid, versions[4];
{
    int rc;
    struct ccb *cp;

    DBGMSG (CTL,("ClientVersion %d.%d", MAJOR_VERSION, MINOR_VERSION));
    versions[0] = MAJOR_VERSION;
    versions[1] = MINOR_VERSION;
    versions[2] = -1;
    versions[3] = -1;

    if (cid > MAXCONVS || cid < 0) {
	DBGMSG (CTL,("ClientVersion - invalid cid: %d", cid));
	rc = SNAP_INVALIDCID;
    }
    else {
	cp = PointerFromCID (cid);
	versions[2] = MAJOR_VERSION;
	versions[3] = cp->servminorversion;
	rc = SNAP_SUCCESS;
    }

    return (rc);
}


/*
  *  SetConvParms installs new session parameters that will
  *  remain in effect until a subsequent SetConvParms call.
  *  The prior connection parameters are returned in the
  *  passed CPARMS structure.
  *
  */

int SNAP_SetConvParms (cid, connparms)
int cid;
SNAP_CPARMS *connparms;
{
    struct  ccb *cp;
    if (cid > MAXCONVS || cid < 0) {
	DBGMSG (SNR,("SetConvParms - invalid cid: %d", cid));
	return (SNAP_INVALIDCID);
    }

    cp = PointerFromCID (cid);

    return (OverrideSessionParms (SET, cp, connparms));
}


/*
  *  Begin a conversation with a server
  *  Returns an integer "conversation id" or (if < 0) an error code
  *
  */

int SNAP_BeginConv (guardian, host, server, userid, password, plen, ptype,
		     connparms)
char	*guardian, *server, *host, *userid, *password;
int	plen, ptype;
SNAP_CPARMS *connparms;
{
    int cid, i, msglen, replylen;
    struct  ccb *cp;
    struct  hostent *hp;
    struct  servent *sp;
    char    *rp, *saptr, *keyptr, *mp, *pp;
    char    bcmsg[GUARDIAN_MAXFRAG];
    char    reply[100];
    char    msghdr[SNAP_HDRLENGTH];
    SNAP_integer    temp, rc;

    DBGMSG (BEG,("BeginConv - guardian: \"%s\", host: \"%s\"",guardian,host));
    DBGMSG (BEG,("BeginConv - server: \"%s\", userid: \"%s\"",server,userid));
    DBGMSG (BEG,("BeginConv - password at: %s length: %d, type: %d",
		  PTR(password), plen, ptype));
    DBGMSG (BEG,("BeginConv - maxtime: %d, reserved: %d, timeout: %d",
		  connparms->maxtime, connparms->maxmsgsize, connparms->timeout));

    if (SNAP_anchor.initflag == NO) {
	DBGMSG (BEG,("BeginConv - ClientInit has not been performed"));
	return (SNAP_NOINIT);
    }

    for (cid=0; PointerFromCID(cid) != NULL; cid++);	/* Allocate new  */
    if (cid == MAXCONVS) {				/* connection id */
	DBGMSG (BEG,("BeginConv - max conversations exceeded"));
	return (SNAP_NOMORECONVS);
    }

    if ((PointerFromCID(cid) =
	  (struct ccb *)malloc (sizeof (struct ccb))) == NULL) {
	DBGMSG (BEG+CTL,("BeginConv - couldn't allocate CCB"));
	return (SNAP_NOMEMORY);
    }

    cp = PointerFromCID (cid);
    DBGMSG (BEG+CTL,("BeginConv - CCB at: %s", PTR(cp)));

    /* Fill in CCB with default values */

    if (connparms) {
	cp->maxtime = connparms->maxtime;
	cp->timeout = connparms->timeout;
    }
    else {
	cp->maxtime = 30;
	cp->timeout = 5;
    }
    cp->serverdead = FALSE;
    cp->encryptlevel = SNAP_NOENCRYPT;
    cp->lastseqno = (0x0000FFFFL & time(NULL))+1;
    cp->convid = cid;
    cp->maxfrag = CLIENT_MAXFRAG;
    cp->serverwindow = SERVER_WINDOW;
    for (i=0; i<CLIENT_WINDOW; i++)
	cp->rstatus[i] = FRAG_NOT_RECEIVED;
    for (i=0; i<MAXWINDOW; i++)
	cp->sstatus[i] = FRAG_NOT_RECEIVED;

    if ((host == NULL) || (strlen(host) == 0)) {
	char tempname[64];
	gethostname (tempname, sizeof(tempname));
	hp = gethostbyname (tempname);
    }
    else
	hp = gethostbyname (host);

    sp = getservbyname (guardian,NULL);

    if (SNAP_socket == -1) {			/* Create a socket */
	SNAP_socket = socket (AF_INET, SOCK_DGRAM, PF_UNSPEC);
	DBGMSG (BEG,("BeginConv - socket handle is %d",SNAP_socket));
    }

    if ((cp->maxtime <0) ||			  /* Validate parameters */
	 (cp->timeout <0) ||
	 (cp->maxtime < cp->timeout) ||
	 (connparms == NULL) ||
	 ((connparms->encryptlevel != SNAP_NOENCRYPT) &&
	  (connparms->encryptlevel != SNAP_ENCRYPT)) ||
	 (userid == NULL)  ||
	 (server == NULL)  ||
	 (guardian == NULL) ||
	 (hp == NULL)	 ||
	 (SNAP_socket == -1)) {
	DBGMSG (BEG,("BeginConv - parm validation failure"));
	ReleaseCCB (cid);
	return (SNAP_BADPARMS);
    }

    if (SetBuffer ((int) CLIENT_MAXFRAG) != SNAP_SUCCESS) {
	DBGMSG (BEG,("BeginConv - Couldn't acquire transmit buffer"));
	ReleaseCCB (cid);
	return (SNAP_NOMEMORY);
    }

    cp->server_na.sin_family = AF_INET; 	/* Complete network address */
    if(sp) cp->server_na.sin_port = htons(sp->s_port);
    else cp->server_na.sin_port = htons(2120);
    bcopy (hp->h_addr,&cp->server_na.sin_addr.s_addr,hp->h_length);
    for (i=0;i<8;i++)
	cp->server_na.sin_zero[i] = 0;
    bcopy (&cp->server_na, &cp->guardian_na, sizeof(NETADDR));

    DBGMSG (BEG,("BeginConv - Host address: %04x %04x %016x",
		  cp->server_na.sin_family, cp->server_na.sin_port,
		  cp->server_na.sin_addr.s_addr));

    /*
      * Now we will construct the authentication message.
      * It's body portion has the following format:
  *
  *	    SNAP_string     servername;
  *	    SNAP_string     client_userid;
  *	    SNAP_bytestring client_password;
  *	    SNAP_integer    password_type;
  */

    quittime = time(NULL) + cp->maxtime;

    SNAP_BuildStandardHdr (msghdr, SNAP_AUTHENTICATE,
			    cp->convid, cp->lastseqno);
    mp = SNAP_AppendStringToMsg   (bcmsg, server);
    pp = SNAP_AppendStringToMsg   (mp, userid);
    mp = SNAP_AppendBytesToMsg	  (pp, password, plen);
    SNAP_ExtractBytesFromMsg (pp, &pp);
    scramble (pp, plen);
    mp = SNAP_AppendIntToMsg	  (mp, (SNAP_integer) ptype);

    msglen = mp - bcmsg;

    if ((replylen = SendReceive (cp, (FARCHAR *)bcmsg, msglen, msghdr,
				  (FARCHAR *)reply, sizeof(reply))) < SNAP_SUCCESS) {
	DBGMSG (BEG,("BeginConv - failed guardian exchange: %d",replylen));
	ReleaseCCB (cid);
	return (SNAP_XMITFAIL);
    }

    /*
      * Now we will examine the authentication reply.
      * Its body portion has the following format:
      *
      *	    SNAP_integer    returncode;
      *	    SNAP_bytestring server_na;
      *	    SNAP_bytestring sessionkey;
      */

    rp = SNAP_ExtractIntFromMsg (reply, &rc);

    connparms->guardian_rc = rc;
    if (rc != SNAP_SUCCESS) {
	DBGMSG (BEG,("BeginConv - guardian error during authenticate: %ld",rc));
	ReleaseCCB (cid);
	return (SNAP_GUARDIAN_ERROR);
    }

    SNAP_ExtractIntFromMsg (rp, &temp);

    if (temp != sizeof(NETADDR)) {
	DBGMSG (BEG,("BeginConv - improper netaddr format: %ld",temp));
	ReleaseCCB (cid);
	return (SNAP_XMITFAIL);
    }

    rp = SNAP_ExtractBytesFromMsg (rp, &saptr);
    bcopy (saptr, &cp->server_na, sizeof(NETADDR));
    SNAP_NetaddrFromNBO (&cp->server_na);
    DBGMSG (BEG, ("BeginConv - server address is <%d, %d, 0x%x>",
		   cp->server_na.sin_family, cp->server_na.sin_port,
		   cp->server_na.sin_addr.s_addr));

    SNAP_ExtractIntFromMsg (rp, &temp);

    if (temp != SNAP_KEYLEN) {
	DBGMSG (BEG,("BeginConv - improper key format: %ld",temp));
	ReleaseCCB (cid);
	return (SNAP_XMITFAIL);
    }

    rp = SNAP_ExtractBytesFromMsg (rp, &keyptr);
    bcopy (keyptr, cp->sessionkey, SNAP_KEYLEN);

    DBGMSG (BEG,("BeginConv - new conversation id: %d",cid));

    /*
      * From here on the encryption option is allowed to take effect
      *
      */

    cp->encryptlevel = connparms->encryptlevel;

    /*
      * Next we construct the server's begin conversation message.
      * It's body portion has the following format:
      *
      *	    SNAP_integer    suggested max fragment size;
      *	    SNAP_integer    client's receive window size;
      *	    SNAP_integer    client's major version number;
      *	    SNAP_integer    client's minor version number;
      */

    SNAP_BuildStandardHdr (msghdr, SNAP_BEGINCONV,
			    cp->convid, cp->lastseqno);

    /* Force server to restart this conversation if necessary */

    msghdr[0] |= ACCEPT_SEQNO;

    mp = SNAP_AppendIntToMsg (bcmsg, (SNAP_integer) CLIENT_MAXFRAG);
    mp = SNAP_AppendIntToMsg (mp, (SNAP_integer) CLIENT_WINDOW);
    mp = SNAP_AppendIntToMsg (mp, (SNAP_integer) MAJOR_VERSION);
    mp = SNAP_AppendIntToMsg (mp, (SNAP_integer) MINOR_VERSION);

    msglen = mp - bcmsg;

    if ((replylen = SendReceive (cp, (FARCHAR *)bcmsg, msglen, msghdr,
				  (FARCHAR *)reply, sizeof(reply))) < SNAP_SUCCESS) {
	DBGMSG (BEG,("BeginConv - failed server exchange: %d",replylen));
	ReleaseCCB (cid);
	return (replylen);
    }

    /*
      * Finally, we examine the begin conversation reply.
      * Its body portion has the following format:
      *
      *	    SNAP_integer    returncode;
      *	    SNAP_integer    agreed upon max fragment size;
      *	    SNAP_integer    server's receive window size;
      *	    SNAP_integer    server's major version number;
      *	    SNAP_integer    server's minor version number;
      */

    rp = SNAP_ExtractIntFromMsg (reply, &rc);
    if (rc != SNAP_SUCCESS) {
	DBGMSG (BEG,("BeginConv - server rejection: %ld",rc));
	ReleaseCCB (cid);
	return (SNAP_SERVERREJECT);
    }

    rp = SNAP_ExtractIntFromMsg (rp, &temp);
    if (temp > CLIENT_MAXFRAG) {
	DBGMSG (BEG,("BeginConv - server failed frag negotiation: %ld",temp));
	SNAP_EndConv (cid, (FARCHAR *) NULL, 0, (SNAP_CPARMS *) NULL);
	ReleaseCCB (cid);
	return (SNAP_XMITFAIL);
    }
    cp->maxfrag = temp;

    if ((SetBuffer (cp->maxfrag)) == SNAP_NOMEMORY) {
	SNAP_EndConv (cid, (FARCHAR *) NULL, 0, (SNAP_CPARMS *) NULL);
	ReleaseCCB (cid);
	return (SNAP_NOMEMORY);
    }

    rp = SNAP_ExtractIntFromMsg (rp, &temp);
    cp->serverwindow = temp;

    rp = SNAP_ExtractIntFromMsg (rp, &temp);
    if (temp != MAJOR_VERSION) {
	DBGMSG (BEG,("BeginConv - server failed version check: %ld",temp));
	SNAP_EndConv (cid, (FARCHAR *) NULL, 0, (SNAP_CPARMS *) NULL);
	ReleaseCCB (cid);
	return (SNAP_XMITFAIL);
    }

    rp = SNAP_ExtractIntFromMsg (rp, &temp);
    cp->servminorversion = temp;

    return (cid);
}


/*
  *  The SNAP "Send With Reply" Service.
  *  Returns the number of bytes received or (if negative) an error code
  *
  */

int SNAP_SendWithReply (cid, sendata, sdatalen, replybuf, rbuflen, msgparms)
int	cid, sdatalen, rbuflen;
FARCHAR *sendata;
FARCHAR *replybuf;
SNAP_CPARMS *msgparms;
{
    int     replylen;
    struct  ccb *cp;
    char    msghdr[SNAP_HDRLENGTH];

    DBGMSG (SWR,("SendWithReply - cid: %d, msglen: %d", cid, sdatalen));
    DBGMSG (SWR,("SendWithReply - msg at: %s", PTR(sendata)));
    DBGMSG (SWR,("SendWithReply - reply at: %s", PTR(replybuf)));
    DBGMSG (SWR,("SendWithReply - reply buflen: %d", rbuflen));

    if (cid > MAXCONVS || cid < 0) {
	DBGMSG (SWR,("SendWithReply - invalid cid: %d",cid));
	return (SNAP_INVALIDCID);
    }

    if ((sendata == NULL) || (replybuf == NULL)) {
	DBGMSG (SWR,("SendWithReply - invalid buffer ptrs, s: %s r: %s",
		     PTR(sendata), PTR(replybuf)));
	return (SNAP_BADPARMS);
    }

    cp = PointerFromCID (cid);
    if (cp->serverdead)
	return (SNAP_SERVERDIED);

    if (OverrideSessionParms (SET, cp, msgparms) != SNAP_SUCCESS)
	return (SNAP_BADPARMS);
    quittime = time(NULL) + cp->maxtime;

    if ((SetBuffer (cp->maxfrag)) == SNAP_NOMEMORY)
	return (SNAP_NOMEMORY);

    SNAP_BuildStandardHdr (msghdr, SNAP_SENDWITHREPLY,
			    cp->convid, cp->lastseqno);

    if ((replylen = SendReceive (cp, sendata, sdatalen, msghdr,
				  replybuf, rbuflen)) < SNAP_SUCCESS) {
	DBGMSG (SWR,("SendWithReply - error from SendReceive: %d",replylen));
	OverrideSessionParms (RESET, cp, msgparms);
	return (replylen);
    }

    DBGMSG (SWR,("SendWithReply - reply length: %d",replylen));
    OverrideSessionParms (RESET, cp, msgparms);
    return (replylen);
}


/*
  *  The SNAP "Send No Reply" Service.
  *  Returns SUCCESS or (if negative) an error code
  *
  */

int SNAP_SendNoReply (cid, sendata, sdatalen, msgparms)
int	cid, sdatalen;
FARCHAR *sendata;
SNAP_CPARMS *msgparms;
{
    int     rc;
    struct  ccb *cp;
    char    msghdr[SNAP_HDRLENGTH];

    DBGMSG (SNR,("SendNoReply - cid: %d, msglen: %d", cid, sdatalen));
    DBGMSG (SNR,("SendNoReply - msg at: %s", PTR(sendata)));

    if (cid > MAXCONVS || cid < 0) {
	DBGMSG (SNR,("SendNoReply - invalid cid: %d", cid));
	return (SNAP_INVALIDCID);
    }

    if (sendata == NULL) {
	DBGMSG (SNR,("SendNoReply - invalid buffer ptr: %s",
		     PTR(sendata)));
	return (SNAP_BADPARMS);
    }

    cp = PointerFromCID (cid);
    if (cp->serverdead)
	return (SNAP_SERVERDIED);

    if (OverrideSessionParms (SET, cp, msgparms) != SNAP_SUCCESS)
	return (SNAP_BADPARMS);
    quittime = time(NULL) + cp->maxtime;

    if ((SetBuffer (cp->maxfrag)) == SNAP_NOMEMORY)
	return (SNAP_NOMEMORY);

    SNAP_BuildStandardHdr (msghdr, SNAP_SENDNOREPLY,
			    cp->convid, cp->lastseqno++);

    if ((rc = SNAP_SendMsg (SNAP_socket, sendata, sdatalen, &cp->server_na,
			     cp->timeout, quittime, cp->encryptlevel, cp->sessionkey,
			     msghdr, xmitbuf, bufsize, 1, cp->maxfrag, cp->serverwindow,
			     cp->sstatus, FALSE, FALSE, NotifyHandler)) < 0) {
	DBGMSG (SNR,("SendNoReply - error from SNAP_SendMsg: %d",rc));
	OverrideSessionParms (RESET, cp, msgparms);
	return (rc);
    }

    OverrideSessionParms (RESET, cp, msgparms);
    return (SNAP_SUCCESS);
}

/*
  *  The SNAP "End Conversation" Service.
  *  An optional msg body may be passed to the server
  *  [Specify NULL for msg ptr or 0 for the length to omit message]
  *  Returns SUCCESS or (if negative) an error code.
  *  The CCB is always destroyed.
  *
  */

int SNAP_EndConv (cid, sendata, sdatalen, msgparms)
int	cid, sdatalen;
FARCHAR *sendata;
SNAP_CPARMS *msgparms;
{
    int     rc;
    struct  ccb *cp;
    char    msghdr[SNAP_HDRLENGTH];

    DBGMSG (END,("EndConv - cid: %d, msglen: %d", cid, sdatalen));
    DBGMSG (END,("EndConv - msg: %s", PTR(sendata)));

    if (cid > MAXCONVS || cid < 0) {
	DBGMSG (END,("EndConv - invalid cid: %d", cid));
	return (SNAP_INVALIDCID);
    }

    rc = SNAP_SUCCESS;
    cp = PointerFromCID (cid);
    if (cp->serverdead)
	rc = SNAP_SERVERDIED;
    else {
	if (OverrideSessionParms (SET, cp, msgparms) != SNAP_SUCCESS)
	    return (SNAP_BADPARMS);
	quittime = time(NULL) + cp->maxtime;

	if ((SetBuffer (cp->maxfrag)) == SNAP_NOMEMORY)
	    return (SNAP_NOMEMORY);

	SNAP_BuildStandardHdr (msghdr, SNAP_ENDCONV,
			       cp->convid, cp->lastseqno++);

	if ((rc = SNAP_SendMsg (SNAP_socket, sendata, sdatalen, &cp->server_na,
				cp->timeout, quittime, cp->encryptlevel, cp->sessionkey,
				msghdr, xmitbuf, bufsize, 1, cp->maxfrag, cp->serverwindow,
				cp->sstatus, FALSE, FALSE, NotifyHandler)) < 0) {
	    DBGMSG (END,("EndConv - error from SNAP_SendMsg: %d",rc));
	    OverrideSessionParms (RESET, cp, msgparms);
	}
	else
	    rc = SNAP_SUCCESS;
    }

    ReleaseCCB (cid);
    return (rc);
}

