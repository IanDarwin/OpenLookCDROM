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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/snap2/RCS/snapserv.c,v 2.11 1993/05/04 01:49:46 susan Exp $";
#endif

#include <stdio.h>
#include <andrewos.h> /* sys/types.h sys/time.h */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>


#include <snap.h>
#include <snapcom.h>

int serv_initflag;
PRIVATE int serv_socket, serv_maxtime, serv_encryptopt;
PRIVATE int serv_timeout, serv_maxbuf, serv_maxmsg, next_scid;
PRIVATE char serv_key[SNAP_KEYLEN];
PRIVATE char *serv_msgbuf;

struct	ccb {			    /* Conversation Control Block   */
    struct ccb	*nextccb;	    /* Pointer to next CCB	    */
    NETADDR	 client_na;	    /* Network address of client    */
    char	*replybuf;	    /* Address of last reply buffer */
    int 	 replylen;	    /* Total length of last reply   */
    int 	 encryptreply;	    /* Encrypt option for next reply*/
    SNAP_integer lastseqno;	    /* Last seqno received	    */
    int 	 replyneeded;	    /* YES = reply still unsent     */
    int 	 cconvid;	    /* Client conv. id for session  */
    int 	 sconvid;	    /* Server conv. id for session  */
    int 	 clientminorvers;   /* Minor version # of client    */
    int 	 clientwindow;	    /* Client receive window size   */
    int 	 fragcnt;	    /* Frags since last ack	    */
    int 	 fragsdone;	    /* Frags completely acked	    */
    int 	 maxfrag;	    /* Agreed upon max frag size    */
    int 	 inmsgready;	    /* TRUE = all frags received    */
    int 	 inmsgdatalen;	    /* length of data in inmsg	    */
    char	*inmsg; 	    /* inbound msg buffer	    */
    char	 rstatus[SERVER_WINDOW]; /* Receive window status   */
    char	 sstatus[MAXWINDOW];/* Send window status	    */
};

PRIVATE struct ccb *ccbhead;	    /* Anchor for CCB list */
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
  *  Create a new CCB
  *
  */

PRIVATE struct ccb *NewCCB (prevccb, source_na, cid, seqno)
struct ccb *prevccb;
NETADDR *source_na;
SNAP_integer cid, seqno;
{
    int i;
    struct ccb *newccb;

    DBGMSG (CTL,("NewCCB - cid: %ld, seqno: %ld",cid,seqno));

    if ((newccb = (struct ccb *) malloc (sizeof (struct ccb))) == NULL) {
	DBGMSG (CTL,("NewCCB allocation failed"));
	return (NULL);
    }

    if ((newccb->inmsg = (char *) malloc (serv_maxmsg)) == NULL) {
	DBGMSG (CTL,("NewCCB msg buffer allocation failed"));
	free (newccb);
	return (NULL);
    }

    DBGMSG (CTL,("NewCCB allocated at %s", PTR(newccb)));

    if (prevccb != NULL) {
	DBGMSG (CTL,("NewCCB - prior CCB in list: %s", PTR(prevccb)));
	prevccb->nextccb = newccb;
    }
    else {
	DBGMSG (CTL,("NewCCB - first CCB"));
	ccbhead = newccb;
    }
    bcopy (source_na, &newccb->client_na, sizeof(NETADDR));

    newccb->replybuf	 = NULL;
    newccb->lastseqno	 = seqno;
    newccb->cconvid	 = cid;
    newccb->sconvid	 = next_scid++;
    newccb->nextccb	 = NULL;
    newccb->replyneeded  = NO;
    newccb->inmsgready	 = FALSE;
    newccb->inmsgdatalen = 0;
    newccb->replylen	 = 0;
    newccb->fragcnt	 = 0;
    newccb->fragsdone	 = 0;
    newccb->encryptreply = serv_encryptopt;
    newccb->clientwindow = CLIENT_WINDOW;
    newccb->maxfrag	 = SERVER_MAXFRAG;
    newccb->clientminorvers = -1;
    for (i=0; i<SERVER_WINDOW; i++)
	newccb->rstatus[i] = FRAG_NOT_RECEIVED;
    for (i=0; i<MAXWINDOW; i++)
	newccb->sstatus[i] = FRAG_NOT_RECEIVED;

    DBGMSG (CTL,("NewCCB - CCB initialized"));

    return (newccb);
}


/*
  *  Find a CCB from the CCB list using its network address.
  *  The "createflag" parameter controls whether one will be
  *  created and added to the list automatically.
  *
  */

PRIVATE struct ccb *FindCCBbyNA (source_na, cid, seqno, newflag, createflag)
NETADDR *source_na;
SNAP_integer cid, seqno;
int newflag, createflag;
{
    struct ccb *p,*nextp,*prevp;

    DBGMSG (CTL,
	     ("FindCCBbyNA - seqno: %ld, client: %04x %04x %016x",
	      seqno,
	      source_na->sin_family, source_na->sin_port,
	      source_na->sin_addr.s_addr));
    DBGMSG (CTL,("FindCCBbyNA - cid: %ld", cid));

    prevp = NULL;
    for (p=ccbhead; p != NULL; prevp=p,p=nextp) {
	nextp = p->nextccb;
	if ((bcmp (source_na, &p->client_na, sizeof(NETADDR)) == 0) &&
	    (cid == p->cconvid)) {
	    DBGMSG (CTL,("FindCCBbyNA - CCB at %s", PTR(p)));
	    if (newflag) {
		p->lastseqno = seqno-1;
		DBGMSG (CTL,("FindCCBbyNA - seqno forced"));
	    }
	    return (p);
	}
    }
    if (createflag)
	return (NewCCB (prevp,source_na,cid,seqno));
    else
	return (NULL);
}


/*
  *  Find a CCB from the CCB list using its Server CID.
  *
  */

PRIVATE struct ccb *FindCCBbyCID (cid)
int cid;
{
    struct ccb *p,*nextp;

    DBGMSG(CTL,("FindCCBbyCID - cid: %d",cid));

    for (p=ccbhead; p != NULL; p=nextp) {
	nextp = p->nextccb;
	if (cid == p->sconvid) {
	    DBGMSG (CTL,("FindCCBbyCID - CCB at %s", PTR(p)));
	    return (p);
	}
    }
    DBGMSG (CTL,("FindCCBbyCID - no matching CCB!"));
    return (NULL);
}


/*
  *  Free a CCB and its associated storage
  *
  */

PRIVATE FreeCCB (cp)
struct ccb *cp;
{
    DBGMSG (CTL,("FreeCCB - CCB at %s", PTR(cp)));
    if (cp->inmsg != NULL) {
	DBGMSG (CTL,("FreeCCB - inmsg at %s", PTR(cp->inmsg)));
	free (cp->inmsg);
    }
    if (cp->replybuf != NULL) {
	DBGMSG (CTL,("FreeCCB - replybuf at %s", PTR(cp->replybuf)));
	free (cp->replybuf);
    }
    free (cp);
}


/*
  *  Remove a CCB from the list and free it
  *
  */

PRIVATE ReleaseCCB (cp)
struct ccb *cp;
{
    struct ccb *prevp, *nextp, *p;
    DBGMSG (CTL,("ReleaseCCB - CCB: %s", PTR(cp)));
    if (cp != NULL) {
	prevp = NULL;
	for (p=ccbhead; p != NULL; prevp=p,p=nextp) {
	    nextp = p->nextccb;
	    if (cp == p) {
		FreeCCB (cp);
		if (prevp == NULL)
		    ccbhead = nextp;
		else
		    prevp->nextccb = nextp;
		return;
	    }
	}
	DBGMSG (CTL,("ReleaseCCB - error: CCB not on list"));
    }
    else
	DBGMSG (CTL,("ReleaseCCB - error: CCB ptr is null"));
}


/*
  *  GetWaitingMsgs polls the CCB list to see if any completed
      *  messages are available yet.
      *
      */

PRIVATE int GetWaitingMsgs (msgptr, type, cid)
FARCHAR **msgptr;
int *type, *cid;
{
    struct ccb *p,*nextp;
    static struct ccb *ccbstart = NULL;
    SNAP_integer msg_op, msg_seqno;
    int rc;
    char *m;

    DBGMSG(RCV,("GetWaitingMsgs"));

    if (ccbstart == NULL)
	ccbstart = ccbhead;

    for (p = ccbstart; ((p != NULL) && (p->nextccb != ccbstart)); p = nextp) {
	nextp = p->nextccb;
	if ((ccbstart != ccbhead) && (p != ccbstart) && (nextp == NULL))
	    nextp = ccbhead;
	if (p->inmsgready) {
	    DBGMSG (RCV,("GetWaitingMsgs - found one, CCB: %s", PTR(p)));
	    m = SNAP_ExtractIntFromMsg ((p->inmsg)+(2*sizeof(SNAP_integer)),
					&msg_op);
	    SNAP_ExtractIntFromMsg (m, &msg_seqno);
	    *cid = p->sconvid;
	    *type = (int) msg_op;
	    *msgptr = (p->inmsg)+SNAP_HDRLENGTH+FRAG_HDRLENGTH;
	    rc = p->inmsgdatalen;

	    NewMsgAccepted (p, msg_seqno, msg_op);

	    if (msg_op == SNAP_ENDCONV) {
		DBGMSG (RCV,("GetWaitingMsgs - conv ended by client"));
		ReleaseCCB (p);
	    }

	    ccbstart = nextp;
	    return (rc);
	}
    }
    DBGMSG (RCV,("GetWaitingMsgs - none available yet!"));
    ccbstart = NULL;
    return (SNAP_FAIL);
}


/*
  *  Reset the current CCB to reflect the acceptance of a new message
  *
  */

PRIVATE NewMsgAccepted (cp, seqno, msgop)
struct ccb *cp;
SNAP_integer seqno, msgop;
{
    cp->lastseqno = seqno;
    if (cp->replybuf != NULL) {
	free (cp->replybuf);
	cp->replybuf = NULL;
    }
    cp->encryptreply =
      (*(cp->inmsg) & ENCRYPTED_MSG) ? SNAP_ENCRYPT : SNAP_NOENCRYPT;
    if (cp->encryptreply == SNAP_NOENCRYPT)
	cp->encryptreply = serv_encryptopt;
    if (msgop == SNAP_SENDWITHREPLY)
	cp->replyneeded = YES;
    else
	cp->replyneeded = NO;
    ResetFragState (cp);
    if ((msgop != SNAP_BEGINCONV) && (msgop != SNAP_SENDNOREPLY))
	SNAP_PurgePacketQueue (&cp->client_na, cp->cconvid, seqno);
}


/*
  *  Reset the current CCB to reflect the receipt of no fragments as yet
  *
  */
PRIVATE ResetFragState (cp)
struct ccb *cp;
{
    int i;
    cp->fragsdone = 0;
    cp->fragcnt = 0;
    for (i=0; i<SERVER_WINDOW; i++)
	cp->rstatus[i] = FRAG_NOT_RECEIVED;
    cp->inmsgready = FALSE;
    cp->inmsgdatalen = 0;
}


/*
  *  Reset the current CCB to reflect the sending of a new reply
  *
  */

PRIVATE NewReplySent (cp, msgbuf, msglen)
struct ccb *cp;
FARCHAR *msgbuf;
int msglen;
{
    int i;
    char *mybuf;

    cp->replyneeded = NO;
    if (cp->replybuf != NULL) {
	cp->replylen = msglen;
	mybuf = cp->replybuf;

	/* For portability to PC environment don't use library routines
* to copy the user's data.
	 */

	for (i=0; i<msglen; i++)
	    *(mybuf++) = msgbuf[i];
    }
    ResetFragState (cp);
}


/*
  *  PassiveNackResponse returns missing fragments in response to
  *  stranded SNAP_ACKs carrying negative acknowledgements.
  */

PRIVATE PassiveNackResponse (packet, seqno, cp)
char *packet;
SNAP_integer seqno;
struct ccb *cp;
{
    char *p, msghdr[SNAP_HDRLENGTH], fragstatus[MAXWINDOW];
    SNAP_integer fragword, datalen;
    int frag1, frag2, i;

    DBGMSG (RCV,("PassiveNackResponse - seqno: %ld", seqno));

    if (cp->replybuf == NULL || cp->lastseqno != seqno)
	return;

    p = SNAP_ExtractIntFromMsg (packet+SNAP_HDRLENGTH, &fragword);
    p = SNAP_ExtractIntFromMsg (p, &datalen);

    frag1 = (int) (fragword & MAXSHORT);
    frag2 = (int) ((fragword >> (BITSPERBYTE*2)) & MAXSHORT);

    DBGMSG (RCV,("PassiveNackResponse - frag1: %d, frag2: %d", frag1, frag2));
    DBGMSG (RCV,("PassiveNackResponse - datalen: %ld", datalen));

    if ((frag2 < frag1) || (frag2-frag1+1 != (int) datalen) ||
	 (datalen > MAXWINDOW)) {
	DBGMSG (RCV,("PassiveNackResponse - invalid ack discarded"));
	return;
    }

    for (i=0; i<datalen; i++)
	fragstatus[i] = *(p+i);

    SNAP_BuildStandardHdr (msghdr, SNAP_REPLY, cp->cconvid, seqno);

    for (i=0; i<datalen; i++) {
	if (fragstatus[i] == FRAG_NOT_RECEIVED) {
	    SNAP_SendFrag (serv_socket, cp->replybuf, cp->replylen,
			   &cp->client_na, cp->encryptreply, serv_key, msghdr,
			   serv_msgbuf, frag1+i, cp->maxfrag);
	}
    }
}


/*
  *  Perform sequence number checking
  *
  *  Returns
  *	1   if seqno has been advanced by the client
      *	0   if seqno matched as expected
	  *     -1   if seqno is for a previous msg
	      *
	      */

PRIVATE int SeqNoCheck (cp, seqno)
struct ccb *cp;
SNAP_integer seqno;
{
    char msghdr[SNAP_HDRLENGTH];
    long quittime;

    DBGMSG (ACC,("SeqNoCheck - old: %ld vs new: %ld",cp->lastseqno,seqno));
    if (seqno == (cp->lastseqno)+1) {
	DBGMSG (ACC,("SeqNoCheck - passed"));
	return (0);
    }
    else {
	if (seqno == cp->lastseqno) {
	    if (cp->replybuf != NULL) {
		char tempstatus[SERVER_WINDOW];

		DBGMSG (ACC,("SeqNoCheck - rexmitting last reply!"));

		SNAP_BuildStandardHdr (msghdr, SNAP_REPLY,
				       cp->cconvid, cp->lastseqno);

		quittime = time(NULL) + serv_maxtime;

		SNAP_SendMsg (serv_socket, (FARCHAR *) cp->replybuf,
			      cp->replylen, &cp->client_na, serv_timeout,
			      quittime, cp->encryptreply, serv_key, msghdr,
			      serv_msgbuf, serv_maxbuf, 1, cp->maxfrag,
			      cp->clientwindow, tempstatus, FALSE, TRUE,
			      NotifyHandler);
	    }
	    else {
		DBGMSG (ACC,("SeqNoCheck - no reply to retransmit"));
	    }
	}
	else {
	    DBGMSG (ACC,("SeqNoCheck - expecting %ld recd %ld",
			 (cp->lastseqno)+1,seqno));
	    DBGMSG (ACC,("SeqNoCheck - out of sequence condition!"));
	    if (seqno > cp->lastseqno+1) {
		DBGMSG (ACC,
			("SeqNoCheck - forcibly advancing seqno to next msg"));
		cp->lastseqno = seqno-1;
		return (1);
	    }
	}
	return (-1);
    }
}


/*
  *  Handle a Begin Conversation request
  *
  */

PRIVATE BeginConversation (msgbuf, source_na, convid, seqno)
char *msgbuf;
NETADDR *source_na;
SNAP_integer convid, seqno;
{
    SNAP_integer client_maxfrag, client_window, client_majvers, client_minvers;
    char bcmsg[100], *bc, msghdr[SNAP_HDRLENGTH];
    struct ccb *cp;
    int rc, msglen;
    long quittime;

    /*
      * The body of the begin conversation message has the
      * following format:
      *
      *	    SNAP_integer    Suggested max fragment size;
      *	    SNAP_integer    Client's receive window size;
*	    SNAP_integer    Client's major version number;
      *	    SNAP_integer    Client's minor version number;
*/

    bc = SNAP_ExtractIntFromMsg (msgbuf+SNAP_HDRLENGTH+FRAG_HDRLENGTH,
				  &client_maxfrag);
    bc = SNAP_ExtractIntFromMsg (bc, &client_window);
    bc = SNAP_ExtractIntFromMsg (bc, &client_majvers);
    SNAP_ExtractIntFromMsg (bc, &client_minvers);

    DBGMSG (BEG,("BeginConv - client max frag: %ld", client_maxfrag));
    DBGMSG (BEG,("BeginConv - client window: %ld", client_window));
    DBGMSG (BEG,("BeginConv - client version: %d.%d",
		  (int) client_majvers, (int) client_minvers));

    rc = SNAP_SUCCESS;

    if ((cp = FindCCBbyNA (source_na, convid, seqno,
			    FALSE, TRUE)) == NULL) {
	DBGMSG (BEG,("BeginConv - out of memory!"));
	rc = SNAP_NOMEMORY;
    }

    if (cp->replybuf != NULL) {     /* This is done in case this BegConv */
	free (cp->replybuf);	    /* is actually a re-connect 	 */
	cp->replybuf = NULL;
    }

    cp->lastseqno = seqno;
    cp->maxfrag = (client_maxfrag > SERVER_MAXFRAG) ? SERVER_MAXFRAG :
      (int) client_maxfrag;
    cp->clientwindow = (int) client_window;

    if (client_majvers != MAJOR_VERSION) {
	DBGMSG (BEG,("BeginConv - client failed version check: %ld",
		     client_majvers));
	rc = SNAP_WRONGVERSION;
    }

    cp->clientminorvers = client_minvers;

    cp->replybuf = (char *) malloc (sizeof(bcmsg));
    if (cp->replybuf == NULL) {
	DBGMSG (BEG,("BeginConv - cannot malloc rexmit buffer"));
	rc = SNAP_NOMEMORY;
    }

    if (rc = SNAP_SUCCESS)
	NewMsgAccepted (cp, seqno, (SNAP_integer) SNAP_BEGINCONV);

    /*
      * The body of the begin conversation reply has the
      * following format:
      *
      *	    SNAP_integer    Return code;
      *	    SNAP_integer    Agreed upon max fragment size;
      *	    SNAP_integer    Server's receive window size;
      *	    SNAP_integer    Server's major version number;
      *	    SNAP_integer    Server's minor version number;
      */

    SNAP_BuildStandardHdr (msghdr, SNAP_REPLY,
			    cp->cconvid, cp->lastseqno);

    bc = SNAP_AppendIntToMsg (bcmsg, (SNAP_integer) rc);
    bc = SNAP_AppendIntToMsg (bc, (SNAP_integer) cp->maxfrag);
    bc = SNAP_AppendIntToMsg (bc, (SNAP_integer) SERVER_WINDOW);
    bc = SNAP_AppendIntToMsg (bc, (SNAP_integer) MAJOR_VERSION);
    bc = SNAP_AppendIntToMsg (bc, (SNAP_integer) MINOR_VERSION);

    msglen = bc - bcmsg;
    quittime = time(NULL) + serv_maxtime;

    SNAP_SendMsg (serv_socket, (FARCHAR *) bcmsg, msglen, &cp->client_na,
		   serv_timeout, quittime, cp->encryptreply, serv_key, msghdr,
		   serv_msgbuf, serv_maxbuf, 1, cp->maxfrag, cp->clientwindow,
		   cp->sstatus, FALSE, TRUE, NotifyHandler);

    if (rc != SNAP_SUCCESS)
	ReleaseCCB (cp);
    else
	NewReplySent (cp, (FARCHAR *) bcmsg, msglen);
}


/*
  *  Accept a message fragment
  *
  *  This procedure keeps track of inbound fragments by
  *
  *	1. Determining when a window ack is due (and sending it)
  *	2. Accumulating fragment data by conversation
  *	3. Determining when a complete logical message is available
  */

PRIVATE AcceptFrag (msgbuf, msglen, msgop, cp)
char *msgbuf;
int msglen;
SNAP_integer msgop;
struct ccb *cp;
{
    SNAP_integer fragword, datalen;
    int frag, lastfrag, fragstart, fragend, length;
    int lastwindow, windowdone, i, j, temp;
    char *fp, *target;

    /*
      * All SNAP message fragments have a fragment header with the
      * following fields:
      *
      *	    SNAP_integer    fragword;
      *	    SNAP_integer    datalen;
      */

    fp = SNAP_ExtractIntFromMsg (msgbuf+SNAP_HDRLENGTH, &fragword);
    frag = (int) (fragword & MAXSHORT);
    lastfrag = (int) (fragword >> (BITSPERBYTE*2));
    DBGMSG (ACC,("AcceptFrag - %d of %d", frag, lastfrag));

    fp = SNAP_ExtractIntFromMsg (fp, &datalen);
    if (datalen + 6*sizeof(SNAP_integer) != msglen) {
	DBGMSG (ACC,("AcceptFrag - inconsistant length rejected"));
	return;
    }

    if ((frag != lastfrag) && (datalen != cp->maxfrag)) {
	DBGMSG (ACC,("AcceptFrag - short intermediate frag dropped"));
	return;
    }

    fragstart = ((frag-1)*(cp->maxfrag))+SNAP_HDRLENGTH+FRAG_HDRLENGTH;
    fragend = fragstart + datalen - 1;

    if (fragend >= serv_maxmsg)
	fragend = serv_maxmsg-1;

    if (frag > cp->fragsdone) {
	(cp->fragcnt)++;
	cp->rstatus[(frag-1) % SERVER_WINDOW] = FRAG_RECEIVED;
    }
    else {
	char goodacks[SERVER_WINDOW];
	DBGMSG (ACC,("AcceptFrag - dropping duplicate, re-acking"));
	for (i=0; i<SERVER_WINDOW; i++)
	    goodacks[i] = FRAG_RECEIVED;
	SNAP_SendAck (serv_socket, serv_key, serv_msgbuf,
		      cp->cconvid, cp->lastseqno+1, (cp->fragsdone-SERVER_WINDOW)+1,
		      cp->fragsdone, goodacks, serv_encryptopt, &cp->client_na);
	return;
    }

    target = (cp->inmsg)+fragstart;
    length = fragend - fragstart + 1;

    DBGMSG (ACC,("AcceptFrag - length: %d", length));

    if (length)
	bcopy (msgbuf+SNAP_HDRLENGTH+FRAG_HDRLENGTH, target, length);

    temp = fragstart+length-SNAP_HDRLENGTH-FRAG_HDRLENGTH;
    if (temp > cp->inmsgdatalen)
	cp->inmsgdatalen = temp;

    if (frag == 1)
	bcopy (msgbuf, cp->inmsg, SNAP_HDRLENGTH+FRAG_HDRLENGTH);

    if (frag > (lastfrag - ((lastfrag % SERVER_WINDOW == 0) ?
			     SERVER_WINDOW : lastfrag % SERVER_WINDOW)))
	lastwindow = TRUE;
    else
	lastwindow = FALSE;

    DBGMSG (ACC,("AcceptFrag - lastwindow: %s",
		  lastwindow ? "TRUE" : "FALSE"));

    windowdone = TRUE;
    j = SNAP_WindowSize (lastwindow, lastfrag, SERVER_WINDOW);
    for (i=0; i<j; i++)
	if (cp->rstatus[i] == FRAG_NOT_RECEIVED)
	    windowdone = FALSE;

    DBGMSG (ACC,("AcceptFrag - windowdone: %s",
		  windowdone ? "TRUE" : "FALSE"));

    if ((lastwindow) &&
	 ((msgop == SNAP_SENDWITHREPLY) || (msgop == SNAP_SENDNOREPLY) ||
	  (msgop == SNAP_ENDCONV))) {
	if (windowdone) {
	    cp->inmsgready = TRUE;
	    if (!(*msgbuf & MANDATORY_ACK))
		cp->fragsdone = lastfrag;
	}
	else
	    cp->inmsgready = FALSE;
	if (!(*msgbuf & MANDATORY_ACK))
	    return;
    }

    if ((windowdone) || (cp->fragcnt >= SERVER_WINDOW)) {

	SNAP_SendAck (serv_socket, serv_key, serv_msgbuf,
		      cp->cconvid, cp->lastseqno+1, (cp->fragsdone)+1,
		      (lastwindow) ? lastfrag : cp->fragsdone+SERVER_WINDOW,
		      cp->rstatus, serv_encryptopt, &cp->client_na);

	cp->fragcnt = 0;
	if (windowdone) {
	    cp->fragcnt = 0;
	    for (i=0; i<SERVER_WINDOW; i++)
		cp->rstatus[i] = FRAG_NOT_RECEIVED;
	    if (lastwindow)
		cp->fragsdone = lastfrag;
	    else
		cp->fragsdone += SERVER_WINDOW;
	}
    }

    DBGMSG (ACC,("AcceptFrag - fragsdone: %d", cp->fragsdone));

}


/*
  *  Initialize the SNAP Server support layer
  *
  */

int SNAP_ServerInit (socket, connparms, key)
int socket;
SNAP_CPARMS *connparms;
char *key;
{
    DBGMSG (SUT,("ServerInit"));
    serv_initflag = NO;
    if ((connparms->maxtime <0) ||		/* Validate parameters */
	 (connparms->timeout <0) ||
	 (connparms->maxtime < connparms->timeout) ||
	 (connparms->maxmsgsize <=0) ||
	 ((connparms->encryptlevel != SNAP_NOENCRYPT) &&
	  (connparms->encryptlevel != SNAP_ENCRYPT)) ||
	 (key == NULL) ||
	 (socket < 0)) {
	DBGMSG (SUT,("ServerInit - parm validation failure"));
	return (SNAP_BADPARMS);
    }
    serv_socket = socket;
    serv_maxtime = connparms->maxtime;
    serv_encryptopt = connparms->encryptlevel;
    serv_maxmsg = connparms->maxmsgsize + SNAP_HDRLENGTH + FRAG_HDRLENGTH;
    serv_maxbuf = SERVER_MAXFRAG + SNAP_HDRLENGTH + FRAG_HDRLENGTH;
    serv_timeout = connparms->timeout;
    next_scid = 0;
    bcopy (key, serv_key, SNAP_KEYLEN);

    if ((serv_msgbuf = (char *) malloc(serv_maxbuf)) == NULL) {
	DBGMSG (SUT,("ServerInit - Couldn't allocate transmit buffer"));
	return (SNAP_NOMEMORY);
    }

    ccbhead = NULL;
    serv_initflag = YES;
    return (SNAP_SUCCESS);
}


/*
  *  Terminate the SNAP Server support layer
  *
  */

int SNAP_ServerTerm ()
{
    struct ccb *p,*nextp;
    DBGMSG (SUT,("ServerTerm"));
    free (serv_msgbuf);
    serv_initflag = NO;
    for (p=ccbhead; p != NULL; p=nextp) {
	nextp = p->nextccb;
	FreeCCB (p);
    }
    ccbhead = NULL;
    return (SNAP_SUCCESS);
}


/*
  *  Returns the SNAP version numbers for the server side
      *
      */

int SNAP_ServerVersion (cid, versions)
int cid, versions[4];
{
    int rc;
    struct ccb *cp;

    DBGMSG (CTL,("ServerVersion %d.%d", MAJOR_VERSION, MINOR_VERSION));
    versions[0] = -1;
    versions[1] = -1;
    versions[2] = MAJOR_VERSION;
    versions[3] = MINOR_VERSION;

    if ((cp = FindCCBbyCID (cid)) == NULL) {
	DBGMSG (CTL,("ServerVersion - invalid cid: %d",cid));
	rc = SNAP_INVALIDCID;
    }
    else {
	versions[0] = MAJOR_VERSION;
	versions[1] = cp->clientminorvers;
	rc = SNAP_SUCCESS;
    }

    return (rc);
}


/*
  *  SNAP_Accept obtains a message from a client.
  *
  *  Returns the number of bytes of user data (zero-length messages are
						*  permitted), or, if negative, an error code.
						    *
						    *  The caller must provide pointers to the following additional output
						    *  parameters:
						    *
						    *	msgptr	The address of the data portion of the message
						    *	type	The SNAP opcode associated with the message
						    *	cid	The conversation id
						    *
						    *  A four parameter governs how accept will respond to receive timeouts.
						    *  If the value is greater than zero accept will return to the caller
						    *  after that interval has expired, otherwise accept will return to the
						    *  caller only when a complete message is available, or a non-timeout
						    *  error has occurred.
						    *
						    *  Note that the conversation id used by the server is not necessarily the
						    *  same as that used by the client, since the conversation id is guaranteed
						    *  to be unique in both contexts.
						    */

int SNAP_Accept (msgptr, type, cid, accept_timeout)
FARCHAR **msgptr;
int *type, *cid, accept_timeout;
{
    int recvlen, newflag, rc;
    char *p;
    SNAP_integer snap_op, snap_seqno, snap_conv;
    NETADDR source_na;
    struct ccb *cp;
    long quittime;

    DBGMSG (ACC,("Accept"));

    while (TRUE) {

	if ((recvlen = GetWaitingMsgs (msgptr, type, cid)) >= 0) {
	    DBGMSG (ACC,("Accept len: %d, type: %d, cid: %d, msg at %s",
			 recvlen, *type, *cid, PTR(*msgptr)));
	    return (recvlen);
	}

	quittime = time(NULL) +
	  (accept_timeout <= 0? serv_maxtime: accept_timeout);

	if ((recvlen = SNAP_CommonReceive (serv_socket,
					   (accept_timeout <= 0 ? serv_timeout : accept_timeout),
					   quittime, serv_key, serv_msgbuf, serv_maxbuf,
					   &source_na)) < 0) {
	    DBGMSG (ACC,("Accept - receive error: %d",recvlen));
	    if (recvlen == SNAP_TIMEOUT || recvlen == SNAP_SELECTFAILED) {
		if (accept_timeout > 0) /* Accept does return on timeouts */
		    return (recvlen);
		continue;   /* Accept does not return on timeouts */
	    }
	    else
		return (recvlen);
	}

	DBGMSG (ACC,("Accept - received msg len: %d",recvlen));

	/*
	 * Now we will examine the message header.
	 * It has the following format:
	 *
	 *	SNAP_integer	checksum; <-- standard header
	   *	SNAP_integer	conv;	  <-- standard header
	     *	SNAP_integer	opcode;   <-- standard header
	       *	SNAP_integer	seqno;	  <-- standard header
		 *	SNAP_integer	fragword; <-- frag header
		   *	SNAP_integer	datalen;  <-- frag header
		     *	SNAP_bytestring messagetext;
		   */

	p = SNAP_ExtractIntFromMsg (serv_msgbuf+sizeof(SNAP_integer),
				    &snap_conv);
	DBGMSG (ACC,("Accept - received cid is %ld",snap_conv));

	p = SNAP_ExtractIntFromMsg (p, &snap_op);
	DBGMSG (ACC,("Accept - received opcode is %ld",snap_op));

	if ((snap_op != SNAP_SENDWITHREPLY) &&
	    (snap_op != SNAP_SENDNOREPLY) &&
	    (snap_op != SNAP_BEGINCONV) &&
	    (snap_op != SNAP_ENDCONV) &&
	    (snap_op != SNAP_ACK))
	    continue;

	p = SNAP_ExtractIntFromMsg (p, &snap_seqno);
	DBGMSG (ACC,("Accept - received seqno is %ld",snap_seqno));

	newflag = *serv_msgbuf & ACCEPT_SEQNO;
	DBGMSG (ACC,("Accept - seqno flag is %d", newflag));

	if (snap_op != SNAP_BEGINCONV) {
	    if ((cp = FindCCBbyNA (&source_na, snap_conv, snap_seqno,
				   newflag, FALSE)) == NULL) {
		DBGMSG (ACC,("Accept - no such cid"));
		continue;
	    }

	    DBGMSG (ACC,("Accept - Found CCB at %s", PTR(cp)));

	    if (snap_op == SNAP_ACK) {
		PassiveNackResponse (serv_msgbuf, snap_seqno, cp);
		continue;
	    }

	    if ((rc = SeqNoCheck (cp,snap_seqno)) == 0) {
		if (cp->replyneeded == YES) {
		    DBGMSG (ACC,
			    ("Accept - inbound packet for client cid %d discarded",
			     cp->cconvid));
		    DBGMSG (ACC,("Accept - server cid: %d", cp->sconvid));
		    DBGMSG (ACC,("Accept - [server has outstanding reply]"));
		    continue;
		}
	    }
	    else {
		if (rc == 1)		    /* Accept packet but throw out */
		    ResetFragState (cp);    /* any unfinished prior msgs   */
		else {
		    DBGMSG (ACC,("Accept - looking for next packet"));
		    continue;
		}
	    }
	}
	else {
	    BeginConversation (serv_msgbuf, &source_na, snap_conv, snap_seqno);
	    continue;
	}

	DBGMSG (ACC,("Accept - packet accepted"));

	if (cp->replybuf != NULL) {
	    DBGMSG (ACC,("Accept - freeing former reply buffer"));
	    free (cp->replybuf);
	    cp->replybuf = NULL;
	}

	AcceptFrag (serv_msgbuf, recvlen, snap_op, cp);
    }
}


/*
  *  Reply to a message from a client over a particular conversation
  *  Returns number of reply bytes sent (or if negative, an error code)
  *
  */

int SNAP_Reply (msgbuf, mbuflen, cid)
FARCHAR *msgbuf;
int mbuflen, cid;
{
    struct ccb *cp;
    int rc;
    char msghdr[SNAP_HDRLENGTH];
    long quittime;

    DBGMSG (REP,("Reply - msg at %s, length: %d, cid: %d",
		  PTR(msgbuf), mbuflen, cid));

    if ((cp = FindCCBbyCID (cid)) == NULL) {
	DBGMSG (REP,("Reply - invalid cid: %d",cid));
	return (SNAP_INVALIDCID);
    }

    if (cp->replyneeded != YES) {
	DBGMSG (REP,("Reply - unsolicited reply not allowed"));
	return (SNAP_NOREPLYDUE);
    }

    if (mbuflen <= (serv_maxmsg - SNAP_HDRLENGTH - FRAG_HDRLENGTH)) {
	cp->replybuf = (char *) malloc (mbuflen);
	if (cp->replybuf == NULL) {
	    DBGMSG (REP,("Reply - cannot malloc buffer"));
	    return (SNAP_NOMEMORY);
	}
    }
    else {
	DBGMSG (REP,("Reply - length %d exceeds %d",
		     mbuflen, serv_maxmsg - SNAP_HDRLENGTH - FRAG_HDRLENGTH));
	return (SNAP_BUFFERLIMIT);
    }

    SNAP_BuildStandardHdr (msghdr, SNAP_REPLY,
			    cp->cconvid, cp->lastseqno);

    quittime = time(NULL) + serv_maxtime;

    if ((rc = SNAP_SendMsg (serv_socket, msgbuf, mbuflen,
			     &cp->client_na, serv_timeout, quittime, cp->encryptreply,
			     serv_key, msghdr, serv_msgbuf, serv_maxbuf, 1, cp->maxfrag,
			     cp->clientwindow, cp->sstatus, FALSE, TRUE,
			     NotifyHandler)) < 0) {

	DBGMSG (REP,("Reply - xmit failed: %d", rc));
    }

    NewReplySent (cp, msgbuf, mbuflen);

    return (mbuflen);
}


/*
  *  Since SNAP_Accept does not return to the caller until a complete
  *  message is available, this routine may be called to determine
  *  whether or not SNAP_Accept will return immediately with a new
  *  message.
  *
  *  Returns TRUE if a complete message is available to be "accepted",
      *  otherwise returns FALSE.
      *
      */

int SNAP_MsgPending ()
{
    struct ccb *p,*nextp;

    for (p=ccbhead; p != NULL; p=nextp) {
	nextp = p->nextccb;
	if (p->inmsgready) {
	    DBGMSG (CTL,("MsgPending - yes"));
	    return (TRUE);
	}
    }
    return (FALSE);
}


/*
  *  Returns the number of conversations currently established
  *
  */

int SNAP_ConvCount ()
{
    struct ccb *p,*nextp;
    int i;

    i = 0;
    for (p=ccbhead; p != NULL; p=nextp) {
	nextp = p->nextccb;
	i++;
    }

    DBGMSG (CTL,("Conv count: %d", i));
    return (i);
}
