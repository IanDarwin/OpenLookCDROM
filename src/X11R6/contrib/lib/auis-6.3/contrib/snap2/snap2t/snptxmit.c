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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/snap2t/RCS/snptxmit.c,v 1.13 1993/05/04 01:50:02 susan Exp $";
#endif

#ifdef _IBMR2
#ifndef _BSD
#define _BSD 43
#endif
#endif

#include <stdio.h>
#include <andrewos.h> /* sys/types.h sys/time.h */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>


#include <snap.h>
#define EXTERN
#include <snaptcom.h>

#define ACK_TIMEOUT 120000  /* microseconds */

/*
 *  SNAP_Ptr formats pointers appropriately based on machine type
 *  and memory model.  2 static return string areas are provided to
 *  permit up to 2 uses of this function per printf.
 *
 */

char *SNAP_Ptr (len,x)
int len;
char *x;
{
    static char outstr1[15], outstr2[15];
    static int sw = 0;
    char *outstr;
    if (sw) {
	sw = 0;
	outstr = outstr2;
    }
    else {
	sw = 1;
	outstr = outstr1;
    }
#ifdef IBMPC
    if (len == 2)
	sprintf (outstr,"0x%04x", x);
    else
	sprintf (outstr,"0x%04x:0x%04x", (*((unsigned *)&(x) + 1)),
		 (*((unsigned *)&(x))));
#else /* IBMPC */
    sprintf (outstr,"0x%08x", x);
#endif /* IBMPC */
    return (outstr);
}


/*
  * ChecksumAndEncrypt computes the 24-bit checksum and encrypts
  * a message (packet) with one pass over the data.  The checksum
  * is inserted directly into the packet
  */

PRIVATE ChecksumAndEncrypt (msg, len, key)
char *msg, *key;
int len;
{
    SNAP_integer sum;
    int i,j;
    char temp;

    temp = *msg;

    if (key == NULL) {
	SNAP_AppendIntToMsg (msg, (SNAP_integer) 0);
	*msg = temp;
	return;
    }

    sum = msg[0] & 0x0FF;	    /* Note that SNAP header flag byte	 */
    /* is not encrypted, but is included */
    for (i=4, j=3; i<len; i++) {    /* in the checksum. 		 */
	sum += msg[i] & 0x0FF;
	msg[i] ^= key[j++];
	if (j >= SNAP_KEYLEN)
	    j = 0;
    }
    sum &= 0x0FFFFFF;
    DBGMSG (MSG,("ChecksumAndEncrypt - checksum: %ld", sum));

    SNAP_AppendIntToMsg (msg, sum);
    msg[0]  = temp;
    msg[1] ^= key[0];
    msg[2] ^= key[1];
    msg[3] ^= key[2];
}


/*
  * DecryptAndChecksum decrypts the message (packet) and computes
  * the 24-bit checksum with one pass over the data
  *
  */

PRIVATE SNAP_integer DecryptAndChecksum (msg, len, key)
char *msg, *key;
int len;
{
    SNAP_integer sum;
    int i,j;

    if (key == NULL)
	return ((SNAP_integer) 0);

    msg[1] ^= key[0];
    msg[2] ^= key[1];
    msg[3] ^= key[2];

    sum = msg[0] & 0x0FF;	    /* Note that SNAP header flag byte	 */
    /* is not decrypted, but is included */
    for (i=4, j=3; i<len; i++) {    /* in the checksum. 		 */
	msg[i] ^= key[j++];
	if (j >= SNAP_KEYLEN)
	    j = 0;
	sum += msg[i] & 0x0FF;
    }
    sum &= 0x0FFFFFF;
    DBGMSG (MSG,("DecryptAndChecksum - checksum result: %ld", sum));
    return (sum);
}


/*
  *  Build a standard SNAP header for an outgoing message.
      *
      *  The SNAP standard header has the following format:
      *
      *	SNAP_integer	flags + 24-bit checksum;
  *	SNAP_integer	convid;
  *	SNAP_integer	opcode;
  *	SNAP_integer	seqno;
  *
*  The flags/checksum field is inserted during SendFrag processing
*
*/

char *SNAP_BuildStandardHdr (buf, op, convid, lastseqno)
char *buf;
int op, convid;
SNAP_integer lastseqno;
{
    char *sh;

    /*
      * The SNAP standard header has the following format:
      *
      *	    SNAP_integer    flags + 24-bit checksum;
      *	    SNAP_integer    convid;
      *	    SNAP_integer    opcode;
      *	    SNAP_integer    seqno;
      */

    DBGMSG (SND,("BuildStandardHdr - op: %d, convid: %d, seqno: %ld",
		  op, convid, lastseqno));

    sh = SNAP_AppendIntToMsg (buf, (SNAP_integer) 0);
    sh = SNAP_AppendIntToMsg (sh,  (SNAP_integer) convid);
    sh = SNAP_AppendIntToMsg (sh,  (SNAP_integer) op);
    sh = SNAP_AppendIntToMsg (sh,  lastseqno);
    return (sh);
}


/*
  *  ConstructFrag builds a message fragment given a SNAP header, a
  *  pointer to the complete user message, and a fragment number.
  *
  *  The format of a message fragment is:
  *
  *	SNAP standard header;
  *
*	SNAP_integer	abuttal of lastfrag # and frag #
*	SNAP_integer	length of subsequent data portion
*	Unstructured	Arbitrary user data
*
*/

PRIVATE int ConstructFrag (buffer, hdr, msg, msglen, frag, fragsize)
int msglen, frag, fragsize;
FARCHAR *msg;
char *hdr, *buffer;
{
    SNAP_integer fragword;
    char *buf2;
    int lastfrag, fragstart, fragend, i, datalen;

    DBGMSG (SND,("ConstructFrag - buffer at %s, hdr at %s",
		  PTR(buffer), PTR(hdr)));
    DBGMSG (SND,("ConstructFrag - msg at %s, msglen: %d", PTR(msg),
		  msglen));
    DBGMSG (SND,("ConstructFrag - frag: %d, fragsize: %d", frag, fragsize));

    lastfrag = (msglen+fragsize-1) / fragsize;
    if (lastfrag == 0)		    /* to allow for 0-length msgs */
	lastfrag = 1;

    if (frag < 1 || frag > lastfrag) {
	DBGMSG (SND,("ConstructFrag - invalid frag: %d", frag));
	return 0;
    }

    fragstart = (frag-1)*fragsize;
    fragend   = fragstart+fragsize-1;
    if (fragend > msglen-1)
	fragend = msglen-1;
    if (fragend < fragstart)
	datalen = 0;
    else
	datalen = fragend - fragstart + 1;

    DBGMSG (SND,("ConstructFrag - start: %d end: %d datalen: %d",
		  fragstart, fragend, datalen));
    DBGMSG (SND,("ConstructFrag - %d of %d", frag, lastfrag));

    bcopy (hdr, buffer, SNAP_HDRLENGTH);
    fragword = (((SNAP_integer) lastfrag) << (BITSPERBYTE*2)) + frag;
    buf2 = SNAP_AppendIntToMsg (buffer+SNAP_HDRLENGTH, fragword);
    buf2 = SNAP_AppendIntToMsg (buf2, (SNAP_integer) datalen);
    if (datalen == 0)
	return (SNAP_HDRLENGTH + FRAG_HDRLENGTH);

    /* For portability to PC environment don't use library routines
* to copy the user's data.
      */

    for (i=fragstart; i<=fragend; i++)
	*(buf2++) = msg[i];

    return (datalen + SNAP_HDRLENGTH + FRAG_HDRLENGTH);
}


/*
  *  SendFrag constructs and transmits a specified message fragment.
  *  The fragment is checksum'ed and then encrypted (if requested).
*  The flag byte (the 1st byte of the SNAP header), although always
*  passed in the clear, is part of the checksum, which is itself
*  encrypted (hence the flag byte cannot be altered maliciously).
*
*/

int SNAP_SendFrag
(socket, msg, msglen, target_na, encryptopt,
  key, hdr, buffer, frag, fragsize)
int socket, msglen, encryptopt, frag, fragsize;
FARCHAR *msg;
char *key, *hdr, *buffer;
NETADDR *target_na;
{
    int rc, fraglen;

    fraglen = ConstructFrag (buffer, hdr, msg, msglen, frag, fragsize);

    DBGMSG (SND,("SendFrag - fraglen: %d", fraglen));

    if (fraglen == 0)
	return (SNAP_BADPARMS);

    if (encryptopt == SNAP_ENCRYPT)
	*buffer |= ENCRYPTED_MSG;
    else
	key = NULL;

#ifdef	DEBUG
    {
	int i;
	DBGMSG (SND+NCR,("SendFrag - unencrypted packet: "));
	for (i=0; (i<fraglen && i<200); i++) {
	    DBGMSG (SND+NCR+NPR,("%02x", buffer[i]));
	    if ((i % 10) == 0)
		DBGMSG (SND+NPR,(" "));
	}
	DBGMSG (SND+NPR,(" "));
    }
#endif /* DEBUG */

    ChecksumAndEncrypt (buffer, fraglen, key);

    /* Now send the fragment */

    DBGMSG (SND,("SendFrag - now sending frag at %s, len: %d",
		  PTR(buffer), fraglen));
    DBGMSG (SND,
	     ("SendFrag - to: %04x %04x %016x",
	      target_na->sin_family, target_na->sin_port,
	      target_na->sin_addr.s_addr));

    if ((rc = sendto
	  (socket, buffer, fraglen, 0, target_na, sizeof(NETADDR)))
	 == -1) {
	DBGMSG (SND,("SendFrag - error from sendto: %d, frag: %d",
		     errno, frag));
	rc = SYSTEMERROR (errno);
    }
    else {
	if (rc == fraglen) {
	    DBGMSG (SND,("SendFrag - success, frag: %d", frag));
	    rc = SNAP_SUCCESS;
	}
	else {
	    DBGMSG (SND,("SendFrag - wrong count from sendto: %d", rc));
	    rc = SNAP_XMITFAIL;
	}
    }

    return (rc);
}


/*
  *  SNAP_SendMsg transmits a complete message (which may be composed
						 *  of multiple fragments).  Fragments are sent within an ack
  *  window.  When a window is completely acked no further
  *  retransmission of its fragments is necessary.  If a window
  *  is only partially acked, or no ack arrives, its fragments
  *  are retransmitted subject to the quittime parameter.  If the
  *  msg opcode is SNAP_SENDWITHREPLY then no ack is expected for
      *  the final window (the eventual reply will serve as a positive
			    *  ack for the entire window, or the lack of a reply will cause
				*  the entire last window to be retransmitted).
	*
	*  Each fragment of a message carries a complete SNAP header,
	*  but the sequence number is the same for each fragment in
	    *  the message.
	    *
	    *  Since the sending mechanism should always be unimpeded, any
	    *  send error that persists will abort the SNAP_SendMsg operation.
	    *
	    *  Here's a breakdown of all of the arguments to SNAP_SendMsg:
  *
  *	socket	    The socket number to use for access to the network.
      *	msg	    The user's data.
	    *	msglen	    The length of the user's data.
      *	target	    The destination network address.
      *	timeout     The timeout interval in seconds.
      *	quittime    The time to cease and desist all attempts to send
      *	cropt	    The encryption option.
      *	key	    The encryption key for this message.
	  *	hdr	    The SNAP standard header.
	  *	buf	    The area used to construct fragments & receive acks.
	  *	bsize	    The size of buf in bytes.
	  *	f1	    The number of the first fragment to send (normally 1).
	    *	fsize	    The largest fragment size.
	    *	wsize	    The largest window size.
	    *	ackedfrags  The area used to handle a window's worth of acks.
	    *	forceack    The indicator to suppress the ack optimizations.
	    *	qflag	    The indicator that controls whether unexpected packets
	    *			will be queued for later receive processing.
		*	notify	    The procedure that handles notify packets
		*
		*  Return code as follows:
		*
		*	(negative)  Error code
		  *	(positive)  Number of last fragment of last acked window
		  *
		  *  Note that single fragment SNAP_REPLYs appear as acknowledged, but
		  *  no ack is in fact obtained in this case, since the server keeps
		      *  them until the next request for the same conversation is received
			  *  and will simply retransmit them if necessary.
			      *
			      */

int SNAP_SendMsg
(socket, msg, msglen, target, timeout, quittime,
  cropt, key, hdr, buf, bsize, f1, fsize, wsize, ackedfrags,
  forceack, qflag, notify)
int socket, msglen, timeout, cropt, bsize, f1, fsize, wsize, forceack, qflag;
long quittime;
FARCHAR *msg;
char *key, *hdr, *buf, *ackedfrags;
NETADDR *target;
int (*notify)();
{
    int frag, frags_sent_this_window, remaining_bytes, rc, acknowledged;
    int i, finished_frags, remaining_at_start_of_window, lastfrag;
    int first_time, replyack;
    SNAP_integer convid, seqno, op;
    char *p;

    DBGMSG (SND,("SNAP_SendMsg - msglen: %d", msglen));

    lastfrag = (msglen+fsize-1) / fsize;
    if (lastfrag == 0)
	lastfrag = 1;
    if (f1 > lastfrag)
	f1 = lastfrag;

    f1--;   /* Make 0-relative */

    remaining_bytes = msglen - (fsize*f1);
    finished_frags = f1;

    if (forceack)
	*hdr |= MANDATORY_ACK;

    p = SNAP_ExtractIntFromMsg (hdr + sizeof(SNAP_integer), &convid);
    p = SNAP_ExtractIntFromMsg (p, &op);
    SNAP_ExtractIntFromMsg (p, &seqno);

    first_time = TRUE;	    /* Allow zero-length msgs to be sent! */

    while ((remaining_bytes > 0) || first_time) {

	for (i=0; i<wsize; i++)
	    ackedfrags[i] = FRAG_NOT_RECEIVED;
	acknowledged = FALSE;
	remaining_at_start_of_window = remaining_bytes;

	while (!acknowledged) {
	    remaining_bytes = remaining_at_start_of_window;
	    frags_sent_this_window = 0;

	    for (i=1;
		 (first_time || ((remaining_bytes > 0) && (i <= wsize))); i++) {

		first_time = FALSE;

		if (ackedfrags[i-1] == FRAG_NOT_RECEIVED) {
		    frag = finished_frags + i;
		    if ((rc = SNAP_SendFrag (
					     socket, msg, msglen, target, cropt,
					     key, hdr, buf, frag, fsize))
			!= SNAP_SUCCESS)
			return (rc);
		}

		frags_sent_this_window++;
		remaining_bytes -= fsize;
	    }

	    /* Only SendWithReply and Reply are acknowledged messages
	     */

	    if ((op == SNAP_SENDWITHREPLY) || (op == SNAP_REPLY)) {

		/* Special optimization: no ack needed for last window
		    * of SendWithReply
		    */

		replyack = FALSE;
		if ((op == SNAP_SENDWITHREPLY) &&
		    (frags_sent_this_window + finished_frags == lastfrag)) {
		    if (!forceack) {
			DBGMSG (SND,
				("SendMsg - No ack needed for last window"));
			DBGMSG (SND,("SendMsg - finished frags: %d",
				     finished_frags));
			return (finished_frags);
		    }
		    else
			replyack = TRUE;
		}

		/* Another special case - single fragment replies
		    * do not require any acknowledgment since they will
			  * simply be retransmitted if necessary
			      */

		if (op == SNAP_REPLY && lastfrag == 1)
		    acknowledged = TRUE;
		else {
		    acknowledged = WaitForAck (socket, key, buf, bsize,
					       ackedfrags, finished_frags+1, quittime,
					       seqno, target, (int) convid, qflag, wsize,
					       replyack, notify);

		    if (acknowledged < 0) {
			DBGMSG (SND,
				("SendMsg - error from ack: %d",
				 acknowledged));
			if (acknowledged == SNAP_TIMEOUT ||
			    acknowledged == SNAP_SERVERDIED)
			    return (acknowledged);
			else
			    acknowledged = FALSE;
		    }
		}
	    }
	    else
		acknowledged = TRUE;
	}

	finished_frags += frags_sent_this_window;
    }

    DBGMSG (SND,("SendMsg - finished frags: %d", finished_frags));

    if ((finished_frags == lastfrag) && (qflag))
	SNAP_PurgePacketQueue (target, (int) convid, seqno);

    return (finished_frags);
}


/*
  *  WaitForAck synchronizes a particular conversation by insisting
  *  on the receipt of an ACK packet.  The ACK packet can acknowledge
  *  some or all of the packets in the current window.  Those not
  *  acknowledged must be resent.
  *
  *  The return code from WaitForAck takes on the following
  *  values:
  *
  *    <0 - Receive error or timeout
  *     0 - Some fragments were nacked (consult ackedfrag array)
  *    >0 - All fragments have been acked
  *
  */

PRIVATE int WaitForAck (socket, key, buf, bsize, ackedfrags, firstfrag,
			 quittime, seqno, source, convid, qflag, wsize, replyack, notify)
char *key, *buf, *ackedfrags;
int socket, bsize, firstfrag, convid, qflag, wsize, replyack;
SNAP_integer seqno;
NETADDR *source;
long quittime;
int (*notify)();
{
    int recvlen, frag1, frag2, i, rc;
    SNAP_integer recv_op, recv_seqno, fragword, datalen;
    char *p;

    DBGMSG (ACK,("WaitForAck - seqno: %ld, convid: %d qflag: %d",
		  seqno, convid, qflag));

    while (TRUE) {
	recvlen = SpecificReceive
	  (socket, 0, quittime, WAITFORACK_TRIES, key, buf, bsize,
	   source, convid, 0, qflag, notify);

	if (recvlen < 0) {
	    DBGMSG (ACK,("WaitForAck - lost ack"));
	    return (recvlen);
	}

	p = SNAP_ExtractIntFromMsg (buf+2*sizeof(SNAP_integer), &recv_op);
	p = SNAP_ExtractIntFromMsg (p, &recv_seqno);
	p = SNAP_ExtractIntFromMsg (p, &fragword);
	p = SNAP_ExtractIntFromMsg (p, &datalen);

	frag1 = (int) (fragword & MAXSHORT);	/* 1st fragment acknowledged  */
	frag2 = (int) ((fragword >>(BITSPERBYTE*2))
		       & MAXSHORT);			/* number of frags ack/nacked */

	if (frag2 < frag1) {
	    DBGMSG (ACK,
		    ("WaitForAck - invalid ack, frag1: %d, frag2: %d",
		     frag1, frag2));
	    continue;
	}
	if (seqno != recv_seqno) {
	    if (recv_seqno > seqno) {
		DBGMSG (ACK, ("WaitForAck - higher seqno %ld", recv_seqno));
		DBGMSG (ACK, ("WaitForAck - partner has given up!"));
		SNAP_PurgePacketQueue (source, convid, seqno);
		if (EnqueuePacket (buf, recvlen, source) != SNAP_SUCCESS) {
		    DBGMSG (ACK,("EnqueuePacket failed"));
		}
		return (SNAP_TIMEOUT);
	    }

	    DBGMSG (ACK,("WaitForAck - discarded packet with wrong seqno"));
	    DBGMSG (ACK,("WaitForAck - expected: %ld, rcvd: %ld",
			 seqno, recv_seqno));
	    continue;
	}

	if (recv_op == SNAP_ACK) {

	    DBGMSG (ACK,("WaitForAck - received ack for frags %d thru %d",
			 frag1, frag2));

	    /* If the receiving end never even saw the first packet,
	     * a NACK with a sequence number of zero is accepted.
	     */

	    if ((frag1 == 1) && (recv_seqno == 0)) {
		int badack;
		badack = FALSE;
		for (i=0; i<frag2; i++)
		    if (*(p+i) != FRAG_NOT_RECEIVED) {
			DBGMSG (ACK,
				("WaitForAck - ACK packet with seqno missing"));
			badack = TRUE;
			break;
		    }
		if (badack)
		    continue;
	    }

	    if (firstfrag != frag1) {
		DBGMSG (ACK,("WaitForAck - discarded packet for wrong window"));
		DBGMSG (ACK,("WaitForAck - expected: %d, rcvd: %d",
			     firstfrag, frag1));
		continue;
	    }

	    if ((frag2-frag1+1) != (int) datalen) {
		DBGMSG (ACK,("WaitForAck - discarded inconsistant ack packet"));
		DBGMSG (ACK,("WaitForAck - datalen: %ld, frags acked: %d",
			     datalen, frag2-frag1+1));
		continue;
	    }

	    rc = 1;
	    for (i=0; i<(frag2-frag1+1); i++)
		if ((ackedfrags[i] = *(p++)) == FRAG_NOT_RECEIVED)
		    rc = 0;
	    return (rc);
	}
	else {
	    if ((replyack) && (recv_op == SNAP_REPLY)) {
		if (EnqueuePacket (buf, recvlen, source) == SNAP_SUCCESS) {
		    for (i=0; i<wsize; i++)
			ackedfrags[i] = FRAG_RECEIVED;
		    return (1);
		}
		DBGMSG (ACK,("EnqueuePacket failed"));
	    }

	    DBGMSG (ACK,("WaitForAck - discarded non ack packet"));
	    DBGMSG (ACK,
		    ("WaitForAck - op: %ld, seqno %ld, f1: %d, f2: %d, len: %ld",
		     recv_op, recv_seqno, frag1, frag2, datalen));
	    continue;
	}
    }
}


/*
  *  SNAP_SendAck builds and sends an acknowledgement packet.
  *
  */

SNAP_SendAck (socket, key, buf, convid, seqno, frag, lastfrag,
	       ackedfrags, encryptopt, target)
int socket, convid, frag, lastfrag, encryptopt;
char *key, *buf, *ackedfrags;
SNAP_integer seqno;
NETADDR *target;
{
    int i, acklen;
    char *p;
    SNAP_integer fragword;

    DBGMSG (ACK,("SendAck - frag: %d, lastfrag: %d", frag, lastfrag));

#ifdef DEBUG
    {
	int i, nack = FALSE;
	DBGMSG (ACK+NCR,("Fragstatus: "));
	for (i=0; i<(lastfrag-frag+1); i++) {
	    if (ackedfrags[i] == FRAG_NOT_RECEIVED) {
		DBGMSG (ACK+NPR+NCR,("NACK "));
		nack = TRUE;
	    }
	    else
		DBGMSG (ACK+NPR+NCR,("ACK "));
	}
	DBGMSG (ACK+NPR,(" "));
	if (nack)
	    DBGMSG (ACK,("SendAck - ack is a nack!"));
    }
#endif /* DEBUG */

    fragword = (((SNAP_integer) lastfrag) << (BITSPERBYTE*2)) + frag;
    p = SNAP_BuildStandardHdr (buf, SNAP_ACK, convid, seqno);
    p = SNAP_AppendIntToMsg (p, fragword);
    p = SNAP_AppendIntToMsg (p, (SNAP_integer) (lastfrag-frag+1));
    for (i=0; i<(lastfrag-frag+1); i++)
	*(p+i) = ackedfrags[i];
    acklen = SNAP_HDRLENGTH + FRAG_HDRLENGTH + (lastfrag-frag+1);

    if (encryptopt == SNAP_ENCRYPT)
	*buf |= ENCRYPTED_MSG;
    else
	key = NULL;

    ChecksumAndEncrypt (buf, acklen, key);

    /* Now send the ACK */

    if (sendto (socket, buf, acklen, 0, target, sizeof(NETADDR))
	 == -1) {
	DBGMSG (ACK,("SendAck - error from sendto: %d", errno));
    }
    else {
	DBGMSG (ACK,("SendAck - success"));
    }
}


/*
  *  SNAP_ReceiveMsg obtains a complete logical message and places
  *  it directly into the user's receive buffer.
	    *
	    *  The message is received in fragments which are grouped in
	    *  acknowledgement windows.  A window may be acknowledged partially
	    *  or completely.  Missing fragments for partially acknowledged
		*  windows are retransmitted by the SendMsg protocol.	As an
		*  optimization no acknowledgements are sent for messages that
		    *  consist of a single window with one fragment.
		    *
		    */

int SNAP_ReceiveMsg
(socket, timeout, quittime, recvbuf, rbuflen, key, cropt,
  fragstatus, windowsize, buf, bsize, fsize, source, convid,
  op, seqno, notify)
int socket, timeout, rbuflen, cropt, windowsize;
int bsize, fsize, convid, op;
FARCHAR *recvbuf;
char *key, *fragstatus, *buf;
NETADDR *source;
SNAP_integer seqno;
long quittime;
int (*notify)();
{
    int i, j, recvlen, frag, lastfrag, windowdone, lastwindow;
    int fragcnt, fragsdone, anyfragsrecved, lastfraglen;

    DBGMSG (RCV,("ReceiveMsg - buf at: %s, buflen: %d", PTR(recvbuf),
		  rbuflen));
    DBGMSG (RCV,("ReceiveMsg - fragsize: %d, windowsize: %d",
		  fsize, windowsize));
    DBGMSG (RCV,("ReceiveMsg - seqno: %ld", seqno));

    for (i=0; i<windowsize; i++)
	fragstatus[i] = FRAG_NOT_RECEIVED;
    fragcnt = 0;
    fragsdone = 0;
    frag = 1;
    lastfrag = 1;
    anyfragsrecved = 0;
    lastfraglen = 0;

    while (TRUE) {
	recvlen = ReceiveFrag (socket, timeout, quittime, key, buf, bsize,
			       recvbuf, rbuflen, fsize, &frag, &lastfrag, source, convid, op,
			       &seqno, fragsdone, windowsize, cropt, notify);

	DBGMSG (RCV,("ReceiveMsg - %d from ReceiveFrag", recvlen));
	DBGMSG (RCV,("ReceiveMsg - frag: %d, lastfrag: %d", frag, lastfrag));

	if (recvlen > 0) {
	    fragstatus[(frag-1)%windowsize] = FRAG_RECEIVED;
	    fragcnt++;
	    anyfragsrecved++;

	    if (frag == lastfrag)
		lastfraglen = recvlen;

	    if (frag > (lastfrag - ((lastfrag % windowsize == 0) ?
				    windowsize : lastfrag % windowsize)))
		lastwindow = TRUE;
	    else
		lastwindow = FALSE;

	    DBGMSG (RCV,("ReceiveMsg - lastwindow: %s",
			 (lastwindow ? "TRUE" : "FALSE")));

	    windowdone = TRUE;
	    j = SNAP_WindowSize (lastwindow, lastfrag, windowsize);
	    for (i=0; i<j; i++)
		if (fragstatus[i] == FRAG_NOT_RECEIVED)
		    windowdone = FALSE;

	    DBGMSG (RCV,("ReceiveMsg - windowdone: %s",
			 (windowdone ? "TRUE" : "FALSE")));

	    if ((windowdone) || (fragcnt >= windowsize)) {

		/* As an optimization, no ack is sent for
		    * single fragment messages (which can simply be
						* retransmitted if necessary).
		      */

		if (lastfrag != 1)
		    SNAP_SendAck (socket, key, buf, convid, seqno, fragsdone+1,
				  (lastwindow ? lastfrag : fragsdone+windowsize),
				  fragstatus, cropt, source);
		fragcnt = 0;
		if (windowdone) {
		    if (lastwindow)
			fragsdone = lastfrag;
		    else
			fragsdone += windowsize;
		    for (i=0; i<windowsize; i++)
			fragstatus[i] = FRAG_NOT_RECEIVED;
		}
	    }
	    if (fragsdone == lastfrag)
		return ((fsize*(lastfrag-1))+lastfraglen);
	}
	else {
	    if (recvlen == SNAP_TIMEOUT || recvlen == SNAP_RESEND ||
		recvlen == SNAP_SERVERDIED)
		return (recvlen);
	    if (anyfragsrecved) {
		if (fragsdone+windowsize >= lastfrag)
		    lastwindow = TRUE;
		else
		    lastwindow = FALSE;
		SNAP_SendAck (socket, key, buf, convid, seqno, fragsdone+1,
			      (lastwindow ? lastfrag : fragsdone+windowsize),
			      fragstatus, cropt, source);
		fragcnt = 0;
	    }
	    else
		return (recvlen);
	}
    }
}


/*
  *  ReceiveFrag handles the receipt and validation of a message fragment.
  *
  */

PRIVATE int ReceiveFrag (socket, timeout, quittime, key, buf, bsize,
			  userbuf, usize, fsize, frag, lastfrag, source, convid, op, seqno,
			  fragsdone, wsize, cropt, notify)
int socket, timeout, bsize, usize, fsize, *frag, *lastfrag, convid, op;
int fragsdone, wsize, cropt;
char *key, *buf;
FARCHAR *userbuf;
NETADDR *source;
SNAP_integer *seqno;
long quittime;
int (*notify)();
{
    int recvlen, tfrag, tlastfrag;
    char *rp;
    SNAP_integer recv_op, recv_seqno;
    SNAP_integer fragword, datalen;

    DBGMSG (RCV, ("ReceiveFrag - seqno: %ld", *seqno));

    while (TRUE) {
	if ((recvlen = SpecificReceive (socket, timeout, quittime,
					RECEIVEFRAG_TRIES, key, buf, bsize, source, convid, 0,
					FALSE, notify)) > 0) {

	    DBGMSG (RCV,("ReceiveMsg - %d from SpecificReceive", recvlen));

	    /*
	     * All SNAP message fragments have a header that consists of
	     * the following items:
	     *
	     *	    SNAP_integer    flags + 24-bit checksum;
	     *	    SNAP_integer    convid;
	     *	    SNAP_integer    opcode;
	     *	    SNAP_integer    seqno;
	     *	    SNAP_integer    fragword;
	     *	    SNAP_integer    datalen;
	     */

	    rp = SNAP_ExtractIntFromMsg (buf+2*sizeof(SNAP_integer), &recv_op);
	    DBGMSG (RCV,("ReceiveFrag - opcode: %ld",recv_op));

	    rp = SNAP_ExtractIntFromMsg (rp, &recv_seqno);
	    DBGMSG (RCV,("ReceiveFrag - seqno: %ld", recv_seqno));

	    rp = SNAP_ExtractIntFromMsg (rp, &fragword);

	    rp = SNAP_ExtractIntFromMsg (rp, &datalen);
	    if (datalen + 6*sizeof(SNAP_integer) != recvlen) {
		DBGMSG (RCV,("ReceiveFrag - inconsistant length rejected"));
		continue;
	    }

	    if ((*seqno != 0) && (recv_seqno != *seqno)) {
		DBGMSG (RCV,("ReceiveFrag - seqno mismatch: %ld expected: %ld",
			     recv_seqno, *seqno));
		continue;
	    }

	    if ((op != 0) && (recv_op != (SNAP_integer) op) &&
		(recv_op != (SNAP_integer) SNAP_ACK)) {
		DBGMSG (RCV,("ReceiveFrag - op mismatch: %ld expected: %d",
			     recv_op, op));
		continue;
	    }

	    if ((fragsdone == 0) && (recv_op == (SNAP_integer) SNAP_ACK))
		return (SNAP_RESEND);

	    tfrag = (int) (fragword & MAXSHORT);
	    tlastfrag = (int) (fragword >> (BITSPERBYTE*2));

	    if ((tfrag != tlastfrag) && (datalen != fsize)) {
		DBGMSG (RCV,("ReceiveFrag - short intermediate frag dropped"));
		continue;
	    }

	    if (tfrag <= fragsdone) {
		char goodacks[MAXWINDOW];
		int i;
		DBGMSG (RCV,("ReceiveFrag - dropping duplicate, re-acking"));
		for (i=0; i<wsize; i++)
		    goodacks[i] = FRAG_RECEIVED;
		SNAP_SendAck (socket, key, buf, convid, recv_seqno,
			      fragsdone-wsize+1, fragsdone, goodacks, cropt, source);
		continue;
	    }

	    *frag = tfrag;	    /* Pass back actual frag & */
	    *lastfrag = tlastfrag;  /*	seqno info	       */
	    *seqno = recv_seqno;

	    DepositFrag (userbuf, usize, *frag, fsize, rp, (int) datalen);

	    DBGMSG (RCV,("ReceiveFrag - len: %d",datalen));
	    return (datalen);
	}
	else
	    return (recvlen);	    /* receive error occurred */
    }
}


/*
  *  DepositFrag places a message fragment directly into the user's
  *  message buffer.
  *
  */

PRIVATE DepositFrag (userbuf, usize, frag, fsize, data, datalen)
FARCHAR *userbuf;
int usize, frag, fsize, datalen;
char *data;
{
    int fragstart, fragend, i;

    fragstart = (frag-1)*fsize;
    fragend   = fragstart+datalen-1;
    if (fragend > usize-1)
	fragend = usize-1;
    if (fragend < fragstart)
	return;

    /* For portability to PC environment don't use library routines
  * to copy into the user's buffer.
      */

    for (i=fragstart; i<=fragend; i++)
	userbuf[i] = *(data++);
}

/*
  *  CommonReceive is the lowest level receive facility in SNAP
  *
  *  When invoked it first looks for a packet on an internal
      *  receive queue.  If none are queued it looks for a packet
	  *  from the indicated socket.
	  *
	  *  If it obtains a packet, it decrypts it (if necessary) and validates
	    *  the checksum.  Packets with invalid checksums or too short to
	    *  contain a valid SNAP header are discarded.
	    *
	    */

int SNAP_CommonReceive (socket, timeout, quittime, key, buf, bsize, recv_na)
int socket, timeout, bsize;
long quittime;
unsigned char *key, *buf;
NETADDR *recv_na;
{
    int rfd, recvlen, rc, na_len, skipcrypt;
    struct timeval recv_timeout;
    unsigned char msgflags;
    SNAP_integer recv_chksum, msg_checksum;
    int lasttime = FALSE;

    while (!lasttime) {

	DBGMSG (RCV,("CommonReceive - buf: %s, timeout: %d",
		     PTR(buf), timeout));

	DBGMSG (RCV,("CommonReceive - checking internal queue"));
	recvlen = DequeuePacket (buf, bsize, recv_na);

	/* Now issue "select" unless we already have a packet from
	 * our internal queue.
	 */

	if (recvlen)
	    rc = 1;	/* We have a packet, pretend select succeeded */
	else {
	    rfd = 1 << socket;
	    recv_timeout.tv_sec = TimeInterval (timeout, quittime, &lasttime);
	    if (timeout || lasttime)
		recv_timeout.tv_usec = 0;
	    else
		recv_timeout.tv_usec = ACK_TIMEOUT;


	    if ((recv_timeout.tv_sec <= 0) && (recv_timeout.tv_usec == 0))
		rc = 0;
	    else {
		DBGMSG (RCV,
			("CommonReceive - adjusted timeout: %ld sec %ld usec",
			 recv_timeout.tv_sec, recv_timeout.tv_usec));
		rc = select (BITSPERBYTE*sizeof(int), &rfd, 0, 0,
			     &recv_timeout);
	    }
	}

	if (rc == -1) {
	    DBGMSG (RCV,("CommonReceive - error from select: %d",errno));
	    return SYSTEMERROR(errno);
	}
	else if (rc == 0) {
	    DBGMSG (RCV,("CommonReceive - select timeout"));
	    if (lasttime)
		return SNAP_TIMEOUT;
	    else
		return SNAP_SELECTFAILED;
	}
	else {
	    DBGMSG (RCV,("CommonReceive - successful select or dequeue"));

	    na_len = sizeof(NETADDR);

	    skipcrypt = FALSE;
	    if (recvlen) {
		DBGMSG (RCV,("CommonReceive - accepting packet from queue"));
		skipcrypt = TRUE;
	    }
	    else {
		DBGMSG (RCV,("CommonReceive, recvfrom - buf: %s", PTR(buf)));
		if ((recvlen = recvfrom (socket, buf, bsize, 0,
					 recv_na, &na_len)) == -1)
		{
		    DBGMSG (RCV,
			    ("CommonReceive - error from recvfrom: %d",errno));
		    return SYSTEMERROR(errno);
		}
	    }

	    if (na_len != sizeof(NETADDR)) {
		DBGMSG (2,("CommonReceive - improper netaddr len: %d ",na_len));
		continue;
	    }

	    DBGMSG (RCV,("CommonReceive - received packet"));
	    DBGMSG (RCV,("CommonReceive - length: %d", recvlen));
	    DBGMSG (RCV,
		    ("CommonReceive - address: %04x %04x %016x",
		     recv_na->sin_family, recv_na->sin_port,
		     recv_na->sin_addr.s_addr));
	    DBGMSG (RCV,("CommonReceive - buf: %s", PTR(buf)));

#ifdef	DEBUG
	    {
		int i;
		DBGMSG (RCV+NCR,("CommonReceive - packet: "));
		for (i=0; (i<recvlen && i<200); i++) {
		    DBGMSG (RCV+NCR+NPR,("%02x", buf[i]));
		}
		DBGMSG (RCV+NPR,(" "));
	    }
#endif /* DEBUG */

	    if (recvlen < 6*sizeof(SNAP_integer)) {
		DBGMSG (RCV,("CommonReceive - short recv rejected"));
		continue;
	    }

	    /*
	     * All SNAP messages start with a SNAP_integer that
	     * contains 8 bits of flags and a 24-bit checksum.
	     *
	     */

	    if ((skipcrypt) || (!(*buf & ENCRYPTED_MSG)))
		msg_checksum = 0;
	    else
		msg_checksum = DecryptAndChecksum ((char *)buf, recvlen, (char *)key);

#ifdef	DEBUG
	    {
		int i;
		DBGMSG (RCV+NCR,("CommonReceive - decrypted packet: "));
		for (i=0; (i<recvlen && i<200); i++) {
		    DBGMSG (RCV+NCR+NPR,("%02x", buf[i]));
		}
		DBGMSG (RCV+NPR,(" "));
	    }
#endif /* DEBUG */
	    msgflags = *buf;
	    *buf = 0; /* remove flags from checksum word for Extract */
	    SNAP_ExtractIntFromMsg (buf, &recv_chksum);
	    DBGMSG (RCV,("CommonReceive - checksum: %ld", recv_chksum));
	    *buf = msgflags;

	    if ((msg_checksum) && (recv_chksum != msg_checksum)) {
		DBGMSG (RCV,("CommonReceive - checksum rejected"));
		continue;
	    }

	    /* Restore checksum in case this packet must be queued */

	    SNAP_AppendIntToMsg (buf, recv_chksum);
	    *buf = msgflags;
	    return (recvlen);		/* successful receive */
	}
    }

    return (SNAP_TIMEOUT);
}


/*
  *  SpecificReceive waits for a packet from a specific network address
      *  with a designated conversation id.	If a packet is received from
      *  any other source, the argument "qflag" determines whether or not
      *  to queue the packet for later receive processing, or simply drop it.
	  *
	  */

PRIVATE int SpecificReceive
(socket, timeout, quittime, tries, key, buf, bsize, source, convid,
  op, qflag, NotifyHandler)
int socket, timeout, bsize, op, qflag;
long quittime;
int tries, convid;
char *key, *buf;
NETADDR *source;
int (*NotifyHandler)();
{
    int recvlen, reject, local_qflag, rc;
    NETADDR recv_na;
    SNAP_integer recv_conv, recv_op;
    char *b;

    DBGMSG (RCV, ("SpecificReceive"));

    /* If SpecificReceive will be placing unwanted packets into the
      * internal queue, a special marker element is first inserted to
      * guarantee we will not traverse the internal queue forever.
      */

    if (qflag)
	ForceSelectMarker ();

    while (tries--) {
	local_qflag = qflag;
	recvlen = SNAP_CommonReceive
	  (socket, timeout, quittime, (unsigned char *)key, (unsigned char *)buf, bsize, &recv_na);

	DBGMSG (RCV,("SpecificReceive - packet len: %d", recvlen));

	if (recvlen < 0) {
	    if (qflag)
		PurgeForceMarker ();
	    return (recvlen);
	}

	b = SNAP_ExtractIntFromMsg (buf + sizeof (SNAP_integer), &recv_conv);
	SNAP_ExtractIntFromMsg (b, &recv_op);
	reject = FALSE;

	rc = SNAP_SUCCESS;
	if (recv_op == SNAP_NOTIFY) {
	    DBGMSG (RCV,("SpecificReceive - notify packet"));
	    rc = (*NotifyHandler) (buf, convid, &recv_na);
	    reject = TRUE;
	    local_qflag = FALSE;
	    if (rc == SNAP_SERVERDIED) {
		if (qflag)
		    PurgeForceMarker ();
		return (rc);
	    }
	}
	else if (op != 0)
	    reject = ((SNAP_integer) op == recv_op ? FALSE : TRUE);

	if ((reject) || (recv_conv != (SNAP_integer) convid) ||
	    (bcmp (&recv_na, source, sizeof(NETADDR)) != 0)) {
	    if (local_qflag) {
		if (EnqueuePacket (buf, recvlen, &recv_na) != SNAP_SUCCESS)
		{
		    DBGMSG (RCV,
			    ("SpecificReceive - storage shortage, packet dropped"));
		}
	    }
	    else {
		DBGMSG (RCV,("SpecificReceive - packet dropped"));
	    }

	    /* Don't count the foreign fragment as a failed try */

	    tries++;
	}
	else {
	    DBGMSG (RCV,("SpecificReceive - found qualifying packet"));
	    if (qflag)
		PurgeForceMarker ();
	    return (recvlen);
	}
    }

    DBGMSG (RCV,("SpecificReceive - retries exhausted"));

    if (qflag)
	PurgeForceMarker ();
    return (SNAP_NOMORERETRIES);
}


typedef struct qe {
    struct qe *nextqe;
    NETADDR qa;
    int size;
} QUEUE_ELEMENT;

PRIVATE QUEUE_ELEMENT ForceSelect;

PRIVATE QUEUE_ELEMENT *q_first = NULL, *q_last = NULL;


PRIVATE QUEUE_ELEMENT *FindQel (na, convid, seqno, hdr, startq)
NETADDR *na;
int convid;
SNAP_integer seqno;
char *hdr;
QUEUE_ELEMENT *startq;
{
    QUEUE_ELEMENT *q;
    char *c;
    SNAP_integer tconvid, tseqno;

    for (q=startq; q != NULL; q=q->nextqe) {
	if (bcmp (na, &q->qa, sizeof(NETADDR)) != 0)
	    continue;
	c = (char *) q;
	if (hdr != NULL) {
	    if (bcmp (hdr+1, c+sizeof(QUEUE_ELEMENT)+1,
		      SNAP_HDRLENGTH+FRAG_HDRLENGTH-1) == 0)
		return (q);
	    else
		continue;
	}
	c = SNAP_ExtractIntFromMsg
	  (c+sizeof(QUEUE_ELEMENT)+sizeof(SNAP_integer), &tconvid);
	SNAP_ExtractIntFromMsg (c+sizeof(SNAP_integer), &tseqno);
	if ((tconvid != (SNAP_integer) convid) || (tseqno != seqno))
	    continue;
	return (q);
    }
    return (q);
}


/*
  *  DequeuePacket obtains a previously received packet from an
  *  internal queue.  Packets may be placed on the internal queue
  *  using EnqueuePacket.  It recognizes a zero-length packet as
  *  a special marker that is not free'd.
	     *
	     *  Returns the length of the obtained packet (0 means no packet available)
	     *
	     */

PRIVATE int DequeuePacket (buf, bufsize, recv_na)
char *buf;
int bufsize;
NETADDR *recv_na;
{
    QUEUE_ELEMENT *q;
    char *pp;
    int rsize;

    DBGMSG (PKQ,("DequeuePacket"));
    DBGMSG (PKQ,("Addr: %s, size: %d", PTR(buf), bufsize));

    if (q_first == NULL) {
	DBGMSG (PKQ,("DequeuePacket - q empty"));
	return (0);
    }

    q = q_first;
    q_first = q->nextqe;
    if (q_first == NULL)
	q_last = NULL;

    if (recv_na != NULL)
	bcopy (&q->qa, recv_na, sizeof(NETADDR));
    rsize = q->size > bufsize ? bufsize : q->size;
    if (rsize) {
	pp = (char *) q;
	bcopy (pp + sizeof(QUEUE_ELEMENT), buf, rsize);
	free (q);
    }
    else {

	/* Make certain that the "force select" marker stays on
	 * the internal queue until explicitly removed
	 */

	ForceSelectMarker ();
    }

    DBGMSG (PKQ,("DequeuePacket - found one, size: %d", rsize));
    DBGMSG (PKQ,("DequeuePacket - queue element at %s", PTR(q)));

    return (rsize);
}


/*
  *  EnqueuePacket places an already obtained packet onto an internal
  *  queue from which it may be retrieved using DequeuePacket.
  *  It recognizes a zero-length packet as a special non-malloc'ed marker.
  *
  *  Returns SNAP_FAIL (no more storage) or SNAP_SUCCESS
  *
  */

PRIVATE int EnqueuePacket (packet, size, na)
char *packet;
int size;
NETADDR *na;
{
    QUEUE_ELEMENT *p;
    char *pp;

    DBGMSG (PKQ,("EnqueuePacket"));
    DBGMSG (PKQ,("Addr: %s, size: %d", PTR(packet), size));
    if (size) {
	if (FindQel (na, 0, (SNAP_integer) 0, packet, q_first) != NULL) {
	    DBGMSG (PKQ,("EnqueuePacket - dropping duplicate"));
	    return (SNAP_SUCCESS);
	}
	if ((p = (QUEUE_ELEMENT *) malloc (size + sizeof (QUEUE_ELEMENT)))
	    == NULL)
	    return (SNAP_FAIL);
    }
    else
	p = &ForceSelect;

    p->size = size;

    if (na != NULL)
	bcopy (na, &p->qa, sizeof(NETADDR));

    if (size) {
	pp = (char *) p;
	bcopy (packet, pp + sizeof(QUEUE_ELEMENT), size);
    }
    p->nextqe = NULL;

    if (q_last != NULL) {
	q_last->nextqe = p;
	q_last = p;
    }
    else {
	q_last = p;
	q_first = p;
    }

    DBGMSG (PKQ,("EnqueuePacket - new queue element at %s", PTR(p)));
    return (SNAP_SUCCESS);
}


/*
  *  ForceSelectMarker places a special zero-length packet entry onto the
  *  internal queue to serve as a "force select" signal to CommonReceive.
  *  Since no malloc is involved in this operation, it cannot fail.
  *
  */

PRIVATE ForceSelectMarker ()
{
    EnqueuePacket ((char *) NULL, 0, (NETADDR *) NULL);
    DBGMSG (PKQ,("ForceSelect marker inserted"));
}


/*
  *  PurgeForceMarker locates and removes the special marker element
  *  that means "force select" from the internal queue.
  *
  */

PRIVATE PurgeForceMarker ()
{
    QUEUE_ELEMENT *q;

    DBGMSG (PKQ,("Purge ForceSelect marker"));
    for (q=q_first; q != NULL; q=q->nextqe) {
	if (q->size == 0) {
	    RemoveQel (q);
	    DBGMSG (PKQ,("ForceSelect marker purged"));
	    return;
	}
    }
    DBGMSG (PKQ,("ForceSelect marker not found"));
}


/*
  *  RemoveQel locates, disconnects, and frees a specified queue element
  *
  */

PRIVATE RemoveQel (r)
QUEUE_ELEMENT *r;
{
    QUEUE_ELEMENT *q, *priorq;

    DBGMSG (PKQ,("RemoveQel - element at %s", PTR(r)));
    for (priorq=NULL, q=q_first; q != NULL; priorq=q, q=q->nextqe) {
	if (q == r) {
	    if (priorq != NULL)
		priorq->nextqe = q->nextqe;
	    else {
		q_first = q->nextqe;
	    }
	    if ((q == q_last) || (q_first == NULL))
		q_last = q_first;
	    if (q->size != 0)	/* No free for "force select marker" */
		free(q);
	    DBGMSG (PKQ,("RemoveQel succeeded"));
	    return;
	}
    }
    DBGMSG (PKQ,("RemoveQel failed"));
}


/*
  *  PurgePacketQueue removes all packets from the internal queue that
  *  has the specified network address, conversation id, and sequence number.
  *
  */

SNAP_PurgePacketQueue (na, convid, seqno)
NETADDR *na;
int convid;
SNAP_integer seqno;
{
    QUEUE_ELEMENT *q, *start;

    DBGMSG (PKQ,("PurgePacketQueue - cid: %d, seqno: %ld", convid, seqno));
    start = q_first;
    while ((q = FindQel (na, convid, seqno, NULL, start)) != NULL) {
	start = q->nextqe;
	RemoveQel (q);
    }
}


long time();


/*
  *  Compute a timeout interval that will not exceed the
  *  user-specified SNAP request time.
  */

PRIVATE int TimeInterval (timeout, quittime, lastflag)
int timeout;
long quittime;
int *lastflag;
{
    int temp;
    if ((temp = quittime - time(NULL)) < timeout) {
	*lastflag = TRUE;
	return (temp);
    }
    *lastflag = FALSE;
    return (timeout);
}


/*
  *  SNAP_WindowSize returns the number of fragments in the current window.
  *
  */

int SNAP_WindowSize (lastwindow, lastfrag, wsize)
int lastwindow, lastfrag, wsize;
{
    int i;
    i = ((lastwindow) ? (lastfrag % wsize) : wsize);
    if (i==0)
	return (wsize);
    return (i);
}
