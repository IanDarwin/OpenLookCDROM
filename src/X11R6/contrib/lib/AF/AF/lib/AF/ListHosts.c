/* Copyright    Massachusetts Institute of Technology    1986	*/
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/* This can really be considered an os dependent routine */

#define NEED_REPLIES
#include "Alibint.h"
/*
 * can be freed using XFree.
 */

AFHostAddress *AFListHosts(register AFAudioConn *aud,
			   int *nhosts,	/* RETURN */
			   ABool *enabled) /* RETURN */
{
    register AFHostAddress *outbuf = 0, *op;
    aListHostsReply reply;
    long nbytes;
    unsigned char *buf, *bp;
    register int i;
    register aListHostsReq *req;

    LockConnection(aud);
    aud = aud->connection;
    GetReq(ListHosts, req);

    if (!_AReply (aud, (aReply *) &reply, 0, aFalse)) {
       UnlockConnection(aud);
       SyncHandle();
       return (AFHostAddress *) NULL;
    }

    if (reply.nHosts) {
	nbytes = reply.length << 2;	/* compute number of bytes in reply */
	op = outbuf = (AFHostAddress *)
	    Xmalloc((unsigned)(nbytes + reply.nHosts * sizeof(AFHostAddress)));

	if (! outbuf) {	
	    _AEatData(aud, (unsigned long) nbytes);
	    UnlockConnection(aud);
	    SyncHandle();
	    return (AFHostAddress *) NULL;
	}
	bp = buf = 
	    ((unsigned char  *) outbuf) + reply.nHosts * sizeof(AFHostAddress);

	_ARead (aud, (char *) buf, nbytes);

	for (i = 0; i < reply.nHosts; i++) {
	    op->family = ((aHostEntry *) bp)->family;
	    op->length =((aHostEntry *) bp)->length; 
	    op->address = (char *) (((aHostEntry *) bp) + 1);
	    bp += SIZEOF(aHostEntry) + (((op->length + 3) >> 2) << 2);
	    op++;
	}
    }

    *enabled = reply.enabled;
    *nhosts = reply.nHosts;
    UnlockConnection(aud);
    SyncHandle();
    return (outbuf);
}


    


