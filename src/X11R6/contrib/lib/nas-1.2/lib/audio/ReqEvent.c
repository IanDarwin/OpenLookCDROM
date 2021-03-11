/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)ReqEvent.c,v 1.2 1993/01/13 21:19:08 jim Exp $
 */

/* Portions derived from */
/* Copyright 	Massachusetts Institute of Technology  1986 */
/* $XConsortium: XPutBEvent.c,v 11.11 91/01/06 11:47:23 rws Exp $ */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include "Alibint.h"

AuBool AuRequeueEvent (aud, event, skip)
    register AuServer *aud;
    register AuEvent *event;
    register int skip;
{
    register _AuQEvent *qelt;
    register _AuQEvent *prev;

    _AuLockServer (aud);
    if (!aud->qfree) {
	if (!(aud->qfree = (_AuQEvent *) Aumalloc (sizeof (_AuQEvent)))) {
	    _AuUnlockServer (aud);
	    return AuFalse;
	}
	aud->qfree->next = (_AuQEvent *) NULL;
    }

    qelt = aud->qfree;			/* get an event to use */
    aud->qfree = qelt->next;

    qelt->event = *event;

    if (skip <= 0) {			/* on head */
	prev = NULL;
    } else if (skip >= aud->qlen) {	/* on tail (could be empty head too) */
	prev = aud->tail;
    } else {				/* walk the queue to find position */
	prev = aud->head;
	while (--skip > 0)
	    prev = prev->next;
    }

    if (prev) {				/* not head */
	qelt->next = prev->next;
	prev->next = qelt;
    } else {				/* is head */
	qelt->next = aud->head;
	aud->head = qelt;
    }

    if (prev == aud->tail)		/* see if is new tail */
	aud->tail = qelt;

    aud->qlen++;
    _AuUnlockServer (aud);

    return AuTrue;
}
