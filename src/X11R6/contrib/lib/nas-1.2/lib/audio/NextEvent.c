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
 * $NCDId: @(#)NextEvent.c,v 1.4 1993/01/13 21:19:04 jim Exp $
 */

/* Portions derived from */
/* $XConsortium: XNextEvent.c,v 11.16 91/01/06 11:47:06 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

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

/* 
 * Return next event in queue, or if none, flush output and wait for
 * events.
 */

void
AuNextEvent (aud, dequeue, event)
	register AuServer *aud;
	AuBool dequeue;
	register AuEvent *event;
{
	register _AuQEvent *qelt;
	
	_AuLockServer(aud);
	
	if (aud->head == NULL)		/* block until event arrives */
	    _AuReadEvents(aud);
	qelt = aud->head;
	*event = qelt->event;

	if (dequeue) {
	    /* move the head of the queue to the free list */
	    if ((aud->head = qelt->next) == NULL)
		aud->tail = NULL;
	    qelt->next = aud->qfree;
	    aud->qfree = qelt;
	    aud->qlen--;
	}
	_AuUnlockServer(aud);
	return;
}

