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
 * $NCDId: @(#)ScanEvents.c,v 1.5 1993/01/29 20:47:28 lemke Exp $
 */

/* Portions derived from */
/* $XConsortium: XChkIfEv.c,v 11.11 91/01/06 11:44:28 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985, 1987	*/

#include "Alibint.h"

/*
 * AuScanEvents - Walk the queue of events (refilling if necessary) looking
 * for an event that matches a caller-specified predicate.  If one is found,
 * the search stops and the event is optionally dequeued.  The mode argument
 * indicates how far the routine should keep scanning looking for a match:
 * 
 *     AuEventsQueuedAlready		Only those events in the queue are
 * 					are scaned.
 * 
 *     AuEventsQueuedAfterReading	If no match is found from the queue,
 * 					a read is attempted see if there are
 * 					any pending.
 * 
 *     AuEventsQueuedAfterFlush		If no match is found after reading,
 * 					the output buffer is flushed.
 */

AuBool AuScanEvents (aud, mode, dequeue, predicate, arg, event)
    register AuServer *aud;
    int mode;
    AuBool dequeue;
    AuBool (*predicate)(
#if NeedNestedPrototypes
	AuServer *,			/* server */
	AuEvent *,			/* event */
	AuPointer			/* arg */
#endif
        );				/* function to call */
    AuPointer arg;
    register AuEvent *event;		/* AuEvent to be filled in. */
{
    register _AuQEvent *prev, *qelt;
    int n;			/* time through count */

    if (mode > AuEventsQueuedAfterFlush)
	return AuFalse;

    _AuLockServer(aud);
    prev = NULL;
    for (n = AuEventsQueuedAlready; n <= mode; n++) {
	switch (n) {
	  case AuEventsQueuedAfterReading:
	    (void) _AuEventsQueued (aud, AuEventsQueuedAfterReading);
	    break;

	  case AuEventsQueuedAfterFlush:
	    _AuFlush (aud);
	    break;
	}

	for (qelt = prev ? prev->next : aud->head; qelt;
	     prev = qelt, qelt = qelt->next) {
	    if ((*predicate)(aud, &qelt->event, arg)) {	   /* found a match */
		*event = qelt->event;
		if (dequeue) {
		    if (prev) {
			if ((prev->next = qelt->next) == NULL)
			    aud->tail = prev;
		    } else {
			if ((aud->head = qelt->next) == NULL)
			    aud->tail = NULL;
		    }
		    qelt->next = aud->qfree;
		    aud->qfree = qelt;
		    aud->qlen--;
		}
		_AuUnlockServer(aud);
		return AuTrue;
	    }
	}
    }
    _AuUnlockServer(aud);
    return AuFalse;
}
