/* Copyright    Massachusetts Institute of Technology    1985, 1987	*/
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

#define NEED_EVENTS
#include "Alibint.h"

extern _AFQEvent *_afqfree;

/* 
 * Check existing events in queue to find if any match.  If so, return.
 * If not, flush buffer and see if any more events are readable. If one
 * matches, return.  If all else fails, tell the user no events found.
 */

ABool AFCheckIfEvent (
        register AFAudioConn *aud,
	register AFEvent *event		/* XEvent to be filled in. */,
	ABool (*predicate)()		/* function to call */,
	char *arg
	)
{
	register _AFQEvent *prev, *qelt;
	int n;			/* time through count */

        LockConnection(aud);
	aud = aud->connection;
	prev = NULL;
	for (n = 3; --n >= 0;) {
	    for (qelt = prev ? prev->next : aud->head;
		 qelt;
		 prev = qelt, qelt = qelt->next) {
		if ((*predicate)(aud, &qelt->event, arg)) {
		    *event = qelt->event;
		    if (prev) {
			if ((prev->next = qelt->next) == NULL)
			    aud->tail = prev;
		    } else {
			if ((aud->head = qelt->next) == NULL)
			aud->tail = NULL;
		    }
		    qelt->next = _afqfree;
		    _afqfree = qelt;
		    aud->qlen--;
		    UnlockConnection(aud);
		    return ATrue;
		}
	    }
	    switch (n) {
	      case 2:
		_AEventsQueued(aud, AQueuedAfterReading);
		break;
	      case 1:
		_AFlush(aud);
		break;
	    }
	}
	UnlockConnection(aud);
	return AFalse;
}
