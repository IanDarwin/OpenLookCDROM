/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)HandleEv.c,v 1.9 1993/08/11 17:01:58 greg Exp $
 */

/*
 * This file contains utilities contains an event handler takes care of
 * dispatching the various events that might come into an application. The
 * idea is to hide most of the mainline activity so that simple applications
 * don't have to fuss with things.
 * 
 * For example, an application that wants to play a sound from a file or memory
 * buffer asynchronously needs to wait for events telling it when it can
 * write to the ImportElement in the flow.
 */

#include "Alibint.h"

void
AuHandleEvents(aud)
AuServer       *aud;
{
    int             nevents;
    AuEvent         event;

    while (1)
    {
	/* do our best to find an event */
	nevents = _AuEventsQueued(aud, AuEventsQueuedAlready);

	if (!nevents)
	{
	    nevents = _AuEventsQueued(aud, AuEventsQueuedAfterFlush);

	    if (!nevents)
	    {
		nevents = _AuEventsQueued(aud, AuEventsQueuedAfterReading);

		if (!nevents)
		    return;
	    }
	}

	/* got one, now do something with it */
	for (; nevents > 0; nevents--)
	{
	    AuNextEvent(aud, AuTrue, &event);	/* dequeue the event */
	    (void) AuDispatchEvent(aud, &event);	/* go deal with it */
	}
    }
}

AuBool
AuDispatchEvent(aud, event)
AuServer       *aud;
AuEvent        *event;
{
    AuBool          result = AuFalse;
    AuEventHandlerRec *handler = NULL, *next;

    do
    {
	if ((handler = AuLookupEventHandler(aud, event, handler)) 
        					!= (AuEventHandlerRec *) NULL)
	{
            next = handler->next;	/* could be removed by handler */
	    result |= (*(handler->callback)) (aud, event, handler);
	    handler = next;
	}
    } while (handler);

    return result;
}

AuEventHandlerRec *
AuLookupEventHandler(aud, event, handler)
register AuServer *aud;
register AuEvent *event;
register AuEventHandlerRec *handler;
{
    /*
     * From the given start location (NULL indicates beginning), walk down
     * the handlers list looking for a match.
     */
    if (!handler)
	handler = aud->eventhandlerq;

    /*
     * This could be done more efficiently than a linear lookup, but there
     * are relatively few events in the audio protocol, so the overhead
     * should be acceptable.
     */
    for (; handler; handler = handler->next)
    {
	if ((handler->mask & AuEventHandlerTypeMask) &&
	    (handler->type != event->type))
	    continue;

	if ((handler->mask & AuEventHandlerIDMask) &&
	    (handler->id != event->auany.id))
	    continue;

	break;			/* got one! */
    }

    return handler;
}

AuEventHandlerRec *
AuRegisterEventHandler(aud, mask, type, id, callback, data)
AuServer       *aud;
AuMask          mask;
int             type;
AuID            id;
AuEventHandlerCallback callback;
AuPointer       data;
{
    AuEventHandlerRec *handler;

    if (!(handler = (AuEventHandlerRec *) Aumalloc(sizeof(AuEventHandlerRec))))
	return NULL;

    handler->aud = aud;
    handler->mask = mask;
    handler->type = type;
    handler->id = id;
    handler->callback = callback;
    handler->data = data;

    _AuAddToLinkedList(aud->eventhandlerq, handler);

    return handler;
}

void
AuUnregisterEventHandler(aud, handler)
AuServer       *aud;
AuEventHandlerRec *handler;
{
    _AuRemoveFromLinkedList(aud->eventhandlerq, handler);
    Aufree(handler);
}
