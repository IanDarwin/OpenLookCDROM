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
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)auevents.c,v 1.9 1994/04/21 21:50:59 greg Exp $
 */

#include	"misc.h"
#include	"dixstruct.h"
#ifdef sgi
#define		_BSD_SIGNALS
#endif
#include 	<sys/signal.h>
#include	<audio/audio.h>
#include	<audio/Aproto.h>
#include	"au.h"

extern void WriteEventsToClient(), AuProcessClockedFlows();

typedef struct _EventQueueRec
{
    auEvent         event;
    AuBool          processClockedFlows;
    FlowPtr         flow;
    ClientPtr       client;
    struct _EventQueueRec *prev,
                   *next;
}               EventQueueRec, *EventQueuePtr;

EventQueuePtr   AuEventQueue;
static EventQueuePtr AuEventQueueTail;

#define AddToEventQueue(e)						       \
{									       \
    if (AuEventQueueTail)						       \
    {									       \
	AuEventQueueTail->next = (e);					       \
	(e)->prev = AuEventQueueTail;					       \
    }									       \
    else								       \
    {									       \
	(e)->prev = NULL;						       \
	AuEventQueue = (e);						       \
    }									       \
									       \
    (e)->next = NULL;							       \
    AuEventQueueTail = (e);						       \
}

static void
WriteAuEventsToClient(pClient, count, events)
ClientPtr       pClient;
int             count;
auEvent        *events;
{
    WriteEventsToClient(pClient, count, events);
}

static void
fillInEvent(kind, reason, el, ev)
int             kind,
                reason;
FlowElementPtr  el;
auEvent        *ev;
{
    AuUint32           numBytes = 0;
    ComponentPtr    c = el->component;

    switch (el->raw->type)
    {
	case AuElementTypeImportClient:
	    numBytes = c->dataSize - c->currentSize - c->incoming;

	    if (el->state != AuStateStop)
		c->incoming += numBytes;
	    break;
	case AuElementTypeImportDevice:
	    break;
	case AuElementTypeImportBucket:
	    break;
	case AuElementTypeImportWaveForm:
	    break;
	case AuElementTypeExportClient:
	    numBytes = c->currentSize - c->outgoing;
	    c->outgoing += numBytes;
	    break;
	case AuElementTypeExportDevice:
	    break;
	case AuElementTypeExportBucket:
	    break;
    }

    UpdateCurrentTimeIf();

    ev->u.u.type = AuEventTypeElementNotify;
    ev->u.u.time = currentTime.milliseconds;

    ev->u.elementNotify.flow = el->flow->flowId;
    ev->u.elementNotify.element_num = el->elementNum;
    ev->u.elementNotify.kind = kind;
    ev->u.elementNotify.reason = reason;
    ev->u.elementNotify.prev_state = el->prevState;
    ev->u.elementNotify.cur_state = el->state;
    ev->u.elementNotify.num_bytes = numBytes;
}

void
ProcessAudioEvents()
{
    AuBlock           l;
    EventQueuePtr   p,
                    next;

    l = AuBlockAudio();
    p = AuEventQueue;
    AuUnBlockAudio(l);

    while (p)
    {
	if (p->processClockedFlows)
	    AuProcessClockedFlows();
	else
	{
	    p->event.u.u.sequenceNumber = p->client->sequence;
	    WriteAuEventsToClient(p->client, 1, &p->event);
	}

	l = AuBlockAudio();
	RemoveFromLinkedList(AuEventQueue, p);

	if (!AuEventQueue)
	    AuEventQueueTail = NULL;

	next = p->next;
	AuUnBlockAudio(l);

	AuProtectedFree(p);
	p = next;
    }
}

void
AuRequestElementNotifyEvent(kind, reason, el)
int             kind,
                reason;
FlowElementPtr  el;
{
    EventQueuePtr   p;

    if (!(p = (EventQueuePtr) AuProtectedMalloc(sizeof(EventQueueRec))))
	return;

    if (kind == AuElementNotifyKindSpecial)
	p->processClockedFlows = AuTrue;
    else
    {
	p->processClockedFlows = AuFalse;
	p->client = (ClientPtr) (el->client);
	p->flow = el->flow;

	fillInEvent(kind, reason, el, &p->event);
    }

    AddToEventQueue(p);
    AuCallbackIf(AuEventPostedCB, ());
    WAKEUP_SERVER();
}

#define MONITOR_EVENT_DATA_SPACE	12

void
AuRequestMonitorNotifyEvent(el, data)
FlowElementPtr  el;
AuUint8          *data;
{
    EventQueuePtr   p;
    auEvent        *ev;
    AuUint32           count,
                    numBytes;
    ComponentPtr    c = el->component;

    numBytes = c->bytesPerSample * 2;
    count = (numBytes + (MONITOR_EVENT_DATA_SPACE - 1)) /
	MONITOR_EVENT_DATA_SPACE;

    while (count--)
    {
	if (!(p = (EventQueuePtr) AuProtectedMalloc(sizeof(EventQueueRec))))
	    return;

	p->processClockedFlows = AuFalse;
	p->client = (ClientPtr) (el->client);
	p->flow = el->flow;

	ev = &p->event;

	UpdateCurrentTimeIf();

	ev->u.u.type = AuEventTypeMonitorNotify;
	ev->u.u.time = currentTime.milliseconds;

	ev->u.monitorNotify.flow = el->flow->flowId;
	ev->u.monitorNotify.element_num = el->elementNum;
	ev->u.monitorNotify.format = c->format;
	ev->u.monitorNotify.num_tracks = c->numTracks;
	ev->u.monitorNotify.count = count;
	ev->u.monitorNotify.num_fields = 2;

	bcopy(data, (char *) &ev->u.monitorNotify.data,
	      MONITOR_EVENT_DATA_SPACE);
	data += MONITOR_EVENT_DATA_SPACE;

	AddToEventQueue(p);
    };

    AuCallbackIf(AuEventPostedCB, ());
    WAKEUP_SERVER();
}

void
AuDequeueEvents(flow)
FlowPtr         flow;
{
    AuBlock           l;
    EventQueuePtr   p,
                    next;

    l = AuBlockAudio();
    p = AuEventQueue;
    AuUnBlockAudio(l);

    while (p)
    {
	l = AuBlockAudio();
	next = p->next;
	AuUnBlockAudio(l);

	if (p->flow == flow)
	{
	    l = AuBlockAudio();

	    RemoveFromLinkedList(AuEventQueue, p);

	    if (!AuEventQueue)
		AuEventQueueTail = NULL;

	    AuUnBlockAudio(l);
	    AuProtectedFree(p);
	}

	p = next;
    }
}
