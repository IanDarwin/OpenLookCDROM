/************************************************************
Copyright 1987, 1990 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

********************************************************/
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

#include "audio.h"
#include "misc.h"
#include "resource.h"
#include "audioproto.h"
#include "inputstr.h"
#include "audiodev.h"
#include "diastruct.h"

extern void (* EventSwapVector[128]) ();
extern void (* ReplySwapVector[256]) ();

#define EXTENSION_EVENT_BASE  64

#define NoSuchEvent 0x80000000	/* so doesn't match ANoEventMask */
#define AllEventMasks (lastEventMask|(lastEventMask-1))

#ifdef DEBUG
static debug_events = 0;
#endif
InputInfo inputInfo;

void WriteEventsToClient();

extern ABool permitOldBugs;

static AMask lastEventMask;

#define CantBeFiltered ANoEventMask
static AMask filters[128] =
{
	NoSuchEvent,		       /* 0 */
	NoSuchEvent,		       /* 1 */
	CantBeFiltered,
	CantBeFiltered,
	CantBeFiltered,
	CantBeFiltered,
	CantBeFiltered,
	CantBeFiltered	
};

void
InitEvents()
{
    inputInfo.numDevices = 0;
    inputInfo.devices = (DeviceIntPtr)NULL;
    inputInfo.off_devices = (DeviceIntPtr)NULL;
    currentTime.months = 0;
    currentTime.milliseconds = GetTimeInMillis();
}

void
FilterEvents(pEvent,device)
aEvent	*pEvent;
int	device;
{
    int type;
    int i;
    AMask filter;
    ClientPtr client;

    type = pEvent->u.u.type;
    filter = filters[type];
    for (i=1; i<currentMaxClients; i++){
	client = clients[i];
	if ((client) && (client != serverClient) 
	  && (!client->clientGone) 
	  && ((filter == CantBeFiltered) 
	  || (client->selectMask[device] & filter)))
	 {
	   /* all extension events must have a sequence number */
	   pEvent->u.u.sequenceNumber = client->sequence;
	   WriteEventsToClient(client, 1, pEvent);
         }
    }
}

void
WriteEventsToClient(pClient, count, events)
    ClientPtr	pClient;
    int		count;
    aEvent	*events;
{
    if(pClient->swapped)
    {
        int	i;
        aEvent	eventTo, *eventFrom;

	for(i = 0; i < count; i++)
	{
	    eventFrom = &events[i];
	    /* Remember to strip off the leading bit of type in case
	       this event was sent with "SendEvent." */
	    (*EventSwapVector[eventFrom->u.u.type & 0177])
		(eventFrom, &eventTo);
	    (void)WriteToClient(pClient, sizeof(aEvent), (char *)&eventTo);
	}
    }
    else
    {
	(void)WriteToClient(pClient, count * sizeof(aEvent), (char *) events);
    }
}
