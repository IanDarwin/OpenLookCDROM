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
#include <server/include/misc.h>
#include <server/include/audiodev.h>
#include <server/include/input.h>
#include <audioproto.h>
#include <audio.h>
#include "dda.h"
#include "bba.h"
#include "max_io.h"
#include "physdevice.h"
#include "event.h"

extern void FilterEvent();

void
ProcessInputEvents()
{
    struct interrupt_event event; 
    maxPhysDevice *pDev;

    pDev = &physDevices[0];

    /* Check event queue. */
    if(GetEvent(pDev->bba, &event) == 0) return;

    switch(event.type){
    case UNKNOWN:
    default:
	    ErrorF("Unexpected event received in ProcessInputEvents %d\n",
		    event.type);
	    break;
    }
}


/*
 * returns an event, principally to hide the details of the queue.
 * May craft a server internal event. 
 */
int GetEvent(struct bba_info *info, struct interrupt_event *event)
{
	struct interrupt_event *e;
	int i;
	if (info->head == info->tail) return 0;
	i = info->head;
	e = (struct interrupt_event *)((int)(info->us_start) 
			+ info->event_size * i);
	*event = *e;
	if (i >= info->event_list_size - 1) i = info->head = 0;
	else			 i = ++info->head;
	return 1;
}
/*
 * flushes event queue
 */
int FlushEvent(struct bba_info *info)
{
	struct interrupt_event e;

	int i=0;
	while(GetEvent(info, &e) != 0)
		++i;
	return i;
}

