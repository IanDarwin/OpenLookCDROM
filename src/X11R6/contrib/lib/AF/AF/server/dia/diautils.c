/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  
******************************************************************/
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
#include "Amd.h"
#include "misc.h"
#include "diastruct.h"
#include "audiodev.h"

/*
 * CompareTimeStamps returns -1, 0, or +1 depending on if the first
 * argument is less than, equal to or greater than the second argument.
 */

int
CompareTimeStamps(a, b)
    TimeStamp a, b;
{
    if (a.months < b.months)
	return EARLIER;
    if (a.months > b.months)
	return LATER;
    if (a.milliseconds < b.milliseconds)
	return EARLIER;
    if (a.milliseconds > b.milliseconds)
	return LATER;
    return SAMETIME;
}

/*
 * convert client times to server TimeStamps
 */

#define HALFMONTH ((unsigned long) 1<<31)
TimeStamp
ClientTimeToServerTime(c)
     CARD32 c;
{
    TimeStamp ts;
    if (c == ACurrentTime)
	return currentTime;
    ts.months = currentTime.months;
    ts.milliseconds = c;
    if (c > currentTime.milliseconds)
    {
	if (((unsigned long) c - currentTime.milliseconds) > HALFMONTH)
	    ts.months -= 1;
    }
    else if (c < currentTime.milliseconds)
    {
	if (((unsigned long)currentTime.milliseconds - c) > HALFMONTH)
	    ts.months += 1;
    }
    return ts;
}

/* No-op Don't Do Anything : sometimes we need to be able to call a procedure
 * that doesn't do anything. 
 */
void
NoopDDA()
{
}

typedef struct _BlockHandler {
    void    (*BlockHandler)();
    void    (*WakeupHandler)();
    pointer blockData;
} BlockHandlerRec, *BlockHandlerPtr;

static BlockHandlerPtr	handlers;
static int		numHandlers;
static int		sizeHandlers;

/* called from the OS layer */
BlockHandler(pTimeout, pReadmask)
pointer	pTimeout;	/* DIA doesn't want to know how OS represents time */
pointer pReadmask;	/* nor how it represents the set of descriptors */
{
    register int i;
    
    for (i = 0; i < audioDeviceInfo.numDevices; i++)
	(* audioDeviceInfo.devices[i]->BlockHandler)(i, 
				audioDeviceInfo.devices[i]->blockData,
				pTimeout, pReadmask);
    for (i = 0; i < numHandlers; i++)
	(*handlers[i].BlockHandler) (handlers[i].blockData,
				     pTimeout, pReadmask);
}


WakeupHandler(result, pReadmask)
unsigned long	result;	/* 32 bits of undefined result from the wait */
pointer pReadmask;	/* the resulting descriptor mask */
{
    register int i;
    for (i = numHandlers - 1; i >= 0; i--)
	(*handlers[i].WakeupHandler) (handlers[i].blockData,
				      result, pReadmask);
    for (i = 0; i < audioDeviceInfo.numDevices; i++)
	(* audioDeviceInfo.devices[i]->WakeupHandler)(i, 
				audioDeviceInfo.devices[i]->wakeupData,
				result, pReadmask);
}

ABool
RegisterBlockAndWakeupHandlers (blockHandler, wakeupHandler, blockData)
    void    (*blockHandler)();
    void    (*wakeupHandler)();
    pointer blockData;
{
    BlockHandlerPtr new;

    if (numHandlers >= sizeHandlers)
    {
    	new = (BlockHandlerPtr) xrealloc (handlers, (numHandlers + 1) *
				      	  sizeof (BlockHandlerRec));
    	if (!new)
	    return FALSE;
    	handlers = new;
	sizeHandlers = numHandlers + 1;
    }
    handlers[numHandlers].BlockHandler = blockHandler;
    handlers[numHandlers].WakeupHandler = wakeupHandler;
    handlers[numHandlers].blockData = blockData;
    numHandlers = numHandlers + 1;
    return TRUE;
}

void
RemoveBlockAndWakeupHandlers (blockHandler, wakeupHandler, blockData)
    void    (*blockHandler)();
    void    (*wakeupHandler)();
    pointer blockData;
{
    int	    i;

    for (i = 0; i < numHandlers; i++)
	if (handlers[i].BlockHandler == blockHandler &&
	    handlers[i].WakeupHandler == wakeupHandler &&
	    handlers[i].blockData == blockData)
	{
	    for (; i < numHandlers - 1; i++)
		handlers[i] = handlers[i+1];
	    numHandlers--;
	    break;
	}
}

InitBlockAndWakeupHandlers ()
{
    xfree (handlers);
    handlers = (BlockHandlerPtr) 0;
    numHandlers = 0;
    sizeHandlers = 0;
}

int
Ones(mask)                /* HACKMEM 169 */
    AMask mask;
{
    register AMask y;

    y = (mask >> 1) &033333333333;
    y = mask - y - ((y >>1) & 033333333333);
    return (((y + (y >> 3)) & 030707070707) % 077);
}
