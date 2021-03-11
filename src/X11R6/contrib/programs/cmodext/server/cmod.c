/*

Copyright (c) 1990 - 1994  FUJITSU LIMITED
Copyright (c) 1990 - 1991  PFU Limited

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE FUJITSU LIMITED AND PFU LIMITED BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED and
PFU Limited shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from the FUJITSU LIMITED and PFU Limited.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
                               fujiwara@a80.tech.yk.fujitsu.co.jp

*/

#ident "@(#)cmod.c	1.7 2/1/91"

#define NEED_REPLIES
#define NEED_EVENTS
#include <stdio.h>
#include "X.h"
#include "Xproto.h"
#include "misc.h"
#include "os.h"
#include "windowstr.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "extnsionst.h"
#include "dixstruct.h"
#include "resource.h"
#include "opaque.h"
#define _CONTROLMODIFIERS_SERVER_	/* don't want Xlib structures */
#include "cmodstr.h"
#include "regionstr.h"
#include "gcstruct.h"
#include "inputstr.h"
#include <sys/time.h>

static CARD32 validReport = 0;
static CARD32 validChange = 0;

static unsigned char	ControlModifiersReqCode;
static int		ControlModifiersEventBase;
static int		ControlModifiersErrorBase;
static int		ControlModifiersScreenIndex = -1;
static int		ControlModifiersWindowIndex = -1;

/*
 * ControlModifiersExtensionInit
 *
 * Called from InitExtensions in main()
 *
 */

void
ControlModifiersExtensionInit()
{
    ExtensionEntry	*extEntry;
    int			i;
    ScreenPtr		pScreen;
    int			ProcControlModifiersDispatch();
    int			SProcControlModifiersDispatch();
    void		ControlModifiersResetProc();
    void		SNotifyEvent();
    WindowPtr		ControlModifiersCreateWindow();

    ControlModifiersGetInfo(&validReport, &validChange);
    if (validReport == 0)
	return;

    /*
     * allocate private pointers in windows.
     */
    if ((ControlModifiersScreenIndex = AllocateScreenPrivateIndex ()) < 0)
	return;
    ControlModifiersWindowIndex = AllocateWindowPrivateIndex ();
    for (i = 0; i < screenInfo.numScreens; i++)
    {
	pScreen = screenInfo.screens[i];
	pScreen->devPrivates[ControlModifiersScreenIndex].val = (long)pScreen->CreateWindow;
	pScreen->CreateWindow = (Bool (*)())ControlModifiersCreateWindow;
    }
    if ((extEntry = AddExtension(CONTROLMODIFIERS_PROTOCOL_NAME,
				 ControlModifiersNumberEvents, 
				 ControlModifiersNumberErrors,
				 ProcControlModifiersDispatch,
				 SProcControlModifiersDispatch,
				 ControlModifiersResetProc,
				 StandardMinorOpcode)))
    {
	ControlModifiersReqCode = (unsigned char)extEntry->base;
	ControlModifiersEventBase = extEntry->eventBase;
	ControlModifiersErrorBase = extEntry->errorBase;
	EventSwapVector[ControlModifiersEventBase + ControlModifiersNotify]
								= SNotifyEvent;
    }
}


/*
 * clear the event mask when the window is created
 */ 

WindowPtr
ControlModifiersCreateWindow (pWin)
WindowPtr	pWin;
{
    pWin->devPrivates[ControlModifiersWindowIndex].val = 0;
    return ((*((WindowPtr (*)())(pWin->drawable.pScreen->devPrivates[ControlModifiersScreenIndex].val)))(pWin));
}

static int
ProcControlModifiersDispatch (client)
    register ClientPtr	client;
{
    REQUEST(xReq);
    switch (stuff->data) {
    case X_ControlModifiersGetVersion:
	return ProcGetVersion (client);
    case X_ControlModifiersSetMask:
	return ProcSetMask (client);
    case X_ControlModifiersGetMask:
	return ProcGetMask (client);
    case X_ControlModifiersGetState:
	return ProcGetState (client);
    case X_ControlModifiersChangeState:
	return ProcChangeState (client);
    case X_ControlModifiersGrab:
	return ProcGrab (client);
    case X_ControlModifiersUngrab:
	return ProcUngrab (client);
    default:
	return BadRequest;
    }
}

static int
SProcControlModifiersDispatch (client)
    register ClientPtr	client;
{
    REQUEST(xReq);
    switch (stuff->data) {
    case X_ControlModifiersGetVersion:
	return SProcGetVersion (client);
    case X_ControlModifiersSetMask:
	return SProcSetMask (client);
    case X_ControlModifiersGetMask:
	return SProcGetMask (client);
    case X_ControlModifiersGetState:
	return SProcGetState (client);
    case X_ControlModifiersChangeState:
	return SProcChangeState (client);
    case X_ControlModifiersGrab:
	return SProcGrab (client);
    case X_ControlModifiersUngrab:
	return SProcUngrab (client);
    default:
	return BadRequest;
    }
}

/*ARGSUSED*/
static void
ControlModifiersResetProc (extEntry)
ExtensionEntry	*extEntry;
{
    int			    i;
    ScreenPtr		    pScreen;

    for (i = 0; i < screenInfo.numScreens; i++)
    {
	pScreen = screenInfo.screens[i];
	if (pScreen->devPrivates[ControlModifiersScreenIndex].val)
	{
	    pScreen->CreateWindow =
		(Bool (*)())(pScreen->devPrivates[ControlModifiersScreenIndex].val);
	    pScreen->devPrivates[ControlModifiersScreenIndex].val = 0;
	}
    }
    return;
}

static int
ProcGetVersion (client)
    register ClientPtr	client;
{
    REQUEST(xControlModifiersGetVersionReq);
    xControlModifiersGetVersionReply	rep;
    register int	n;

    REQUEST_SIZE_MATCH (xControlModifiersGetVersionReq);
    rep.type = X_Reply;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;
    rep.majorVersion = CONTROLMODIFIERS_MAJOR_VERSION;
    rep.minorVersion = CONTROLMODIFIERS_MINOR_VERSION;
    rep.validReport = validReport;
    rep.validChange = validChange;
    if (client->swapped) {
    	swaps(&rep.sequenceNumber, n);
    	swapl(&rep.length, n);
    }
    WriteToClient(client, sizeof (xControlModifiersGetVersionReply), (char *)&rep);
    return (client->noClientException);
}

static int
ProcSetMask (client)
    register ClientPtr	client;
{
    REQUEST (xControlModifiersSetMaskReq);
    WindowPtr	pWin;

    REQUEST_SIZE_MATCH (xControlModifiersSetMaskReq);
    if (!(pWin = LookupWindow (stuff->window, client)))
	return BadWindow;
    pWin->devPrivates[ControlModifiersWindowIndex].val = stuff->mask;
    return Success;
}

static int
ProcGetMask (client)
    register ClientPtr	client;
{
    REQUEST (xControlModifiersGetMaskReq);
    WindowPtr		pWin;
    xControlModifiersGetMaskReply	rep;
    register int	n;

    REQUEST_SIZE_MATCH (xControlModifiersGetMaskReq);
    if (!(pWin = LookupWindow (stuff->window, client)))
	return BadWindow;
    rep.type = X_Reply;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;
    rep.mask = pWin->devPrivates[ControlModifiersWindowIndex].val;
    if (client->swapped) {
    	swaps(&rep.sequenceNumber, n);
    	swapl(&rep.length, n);
    }
    WriteToClient(client, sizeof (xControlModifiersGetMaskReply), (char *)&rep);
    return (client->noClientException);
}

static int
ProcGetState (client)
    register ClientPtr	client;
{
    REQUEST(xControlModifiersGetStateReq);
    xControlModifiersGetStateReply	rep;
    register int		n;
    extern ControlModifiersGetState();

    REQUEST_SIZE_MATCH (xControlModifiersGetStateReq);
    rep.type = X_Reply;
    rep.length = 0;
    rep.sequenceNumber = client->sequence;
    rep.state = ControlModifiersGetState();
    if (client->swapped) {
    	swaps(&rep.sequenceNumber, n);
    	swapl(&rep.length, n);
    }
    WriteToClient(client, sizeof (xControlModifiersGetStateReply), (char *)&rep);
    return (client->noClientException);
}

static int
ProcChangeState (client)
    register ClientPtr	client;
{
    REQUEST (xControlModifiersChangeStateReq);
    WindowPtr	pWin;
    extern ControlModifiersChangeState();

    REQUEST_SIZE_MATCH (xControlModifiersChangeStateReq);
    ControlModifiersChangeState(stuff->change, stuff->state);
    return Success;
}

static int
ProcGrab (client)
    register ClientPtr	client;
{
    REQUEST (xControlModifiersGrabReq);
    WindowPtr	pWin;
    extern ControlModifiersGrab();

    REQUEST_SIZE_MATCH (xControlModifiersGrabReq);
    ControlModifiersGrab();
    return Success;
}

static int
ProcUngrab (client)
    register ClientPtr	client;
{
    REQUEST (xControlModifiersUngrabReq);
    WindowPtr	pWin;
    extern ControlModifiersUngrab();

    REQUEST_SIZE_MATCH (xControlModifiersUngrabReq);
    ControlModifiersUngrab();
    return Success;
}

static int
SProcGetVersion (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xControlModifiersGetVersionReq);

    swaps (&stuff->length, n);
    return ProcGetVersion (client);
}

static int
SProcSetMask (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xControlModifiersSetMaskReq);

    swaps (&stuff->length, n);
    return ProcSetMask (client);
}

static int
SProcGetMask (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xControlModifiersGetMaskReq);

    swaps (&stuff->length, n);
    return ProcSetMask (client);
}

static int
SProcGetState (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xControlModifiersGetStateReq);

    swaps (&stuff->length, n);
    return ProcGetState (client);
}

static int
SProcChangeState (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xControlModifiersChangeStateReq);

    swaps (&stuff->length, n);
    return ProcChangeState (client);
}

static int
SProcGrab (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xControlModifiersGrabReq);

    swaps (&stuff->length, n);
    return ProcGrab (client);
}

static int
SProcUngrab (client)
    register ClientPtr	client;
{
    register int    n;
    REQUEST (xControlModifiersUngrabReq);

    swaps (&stuff->length, n);
    return ProcUngrab (client);
}

int
ProcControlModifiersNotifyEvent(pWin, ev)
WindowPtr pWin;
xControlModifiersEvent *ev;
{
    if (pWin->devPrivates[ControlModifiersWindowIndex].val & ev->change) {
	DeliverEvents(pWin, ev, 1, (WindowPtr)NULL);
    }
    return(WT_WALKCHILDREN);
}

void
ControlModifiersNotifyEvent(change, state)
CARD32 change;
CARD32 state;
{
    int		i;
    ScreenPtr	pScreen;
    xControlModifiersEvent	ev;

    ev.type = ControlModifiersEventBase;
    ev.change = change;
    ev.state  = state;
    for (i = 0; i < screenInfo.numScreens; i++) {
	pScreen = screenInfo.screens[i];
	WalkTree(pScreen, ProcControlModifiersNotifyEvent, &ev);
    }
}

static void
SNotifyEvent (from, to)
    xControlModifiersEvent	*from, *to;
{
    to->type = from->type;
    cpswaps (from->sequenceNumber, to->sequenceNumber);
}
