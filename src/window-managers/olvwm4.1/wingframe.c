/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

/*
 * wingframe.c -- generic frame window routines
 */

#ifdef IDENT
#ident	"@(#)wingframe.c	1.3 olvwm version 12/7/92"
#endif

/*
 * Based on
#ident	"@(#)wingframe.c	26.27	91/09/14 SMI"
 *
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#include "i18n.h"
#include <olgx/olgx.h>

#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "globals.h"
#include "events.h"
#include "virtual.h"

/***************************************************************************
* private data
***************************************************************************/

/*
 * REMIND
 * The hadSelect variable is necessary because we sometimes receive
 * MotionNotify events with all buttons up, even though we ask for only
 * ButtonMotionMask.  We set hadSelect only on receipt of an event that is 
 * bound to the Select action, and we ignore motion events that occur if
 * hadSelect isn't set.
 */
static  Bool            hadSelect = False;
static  Time            timeOfLastButPress = 0;
static  int             buttonPressX;
static  int             buttonPressY;
static  XButtonEvent    lastSelectRelease;
static  XButtonEvent    lastSelectPress;
static  Time            lastDoubleClickTime;
static	SemanticAction	currentAction = ACTION_NONE;

/***************************************************************************
* forward-declared functions
***************************************************************************/

extern void ClientSetCurrent();

/***************************************************************************
* static functions
***************************************************************************/

void
autoRaise(frame)
    WinGenericFrame	*frame;
{
    Bool samescreen;
    Window root, child;
    int rootx, rooty, winx, winy;
    unsigned int state;
    Client *cli;

    /* Frame may have gone away */
    if (frame == NULL ||
        (frame->core.kind != WIN_FRAME && frame->core.kind != WIN_ICON))
    {
        return;
    }

    cli = frame->core.client;
    if (cli->isFocus) {
        if (GRV.FocusFollowsMouse) {
            samescreen = XQueryPointer(cli->dpy, WinRootID(frame),
                    &root, &child, &rootx, &rooty, &winx, &winy, &state);
            if (samescreen && child == frame->core.self)
                ClientFront(cli);
        } else {
            ClientFront(cli);
        }
    }    
}

/***************************************************************************
* global functions
***************************************************************************/

/*
 * GFrameSelect -- handle selection state change
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
int
GFrameSelect(dpy, winInfo, selected)
Display	*dpy;
WinGeneric *winInfo;
Bool selected;
{
	(WinFunc(winInfo,core.drawfunc))(dpy, winInfo);
}

/*
 * GFrameFocus -- handle focus changes
 *
 * If we're in auto-raise mode, raise the window.  But if we're in focus-
 * follows-mouse, we query the pointer to make sure we're still in the same 
 * window before we do this raise.  This is to avoid restacking loops.
 *
 * If colormap installation is tracking the input focus, get the first entry 
 * in the client's WM_COLORMAP_WINDOWS list and install its colormap; 
 * otherwise, install the pane's colormap.
 */
/*ARGSUSED*/
int
GFrameFocus(dpy, winInfo, focus)
Display		*dpy;
WinGenericFrame *winInfo;
Bool		focus;
{
	WinGeneric *cmwi;

	if (focus) {
	    ClientSetCurrent(winInfo->core.client);
	    ClientSetWindowState(winInfo->core.client);

	    if (GRV.ColorTracksInputFocus) {
		if (winInfo->core.client->colormapWins)
		    cmwi = winInfo->core.client->colormapWins->value;
		else
		    cmwi = (WinGeneric *) winInfo->fcore.panewin;
		LockColormap(dpy, winInfo->core.client, cmwi);
	    }

	    if (GRV.AutoRaise) {
		if (GRV.AutoRaiseDelay > 0) {
		    TimeoutCancel();
		    TimeoutRequest(GRV.AutoRaiseDelay, autoRaise, winInfo);
		} else {
		    autoRaise(winInfo);
		}
	    }
	}
}

/*
 * GFrameSetConfigFunc -- change configuration of frame window
 * REMIND: ensure that transient windows are treated properly.
 */
int
GFrameSetConfigFunc(dpy, win)
Display	*dpy;
WinGenericFrame *win;
{
        XWindowChanges xwc;
	int	dw, dh;
	Client	*cli = win->core.client;

        if (win->core.dirtyconfig)
        {
		if (cli->sticky) {
		    dw = DisplayWidth(win->core.client->dpy,
				      win->core.client->screen);
		    dh = DisplayHeight(win->core.client->dpy,
				       win->core.client->screen);

		    if (win->core.x + (int) win->core.width < 0) {
			win->core.x = (dw + (win->core.x % dw)) % dw; 
			win->core.dirtyconfig |= CWX;
		    }
		    else if (win->core.x >= dw) {
			win->core.x = win->core.x % dw; 
			win->core.dirtyconfig |= CWX;
		    }
		    if (win->core.y + (int) win->core.height < 0) {
			win->core.y = (dh + (win->core.y % dh)) % dh; 
			win->core.dirtyconfig |= CWY;
		    }
		    else if (win->core.y >= dh) {
			win->core.y = win->core.y % dh; 
			win->core.dirtyconfig |= CWY;
		    }
		}
                xwc.x = win->core.x;
                xwc.y = win->core.y;
                xwc.width = win->core.width;
                xwc.height = win->core.height;
		xwc.sibling = win->core.stack_sib;
		xwc.stack_mode = win->core.stack_mode;
		if ((win->core.dirtyconfig & CWSibling) && 
		    !(win->core.dirtyconfig &CWStackMode))
			win->core.dirtyconfig &= ~CWSibling;

                ConfigureWindow(dpy,win,win->core.dirtyconfig,&xwc);
		win->core.dirtyconfig &= ~(CWX|CWY|CWWidth|CWHeight|CWSibling|CWStackMode);
        }
}

/* GFrameSetStack -- set the frame's stacking position.   Does not initiate
 *	a configuration change.
 */
void
GFrameSetStack(win, mask, mode, sib)
WinGenericFrame *win;
int mask;
int mode;
Window sib;
{
	WinGeneric *wsib;

	if ((mask & CWSibling) && (mask & CWStackMode))
	{
		wsib = WIGetInfo(sib);
		if (wsib != NULL)
		{
			win->core.stack_sib = wsib->core.client->framewin->core.self;
			win->core.dirtyconfig |= CWSibling;
		}
	}
	if (mask & CWStackMode)
	{
		win->core.stack_mode = mode;
		win->core.dirtyconfig |= CWStackMode;
	}
}

/* GFrameSetConfig - set the frame's size and position -- generally used in
 * resizing or moving the window.  We position the frame and resize the pane.
 * If the pane has refused resizing we skip that part.
 */
void
GFrameSetConfig(win,x,y,w,h)
WinGenericFrame *win;
int x,y,w,h;
{
        WinPane *pane = (WinPane *)win->fcore.panewin;

        (WinFunc(win,core.newposfunc))(win,x,y);
	if (pane != NULL)
	{
            if (WinFunc(pane,pcore.setsizefunc) != NULL)
                (WinFunc(pane,pcore.setsizefunc))(pane,
                    w-(WinFunc(win,fcore.widthleft))(win)-(WinFunc(win,fcore.widthright))(win),
                    h-(WinFunc(win,fcore.heighttop))(win)-(WinFunc(win,fcore.heightbottom))(win));
            WinCallConfig(win->core.client->dpy, pane, NULL);
	}
	else
	{
            WinCallConfig(win->core.client->dpy, win, NULL);
	}
}


/***************************************************************************
* global event functions
***************************************************************************/

/*
 * GFrameEventButtonRelease -- a button has been released
 *
 */
int
GFrameEventButtonRelease(dpy, event, frameInfo)
Display *dpy;
XEvent  *event;
WinGenericFrame *frameInfo;
{
        WinGenericPane	*winPane = (WinGenericPane*)frameInfo->fcore.panewin;
        Client *cli = frameInfo->core.client;

	if (!AllButtonsUp(event))
	    return;

#define bevent  (event->xbutton)

        switch (currentAction)
        {
        case ACTION_SELECT:
		if (GRV.SelectWindows) {
		    ClearSelections(dpy);
		    AddSelection(cli, event->xbutton.time);
		}

		ClientSetCurrent(cli);
		ClientFront(cli);

                hadSelect = False;

                if (WinFunc(frameInfo,fcore.selectClick) != NULL)
                {
		    (WinFunc(frameInfo,fcore.selectClick))(dpy,event,frameInfo);
                }

                /* If the click happened in the decoration windows,
                 * (i.e. not in the pane), check for a double click.
		 * Check last click time to see if we are in double
		 * click land.  Also check that the user hasn't just
		 * hit the button 3 times very rapidly.  This would
		 * cause a cycle of the window changing size.
		 * REMIND this is more nasty than it should be because
		 * not all frames are yet required to have panes.
		 * Once all panes have frames the subwindow test gets
		 * easier.
                 */
                if ((WinFunc(frameInfo,fcore.selectDoubleClick) != NULL) &&
		    ((winPane == NULL) || (bevent.subwindow != winPane->core.self)) &&
                   ((bevent.time-lastSelectRelease.time) <= GRV.DoubleClickTime) &&
                   ((bevent.time-lastDoubleClickTime) > GRV.DoubleClickTime))
                {
                    /* we have a double click */
		    (WinFunc(frameInfo,fcore.selectDoubleClick))(dpy,event,frameInfo);
                    lastDoubleClickTime = bevent.time;
                 }
                lastSelectRelease = event->xbutton;
		currentAction = ACTION_NONE;
                break;  /* out of ACTION_SELECT case */
	case ACTION_ADJUST:
                if (WinFunc(frameInfo,fcore.adjustClick) != NULL)
                {
		    (WinFunc(frameInfo,fcore.adjustClick))(dpy,event,frameInfo);
                }
		currentAction = ACTION_NONE;
		break;

        }
}

/*
 * GFrameEventMotionNotify -- a button is down and the pointer is moving
 */
int
GFrameEventMotionNotify(dpy, event, frameInfo)
Display *dpy;
XEvent  *event;
WinGenericFrame *frameInfo;
{
        /* We get this only after a Select press */
        if (hadSelect == False) /* watch for erroneous motions */
        {
                return;
        }

	if (!event->xmotion.same_screen)
		return;

        /* See if we have moved more than the threshold amount. */
        if ((ABS(event->xmotion.x - buttonPressX) < GRV.MoveThreshold) &&
            (ABS(event->xmotion.y - buttonPressY) < GRV.MoveThreshold))
                return;

	(WinFunc(frameInfo,fcore.selectDrag))(dpy, event, frameInfo, &lastSelectPress);

        /*
         * UserMoveWindows() will grab the pointer and handle events
         * using an interposer, so we can clear the hadSelect flag.
         */
        hadSelect = False;
}

/*
 * GFrameEventButtonPress -- a mouse button has gone down.
 */
int
GFrameEventButtonPress(dpy, event, frameInfo)
Display *dpy;
XEvent  *event;
WinGenericFrame *frameInfo;
{
        WinPane         *winPane = (WinPane*)frameInfo->fcore.panewin;
        Window          panewindow = winPane->core.self;
	SemanticAction	a;
	unsigned int	ignoremask;

	ignoremask = ModMaskMap[MOD_CONSTRAIN] |
		     ModMaskMap[MOD_INVERT] |
		     ModMaskMap[MOD_REDUCE];

	a = ResolveMouseBinding(dpy, event, ignoremask);

	switch (a) {
        case ACTION_SELECT:
		currentAction = a;
                /*
		 * Save the location where the button went down so we
                 * can see if the user moves the mouse more than
                 * GRV.MoveThreshold, and wants to move the window.
                 */
                buttonPressX = event->xbutton.x;
                buttonPressY = event->xbutton.y;

                if (!GRV.FocusFollowsMouse && 
		    (WinFunc(frameInfo,fcore.selectPress) != NULL))
                {
                /* It is possible for us to replay the event and
                 * have the window, (decoration window, e.g. the
                 * resize corner, the titlebar), in which the
                 * button press happened to ignore it.
                 * In this case we would get the event
                 * again.  For example, the user could button press
                 * in the title bar, (which doesn't select this event),
                 * and have this EventFrame routine get the same event
                 * twice.  So, we check that the time stamp of this
                 * button press is different than the last.
                 */
                        if (event->xbutton.time == timeOfLastButPress)
                                /* We already dealt with this event. */
                                break;
                        timeOfLastButPress = event->xbutton.time;
                        if (event->xbutton.subwindow == panewindow)
                        {
			    (WinFunc(frameInfo,fcore.selectPress))(dpy,event,frameInfo);
                        }

                        /* Let the button press through
                         * if we had grabbed it.
                         */
                        XAllowEvents(dpy, ReplayPointer, CurrentTime);
                }  /* End if not GRV.FocusFollowsMouse */

                lastSelectPress = event->xbutton;
                hadSelect = True;
                break;  /* Break case ACTION_SELECT */

        case ACTION_ADJUST:
	    currentAction = a;
	    if (!GRV.FocusFollowsMouse) {
		if (event->xbutton.time == timeOfLastButPress)
		    break;
		timeOfLastButPress = event->xbutton.time;
		if (event->xbutton.subwindow == panewindow
		    && WinFunc(frameInfo,fcore.adjustPress) != NULL) {
		    (WinFunc(frameInfo,fcore.adjustPress))
			(dpy,event,frameInfo);
		}
		XAllowEvents(dpy, ReplayPointer, CurrentTime);
	    }
	    break;
        case ACTION_MENU:
	    currentAction = a;
	    if (WinFunc(frameInfo,fcore.menuPress) != NULL)
		(WinFunc(frameInfo,fcore.menuPress))(dpy, event, frameInfo);
            break;

        }  /*  End switch on button pressed */
}


/*
 * GFrameEventEnterNotify
 *
 * If we entered from outside and we're in follows-mouse mode, set the focus.
 * If we enter the frame from the pane or directly from elsewhere (i.e.
 * nonlinearly) install the appropriate colormap.
 */
int
GFrameEventEnterNotify(dpy, event, frameInfo)
Display	*dpy;
XEvent	*event;
WinGenericFrame *frameInfo;
{
    Client *cli = frameInfo->core.client;

    if (GRV.FocusFollowsMouse && event->xcrossing.detail != NotifyInferior)
	ClientSetFocus(cli,True,event->xcrossing.time);

    switch (event->xcrossing.detail) {
    case NotifyInferior:
    case NotifyNonlinear:
	ColorWindowCrossing(dpy, event, cli->scrInfo->rootwin);
	break;
    }
}


/*
 * GFrameEventFocus
 *
 * If the focus changed normally (i.e. not as the result of a keyboard grab or 
 * ungrab), call the focus functions.
 */
int
/* ARGSUSED */
GFrameEventFocus(dpy, event, frameInfo)
    Display *dpy;
    XEvent *event;
    WinGenericFrame *frameInfo;
{
    switch (event->xfocus.mode) {
    case NotifyNormal:
    case NotifyWhileGrabbed:
	if (event->xfocus.detail <= NotifyNonlinearVirtual) {
	    if (event->type == FocusIn)
		WinCallFocus(frameInfo, True);
	    else {
	        Client	*cli;

	        cli = frameInfo->core.client;
	        /*
	         * When we lose focus,
	         * we have to WinCallFocus on both the Icon and the regular
	         * Frame:  if we double click on an icon, it gets input
	         * focus and when it loses it, it must redraw the frame.
	         * Otherwise, the newly opened frame gets drawn as if it
	         * had input focus
		 *
		 * We can't do this when getting focus, since it confuses
		 * autoraise.  There's got to be a better way . . .
	         */
	        if (cli->framewin)
	            WinCallFocus(cli->framewin, False);
	        if (cli->iconwin)
	            WinCallFocus(cli->iconwin, False);
	    }
	}
	break;
    }
}
