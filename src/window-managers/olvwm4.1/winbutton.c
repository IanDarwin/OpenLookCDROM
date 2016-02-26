/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef IDENT
#ident	"@(#)winbutton.c	1.5 olvwm version 07 Jan 1994"
#endif

/*
 * Based on
#ident	"@(#)winbutton.c	26.31	93/06/28 SMI"
 *
 */

#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include <olgx/olgx.h>

#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "globals.h"
#include "menu.h"
#include "events.h"

extern void FrameAllowEvents();
extern Bool DoDefaultMenuAction();
extern Atom AtomChangeState;

/***************************************************************************
* private data
***************************************************************************/

#define in_windowmark(win,x,y) \
	( (x) >= 0 && (y) >= 0 && \
	  (x) <= Abbrev_MenuButton_Width(WinGI((win),NORMAL_GINFO)) && \
	  (y) <= Abbrev_MenuButton_Height(WinGI((win),NORMAL_GINFO)) \
	)
static Bool buttonActive = False;
static ClassButton classButton;
static SemanticAction currentAction = ACTION_NONE;

/***************************************************************************
* private functions
***************************************************************************/

static int drawButton();

static void 
doUnhilite(act, mode, winInfo)
    int act;
    MenuTrackMode mode;
    WinButton *winInfo;
{
    Graphics_info	*gis;
    long flags;

    gis = (winInfo->core.client->isFocus) ? WinGI(winInfo, INPUTFOCUS_GINFO) :
					    WinGI(winInfo, NORMAL_GINFO);
    if (act != SYNC_CHANGECLICK)
	flags = OLGX_NORMAL | OLGX_ERASE;
    else if (mode == MODE_CLICK)
	flags = OLGX_BUSY | OLGX_ERASE | OLGX_NORMAL;
    else {
	/* don't do this; it's unsettling to press it in when you drag again */
	return;
    }

    olgx_draw_abbrev_button(gis, winInfo->core.self, 0, 0, flags);
}

/* 
 * eventButtonPress - handle button press events on the close button window.  
 */
static int
eventButtonPress(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinButton	*winInfo;
{
	Client *cli = winInfo->core.client;
	WinPaneFrame *winFrame = cli->framewin;
	SemanticAction a;
	Graphics_info	*gis;
	
	if (cli->isFocus)
	    gis = WinGI(winInfo, INPUTFOCUS_GINFO);
	else gis = WinGI(winInfo,NORMAL_GINFO);

	a = MenuMouseAction(dpy, event, ModMaskMap[MOD_CONSTRAIN]);

	if (winInfo->ignore) {
	    FrameAllowEvents(cli, event->xbutton.time);
	    return;
	}

        switch (a) {
	case ACTION_SELECT:
	    olgx_draw_abbrev_button(gis, winInfo->core.self, 
				    0, 0, OLGX_INVOKED);
	    /*
	     * REMIND: bad style.  This is grabbing the pointer after
	     * the fact.  We should set up a passive grab instead.
	     */
	    XGrabPointer(dpy, winInfo->core.self, False,
		 (ButtonReleaseMask | ButtonPressMask | PointerMotionMask),
		 GrabModeAsync, GrabModeAsync, None,
		 GRV.CloseDownPointer, CurrentTime);
	    buttonActive = True;
	    currentAction = a;
	    break;

        case ACTION_MENU:
	    olgx_draw_abbrev_button(gis, winInfo->core.self, 
				    0, 0, OLGX_INVOKED);
	    if (winFrame->core.client->wmDecors->menu_type != MENU_NONE)
		ShowStandardMenuSync(winFrame, event, True, doUnhilite, winInfo);
	    break;

	default:
	    FrameAllowEvents(cli, event->xbutton.time);
	    return;
        }
}

/*
 * eventButtonRelease - handle button release events on the close button
 * window.  When we handle an event, start ignoring mouse events on the button
 * and send a ClientMessage to ourself.  When we receive the ClientMessage,
 * stop ignore events.  This is so that double-clicking on the button doesn't
 * close and then reopen the window (or perform the default action twice).
 */
static int
eventButtonRelease(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinButton	*winInfo;
{
	Client *cli = winInfo->core.client;
	int x,y;
	XClientMessageEvent ce;

	FrameAllowEvents(cli, event->xbutton.time);

	if (!AllButtonsUp(event))
	    return;

        XUngrabPointer(dpy, CurrentTime);

        x = event->xbutton.x;
        y = event->xbutton.y;

	if (buttonActive) {
	    drawButton(dpy, winInfo);
	    buttonActive = False;
	}

        if (!in_windowmark(winInfo,x,y) || currentAction != ACTION_SELECT) {
	    return;
        }

	if (! winInfo->ignore) {
	    if (!DoDefaultMenuAction(cli->framewin)) {
		ClientOpenCloseToggle(cli,event->xbutton.time);
	    }
	    ce.type = ClientMessage;
	    ce.window = winInfo->core.self;
	    ce.message_type = AtomChangeState;
	    ce.format = 32;
	    XSendEvent(dpy, winInfo->core.self, False, NoEventMask,
		       (XEvent *) &ce);
	    winInfo->ignore = True;
	}

	currentAction = ACTION_NONE;
}

/* 
 * eventMotionNotify - handle motion notify events on the close button window.  
 */
static int
eventMotionNotify(dpy, event, winInfo)
Display	*dpy;
XEvent	*event;
WinButton	*winInfo;
{
	int 	x,y;
	Client *cli = winInfo->core.client;
	Graphics_info	*gis;

	if (cli->isFocus)
	    gis = WinGI(winInfo, INPUTFOCUS_GINFO);
	else gis = WinGI(winInfo,NORMAL_GINFO);

	if (!event->xmotion.same_screen || currentAction != ACTION_SELECT)
		return;

        x = event->xmotion.x;
        y = event->xmotion.y;
        if ( buttonActive && !in_windowmark(winInfo,x,y) ) {
	    drawButton(dpy, winInfo);
            buttonActive = False;
        } else if ( !buttonActive && in_windowmark(winInfo,x,y) ) {
            olgx_draw_abbrev_button(gis, winInfo->core.self,
	    	0, 0, OLGX_INVOKED);
            buttonActive = True;
        }
}

/*
 * eventClientMessage - handle ClientMessage events sent to the button.  In
 * eventButtonRelease, we send a ClientMessage to ourself.  When we receive
 * it, stop ignoring button press events.
 */
static int
eventClientMessage(dpy, ce, winInfo)
    Display *dpy;
    XClientMessageEvent *ce;
    WinButton *winInfo;
{
    if (ce->message_type == AtomChangeState)
	winInfo->ignore = False;
    return 0;
}

/*
 * drawButton -- draw the window button
 */
/*ARGSUSED*/
static int
drawButton(dpy, winInfo)
Display	*dpy;
WinButton *winInfo;
{
    Client		*cli = winInfo->core.client;
    GC 			windowGC = WinGC(winInfo,WINDOW_GC);
    XGCValues		gcv;
    int			focusLines = (GRV.FocusFollowsMouse ? 1 : 0) ^
				     (GRV.InvertFocusHighlighting ? 1 : 0);
    Graphics_info	*gis;

    if (cli->isFocus)
	gis = WinGI(winInfo, INPUTFOCUS_GINFO);
    else gis = WinGI(winInfo,NORMAL_GINFO);

    /*
     * Erase the background first.  Unfortunately, we can't depend on
     * OLGX_ERASE to do the right thing, because it (a) erases only in BG1,
     * and (b) erases only in 2D mode.  We need to erase a background color
     * that depends on the state of the frame.  If we're in click-focus and we
     * have the focus, draw in BG2; otherwise, draw in BG1.
     */

    /* Temporarily set background to BG2 if click-to-type */
    if (!focusLines && winInfo->core.client->isFocus && Win3D(winInfo)) {
	XGetGCValues(dpy,windowGC,GCBackground,&gcv);
	XSetBackground(dpy,windowGC,cli->scrInfo->colorInfo.bg2Color);
    }

    XFillRectangle(dpy, winInfo->core.self, windowGC, 0, 0,
		   Abbrev_MenuButton_Width(gis),
		   Abbrev_MenuButton_Height(gis));

    /* Restore background back to BG1 */
    if (!focusLines && winInfo->core.client->isFocus && Win3D(winInfo)) {
	XSetBackground(dpy,windowGC,gcv.background);
    }

    olgx_draw_abbrev_button(gis, winInfo->core.self,
			    0, 0, OLGX_NORMAL | OLGX_ERASE);

    /*
     * REMIND: hack for working around OLGX deficiency.  OLGX erases the
     * "ears" at each corner of the window button to the background color.  
     * They should really be filled in with the foreground color.
     */
    if (!focusLines && winInfo->core.client->isFocus && !Win3D(winInfo)) {
	XDrawRectangle(dpy, winInfo->core.self, WinGC(winInfo,FOREGROUND_GC),
		       0, 0,
		       Abbrev_MenuButton_Width(gis)-1,
		       Abbrev_MenuButton_Height(gis)-1);
	XDrawPoint(dpy, winInfo->core.self, WinGC(winInfo,FOREGROUND_GC),
		   Abbrev_MenuButton_Width(gis)-1,
		   Abbrev_MenuButton_Height(gis)-1);
    }

}


/*
 * DestroyButton -- destroy the close button window resources and free any allocated
 *	data.
 */
static int
/* ARGSUSED */
destroyButton(dpy, winInfo)
Display	*dpy;
WinButton *winInfo;
{
	/* free our data and throw away window */
	ScreenDestroyWindow(winInfo->core.client->scrInfo, winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/* 
 * focusButton - the focus or selection state has changed
 */
static int
/* ARGSUSED */
focusButton(dpy, winInfo, selected)
Display *dpy;
WinButton *winInfo;
Bool selected;
{
        (WinFunc(winInfo,core.drawfunc))(dpy, winInfo);
}

/*
 * heightfuncButton - recomputes the height of the close button window
 */
static int 
/* ARGSUSED */
heightfuncButton(win, pxcre)
WinButton *win;
XConfigureRequestEvent *pxcre;
{
	return Abbrev_MenuButton_Width(WinGI(win,NORMAL_GINFO));
}

/*
 * widthfuncButton - recomputes the width of the close button window
 */
static int 
/* ARGSUSED */
widthfuncButton(win, pxcre)
WinButton *win;
XConfigureRequestEvent *pxcre;
{
	return Abbrev_MenuButton_Height(WinGI(win,NORMAL_GINFO));
}


/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeButton  -- create the close button window. Return a WinGeneric structure.
 */
WinButton *
MakeButton(dpy, par, x, y)
Display	*dpy;
WinGeneric *par;
int x,y;
{
	WinButton *w;
	Window win;
        unsigned long valuemask;
        XSetWindowAttributes attributes;
        Graphics_info	*gisNormal = WinGI(par,NORMAL_GINFO);

        attributes.event_mask =
	    ButtonReleaseMask | ButtonPressMask | ExposureMask;
	attributes.cursor = GRV.CloseUpPointer;
        valuemask = CWEventMask | CWCursor;

        win = ScreenCreateWindow(par->core.client->scrInfo, par->core.self,
                        x, y,
			Abbrev_MenuButton_Width(gisNormal), 
			Abbrev_MenuButton_Height(gisNormal),
                        valuemask,
                        &attributes);

	/* create the associated structure */
	w = MemNew(WinButton);
	w->core.self = win;
	w->class = &classButton;
	w->core.kind = WIN_WINBUTTON;
	WinAddChild(par,w);
	w->core.children = NULL;
	w->core.client = par->core.client;
	w->core.x = x;	
	w->core.y = y;
	w->core.width = Abbrev_MenuButton_Width(gisNormal);
	w->core.height = Abbrev_MenuButton_Height(gisNormal);
	w->core.dirtyconfig = 0;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:CloseButton";
	w->ignore = False;

	/* register the window */
	WIInstallInfo(w);

        MapWindow(w);

	return w;
}

void
/* ARGSUSED */
ButtonInit(dpy)
Display *dpy;
{
        classButton.core.kind = WIN_WINBUTTON;
        classButton.core.xevents[ButtonPress] = eventButtonPress;
        classButton.core.xevents[ButtonRelease] = eventButtonRelease;
        classButton.core.xevents[MotionNotify] = eventMotionNotify;
        classButton.core.xevents[Expose] = WinEventExpose;
	classButton.core.xevents[ClientMessage] = eventClientMessage;
        classButton.core.focusfunc = focusButton;
        classButton.core.drawfunc = drawButton;
        classButton.core.destroyfunc = destroyButton;
        classButton.core.selectfunc = NULL;
        classButton.core.newconfigfunc = WinNewConfigFunc;
        classButton.core.newposfunc = WinNewPosFunc;
        classButton.core.setconfigfunc = WinSetConfigFunc;
        classButton.core.createcallback = NULL;
        classButton.core.heightfunc = heightfuncButton;
        classButton.core.widthfunc = widthfuncButton;
}

