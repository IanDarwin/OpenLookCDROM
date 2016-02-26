/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef IDENT
#ident	"@(#)winbusy.c	1.3 olvwm version 07 Jan 1994"
#endif

/*
 * Based on
#ident	"@(#)winbusy.c	26.12	93/06/28 SMI"
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

/***************************************************************************
* global data
***************************************************************************/

/***************************************************************************
* private data
***************************************************************************/

static ClassBusy classBusy;
static Bool busyDisabled = True;

/***************************************************************************
* private functions
***************************************************************************/

/*
 * DestroyBusy -- destroy the busy window resources and free any allocated
 *	data.
 */
static int
destroyBusy(dpy, winInfo)
Display	*dpy;
WinBusy *winInfo;
{
	/* free our data and throw away window */
	XUndefineCursor(dpy, winInfo->core.self);
	WinRemoveChild(winInfo->core.parent,winInfo);
	ScreenDestroyWindow(winInfo->core.client->scrInfo, winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	PaintVirtualWindow(winInfo->core.parent);
	MemFree(winInfo);
}


/*
 * widthfuncBusy - recomputes the height of the busy window
 */
int
/* ARGSUSED */
widthfuncBusy(win, pxcre)
WinBusy *win;
XConfigureRequestEvent *pxcre;
{
	WinGenericFrame *frame = (WinGenericFrame *)win->core.parent;
	return frame->fcore.panewin->core.width;
}

/*
 * heightfuncBusy - recomputes the height of the busy window
 */
int
/* ARGSUSED */
heightfuncBusy(win, pxcre)
WinBusy *win;
XConfigureRequestEvent *pxcre;
{
	WinGenericFrame *frame = (WinGenericFrame *)win->core.parent;
	return frame->fcore.panewin->core.height;
}


/*
 *
 * REMIND
 * 
 * Busy windows are effectively "turned off" by the static Bool busyDisabled.
 * The reason for this is that having a busy window on the screen over the
 * application's window will prevent that application from receiving drag-n-
 * drop messages properly.  When busyDisabled is set, the following newconfig,
 * newpos, and setconfig functions prevent the busy window from changing from
 * its initial state.  The initial state is set in MakeBusy, which makes the 
 * busy window very small and above the upper left corner of the frame window.
 */

static int
newConfigBusy(winInfo, pxcre)
    WinBusy *winInfo;
    XConfigureRequestEvent *pxcre;
{
    if (busyDisabled)
	return 0;
    else
	return WinNewConfigFunc(winInfo, pxcre);
}


static int
newPosBusy(winInfo, x, y)
    WinBusy *winInfo;
    int x, y;
{
    if (busyDisabled)
	return 0;
    else
	return WinNewPosFunc(winInfo, x, y);
}


static int
setConfigBusy(dpy, winInfo)
    Display *dpy;
    WinBusy *winInfo;
{
    if (busyDisabled)
	return 0;
    else {
	WinSetConfigFunc(dpy, winInfo);
	return 1;
    }
}



/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeBusy  -- create the busy window.  The Busy window is exactly the
 *	same size as its parent and gets all keyboard and pointer events
 *	for the frame.  The parent is assumed to be a frame.
 */
WinBusy *
MakeBusy(dpy, par)
Display	*dpy;
WinGenericFrame *par;
{
	WinBusy *w;
	Window win;
        unsigned long valuemask;
        XSetWindowAttributes attributes;

	/* create the associated structure */
	w = MemNew(WinBusy);
	w->core.kind = WIN_BUSY;
	w->class = &classBusy;
	WinAddChild(par,w);
	w->core.children = NULL;
	w->core.client = par->core.client;
	if (busyDisabled) {
	    w->core.x = -10;
	    w->core.y = -10;
	    w->core.width = 1;
	    w->core.height = 1;
	} else {
	    w->core.x = par->fcore.panewin->core.x;	
	    w->core.y = par->fcore.panewin->core.y;
	    w->core.width = par->fcore.panewin->core.width;
	    w->core.height = par->fcore.panewin->core.height;
	}
	w->core.dirtyconfig = CWX | CWY | CWWidth | CWHeight;
	w->core.exposures = NULL;
	w->isFocus = False;

	/* inheirit help from the parent frame */
	w->core.helpstring = par->core.helpstring;

	/* create the window */
        attributes.event_mask = ButtonReleaseMask | ButtonPressMask | 
	    KeyPressMask;
	attributes.cursor = GRV.BusyPointer;
        valuemask = CWEventMask | CWCursor;
        win = ScreenCreateWindow(par->core.client->scrInfo,
				 par->core.self,
                        	 w->core.x, w->core.y,
				 w->core.width, w->core.height,
                        	 valuemask,
                        	 &attributes);

	/* fill out remaining fields */
	w->core.self = win;
	WIInstallInfo(w);
        MapRaised(w);
	PaintVirtualWindow(par);

	return w;
}

void
/* ARGSUSED */
BusyInit(dpy)
Display *dpy;
{
	classBusy.core.kind = WIN_BUSY;
	classBusy.core.xevents[ButtonPress] = NoFocusEventBeep;
	classBusy.core.xevents[ButtonRelease] = NoFocusEventBeep;
	classBusy.core.xevents[KeyPress] = NoFocusEventBeep;
	classBusy.core.xevents[KeyRelease] = NoFocusEventBeep;
	classBusy.core.focusfunc = NULL;
	classBusy.core.drawfunc = NULL;
	classBusy.core.destroyfunc = destroyBusy;
	classBusy.core.selectfunc = NULL;
	classBusy.core.newconfigfunc = newConfigBusy;
	classBusy.core.newposfunc = newPosBusy;
	classBusy.core.setconfigfunc = setConfigBusy;
	classBusy.core.createcallback = NULL;
	classBusy.core.heightfunc = heightfuncBusy;
	classBusy.core.widthfunc = widthfuncBusy;
}
