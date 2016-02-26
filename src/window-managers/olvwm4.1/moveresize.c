/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef IDENT
#ident "@(#)moveresize.c	1.7 olvwm version 09 Feb 1994"
#endif

/*
 * Based on
#ident	"@(#)moveresize.c	26.54	93/06/28 SMI"
 *
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "events.h"
#include "list.h"
#include "globals.h"
#include "group.h"
#include "virtual.h"
#include "error.h"

/* REMIND - find out how to get rid of this */
extern int Resize_width, Resize_height;

typedef enum {
    Unconstrained,		/* resizing not constrained */
    EitherConstrained,		/* constrained, but no direction yet */
    HorizConstrained,		/* constrained horizontally */
    VertConstrained		/* constrained vertically */
} Constraint;


#define	DELTA_INCREASE		(1)
#define	DELTA_DECREASE		(-1)
#define	JUMP_INCREASE		(10)
#define	JUMP_DECREASE		(-10)

#define REDUCE_ROUNDER		5
#define REDUCE_DIVIDER		10

/*
 * drawDouble
 *
 * Draw a thick box on the given window, using the given GC.  The box is drawn
 * using four rectangles.  This technique is used instead of wide lines
 * because this routine is used during animation, and the wide line code of
 * some servers is too slow.
 */

#define defrect(r, X, Y, W, H) \
	(r).x = X, (r).y = Y, (r).width = W, (r).height = H

/* ARGSUSED */
static void
drawDouble(dpy, win, si, gc, x, y, w, h)
    Display    *dpy;
    Window	win;
    ScreenInfo *si;
    GC          gc;
    int         x, y, w, h;
{
    int thick = GRV.RubberBandThickness;
    XRectangle  rects[4];
    int		nrects, doublethick;

    if (w == 0 && h == 0)
	return;

    doublethick = 2 * thick;

    /* if too small for box just draw one solid rect */
    if (w <= doublethick || h <= doublethick) {
	defrect(rects[0], x, y, w, h);
	nrects = 1;
    /* else draw all 4 rects for the box */
    } else {
    	defrect(rects[0], x, y, w, thick);
    	defrect(rects[1], x, y + h - thick, w, thick);
    	defrect(rects[2], x, y + thick, thick, h - doublethick);
    	defrect(rects[3], x + w - thick, y + thick, thick, h - doublethick);
    	nrects = 4;
    }
#ifdef ALLPLANES
    if (si->useAllPlanes)
	XAllPlanesFillRectangles(dpy, win, rects, nrects);
    else
#endif /* ALLPLANES */
	XFillRectangles(dpy, win, gc, rects, nrects);
}

#undef defrect


/* ===== status window ==================================================== */


#define HMARGIN 5
#define VMARGIN 3


typedef struct {
    Display	*dpy;
    Window	win;
    int		ypos;
    int		width;
    int		height;
    XFontStruct	*font;
    ScreenInfo	*scrinfo;
} StatusWinInfo;


static StatusWinInfo *
createStatusWindow(dpy, scrinfo, proto)
    Display *dpy;
    ScreenInfo *scrinfo;
    Text *proto;
{
    XSetWindowAttributes attr;
    StatusWinInfo *sw;
    int	changed, x, y;
    unsigned int w, h;

    sw = MemNew(StatusWinInfo);
    sw->dpy = dpy;
    sw->font = TitleFont;
    sw->height = FontHeight(sw->font) + 2*VMARGIN;
    sw->width = FontWidth(sw->font, proto, TextLen(proto)) + 2*HMARGIN;
    sw->scrinfo = scrinfo;

    if (MatchString(GRV.ResizePosition, "center")) {
	x = (DisplayWidth(dpy, scrinfo->screen) - sw->width) / 2;
	y = (DisplayHeight(dpy, scrinfo->screen) - sw->height) / 2;
    }
    else {
	changed = XParseGeometry(GRV.ResizePosition, &x, &y, &w, &h);
	if (changed & XValue)
	    if (changed & XNegative)
		x = DisplayWidth(dpy, scrinfo->screen) + x - sw->width;
	    else ;
	else x = 0;
	if (changed & YValue)
	    if (changed & YNegative)
		y = DisplayHeight(dpy, scrinfo->screen) + y - sw->height;
	    else ;
	else y = 0;
    }
    sw->ypos = FontAscent(sw->font) + VMARGIN;

    attr.border_pixel = 0;
    attr.colormap = scrinfo->colormap;
    attr.save_under = True;
    sw->win = ScreenCreateWindow(scrinfo, scrinfo->rootid, 
			    x, y, sw->width, sw->height,
			    CWColormap | CWBorderPixel | CWSaveUnder, &attr);
    XMapRaised(dpy, sw->win);
    return sw;
}


static void
paintStatusWindow(sw, text)
    StatusWinInfo *sw;
    Text	  *text;
{
    int		textlen;

    if (sw == NULL)
	return;

    olgx_draw_box(sw->scrinfo->gi[NORMAL_GINFO], sw->win, 0, 0,
		  sw->width, sw->height, OLGX_NORMAL | OLGX_ERASE, True);
    
    textlen = TextLen(text);

    DrawText(sw->dpy, sw->win, sw->font, sw->scrinfo->gc[FOREGROUND_GC], 
		(sw->width - FontWidth(sw->font, text, textlen)) / 2,
		sw->ypos, text, textlen);
}


static void
destroyStatusWindow(sw)
    StatusWinInfo *sw;
{
    if (sw == NULL)
	return;

    ScreenDestroyWindow(sw->scrinfo, sw->win);
    MemFree(sw);
}


/* ===== mouse-based window moving ======================================== */


typedef struct {
    Display		*dpy;
    int			initX, initY;
    int			offX, offY;
    int			curX, curY;
    int			rounder, divider;
    List		*winlist;
    WinGenericFrame	*frame;
    StatusWinInfo	*statuswindow;
    char		*statusfmt;
    Constraint		constraint;
    Bool		dragwin;	    /* true=dragwin, false=dragframe */
    Bool		mouse;
    Bool		AutoRaise;	    /* orig value of autoraise */
/*
 * Virtual Desktop things
 */
    int			check_vdm;
    int			vdm_screenX;
    int			vdm_screenY;
    int			inVDM;
    VirtualDesktop	*vdm;
    int			targetScreenX;
    int			targetScreenY;
    int			initScreenX;
    int			initScreenY;
    Region		region;
    Window		*children;
    unsigned int	num_children;
    int			vdm_stacking_order;
} MoveClosure;


static Bool movewinInterposer();
static void *moveOneWindow();
static void *configOneWindow();
static void *configOneWindowCleanup();
static void *drawOneBox();
static void moveDone();


static void
mouseMovePaintStatus(mstuff, x, y)
    MoveClosure *mstuff;
    int x, y;
{
    Text buf[50];

    if (!mstuff->statuswindow)
	return;

    TextSPrintf(buf, mstuff->statusfmt, x, y);
    paintStatusWindow(mstuff->statuswindow, buf);
}

static int
getWindowStackingOrder(win, mstuff)
    Window	win;
    MoveClosure	*mstuff;
{
int	i;

    for (i = 0; i < mstuff->num_children; i++)
	if (mstuff->children[i] == win)
	    return i;
    return 0;
}

static void *
moveAddRegion(cli, mstuff)
    Client	*cli;
    MoveClosure	*mstuff;

{
Region		new;
XRectangle	rect;

    if (!cli->framewin)
	return NULL;
    if (PANEWINOFCLIENT(cli) == PANEWINOFCLIENT(mstuff->vdm->client))
	return NULL;
    if (cli->screen != mstuff->vdm->client->screen)
	return NULL;
    switch(cli->wmState) {
	case IconicState:
	    if (getWindowStackingOrder(cli->iconwin->core.self, mstuff) <
				mstuff->vdm_stacking_order)
		return NULL;
    	    rect.x = cli->iconwin->core.x;
    	    rect.y = cli->iconwin->core.y;
    	    rect.width = cli->iconwin->core.width;
    	    rect.height = cli->iconwin->core.height;
	    break;
	case NormalState:
	    if (getWindowStackingOrder(cli->framewin->core.self, mstuff) <
				mstuff->vdm_stacking_order)
		return NULL;
    	    rect.x = cli->framewin->core.x;
    	    rect.y = cli->framewin->core.y;
    	    rect.width = cli->framewin->core.width;
    	    rect.height = cli->framewin->core.height;
	    break;
	default:
	    return NULL;
    }
    new = XCreateRegion();
    XUnionRectWithRegion(&rect, new, new);
    XSubtractRegion(mstuff->region, new, mstuff->region);
    XDestroyRegion(new);
    return NULL;
}

/*
 * UserMoveWindows
 *
 * Allow the user to move a window or the set of selected windows.  The
 * "first" parameter must be the button event that initiated the interaction.  
 * The "winInfo" parameter must be the frame or icon on which the action was 
 * initiated.  The external boolean DragWindow controls whether the whole 
 * window is moved or whether the outline is moved.
 *
 * TODO:
 * (1) clean up coordinate systems;
 * (2) implement hysteresis against other windows' edges.
 */
void
UserMoveWindows(cli, first)
    Client *cli;
    XEvent *first;
{
    Display *dpy = cli->dpy;
    List *winlist = NULL_LIST;
    static MoveClosure mstuff;
    Time 	timestamp;
    XRectangle	rect;
    Window	dummy_window;

    {
    /*
     * Set up where the VDM is to allow/prevent moving into it
     */
	VirtualDesktop		*vdm;
	unsigned int	w, h, bw, d;
	int		x, y;
	Window		root;

	vdm = cli->scrInfo->vdm;
	if (vdm && vdm->client->wmState != IconicState && vdm->client != cli) {
	    mstuff.check_vdm = GRV.AllowMoveIntoDesktop;
	    XGetGeometry(dpy, vdm->client->framewin->core.self,
			 &root, &x, &y, &w, &h, &bw, &d);
	    mstuff.vdm_screenX = x;
	    mstuff.vdm_screenY = y;
	    XGetGeometry(dpy, vdm->client->framewin->fcore.panewin->core.self,
			 &root, &x, &y, &w, &h, &bw, &d);
	    mstuff.vdm_screenX += x;
	    mstuff.vdm_screenY += y;
	    mstuff.vdm = vdm;
	    rect.x = mstuff.vdm_screenX;
	    rect.y = mstuff.vdm_screenY;
	    rect.width = w;
	    rect.height = h;
	}
	else {
	    mstuff.vdm = vdm;
	    mstuff.check_vdm = False;
	}
	if (first) {
	    if (first->xmotion.window == PANEWINOFCLIENT(vdm->client))
	        mstuff.inVDM = True;
	    else if (VGetInfo(first->xmotion.window))
	        mstuff.inVDM = True;
	    else mstuff.inVDM = False;
	}
	else mstuff.inVDM = False;
    }

    mstuff.dpy = dpy;
    mstuff.offX = 0;
    mstuff.offY = 0;
    mstuff.constraint = Unconstrained;
    mstuff.rounder = 0;
    mstuff.divider = 1;
    /*
     * Disable Autoraise while dragging -- otherwise, we obscure the moveresize
     * box
     */
    mstuff.AutoRaise = GRV.AutoRaise;
    GRV.AutoRaise = False;
    TimeoutCancel();

    if (cli->wmState == IconicState)
	mstuff.frame = (WinGenericFrame *) cli->iconwin;
    else
	mstuff.frame = (WinGenericFrame *) cli->framewin;

    if (first == NULL || first->type == KeyPress) {
	timestamp = (first == NULL) ? CurrentTime : first->xkey.time;
	mstuff.dragwin = False;
	mstuff.curX = mstuff.initX = mstuff.frame->core.x;
	mstuff.curY = mstuff.initY = mstuff.frame->core.y;
	mstuff.mouse = False;
    } else {
	/* it's a ButtonPress */

	mstuff.curX = mstuff.initX = first->xbutton.x_root;
	mstuff.curY = mstuff.initY = first->xbutton.y_root;
	mstuff.dragwin = GRV.DragWindow;
	mstuff.mouse = True;
	timestamp = first->xbutton.time;

	if (first->xbutton.state & ModMaskMap[MOD_INVERT])
	    mstuff.dragwin = !mstuff.dragwin;

	if (first->xbutton.state & ModMaskMap[MOD_REDUCE]) {
	    mstuff.rounder = REDUCE_ROUNDER;
	    mstuff.divider = REDUCE_DIVIDER;
	}

	if (first->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
	    mstuff.constraint = EitherConstrained;
    }
    mstuff.initScreenX = (mstuff.frame->core.x +
				(int) mstuff.frame->core.width / 2 -
				mstuff.vdm->offsetX) / 
		DisplayWidth(dpy, mstuff.frame->core.client->screen);
    mstuff.initScreenY = (mstuff.frame->core.y +
				(int) mstuff.frame->core.height / 2 -
				mstuff.vdm->offsetY) /
		DisplayHeight(dpy, mstuff.frame->core.client->screen);

    /*
     * This is esoteric: we don't want to allow a drag into an obscured
     * part of the VDM.  If we're dragging the outline, then it's easy,
     * since each event has the dest window, which will be the VDM only
     * when we want it to be.  If we're dragging the window, then the window
     * in the event will be the window's frame, and we have to have another
     * test to see if we're over an unobscured part of the VDM.
     *
     * The best I can come up with now is to create a region equal to the
     * unobscured part of the VDM.  This is probably very slow.
     */
    if (mstuff.check_vdm) {
        if (mstuff.dragwin) {
	    mstuff.region = XCreateRegion();
	    /* rect is set up above */
	    XUnionRectWithRegion(&rect, mstuff.region, mstuff.region);
	    if (!XQueryTree(dpy, WinRootID(mstuff.frame),
			   &dummy_window, &dummy_window,
			   &mstuff.children, &mstuff.num_children))
		mstuff.num_children = 0;
	    mstuff.vdm_stacking_order =
		getWindowStackingOrder(mstuff.vdm->client->framewin->core.self,
					&mstuff);
	    ListApply(ActiveClientList, moveAddRegion, &mstuff);
	}
	else mstuff.region = NULL;
    }

    /*
     * If we're dragging the window, and raise-on-move is set, raise it now so
     * that the user drags it around after it's been raised.  Raise only this
     * window, even if several are selected.
     */  
    if (GRV.RaiseOnMove && mstuff.dragwin)
	XRaiseWindow(dpy, mstuff.frame->core.self);

    /*
     * Generate the list of windows to be moved.  If the initial window is 
     * selected, we're moving the selection; otherwise, we're moving just this 
     * window.
     */
    if (IsSelected(cli)) {
	Client *c = (Client *) 0;
	int	allsticky = True;

	while (c = EnumSelections(c)) {
	    if (c->wmState == IconicState)
		winlist = ListCons(c->iconwin, winlist);
	    else
		winlist = ListCons(c->framewin, winlist);
	    /*
	     * Can't move sticky windows into the VDM.  Thus, if all windows
	     * are sticky, we don't allow any move into the VDM, since otherwise
	     * its too confusing
	     */
	    allsticky = allsticky & c->sticky;
	}
	if (allsticky)
	    mstuff.check_vdm = False;
    } else {
	winlist = ListCons(mstuff.frame, NULL_LIST);
	if (cli->sticky)
	    mstuff.check_vdm = False;
    }
    mstuff.winlist = winlist;

    /* Grab the pointer to change the cursor and confine to the root window. */

    if (XGrabPointer(dpy, cli->scrInfo->rootid, True,
		     ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		     GrabModeAsync, GrabModeAsync, cli->scrInfo->rootid,
		     GRV.MovePointer, timestamp) != GrabSuccess)
    {
	ErrorWarning(GetString("failed to grab pointer"));
	return;
    }

    if (XGrabKeyboard(dpy, cli->scrInfo->rootid, False,
		      GrabModeAsync, GrabModeAsync,
		      timestamp) != GrabSuccess)
    {
	/* note: not fatal */
	ErrorWarning(GetString("failed to grab keyboard"));
    }

    InstallInterposer(movewinInterposer, &mstuff);

    if (GRV.ShowMoveGeometry) {
	Text	proto[50];

	mstuff.statusfmt = GetString("location: %4d , %4d");
	TextSPrintf(proto, mstuff.statusfmt, 9999, 9999);
	mstuff.statuswindow = createStatusWindow(dpy, cli->scrInfo, proto);
    }
    else mstuff.statuswindow = NULL;

    /*
     * If we're dragging the outlines, we must also grab the server and draw 
     * the initial set of bounding boxes.
     */
    if (!mstuff.dragwin) {
	XGrabServer(dpy);
	(void) ListApply(mstuff.winlist, drawOneBox, &mstuff);
    }
}


void
moveUpdate(mstuff, event)
    MoveClosure *mstuff;
    XEvent	*event;
{
    if (!mstuff->dragwin)
	(void) ListApply(mstuff->winlist, drawOneBox, mstuff);
	
    if (mstuff->check_vdm && event) {
	if (!mstuff->region) {
	    if (event->xmotion.window == PANEWINOFCLIENT(mstuff->vdm->client))
	        mstuff->inVDM = True;
	    /*
	     * The events on the virtual windows are reported relative to
	     * themselves; so if its a virtual window, we're in the VDM
	     */
	    else if (VGetInfo(event->xmotion.window))
	        mstuff->inVDM = True;
	    else if (mstuff->dragwin) {
	        /*
	         * If we're dragging the window, then we took care of setting
		 * the VDM when we do the regioning stuff.
		 * REMIND:  Can we ever get here?
	         */
	    }    
	    else mstuff->inVDM = False;
	}
	else mstuff->inVDM = XPointInRegion(mstuff->region,
				event->xmotion.x_root, event->xmotion.y_root);
    }

    if (mstuff->inVDM) {
        mstuff->offX = (mstuff->curX - mstuff->vdm_screenX) *
			(mstuff->vdm->resources->scale / mstuff->divider) -
			mstuff->initX + mstuff->vdm->offsetX;
        mstuff->offY = (mstuff->curY - mstuff->vdm_screenY) *
			(mstuff->vdm->resources->scale / mstuff->divider) -
			mstuff->initY + mstuff->vdm->offsetY;
    }
    else {
        mstuff->offX =
	    (mstuff->curX - mstuff->initX + mstuff->rounder) / mstuff->divider;
        mstuff->offY =
	    (mstuff->curY - mstuff->initY + mstuff->rounder) / mstuff->divider;
    }

    if (mstuff->constraint == EitherConstrained) {
	if (ABS(mstuff->offX) > ABS(mstuff->offY))
	    mstuff->constraint = HorizConstrained;
	else
	    mstuff->constraint = VertConstrained;
    }

    if (mstuff->constraint == HorizConstrained) {
	mstuff->offY = 0;
    } else if (mstuff->constraint == VertConstrained) {
	mstuff->offX = 0;
    }

    if (mstuff->dragwin)
	(void) ListApply(mstuff->winlist, moveOneWindow, mstuff);
    else
	(void) ListApply(mstuff->winlist, drawOneBox, mstuff);
}


void
moveKeyDelta(mstuff, dx, dy)
    MoveClosure *mstuff;
    int dx, dy;
{
    mstuff->constraint = Unconstrained;

    if (mstuff->mouse) {
	mstuff->initX -= dx;
	mstuff->initY -= dy;
    } else {
	mstuff->curX += dx;
	mstuff->curY += dy;
    }
    moveUpdate(mstuff, (XEvent *) NULL);
}


/*
 * movewinInterposer
 *
 * Interposer function for moving windows.  Moves the list of windows on each 
 * MotionNotify; releases interposition on ButtonRelease.
 */
/*ARGSUSED*/
static int
movewinInterposer(dpy, event, w, mstuff)
    Display *dpy;
    XEvent *event;
    WinGeneric *w;
    MoveClosure *mstuff;
{
    XEvent nextevent;
    SemanticAction action;

    switch (event->type) {
    case ButtonPress:
	/* ignore if buttons are already down */
	if (!FirstButtonDown(event))
	    break;

	switch (ResolveMouseBinding(dpy, event,
		    ModMaskMap[MOD_REDUCE] | ModMaskMap[MOD_CONSTRAIN]))
	{
	case ACTION_SELECT:
	    mstuff->mouse = True;
	    mstuff->curX = event->xbutton.x_root;
	    mstuff->curY = event->xbutton.y_root;
	    mstuff->initX = mstuff->curX - mstuff->offX;
	    mstuff->initY = mstuff->curY - mstuff->offY;

	    if (event->xbutton.state & ModMaskMap[MOD_REDUCE]) {
		mstuff->rounder = REDUCE_ROUNDER;
		mstuff->divider = REDUCE_DIVIDER;
	    }

	    if (event->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
		mstuff->constraint = EitherConstrained;
	    break;

	default:	/* otherwise, abort the move operation */
	    mstuff->curX = mstuff->initX;
	    mstuff->curY = mstuff->initY;
	    moveUpdate(mstuff, event);
	    moveDone(mstuff);
	}
	break;

    case ButtonRelease:
	if (AllButtonsUp(event))
	    moveDone(mstuff);
	break;

    case MotionNotify:
	/* if the event is off the screen, ignore it */
        if (!event->xmotion.same_screen)
	    break;

	if (!mstuff->mouse)
	    break;

	/*
	 * Motion compression.  If the next event is a MotionNotify,
	 * ignore this one.
	 */
	if (XEventsQueued(dpy, QueuedAfterReading) > 0 &&
	    (XPeekEvent(dpy,&nextevent), nextevent.type == MotionNotify))
	    break;

	mstuff->curX = event->xmotion.x_root;
	mstuff->curY = event->xmotion.y_root;
	moveUpdate(mstuff, event);
	break;

    case KeyPress:
	action = FindKeyboardAction(dpy, event);

	switch (action) {
	case ACTION_UP:
	    moveKeyDelta(mstuff,0,DELTA_DECREASE);
	    break;
	case ACTION_DOWN:
	    moveKeyDelta(mstuff,0,DELTA_INCREASE);
	    break;
	case ACTION_LEFT:
	    moveKeyDelta(mstuff,DELTA_DECREASE,0);
	    break;
	case ACTION_RIGHT:
	    moveKeyDelta(mstuff,DELTA_INCREASE,0);
	    break;
	case ACTION_JUMP_UP:
	    moveKeyDelta(mstuff,0,JUMP_DECREASE);
	    break;
	case ACTION_JUMP_DOWN:
	    moveKeyDelta(mstuff,0,JUMP_INCREASE);
	    break;
	case ACTION_JUMP_LEFT:
	    moveKeyDelta(mstuff,JUMP_DECREASE,0);
	    break;
	case ACTION_JUMP_RIGHT:
	    moveKeyDelta(mstuff,JUMP_INCREASE,0);
	    break;
	case ACTION_EXEC_DEFAULT:
	    moveDone(mstuff);
	    break;
	case ACTION_STOP:
	    mstuff->curX = mstuff->initX;
	    mstuff->curY = mstuff->initY;
	    mstuff->inVDM = False;
	    moveUpdate(mstuff, (XEvent *) NULL);
	    moveDone(mstuff);
	    break;
	
	case ACTION_FRONT:
	    KeyFrontFocus(dpy, event);
	    break;

	default:
	    if (FindModifierMask(event->xkey.keycode) ==
		ModMaskMap[MOD_CONSTRAIN])
	    {
		if (mstuff->mouse) {
		    mstuff->constraint = EitherConstrained;
		    moveUpdate(mstuff, (XEvent *) NULL);
		}
	    } else {
		KeyBeep(dpy, event);
	    }
	    break;
	}
	break;

    case KeyRelease:
	if (FindModifierMask(event->xkey.keycode) ==
	    ModMaskMap[MOD_CONSTRAIN])
	{
	    mstuff->constraint = Unconstrained;
	    moveUpdate(mstuff, (XEvent *) NULL);
	}
	break;

    /* Send out expose's immediately */
    case Expose:
	return DISPOSE_DISPATCH;

    default:
	return DISPOSE_DEFER;
    }

    return DISPOSE_USED;
}

/* windowOff -- function to determine how far a window should be offset
 * given a pointer offset.  Returns both X and Y, by reference.
 */
static void
windowOff(win, mstuff, pox, poy)
WinGenericFrame *win;
MoveClosure *mstuff;
int *pox, *poy;
{
    int pixw = DisplayWidth(win->core.client->dpy, win->core.client->screen);
    int hpoint;
    int pixy = DisplayHeight(win->core.client->dpy, win->core.client->screen);

    if (mstuff->offX == 0)
    {
	*pox = 0;
    }
    else if (mstuff->offX > 0)
    {
        hpoint = pixw - win->core.x - win->core.width;
    	if (!mstuff->inVDM && (mstuff->offX >= hpoint) &&
			(mstuff->offX <= hpoint + GRV.EdgeThreshold))
	    *pox = hpoint;
	else {
	    hpoint = mstuff->vdm->absoluteWidth + mstuff->vdm->offsetX -
		     	win->core.x - win->core.width;
	    if (mstuff->offX >= hpoint + win->core.width - Resize_width)
	        *pox = hpoint + win->core.width - Resize_width;
	    else
	        *pox = mstuff->offX;
	}
    }
    else if (mstuff->offX < 0)
    {
    	if (!mstuff->inVDM && (mstuff->offX <= -win->core.x) &&
			(mstuff->offX >= (-win->core.x - GRV.EdgeThreshold)))
	    *pox = -win->core.x;
	else if (mstuff->offX <= (-win->core.x - win->core.width +
			Resize_width + mstuff->vdm->offsetX))
	    *pox = (-win->core.x - win->core.width +
			Resize_width + mstuff->vdm->offsetX);
	else
	    *pox = mstuff->offX;
    }

    if (mstuff->offY == 0)
    {
	*poy = 0;
    }
    else if (mstuff->offY > 0)
    {
        hpoint = pixy - win->core.y - win->core.height;
    	if (!mstuff->inVDM && (mstuff->offY >= hpoint) &&
			(mstuff->offY <= hpoint + GRV.EdgeThreshold))
	    *poy = hpoint;
	else {
	    hpoint = mstuff->vdm->absoluteHeight + mstuff->vdm->offsetY -
			win->core.y - win->core.height;
	    if (mstuff->offY >= hpoint + win->core.height - Resize_height)
	    	*poy = hpoint + win->core.height - Resize_height;
	    else
	        *poy = mstuff->offY;
	}
    }
    else if (mstuff->offY < 0)
    {
    	if (!mstuff->inVDM && (mstuff->offY <= -win->core.y) &&
			(mstuff->offY >= (-win->core.y - GRV.EdgeThreshold)))
	    *poy = -win->core.y;
	else if (mstuff->offY <= (-win->core.y - win->core.height +
			Resize_height + mstuff->vdm->offsetY))
	    *poy = (-win->core.y - win->core.height + Resize_height +
			mstuff->vdm->offsetY);
	else
	    *poy = mstuff->offY;
    }
}

/*
 * moveOneWindow
 *
 * Apply function for window moving animation.  Draws a window outline or 
 * actually moves the window, depending on DragWindow.
 */
static void *
moveOneWindow(win, mstuff)
    WinGenericFrame *win;
    MoveClosure *mstuff;
{
    int offX, offY;

    windowOff(win, mstuff, &offX, &offY);
    XMoveWindow(mstuff->dpy, win->core.self,
		win->core.x + offX,
		win->core.y + offY);
    XMoveWindow(mstuff->dpy, win->core.virtual,
		(win->core.x + offX - mstuff->vdm->offsetX) /
				mstuff->vdm->resources->scale,
		(win->core.y + offY - mstuff->vdm->offsetY) /
				mstuff->vdm->resources->scale);
    if (win == mstuff->frame)
	mouseMovePaintStatus(mstuff, win->core.x + offX, win->core.y + offY);
    return (void *) 0;
}


/*
 * drawOneBox
 *
 * Apply function for drawing XOR boxes.  Draws a double-width rectangle 
 * around the outline of a single window.
 */
static void *
drawOneBox(w, mstuff)
    WinGenericFrame *w;
    MoveClosure *mstuff;
{
    int offX, offY;

    windowOff(w, mstuff, &offX, &offY);
    drawDouble(mstuff->dpy, w->core.client->scrInfo->rootid,
	       w->core.client->scrInfo, WinGC(w,ROOT_GC),
	       w->core.x + offX, w->core.y + offY,
	       w->core.width, w->core.height);
    drawDouble(mstuff->dpy, PANEWINOFCLIENT(mstuff->vdm->client),
	       w->core.client->scrInfo, WinGC(w,ROOT_GC),
		(w->core.x + offX - mstuff->vdm->offsetX) /
				mstuff->vdm->resources->scale,
		(w->core.y + offY - mstuff->vdm->offsetY) /
				mstuff->vdm->resources->scale,
		w->core.width / mstuff->vdm->resources->scale,
		w->core.height / mstuff->vdm->resources->scale);

    if (w == mstuff->frame)
	mouseMovePaintStatus(mstuff, w->core.x + offX, w->core.y + offY);

    return (void *) 0;
}    


/*
 * doconfigOneWindow
 *
 * Apply function for calling a moved window's configfunc.
 *
 */
static void *
doConfigOneWindow(win, mstuff)
    WinGenericFrame *win;
    MoveClosure *mstuff;
{
    int offX, offY;
    int	dw, dh;

    if (ListIsAMember(win, mstuff->winlist)) {
        windowOff(win, mstuff, &offX, &offY);
        GFrameSetConfig(win, win->core.x + offX, win->core.y + offY,
		        win->core.width, win->core.height);
    }
    else {
	/* Else we're moving a dependent window without having selected it --
	 * ie VirtualMoveGroup is True and we're moving into the VDM.  So
	 * keep the relative screen position of this window
	 */
	dw = DisplayWidth(win->core.client->dpy, win->core.client->screen);
	dh = DisplayHeight(win->core.client->dpy, win->core.client->screen);
	offX = mstuff->targetScreenX * dw;
	offY = mstuff->targetScreenY * dh;
        GFrameSetConfig(win, win->core.x + offX, win->core.y + offY,
		        win->core.width, win->core.height);
    }
    if (GRV.RaiseOnMove)
	RaiseWindow(win);
    return (void *) 0;
}

static void *
configOneClient(cli, mstuff)
    Client	*cli;
    MoveClosure	*mstuff;

{
    doConfigOneWindow((WinGenericFrame *) cli->framewin, mstuff);
    doConfigOneWindow((WinGenericFrame *) cli->iconwin, mstuff);
    cli->flags |= CLMoved;
    return (void *) 0;
}

static void *
configOneWindow(win, mstuff)
    WinGenericFrame *win;
    MoveClosure *mstuff;
{
int	dw, dh;
int 	offX, offY;
int	newScreenX, newScreenY;
Client	*leader;

    if (win->core.client->flags & CLMoved)
	return (void *) 0;
    dw = DisplayWidth(win->core.client->dpy, win->core.client->screen);
    dh = DisplayHeight(win->core.client->dpy, win->core.client->screen);
    windowOff(win, mstuff, &offX, &offY);
    switch(win->core.client->groupmask) {
	case GROUP_LEADER:
	    leader = win->core.client;
	    break;
	case GROUP_DEPENDENT:
	    leader = GroupLeader(win->core.client->groupid);
	    if (!leader) {
		doConfigOneWindow(win, mstuff);
		return (void *) 0;
	    }
	    break;
	case GROUP_INDEPENDENT:
	    doConfigOneWindow(win, mstuff);
	    return (void *) 0;
    }
    if (GRV.VirtualMoveGroups) {
	newScreenX = (win->core.x + (win->core.width / 2) +
				offX - mstuff->vdm->offsetX) / dw;
	mstuff->targetScreenX = newScreenX - mstuff->initScreenX;
	newScreenY = (win->core.y + (win->core.height / 2) +
				offY - mstuff->vdm->offsetY) / dh;
	mstuff->targetScreenY = newScreenY - mstuff->initScreenY;
	if (mstuff->targetScreenY == 0 && mstuff->targetScreenX == 0)
	    doConfigOneWindow(win, mstuff);
	else GroupApply(leader->groupid, configOneClient, mstuff,
			GROUP_LEADER | GROUP_DEPENDENT);
    }
    else doConfigOneWindow(win, mstuff);
    return (void *) 0;
}

static void *
doConfigOneClientCleanup(cli)
    Client	*cli;

{
    cli->flags &= ~CLMoved;
    return (void *) 0;
}

static void *
configOneWindowCleanup(win)
    WinGeneric	*win;
{
Client	*leader;

    if (GRV.VirtualMoveGroups) {
	leader = GroupLeader(win->core.client->groupid);
	if (!leader) {
	    doConfigOneClientCleanup(win->core.client);
	    return (void *) 0;
	}
	GroupApply(leader->groupid, doConfigOneClientCleanup, 0,
			GROUP_LEADER | GROUP_DEPENDENT);
    }
    else win->core.client->flags &= ~CLMoved;
    return (void *) 0;
}

/*
 * moveDone
 *
 * Cleanup function for window moving.  Releases grabs, uninstalls 
 * interposition, cleans up.
 */
static void
moveDone(mstuff)
    MoveClosure *mstuff;
{
    /*
     * If we're dragging the outlines, we must ungrab the server and undraw 
     * the last set of boxes.
     */
    if (!mstuff->dragwin) {
	(void) ListApply(mstuff->winlist, drawOneBox, mstuff);
	XUngrabServer(mstuff->dpy);
    }
    XUngrabPointer(mstuff->dpy, CurrentTime);
    XUngrabKeyboard(mstuff->dpy, CurrentTime);

    /*
     * If we moved the outline (not the whole window) cause the window to be
     * raised at the same time it is moved.  Raise just the frame the user
     * started on, even if several are selected.
     */
    if (GRV.RaiseOnMove && !mstuff->dragwin)
	GFrameSetStack(mstuff->frame, CWStackMode, Above, None);

    (void) ListApply(mstuff->winlist, configOneWindow, mstuff);
    (void) ListApply(mstuff->winlist, configOneWindowCleanup, 0);
    ListDestroy(mstuff->winlist);
    if (mstuff->region) {
	XDestroyRegion(mstuff->region);
	if (mstuff->num_children)
	    XFree((char *) mstuff->children);
	mstuff->region = NULL;
    }

    destroyStatusWindow(mstuff->statuswindow);
    mstuff->statuswindow = NULL;
    GRV.AutoRaise = mstuff->AutoRaise;
    UninstallInterposer();
}


/* ===== mouse-based resizing ============================================= */


/*
 * Note on use of gravity values: in this section, the gravity field is used 
 * to denote the window edge or corner that is being moved.  It's not
 * being used as "gravity" in the usual sense, which is the location that is 
 * being held constant.
 *
 * TODO:
 * (1) implement screen edge hysteresis for resize+move mode (meta key);
 * (2) implement window edge hysteresis.
 */

/*
 * The following enum is arranged specifically so that the values can be 
 * tested with bit operations.  The one-bit indicates down or right if one, up 
 * or left if zero.  The two-bit indicates vertical if one, horizontal if zero.
 * The four-bit indicates a jump if one, normal if zero.
 */
typedef enum {
    RS_LEFT = 0,
    RS_RIGHT,
    RS_UP,
    RS_DOWN,
    RS_J_LEFT,
    RS_J_RIGHT,
    RS_J_UP,
    RS_J_DOWN
} ResizeAction;

#define RS_ISRIGHT	    (1<<0)
#define RS_ISDOWN	    (1<<0)
#define RS_ISVERT	    (1<<1)
#define RS_ISJUMP	    (1<<2)

#define RS_JUMPMULT	    10			/* jump multiplier */

struct {
    int x, y;
} ResizeDeltas[] = {
    {           -1,            0 },	/* left */
    {            1,            0 },	/* right */
    {            0,           -1 },	/* up */
    {            0,            1 },	/* down */
    { -RS_JUMPMULT,            0 },	/* jump left */
    {  RS_JUMPMULT,            0 },	/* jump right */
    {            0, -RS_JUMPMULT },	/* jump up */
    {            0,  RS_JUMPMULT }	/* jump down */
};


typedef struct {
    Client		*cli;
    Constraint		constraint;
    Bool		drawn;
    Bool		moving;
    Bool		useAspect;
    Bool		baseProvided;
    int			origX, origY;
    int			curX, curY;	/* current mouse position */
    int			winX, winY;	/* current window position */
    int			winW, winH;	/* current window height */
    int			minW, minH;
    int			maxW, maxH;
    int			incW, incH;
    int			minAspectX, minAspectY;
    int			maxAspectX, maxAspectY;
    int			baseW, baseH;
    int			borderW, borderH;   /* size of frame border */
    void		(*callback)();
    void		*cbarg;
    StatusWinInfo	*statuswindow;
    char		*statusfmt;
    int			gravity;		/* see note above */
    Bool		mouse;			/* using mouse? */
} ResizeClosure;


/*
 * Macro for adjusting the size of a window to its resize increment.  First,
 * assigns diff to be the difference between the size and the next lesser
 * incremental size.  If diff is greater than half the incremental
 * size, adjust the size upward to the next greater increment, otherwise 
 * adjust downward.  THIS MACRO ALTERS ITS FIRST ARGUMENT.
 *
 * size is the window size to be adjusted
 * base is base size, to be subtracted off before modulo is done
 * i is the incremental size
 */
#define INCADJ(size, base, i)						\
    {									\
	int diff;							\
	diff = ((size) - (base)) % (i);					\
	(size) += (diff > (i)/2) ? (i)-diff : -diff;			\
    }


static void
resizeDraw(rstuff)
    ResizeClosure *rstuff;
{
    drawDouble(rstuff->cli->dpy, WinRootID(rstuff->cli->framewin),
	       rstuff->cli->scrInfo,
	       WinGC(rstuff->cli->framewin, ROOT_GC),
	       rstuff->winX, rstuff->winY, rstuff->winW, rstuff->winH);
    drawDouble(rstuff->cli->dpy,
	       PANEWINOFCLIENT(rstuff->cli->scrInfo->vdm->client),
	       rstuff->cli->scrInfo,
		WinGC(rstuff->cli->framewin, ROOT_GC),
		(rstuff->winX - rstuff->cli->scrInfo->vdm->offsetX) /
				rstuff->cli->scrInfo->vdm->resources->scale,
		(rstuff->winY - rstuff->cli->scrInfo->vdm->offsetY) /
				rstuff->cli->scrInfo->vdm->resources->scale,
		rstuff->winW / rstuff->cli->scrInfo->vdm->resources->scale,
		rstuff->winH / rstuff->cli->scrInfo->vdm->resources->scale);
}


static void
resizePaintStatus(rstuff)
    ResizeClosure *rstuff;
{
    Text buf[30];
    int w, h;

    if (!rstuff->statuswindow)
	return;

    w = rstuff->winW - rstuff->borderW;
    h = rstuff->winH - rstuff->borderH;

    if (rstuff->baseProvided) {
	w -= rstuff->baseW;
	h -= rstuff->baseH;
    }
	
    TextSPrintf(buf, rstuff->statusfmt, w / rstuff->incW, h / rstuff->incH);
    paintStatusWindow(rstuff->statuswindow, buf);
}


static void
/* ARGSUSED */
resizeDone(dpy, e, w, rstuff, doit)
    Display *dpy;
    XEvent *e;
    WinGeneric *w;
    ResizeClosure *rstuff;
    Bool doit;
{
    if (rstuff->drawn)
	resizeDraw(rstuff);

    XUngrabPointer(dpy, e->xbutton.time);
    XUngrabKeyboard(dpy, e->xbutton.time);
    XUngrabServer(dpy);

    if (doit) {
	if (GRV.RaiseOnResize)
	    GFrameSetStack(rstuff->cli->framewin, CWStackMode, Above, None);
	GFrameSetConfig(rstuff->cli->framewin, rstuff->winX, rstuff->winY,
			rstuff->winW, rstuff->winH);
    }

    if (rstuff->callback != NULL)
	(*rstuff->callback)(dpy, rstuff->cbarg);

    destroyStatusWindow(rstuff->statuswindow);
    rstuff->statuswindow = NULL;
    UninstallInterposer();
}


static void
resizeMotion(rstuff, x, y)
    ResizeClosure *rstuff;
    int x, y;
{
    int dx, dy, dW, dH, newW, newH;

    dx = x - rstuff->curX;
    dy = y - rstuff->curY;

    if (ABS(dx) <= GRV.MoveThreshold && ABS(dy) <= GRV.MoveThreshold &&
	!rstuff->drawn)
	return;
    
    if (rstuff->mouse) {
	if (rstuff->constraint == EitherConstrained) {
	    if (ABS(rstuff->origX - x) > ABS(rstuff->origY - y))
		rstuff->constraint = HorizConstrained;
	    else
		rstuff->constraint = VertConstrained;
	}

	if (rstuff->constraint == HorizConstrained) {
	    y = rstuff->origY;
	    dy = y - rstuff->curY;
	} else if (rstuff->constraint == VertConstrained) {
	    x = rstuff->origX;
	    dx = x - rstuff->curX;
	}
    }

    newW = rstuff->winW;
    newH = rstuff->winH;
    if (rstuff->moving) {
	if (dx == 0 && dy == 0)
	    return;
	if (rstuff->drawn)
	    resizeDraw(rstuff);
	rstuff->winX += dx;
	rstuff->winY += dy;
	rstuff->curX += dx;
	rstuff->curY += dy;
	resizeDraw(rstuff);
	rstuff->drawn = True;
	return;
    }

    switch (rstuff->gravity) {
    case NorthWestGravity:
	newW -= dx;
	newH -= dy;
	break;
    case NorthGravity:
	newH -= dy;
	break;
    case NorthEastGravity:
	newW += dx;
	newH -= dy;
	break;
    case WestGravity:
	newW -= dx;
	break;
    case CenterGravity:
	break;
    case EastGravity:
	newW += dx;
	break;
    case SouthWestGravity:
	newW -= dx;
	newH += dy;
	break;
    case SouthGravity:
	newH += dy;
	break;
    case SouthEastGravity:
	newW += dx;
	newH += dy;
	break;
    }

    /*
     * Convert from frame size to pane size, apply the constraints, then 
     * convert back to frame size.
     */

    newW -= rstuff->borderW;
    newH -= rstuff->borderH;

    INCADJ(newW, rstuff->baseW, rstuff->incW);
    INCADJ(newH, rstuff->baseH, rstuff->incH);

    newW = MAX(rstuff->minW, MIN(rstuff->maxW, newW));
    newH = MAX(rstuff->minH, MIN(rstuff->maxH, newH));

    if (rstuff->useAspect &&
	newW * rstuff->maxAspectY > newH * rstuff->maxAspectX)
    {
	if (rstuff->maxAspectX > rstuff->maxAspectY) {
	    /* max aspect is wider than tall; increase height. */
	    newH = (newW * rstuff->maxAspectY) / rstuff->maxAspectX;
	    if (newH > rstuff->maxH) {
		newH = rstuff->maxH;
		newW = (newH * rstuff->maxAspectX) / rstuff->maxAspectY;
	    }
	} else {
	    /* max aspect is taller than wide; decrease width. */
	    newW = (newH * rstuff->maxAspectX) / rstuff->maxAspectY;
	    if (newW < rstuff->minW) {
		newW = rstuff->minW;
		newH = (newW * rstuff->maxAspectY) / rstuff->maxAspectX;
	    }
	}
    }

    if (rstuff->useAspect &&
	newW * rstuff->minAspectY < newH * rstuff->minAspectX)
    {
	if (rstuff->minAspectX > rstuff->minAspectY) {
	    /* min aspect is wider than tall; decrease height. */
	    newH = (newW * rstuff->minAspectY) / rstuff->minAspectX;
	    if (newH < rstuff->minH) {
		newH = rstuff->minH;
		newW = (newH * rstuff->minAspectX) / rstuff->minAspectY;
	    }
	} else {
	    /* min aspect is taller than wide; increase width. */
	    newW = (newH * rstuff->minAspectX) / rstuff->minAspectY;
	    if (newW > rstuff->maxW) {
		newW = rstuff->maxW;
		newH = (newW * rstuff->minAspectY) / rstuff->minAspectX;
	    }
	}
    }

    newW += rstuff->borderW;
    newH += rstuff->borderH;

    /*
     * Calculate the change in size (if any) and update the window's origin
     * (winX, winY) depending on which corner is being moved.  Also, update
     * the virtual pointer location (curX, curY).  Don't draw anything if the
     * size hasn't changed.
     */

    dW = newW - rstuff->winW;
    dH = newH - rstuff->winH;

    if (dW == 0 && dH == 0)
	return;

    if (rstuff->drawn)
	resizeDraw(rstuff);

    switch (rstuff->gravity) {
    case NorthWestGravity:
	rstuff->winX -= dW;
	rstuff->winY -= dH;
	rstuff->curX -= dW;
	rstuff->curY -= dH;
	break;
    case NorthGravity:
	rstuff->winX -= dW / 2;
	rstuff->winY -= dH;
	rstuff->curY -= dH;
	break;
    case NorthEastGravity:
	rstuff->winY -= dH;
	rstuff->curX += dW;
	rstuff->curY -= dH;
	break;
    case WestGravity:
	rstuff->winX -= dW;
	rstuff->winY -= dH / 2;
	rstuff->curX -= dW;
	break;
    case CenterGravity:
	rstuff->winX -= dW / 2;
	rstuff->winY -= dH / 2;
	break;
    case EastGravity:
	rstuff->winY -= dH / 2;
	rstuff->curX += dW;
	break;
    case SouthWestGravity:
	rstuff->winX -= dW;
	rstuff->curX -= dW;
	rstuff->curY += dH;
	break;
    case SouthGravity:
	rstuff->winX -= dW / 2;
	rstuff->curY += dH;
	break;
    case SouthEastGravity:
	rstuff->curX += dW;
	rstuff->curY += dH;
	break;
    }

    rstuff->winW = newW;
    rstuff->winH = newH;

    resizeDraw(rstuff);
    resizePaintStatus(rstuff);
    rstuff->drawn = True;
}


void
resizeDelta(rstuff, action)
    ResizeClosure *rstuff;
    ResizeAction action;
{
    int dx, dy;

    if (rstuff->mouse)
	return;

    if (action & RS_ISVERT) {
	switch (rstuff->gravity) {
	case WestGravity:
	case CenterGravity:
	case EastGravity:
	    rstuff->gravity += (action & RS_ISDOWN) ? 3 : -3;
	    break;
	}
    } else {
	switch (rstuff->gravity) {
	case NorthGravity:
	case CenterGravity:
	case SouthGravity:
	    rstuff->gravity += (action & RS_ISRIGHT) ? 1 : -1;
	    break;
	}
    }

    /* generate a delta vector based on which edge is being moved */

    dx = ResizeDeltas[action].x;
    dy = ResizeDeltas[action].y;

    dx *= rstuff->incW;
    dy *= rstuff->incH;

    resizeMotion(rstuff, rstuff->curX + dx, rstuff->curY + dy);
}


static int
resizeInterposer(dpy, e, w, rstuff)
    Display *dpy;
    XEvent *e;
    WinGeneric *w;
    ResizeClosure *rstuff;
{
    unsigned int mask;
    SemanticAction action;
    XEvent nextevent;

    switch (e->type) {
    case ButtonPress:
	if (!FirstButtonDown(e))
	    break;
	switch (ResolveMouseBinding(dpy, e,
		    ModMaskMap[MOD_CONSTRAIN] | ModMaskMap[MOD_INVERT]))
	{
	case ACTION_SELECT:
	    rstuff->mouse = True;
	    rstuff->curX = rstuff->origX = e->xbutton.x_root;
	    rstuff->curY = rstuff->origY = e->xbutton.y_root;
	    if (e->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
		rstuff->constraint = EitherConstrained;
	    if (e->xbutton.state & ModMaskMap[MOD_INVERT])
		rstuff->moving = True;

	    rstuff->gravity = NorthWestGravity;
	    if (e->xbutton.y_root > rstuff->winY + (rstuff->winH / 2))
		rstuff->gravity = SouthWestGravity;
	    if (e->xbutton.x_root > rstuff->winX + (rstuff->winW / 2))
		rstuff->gravity += 2;	/* turns any west into any east */

	    resizeMotion(rstuff, e->xbutton.x_root, e->xbutton.y_root);
	    break;
	default:
	    resizeDone(dpy, e, w, rstuff, False);
	    break;
	}
	break;
	
    case ButtonRelease:
	if (AllButtonsUp(e))
	    resizeDone(dpy, e, w, rstuff, True);
	break;

    case MotionNotify:
	if (!e->xmotion.same_screen || !rstuff->mouse)
	    break;
	
	/*
	 * Motion compression.  If the next event is a MotionNotify,
	 * ignore this one.
	 */
	if (XEventsQueued(dpy, QueuedAfterReading) > 0 &&
	    (XPeekEvent(dpy, &nextevent), nextevent.type == MotionNotify))
	    break;

	resizeMotion(rstuff, e->xmotion.x_root, e->xmotion.y_root);
	break;

    case KeyPress:
	action = FindKeyboardAction(dpy, e);

	switch (action) {

	case ACTION_STOP:
	    resizeDone(dpy, e, w, rstuff, False);
	    break;

	case ACTION_UP:		resizeDelta(rstuff, RS_UP);	    break;
	case ACTION_DOWN:	resizeDelta(rstuff, RS_DOWN);	    break;
	case ACTION_LEFT:	resizeDelta(rstuff, RS_LEFT);	    break;
	case ACTION_RIGHT:	resizeDelta(rstuff, RS_RIGHT);	    break;
	case ACTION_JUMP_UP:	resizeDelta(rstuff, RS_J_UP);	    break;
	case ACTION_JUMP_DOWN:	resizeDelta(rstuff, RS_J_DOWN);	    break;
	case ACTION_JUMP_LEFT:	resizeDelta(rstuff, RS_J_LEFT);	    break;
	case ACTION_JUMP_RIGHT:	resizeDelta(rstuff, RS_J_RIGHT);    break;

	case ACTION_EXEC_DEFAULT:
	    resizeDone(dpy, e, w, rstuff, True);
	    break;

	default:
	    mask = FindModifierMask(e->xkey.keycode);
	    if (mask == ModMaskMap[MOD_CONSTRAIN] && rstuff->mouse) {
		rstuff->constraint = EitherConstrained;
		resizeMotion(rstuff, e->xkey.x_root, e->xkey.y_root);
	    } else if (mask == ModMaskMap[MOD_INVERT]) {
		rstuff->moving = True;
	    } else {
		KeyBeep(dpy, e);
	    }
	    break;
	}
	break;

    case KeyRelease:
	mask = FindModifierMask(e->xkey.keycode);
	if (mask == ModMaskMap[MOD_CONSTRAIN] && rstuff->mouse) {
	    rstuff->constraint = Unconstrained;
	    resizeMotion(rstuff, e->xkey.x_root, e->xkey.y_root);
	} else if (mask == ModMaskMap[MOD_INVERT])
	    rstuff->moving = False;
	break;

    case Expose:
	return DISPOSE_DISPATCH;

    default:
	return DISPOSE_DEFER;
    }

    return DISPOSE_USED;
}



/*
 * Install an interposer for resizing with the mouse.
 */
void
UserResizeWin(cli, trigger, corner, callback, cbarg)
    Client *cli;
    XEvent *trigger;
    WhichResize corner;
    void (*callback)();
    void *cbarg;
{
    static ResizeClosure rstuff;
    XSizeHints *sh = cli->normHints;
    Window root = WinRootID(cli->framewin);
    Time timestamp;
    int frameMinWidth, frameMinHeight;
    Cursor	resizePointer;

    if (GRV.SpecialResizePointers)
	resizePointer = GRV.ResizePointers[corner];
    else resizePointer = GRV.ResizePointer;

    if (trigger == NULL || trigger->type == KeyPress) {
	if (trigger == NULL)
	    timestamp = CurrentTime;
	else
	    timestamp = trigger->xkey.time;
	rstuff.mouse = False;
	rstuff.origX = rstuff.curX = cli->framewin->core.x;
	rstuff.origY = rstuff.curY = cli->framewin->core.y;
    } else {
	/* it's a button press */
	timestamp = trigger->xbutton.time;
	rstuff.mouse = True;
	if (trigger->xbutton.state & ModMaskMap[MOD_CONSTRAIN])
	    rstuff.constraint = EitherConstrained;
	else
	    rstuff.constraint = Unconstrained;

	if (trigger->xbutton.state & ModMaskMap[MOD_INVERT])
	    rstuff.moving = True;
	else
	    rstuff.moving = False;
	rstuff.origX = rstuff.curX = trigger->xbutton.x_root;
	rstuff.origY = rstuff.curY = trigger->xbutton.y_root;
    }

    switch (corner) {
    case upleft:	rstuff.gravity = NorthWestGravity;	break;
    case upright:	rstuff.gravity = NorthEastGravity;	break;
    case lowleft:	rstuff.gravity = SouthWestGravity;	break;
    case lowright:	rstuff.gravity = SouthEastGravity;	break;
    case keyevent:	rstuff.gravity = CenterGravity;		break;
    }

    /* Grab the pointer to change the cursor and confine to the root window. */
    if (XGrabPointer(cli->dpy, root, True,
		     ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		     GrabModeAsync, GrabModeAsync, root,
		     resizePointer, timestamp) != GrabSuccess)
    {
	ErrorWarning(GetString("failed to grab pointer"));
	return;
    }

    if (XGrabKeyboard(cli->dpy, root, False,
		      GrabModeAsync, GrabModeAsync,
		      timestamp) != GrabSuccess)
    {
	/* note: not fatal */
	ErrorWarning(GetString("failed to grab keyboard"));
    }

    XGrabServer(cli->dpy);

    /* Fill in the closure for the interposer. */

    rstuff.drawn = False;
    rstuff.cli = cli;
    rstuff.winX = cli->framewin->core.x;
    rstuff.winY = cli->framewin->core.y;
    rstuff.winW = cli->framewin->core.width;
    rstuff.winH = cli->framewin->core.height;

    rstuff.callback = callback;
    rstuff.cbarg = cbarg;

    /*
     * Look at the client's size hints and update the closure appropriately.
     */

    rstuff.minW = rstuff.minH = 1;
    rstuff.maxW = rstuff.maxH = 32767; /* REMIND: max value of signed short */
    rstuff.incW = rstuff.incH = 1;
    rstuff.useAspect = False;
    rstuff.baseW = rstuff.baseH = 0;

    if (sh != NULL) {
	if (sh->flags & PMinSize) {
	    rstuff.minW = sh->min_width;
	    rstuff.minH = sh->min_height;
	} else if (sh->flags & PBaseSize) {
	    rstuff.minW = MAX(1,sh->base_width);
	    rstuff.minH = MAX(1,sh->base_height);
	}

	if (sh->flags & PMaxSize) {
	    rstuff.maxW = sh->max_width;
	    rstuff.maxH = sh->max_height;
	}

	if ((sh->flags & PResizeInc) &&
	    sh->width_inc > 0 && sh->height_inc > 0)
	{
	    rstuff.incW = sh->width_inc;
	    rstuff.incH = sh->height_inc;
	}

	if (sh->flags & PAspect) {
	    rstuff.useAspect = True;
	    rstuff.minAspectX = sh->min_aspect.x;
	    rstuff.minAspectY = sh->min_aspect.y;
	    rstuff.maxAspectX = sh->max_aspect.x;
	    rstuff.maxAspectY = sh->max_aspect.y;
	}

	if (sh->flags & PBaseSize) {
	    rstuff.baseW = sh->base_width;
	    rstuff.baseH = sh->base_height;
	    rstuff.baseProvided = True;
	} else if (sh->flags & PMinSize) {
	    rstuff.baseW = sh->min_width;
	    rstuff.baseH = sh->min_height;
	    rstuff.baseProvided = False;
	}
    }

    /* figure in the size of the frame decorations */

    rstuff.borderW
	= FrameWidthLeft(cli->framewin) + FrameWidthRight(cli->framewin);
    rstuff.borderH
	= FrameHeightTop(cli->framewin) + FrameHeightBottom(cli->framewin);

    FrameMinSize(cli->framewin, &frameMinWidth, &frameMinHeight);
    frameMinWidth -= rstuff.borderW;
    frameMinHeight -= rstuff.borderH;
    rstuff.minW = MAX(rstuff.minW, frameMinWidth);
    rstuff.minH = MAX(rstuff.minH, frameMinHeight);

    /* map the geom window and draw the initial outline, if necessary */

    if (GRV.ShowResizeGeometry) {
	Text	proto[50];

	rstuff.statusfmt = GetString("size: %4d, %4d");
	TextSPrintf(proto, rstuff.statusfmt, 9999, 9999);
	rstuff.statuswindow = createStatusWindow(cli->dpy, cli->scrInfo, proto);
    }
    else rstuff.statuswindow = NULL;

    if (trigger == NULL || trigger->type == KeyPress) {
	resizeDraw(&rstuff);
	rstuff.drawn = True;
    }

    resizePaintStatus(&rstuff);

    InstallInterposer(resizeInterposer, &rstuff);
}


/* ===== root bounding box ================================================ */


typedef struct _rootboxclosure {
    int x0, y0;
    int x, y;
    unsigned int w, h;
    WinRoot *rootWin;
    ScreenInfo *scrInfo;
    GC rootGC;
    void *closure;
    void (*callback)();
} RootBoxClosure;


static int
/* ARGSUSED */
rootBoxInterposer(dpy, event, w, rbc)
    Display *dpy;
    XEvent *event;
    WinGeneric *w;
    RootBoxClosure *rbc;
{
    XEvent nextevent;
    register int ex, ey;

    switch (event->type) {

    case ButtonPress:
	return DISPOSE_USED;

    case MotionNotify:
	/* if the event is off the screen, ignore it */
	if (event->xmotion.root != rbc->scrInfo->rootid)
	    return DISPOSE_USED;

	/*
	 * Motion compression.  If the next event is a MotionNotify,
	 * ignore this one.
	 */
	if (XEventsQueued(dpy, QueuedAfterReading) > 0 &&
	    (XPeekEvent(dpy, &nextevent), nextevent.type == MotionNotify))
	{
	    return DISPOSE_USED;
	}

	/* erase old box */
	drawDouble(dpy, rbc->scrInfo->rootid, rbc->scrInfo, rbc->rootGC,
		   rbc->x, rbc->y, rbc->w, rbc->h);

	/* update closure with new position */

	ex = event->xmotion.x_root;
	ey = event->xmotion.y_root;

	if (ex > rbc->x0) {
	    rbc->x = rbc->x0;
	    rbc->w = ex - rbc->x;
	} else {
	    rbc->x = ex;
	    rbc->w = rbc->x0 - rbc->x;
	}

	if (ey > rbc->y0) {
	    rbc->y = rbc->y0;
	    rbc->h = ey - rbc->y;
	} else {
	    rbc->y = ey;
	    rbc->h = rbc->y0 - rbc->y;
	}

	/* draw new box */

	drawDouble(dpy, rbc->scrInfo->rootid, rbc->scrInfo, rbc->rootGC,
		   rbc->x, rbc->y, rbc->w, rbc->h);
	return DISPOSE_USED;

    case ButtonRelease:
	if (!AllButtonsUp(event))
	    return DISPOSE_USED;
	break;

    case KeyPress:
	if (FindKeyboardAction(dpy, event) != ACTION_STOP) {
	    KeyBeep(dpy,event);
	    return DISPOSE_USED;
	}
	break;

    default:
	return DISPOSE_DEFER;
    }

    /*
     * erase outline, let go of server, call the callback
     */
    drawDouble(dpy, rbc->scrInfo->rootid, rbc->scrInfo, rbc->rootGC,
	       rbc->x, rbc->y, rbc->w, rbc->h);

    XUngrabServer(dpy);
    XUngrabPointer(dpy, CurrentTime);
    XUngrabKeyboard(dpy, CurrentTime);

    (*rbc->callback)(dpy, rbc->rootWin, rbc->x, rbc->y, rbc->w, rbc->h,
		event->xbutton.time, rbc->closure);

    UninstallInterposer();
    return DISPOSE_USED;
}


/*
 * TraceRootBox -- trace an XOR box with the initial point specified
 *	by pEvent, which is assumed to be a ButtonPress event.  Call the 
 *	callback function when done.
 */
void
TraceRootBox(dpy, winInfo, pEvent, callback, closure)
Display	*dpy;
WinRoot *winInfo;
XEvent	*pEvent;
void	(*callback)();
void	*closure;
{
	static RootBoxClosure rbc;
	Window rootID = WinRootID(winInfo);

	/* Change and confine the cursor. */
	if (XGrabPointer(dpy, rootID, True,
		ButtonReleaseMask | PointerMotionMask,
		GrabModeAsync, GrabModeAsync, rootID, GRV.BasicPointer,
		pEvent->xbutton.time) != GrabSuccess)
	{
	    ErrorWarning(GetString("failed to grab pointer"));
	    return;
	}

	if (XGrabKeyboard(dpy, rootID, False, GrabModeAsync, GrabModeAsync,
			  pEvent->xbutton.time) != GrabSuccess)
	{
	    /* note: not fatal */
	    ErrorWarning(GetString("failed to grab keyboard"));
	}

	rbc.x = rbc.x0 = pEvent->xbutton.x_root;
	rbc.y = rbc.y0 = pEvent->xbutton.y_root;
	rbc.w = rbc.h = 0;
	rbc.rootWin = winInfo;
	rbc.scrInfo = winInfo->core.client->scrInfo;
	rbc.rootGC = WinGC(winInfo, ROOT_GC);
	rbc.callback = callback;
	rbc.closure = closure;

	/* Grab the server, then draw the initial outline. */
	XGrabServer(dpy);
	drawDouble(dpy, rootID, rbc.scrInfo, WinGC(winInfo, ROOT_GC),
		   rbc.x, rbc.y, 0, 0);

	InstallInterposer(rootBoxInterposer, &rbc);
	return;
}
