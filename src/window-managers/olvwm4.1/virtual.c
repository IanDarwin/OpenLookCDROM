/*
 * (c) Copyright 1991 Scott Oaks.  See LEGAL_NOTICE file for terms of the
 * license
 */

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <sys/param.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "st.h"
#include "globals.h"
#include "win.h"
#include "menu.h"
#include "math.h"
#include "virtual.h"
#include "patchlevel.h"

#include "vdm.icon"
#include "vdm.mask"

#define INIT   register char *sp = instring;
#define GETC() (*sp++)
#define PEEKC()     (*sp)
#define UNGETC(c)   (--sp)
#define RETURN(c)   return;
#define ERROR(val)  regerr(val)
#define TRUE 1
#define FALSE 0

#include <regexp.h>
#ifdef REGEXP
regexp *expbuf;
#endif

#ifdef IDENT
#ident "@(#)virtual.c	1.6 olvwm version 07 Jan 1994"
#endif

/* Class Function Vector; a virtual pane is the VDM window in which all
 * the little virtual windows appear (and to which said windows are
 * parented)
 */
static ClassVirtualPane	classVirtualPane;

/*
 * Last select time in the VDM.  This is technically different for each
 * VDM, but who can click in a VDM, move between screens, and then click
 * again?
 */
static int	lastSelectTime;

extern List	*ScreenInfoList;

extern Button	*MakeUpLeftButton(),*MakeLeftButton(),*MakeDownLeftButton(),
		*MakeUpButton(),*MakeHomeButton(),*MakeDownButton(),
		*MakeUpRightButton(),*MakeRightButton(),*MakeDownRightButton();

extern char	*ExpandPath();

/*
 * Semantic action associated with each of the buttons above.  These must
 * be in the same order as the button array; it would be better if the Button
 * structure had an opaque entry for this purpose.
 */

static SemanticAction	vdmButtonActions[] = {
    ACTION_UPLEFT,
    ACTION_LEFT,
    ACTION_DOWNLEFT,
    ACTION_UP,
    ACTION_HOME,
    ACTION_DOWN,
    ACTION_UPRIGHT,
    ACTION_RIGHT,
    ACTION_DOWNRIGHT,
};

static unsigned char pixdata[] = { 0xaa, 0x55 };

#define VDMSelectMask (ButtonPressMask | ButtonReleaseMask | \
		       ButtonMotionMask | ExposureMask )

#define CEIL(a,b)	(((a)+(b)-1)/(b))

/*
 * ==========================================================================
 *
 * VDM Utility/Drawing/Moving Functions
 *
 */

/*
 * Draw the dotted lines on the VDM background
 */
static void
drawVDMGrid(dpy, vdm)
    Display		*dpy;
    VirtualDesktop	*vdm;

{
int		i;
XPoint		pts[5];
int		dh = DisplayHeight(dpy, vdm->client->screen);
int		dw = DisplayWidth(dpy, vdm->client->screen);
ScreenInfo	*scrInfo = vdm->client->scrInfo;

    XClearArea(dpy, PANEWINOFCLIENT(vdm->client), 0, 0, 0, 0, 0);
    XSetForeground(dpy, scrInfo->gc[VDM_GC],
		   scrInfo->colorInfo.virtualGridColor);
    if (vdm->resources->grid == GridVisible) {
	XSetLineAttributes(dpy, scrInfo->gc[VDM_GC], 0, LineOnOffDash,
			   CapNotLast, JoinMiter);
	pts[0].y = 0;
	pts[1].y = vdm->height;
	for (i = dw; i < vdm->absoluteWidth; i += dw) {
	    pts[0].x = pts[1].x = i / vdm->resources->scale;
	    XDrawLines(dpy, PANEWINOFCLIENT(vdm->client), scrInfo->gc[VDM_GC],
			pts, 2, CoordModeOrigin);
	}
	pts[0].x = 0;
	pts[1].x = vdm->width;
	for (i = dh; i < vdm->absoluteHeight; i+= dh) {
	    pts[0].y = pts[1].y = i / vdm->resources->scale;
	    XDrawLines(dpy, PANEWINOFCLIENT(vdm->client), scrInfo->gc[VDM_GC],
			pts, 2, CoordModeOrigin);
	}
	XSetLineAttributes(dpy, scrInfo->gc[VDM_GC], 0,
			   LineSolid, CapNotLast, JoinMiter);
    }
    pts[0].x = pts[4].x = -vdm->offsetX / vdm->resources->scale;
    pts[0].y = pts[4].y = -vdm->offsetY / vdm->resources->scale;
    pts[1].x = (dw - vdm->offsetX) / vdm->resources->scale;
    pts[1].y = pts[0].y;
    pts[2].x = pts[1].x;
    pts[2].y = (dh - vdm->offsetY) / vdm->resources->scale;
    pts[3].x = pts[0].x;
    pts[3].y = pts[2].y;
    XDrawLines(dpy, PANEWINOFCLIENT(vdm->client), scrInfo->gc[VDM_GC],
		pts, 5, CoordModeOrigin);
    XSetForeground(dpy, scrInfo->gc[VDM_GC], scrInfo->colorInfo.virtualFontColor);
}

/*
 *  Function for re-placing (not replacing!) all windows when the view into
 *  the desktop is changed
 */
typedef struct _replacestickyinfo {
    struct deltas	*deltas;
    int			screen;
} replaceStickyInfo;

static void *
replaceSticky(cli, c)
    Client		*cli;
    replaceStickyInfo	*c;

{
WinGenericFrame	*win;

    if (cli->screen != c->screen)
	return NULL;
    if (cli->wmInstance && !strcmp(cli->wmInstance, "OI_TRANSIENT"))
	return NULL;
    if (cli->wmClass && !strcmp(cli->wmClass, "OI_TRANSIENT"))
	return NULL;
    if (cli && !cli->sticky) {
	win = (WinGenericFrame *) cli->framewin;
	if (win)
	    GFrameSetConfig(win, (int) (win->core.x - c->deltas->delta_x),
			    (int) (win->core.y - c->deltas->delta_y),
			    win->core.width, win->core.height);
	win = (WinGenericFrame *) cli->iconwin;
	if (win)
	    GFrameSetConfig(win, (int) (win->core.x - c->deltas->delta_x),
			    (int) (win->core.y - c->deltas->delta_y),
			    win->core.width, win->core.height);
    }
    else if (cli) {
	/*
	 * client is sticky; its real window stays where it is but its
	 * virtual window must move
	 */
	if (cli->framewin)
	    XMoveWindow(cli->dpy, cli->framewin->core.virtual,
		(cli->framewin->core.x) / cli->scrInfo->vdm->resources->scale +
				cli->scrInfo->vdm->screenX,
		(cli->framewin->core.y) / cli->scrInfo->vdm->resources->scale +
				cli->scrInfo->vdm->screenY);
	if (cli->iconwin)
	    XMoveWindow(cli->dpy, cli->iconwin->core.virtual,
		(cli->iconwin->core.x) / cli->scrInfo->vdm->resources->scale +
				cli->scrInfo->vdm->screenX,
		(cli->iconwin->core.y) / cli->scrInfo->vdm->resources->scale +
				cli->scrInfo->vdm->screenY);
    }
    return NULL;
}

static void
moveDesktop(dpy, deltas, vdm)
    Display		*dpy;
    struct deltas	*deltas;
    VirtualDesktop	*vdm;
{
extern List	*ActiveClientList;
replaceStickyInfo	info;

    vdm->offsetX -= deltas->delta_x;
    vdm->offsetY -= deltas->delta_y;
    vdm->screenX = -vdm->offsetX / vdm->resources->scale;
    vdm->screenY = -vdm->offsetY / vdm->resources->scale;
    info.screen = vdm->client->screen;
    info.deltas = deltas;
    ListApply(ActiveClientList, replaceSticky, &info);
    drawVDMGrid(dpy, vdm);
}

/*
 * Make sure the given deltas don't move off the screen
 */
static void
constrainDeltas(dpy, vdm, deltas)
    Display		*dpy;
    VirtualDesktop	*vdm;
    struct deltas	*deltas;
{
int	dw = DisplayWidth(dpy, vdm->client->screen);
int	dh = DisplayHeight(dpy, vdm->client->screen);

    if (vdm->offsetX - deltas->delta_x < dw - vdm->absoluteWidth)
	deltas->delta_x = vdm->offsetX + vdm->absoluteWidth - dw;
    else if (vdm->offsetX - deltas->delta_x > 0)
	deltas->delta_x = vdm->offsetX;

    if (vdm->offsetY - deltas->delta_y < dh - vdm->absoluteHeight)
	deltas->delta_y = vdm->offsetY + vdm->absoluteHeight - dh;
    else if (vdm->offsetY - deltas->delta_y > 0)
	deltas->delta_y = vdm->offsetY;
}
 
static Bool
vdmPerformAction(dpy, vdm, a)
    Display		*dpy;
    VirtualDesktop	*vdm;
    SemanticAction	a;

{
struct deltas	deltas;
int	dh = DisplayHeight(dpy, vdm->client->screen);
int	dw = DisplayWidth(dpy, vdm->client->screen);

    deltas.delta_x = deltas.delta_y = 0;
    switch(a) {
	case ACTION_UP:
	case ACTION_UP_V:
	    deltas.delta_y = -dh;
	    break;
	case ACTION_JUMP_UP:
	    deltas.delta_y = -dh * 20;
	    break;
	case ACTION_HALF_UP:
	    deltas.delta_y = -dh * .5;
	    break;
	case ACTION_DOWN:
	case ACTION_DOWN_V:
	    deltas.delta_y = dh;
	    break;
	case ACTION_JUMP_DOWN:
	    deltas.delta_y = dh * 20;
	    break;
	case ACTION_HALF_DOWN:
	    deltas.delta_y = dh * .5;
	    break;
	case ACTION_LEFT:
	case ACTION_LEFT_V:
	    deltas.delta_x = -dw;
	    break;
	case ACTION_JUMP_LEFT:
	    deltas.delta_x = -dw * 20;
	    break;
	case ACTION_HALF_LEFT:
	    deltas.delta_x = -dw * .5;
	    break;
	case ACTION_RIGHT:
	case ACTION_RIGHT_V:
	    deltas.delta_x = dw;
	    break;
	case ACTION_JUMP_RIGHT:
	    deltas.delta_x = dw * 20;
	    break;
	case ACTION_HALF_RIGHT:
	    deltas.delta_x = dw * .5;
	    break;
	case ACTION_UPLEFT:
	case ACTION_UPLEFT_V:
	    deltas.delta_x = -dw;
	    deltas.delta_y = -dh;
	    break;
	case ACTION_JUMP_UPLEFT:
	    deltas.delta_x = -dw * 20;
	    deltas.delta_y = -dh * 20;
	    break;
	case ACTION_HALF_UPLEFT:
	    deltas.delta_x = -dw * .5;
	    deltas.delta_y = -dh * .5;
	    break;
	case ACTION_UPRIGHT:
	case ACTION_UPRIGHT_V:
	    deltas.delta_x = dw;
	    deltas.delta_y = -dh;
	    break;
	case ACTION_JUMP_UPRIGHT:
	    deltas.delta_x = dw * 20;
	    deltas.delta_y = -dh * 20;
	    break;
	case ACTION_HALF_UPRIGHT:
	    deltas.delta_x = dw * .5;
	    deltas.delta_y = -dh * .5;
	    break;
	case ACTION_HOME:
	case ACTION_HOME_V:
	    deltas.delta_x = vdm->offsetX;
	    deltas.delta_y = vdm->offsetY;
	    break;
	case ACTION_DOWNLEFT:
	case ACTION_DOWNLEFT_V:
	    deltas.delta_x = -dw;
	    deltas.delta_y = dh;
	    break;
	case ACTION_JUMP_DOWNLEFT:
	    deltas.delta_x = -dw * 20;
	    deltas.delta_y = dh * 20;
	    break;
	case ACTION_HALF_DOWNLEFT:
	    deltas.delta_x = -dw * .5;
	    deltas.delta_y = dh * .5;
	    break;
	case ACTION_DOWNRIGHT:
	case ACTION_DOWNRIGHT_V:
	    deltas.delta_x = dw;
	    deltas.delta_y = dh;
	    break;
	case ACTION_JUMP_DOWNRIGHT:
	    deltas.delta_x = dw * 20;
	    deltas.delta_y = dh * 20;
	    break;
	case ACTION_HALF_DOWNRIGHT:
	    deltas.delta_x = dw * .5;
	    deltas.delta_y = dh * .5;
	    break;
	case ACTION_GOTO_1:
	case ACTION_GOTO_2:
	case ACTION_GOTO_3:
	case ACTION_GOTO_4:
	case ACTION_GOTO_5:
	case ACTION_GOTO_6:
	case ACTION_GOTO_7:
	case ACTION_GOTO_8:
	case ACTION_GOTO_9:
	case ACTION_GOTO_10:
	    deltas.delta_x = vdm->offsetX +
				((int) (a - ACTION_GOTO_1) % vdm->columns) * dw;
	    deltas.delta_y = vdm->offsetY +
				((int) (a - ACTION_GOTO_1) / vdm->columns) * dh;
	    break;
	default:
	    return False;
    }
    constrainDeltas(dpy, vdm, &deltas);

    if (fabs(deltas.delta_x) < 0.1 && fabs(deltas.delta_y) < 0.1)
	return False;
    moveDesktop(dpy, &deltas, vdm);
    return True;
}

/*
 * Given root and logical coordinates relative to the VDM, turn them into
 * absolute screen coordinates
 */
static void
translateVirtualCoords(vdm, root_x, root_y, x, y)
    VirtualDesktop	*vdm;
    int			*root_x, *root_y, *x, *y;

{
int     	tx, ty;
unsigned int	tw, th, bw, d;
Window  	root;

    if (x)
	*x *= vdm->resources->scale;
    if (y)
	*y *= vdm->resources->scale;
    if (!root_x || !root_y)
	return;
    XGetGeometry(vdm->client->dpy, vdm->client->framewin->core.self,
		 &root, &tx, &ty, &tw, &th, &bw, &d);
    *root_x -= tx;
    *root_y -= ty;
    XGetGeometry(vdm->client->dpy,
		 vdm->client->framewin->fcore.panewin->core.self,
		 &root, &tx, &ty, &tw, &th, &bw, &d);
    *root_x -= tx;
    *root_y -= ty;
    *root_x *= vdm->resources->scale;
    *root_x += vdm->offsetX;
    *root_y *= vdm->resources->scale;
    *root_y += vdm->offsetY;
}

void
calculateVirtualDesktopSize(dpy, screen, v)
    Display		*dpy;
    int			screen;
    VirtualDesktop	*v;

{
    int	width, height;
    int	dw = DisplayWidth(dpy, screen);
    int	dh = DisplayHeight(dpy, screen);

    sscanf(v->resources->size, "%dx%d", &width, &height);
    if (width < dw)
	v->columns = width;
    else v->columns = CEIL(width, dw);
    if (height < dh)
	v->rows = height;
    else v->rows = CEIL(height, dh);

    v->absoluteWidth = v->columns * dw;
    v->absoluteHeight = v->rows * dh;

    v->screenWidth = dw / v->resources->scale;
    v->screenHeight = dh / v->resources->scale;
    if (v->resources->grid) {
	v->width = v->columns * v->screenWidth;
	v->height = v->rows * v->screenHeight;
    }
    else {
	v->width = v->absoluteWidth / v->resources->scale;
	v->height = v->absoluteHeight / v->resources->scale;
    }
}

/*
 * Allocate a virtual desktop for the given screen.
 * See virtual.h for a definition of all the fields in the returned structure.
 *
 * This doesn't deal with the window stuff, just with the fields.  The
 * window is actually created and the colors set etc. below in MakeVDM
 *
 */

static VirtualDesktop *
allocVirtualDesktop(dpy, screen, rsc)
    Display		*dpy;
    int			screen;
    VirtualResources	*rsc;

{
    VirtualDesktop	*v;

    v = (VirtualDesktop *) MemAlloc(sizeof(VirtualDesktop));
    v->resources = rsc;
    v->offsetX = 0;
    v->offsetY = 0;
    v->screenX = 0;
    v->screenY = 0;
    calculateVirtualDesktopSize(dpy, screen, v);
    return v;
}

static void
constrainOutline(dpy, stuff, x, y, snap)
    Display	*dpy;
    VDMstuff	*stuff;
    int		x, y, snap;
{
int	dw = DisplayWidth(dpy, stuff->vdm->client->screen);
int	dh = DisplayHeight(dpy, stuff->vdm->client->screen);

    if (snap) {
	stuff->vdm->screenX = (((x * stuff->vdm->resources->scale) / dw) * dw) /
						stuff->vdm->resources->scale;
	stuff->vdm->screenY = (((y * stuff->vdm->resources->scale) / dh) * dh) /
						stuff->vdm->resources->scale;
    }
    else {
	stuff->vdm->screenX = x - stuff->pointerX + stuff->initX;
	stuff->vdm->screenY = y - stuff->pointerY + stuff->initY;
    }

    if (stuff->vdm->screenX < 0)
	stuff->vdm->screenX = 0;
    else if (stuff->vdm->screenX + stuff->vdm->screenWidth >
			stuff->vdm->width + stuff->vdm->resources->scale)
	stuff->vdm->screenX = (stuff->vdm->absoluteWidth - dw) /
					stuff->vdm->resources->scale;

    if (stuff->vdm->screenY < 0)
	stuff->vdm->screenY = 0;
    else if (stuff->vdm->screenY + stuff->vdm->screenHeight >
			stuff->vdm->height + stuff->vdm->resources->scale)
	stuff->vdm->screenY = (stuff->vdm->absoluteHeight - dh) /
					stuff->vdm->resources->scale;
}

/*
 * ==========================================================================
 *  Interposition functions for mouse dragging
 */

static void
vdmMoveUpdate(ev, stuff)
    XEvent	*ev;
    VDMstuff	*stuff;
{
    XDrawRectangle(stuff->vdm->client->dpy, PANEWINOFCLIENT(stuff->vdm->client),
		   stuff->gc, stuff->vdm->screenX, stuff->vdm->screenY,
		   stuff->vdm->screenWidth + 1, stuff->vdm->screenHeight);
    constrainOutline(stuff->vdm->client->dpy, stuff,
		     ev->xmotion.x, ev->xmotion.y,
		     (stuff->vdm->resources->grid) ?
			!(ev->xbutton.state & ControlMask) :
		     	 (ev->xbutton.state & ControlMask));
    XDrawRectangle(stuff->vdm->client->dpy, PANEWINOFCLIENT(stuff->vdm->client),
		   stuff->gc, stuff->vdm->screenX, stuff->vdm->screenY,
		   stuff->vdm->screenWidth + 1, stuff->vdm->screenHeight);
    stuff->numMoves++;
}

static void
vdmMoveDone(ev, stuff)
    XEvent	*ev;
    VDMstuff	*stuff;
{
struct deltas	deltas;
int		dw = DisplayWidth(stuff->vdm->client->dpy,
				  stuff->vdm->client->screen);
int		dh = DisplayHeight(stuff->vdm->client->dpy,
				   stuff->vdm->client->screen);

    UninstallInterposer();
    XDrawRectangle(stuff->vdm->client->dpy, PANEWINOFCLIENT(stuff->vdm->client),
		   stuff->gc, stuff->vdm->screenX, stuff->vdm->screenY,
		   stuff->vdm->screenWidth + 1, stuff->vdm->screenHeight);
    if (ev->xbutton.time - lastSelectTime <= GRV.DoubleClickTime) {
	translateVirtualCoords(stuff->vdm,
			       &ev->xbutton.x_root, &ev->xbutton.y_root,
			       &ev->xbutton.x, &ev->xbutton.y);
	VDMMoveTo(stuff->vdm->client->dpy, stuff->vdm->client,
		  ev->xbutton.x_root, ev->xbutton.y_root);
	return;
    }
    lastSelectTime = ev->xbutton.time;
    if (!stuff->numMoves)
	return;
    constrainOutline(stuff->vdm->client->dpy, stuff,
		     ev->xmotion.x, ev->xmotion.y,
		     (stuff->vdm->resources->grid) ?
				!(ev->xbutton.state & ControlMask) :
				 (ev->xbutton.state & ControlMask));
    drawVDMGrid(stuff->vdm->client->dpy, stuff->vdm);
    deltas.delta_x = (stuff->vdm->screenX - stuff->initX) *
		stuff->vdm->resources->scale + stuff->vdm->resources->scale;
    deltas.delta_y = (stuff->vdm->screenY - stuff->initY) *
		stuff->vdm->resources->scale + stuff->vdm->resources->scale;
    if ((stuff->vdm->resources->grid && !(ev->xbutton.state & ControlMask)) ||
	(!stuff->vdm->resources->grid && (ev->xbutton.state & ControlMask))) {
	deltas.delta_x =
		(((int) (deltas.delta_x - stuff->vdm->offsetX) / dw) * dw) +
				stuff->vdm->offsetX;
	deltas.delta_y =
		(((int) (deltas.delta_y - stuff->vdm->offsetY) / dh) * dh) +
				stuff->vdm->offsetY;
    }
    moveDesktop(stuff->vdm->client->dpy, &deltas, stuff->vdm);
}

static Bool
/* ARGSUSED */
vdmInterposer(dpy, event, w, stuff)
    Display	*dpy;
    XEvent	*event;
    WinGeneric	*w;
    VDMstuff	*stuff;
{
XEvent		nextevent;
SemanticAction	action;

    switch(event->type) {
	case ButtonRelease:
	    if (AllButtonsUp(event)) {
		vdmMoveDone(event, stuff);
	    }
	    break;
	case MotionNotify:
	    if (!event->xmotion.same_screen)
		break;
	    if ((XEventsQueued(dpy, QueuedAfterReading) > 0) &&
		(XPeekEvent(dpy, &nextevent), nextevent.type == MotionNotify))
		/* Ignore this event, there's another one coming */
		break;
	    vdmMoveUpdate(event, stuff);
	    break;
	case Expose:
	    return DISPOSE_DISPATCH;
	case KeyPress:
	    action = FindKeyboardAction(dpy, event);
	    if (action == ACTION_STOP) {
		event->xany.type = ButtonRelease;
		event->xbutton.time = 0;
		stuff->numMoves = 0;
		vdmMoveDone(event, stuff);
	    }
	    else return DISPOSE_DEFER;
	    break;
	default:
	    return DISPOSE_DEFER;
    }
    return DISPOSE_USED;
}

/*
 * ===========================================================================
 *
 * Event functions for the VDM
 */
static
vdmExpose(dpy, event, winInfo)
    Display	*dpy;
    XEvent	*event;
    WinGeneric	*winInfo;
{
XEvent	dummy;

    if (event->xexpose.count)
	return;
    drawVDMGrid(dpy, winInfo->core.client->scrInfo->vdm);
    while (XCheckTypedWindowEvent(dpy, event->xany.window, Expose, &dummy))
	;	/* empty */
}

static
vdmKeyPress(dpy, ev, winInfo)
    Display	*dpy;
    XEvent	*ev;
    WinGeneric	*winInfo;
{
    if (!KeyMoveVDM(dpy, ev))
	if (GRV.Beep == BeepAlways)
	    NoFocusEventBeep(dpy, ev, winInfo);
}

static
vdmConfigure(dpy, event, winInfo)
    Display		*dpy;
    XConfigureEvent	*event;
    WinVirtual		*winInfo;
{
VirtualDesktop	*vdm;
int		dw, dh;
Bool		doit;
struct deltas	deltas;

    vdm = winInfo->core.client->scrInfo->vdm;
    dw = DisplayWidth(dpy, winInfo->core.client->screen);
    dh = DisplayHeight(dpy, winInfo->core.client->screen);
    vdm->width = event->width;
    vdm->height = event->height;
    if (vdm->resources->grid) {
	    vdm->absoluteHeight = (vdm->height - 3) * vdm->resources->scale;
	    vdm->absoluteHeight = ((vdm->absoluteHeight - 1) / dh + 1) * dh;
	    vdm->absoluteWidth = (vdm->width - 3) * vdm->resources->scale;
	    vdm->absoluteWidth = ((vdm->absoluteWidth - 1) / dw + 1) * dw;
	    vdm->rows = CEIL(vdm->absoluteHeight, dh);
	    vdm->columns = CEIL(vdm->absoluteWidth, dw);
    }
    else {
	    vdm->absoluteHeight = vdm->height * vdm->resources->scale;
	    vdm->absoluteWidth = vdm->width * vdm->resources->scale;
	    vdm->rows = CEIL(vdm->absoluteHeight, dh);
	    vdm->columns = CEIL(vdm->absoluteWidth, dw);
    }
    doit = False;
    deltas.delta_x = deltas.delta_y = 0;
    if (vdm->screenX + vdm->screenWidth > vdm->width) {
	doit = True;
	deltas.delta_x = (vdm->width - vdm->screenWidth - vdm->screenX) *
				vdm->resources->scale;
    }
    if (vdm->screenY + vdm->screenHeight > vdm->height) {
	doit = True;
	deltas.delta_y = (vdm->height - vdm->screenHeight - vdm->screenY) *
				vdm->resources->scale;
    }
    if (doit) {
	if (deltas.delta_x < 0)
	    deltas.delta_x = ((int) (deltas.delta_x - vdm->resources->scale) / dw) * dw;
	else deltas.delta_x = ((int) (deltas.delta_x + vdm->resources->scale) / dw) * dw;
	if (deltas.delta_y < 0)
	    deltas.delta_y = ((int) (deltas.delta_y - vdm->resources->scale) / dh) * dh;
	else deltas.delta_y = ((int) (deltas.delta_y + vdm->resources->scale) / dh) * dh;
	moveDesktop(dpy, &deltas, vdm);
    }
}

static
vdmNewConfigure(winInfo, pxcre)
    WinVirtual			*winInfo;
    XConfigureRequestEvent	*pxcre;
{
int	oldWidth, oldHeight, oldX, oldY;
Client	*cli = winInfo->core.client;
WinPaneFrame	*winFrame = cli->framewin;

    if (!pxcre)
	return winInfo->core.dirtyconfig;
    WinRootPos(winInfo, &oldX, &oldY);
    oldWidth = winInfo->core.width;
    oldHeight = winInfo->core.height;

    if ((pxcre->value_mask & CWHeight) && (pxcre->height != oldHeight)) {
	winInfo->core.height = pxcre->height;
	winInfo->core.dirtyconfig |= CWHeight;
    }
    if ((pxcre->value_mask & CWWidth) && (pxcre->width != oldWidth)) {
	winInfo->core.width = pxcre->width;
	winInfo->core.dirtyconfig |= CWWidth;
    }
    /*if (pxcre->value_mask & CWBorderWidth)
	winFrame->pcore.oldBorderWidth = pxcre->border_width;*/
    if (pxcre->value_mask & (CWX|CWY))
	FrameSetPosAbsolute(winFrame,
			    (pxcre->value_mask & CWX) ? (pxcre->x) : oldX,
			    (pxcre->value_mask & CWY) ? (pxcre->y) : oldY);
    if (pxcre->value_mask & (CWStackMode | CWSibling))
	GFrameSetStack(winFrame, pxcre->value_mask, pxcre->detail, pxcre->above);
    return winInfo->core.dirtyconfig;
}
	
static
vdmButtonPress(dpy, event, winInfo)
    Display	*dpy;
    XEvent	*event;
    WinGeneric	*winInfo;
{
static VDMstuff	stuff;
SemanticAction	a;
ScreenInfo	*scrInfo = winInfo->core.client->scrInfo;

    a = ResolveMouseBinding(dpy, event, ModMaskMap[MOD_CONSTRAIN] |
					ModMaskMap[MOD_INVERT] |
					ModMaskMap[MOD_REDUCE]);
    switch(a) {
	case ACTION_SELECT:
    	    stuff.vdm = scrInfo->vdm;
    	    stuff.pointerX = event->xbutton.x;
    	    stuff.pointerY = event->xbutton.y;
    	    stuff.gc = scrInfo->gc[ROOT_GC];
    	    stuff.initX = scrInfo->vdm->screenX;
    	    stuff.initY = scrInfo->vdm->screenY;
    	    stuff.numMoves = 0;
    	    XDrawRectangle(stuff.vdm->client->dpy,
			   PANEWINOFCLIENT(stuff.vdm->client),
		   	   stuff.gc, stuff.vdm->screenX, stuff.vdm->screenY,
		   	   stuff.vdm->screenWidth + 1, stuff.vdm->screenHeight);
    	    InstallInterposer(vdmInterposer, &stuff);
	    break;
	case ACTION_MENU:
	    MenuShowSync(dpy, winInfo, scrInfo->menuTable[MENU_VDM],
			 event, NULL, NULL, False, False);
	    break;
    }
}

static
/* ARGSUSED */
vdmRedraw(dpy, win)
    Display	*dpy;
    WinGeneric	*win;
{
    drawVDMGrid(win->core.client->dpy, win->core.client->scrInfo->vdm);
}

static
vdmExit()
{
    fprintf(stderr, "VDM destroy\n");
}

static
vdmButtonRelease()
{
/*
 * Need this for middle button presses
 */
}

static
vdmSetConfigure(dpy, winInfo)
    Display	*dpy;
    WinVirtual	*winInfo;
{
XConfigureEvent	ce;
XWindowChanges	xwc;

    if (winInfo->core.dirtyconfig) {
	xwc.x = winInfo->core.x;
	xwc.y = winInfo->core.y;
	xwc.width = winInfo->core.width;
	xwc.height = winInfo->core.height;
	ConfigureWindow(dpy, winInfo,
			 winInfo->core.dirtyconfig, &xwc);
	winInfo->core.dirtyconfig &= ~(CWX|CWY|CWWidth|CWHeight);
    }
    ce.type = ConfigureNotify;
    ce.serial = 0L;
    ce.event = winInfo->core.self;
    ce.window = winInfo->core.self;
    WinRootPos(winInfo, &ce.x, &ce.y);
    ce.width = winInfo->core.width;
    ce.height = winInfo->core.height;
    ce.border_width = 0;
    ce.above = None;
    ce.override_redirect = False;
    /* 
     * Elsewhere, we send a configure event to the client, but since the
     * client is us, let's just call that function directly . . .
     */
    vdmConfigure(dpy, &ce, winInfo);
}

static
vdmButtonMotion()
{
/*
 * Need this for middle button presses
 */
}

static
vdmNewpos(win, x, y)
    WinVirtual	*win;
    int		x, y;
{
    if (win->core.x != x) {
	win->core.x = x;
	win->core.dirtyconfig |= CWX;
    }
    if (win->core.y != y) {
	win->core.y = y;
	win->core.dirtyconfig |= CWY;
    }
    return win->core.dirtyconfig;
}

static
vdmSelect()
{
}

/*
 * setsizePane - set the pane to a particular size, and initiate a reconfigure
 */
static int
vdmSetSize(win,w,h)
WinVirtual *win;
int w, h;
{
    if (win->core.width != w) {
	win->core.width = w;
	win->core.dirtyconfig |= CWWidth;
    }

    if (win->core.height != h) {
	win->core.height = h;
	win->core.dirtyconfig |= CWHeight;
    }
}   

/*
 * Callback function when VDM frame is mapped
 */
static
vdmSetupFrame(win, cli, frame)
    WinGeneric		*win;
    Client		*cli;
    WinGenericFrame	*frame;
{
    frame->fcore.panewin = (WinGenericPane *) win;
    cli->scrInfo->vdm->client = cli;
    cli->sticky = True;
    cli->flags |= CLOlwmOwned;
    win->core.parent = (WinGeneric *) frame;
    WinAddChild(frame, win);
    XReparentWindow(cli->dpy, win->core.self, frame->core.self,
		    win->core.x, win->core.y);
    cli->wmHints->icon_pixmap = XCreateBitmapFromData(cli->dpy,
				RootWindow(cli->dpy, cli->screen), (char *) vdm_bits,
				vdm_width, vdm_height);
    cli->wmHints->icon_mask = XCreateBitmapFromData(cli->dpy,
				RootWindow(cli->dpy, cli->screen), (char *) vdmmask_bits,
				vdmmask_width, vdmmask_height);
}

static
/* ARGSUSED */
vdmComputeWidth(dpy, event, winInfo)
    Display	*dpy;
    XEvent	*event;
    WinGeneric	*winInfo;
{
}

static
/* ARGSUSED */
vdmComputeHeight(dpy, event, winInfo)
    Display	*dpy;
    XEvent	*event;
    WinGeneric	*winInfo;
{
}

static
vdmProperty(dpy, event, winInfo)
Display *dpy;
XPropertyEvent *event;
WinVirtual *winInfo;

{
Window		pane;
XSizeHints	sizeHints;
Bool		preICCCM;
Client		*cli = winInfo->core.client->scrInfo->vdm->client;

    switch(event->atom) {
	case XA_WM_NORMAL_HINTS:
	    StateUpdateWMNormalHints(cli, event);
	    break;
	
	case XA_WM_NAME:
	    FrameUpdateHeader(cli, event);
	    break;
    }
}

/*
 * =======================================================================
 * VDM Service Functions
 */

static XTextProperty *
MakeWTitle(rsc)
    VirtualResources *rsc;
{
static XTextProperty	newName;

    newName.value = (unsigned char *) strrchr(rsc->background, '/');
    if (!newName.value)
	newName.value = (unsigned char *) rsc->background;
    else newName.value++;

    newName.encoding = XA_STRING;
    newName.format = 8;
    newName.nitems = strlen((char*) newName.value);

    return &newName;
}

void
SetWName(dpy, vdm)
Display *dpy;
VirtualDesktop	*vdm;
{
    XSetWMName(dpy, PANEWINOFCLIENT(vdm->client),
		MakeWTitle(vdm->resources));
}

void
RedrawVDM(vdm)
    VirtualDesktop	*vdm;
{
    drawVDMGrid(vdm->client->dpy, vdm);
}

/* ARGSUSED */
void
VirtualInit(dpy)
    Display	*dpy;

{
    classVirtualPane.core.kind = WIN_VIRTUAL;
    classVirtualPane.core.xevents[KeyPress] = vdmKeyPress;
    classVirtualPane.core.xevents[ButtonPress] = vdmButtonPress;
    classVirtualPane.core.xevents[ButtonRelease] = vdmButtonRelease;
    classVirtualPane.core.xevents[MotionNotify] = vdmButtonMotion;
    classVirtualPane.core.xevents[ConfigureNotify] = vdmConfigure;
    classVirtualPane.core.xevents[Expose] = vdmExpose;
    classVirtualPane.core.xevents[PropertyNotify] = vdmProperty;
    classVirtualPane.core.focusfunc = NULL;
    classVirtualPane.core.drawfunc = vdmRedraw;
    classVirtualPane.core.destroyfunc = vdmExit;
    classVirtualPane.core.selectfunc = vdmSelect;
    classVirtualPane.core.newconfigfunc = vdmNewConfigure;
    classVirtualPane.core.newposfunc = vdmNewpos;
    classVirtualPane.core.setconfigfunc = vdmSetConfigure;
    classVirtualPane.core.createcallback = vdmSetupFrame;
    classVirtualPane.core.heightfunc = vdmComputeHeight;
    classVirtualPane.core.widthfunc = vdmComputeWidth;
    classVirtualPane.pcore.setsizefunc = vdmSetSize;
}

/*
 * Create an instance of the VDM -- this creates the logical virtual desktop
 * and then creates the VDM representation of it.
 */

void
MakeVDM(dpy, scrInfo, client, rsc)
    Display		*dpy;
    ScreenInfo		*scrInfo;
    Client		*client;
    VirtualResources	*rsc;

{
static XTextProperty	wName = {(unsigned char *) "Virtual Desktop",
					XA_STRING, 8, 15 };
static XTextProperty	iName = {(unsigned char *) "Desktop",
					XA_STRING, 8, 7 };
    XTextProperty	*wTitle;
    XSetWindowAttributes attr;
    XSizeHints *sizeHints;
    XWMHints *wmHints;
    XClassHint *classHints;
    unsigned attrMask;
    WinVirtual		*w;
    VirtualDesktop	*v;

    v = allocVirtualDesktop(dpy, scrInfo->screen, rsc);
    w = MemNew(WinVirtual);

    sizeHints = XAllocSizeHints();
    sizeHints->flags = PBaseSize;
    sizeHints->base_width = v->width;
    sizeHints->base_height = v->height;
    sizeHints->min_width = v->screenWidth;
    sizeHints->min_height = v->screenHeight;
    if (rsc->grid) {
	sizeHints->flags = PMinSize | PResizeInc;
	sizeHints->width_inc = v->screenWidth;
	sizeHints->height_inc = v->screenHeight;
    }
    else sizeHints->flags = PMinSize;

    /* REMIND Add an icon file attrbute */
    wmHints = XAllocWMHints();
    if (GRV.VirtualIconic)
	wmHints->initial_state = IconicState;
    else wmHints->initial_state = NormalState;
    wmHints->flags = StateHint | InputHint | IconWindowHint;
    wmHints->input = False;

    wmHints->icon_window = XCreateSimpleWindow(dpy,
				RootWindow(dpy, scrInfo->screen),
				0, 0, vdm_width, vdm_height, 0, 0,
				scrInfo->colorInfo.workspaceRootPixel);

    classHints = XAllocClassHint();
    classHints->res_name = "virtualDesktop";
    classHints->res_class = "olvwm";
 
    attrMask = CWEventMask;

    if ((DefaultDepth(dpy, scrInfo->screen) == 1) && !rsc->background) {
	attrMask |= CWBackPixmap;
	attr.background_pixmap = XCreatePixmapFromBitmapData(dpy,
			RootWindow(dpy, scrInfo->screen),
			(char *) pixdata, 8, 2,
			scrInfo->colorInfo.fgColor, scrInfo->colorInfo.bgColor,
			1);
    }
    else if (!rsc->background) {
	attrMask |= CWBackPixel;
	attr.background_pixel = scrInfo->colorInfo.virtualBgColor;
    }
    else {
	attrMask |= CWBackPixmap;
	rsc->pixInfo.ncolors = 0;
	rsc->pixInfo.colors = NULL;
	rsc->pixInfo.fg = scrInfo->colorInfo.virtualFgColor;
	rsc->pixInfo.bg = scrInfo->colorInfo.virtualBgColor;
	if (MakePixmap(dpy, scrInfo, rsc->background, &rsc->pixInfo)) {
	    attr.background_pixmap = rsc->pixInfo.pixmap;
	    sizeHints->flags |= PMaxSize;
	    sizeHints->max_width = rsc->pixInfo.width;
	    sizeHints->max_height = rsc->pixInfo.height;
	}
	else {
	    attr.background_pixmap = XCreatePixmapFromBitmapData(dpy,
			RootWindow(dpy, scrInfo->screen),
			(char *) pixdata, 8, 2,
			scrInfo->colorInfo.fgColor, scrInfo->colorInfo.bgColor,
			DefaultDepth(dpy, scrInfo->screen));
	}
    }
    attr.event_mask = ExposureMask | ButtonPressMask | ButtonReleaseMask |
		      ButtonMotionMask | KeyPressMask | PropertyChangeMask;

    w->class = &classVirtualPane;
    w->core.kind = WIN_VIRTUAL;
    w->core.children = NULL;
    w->core.client = client;
    w->core.x = sizeHints->x;
    w->core.y = sizeHints->y;
    w->core.width = sizeHints->base_width;
    w->core.height = sizeHints->base_height;
    w->core.dirtyconfig = CWX | CWY | CWWidth | CWHeight;
    w->core.exposures = NULL;
    w->core.helpstring = "olvwm:VDM";
    w->core.colormap = NULL;		/* initialized in callback */

    w->core.self = XCreateWindow(dpy, RootWindow(dpy, scrInfo->screen),
	    			sizeHints->x, sizeHints->y,
	    			sizeHints->base_width, sizeHints->base_height,
				0,
			/* REMIND olwm visual affects this? */
	    			DefaultDepth(dpy, scrInfo->screen), InputOutput,
	    			CopyFromParent, attrMask, &attr);
    wTitle = &wName;
    if (rsc->pixInfo.pixmap != None) {
	wTitle = MakeWTitle(rsc);
	XFreePixmap(dpy, rsc->pixInfo.pixmap);
    }
    XSetWMProperties(dpy, w->core.self, wTitle, &iName, NULL, 0,
		     sizeHints, wmHints, classHints);

    WIInstallInfo((WinGeneric *)w);
    scrInfo->vdm = v;

    v->client = StateNew(dpy, RootWindow(dpy, scrInfo->screen), w->core.self, False, w);
    /*
     * Sigh -- the call back was called before the icon frame was created
     * so we can't do this there like we'd like to.  And it was called
     * after StateNew processes the wmHints . . .
     */
    ((WinIconPane *) v->client->iconwin->fcore.panewin)->iconPixmap =
					v->client->wmHints->icon_pixmap;
    ((WinIconPane *) v->client->iconwin->fcore.panewin)->iconMask =
					v->client->wmHints->icon_mask;
    VirtualSetGeometry(v->client->framewin,
		       v->client->scrInfo->vdm->resources->geometry);
    VirtualSetGeometry(v->client->iconwin,
		       v->client->scrInfo->vdm->resources->iconGeometry);

    XFree((char *) sizeHints);
    XFree((char *) wmHints);
    XFree((char *) classHints);
}

void
DestroyVDM(dpy, vdm, cm)
    Display		*dpy;
    VirtualDesktop	*vdm;
    Colormap		*cm;
{
    FreePixmapColors(dpy, vdm->resources->pixInfo.ncolors,
			vdm->resources->pixInfo.colors, cm);
    MemFree(vdm->resources);
    MemFree(vdm);
}

void
InitVDMMenu(dpy, scrInfo)
    Display	*dpy;
    ScreenInfo	*scrInfo;
{
    char	title[20];
    Button	*vdmButtons[9];
    /*
     * Make the VDMs menu
     */
    sprintf(title, "OLVWM 4.%0d", PATCHLEVELv4);
    vdmButtons[0] = MakeUpLeftButton(dpy, scrInfo);
    vdmButtons[1] = MakeLeftButton(dpy, scrInfo);
    vdmButtons[2] = MakeDownLeftButton(dpy, scrInfo);
    vdmButtons[3] = MakeUpButton(dpy, scrInfo);
    vdmButtons[4] = MakeHomeButton(dpy, scrInfo);
    vdmButtons[5] = MakeDownButton(dpy, scrInfo);
    vdmButtons[6] = MakeUpRightButton(dpy, scrInfo);
    vdmButtons[7] = MakeRightButton(dpy, scrInfo);
    vdmButtons[8] = MakeDownRightButton(dpy, scrInfo);
    scrInfo->menuTable[MENU_VDM] = CreateMenu(strdup(title), vdmButtons,
				sizeof(vdmButtons)/sizeof(vdmButtons[0]),
				True, "olvwm:MotionMenu");
    scrInfo->menuTable[MENU_VDM]->prefColSize = 3;
    scrInfo->menuTable[MENU_VDM]->buttonDefault = 4;
}

/*
 * Create the virtual representation of the client's frame and icon
 */
void
MakeVirtual(cli)
    Client	*cli;

{
int	width, height;
int	x, y;
Window	virtual;

    /*
     * Subtract 1 for those window borders
     */
    width = cli->framewin->core.width / cli->scrInfo->vdm->resources->scale - 1;
    height = cli->framewin->core.height / cli->scrInfo->vdm->resources->scale-1;

    x = cli->framewin->core.x / cli->scrInfo->vdm->resources->scale +
			cli->scrInfo->vdm->screenX;
    y = cli->framewin->core.y / cli->scrInfo->vdm->resources->scale +
			cli->scrInfo->vdm->screenY;

    if (width <= 0)
	width = 1;
    if (height <= 0)
	height = 1;
    virtual = XCreateSimpleWindow(cli->dpy,
		PANEWINOFCLIENT(cli->scrInfo->vdm->client),
		x, y, width, height, 1, cli->scrInfo->colorInfo.borderColor,
		cli->scrInfo->colorInfo.virtualFgColor);
    if (PANEWINOFCLIENT(cli) != PANEWINOFCLIENT(cli->scrInfo->vdm->client))
	XSelectInput(cli->dpy, virtual, VDMSelectMask);
    else XSelectInput(cli->dpy, virtual, ExposureMask);
    cli->framewin->core.virtual = virtual;
    VInstallInfo(cli->framewin);
    MakeVirtualIcon(cli);
}

MakeVirtualIcon(cli)
    Client	*cli;

{
int	width, height;
int	x, y;

    width = cli->iconwin->core.width / cli->scrInfo->vdm->resources->scale;
    height = cli->iconwin->core.height / cli->scrInfo->vdm->resources->scale;

    x = cli->iconwin->core.x / cli->scrInfo->vdm->resources->scale +
			cli->scrInfo->vdm->screenX;
    y = cli->iconwin->core.y / cli->scrInfo->vdm->resources->scale +
			cli->scrInfo->vdm->screenY;

    if (width <= 0)
	width = 1;
    if (height <= 0)
	height = 1;
    cli->iconwin->core.virtual = XCreateSimpleWindow(cli->dpy,
				    PANEWINOFCLIENT(cli->scrInfo->vdm->client),
				    x, y, width, height, 1,
				    cli->scrInfo->colorInfo.borderColor,
				    cli->scrInfo->colorInfo.virtualFgColor);
    XSelectInput(cli->dpy, cli->iconwin->core.virtual, VDMSelectMask);
    VInstallInfo(cli->iconwin);
}

/*
 * Call to set/unset the focus of the given window
 */
void
VirtualChangeFocus(win, focus)
    WinGenericFrame	*win;
    Bool		focus;
{
int	length;

    switch(win->core.kind) {
	case WIN_ICON:
	    length = ((WinIconFrame *) win)->nameLength;
	    break;
	case WIN_FRAME:
	    length = ((WinPaneFrame *) win)->nameLength;
	    break;
	default:
	    return;
    }
    if (focus)
	XSetWindowBackground(win->core.client->dpy, win->core.virtual,
			win->core.client->scrInfo->colorInfo.virtualInputColor);
    else XSetWindowBackground(win->core.client->dpy, win->core.virtual,
			win->core.client->scrInfo->colorInfo.virtualFgColor);
    PaintVirtualWindow(win);
    CheckBindingState(win, focus);
}

/*
 * Refresh the window's virtual representation
 */
int
PaintVirtualWindow(win)
    WinGenericFrame	*win;
{
Client	*cli = win->core.client;
int	length;

    switch(win->core.kind) {
	case WIN_ICON:
	    length = ((WinIconFrame *) win)->nameLength;
	    break;
	case WIN_FRAME:
	    length = ((WinPaneFrame *) win)->nameLength;
	    break;
	default:
	    return;
    }
    XClearArea(cli->dpy, win->core.virtual, 0, 0, 0, 0, False);
    if (cli->isBusy)
	XFillRectangle(cli->dpy, win->core.virtual, WinGC(win,BUSY_GC),
			0, 0, win->core.width, win->core.height);
    XDrawImageString(cli->dpy, win->core.virtual,
		     (cli->isFocus) ? cli->scrInfo->gc[VDM_INPUT_GC] :
			       cli->scrInfo->gc[VDM_GC],
		     1, cli->scrInfo->vdm->max_ascent + 1,
		     win->fcore.name, length);
}

/*
 * Dispatch the event to its virtual window if it belongs to a virtual window
 */
DispatchVirtual(dpy, event)
    Display	*dpy;
    XEvent	*event;
{
WinGeneric	*win, *VGetInfo();
static SemanticAction	currentVDMAction;
unsigned int    ignoremask;

    if (event->xany.type == KeymapNotify || event->xany.type == MappingNotify)
	return False;
    win = VGetInfo(event->xany.window);
    if (!win)
	return False;
    switch(event->xany.type) {
	case Expose:
	    PaintVirtualWindow(win);
	    break;

	/*
	 * For mouse events, we generally try to pretend that the mouse event
	 * happened on the frame instead of on the virtual window.
	 * We pretend that adjust and select events occured on the frame
	 * and just pass them to the frame's routine (after translating
	 * the root coordinates).  But we don't translate root coordinates
	 * for menus because we want the menu to come up where the mouse
	 * was
	 */
	case ButtonPress:
    	    ignoremask = ModMaskMap[MOD_CONSTRAIN] |
		 		ModMaskMap[MOD_INVERT] |
				ModMaskMap[MOD_REDUCE];
	    currentVDMAction = ResolveMouseBinding(dpy, event, ignoremask);
	    if (currentVDMAction == ACTION_NONE)
		return True;
	    if ((currentVDMAction == ACTION_SELECT) &&
		(event->xbutton.time - lastSelectTime <= GRV.DoubleClickTime)) {
		/*
		 * We have a double click in the VDM.  This is an exception to
		 * the above rule:  if we pass this to the frame, then the
		 * frame will go full size, which is not very interesting.
		 * So for this one we pretend it's a double click in the
		 * VDM itself
		 */
		translateVirtualCoords(win->core.client->scrInfo->vdm,
				&event->xmotion.x_root, &event->xmotion.y_root,
				&event->xmotion.x, &event->xmotion.y);
		VDMMoveTo(win->core.client->dpy, win->core.client,
			  event->xbutton.x_root, event->xbutton.y_root);
		return True;
	    }
	    if (currentVDMAction == ACTION_MENU)
		translateVirtualCoords(win->core.client->scrInfo->vdm,
				       NULL, NULL,
				       &event->xmotion.x, &event->xmotion.y);
	    else translateVirtualCoords(win->core.client->scrInfo->vdm,
				&event->xmotion.x_root, &event->xmotion.y_root,
				&event->xmotion.x, &event->xmotion.y);
	    GFrameEventButtonPress(dpy, event, win);
	    break;
	
	case MotionNotify:
	    translateVirtualCoords(win->core.client->scrInfo->vdm,
				&event->xmotion.x_root, &event->xmotion.y_root,
				&event->xmotion.x, &event->xmotion.y);
	    GFrameEventMotionNotify(dpy, event, win);
	    break;

	case ButtonRelease:
	    if (currentVDMAction == ACTION_NONE)
		return True;
	    if (currentVDMAction == ACTION_MENU)
		translateVirtualCoords(win->core.client->scrInfo->vdm,
				       NULL, NULL,
				       &event->xmotion.x, &event->xmotion.y);
	    else translateVirtualCoords(win->core.client->scrInfo->vdm,
				&event->xmotion.x_root, &event->xmotion.y_root,
				&event->xmotion.x, &event->xmotion.y);
	    GFrameEventButtonRelease(dpy, event, win);
	    if (currentVDMAction == ACTION_SELECT)
		lastSelectTime = event->xbutton.time;
	    currentVDMAction = ACTION_NONE;
	    break;

	default:
	    fprintf(stderr, "unwanted %d event\n", event->xany.type);
	    break;
    }
    return True;
}

MakeSticky(cli, sticky)
    Client	*cli;
    Bool	sticky;

{
int	x, y;

    /*
     * Can't unstick the VDM
     */
    if (cli->groupid == PANEWINOFCLIENT(cli->scrInfo->vdm->client) && !sticky) {
	if (GRV.Beep == BeepAlways)
	    XBell(cli->dpy, 100);
	return !NULL;
    }
    cli->sticky = sticky;
    if (sticky && !GRV.VirtualDrawSticky)
	XUnmapWindow(cli->dpy, (cli->wmState == IconicState) ?
				cli->iconwin->core.virtual :
				cli->framewin->core.virtual);
    else XMapWindow(cli->dpy, (cli->wmState == IconicState) ?
				cli->iconwin->core.virtual :
				cli->framewin->core.virtual);
    if (cli->sticky) {
	x = cli->framewin->core.x;
	y = cli->framewin->core.y;
	cli->framewin->core.dirtyconfig |= CWX;
	GFrameSetConfig(cli->framewin, x, y,
			cli->framewin->core.width, cli->framewin->core.height);

	x = cli->iconwin->core.x;
	y = cli->iconwin->core.y;
	cli->iconwin->core.dirtyconfig |= CWX;
	IconSetPos(cli->iconwin, x + ICON_HORZBORDER, y + ICON_VERTBORDER);
	WinCallConfig(cli->dpy, cli->iconwin, NULL);
    }
    return NULL;
}

/*
 * Return true if the given key action should affect the VDM
 */
KeyMoveVDM(dpy, ev)
    Display *dpy;
    XEvent  *ev;
{
VirtualDesktop	*vdm;
WinGeneric	*root;
SemanticAction	a;

    if (ev->xany.type != KeyPress)
	/* Root window uses same function for Up and Down events */
	return False;

    root = WIGetInfo(ev->xkey.root);
    if (!root) {
	/* root is not managed by this wm, running in -single mode */
	return False;
    }
    if (!root->core.client) {
	return False;
    }
    if (!root->core.client->scrInfo) {
	return False;
    }
    vdm = root->core.client->scrInfo->vdm;

    a = FindNewKeyboardAction(dpy, ev);
    if (a == ACTION_NONE)
	return False;
    if (a == ACTION_VIRTUAL)
	return CheckForKeyProg(dpy, ev);

    return vdmPerformAction(dpy, vdm, a);
}

/*
 * Update the selection state of the given window's virtual representation
 */
VirtualSelect(win, sel)
    WinGeneric	*win;
    Bool	sel;

{
    if (win->core.virtual)
	XSetWindowBorderWidth(win->core.client->dpy,
			      win->core.virtual, (sel) ? 2 : 1);
}

/*
 * Move to point x, y and save the desktop; we do this when save workspace
 * is called so that all the coordinates will be correct for startup
 * position
 */
VirtualSaveDesktops(dpy, x, y)
    Display	*dpy;
    int		x, y;
{
struct deltas	deltas;
ScreenInfo	*si;
List		*l = ScreenInfoList;

    for (si = ListEnum(&l); si; si = ListEnum(&l)) {
	si->vdm->saveX = si->vdm->offsetX;
	si->vdm->saveY = si->vdm->offsetY;
	deltas.delta_x = si->vdm->offsetX - x;
	deltas.delta_y = si->vdm->offsetY - y;
	moveDesktop(dpy, &deltas, si->vdm);
    }
}

VirtualRestoreDesktops(dpy)
    Display	*dpy;
{
struct deltas	deltas;
ScreenInfo	*si;
List		*l = ScreenInfoList;

    for (si = ListEnum(&l); si; si = ListEnum(&l)) {
	deltas.delta_x = -si->vdm->saveX;
	deltas.delta_y = -si->vdm->saveY;
	moveDesktop(dpy, &deltas, si->vdm);
    }
}

VirtualCleanup(dpy)
    Display	*dpy;

{
    VirtualSaveDesktops(dpy, 0, 0);
}

ResizeVDM(vdm, size)
    VirtualDesktop	*vdm;
    char		*size;
{
WinGenericFrame	*win;

    win = (WinGenericFrame *) vdm->client->framewin;
    if (vdm->resources->size)
	free(vdm->resources->size);
    vdm->resources->size = strdup(size);
    calculateVirtualDesktopSize(vdm->client->dpy, vdm->client->screen, vdm);
    GFrameSetConfig(win, win->core.x, win->core.y,
		vdm->width + (WinFunc(win,fcore.widthleft))(win) +
			     (WinFunc(win,fcore.widthright))(win),
		vdm->height + (WinFunc(win,fcore.heighttop))(win) +
			     (WinFunc(win,fcore.heightbottom))(win));
}

/*
 *============================================================================
 *
 * Functions to reflect resources changes
 */

static void *
remakeVirtual(cli)
    Client	*cli;
{
Display	*dpy = cli->dpy;

    if (cli->framewin) {
	VUninstallInfo(cli->framewin->core.virtual);
	VUninstallInfo(cli->iconwin->core.virtual);
	XDestroyWindow(dpy, cli->framewin->core.virtual);
	XDestroyWindow(dpy, cli->iconwin->core.virtual);
	MakeVirtual(cli);
	if (!cli->sticky || GRV.VirtualDrawSticky)
	    XMapWindow(dpy, (cli->wmState == IconicState) ?
				cli->iconwin->core.virtual :
				cli->framewin->core.virtual);
    }
    return NULL;
}

static void *
restickVirtual(cli)
    Client	*cli;
{
    if (!cli->iconwin || !cli->framewin)
	return NULL;
    if (cli->sticky && !GRV.VirtualDrawSticky)
	XUnmapWindow(cli->dpy, (cli->wmState == IconicState) ?
				cli->iconwin->core.virtual :
				cli->framewin->core.virtual);
    else XMapWindow(cli->dpy, (cli->wmState == IconicState) ?
				cli->iconwin->core.virtual :
				cli->framewin->core.virtual);
    return NULL;
}

static void
updateVirtualWindow(cli)
    Client	*cli;

{
Display	*dpy = cli->dpy;

    if (cli->framewin) {
	XSetWindowBackground(dpy, cli->framewin->core.virtual,
				cli->scrInfo->colorInfo.virtualFgColor);
	XSetWindowBackground(dpy, cli->iconwin->core.virtual,
				cli->scrInfo->colorInfo.virtualFgColor);
    }
}

/*
 * Resize the VDM with the new scale
 */
RescaleVDM(vdm, scale)
    VirtualDesktop	*vdm;
    int			scale;

{
int	orig_scale;
WinGenericFrame	*win;

    orig_scale = vdm->resources->scale;
    vdm->resources->scale = scale;
    calculateVirtualDesktopSize(vdm->client->dpy, vdm->client->screen, vdm);
    vdm->screenX = (vdm->screenX * orig_scale) / scale;
    vdm->screenY = (vdm->screenY * orig_scale) / scale;
    vdm->client->normHints->width_inc = vdm->screenWidth;
    vdm->client->normHints->height_inc = vdm->screenHeight;
    win = (WinGenericFrame *) vdm->client->framewin;
    GFrameSetConfig(win, win->core.x, win->core.y,
		vdm->width + (WinFunc(win,fcore.widthleft))(win) +
			     (WinFunc(win,fcore.widthright))(win),
		vdm->height + (WinFunc(win,fcore.heighttop))(win) +
			      (WinFunc(win,fcore.heightbottom))(win));
    ListApply(ActiveClientList, remakeVirtual, 0);
}

VirtualUpdateVirtualWindows(cli)
    Client	*cli;
{
    ListApply(ActiveClientList, updateVirtualWindow, 0);
    ClientRefresh(cli);
}

VirtualUpdateDrawSticky(cli)
    Client	*cli;
{
    ListApply(ActiveClientList, restickVirtual, 0);
    ClientRefresh(cli);
}

/*
 * Size the VDM with the new geometry
 */
VirtualSetGeometry(win, geom)
    WinGeneric	*win;
    char	*geom;
{
int	changed;
int	newx, newy, x, y;
unsigned int w, h;
Display	*dpy = win->core.client->dpy;
ScreenInfo	*scrInfo = win->core.client->scrInfo;

	changed = XParseGeometry(geom, &x, &y, &w, &h);
	if (changed & (XValue | YValue)) {
	    if (changed & XValue)
		if (changed & XNegative)
		    newx = DisplayWidth(dpy, scrInfo->screen) + x - win->core.width;
		else newx = x;
	    else newx = win->core.x;
	    w = win->core.width;
	    if (newx > DisplayWidth(dpy, scrInfo->screen) - w)
		newx = DisplayWidth(dpy, scrInfo->screen) - w;
	    if (changed & YValue)
		if (changed & YNegative)
		    newy = DisplayHeight(dpy, scrInfo->screen) + y - win->core.height;
		else newy = y;
	    else newy = win->core.y;
	    h = win->core.height;
	    if (newy > DisplayHeight(dpy, scrInfo->screen) - h)
		newy = DisplayHeight(dpy, scrInfo->screen) - h;
	    GFrameSetConfig(win, newx, newy, win->core.width, win->core.height);	   
	}
}

/*
 * Perform an action from the VDM menu
 */
int
/* ARGSUSED */
VDMMenuAction(dpy, winInfo, menuInfo, btn)
    Display	*dpy;
    WinPinMenu	*winInfo;
    MenuInfo	*menuInfo;
    int		btn;

{

    if (!vdmPerformAction(dpy, winInfo->core.client->scrInfo->vdm,
			    vdmButtonActions[btn]) && GRV.Beep == BeepAlways)
	XBell(dpy, 100);
}

/*
 * Move to the logical screen on the desktop containing point x, y
 *
 * This move is constrained to a logical screen, even if the grid is
 * off.
 */
VDMMoveTo(dpy, client, x, y)
    Display	*dpy;
    Client	*client;
    int		x, y;
{
int	dw, dh;
struct deltas	deltas;

    dw = DisplayWidth(dpy, client->screen);
    dh = DisplayHeight(dpy, client->screen);
    deltas.delta_x = ((x - client->scrInfo->vdm->offsetX) / dw) * dw
			+ client->scrInfo->vdm->offsetX;
    deltas.delta_y = ((y - client->scrInfo->vdm->offsetY) / dh) * dh
			+ client->scrInfo->vdm->offsetY;
    constrainDeltas(dpy, client->scrInfo->vdm, &deltas);
    moveDesktop(dpy, &deltas, client->scrInfo->vdm);
}

/*
 * Goto logical screen passed 
 */

VDMGoto(dpy, client, screen)
    Display	*dpy;
    Client	*client;
    int		screen;
{
int	dw, dh;

    screen--;
    dw = DisplayWidth(dpy, client->screen);
    dh = DisplayHeight(dpy, client->screen);
    VDMMoveTo(dpy, client, dw * (screen % client->scrInfo->vdm->columns) +
					client->scrInfo->vdm->offsetX,
		dh * (screen / client->scrInfo->vdm->columns) +
					client->scrInfo->vdm->offsetY);
}

MoveDesktopFunc(dpy, wininfo, menuinfo, btn)
    Display	*dpy;
    WinGeneric	*wininfo;
    MenuInfo	*menuinfo;
    int		btn;
{
int	n, x, y;

    n = sscanf(menuinfo->menu->buttons[btn]->generate_args, "%d%d", &x, &y);
    switch(n) {
	default:
	    VDMMoveTo(dpy, wininfo->core.client, x, y);
	    break;
	case 1:
	    VDMGoto(dpy, wininfo->core.client, x);
	    break;
	case 0:
	    fprintf(stderr, gettext("olvwm:  Bad movedesktop switch\n"));
	    break;
    }
}
	    
/*
 *==========================================================================
 *
 * Utility functions to replace window mapping, configuring, etc.; these
 * functions work just like their X counterparts but affect both the
 * X window and its virtual representation.
 */

MapWindow(win)
    WinGeneric	*win;
{
    XSetWindowBorderWidth(win->core.client->dpy,
		win->core.virtual, (win->core.client->isSelected) ? 2 : 1);
    XMapWindow(win->core.client->dpy, win->core.self);
    if (GRV.VirtualDrawSticky || !win->core.client->sticky)
        XMapWindow(win->core.client->dpy, win->core.virtual);
    if (GRV.VirtualRaiseVDM && win->core.client->scrInfo->vdm->client)
	RaiseWindow(win->core.client->scrInfo->vdm->client->framewin);
}

UnmapWindow(win)
    WinGeneric	*win;
{
    XUnmapWindow(win->core.client->dpy, win->core.self);
    if (GRV.VirtualDrawSticky || !win->core.client->sticky)
        XUnmapWindow(win->core.client->dpy, win->core.virtual);
}

MapRaised(win)
    WinGeneric	*win;
{
    XSetWindowBorderWidth(win->core.client->dpy,
		win->core.virtual, (win->core.client->isSelected) ? 2 : 1);
    XMapRaised(win->core.client->dpy, win->core.self);
    if (GRV.VirtualDrawSticky || !win->core.client->sticky)
        XMapRaised(win->core.client->dpy, win->core.virtual);
    if (GRV.VirtualRaiseVDM && win->core.client->scrInfo->vdm->client)
	RaiseWindow(win->core.client->scrInfo->vdm->client->framewin);
}

RaiseWindow(win)
    WinGeneric	*win;
{
    XRaiseWindow(win->core.client->dpy, win->core.self);
    XRaiseWindow(win->core.client->dpy, win->core.virtual);
    if (GRV.VirtualRaiseVDM &&
	win->core.client->scrInfo->vdm->client &&
	win->core.client->scrInfo->vdm->client &&
  win->core.self != win->core.client->scrInfo->vdm->client->framewin->core.self)
	RaiseWindow(win->core.client->scrInfo->vdm->client->framewin);
}

LowerWindow(win)
    WinGeneric	*win;
{
    XLowerWindow(win->core.client->dpy, win->core.self);
    XLowerWindow(win->core.client->dpy, win->core.virtual);
    if (GRV.VirtualRaiseVDM)
	RaiseWindow(win->core.client->scrInfo->vdm->client->framewin);
}

DestroyWindow(win)
    WinGeneric	*win;
{
    XDestroyWindow(win->core.client->dpy, win->core.self);
    WIUninstallInfo(win->core.self);
    if (win->core.virtual)
	XDestroyWindow(win->core.client->dpy, win->core.virtual);
}

DeleteProperty(dpy, win, atom)
    Display		*dpy;
    WinGenericFrame	*win;
    Atom		atom;

{
    XDeleteProperty(dpy, win->core.self, atom);
    if (win->core.virtual)
	XDeleteProperty(dpy, win->core.virtual, atom);
}

ChangeProperty(dpy, win, property, type, format, mode, data, nelements)
   Display		*dpy;
   WinGenericFrame	*win;
   Atom			property, type;
   int			format;
   int			mode;
   unsigned char	*data;
   int			nelements;
{
    XChangeProperty(dpy, win->core.self, property, type, format,
		    mode, data, nelements);
    if (win->core.virtual)
	XChangeProperty(dpy, win->core.virtual, property, type, format,
		    mode, data, nelements);
}

ConfigureWindow(dpy, win, mask, values)
    Display	*dpy;
    WinGeneric	*win;
    int		mask;
    XWindowChanges	*values;

{
int		scale;
WinGeneric	*tmp;
VirtualDesktop	*vdm;

    vdm = win->core.client->scrInfo->vdm;
    /*
     * Make sure the window is somewhere on the visible desktop
     */
    if (win->core.x + (int) win->core.width < vdm->offsetX) {
        win->core.x = vdm->offsetX;
        win->core.dirtyconfig |= CWX;
    }
    else if (win->core.x > vdm->absoluteWidth + vdm->offsetX) {
        win->core.x = vdm->absoluteWidth + vdm->offsetX - win->core.width;
        win->core.dirtyconfig |= CWX;
    }
    if (win->core.y + (int) win->core.height < vdm->offsetY) {
        win->core.y = vdm->offsetY;
        win->core.dirtyconfig |= CWY;
    }
    else if (win->core.y > vdm->absoluteHeight + vdm->offsetY) {
        win->core.y = vdm->absoluteHeight + vdm->offsetY - win->core.height;
        win->core.dirtyconfig |= CWY;
    }      

    scale = vdm->resources->scale;
    XConfigureWindow(dpy, win->core.self, mask, values);
    if (mask & CWX)
	values->x = (win->core.x - vdm->offsetX) / scale;
    if (mask & CWY)
	values->y = (win->core.y - vdm->offsetY) / scale;
    if (mask & CWWidth)
	values->width = win->core.width / scale;
    if (mask & CWHeight)
	values->height = win->core.height / scale;
    if (mask & CWSibling) {
	tmp = WIGetInfo(values->sibling);
	if (tmp) {
	    values->sibling = tmp->core.virtual;
	}
	else {
	    mask |= ~(CWSibling|CWStackMode);
	}
    }
    XConfigureWindow(dpy, win->core.virtual, mask, values);
    if (GRV.VirtualRaiseVDM)
	RaiseWindow(vdm->client->framewin);
}

/*
 * There's another cmpButton in olvwmrc.y, but that comparison checks for
 * the icon flag and is case insensitive.  We need a simple case sensitive
 * check here
 */
static int
cmpButton(b1, b2)
    Button	**b1, **b2;

{
    return strcmp(gettext((*b1)->label[0].string),
		  gettext((*b2)->label[0].string));
}

/*
 * Menu generation function for DIRMENU
 *
 */
/* ARGSUSED */
GenDirMenuFunc(dpy, menuInfo, bindex, cache, winInfo, depth)
    Display	*dpy;
    MenuInfo	*menuInfo;
    int		bindex;
    MenuCache	*cache;
    WinGeneric	*winInfo;
    int		depth;

{
struct _menu	*menu;
DIR	*dir;
struct dirent	*ent;
Button	*b;
char	s[MAXNAMLEN], dirname[MAXPATHLEN - MAXNAMLEN], *newname, *ExpandPath();
char	pattern[MAXNAMLEN];
extern int AppMenuFunc();
MenuCache	*menuCache;
int		slot;

    menuCache = winInfo->core.client->scrInfo->menuCache;
    for (slot = 0; slot < menuCache->nextSlot; slot++)
	if (menuInfo->buttons[bindex].subMenu == menuCache->menuInfoList[slot])
	    break;
    if (slot == menuCache->nextSlot)
	slot = MENU_NEWSLOT;

    MenuInfoDestroy(menuInfo->buttons[bindex].subMenu);
    menu = (Menu *) MemAlloc(sizeof(Menu));
    menu->buttons = NULL;
    menu->buttonCount = 0;
    menu->buttonDefault = NOBUTTON;
    menu->hasPushPin = False;
    menu->menudirty = True;
    menu->btnPerCol = 0;
    menu->maxLabWidth = 0;
    menu->prefColSize = 0;

    pattern[0] = '\0';
    if ((newname = ExpandPath(menuInfo->menu->buttons[bindex]->generate_args,
				True)) == NULL)
	strcpy(dirname, (char *) menuInfo->menu->buttons[bindex]->generate_args);
    else {
	strcpy(dirname, newname);
	MemFree(newname);
    }
    if ((dir = opendir(dirname)) == NULL) {
	if (newname = strrchr(dirname, '/')) {
	    strcpy(pattern, newname + 1);
	    dirname[strlen(dirname) - strlen(pattern) - 1] = '\0';
	    rexInit(pattern);
	}
	if ((newname == NULL) || ((dir = opendir(dirname)) == NULL)) {
	    ErrorWarning(gettext("An invalid directory was named for DIRMENU"));
	    menuInfo->buttons[bindex].subMenu =
			MenuInfoCreate(cache, winInfo, menu, depth, slot);
	    return;
	}
    }
    while (ent = readdir(dir)) {
	if (ent->d_name[0] == '.')
	    continue;
	if (pattern[0] && !rexMatch(ent->d_name))
	    continue;

	b = (Button *) MemAlloc(sizeof(Button));
	if (!menu->buttonCount++)
	    menu->buttons = (Button **) MemAlloc(sizeof(Button *));
	else menu->buttons = (Button **)
		MemRealloc(menu->buttons, menu->buttonCount * sizeof(Button *));
	menu->buttons[menu->buttonCount - 1] = b;
	b->label[0].kind = StringLabel;
	b->label[1].kind = NoType;
	sprintf(s, "exec %s/%s", dirname, ent->d_name);
	b->label[0].string = MemNewString(ent->d_name);
	b->label[1].string = NULL;
	b->helpstring[0] = b->helpstring[1] = NULL;
	b->which = 0;
	b->has_submenu = False;
	b->enabled = True;
	b->visible = True;
	b->callback = AppMenuFunc;
	b->action.submenu = (void *) MemNewString(s);
	b->generate_func = NULL;
    }
    closedir(dir);
    if (GRV.VirtualDirSort == SortAlpha || GRV.VirtualDirSort == SortAlphaAll)
	qsort(menu->buttons, menu->buttonCount, sizeof(Button *), cmpButton);
    menuInfo->buttons[bindex].subMenu =
				MenuInfoCreate(cache, winInfo, menu, depth, slot);
}

static
regerr(val)
int val;
{
    switch(val) {
	case 11:
	    ErrorWarning(gettext("DIRMENU: Range endpoint too large.\n"));
	    break;
	case 16:
	    ErrorWarning(gettext("DIRMENU: Bad number.\n"));
	    break;
	case 25:
	    ErrorWarning(gettext("DIRMENU: ``\\ digit'' out of range.\n"));
	    break;
	case 36:
	    ErrorWarning(gettext("DIRMENU: Illegal or missing delimiter.\n"));
	    break;
	case 41:
	    ErrorWarning(gettext("DIRMENU: No remembered search string.\n"));
	    break;
	case 42:
	    ErrorWarning(gettext("DIRMENU: \\( \\) imbalance.\n"));
	    break;
	case 43:
	    ErrorWarning(gettext("DIRMENU: Too many \\(.\n"));
	    break;
	case 44:
	    ErrorWarning(gettext("DIRMENU: More than 2 numbers given in \\{ \\}.\n"));
	    break;
	case 45:
	    /* { gratutitous brace to match next line for vi */
	    ErrorWarning(gettext("DIRMENU: } expected after \\.\n"));
	    break;
	case 46:
	    ErrorWarning(gettext("DIRMENU: First number exceeds second in \\{ \\}.\n"));
	    break;
	case 49:
	    ErrorWarning(gettext("DIRMENU: [] imbalance.\n"));
	    break;
	case 50:
	    ErrorWarning(gettext("DIRMENU: Regular expression too long.\n"));
	    break;
	default:
	    ErrorWarning(gettext("DIRMENU: Unexpected Error Code\n"));
	    break;
    }
}

static char expbuf[256];

static
rexMatch(string)
    char *string;
{
#ifdef REGEXP
    return regexec(expbuf, string);
#else
    return step(string,expbuf);
#endif
}

static
rexInit(pattern)
    char *pattern;
{
int i, j, n;
char newPattern[256];

    strcpy(newPattern,"");
    n = strlen(pattern);
    newPattern[0] = '^';
    for(i = 0, j = 1; i < n; i++) {
	switch(pattern[i]) {
	    case '?':
		newPattern[j++] = '.';
		break;
	    case '*':
		if (i == 0)
		    newPattern[j++] = '.';
		newPattern[j++] = '*';
		break;
	    case '.':
		newPattern[j++] = '\\';
		newPattern[j++] = '.';
		break;
	    default:
		newPattern[j++] = pattern[i];
	}
    }
    newPattern[j++] = '$';
    newPattern[j++] = '\0';
#ifdef REGEXP
    expbuf = regcomp(newPattern);
#else
    compile(newPattern, expbuf, &expbuf[256], '\0');
#endif
}
