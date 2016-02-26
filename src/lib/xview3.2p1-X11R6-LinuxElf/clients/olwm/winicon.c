#ident	"@(#)winicon.c	26.42	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"		/* needed by olgx.h */
#include <olgx/olgx.h>

#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "globals.h"
#include "slots.h"
#include "group.h"

extern 	Bool	PropGetWMName();
extern 	Bool	PropGetWMIconName();

/***************************************************************************
* private data
***************************************************************************/

/* border sizes, in pixels */
#define ICON_HORZBORDER 3
#define ICON_VERTBORDER 3

/* Class function vector */
static ClassIconFrame classIconFrame;

/***************************************************************************
* private event functions
***************************************************************************/

static int
menuPressIcon(dpy,event,iconInfo)
Display *dpy;
XEvent *event;
WinIconFrame *iconInfo;
{
    if (iconInfo->core.client->wmDecors->menu_type != MENU_NONE)
	ShowStandardMenu(iconInfo, event, False);
}

static int
selectDoubleClickIcon(dpy,event,iconInfo)
Display *dpy;
XEvent *event;
WinIconFrame *iconInfo;
{
	StateNormal(iconInfo->core.client, event->xbutton.time);
}

static int
adjustClickIcon(dpy,event,iconInfo)
Display *dpy;
XEvent *event;
WinIconFrame *iconInfo;
{
        ToggleSelection(iconInfo->core.client, event->xbutton.time);
}

/***************************************************************************
* private functions
***************************************************************************/

/*
 * iconCalcName - calc position/size of icon name
 */
static void
iconCalcName(winIcon,pane)
	WinIconFrame *winIcon;
	Window 	pane;
{
	Display *dpy = winIcon->core.client->dpy;

	winIcon->nameLength = TextLen(winIcon->fcore.name);
	winIcon->nameWidth = FontWidth(IconFont,
				      winIcon->fcore.name,winIcon->nameLength);
	winIcon->nameX = (winIcon->core.width - winIcon->nameWidth)/2;

	/* 
	 * Position the text one pixel above the ICON_VERTBORDER and
	 * the descent of the font
	 */
	winIcon->nameY = winIcon->core.height - ICON_VERTBORDER - 1
			 - FontDescent(IconFont);
}

/* 
 * iconSetName -- set the icon name and possibly redraw
 */
static void
iconSetName(winIcon,pane)
	WinIconFrame *winIcon;
	Window pane;
{
	Display *dpy = winIcon->core.client->dpy;

	if (winIcon->fcore.name)
		MemFree(winIcon->fcore.name);

	if (!PropGetWMIconName(dpy,pane,&(winIcon->fcore.name)) &&
	    !PropGetWMName(dpy,pane,&(winIcon->fcore.name))) {
		winIcon->fcore.name = MemNewText(GRV.DefaultWinName);
	}

	iconCalcName(winIcon,pane);

	if (!winIcon->core.dirtyconfig)
		(WinFunc(winIcon,core.drawfunc))(dpy,winIcon);
}

/* selectDragIcon -- the user has held the select button down long enough
 * to initiate a drag.  Unpin the icon slot and start a window-move.
 */
static int
selectDragIcon(dpy, ev, iframe, lastpress)
Display *dpy;
XEvent *ev;
WinIconFrame *iframe;
XButtonEvent *lastpress;
{
	SlotFree(iframe);
	iframe->fManuallyPositioned = True;
	ClientMove(iframe->core.client,lastpress);
}


/*
 * newconfigIcon -- compute a new configuration of icon window
 */
static int
newconfigIcon(winInfo, pxcre)
WinIconFrame *winInfo;
XConfigureRequestEvent *pxcre;
{
        Client 	*cli = winInfo->core.client;
        WinPane *winPane = (WinPane *)winInfo->fcore.panewin;
        int 	neww,newh;

        neww = winInfo->fcore.panewin->core.width + 2*widthBothIcon(winInfo);
        newh = winInfo->fcore.panewin->core.height + heightTopIcon(winInfo) +
            heightBottomIcon(winInfo);

        if (neww != winInfo->core.width)
        {
                winInfo->core.width = neww;
                winInfo->core.dirtyconfig |= CWWidth;
        }

        if (newh != winInfo->core.height)
        {
                winInfo->core.height = newh;
                winInfo->core.dirtyconfig |= CWHeight;
        }

        if (winInfo->core.dirtyconfig)
        {
                (WinFunc(winPane,core.newposfunc))(winPane, 
			widthBothIcon(winInfo), heightTopIcon(winInfo));
        }

	if (winInfo->core.dirtyconfig & (CWWidth | CWHeight))
	{
                iconCalcName(winInfo,PANEWINOFCLIENT(cli));
	}

	return winInfo->core.dirtyconfig;
}

/*
 * The icon is being moved to a new (x,y) location.  If the icon slot has not
 * yet been allocated, do so if appropriate.  Otherwise, blindly accept the
 * (x,y) position.
 */
static int
newposIcon(winInfo,x,y)
WinIconFrame *winInfo;
int x,y;
{
	Client *cli = winInfo->core.client;

	WinNewPosFunc(winInfo,x,y);
	if (winInfo->iconslot == NULL && ClientHasIcon(cli)) {
	    SlotAlloc(winInfo, winInfo->fManuallyPositioned, GRV.FSnapToGrid);
	}
	return winInfo->core.dirtyconfig;
}


static void
drawDashedRect(dpy, winInfo, win, x, y, w, h)
    Display *dpy;
    WinIconFrame *winInfo;
    Window win;
    int x, y, w, h;
{
    XPoint pts[5];

    pts[0].x = x;	pts[0].y = y;
    pts[1].x = x;	pts[1].y = y + h;
    pts[2].x = x + w;	pts[2].y = y + h;
    pts[3].x = x + w;	pts[3].y = y;
    pts[4].x = x;	pts[4].y = y;

    /*
     * The following is necessary because IconBorderGC uses the LineOnOffDash
     * line-style, which is faster than LineDoubleDash on some servers.
     */
    XDrawLines(dpy, win, WinGC(winInfo,WORKSPACE_GC),
		pts, 5, CoordModeOrigin);
    XDrawLines(dpy, win, WinGC(winInfo,ICON_BORDER_GC),
		pts, 5, CoordModeOrigin);
}

/*
 * drawIconBorder -- based on the value of select, draw the border for an icon
 */
static void
drawIconBorder(dpy, winInfo, select)
Display *dpy;
WinIconFrame *winInfo;
Bool    select;
{
	int 	x, y;           /* values for use with */
	unsigned int width, height;  /* rectangle drawn for border */
	Window 	w = winInfo->core.self;
	GC	borderGC = WinGC(winInfo,BORDER_GC);
	GC	workspaceGC = WinGC(winInfo,WORKSPACE_GC);

	x = y = 0;
	width = winInfo->core.width - 1;
	height = winInfo->core.height - 1;

	/*
	 * If 3D is used, give "borderless" icons.  Otherwise, give black and 
	 * white borders.
	 */
	if (select) {
	    XDrawRectangle(dpy, w, borderGC,
			   x, y, width, height );
	    XDrawRectangle(dpy, w, borderGC,
			   x+1, y+1, width-2, height-2 );
	    XDrawRectangle(dpy, w, borderGC,
			   x+2, y+2, width-4, height-4 );
	} else {
	    XDrawRectangle(dpy, w, workspaceGC,
			   x, y, width, height);
	    if (Win3D(winInfo)) {
		XDrawRectangle(dpy, w, workspaceGC, 
			       x+1, y+1, width-2, height-2);
	    } else {
#ifdef notdef
		XDrawRectangle(dpy, w, IconBorderGC, 
			       x+1, y+1, width-2, height-2);
#endif /* notdef */
		drawDashedRect(dpy, winInfo, w, x+1, y+1, width-2, height-2);
	    }

	    XDrawRectangle(dpy, w, workspaceGC,
			   x+2, y+2, width-4, height-4);
	}

#ifdef notdef
/*
 * This stuff was used for the attempt at 3D-look icons.
 * It has been abandoned in favor of the "borderless" icon look.
 */

        /* initial values for first rectangle */
        x = 0;
        y = 0;
        /* need to subtract one, based on how XDrawRectangle works */
        width = winInfo->core.width - 1;
        height = winInfo->core.height - 1;

        /* draw three rectangles for border */
	for ( rectangle = 0 ; rectangle < 3 ; rectangle++ )
	{
              switch( rectangle )
              {
              case 0:         /* outermost rectangle */
                      if (Win3D(winInfo))
                      {
                              if ( select )
                                      olgxState = OLGX_INVOKED;
                              else
                                      olgxState = OLGX_NORMAL;

                              olgx_draw_box( olgx_gisnormal,
                                             winInfo->core.self,
                                             x, y, width+1, height+1,
                                             olgxState, 0 );
                              drawRectangle = False;
                      }
                      else
                      {
                              highlightGC = select
                                              ? DrawSelectedGC
                                              : DrawBackgroundGC;
                              drawRectangle = True;
                      }
                      break;
              case 1:         /* middle rectangle */
                      if ( select )
                                      highlightGC = DrawSelectedGC;
                      else if (Win3D(winInfo))
                              highlightGC = DrawBackgroundGC;
                      else    /* REMIND eventually need to handle
                               * IconBorder resource when 2d & ColorDisplay
                               */
                              highlightGC = IconBorderGC;
                      drawRectangle = True;
                      break;
              case 2:         /* innermost rectangle */
              default:
                      highlightGC = select ? DrawSelectedGC
                                           : DrawBackgroundGC;
                      drawRectangle = True;
                      break;
              }

              if ( drawRectangle )
                      XDrawRectangle( dpy, winInfo->core.self, highlightGC,
                                      x, y, width, height );
              x++;
              y++;
              width -= 2;
              height -= 2;
      }
#endif /* notdef */
}

/*
 * drawIcon -- draw the icon window
 */
/*ARGSUSED*/  /* dpy arg will be used when multiple Displays supported */
static int
drawIcon(dpy, winInfo)
Display       *dpy;
WinIconFrame *winInfo;
{
	Window		frameWin = winInfo->core.self;

	XFillRectangle(dpy, frameWin, WinGC(winInfo,WORKSPACE_GC),
		0, 0, winInfo->core.width, winInfo->core.height);

	/* draw icon name */
	if (winInfo->core.client->wmDecors->flags & WMDecorationIconName)
		DrawText(dpy,frameWin,IconFont,WinGC(winInfo,ICON_NORMAL_GC),
			 winInfo->nameX, winInfo->nameY, 
			 winInfo->fcore.name, winInfo->nameLength);

	/* draw border */
	drawIconBorder(dpy, winInfo, winInfo->core.client->isSelected);
}


/*
 * DestroyIcon -- destroy the icon window resources and free any allocated
 *    data.
 */
static int
destroyIcon(dpy, winInfo)
Display       *dpy;
WinIconFrame *winInfo;
{
	/* 
	 * Free our data and throw away window
	 */
	SlotFree(winInfo);
	ListDestroy(winInfo->core.children);
	MemFree(winInfo->fcore.name);
	ScreenDestroyWindow(winInfo->core.client->scrInfo, winInfo->core.self);
	WIUninstallInfo(winInfo->core.self);
	MemFree(winInfo);
}

/*
 * heightIconName - returns the height of the icon name portion of
 *	the total icon height.
 */
static int
heightIconName(win)
WinIconFrame	*win;
{
	if (win->core.client->wmDecors->flags & WMDecorationIconName)
	{
		return (FontHeight(IconFont) + ICON_VERTBORDER);
	} else {
		return 0;
	}
}

/*
 * heightTopIcon - returns the height of the top portion of the icon window.
 *	If the IconPane (image/window) is too small then increase the
 *	the top height to bring total height to the minimal icon
 *	window size of ICON_WIN_HEIGHT.  Otherwise use the default
 *	border size.
 */
static int
heightTopIcon(win)
WinIconFrame	*win;
{
	WinIconPane *winPane = (WinIconPane *)(win->fcore.panewin);
	int availHeight,basicbottom;

	availHeight = ICON_WIN_HEIGHT - heightIconName(win);

	if (winPane->core.height < availHeight) {
		return (availHeight-winPane->core.height)/2;
	} else {
		return ICON_VERTBORDER;
	}
}

/*
 * heightBottomIcon - returns the height of the bottom portion of
 *	the icon window - which includes the icon name string (if any).
 *	If the IconPane (image/window) is too small then increase the
 *	the bottom height to bring total height to the minimal icon
 *	window size of ICON_WIN_HEIGHT.  Otherwise use the default
 *	border size.
 */
static int
heightBottomIcon(win)
WinIconFrame	*win;
{
	WinIconPane *winPane = (WinIconPane *)(win->fcore.panewin);
	int nameHeight,availHeight;

	nameHeight = heightIconName(win);

	availHeight = ICON_WIN_HEIGHT - nameHeight;

	if (winPane->core.height < availHeight) {
		return (availHeight - winPane->core.height)/2 + nameHeight;
	} else {
		return nameHeight + ICON_VERTBORDER;
	}
}

/* The icon pane has the same border width on either side, so this function
 * is used to calculate both border widths.
 */
static int
widthBothIcon(win)
WinIconFrame	*win;
{
	WinIconPane *winPane = (WinIconPane *)(win->fcore.panewin);

	if (winPane->iconClientWindow)
	{
	    return ICON_HORZBORDER;
	}
	else
	{
	    if (winPane->core.width < ICON_WIN_WIDTH - 2*ICON_HORZBORDER)
	    {
		return (ICON_WIN_WIDTH-winPane->core.width)/2;
	    }
	    else
		return ICON_HORZBORDER;
	}
}

/* 
 * fullrestoreIcon
 *	Switch icon menus and if this client is iconic then
 *	open it.
 */
static int
fullrestoreIcon(client,timestamp)
Client	*client;
Time	timestamp;
{
	WinIconFrame	*iconInfo = client->iconwin;

	if (client->wmState == IconicState)
	    StateNormal(client,timestamp);

	iconInfo->fcore.fullsize = !iconInfo->fcore.fullsize;
}


/*
 * eventConfigureRequest -- handle ConfigureRequest events on the icon frame.
 *
 * Clients aren't supposed to configure their icon windows, so this routine 
 * simply issues a synthetic ConfigureNotify event that indicates the request 
 * has been refused.
 */
static int
eventConfigureRequest(dpy, req, iconInfo)
    Display *dpy;
    XConfigureRequestEvent *req;
    WinIconFrame *iconInfo;
{
    XConfigureEvent notify;
    WinIconPane *pane = (WinIconPane *) iconInfo->fcore.panewin;

    notify.type = ConfigureNotify;
    notify.window = notify.event = req->window;
    notify.x = pane->core.x;
    notify.y = pane->core.y;
    notify.width = pane->core.width;
    notify.height = pane->core.height;
    notify.above = None;
    notify.override_redirect = False;

    /* REMIND: should be NORMAL_BORDERWIDTH from winipane.c */
    notify.border_width = 0; 

    (void) XSendEvent(dpy, req->window, False, StructureNotifyMask,
		      (XEvent *) &notify);
#ifdef DEBUG
    ErrorWarning("ignoring ConfigureRequest event on icon.");
#endif /* DEBUG */
}


/***************************************************************************
* global functions
***************************************************************************/

/*
 * MakeIcon  -- create the icon window. Return a WinIconFrame structure.
 *	Note that unlike most Make functions, icons are not mapped right
 *	away.
 */
WinIconFrame *
MakeIcon(cli,panewin,paneattrs)
Client *cli;
Window panewin;		
XWindowAttributes *paneattrs;
{
	Display		*dpy = cli->dpy;
	WinIconFrame	*w;
        XSetWindowAttributes attributes;
        unsigned long   valuemask;
	XWMHints	*wmHints = cli->wmHints;
	Screen		*screen = ScreenOfDisplay(dpy, cli->scrInfo->screen);

	/* create the window structure */
	w = MemNew(WinIconFrame);
	w->core.kind = WIN_ICON;
	w->class = &classIconFrame;
	w->core.parent = NULL;
	w->core.children = NULL;
	w->core.client = cli;
	w->core.width = ICON_WIN_WIDTH;
	w->core.height = ICON_WIN_HEIGHT;

	/* fill out  the associated structure */
	w->core.dirtyconfig = CWX|CWY|CWWidth|CWHeight;
	w->core.colormap = None;
	w->core.exposures = NULL;
	w->core.helpstring = "olwm:Icon";

	/* create the icon frame */

	attributes.border_pixel = 0;
	attributes.colormap = cli->scrInfo->colormap;
	attributes.event_mask = ButtonPressMask | ButtonReleaseMask |
				ExposureMask | ButtonMotionMask |
				EnterWindowMask | FocusChangeMask |
				SubstructureRedirectMask;

	valuemask = CWBorderPixel | CWColormap | CWEventMask;

	w->core.self = ScreenCreateWindow(cli->scrInfo, cli->scrInfo->rootid,
	    w->core.x, w->core.y, 1, 1, valuemask, &attributes);

	/* install icon frame in client */
	cli->iconwin = w;	/* REMIND: should be called cli->iconframe */
	
	/*
	 * Use the icon position in the hints if that position is valid.  The 
	 * notion of validity is somewhat asymmetrical, as we don't have the
	 * icon's size yet.  We constrain the position to be in the rectangle 
	 * bounded by (0,0) and (screen_width - ICON_HORZBORDER,
	 * screen_height - ICON_VERTBORDER).  This has the effect of 
	 * constraining icons to be fully onscreen at the left and top but 
	 * merely lapping on a few pixels at the bottom and right.
	 *
	 * If the position isn't specified in the hints, or it is specified 
	 * but is invalid, set the position to zero and mark the icon to be 
	 * positioned automatically.
	 */

	if (wmHints != NULL && (wmHints->flags & IconPositionHint) &&
	    wmHints->icon_x >= 0 && wmHints->icon_y >= 0 &&
	    wmHints->icon_x <= (WidthOfScreen(screen) - ICON_HORZBORDER) &&
	    wmHints->icon_y <= (HeightOfScreen(screen) - ICON_VERTBORDER))
	{
		w->core.x = wmHints->icon_x;
		w->core.y = wmHints->icon_y;
		w->fManuallyPositioned = True;
	} else {
		/* to be fixed up at config time */
		w->core.x = 0;
		w->core.y = 0;
		w->fManuallyPositioned = False;
	}

	/* register the window */
	WIInstallInfo(w);

	/* set cursor for frame */
	XDefineCursor(dpy, w->core.self, GRV.IconPointer);

	iconSetName(w, panewin);

	w->fcore.fullsize = False;

	return w;
}

void
IconInit(dpy)
Display *dpy;
{
	classIconFrame.core.kind = WIN_ICON;
	classIconFrame.core.xevents[Expose] = WinEventExpose;
	classIconFrame.core.xevents[ButtonRelease] = GFrameEventButtonRelease;
	classIconFrame.core.xevents[MotionNotify] = GFrameEventMotionNotify;
	classIconFrame.core.xevents[ButtonPress] = GFrameEventButtonPress;
	classIconFrame.core.xevents[EnterNotify] = GFrameEventEnterNotify;
	classIconFrame.core.xevents[FocusIn] = GFrameEventFocus;
	classIconFrame.core.xevents[FocusOut] = GFrameEventFocus;
	classIconFrame.core.xevents[ConfigureRequest] = eventConfigureRequest;
	classIconFrame.core.focusfunc = GFrameFocus;
	classIconFrame.core.drawfunc = drawIcon;
	classIconFrame.core.destroyfunc = destroyIcon;
	classIconFrame.core.selectfunc = GFrameSelect;
	classIconFrame.core.newconfigfunc = newconfigIcon;
	classIconFrame.core.newposfunc = newposIcon;
	classIconFrame.core.setconfigfunc = GFrameSetConfigFunc;
	classIconFrame.core.createcallback = NULL;
	classIconFrame.core.heightfunc = NULL;
	classIconFrame.core.widthfunc =  NULL;
	classIconFrame.fcore.heighttop = heightTopIcon;
	classIconFrame.fcore.heightbottom = heightBottomIcon;
	classIconFrame.fcore.widthleft = widthBothIcon;
	classIconFrame.fcore.widthright = widthBothIcon;
	classIconFrame.fcore.menuPress = menuPressIcon;
	classIconFrame.fcore.adjustPress = NULL;
	classIconFrame.fcore.adjustClick = adjustClickIcon;
	classIconFrame.fcore.selectPress = NULL;
	classIconFrame.fcore.selectClick = NULL;
	classIconFrame.fcore.selectDoubleClick = selectDoubleClickIcon;
	classIconFrame.fcore.selectDrag = selectDragIcon;
	classIconFrame.fcore.fullrestoreToggle = fullrestoreIcon;
}

/*
 * DrawIconToWindowLines -- draw the "zoom" lines when a window is
 * opening or closing.  The lines are drawn from the corners of the icon to 
 * the corners of the window frame.
 */

#define NSEGS	    4

void
DrawIconToWindowLines(dpy, iconInfo, winInfo)
Display *dpy;
WinPaneFrame *winInfo;
WinIconFrame *iconInfo;
{
        int	    ii;
	GC	    rootGC;
	Window	    root;
	XSegment    segs[NSEGS];
#ifdef ALLPLANES
	Bool	    allplanes = winInfo->core.client->scrInfo->useAllPlanes;
#endif

	if (GRV.IconFlashCount <= 0)
	    return;

	rootGC = WinGC(winInfo, ROOT_GC);
	root = WinRootID(winInfo);

	XGrabServer(dpy);

	segs[0].x1 = iconInfo->core.x;
	segs[0].y1 = iconInfo->core.y;
	segs[0].x2 = winInfo->core.x;
	segs[0].y2 = winInfo->core.y;

	segs[1].x1 = iconInfo->core.x;
	segs[1].y1 = iconInfo->core.y + iconInfo->core.height;
	segs[1].x2 = winInfo->core.x;
	segs[1].y2 = winInfo->core.y + winInfo->core.height;

	segs[2].x1 = iconInfo->core.x + iconInfo->core.width;
	segs[2].y1 = iconInfo->core.y;
	segs[2].x2 = winInfo->core.x + winInfo->core.width;
	segs[2].y2 = winInfo->core.y;

	segs[3].x1 = iconInfo->core.x + iconInfo->core.width;
	segs[3].y1 = iconInfo->core.y + iconInfo->core.height;
	segs[3].x2 = winInfo->core.x + winInfo->core.width;
	segs[3].y2 = winInfo->core.y + winInfo->core.height;

        for(ii=0; ii < GRV.IconFlashCount; ii++) {

                /* draw */

#ifdef ALLPLANES
		if (allplanes)
		    XAllPlanesDrawSegments(dpy, root, segs, NSEGS);
		else
#endif /* ALLPLANES */
		    XDrawSegments(dpy, root, rootGC, segs, NSEGS);

                XFlush(dpy);
		olwm_usleep((unsigned) GRV.IconFlashOnTime);

                /* erase */

#ifdef ALLPLANES
		if (allplanes)
		    XAllPlanesDrawSegments(dpy, root, segs, NSEGS);
		else
#endif /* ALLPLANES */
		    XDrawSegments(dpy, root, rootGC, segs, NSEGS);

                XFlush(dpy);
		olwm_usleep((unsigned) GRV.IconFlashOffTime);
        }

	XUngrabServer(dpy);
}

#undef NSEGS


/* 
 * IconUpdateName -- the icon name property has been changed
 */
void
IconUpdateName(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	iconSetName(cli->iconwin,PANEWINOFCLIENT(cli));
}

/*
 * Set the icon's (x,y) location explicitly.  This information is typically
 * taken from the WM_HINTS structure.  Since the coordinates specify the 
 * absolute position of the icon pane, we must subtract the icon border to get 
 * the position if the icon frame.
 */
void
IconSetPos(win,x,y)
WinIconFrame *win;
int x,y;
{
    (WinFunc(win,core.newposfunc))(win,x-ICON_HORZBORDER,y-ICON_VERTBORDER);
}

/*
 * IconShow -- map an icon onto the screen, handling reparenting and
 * save-sets for icon panes.   In click-to-type, if this is the current 
 * client, select the icon to ensure that it's clear that this icon has the 
 * input focus.
 */
void
IconShow(cli, winIcon)
    Client *cli;
    WinIconFrame *winIcon;
{
    WinIconPane *pane = (WinIconPane *)winIcon->fcore.panewin;
    extern Client *CurrentClient;

    if (! GRV.FocusFollowsMouse && cli == CurrentClient) {
	ClearSelections(cli->dpy);
	AddSelection(cli, LastEventTime);
    }

    XReparentWindow(cli->dpy, pane->core.self, winIcon->core.self,
			pane->core.x, pane->core.y);
    XMapWindow(cli->dpy, pane->core.self);
    if (pane->iconClientWindow)
        XChangeSaveSet(cli->dpy, pane->core.self, SetModeInsert);
    XMapWindow(cli->dpy, winIcon->core.self);
}


/*
 * IconHide -- remove an icon from the screen, handling reparenting and
 * save-sets for icon panes.
 */
void
IconHide(cli, winIcon)
    Client *cli;
    WinIconFrame *winIcon;
{
    WinIconPane *pane = (WinIconPane *)winIcon->fcore.panewin;

    XUnmapWindow(cli->dpy, winIcon->core.self);
    XUnmapWindow(cli->dpy, pane->core.self);
    XReparentWindow(cli->dpy, pane->core.self, cli->scrInfo->rootid,
			winIcon->core.x + pane->core.x,
			winIcon->core.y + pane->core.y);
    if (pane->iconClientWindow)
        XChangeSaveSet(cli->dpy, pane->core.self, SetModeDelete);
}
