#ident	"@(#)states.c	26.66	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

/* states.c - functions relating to changes in client state 
 *	(Normal, Iconic, Withdrawn)
 */

#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "group.h"
#include "globals.h"
#include "properties.h"

/***************************************************************************
* global data
***************************************************************************/

extern Atom AtomWMClass;
extern Atom AtomDecorAdd;
extern Atom AtomDecorClose;
extern Atom AtomDecorDel;
extern Atom AtomDecorFooter;
extern Atom AtomDecorHeader;
extern Atom AtomDecorIconName;
#ifdef OW_I18N_L4
extern Atom AtomDecorIMStatus;
#endif
extern Atom AtomDecorPin;
extern Atom AtomDecorResize;
extern Atom AtomDeleteWindow;
extern Atom AtomMenuFull;
extern Atom AtomMenuLimited;
extern Atom AtomNone;
extern Atom AtomPinIn;
extern Atom AtomPinOut;
extern Atom AtomProtocols;
extern Atom AtomSaveYourself;
extern Atom AtomTakeFocus;
extern Atom AtomWinAttr;
extern Atom AtomWTBase;
extern Atom AtomWTCmd;
extern Atom AtomWTHelp;
extern Atom AtomWTNotice;
extern Atom AtomWTOther;

extern	int	WinDrawFunc();
extern	void	IconPaneSetPixmap();
extern	void	IconPaneSetMask();

/***************************************************************************
* private data
***************************************************************************/

/* sanity checks for getting stuff out of hints */
#define IsCard16(x)	((x) == ((unsigned short)(x)) && (x) > 0 )
#define IsInt16(x)	((x) == ((short) (x)))

static WMDecorations BaseWindow = {
    WMDecorationCloseButton | WMDecorationResizeable | WMDecorationHeader 
	| WMDecorationIconName,
    MENU_FULL,
    0,
    PIN_IN,
    0
};

static WMDecorations CmdWindow = {
    WMDecorationPushPin | WMDecorationResizeable | WMDecorationHeader
	| WMDecorationIconName,
    MENU_LIMITED,
    0,
    PIN_IN,
    0
};

static WMDecorations NoticeWindow = {
    WMDecorationIconName,
    MENU_NONE,
    0,
    PIN_IN,
    0
};

static WMDecorations HelpWindow = {
    WMDecorationPushPin | WMDecorationHeader | WMDecorationIconName
	| WMDecorationWarpToPin,
    MENU_LIMITED,
    0,
    PIN_IN,
    0
};

static WMDecorations OtherWindow = {
    WMDecorationIconName,
    MENU_NONE,
    0, 
    PIN_IN,
    0
};

static WMDecorations TransientWindow = {
    WMDecorationResizeable | WMDecorationIconName,
    MENU_LIMITED,
    0,
    PIN_IN,
    0
};

static WMDecorations MinimalWindow = {
    WMDecorationResizeable | WMDecorationIconName,
    MENU_FULL,
    0,
    PIN_IN,
    0
};

typedef struct {
	char *class, *instance;
} minimalclosure;


/***************************************************************************
* private functions
***************************************************************************/

/*
 * Determine FocusMode from wmHints and protocols
 */
static FocusMode
focusModeFromHintsProtocols(wmHints,protocols)
	XWMHints	*wmHints;
	int		protocols;
{
	FocusMode	focusMode;

	if (wmHints && wmHints->input) {
		if (protocols & TAKE_FOCUS)
			focusMode = LocallyActive;	
		else
			focusMode = Passive;	
	} else { /* wmHints->input == False */
		if (protocols & TAKE_FOCUS)
			focusMode = GloballyActive;	
		else
			focusMode = NoInput;	
	}
	return focusMode;
}

/* 
 * matchInstClass -- run through the list of names to be minimally decorated,
 * and see if this window's class or instance match any.
 */
static Bool
matchInstClass(str, mc)
    char *str;
    minimalclosure *mc;
{
    if ((mc->class == NULL) || (strcmp(str, mc->class) != 0))
	return ((mc->instance != NULL) && (strcmp(str, mc->instance) == 0));
    else
	return True;
}


/*
 * getOlWinDecors - given the window attributes and decoration add/delete
 *	requests, determine what kind of window (according to the OpenLook
 *	kinds of windows) the client represents, and determine what sort of
 *	decorations are appropriate.
 */
WMDecorations *
getOLWinDecors(dpy, win, transient, cli)
Display *dpy;
Window  win;
Bool    transient;
Client	*cli;
{
        WMDecorations	       *decors;
	minimalclosure		mc;
	XWMHints	        *wmHints = cli->wmHints;
	OLWinAttr		winAttrs;
	Bool			oldVersion;
	int			decorFlags;

        decors = MemNew(WMDecorations);

	/*
	 * REMIND: there is no way for a program to specify the default item, 
	 * so this always initialized to zero.
	 */
	decors->def_item = 0;

#ifdef SHAPE
	if (cli->isShaped) {
	    *decors = MinimalWindow;
	    return decors;
	}
#endif

	oldVersion = False;

	/*
	 * If the _OL_WIN_ATTR property is not present then make the
	 * window into a base window unless is a transient window.
	 */
	if (!PropGetOLWinAttr(dpy, win, &winAttrs, &oldVersion)) {
		if (transient) {
			*decors = TransientWindow;
			if (GRV.TransientsTitled)
				decors->flags |= WMDecorationHeader;
		} else {
			*decors = BaseWindow;
		}
	/*
 	 * Else we do have that property; so interpret it
 	 */
	} else {
		/*
		 * Choose the decor from win_type
		 */
		if ((winAttrs.flags & WA_WINTYPE) == 0) {
		    	*decors = BaseWindow;
		} else if (winAttrs.win_type == AtomWTBase) {
		    	*decors = BaseWindow;
		} else if (winAttrs.win_type == AtomWTCmd) {
		    	*decors = CmdWindow;
		} else if (winAttrs.win_type == AtomWTHelp) {
		    	*decors = HelpWindow;
		} else if (winAttrs.win_type == AtomWTNotice) {
		    	*decors = NoticeWindow;
		} else if (winAttrs.win_type == AtomWTOther) {
		    	*decors = OtherWindow;
		}

	    	/* 
		 * Override the decor/menu_type if specified
		 */
	    	if (winAttrs.flags & WA_MENUTYPE) {
			if (winAttrs.menu_type == AtomMenuFull)
				decors->menu_type = MENU_FULL;
			else if (winAttrs.menu_type == AtomMenuLimited)
				decors->menu_type = MENU_LIMITED;
			else if (winAttrs.menu_type == AtomNone)
				decors->menu_type = MENU_NONE;
	   	 }

		/*
	 	 * Backward compatibility.  If we had a old/short attribute 
		 * property, and the client specified an icon window, we're 
		 * probably dealing with an old XView client.  These clients 
		 * assume the window manager doesn't put the icon name in 
		 * the icon, so they paint it into the icon window itself.
		 * Turn off the painting of the icon name for icons of 
		 * these windows.
		 */
		if (oldVersion && wmHints && (wmHints->flags & IconWindowHint))
			decors->flags &= ~WMDecorationIconName;

		/*
		 * Set cancel if something specified
		 */
		if (winAttrs.flags & WA_CANCEL)
			decors->cancel = (winAttrs.cancel != 0);

		/*
		 * Set the pin state
		 */
		if (winAttrs.flags & WA_PINSTATE) {
			decors->pushpin_initial_state = 
						winAttrs.pin_initial_state;
		} else {
			decors->pushpin_initial_state = PIN_OUT;
		}
#ifdef OW_I18N_L4
		/* check if need to decor the IM status */
		if (oldVersion) {
		}
#endif
	}

	/*
 	 * Apply DecorAdd flags
	 */
	if (PropGetOLDecorAdd(dpy, win, &decorFlags)) {
		decors->flags |= decorFlags;
	}

	/*
 	 * Apply DecorDel flags
	 */
	if (PropGetOLDecorDel(dpy, win, &decorFlags)) {
		decors->flags &= ~decorFlags;
	}

	/*
	 * If the instance or class strings match any of the names
	 * listed for minimal decoration, remove the header.
	 */
	mc.class = cli->wmClass;
	mc.instance = cli->wmInstance;
	if (ListApply(GRV.Minimals, matchInstClass, &mc) != NULL)
	{
	    decors->flags &= ~WMDecorationHeader;
	}

	/*
	 * Below, apply constraints to ensure that decorations are
	 * consistent.
	 */

	/* No header implies no window button or pushpin. */

	if (!(decors->flags & WMDecorationHeader)) {
		decors->flags &= ~(WMDecorationHeaderDeco);
	}

	/* Can't have button and pushpin; pushpin wins. */

        if ((decors->flags & WMDecorationCloseButton) &&
            (decors->flags & WMDecorationPushPin))
                decors->flags &= ~(WMDecorationCloseButton);

	/* Don't warp to the pin if there's no pin. */

	if (!(decors->flags & WMDecorationPushPin))
	    decors->flags &= ~WMDecorationWarpToPin;

        return decors;
}


/*
 * clientSpecifiedPosition
 *
 * Return an indication of whether the client has specified a position using
 * its hints.  This is true in the typical case if either the USPosition or
 * PPosition flags are set.  However, if the PPositionCompat option is on, the
 * PPosition flag is ignored if the specified position is at or above and to
 * the left of (1,1).
 *
 * The point of PPositionCompat is that many old clients (X11R3 and prior)
 * always set the PPosition flag, even when they had no useful position to
 * request.  When this occurred, the requested position was almost always
 * (1,1) or thereabouts.
 */
static Bool
clientSpecifiedPosition(normHints, paneAttr)
    XSizeHints		*normHints;
    XWindowAttributes	*paneAttr;
{
    return (normHints->flags & USPosition) ||
	   ((normHints->flags & PPosition) &&
	    !(GRV.PPositionCompat && paneAttr->x <= 1 && paneAttr->y <= 1));
}


/*
 * Return an indication of whether this frame would be visible on the screen 
 * if it were mapped at the given location.  Visibility is defined as having 
 * at least one resize-corner width (or height) on the screen.
 */
static Bool
frameOnScreen(winFrame, scrInfo, x, y)
    WinPaneFrame *winFrame;
    ScreenInfo *scrInfo;
    int x, y;
{
    int dx, dy;
    int sw = DisplayWidth(scrInfo->dpy, scrInfo->screen);
    int sh = DisplayHeight(scrInfo->dpy, scrInfo->screen);

    /* REMIND */
    extern int Resize_width, Resize_height;
    extern void FrameGetGravityOffset();

    FrameGetGravityOffset(winFrame, &dx, &dy);
    x -= dx;
    y -= dy;

    return (x + Resize_width <= sw &&
	    y + Resize_height <= sh &&
	    x + (int) winFrame->core.width >= Resize_width &&
	    y + (int) winFrame->core.height >= Resize_height);
}


/*
 * calcPosition
 *
 * Calculate the next position to place a new window.  This function places
 * all new windows on the diagonal and makes sure that there is enough room on
 * the screen for the new window's size passed in w and h.
 *
 * Changes the x and y members of the attrs structure; also changes the 
 * screen's notion of the location for the next frame.
 */
static void
calcPosition(dpy, screen, attrs, frame)
    Display		*dpy;
    int			screen;
    XWindowAttributes	*attrs;
    WinPaneFrame	*frame;
{
	int		stepValue;
	ScreenInfo	*scrInfo;

	if ((scrInfo = GetScrInfoOfScreen(screen)) == NULL) {
		attrs->x = attrs->y = 0;
		return;
	}

	/* if the height of the current window is too large ... */
	if ((scrInfo->framepos + frame->core.height
		> DisplayHeight(dpy, screen)) ||
	    (scrInfo->framepos + frame->core.width
		> DisplayWidth(dpy, screen)))
	{
	    scrInfo->framepos = 0;
	}

	/* REMIND this should really be based on the header height */
	stepValue = 30;

	/* we will return the current position */
	attrs->x = attrs->y = scrInfo->framepos;

	/* calculate the next return value */
	scrInfo->framepos = scrInfo->framepos + stepValue;
	if ((scrInfo->framepos > DisplayWidth(dpy, screen)) ||
	    (scrInfo->framepos > DisplayHeight(dpy, screen)))
	{
	    scrInfo->framepos = 0;
	}
}


/*
 * iconifyOne -- iconify one client to IconicState from NormalState
 */
static void *
iconifyOne(cli, winIcon)
Client *cli;
WinGeneric *winIcon;
{
	if (cli->groupmask == GROUP_DEPENDENT)
    	    RemoveSelection(cli);
	else
	    DrawIconToWindowLines(cli->dpy, winIcon, cli->framewin);

	XUnmapWindow(cli->dpy, cli->framewin->core.self);
	XUnmapWindow(cli->dpy, PANEWINOFCLIENT(cli));
	cli->framewin->fcore.panewin->pcore.pendingUnmaps++;

	ClientSetWMState(cli,IconicState);

	return NULL;
}

/* deiconifyOne -- deiconify one client to NormalState from IconicState
 */
static void *
deiconifyOne(cli, winIcon, raise)
Client *cli;
WinGeneric *winIcon;
Bool raise;
{
	if (cli->groupmask != GROUP_DEPENDENT)
	    DrawIconToWindowLines(cli->dpy, winIcon, cli->framewin);

	if (raise)
	    XRaiseWindow(cli->dpy, cli->framewin->core.self);

	XMapWindow(cli->dpy, cli->framewin->core.self);
	XMapRaised(cli->dpy, PANEWINOFCLIENT(cli));

	ClientSetWMState(cli,NormalState);

	return NULL;
}


/*
 * markFrame
 *
 * Marks a client's frame window with a given value.  Suitable for calling by
 * ListApply or GroupApply.
 */
static void *
markFrame(cli, value)
    Client *cli;
    int value;
{
    if (cli->framewin != NULL)
	cli->framewin->core.tag = value;
    return NULL;
}


/*
 * unmarkAllFrames -- Clear the tag field of the frame window of every client.
 */
static void *
unmarkAllFrames()
{
    List *l = ActiveClientList;
    Client *tc;

    for (tc = ListEnum(&l); tc != NULL; tc = ListEnum(&l))
	markFrame(tc, 0);
    return NULL;
}


#ifdef DEBUG

static void
printClientList()
{
    List *l = ActiveClientList;
    Client *tc;

    for (tc = ListEnum(&l); tc != NULL; tc = ListEnum(&l))
	printf("0x%x\n", (unsigned int) tc);
    fflush(stdout);
}

static void *
printGroupMember(cli, value)
    Client *cli;
    int value;
{
    printf("0x%x\n", (unsigned int) cli);
    return NULL;
}


static void
printGroupList(id)
    unsigned long id;
{
    GroupApply(id, printGroupMember, 0,
	GROUP_LEADER | GROUP_DEPENDENT | GROUP_INDEPENDENT);
    fflush(stdout);
}


#endif /* DEBUG */

/*
 * deiconifyGroup
 *
 * Deiconify a window group, preserving stacking order.  Mark all the frames
 * that are to be deiconified, then query the server for all children-of-root.
 * Walk backward through this array (i.e. from top to bottom).  For each group
 * member found, stack it just below the previous one (raise the first one to
 * the top) and deiconify it.  Finally, unmark all the frames in the group.
 * Note: this algorithm depends on having the stacking order of windows
 * preserved when the group is iconified.
 */
static void
deiconifyGroup(cli, winIcon)
    Client *cli;
    WinIconFrame* winIcon;
{
    Window root, parent;
    Window *children;
    Window prev = None;
    unsigned int nchildren;
    int i;
    WinGeneric *wi;
    XWindowChanges xwc;

    unmarkAllFrames();

    if (cli->groupmask == GROUP_LEADER) {
	GroupApply(cli->groupid, markFrame, 1,
		   GROUP_LEADER | GROUP_DEPENDENT);
    } else if (cli->groupmask == GROUP_INDEPENDENT) {
	markFrame(cli, 1);
	GroupApply(PANEWINOFCLIENT(cli), markFrame, 1,
		   GROUP_DEPENDENT);
    }

    (void) XQueryTree(cli->dpy, cli->scrInfo->rootid, &root, &parent,
		      &children, &nchildren);

    xwc.stack_mode = Below;
    for (i=nchildren-1; i>=0; --i) {
	wi = WIGetInfo(children[i]);
	if (wi != NULL && wi->core.tag == 1) {
	    if (prev == None) {
		XRaiseWindow(cli->dpy, children[i]);
	    } else {
		xwc.sibling = prev;
		XConfigureWindow(cli->dpy, children[i],
				 CWSibling|CWStackMode, &xwc);
	    }
	    prev = children[i];
	    deiconifyOne(wi->core.client, winIcon, False);
	}
    }

    unmarkAllFrames();

    if (children != NULL)
	XFree((char *)children);
}


/*
 * promoteDependentFollowers -- called for a newly appearing dependent
 * followers.  Promote all dependent followers of "window" to be dependent
 * followers of the leader of the new window "groupid".  In other words,
 * suppose we have a group relationship of C->B->A, where "->" means "is a
 * dependent follower of", and B is a newly appearing window.  For all such
 * windows C that are followers of B, make them followers of A.
 *
 * REMIND we don't update the group data structures while the GroupApply is in
 * progress.  Doing so will corrupt the group data structure.
 */
static void *
promoteDependentFollowers(window, groupid)
    Window window;
    Window groupid;
{
    List *l = ActiveClientList;
    Client *cli;

    unmarkAllFrames();
    GroupApply(window, markFrame, 1, GROUP_DEPENDENT);
    for (cli = ListEnum(&l); cli != NULL; cli = ListEnum(&l)) {
	if (cli->framewin && cli->framewin->core.tag) {
	    GroupRemove(window, cli);
	    GroupAdd(groupid, cli, GROUP_DEPENDENT);
	    cli->groupid = groupid;
	}
    }
    unmarkAllFrames();
}


/***************************************************************************
* global functions
***************************************************************************/


/*
 * StateNew -- A client is mapping a top-level window (either a new window
 *	or a Withdrawn window).  The window may become Iconic or Normal 
 *	depending on the hints.  Check to see if this window needs to be mapped
 *	and if so add the required adornments.
 *		dpy 		-- display pointer
 *		rootWin		-- root window
 *					if None will determine the root window
 *					for the client window
 *		window 		-- client's window
 *		fexisting	-- the window already exists and we
 *				   are starting olwm, so positioning should
 *				   be special-cased
 *		ourWinInfo	-- if is this one of our menu windows, this
 *			will be its WinMenu structure; this window must
 *			be a subclass of Pane
 */
Client *
StateNew(dpy, rootWin, window, fexisting, ourWinInfo)
Display *dpy;
Window rootWin;
Window window;
Bool fexisting;
WinPane *ourWinInfo;
{
	Client 		*cli;
	WinGeneric	*winGeneric;
	WinPane		*winPane;
	WinIconFrame	*winIcon;
	WinPaneFrame	*winFrame;
	WinIconPane	*winIconPane;
	XSizeHints	*normHints;
	Bool		preICCCM;
	Bool		transient = False;
	int		status;
	int		initstate;
	XWindowAttributes paneAttr;
	int 		screen;
	int		tmpx, tmpy;
	ScreenInfo 	*scrInfo;
	int		winState;
	Window		iconWin;

	/*
	 * If the window is thought to be new (i.e. if ourWinInfo is null, as
	 * it is always except for the case of pinned menus) and the window
	 * has already been registered in the WinInfo database and it's
	 * anything other than colormap window, then return.
	 *
	 * This is to head off (a) clients that might be mapping the olwm
	 * frame, (b) clients that map their top-level window (pane) more than
	 * once before olwm can reparent it to a frame, and (c) olwm's own 
	 * popup menus.
	 */
	if (!ourWinInfo &&
	   (winGeneric = WIGetInfo(window)) != NULL &&
	    winGeneric->core.kind != WIN_COLORMAP) {
		return NULL;
	}

	/* Find the screen the client window is on.
	 * If ourWinInfo is valid, use it's screen
	 * Else if know the root then use it's screen
	 * Lastly QueryTree to find out from the server
	 */
	if (ourWinInfo) {
		scrInfo = ourWinInfo->core.client->scrInfo;
	} else if (rootWin != None) {
		if ((scrInfo = GetScrInfoOfRoot(rootWin)) == NULL) 
			return NULL;
	} else {
		Window	root, parent, *children;
		unsigned int	nChild;
		Status	result;
		
		result  = XQueryTree(dpy, window, &root, &parent, 
				&children, &nChild);

		if (result == 0 || parent != root)
			return NULL;
		if ((scrInfo = GetScrInfoOfRoot(root)) == NULL) 
			return NULL;
	}
	screen = scrInfo->screen;

	/*
	 * Select for events on the pane right now (including StructureNotify)
	 * so that we are guaranteed to get a DestroyNotify if the window goes
	 * away.  If the window has already gone away, the call to
	 * XGetWindowAttributes below will tell us without race conditions.
	 */
	if (!ourWinInfo)
	    XSelectInput(dpy, window,
			 PropertyChangeMask | StructureNotifyMask |
			 ColormapChangeMask | EnterWindowMask);

        /* get all the info about the new pane */
        status = XGetWindowAttributes(dpy, window, &paneAttr);
        if ( status == 0 ) {
            return NULL;
        }

	/*
	 * If it's an override-redirect window, or if already exists but is 
	 * unmapped, ignore it after first removing our StructureNotify 
	 * interest.
	 */
	if (paneAttr.override_redirect ||
		(fexisting && paneAttr.map_state != IsViewable)) {
	    if (!ourWinInfo)
		XSelectInput(dpy, window, NoEventMask);
	    return NULL;
	}

	/* Create the client structure so we can start hooking things to it */
	if ((cli = ClientCreate(dpy,screen)) == NULL)
	{
	    return NULL;
	}

#ifdef SHAPE
	{
	    Bool bshaped, cshaped;
	    int bx, by, cx, cy;
	    unsigned int bw, bh, cw, ch;

	    if (ShapeSupported &&
		0 != XShapeQueryExtents(dpy, window, &bshaped, &bx, &by,
					&bw, &bh, &cshaped, &cx, &cy,
					&cw, &ch))
	    {
		XShapeSelectInput(dpy, window, ShapeNotifyMask);
		cli->isShaped = bshaped;
	    } else {
		cli->isShaped = False;
	    }
	}
#endif /* SHAPE */

	/*
 	 * Turn on prop read filtering with set of available properties
 	 */
	PropSetAvailable(dpy,window);

	/*
	 * Get the WM_TRANSIENT_FOR hint.  If the property exists but has a
	 * contents of zero, or the window itself, substitute the root's
	 * window ID.  This is because some (buggy) clients actually write
	 * zero in the WM_TRANSIENT_FOR property, and we want to give them
	 * transient window behavior.
	 */
	if (!PropGetWMTransientFor(dpy,window,cli->scrInfo->rootid,
					&(cli->transientFor))) {
		cli->transientFor = 0;
		transient = False;
	} else {
		transient = True;
	}

	/*
	 * Get the WM_NORMAL_HINTS property.  If it's short, then we have a
	 * pre-ICCCM client on our hands, so we interpret some values 
	 * specially.
	 */
	normHints = MemNew(XSizeHints);

	if (!PropGetWMNormalHints(dpy,window,normHints,&preICCCM)) {
		normHints->win_gravity = NorthWestGravity;
		normHints->flags = PWinGravity;
	}

	/*
	 * We got a short property.  Assume that this is a pre-X11R4
	 * client who's using the short version of the property.  Copy
	 * the data into a correctly-sized structure.  Then, depending
	 * on the flags set, ignore the window's real geometry and use
	 * the data in the hint (but only if it passes some sanity 
	 * checking).  The sanity checking is necessary because early 
	 * versions of XView write a short property, but rely on the 
	 * window manager to look at the window's geometry instead of 
	 * at the values in the hint.
	 */
	if (preICCCM) {
		int	 maxDpyWidth = 2*DisplayWidth(dpy,screen);
		int	 maxDpyHeight = 2*DisplayHeight(dpy,screen);

		if (!fexisting
		    && (normHints->flags & (USPosition|PPosition))
		    && IsInt16(normHints->x) 
		    && IsInt16(normHints->y)
		    && normHints->x > -maxDpyWidth
		    && normHints->y > -maxDpyHeight
		    && normHints->x < maxDpyWidth
		    && normHints->y < maxDpyHeight) {
			paneAttr.x = normHints->x;
			paneAttr.y = normHints->y;
		}
		if ((normHints->flags & (USSize|PSize)) 
		    && IsCard16(normHints->width) 
		    && IsCard16(normHints->height)
		    && normHints->width >= MINSIZE
		    && normHints->height >= MINSIZE
		    && normHints->width < maxDpyWidth
		    && normHints->height < maxDpyHeight) {
			paneAttr.width = normHints->width;
			paneAttr.height = normHints->height;
		}
	}

	cli->normHints = normHints;

	/*
	 * Get the WM_HINTS
	 */
	cli->wmHints = MemNew(XWMHints);

	if (!PropGetWMHints(dpy,window,cli->wmHints)) {
		cli->wmHints->flags = 0L;
	}

	/* 
	 * Get the protocols in which the client will participate
	 */
	if (!PropGetWMProtocols(dpy,window,&(cli->protocols))) {
		cli->protocols = 0;
	}

        /* 
	 * Figure out what focus mode this window intends
	 */
	cli->focusMode = focusModeFromHintsProtocols(cli->wmHints,
						     cli->protocols);

	/* 
	 * Get the window class and instance strings
	 */
	if (!PropGetWMClass(dpy,window,&(cli->wmClass),&(cli->wmInstance))) {
		cli->wmClass = cli->wmInstance = NULL;
		cli->wmClassQ = cli->wmInstanceQ = NULLQUARK;
	}

	cli->wmClassQ = XrmStringToQuark(cli->wmClass);
	cli->wmInstanceQ = XrmStringToQuark(cli->wmInstance);

	ClientSetInstanceVars(cli);

	/* 
	 * Get the OpenLook window type and associated decorations
	 */
	cli->wmDecors = getOLWinDecors(dpy, window, transient, cli);

	/*
	 * Establish window groups.  Policy: if the window is transient, this 
	 * takes priority over any window group specified in WM_HINTS.  If
	 * it's transient, make it be part of the window group of the window 
	 * it is transient for.  Otherwise, use the group specified in
	 * WM_HINTS.  If no group is specified in WM_HINTS, consider the 
	 * window to be the leader of its own group.
	 */
	if (transient) {
	    winGeneric = WIGetInfo(cli->transientFor);
	    if (winGeneric != NULL &&
	    winGeneric->core.client->groupmask == GROUP_DEPENDENT)
		cli->groupid = winGeneric->core.client->groupid;
	    else
		cli->groupid = cli->transientFor;
	} else if ((cli->wmHints) && (cli->wmHints->flags & WindowGroupHint)) {
	    winGeneric = WIGetInfo(cli->wmHints->window_group);
	    if (winGeneric != NULL &&
	    winGeneric->core.client->groupmask == GROUP_DEPENDENT)
		cli->groupid = winGeneric->core.client->groupid;
	    else
		cli->groupid = cli->wmHints->window_group;
	} else {
	    cli->groupid = window;
	}

	/*
	 * Determine group role: leader, independent follower, or dependent
	 * follower.  Leader and independent followers can be iconified
	 * themselves, while dependent followers iconify with their parent.
	 *
	 * A window is a dependent follower if it's a group follower and:
	 * 
	 * - it's transient, or
	 * - it's a popup window.
	 *
	 * A window is considered to be a popup window if:
	 * 
	 * - it has a pin, or
	 * - it has a limited menu.
	 *
	 * If this window is a dependent follower, its followers are
	 * "promoted" to be followers of this window's leader.
	 * 
	 * If a window is a group follower but doesn't satisfy any of these 
	 * criteria, it's considered an independent follower.
	 */

	if (cli->groupid == window) {
	    cli->groupmask = GROUP_LEADER;
	} else {
	    if (transient || ClientIsPopup(cli)) {
		cli->groupmask = GROUP_DEPENDENT;
		promoteDependentFollowers(window, cli->groupid);
	    } else {
		cli->groupmask = GROUP_INDEPENDENT;
	    }
	}
	GroupAdd(cli->groupid,cli,cli->groupmask);


	/* 
	 * Officially set up the frame
	 */
	winFrame = MakeFrame(cli,window,&paneAttr);

	/*
	 * If a client-created window then create the pane for it.  Otherwise,
	 * call the creation callback function; this is used for pinned menus.
	 */
	if (ourWinInfo == NULL) {
		winPane = MakePane(cli,winFrame,window,&paneAttr);
	} else {
		winPane = ourWinInfo;
		(WinClass(winPane)->core.createcallback)(ourWinInfo,cli,
								winFrame);
	}

	/*
	 * We use the window's position if:
	 * 
	 * + it's an existing window, or
	 * + the client has specified that its position be used and its
	 *   position leaves at least part of the frame on the screen.
	 *
	 * Otherwise, we calculate a position for the window and place it 
	 * there.
	 */
	if (! (fexisting ||
	       clientSpecifiedPosition(normHints, &paneAttr) &&
	       frameOnScreen(winFrame, scrInfo, paneAttr.x, paneAttr.y)))
	{
	    calcPosition(dpy, screen, &paneAttr, winFrame);
	}

	/* 
	 * Officially set up the icon
	 */
	winIcon = MakeIcon(cli,window,&paneAttr);
	winIconPane = MakeIconPane(cli,winIcon,cli->wmHints,fexisting);

	/* 
	 * Keep track of any subwindows that need colormap installation
	 */
	TrackSubwindows(cli);

	/* 
	 * Size and generally configure the frame window tree
	 */
	FrameSetPosFromPane(winFrame,paneAttr.x,paneAttr.y);
	WinCallConfig(dpy, winPane, NULL);

	/* 
	 * Size and generally configure the icon window tree
	 */
	WinCallConfig(dpy, winIconPane, NULL);
	if (cli->wmHints != NULL)
	    IconSetPos(winIcon,cli->wmHints->icon_x,cli->wmHints->icon_y);
	else
	    IconSetPos(winIcon,0,0);
	WinCallConfig(dpy, winIcon, NULL);

        /*
	 * We manually move the icon pane window, since all the configuration
	 * has been done with the icon pane parented to root.
	 */
        WinRootPos(winIconPane, &tmpx, &tmpy);
        XMoveWindow(dpy, winIconPane->core.self, tmpx, tmpy);

	/* 
	 * Determine the proper initial state of the window. 
	 * If the window already exists and there is a WM_STATE property 
	 * then use the state that the last window manager left there, 
	 * otherwise use WM_HINTS.
	 */
	if (fexisting &&
	    PropGetWMState(dpy,winPane->core.self,&winState,&iconWin)) {
	    if (winState == IconicState)
		initstate = IconicState;
	    else
		initstate = NormalState;
	} else {
	    /* For new windows, check the initial_state field of WM_HINTS. */
	    if (cli->wmHints && (cli->wmHints->flags & StateHint)
		&& (cli->wmHints->initial_state == IconicState))
		    initstate = IconicState;
	    else
		    initstate = NormalState;
	}

	/*
	 * Don't allow the popup into iconic state if its leader is in normal 
	 * state.
	 */
	if (cli->groupmask == GROUP_DEPENDENT && initstate == IconicState) {
	    Client *leader = GroupLeader(cli->groupid);
	    if (leader != NULL && leader->wmState == NormalState)
		initstate = NormalState;
	}

	ClientProcessDragDropInterest(cli, PropertyNewValue);

	/*
	 * Put the window into the correct initial state
	 */

	/* REMIND - The call to ClientSetWMState() should be
	 * here instead of directly assigning cli->wmState.
	 * But if we did that clients would see a different
	 * event order than it saw in earlier releases.
	 * This way the client will see the MapNotify from
	 * the XMapRaised() and then the PropertyNotify from
	 * ClientSetWMState().
	 * In a future major release we should change this.
	 */
	cli->wmState = initstate;

	switch ( initstate ) {
	case NormalState:
	    XMapRaised(dpy, winFrame->core.self);
	    XMapRaised(dpy, winPane->core.self);
	    if (!fexisting) {
		FrameWarpPointer(cli);
		if (GRV.AutoInputFocus)
		    ClientSetFocus(cli, True, CurrentTime);
		if (GRV.AutoColorFocus)
		    LockColormap(dpy, cli, winPane);
	    }
	    break;
	case IconicState:
	    /* unmap the window in case it was mapped originally */
	    XUnmapWindow(dpy, winPane->core.self);
	    winPane->pcore.pendingUnmaps++;
	    /* dependent group followers don't get their own icons */
	    if (cli->groupmask != GROUP_DEPENDENT)
		IconShow(cli, winIcon);
	    break;
	}
	ClientSetWMState(cli,initstate);

	/* 
	 * Get the window state
	 */
	ClientGetWindowState(cli);

	/*
 	 * Turn off prop read filtering
 	 */
	PropClearAvailable();

	return cli;
}

/*
 * ReparentTree -- called at start up, this routine queries the window
 *	tree and reparents all the windows 
 */
void
ReparentTree(dpy,treeroot)
Display	*dpy;
Window 	treeroot;
{
	unsigned int numChildren;
	Window *children, root, parent, w;
	int ii;
	Client *cli;

	children = NULL;

	if (XQueryTree(dpy, treeroot, &root, &parent,
				      &children, &numChildren)) 
	{
	    for (ii=0; ii<numChildren; ii++)
	    {
		w = children[ii];
		if (WIGetInfo(w) == NULL)
		{
	            cli = StateNew(dpy, treeroot, w, True, NULL);
		    if (cli != NULL)
		    {
			cli->framewin->fcore.panewin->pcore.pendingUnmaps++;	
					/* unmap because of reparent */
		    }
		}
	    }
	}

	if (children != NULL)
		XFree((char *)children);
}


/* 
 * StateIconic - transition a window to IconicState
 */
void
StateIconic(cli,timestamp)
Client *cli;
Time	timestamp;
{
	WinIconFrame 	*iconInfo = cli->iconwin;
	WinPaneFrame 	*frameInfo = cli->framewin;
	unsigned int	groupApplyMask = 0;

	if (iconInfo == NULL || frameInfo == NULL || !ClientHasIcon(cli))
		return;

	/*
	 * Transition from the various states
	 */
	switch (cli->wmState) {

	case IconicState:
		/* If already iconic just return */
		return;

	case NormalState:

		switch (cli->groupmask) {

		/*
		 * Iconify group leader and all dependent followers
		 */
		case GROUP_LEADER:
			IconShow(cli, iconInfo);
			GroupApply(cli->groupid, iconifyOne, iconInfo,
				GROUP_LEADER|GROUP_DEPENDENT);
			break;
		/*
		 * Iconify the group independent follower and its dependent 
		 * followers
		 */
		case GROUP_INDEPENDENT:
			IconShow(cli, iconInfo);
			iconifyOne(cli, iconInfo);
			GroupApply(PANEWINOFCLIENT(cli), iconifyOne,
				iconInfo, GROUP_DEPENDENT);
			break;
		/*
		 * Dont inconify the dependent follower by itself since
		 * it should only go iconic if it's group leader is going 
		 * iconic (above)
		 */
		case GROUP_DEPENDENT:
			return;
		}

		break;

	case InvisibleState:
		/*
		 * Map the icon window and update the client wmState
		 */
		IconShow(cli, iconInfo);
		ClientSetWMState(cli, IconicState);
		break;
	}

	/*
	 * Set focus to the newly iconified client if it's the current
	 * client and we're in click-to-type mode
	 */
	if (cli == CurrentClient && !GRV.FocusFollowsMouse)
		ClientSetFocus(cli, False, timestamp);
}

/* 
 * StateNormal - transition a window to NormalState
 */
void
StateNormal(cli,timestamp)
Client *cli;
Time	timestamp;
{
	WinIconFrame 	*iconInfo = cli->iconwin;
	WinPaneFrame 	*frameInfo = cli->framewin;
	Display 	*dpy = cli->dpy;

	if (iconInfo == NULL || frameInfo == NULL)
		return;

	/*
	 * Transition from the various states
	 */
	switch (cli->wmState) {

	case IconicState:
		/*
	 	 * Unmap the icon.  This must be done before mapping the 
		 * frame windows, so that we get LeaveNotify events (that 
		 * cause us to change focus) before the exposure events on 
		 * the frame.  If we mapped the frames first, they'd be 
		 * painted with the focus highlight.
	 	 */
		IconHide(cli, iconInfo);

		/* Map the frame window and any group followers. */
		if (cli->groupmask == GROUP_DEPENDENT)
			deiconifyOne(cli,iconInfo,True);
		else
			deiconifyGroup(cli, iconInfo);
		break;

	case NormalState:
		/* If already in normal just return */
		return;

	case InvisibleState:
		/*
		 * Map frame and pane and update client's wmState
		 */
		XMapWindow(dpy, frameInfo->core.self);
		XMapRaised(dpy, PANEWINOFCLIENT(cli));
		ClientSetWMState(cli,NormalState);
		break;
	}

	/*
	 * Transfer the focus to the newly mapped frame, but only if we're in
	 * click-to-type mode.
	 */
	if (cli == CurrentClient && !GRV.FocusFollowsMouse)
		ClientSetFocus(cli, True, timestamp);
}

/*
 * StateWithdrawn - a window is being withdrawn; tear down all related
 *	structures; clear the client out of all lists it may be
 * 	on; reparent the pane window
 */
void
StateWithdrawn(cli,timestamp)
Client *cli;
Time	timestamp;
{
	WinIconFrame 	*iconInfo = cli->iconwin;
	WinPaneFrame 	*frameInfo = cli->framewin;
	Display 	*dpy = cli->dpy;

	if (iconInfo == NULL || frameInfo == NULL)
		return;

	/* Zero event mask to cut down on unneeded events */
	XSelectInput(dpy,PANEWINOFCLIENT(cli),NoEventMask);

	/*
	 * Transition from the various states
	 */
	switch (cli->wmState) {

	case IconicState:	
		/* Unmap the icon */
		IconHide(cli, iconInfo);
		break;

	case NormalState:
		/* Unmap the frame and pane. */
        	XUnmapWindow(dpy, frameInfo->core.self);
        	XUnmapWindow(dpy, PANEWINOFCLIENT(cli));
		break;

	case InvisibleState:
		/* Both icon and frame are already unmapped so do nothing */
		break;
	}

        /* Return the pointer if necessary */
	FrameUnwarpPointer(cli);

	/* Move the pane and unparent it */
	FrameUnparentPane(cli, frameInfo, frameInfo->fcore.panewin);

	DestroyClient(cli);
}

/*
 * StateInvisible - transition a window to InvisibleState
 *	Unmap either icon or frame windows and update wmState
 */
void
StateInvisible(cli,timestamp)
Client	*cli;
Time	timestamp;
{
	WinIconFrame 	*iconInfo = cli->iconwin;
	WinPaneFrame 	*frameInfo = cli->framewin;
	Display 	*dpy = cli->dpy;

	if (iconInfo == NULL || frameInfo == NULL)
		return;
	
	/*
	 * Transition from the various states
	 */
	switch (cli->wmState) {

	case IconicState:
		/* Unmap the icon */
		IconHide(cli, iconInfo);
		break;

	case NormalState:
		/* Unmap the frame and pane */
        	XUnmapWindow(dpy, frameInfo->core.self);
        	XUnmapWindow(dpy, PANEWINOFCLIENT(cli));
		frameInfo->fcore.panewin->pcore.pendingUnmaps++;
		break;

	case InvisibleState:
		/* Already in InvisibleState */
		return;
	}

	ClientSetWMState(cli,InvisibleState);
}

/************************************************************************
 *		Top-Level Window Property Update Functions
 ************************************************************************/

/*
 * Refresh SizeHints from WM_NORMAL_HINTS property.  The new values
 * can simply be copied into the client's normHints.
 */
void
StateUpdateWMNormalHints(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	Window		pane;
	XSizeHints	sizeHints;
	Bool		preICCCM;

	if (event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);

	if (!PropGetWMNormalHints(cli->dpy,pane,&sizeHints,&preICCCM))
		return;

	*(cli->normHints) = sizeHints;
}

/*
 * Reapply WMHints from the WM_HINTS property.  Ignore everything but
 * InputHint and Icon{Pixmap/Mask}Hint.
 */
void
StateUpdateWMHints(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	Window		pane;
	XWMHints	wmHints;
	WinIconPane	*iconPane;
	Pixmap		iconMask;

	if (cli->framewin == NULL || cli->iconwin == NULL ||
	    event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);
	iconPane = (WinIconPane *)cli->iconwin->fcore.panewin;

	if (!PropGetWMHints(cli->dpy,pane,&wmHints))
		return;

	if (wmHints.flags & InputHint) {
		cli->focusMode = 
			focusModeFromHintsProtocols(&wmHints,cli->protocols);
	}

	if (wmHints.flags & IconPixmapHint)
		IconPaneSetPixmap(cli->dpy,iconPane,wmHints.icon_pixmap);
	if (wmHints.flags & IconMaskHint) 
		IconPaneSetMask(cli->dpy,iconPane,wmHints.icon_mask);
	if (wmHints.flags & IconPixmapHint || wmHints.flags & IconMaskHint) 
		WinDrawFunc(iconPane);

	if (cli->wmHints == NULL)
		cli->wmHints = MemNew(XWMHints);

	*(cli->wmHints) = wmHints;
}

/*
 * Reset client protocols and focusMode from WM_PROTOCOLS
 */
void
StateUpdateWMProtocols(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	Window		pane;
	int		protocols;

	if (cli->framewin == NULL || event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);

	if (!PropGetWMProtocols(cli->dpy,pane,&protocols))
		return;

	if (cli->protocols == protocols)
		return;

	cli->focusMode = focusModeFromHintsProtocols(cli->wmHints,protocols);
	cli->protocols = protocols;
}

/*
 * StateUpdateWinAttr - reread the _OL_WIN_ATTR property. 
 *	For now just apply WA_PINSTATE.
 */
void
StateUpdateWinAttr(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	OLWinAttr	winAttr;
	Bool		old;
	Window		pane;

	if (cli->framewin == NULL || event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);

	if (!PropGetOLWinAttr(cli->dpy,pane,&winAttr,&old))
		return;
		
	if ((winAttr.flags & WA_PINSTATE) && ClientIsPinnable(cli)) {
		WinPushPin *pushPin = (WinPushPin *)cli->framewin->winDeco;
		PushPinSetPinState(cli->dpy,pushPin,
					winAttr.pin_initial_state,False);
	}
}

/*
 * StateUpdateDecorAdd - read the DecorAdd property and reapply it
 */
void
StateUpdateDecorAdd(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	/* REMIND - this needs to be implemented */
}

/*
 * StateUpdateDecorDel - read the DecorDel property and reapply it
 */
void
StateUpdateDecorDel(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	/* REMIND - this needs to be implemented */
}
