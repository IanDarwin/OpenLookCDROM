#ident "@(#)dsdm.c	1.5	93/06/28"

/*
 *	(c) Copyright 1992 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

/*
 * DSDM - Drop Site Database Manager for drag'n'drop.
 *
 * Master algorithm:
 *
 * Start with visible region as whole screen.
 * For each top-level window, do
 * (0) flatten its interest rectangles
 * (1) intersect interest rects with the top-level window
 * (2) intersect them with the visible region
 * (3) append them to the master list
 * (4) subtract this top-level frame from the visible region
 */

#include <stdio.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "properties.h"
#include "win.h"
#include "error.h"
#include "dsdm.h"

/*
 * Round-trip metering.  Meter the XGetGeometry, XGetWindowAttributes, 
 * XGetWindowProperty, XQueryTree, and XTranslateCoordinates calls.  Calls to 
 * them in the code must be coded as _XWhatever((arg, arg, ...)).  To enable, 
 * just add #define RTMETER.
 */

#ifdef RTMETER

int _rt_count;
#define INC_RT_COUNT ++_rt_count ,

#else

#define INC_RT_COUNT

#endif /* RTMETER */


#define _XGetGeometry(args) (\
	 INC_RT_COUNT \
	 XGetGeometry args)

#define _XGetWindowAttributes(args) (\
	 INC_RT_COUNT \
	 XGetWindowAttributes args)

#define _XGetWindowProperty(args) (\
	 INC_RT_COUNT \
         XGetWindowProperty args)

#define _XQueryTree(args) (\
	 INC_RT_COUNT \
	 XQueryTree args)

#define _XTranslateCoordinates(args) (\
	 INC_RT_COUNT \
	 XTranslateCoordinates args)


/* ===== externs ========================================================== */

extern Atom AtomAtomPair;
extern Atom AtomMultiple;
extern Atom AtomSunDragDropDSDM;
extern Atom AtomSunDragDropInterest;
extern Atom AtomSunDragDropSiteRects;
extern Atom AtomTargets;
extern Atom AtomTimestamp;
extern Atom AtomWMState;

extern Window NoFocusWin;

/* ===== private data ===================================================== */

typedef struct _site {
    int screen;
    unsigned long site_id;
    Window window_id;
    unsigned long flags;
    Region region;
    struct _site *next;
} site_t;

static site_t *MasterSiteList = NULL;
static site_t **NextSite = &MasterSiteList;
static int SitesFound = 0;
static Time selectionTime;
static Bool selectionOwned = False;

/* ===== regions ========================================================== */

/*
 * This region stuff is stolen from region.h.  It is dependent upon the 
 * current Xlib implementation of regions.
 */

typedef struct _box {
    short x1, x2, y1, y2;
} BOX;

typedef struct {
    long size;
    long numRects;
    BOX *rects;
    BOX extents;
} REGION;


#define REGION_NUMRECTS(r) (((REGION *)(r))->numRects)


/* ===== private functions ================================================ */


/*
 * Get the interest property from this window.  If a valid interest property
 * was found, a pointer to the data is returned.  This data must be freed with
 * XFree().  If no valid property is found, NULL is returned.
 */
static void *
GetInterestProperty(dpy, win, nitems)
    Display *dpy;
    Window win;
    unsigned long *nitems;
{
    Status s;
    Atom acttype;
    unsigned long remain;
    int actfmt;
    void *data;

    s = _XGetWindowProperty((dpy, win, AtomSunDragDropInterest, 0L,
			     ENTIRE_CONTENTS, False, AtomSunDragDropInterest,
			     &acttype, &actfmt, nitems, &remain,
			     (unsigned char **) &data));

    if (s != Success)
	return NULL;

    if (acttype == None)
	/* property does not exist */
	return NULL;

    if (acttype != AtomSunDragDropInterest) {
#ifdef DEBUG
	ErrorWarning("dnd interest property has wrong type");
#endif /* DEBUG */
	return NULL;
    }

    if (actfmt != 32) {
#ifdef DEBUG
	ErrorWarning("dnd interest property has wrong format");
#endif /* DEBUG */
	XFree(data);
	return NULL;
    }

    if (remain > 0) {
#ifdef DEBUG
	/*
	 * We didn't read it all, just punt.  REMIND: we should loop around
	 * GetWindowProperty until we have all the data.
	 */
	fputs("dsdm: interest property too long\n", stderr);
#endif /* DEBUG */
	XFree(data);
	return NULL;
    }
    return data;
}


/*
 * Create and return a region that contains a given rectangle.
 */
static Region
MakeRegionFromRect(x, y, w, h)
    int x, y;
    unsigned int w, h;
{
    XRectangle r;
    Region reg;

    r.x = x;
    r.y = y;
    r.width = w;
    r.height = h;
    reg = XCreateRegion();
    XUnionRectWithRegion(&r, reg, reg);
    return reg;
}


/*
 * Create and return a region that contains the geometry of the window.
 * The region returned must be destroyed with XDestroyRegion().  The offset 
 * parameter indicates whether the window's geometry should be offset by its
 * (x,y) location w.r.t. its parent.  If it is false, the region's upper left 
 * corner is at (0,0).
 */
static Region
GetWindowRegion(dpy, winInfo, offset)
    Display *dpy;
    WinGeneric *winInfo;
    Bool offset;
{
    return MakeRegionFromRect(
	offset ? winInfo->core.x : 0,
	offset ? winInfo->core.y : 0,
	winInfo->core.width,
	winInfo->core.height
    );
}    


/*
 * Subtract the area of a window from the current visible region.
 */
static void
SubtractWindowFromVisibleRegion(dpy, winInfo, visrgn)
    Display *dpy;
    Window winInfo;
    Region visrgn;
{
    Region winrgn = GetWindowRegion(dpy, winInfo, True);
    XSubtractRegion(visrgn, winrgn, visrgn);
    XDestroyRegion(winrgn);
}


/*
 * NEXTWORD - a macro to step through the property data.  This macro depends
 * on local variables of ProcessInterestProperty().
 */

#define NEXTWORD(dest) do {						\
	    if (++cur >= datalen) {					\
		if (region != NULL)					\
		    XDestroyRegion(region);				\
		if (toprgn != NULL)					\
		    XDestroyRegion(toprgn);				\
		return;							\
	    }								\
	    (dest) = array[cur];					\
	} while (0)


/*
 * Process a window's drop site interest property.  If the property is in an 
 * invalid format, fail silently.
 */
static void
ProcessInterestProperty(dpy, winInfo, screen, data, datalen, visrgn,
			xoff, yoff)
    Display *dpy;
    WinGeneric *winInfo;
    int screen;
    void *data;
    unsigned long datalen;
    Region visrgn;
    int xoff, yoff;
{
    unsigned long *array = data;
    int cur = 0;
    int i, j, nsites;
    Window wid;
    Window wjunk;
    Window areawin;
    unsigned long sid;
    int areatype;
    int nrects;
    unsigned long flags;
    Region region = NULL;
    Region toprgn = NULL;
    XRectangle rect;
    site_t *site;
    int x, y;
    unsigned int width, height, border, ujunk;
    int junk;

    if (array[cur] != DND_VERSION) {
#ifdef DEBUG
	char msg[100];
	sprintf(msg,
		"unknown drop interest property version (%d) on 0x%x\n",
		array[cur], winInfo->core.self);
	ErrorWarning(msg);
#endif /* DEBUG */
	return;
    }

    toprgn = GetWindowRegion(dpy, winInfo, False);

    NEXTWORD(nsites);
    for (i=0; i<nsites; ++i) {
	NEXTWORD(wid);
	NEXTWORD(sid);
	NEXTWORD(flags);
	NEXTWORD(areatype);
	switch (areatype) {
	case DND_INTEREST_RECT:
	    region = XCreateRegion();
	    NEXTWORD(nrects);
	    for (j=0; j<nrects; ++j) {
		NEXTWORD(rect.x);
		NEXTWORD(rect.y);
		NEXTWORD(rect.width);
		NEXTWORD(rect.height);
		XUnionRectWithRegion(&rect, region, region);
	    }
	    break;
	case DND_INTEREST_WINDOW:
	    region = XCreateRegion();
	    NEXTWORD(nrects);
	    for (j=0; j<nrects; ++j) {
		NEXTWORD(areawin);
		if (0 == _XGetGeometry((dpy, areawin, &wjunk, &junk, &junk,
					&width, &height, &border, &ujunk)))
		{
#ifdef DEBUG
		    char msg[100];
		    sprintf(msg,
			    "XGetGeometry failed on window 0x%x\n",
			    winInfo->core.self);
		    ErrorWarning(msg);
#endif /* DEBUG */
		    continue;
		}
		(void) _XTranslateCoordinates((dpy, areawin,
					       winInfo->core.self, 0, 0,
					       &x, &y, &wjunk));
		rect.x = x - border;
		rect.y = y - border;
		rect.width = width + border;
		rect.height = height + border;
		XUnionRectWithRegion(&rect, region, region);
	    }
	    break;
	default:
#ifdef DEBUG
	    {
		char msg[100];
		sprintf(msg,
			"unknown site area type on window 0x%x\n",
			winInfo->core.self);
		ErrorWarning(msg);
	    }
#endif /* DEBUG */
	    XDestroyRegion(toprgn);
	    return;
	}
	XIntersectRegion(region, toprgn, region);
	XOffsetRegion(region, xoff, yoff);
	XIntersectRegion(region, visrgn, region);
	site = (site_t *) malloc(sizeof(site_t));
	if (site == NULL)
	    break; /* out of memory - stop processing this property */
	site->screen = screen;
	site->site_id = sid;
	site->window_id = wid;
	site->flags = flags;
	site->region = region;
	site->next = NULL;
	(*NextSite) = site;
	NextSite = &site->next;
	++SitesFound;
	region = NULL;
    }
    XDestroyRegion(toprgn);
}


/*
 * FindDropSites
 *
 * For the root window of each screen, get the list of children.  For each 
 * child, get its drop forwarding information and find the top-level window 
 * underneath that child, and get the top-level window's drop site 
 * information.  Add the top-level window's site information and the site 
 * forwarding information to the site database.
 */
static void
FindDropSites(dpy)
    Display *dpy;
{
    Window junk, root, *children;
    int s, i, nchildren;
    Region visrgn, framergn, toprgn;
    void *sitedata;
    void *fwdsitedata;
    int xoff, yoff;
    WinRoot *rootInfo;
    WinGeneric *winInfo;
    WinPane *paneInfo;
    int state;
    unsigned long datalen;
    unsigned long fwdlen;

#ifdef RTMETER
    _rt_count = 0;
#endif

    for (s=0; s<ScreenCount(dpy); ++s) {

	/* Find the virtual root here, if necessary. */
	root = RootWindow(dpy, s);
	rootInfo = (WinRoot *) WIGetInfo(root);
	if (rootInfo == NULL || rootInfo->core.kind != WIN_ROOT)
	    continue;

	visrgn = GetWindowRegion(dpy, rootInfo, False);

	if (_XQueryTree((dpy, root, &junk, &junk, &children,
			 (unsigned int *) &nchildren)) == 0)
	{
#ifdef DEBUG
	    char msg[100];
	    (void) sprintf(msg, "XQueryTree failed on root window 0x%x", root);
	    ErrorWarning(msg);
#endif /* DEBUG */
	    continue;
	}

	/*
	 * Run through the children of root in top-to-bottom order.  For 
	 * frames, get the pane's drop interest and any forwarding interest.  
	 * For icons, get the icon's forwarding interest.  Add any site data 
	 * found to the master site list.
	 */

	for (i = nchildren-1; i >= 0; --i) {

	    winInfo = WIGetInfo(children[i]);
	    if (winInfo == NULL)
		continue;

	    state = winInfo->core.client->wmState;

	    /*
	     * Ignore everything except frames in NormalState and icons in 
	     * IconicState.
	     */
	    if (!((winInfo->core.kind == WIN_FRAME && state == NormalState) ||
		  (winInfo->core.kind == WIN_ICON && state == IconicState)))
	    {
		continue;
	    }

	    fwdsitedata = GetInterestProperty(dpy, children[i], &fwdlen);

	    if (winInfo->core.kind == WIN_FRAME) {
		paneInfo = (WinPane *) PANEOFCLIENT(winInfo->core.client);
		sitedata = GetInterestProperty(dpy, paneInfo->core.self,
		    &datalen);
		WinRootPos(paneInfo, &xoff, &yoff);
		if (sitedata != NULL) {
		    ProcessInterestProperty(dpy, paneInfo, s, sitedata,
					    datalen, visrgn, xoff, yoff);
		    XFree(sitedata);

		    if (fwdsitedata != NULL) {
			framergn = GetWindowRegion(dpy, winInfo, True);
			XIntersectRegion(framergn, visrgn, framergn);
			toprgn = GetWindowRegion(dpy, paneInfo, False);
			XOffsetRegion(toprgn, xoff, yoff);
			XSubtractRegion(framergn, toprgn, framergn);
			ProcessInterestProperty(dpy, winInfo, s, fwdsitedata,
						fwdlen, framergn,
						winInfo->core.x,
						winInfo->core.y);
			XDestroyRegion(framergn);
			XDestroyRegion(toprgn);
			XFree(fwdsitedata);
		    }
		}
	    } else {
		if (fwdsitedata != NULL) {
		    ProcessInterestProperty(dpy, winInfo, s, fwdsitedata,
					    fwdlen, visrgn,
					    winInfo->core.x,
					    winInfo->core.y);
		    XFree(fwdsitedata);
		}
	    }

	    SubtractWindowFromVisibleRegion(dpy, winInfo, visrgn);
	}
	XDestroyRegion(visrgn);
	XFree((char *) children);
    }

#ifdef RTMETER
    printf("roundtrips = %d\n", _rt_count);
#endif
}


/*
 * FreeDropSites
 *
 * Free the memory associated with the list of drop sites.
 */
static void
FreeDropSites()
{
    site_t *next, *temp;

    next = MasterSiteList;
    while (next != NULL) {
	XDestroyRegion(next->region);
	temp = next->next;
	free(next);
	next = temp;
    }
    MasterSiteList = NULL;
    SitesFound = 0;
    NextSite = &MasterSiteList;
}


/*
 * WriteSiteRectList
 * 
 * Write a property containing site rectangle information.  The format 
 * consists of zero or more blocks of 8 words, as follows:
 *	8k+0	screen number
 *	8k+1	site id
 *	8k+2	window id
 *	8k+3	x
 *	8k+4	y
 *	8k+5	width
 *	8k+6	height
 *	8k+7	flags
 */
static void
WriteSiteRectList(dpy, win, prop)
    Display *dpy;
    Window win;
    Atom prop;
{
    unsigned long *cur;
    unsigned long *array;
    site_t *site;
    int numrects = 0;
    REGION *region;
    BOX *box, *last;

    site = MasterSiteList;
    while (site != NULL) {
	numrects += REGION_NUMRECTS(site->region);
	site = site->next;
    }

    array = (unsigned long *) malloc(8 * MAX(numrects,1) * sizeof(int));
	/* MAX is to prevent malloc(0) */

    cur = array;
    site = MasterSiteList;
    while (site != NULL) {
	region = (REGION *) site->region;
	box = region->rects;
	last = box + region->numRects;
	for ( ; box < last ; ++box) {
	    *cur++ = site->screen;
	    *cur++ = site->site_id;
	    *cur++ = site->window_id;
	    *cur++ = box->x1;
	    *cur++ = box->y1;
	    *cur++ = box->x2 - box->x1;
	    *cur++ = box->y2 - box->y1;
	    *cur++ = site->flags;
	}
	site = site->next;
    }

    XChangeProperty(dpy, win, prop, XA_INTEGER, 32, PropModeReplace,
		    (unsigned char *)array, cur - array);
    free(array);
}


/*
 * convertTarget
 *
 * Do the actual work to convert a single DSDM target.
 */
static Bool
convertTarget(dpy, requestor, target, property)
    Display *dpy;
    Window requestor;
    Atom target;
    Atom property;
{
    unsigned long data[10];	/* long enough for most return values */
    unsigned char *propdata = (unsigned char *) data;
    int format, nelements;
    Atom type;

    if (target == AtomTargets) {
	data[0] = AtomTargets;
	data[1] = AtomTimestamp;
	data[2] = AtomMultiple;
	data[3] = AtomSunDragDropSiteRects;
	nelements = 4;
	type = XA_ATOM;
	format = 32;
    } else if (target == AtomTimestamp) {
	data[0] = selectionTime;
	nelements = 1;
	type = XA_INTEGER;
	format = 32;
    } else if (target == AtomSunDragDropSiteRects) {
	FindDropSites(dpy);
	WriteSiteRectList(dpy, requestor, property);
	FreeDropSites();
	return True;
    } else {
	return False;
    }

    /* write the property, free it if necessary, and return success */

    XChangeProperty(dpy, requestor, property, type, format, PropModeReplace,
		    (unsigned char *)propdata, nelements);
    if (propdata != (unsigned char *) data)
	MemFree(propdata);
    return True;
}


/*
 * handleDSDMrequest
 *
 * Respond to a SelectionRequest or SelectionClear event on the DSDM 
 * selection.
 */
static void
handleDSDMrequest(event)
    XEvent *event;
{
    XSelectionEvent reply;
    XSelectionRequestEvent *request;
    Atom *pairs;
    int i;
    Bool writeback = False;
    unsigned long nitems, remain;

    switch (event->type) {
    case SelectionClear:
	selectionOwned = False;
	return;
    case SelectionRequest:
	/* processing proceeds below */
	break;
    }

    request = (XSelectionRequestEvent *) event;

    /*
     * Set up a reply event for refusal.  If a conversion is successful, the 
     * property field will be filled in appropriately.
     */

    reply.type = SelectionNotify;
    reply.requestor = request->requestor;
    reply.selection = AtomSunDragDropDSDM;
    reply.property = None;
    reply.target = request->target;
    reply.time = request->time;

    if (selectionOwned) {
	if (request->target == AtomMultiple) {
	    if (request->property != None) {
		pairs = GetWindowProperty(request->display, request->requestor,
					  request->property, 0L, 100000L,
					  AtomAtomPair, 32, &nitems, &remain);
		if (pairs != NULL) {
		    /*
		     * Process each pair of atoms (target, property).  Watch
		     * out for an odd last atom, and for property atoms of
		     * None.  If the conversion fails, replace it with None in
		     * the original property.
		     */
		    for (i = 0; i+1 < nitems; i += 2) {
			if (pairs[i+1] == None)
			    continue;

			if (!convertTarget(request->display,
			    request->requestor, pairs[i], pairs[i+1]))
			{
			    pairs[i+1] = None;
			    writeback = True;
			}
		    }
		    if (writeback)
			XChangeProperty(request->display, request->requestor,
			    request->property, AtomAtomPair, 32,
			    PropModeReplace, (unsigned char *) pairs, nitems);

		    XFree((char *) pairs);
		    reply.property = request->property;
		}
	    }
	} else {
	    /* backwards compatibility per ICCCM section 2.2 */
	    if (request->property == None)
		request->property = request->target;

	    if (convertTarget(request->display, request->requestor,
			      request->target, request->property))
	    {
		reply.property = request->property;
	    }
	}
    }

    (void) XSendEvent(event->xany.display, reply.requestor, False,
		      NoEventMask, (XEvent *) &reply);
}


/* ===== public functions ================================================= */


/*
 * DragDropStartDSDM
 *
 * Start performing the DSDM function by acquiring the DSDM selection.
 * Grabbing the server guarantees that we'll get the selection.
 */
void
DragDropStartDSDM(dpy)
    Display *dpy;
{
    XGrabServer(dpy);
    selectionTime = TimeFresh();
    XSetSelectionOwner(dpy, AtomSunDragDropDSDM, NoFocusWin, selectionTime);
    XUngrabServer(dpy);
    selectionOwned = True;
}


/*
 * DragDropStopDSDM
 *
 * Stop performing the DSDM function by relinquishing ownership of the DSDM 
 * selection.  Note that this will generate a SelectionClear event.
 */
void
DragDropStopDSDM(dpy)
    Display *dpy;
{
    XSetSelectionOwner(dpy, AtomSunDragDropDSDM, None, selectionTime);
}


/*
 * DragDropInit
 *
 * Register the selection handler for the DSDM selection.  Should be called 
 * exactly once at startup.
 */
void
DragDropInit()
{
    SelectionRegister(AtomSunDragDropDSDM, handleDSDMrequest);
}
