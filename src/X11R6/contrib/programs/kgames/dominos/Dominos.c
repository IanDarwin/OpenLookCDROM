/*
 * $NCDId$
 *
 * Copyright 1992 Network Computing Devices
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of NCD. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  NCD. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * NCD. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NCD.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, Network Computing Devices
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Converters.h>
#include <stdio.h>
#include <ctype.h>
#include "DominosP.h"

#define offset(field)	XtOffsetOf(DominosRec, dominos.field)

#define COLOR_UNSET 10

static XtResource resources[] = {
    {XtNfaceColor, XtCFaceColor, XtRPixel, sizeof (Pixel),
     offset(face_pixel), XtRString, (XtPointer) "black"},
    {XtNpipsColor, XtCPipsColor, XtRPixel, sizeof (Pixel),
     offset(pips_pixel), XtRString, (XtPointer) "white"},
    {XtNroundDominos, XtCRoundDominos, XtRBoolean, sizeof (Boolean),
     offset(round_dominos), XtRImmediate, (XtPointer) True},
    {XtNsize, XtCSize, XtRDimension, sizeof (Dimension),
     offset(size), XtRImmediate, (XtPointer) 60},
    {XtNinputCallback, XtCInputCallback, XtRCallback, sizeof (XtPointer),
     offset(input_callback), XtRCallback, (XtPointer) NULL},
};

#undef offset
#undef hoffset

#include	<X11/Xmu/Drawing.h>

static char defaultTranslations[] =
"<BtnDown>: select()";

static void ActionSelect();
static XtActionsRec actions[] = {
    { "select", ActionSelect },		    /* select card */
};

#define SuperClass  ((SimpleWidgetClass)&simpleClassRec)

static void Realize ();
static void ClassInitialize (), Initialize (), Destroy (), Redisplay ();
static Boolean	SetValues ();

DominosClassRec	dominosClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) SuperClass,
    /* class_name		*/	"Dominos",
    /* widget_size		*/	sizeof(DominosRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	XtInheritResize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	defaultTranslations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
  { /* dominos fields */
    /* ignore			*/	0
  },
};

WidgetClass dominosWidgetClass = (WidgetClass) &dominosClassRec;

#define	ROUND_W(w)	((w)->dominos.size / 12)
#define	ROUND_H(w)	ROUND_W(w)
#define INSET(w)	((w)->dominos.size / 20)

#define MAX(a,b)	    ((a) < (b) ? (b) : (a))
#define PIP_SIZE(w)	    MAX (((w)->dominos.size / 10), 2)
#define PIP_OFF(w)    	    ((w)->dominos.size / 5)

#define DOMINO_MINOR_SIZE(w)   ((w)->dominos.size)
#define DOMINO_MAJOR_SIZE(w)   ((w)->dominos.size * 2)

#define DOMINO_MAJOR_WIDTH(w)  DOMINO_MAJOR_SIZE(w)
#define DOMINO_MAJOR_HEIGHT(w) DOMINO_MAJOR_SIZE(w)
#define DOMINO_MINOR_WIDTH(w)  DOMINO_MINOR_SIZE(w)
#define DOMINO_MINOR_HEIGHT(w) DOMINO_MINOR_SIZE(w)
			    
#define DominoUpright(d)    ((d)->orientation == North || \
			     (d)->orientation == South)

static int
DominoWidth (w, d)
    DominosWidget   w;
    DominoPtr	    d;
{
    if (DominoUpright(d))
	return DOMINO_MINOR_WIDTH(w);
    else
	return DOMINO_MAJOR_WIDTH(w);
}

static int
DominoHeight (w, d)
    DominosWidget   w;
    DominoPtr	    d;
{
    if (DominoUpright(d))
	return DOMINO_MAJOR_HEIGHT(w);
    else
	return DOMINO_MINOR_HEIGHT(w);
}

#define DominoX(w,d)	    (DominoWidth (w, d) / 2)
#define DominoY(w,d)	    (DominoHeight (w, d) / 2)

#define ScreenNo(w) XScreenNumberOfScreen (XtScreen (w))
			    
static void
ClassInitialize()
{
}

#define UsualSuspects(w)	Display *dpy = XtDisplay ((Widget) w); \
				Window	window = XtWindow ((Widget) w); \
				Pixmap tmp_map = (w)->dominos.tmp_map
				
#define GetScreen(w)		int screen = ScreenNo(w)
    			    
static void
GetGCs (w)
    DominosWidget	w;
{
    XGCValues	gcv;
    XtGCMask	mask;
    
    mask = GCForeground | GCGraphicsExposures;
    gcv.graphics_exposures = False;
    /* pips GC */
    gcv.foreground = w->dominos.pips_pixel;
    w->dominos.pips_gc = XtGetGC (w, mask, &gcv);
    /* face GC */
    gcv.foreground = w->dominos.face_pixel;
    w->dominos.face_gc = XtGetGC (w, mask, &gcv);
    /* bg GC */
    gcv.foreground = w->core.background_pixel;
    w->dominos.bg_gc = XtGetGC (w, mask, &gcv);
}

static void
ReleaseGCs (w)
    DominosWidget	w;
{
    XtReleaseGC (w, w->dominos.pips_gc);
    XtReleaseGC (w, w->dominos.face_gc);
    XtReleaseGC (w, w->dominos.bg_gc);
}

static void
GetTmpMap (w)
    DominosWidget	w;
{
    Display *dpy = XtDisplay(w);
    GetScreen(w);

    w->dominos.tmp_map = XCreatePixmap (dpy, RootWindow(dpy, screen),
					DOMINO_MAJOR_WIDTH(w),
					DOMINO_MAJOR_HEIGHT(w),
					w->core.depth);
}

static void
ReleaseTmpMap (w)
    DominosWidget	w;
{
    XFreePixmap (XtDisplay(w), w->dominos.tmp_map);
}

/*ARGSUSED*/
static void 
Initialize (greq, gnew)
    Widget  greq, gnew;
{
    DominosWidget	new = (DominosWidget) gnew;
    
    if (!new->core.width)
	new->core.width = DOMINO_MAJOR_WIDTH(new);
    if (!new->core.height)
	new->core.height = DOMINO_MAJOR_HEIGHT(new);
    GetGCs (new);
    GetTmpMap (new);
}

#define MotionMask ( \
	PointerMotionMask | Button1MotionMask | \
	Button2MotionMask | Button3MotionMask | Button4MotionMask | \
	Button5MotionMask | ButtonMotionMask )
#define PointerGrabMask ( \
	ButtonPressMask | ButtonReleaseMask | \
	EnterWindowMask | LeaveWindowMask | \
	PointerMotionHintMask | KeymapStateMask | \
	MotionMask )
	    
static void Realize (widget, value_mask, attributes)
    Widget		 widget;
    XtValueMask		 *value_mask;
    XSetWindowAttributes *attributes;
{
    unsigned int    event_mask = 0;
#define MAX_BUT	256
    unsigned char   mapping[MAX_BUT];
    int	    i, max;
    
    (*SuperClass->core_class.realize)(widget, value_mask, attributes);
    if (*value_mask & CWEventMask)
	event_mask = attributes->event_mask;
    event_mask &= PointerGrabMask;
    if (event_mask & ButtonPressMask)
    {
	max = XGetPointerMapping (XtDisplay (widget), mapping, MAX_BUT);
	for (i = 0; i < max; i++)
	{
	    if (mapping[i] != 0)
		XtGrabButton (widget, i, AnyModifier, True, event_mask,
			      GrabModeAsync, GrabModeAsync, None, None);
	}
    }
}

static void 
Destroy (gw)
    Widget  gw;
{
    DominosWidget	w = (DominosWidget) gw;

    ReleaseGCs (w);
    ReleaseTmpMap(w);
}

static Boolean
SetValues (gcur, greq, gnew)
    Widget  gcur, greq, gnew;
{
    DominosWidget	cur = (DominosWidget) gcur,
			req = (DominosWidget) greq,
			new = (DominosWidget) gnew;
    Boolean		ret = FALSE;
    if (cur->dominos.pips_pixel !=
	req->dominos.pips_pixel ||
	cur->dominos.face_pixel !=
	req->dominos.face_pixel ||
	cur->core.background_pixel !=
	req->core.background_pixel)
    {
	if (new->dominos.pips_gc)
	    ReleaseGCs (cur);
	GetGCs (new);
	ret = TRUE;
    }
    if (cur->dominos.size != req->dominos.size)
    {
	ReleaseTmpMap(new);
	GetTmpMap(new);
	ret = TRUE;
    }
    return ret;
}

static int
i_sqrt (a)
    int	a;
{
    extern double   sqrt ();
    double	    d_a;

    d_a = (double) a;
    d_a = sqrt (d_a);
    return (int) d_a;
}

static DominoPtr
XYInDomino (w, b, x, y, test_x, test_y, distp, dirp)
    DominosWidget   w;
    DominoPtr	    b;
    int		    x, y;
    int		    *distp;
    Direction	    *dirp;
{
    RectRec	r;
    Direction	dir;
    DominoPtr	peer, closest;
    Direction	to_dir, x_dir, y_dir;
    Direction	sub_dir;
    int		to_dist;
    int		x_dist, y_dist;
    int		sub_dist;

    r.x1 = x - DominoX(w, b);
    r.y1 = y - DominoY(w, b);
    r.x2 = r.x1 + DominoWidth(w, b);
    r.y2 = r.y1 + DominoHeight(w, b);
    to_dir = North;
    to_dist = 0;
    if (test_x < r.x1)
    {
	x_dist = r.x1 - test_x;
	x_dir = West;
    }
    else if (r.x2 < test_x)
    {
	x_dir = East;
	x_dist = test_x - r.x2;
    }
    else
    {
	x_dir = West;
	x_dist = 0;
    }
    if (test_y < r.y1)
    {
	y_dir = North;
	y_dist = r.y1 - test_y;
    }
    else if (r.y2 < test_y)
    {
	y_dir = South;
	y_dist = test_y - r.y2;
    }
    else
    {
	y_dir = South;
	y_dist = 0;
    }
    if (x_dist >= y_dist)
	to_dir = x_dir;
    else
	to_dir = y_dir;
    to_dist = i_sqrt (x_dist * x_dist + y_dist * y_dist);
    closest = b;
    if (to_dist > 0)
    {
	for (dir = North; dir <= West; dir++)
	{
	    if (b->peer[dir])
	    {
		peer = XYInDomino (w, b->peer[dir], 
				   x + PeerX(w, b, dir), y + PeerY(w, b, dir),
				   test_x, test_y, &sub_dist, &sub_dir);
		if (sub_dist < to_dist)
		{
		    to_dist = sub_dist;
		    to_dir = sub_dir;
		    closest = peer;
		    if (sub_dist == 0)
			break;
		}
	    }
	}
    }
    *distp = to_dist;
    *dirp = to_dir;
    return closest;
}

static DominoPtr
XYToDomino (w, x, y, distp, dirp)
    DominosWidget   w;
    int		    x, y;
    int		    *distp;
    Direction	    *dirp;
{
    if (w->dominos.board && *w->dominos.board)
	return XYInDomino (w, *w->dominos.board, 
			   w->dominos.x_off, w->dominos.y_off, x, y,
			   distp, dirp);
    return NULL;
}

static void
ActionSelect (gw, event, params, num_params)
    Widget  gw;
    XEvent  *event;
    String  *params;
    Cardinal	*num_params;
{
    DominosWidget   w = (DominosWidget) gw;
    DominoPtr	    d;
    DominosInputRec input;
    Direction	    dir;
    int		    dist;
    
    d = XYToDomino (w, event->xbutton.x, event->xbutton.y, &dist, &dir);
    input.w = gw;
    input.domino = d;
    input.direction = dir;
    input.distance = dist;
    input.params = params;
    input.event = *event;
    input.num_params = num_params;
    XtCallCallbackList (gw, w->dominos.input_callback, (XtPointer) &input);
}

/*ARGSUSED*/
static void
Redisplay (gw, event, region)
    Widget  gw;
    XEvent  *event;
    Region  region;
{
    DominosWidget   w = (DominosWidget) gw;

    if (w->dominos.board && *w->dominos.board)
	DrawBoard (w, *(w->dominos.board), FALSE);
}

void
DominosSetDominos (gw, boardp)
    Widget	gw;
    DominoPtr	*boardp;
{
    DominosWidget   w = (DominosWidget) gw;

    w->dominos.board = boardp;
    if (XtIsRealized (w))
	XClearWindow (XtDisplay(w), XtWindow(w));
    if (w->dominos.board && *w->dominos.board)
	DrawBoard (w, *(w->dominos.board), TRUE);
}

static void
OutlineDomino (w, d, x, y)
    DominosWidget	w;
    DominoPtr		d;
    int			x, y;
{
    Display *dpy = XtDisplay(w);
    Pixmap  tmp_map = w->dominos.tmp_map;
    int	    width, height;

    width = DominoWidth(w, d);
    height = DominoHeight(w, d);
    if (w->dominos.round_dominos)
    {
	XmuDrawRoundedRectangle (dpy, tmp_map, w->dominos.pips_gc,
				 x + INSET(w)/2, y + INSET(w)/2,
				 width - INSET(w), height - INSET(w),
				 ROUND_W(w), ROUND_H(w));
    }
    else
    {
	XDrawRectangle (dpy, tmp_map, w->dominos.pips_gc,
			x + INSET(w)/2, y + INSET(w)/2,
			width - INSET(w), height - INSET(w));
    }
    if (DominoUpright (d))
	XFillRectangle (dpy, tmp_map, w->dominos.pips_gc,
			x + INSET(w)*2, y + height/2,
			width - INSET(w)*4, 1);
    else
	XFillRectangle (dpy, tmp_map, w->dominos.pips_gc,
			x + width/2, y + INSET(w)*2,
			1, height - INSET(w)*4);
}

static void
FillDomino (w, d, x, y)
    DominosWidget   w;
    DominoPtr	    d;
    int		    x, y;
{
    Display *dpy = XtDisplay(w);
    Pixmap  tmp_map = w->dominos.tmp_map;
    int	width, height;
    
    width = DominoWidth(w, d);
    height = DominoHeight(w, d);
    if (w->dominos.round_dominos)
    {
	XmuFillRoundedRectangle(dpy, tmp_map, w->dominos.face_gc,
				x + INSET(w), y + INSET(w), 
				width - 2*INSET(w), height - 2 * INSET(w),
				ROUND_W(w), ROUND_H(w));
    }
    else
    {
	XFillRectangle(dpy, tmp_map, w->dominos.face_gc,
		       x + INSET(w), y + INSET(w), 
		       width - 2*INSET(w), height - 2*INSET(w));
    }
    OutlineDomino (w, d, x, y);
}

static int
PeerX(w, d, dir)
    DominosWidget   w;
    DominoPtr	    d;
    Direction	    dir;
{
    switch (dir) {
    case North:
    case South:
	return 0;
    case West:
	return -DominoX(w, d) - DominoWidth(w, d->peer[dir]) + DominoX(w, d->peer[dir]);
    case East:
	return -DominoX(w, d) + DominoWidth(w, d) + DominoX(w, d->peer[dir]);
    }
    /*NOTREACHED*/
}

static int
PeerY(w, d, dir)
    DominosWidget   w;
    DominoPtr	    d;
    Direction	    dir;
{
    switch (dir) {
    case East:
    case West:
	return 0;
    case North:
	return -DominoY(w, d) - DominoHeight(w, d->peer[dir]) + 
	        DominoY(w, d->peer[dir]);
    case South:
	return -DominoY(w, d) + DominoHeight(w, d) + 
	        DominoY(w, d->peer[dir]);
    }
    /*NOTREACHED*/
}

static int
BoardSize (w, b, r, x, y)
    DominosWidget   w;
    DominoPtr	    b;
    RectPtr	    r;
    int		    x, y;
{
    RectRec	sub;
    Direction	dir;

    r->x1 = x - DominoX(w, b);
    r->y1 = y - DominoY(w, b);
    r->x2 = r->x1 + DominoWidth(w, b);
    r->y2 = r->y1 + DominoHeight(w, b);
    for (dir = North; dir <= West; dir++)
    {
	if (b->peer[dir])
	{
	    BoardSize (w, b->peer[dir], &sub, 
		       x + PeerX(w, b, dir), y + PeerY(w, b, dir));
	    if (sub.x1 < r->x1)
		r->x1 = sub.x1;
	    if (sub.x2 > r->x2)
		r->x2 = sub.x2;
	    if (sub.y1 < r->y1)
		r->y1 = sub.y1;
	    if (sub.y2 > r->y2)
		r->y2 = sub.y2;
	}
    }
}
    
static void
DrawDominos (w, b, x, y)
    DominosWidget   w;
    DominoPtr	    b;
    int		    x, y;
{
    Direction	dir;

    DrawDomino (w, b, x, y);
    for (dir = North; dir <= West; dir++)
	if (b->peer[dir])
	    DrawDominos (w, b->peer[dir], 
			 x + PeerX(w, b, dir), y + PeerY(w, b, dir));
}

DrawBoard (w, b, ok_resize)
    DominosWidget   w;
    DominoPtr	    b;
    Boolean	    ok_resize;
{
    RectRec	size;
    int		xoff, yoff;
    Dimension	prefered_width, prefered_height, width, height;
    XtGeometryResult	result;

    BoardSize (w, b, &size, 0, 0);
    prefered_width = size.x2 - size.x1;
    prefered_height = size.y2 - size.y1;
    if (prefered_width < DOMINO_MAJOR_WIDTH(w))
	prefered_width = DOMINO_MAJOR_WIDTH(w);
    if (prefered_height < DOMINO_MAJOR_HEIGHT(w))
	prefered_height = DOMINO_MAJOR_HEIGHT(w);
    if (ok_resize && (XtIsRealized (w) || 
		      w->core.width < prefered_width ||
		      w->core.height < prefered_height) && 
	(prefered_width != w->core.width || 
	prefered_height != w->core.height))
    {
	result = XtMakeResizeRequest ((Widget) w,
				      prefered_width, prefered_height,
				      &width, &height);
	if (result == XtGeometryAlmost)
	    result = XtMakeResizeRequest ((Widget) w,
					  width, height, NULL, NULL);
    }
    if (XtIsRealized (w))
    {
	xoff = (w->core.width - prefered_width) / 2 - size.x1;
	yoff = (w->core.height- prefered_height) / 2 - size.y1;
	w->dominos.x_off = xoff;
	w->dominos.y_off = yoff;
	DrawDominos (w, b, xoff, yoff);
    }
}

DrawDomino(w, d, x, y)
    DominosWidget   w;
    DominoPtr	    d;
    int		    x, y;
{
    UsualSuspects(w);
    int	off_x, off_y;
    Pips    p;
    int	    flip;

    XFillRectangle (dpy, tmp_map, w->dominos.bg_gc, 0, 0,
		    32767, 32767);
    FillDomino (w, d, 0, 0);
    flip = !DominoUpright(d);
    if (d->orientation == North || d->orientation == West)
	p = d->pips[0];
    else
	p = d->pips[1];
    off_x = DOMINO_MINOR_WIDTH(w) / 2;
    off_y = DOMINO_MINOR_HEIGHT(w) / 2;
    DrawPips (w, p, off_x, off_y, flip);
    if (d->orientation == North || d->orientation == West)
	p = d->pips[1];
    else
	p = d->pips[0];
    if (!flip)
	off_y = DominoY(w, d) + DOMINO_MINOR_HEIGHT(w) / 2;
    else
	off_x = DominoX(w, d) + DOMINO_MINOR_WIDTH(w) / 2;
    DrawPips (w, p, off_x, off_y, flip);
    XCopyArea (dpy, tmp_map, window, w->dominos.pips_gc,
	       0, 0, DominoWidth(w, d), DominoHeight(w, d),
	       x - DominoX(w, d),  y - DominoY(w, d));
}

DrawPip (w, x, y)
    DominosWidget   w;
    int		    x, y;
{
    XFillArc (XtDisplay (w), w->dominos.tmp_map,
	      w->dominos.pips_gc,
	      x - PIP_SIZE(w) / 2, y - PIP_SIZE(w) / 2, PIP_SIZE(w), PIP_SIZE(w), 0,
	      64 * 360);
}

DrawPips (w, p, x, y, flip)
    DominosWidget   w;
    Pips	    p;
    int		    x, y;
    int		    flip;
{
    int	off_x, off_y;
    int	half_x, half_y;
    
    off_x = PIP_OFF(w);
    off_y = PIP_OFF(w);
    half_x = 0;
    half_y = off_y;
    if (flip)
    {
	half_x = off_x;
	half_y = 0;
	off_x = -off_x;
    }
	
    if (p & 1)
    {
	DrawPip (w, x, y);
	p = p - 1;
    }
    switch (p) {
    case 8:
	DrawPip (w, x - half_x, y - half_y);
	DrawPip (w, x + half_x, y + half_y);
	/* fall through */
    case 6:
	DrawPip (w, x - half_y, y - half_x);
	DrawPip (w, x + half_y, y + half_x);
	/* fall through */
    case 4:
	DrawPip (w, x + off_x, y - off_y);
	DrawPip (w, x - off_x, y + off_y);
	/* fall through */
    case 2:
	DrawPip (w, x - off_x, y - off_y);
	DrawPip (w, x + off_x, y + off_y);
    }
}
