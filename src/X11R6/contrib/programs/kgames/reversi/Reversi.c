#ifndef lint
static char Xrcsid[] = "$XConsortium: Reversi.c,v 1.74 89/10/21 09:41:16 swick Exp $";
#endif /* lint */


/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * Reversi.c - Reversi widget
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/XawInit.h>
#include "ReversiP.h"

/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

/* Private Data */

#define STONE_WIDTH 0.75

#define offset(field) XtOffsetOf(ReversiRec, field)

static XtResource resources[] = {
    {XtNstoneCallback, XtCStoneCallback, XtRCallback, sizeof (XtPointer),
	offset(reversi.callbacks), XtRCallback, (XtPointer) NULL},
    {XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
	offset(core.background_pixel), XtRString, "green"},
    {XtNwhite, XtCForeground, XtRPixel, sizeof(Pixel),
	offset(reversi.white), XtRString, "white"},
    {XtNblack, XtCForeground, XtRPixel, sizeof(Pixel),
	offset(reversi.black), XtRString, "black"},
    {XtNgrid, XtCForeground, XtRPixel,sizeof (Pixel),
	offset(reversi.grid), XtRString, "black"},
};

#define superclass		(&simpleClassRec)

static void Initialize();
static void Resize();
static void Redisplay();
static Boolean SetValues();
static void ClassInitialize();
static void Destroy();
static void Realize();

/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

static void ClassInitialize()
{
} /* ClassInitialize */

static void GetnormalGCs(rw)
    ReversiWidget rw;
{
    XGCValues	values;

    values.foreground	= rw->reversi.white;

    rw->reversi.white_GC = XtGetGC(
	(Widget)rw,
	(unsigned) GCForeground,
	&values);

    values.foreground = rw->reversi.black;

    rw->reversi.black_GC = XtGetGC(
	(Widget)rw,
	(unsigned) GCForeground,
	&values);

    values.foreground = rw->reversi.grid;

    rw->reversi.grid_GC = XtGetGC(
	(Widget)rw,
	(unsigned) GCForeground,
	&values);
}

static void GetGrayTile(rw)
    ReversiWidget   rw;
{
    Pixmap  tile;

    tile       = XmuCreateStippledPixmap(XtScreen((Widget)rw),
					 rw->reversi.white, 
					 rw->reversi.black,
					 rw->core.depth);
    XSetWindowBackgroundPixmap (XtDisplay (rw),
			        XtWindow (rw),
				tile);
    XFreePixmap (XtDisplay (rw), tile);
}

#define MIN_DISTINGUISH	10000.0

static Bool
DistinguishableColors (colorA, colorB)
XColor	*colorA, *colorB;
{
    double	    deltaRed, deltaGreen, deltaBlue;
    double	    dist;

    deltaRed = colorA->red - colorB->red;
    deltaGreen = colorA->green - colorB->green;
    deltaBlue = colorA->blue - colorB->blue;
    dist = deltaRed * deltaRed +
	   deltaGreen * deltaGreen +
 	   deltaBlue * deltaBlue;
    return dist > MIN_DISTINGUISH * MIN_DISTINGUISH;
}

static void
GetWidgetBackground (rw)
    ReversiWidget   rw;
{
    XColor		defs[3];
    Colormap		cmap;
    XWindowAttributes	attr;

    if (rw->core.background_pixel != rw->reversi.white &&
        rw->core.background_pixel != rw->reversi.black)
    {
    	cmap = rw->core.colormap;
    	defs[0].pixel = rw->core.background_pixel;
    	defs[1].pixel = rw->reversi.white;
    	defs[2].pixel = rw->reversi.black;
    	XQueryColors (XtDisplay (rw), cmap, defs, 3);
	if (DistinguishableColors (&defs[0], &defs[1]) &&
	    DistinguishableColors (&defs[0], &defs[2]))
	{
	    return;
	}
    }
    GetGrayTile (rw);
}

/* ARGSUSED */
static void Initialize(request, new)
    Widget request, new;
{
    ReversiWidget rw = (ReversiWidget) new;
    int		x, y;

    GetnormalGCs(rw);

    if (rw->core.width == 0)
        rw->core.width = 100;
    if (rw->core.height == 0)
        rw->core.height = 100;

    (*XtClass(new)->core_class.resize) ((Widget)rw);
    SetTransform (&rw->reversi.t,
		    0, rw->core.width,
 		    0, rw->core.height,
		    -0.5, BOARD_WIDTH - 0.5,
		    -0.5, BOARD_HEIGHT - 0.5);
    for (y = 0; y < BOARD_HEIGHT; y++)
	for (x = 0; x < BOARD_HEIGHT; x++) {
	    rw->reversi.board[x][y] = StoneNone;
	    rw->reversi.animate[x][y].state = AnimateNone;
	}
} /* Initialize */

/*
 * Repaint the widget window
 */

static void PaintEntry (rw, x, y)
    ReversiWidget   rw;
    int		    x, y;
{
    GC	gc;
    double  dx, dy, dw, dh;
    Display *dpy;
    ReversiStone    value;

    dx = (double) x - STONE_WIDTH / 2.0;
    dy = (double) y - STONE_WIDTH / 2.0;
    dw = STONE_WIDTH;
    dh = STONE_WIDTH;
    dpy = XtDisplay (rw);
    switch (rw->reversi.animate[x][y].state)
    {
    case AnimateNone:
	value = rw->reversi.board[x][y];
	break;
    case AnimateA:
	value = rw->reversi.animate[x][y].A;
	break;
    case AnimateB:
	value = rw->reversi.animate[x][y].B;
	break;
    }
    switch (value)
    {
    case StoneBlack:
	gc = rw->reversi.black_GC;
	break;
    case StoneWhite:
	gc = rw->reversi.white_GC;
	break;
    case StoneNone:
	TClearArea (dpy, XtWindow (rw), &rw->reversi.t, dx, dy, dw, dh, FALSE);
	return;
    }
    TFillArc (dpy, XtWindow (rw), gc, &rw->reversi.t, dx, dy, dw, dh, 0, 64 * 360);
}

static void
PaintGrid (rw)
    ReversiWidget   rw;
{
    int	i;
    Display	*dpy;
    Drawable	d;
    Transform	*t;
    double	v;
    GC		gc;

    dpy = XtDisplay (rw);
    d = XtWindow (rw);
    gc = rw->reversi.grid_GC;
    t = &rw->reversi.t;
    for (i = 1; i < BOARD_HEIGHT; i++)
    {
	v = (double) i - 0.5;
	TDrawLine (dpy, d, gc, t, -0.5, v, BOARD_WIDTH + 0.5, v);
    }
    for (i = 1; i < BOARD_WIDTH; i++)
    {
	v = (double) i - 0.5;
	TDrawLine (dpy, d, gc, t, v, -0.5, v, BOARD_HEIGHT + 0.5);
    }
}

/* ARGSUSED */
static void Redisplay(w, event, region)
    Widget w;
    XEvent *event;
    Region region;
{
    ReversiWidget rw = (ReversiWidget) w;
    GC gc;
    int	x, y;
    
    PaintGrid (rw);
    for (y = 0; y < BOARD_HEIGHT; y++)
	for (x = 0; x < BOARD_WIDTH; x++)
	    PaintEntry (rw, x, y);
}

static void Realize(w, value_mask, attributes)
    Widget w;
    XtValueMask *value_mask;
    XSetWindowAttributes *attributes;
{
    ReversiWidget   rw = (ReversiWidget) w;

    (*superclass->core_class.realize)(w, value_mask, attributes);
    GetWidgetBackground (rw);
}

static void Resize(w)
    Widget w;
{
    ReversiWidget	rw = (ReversiWidget) w;

    SetTransform (&rw->reversi.t,
		    0, w->core.width,
 		    0, w->core.height,
		    -0.5, BOARD_WIDTH - 0.5,
		    -0.5, BOARD_HEIGHT - 0.5);
}

/*
 * Set specified arguments into widget
 */

/* ARGSUSED */
static Boolean SetValues(current, request, new)
    Widget current, request, new;
{
}

static void Destroy(w)
    Widget w;
{
    ReversiWidget rw = (ReversiWidget)w;

    XtReleaseGC( w, rw->reversi.white_GC );
    XtReleaseGC( w, rw->reversi.black_GC);
}

XawReversiSetSpot (w, x, y, value)
    Widget	    w;
    int		    x, y;
    ReversiStone    value;
{
    ReversiWidget   rw = (ReversiWidget) w;

    if (rw->reversi.animate[x][y].state != AnimateNone)
    {
	XtRemoveTimeOut (rw->reversi.animate[x][y].timeout);
	rw->reversi.animate[x][y].state = AnimateNone;
    }

    if (rw->reversi.board[x][y] != value)
    {
	rw->reversi.board[x][y] = value;
	PaintEntry (rw, x, y);
    }
}

static void
DoAnimate (closure, interval)
    XtPointer	    closure;
    XtIntervalId    *interval;
{
    Animate *a = (Animate *) closure;

    if (a->togo == 0)
    {
	a->state = AnimateNone;
	a->timeout = 0;
    }
    else
    {
	--a->togo;
	if (a->state == AnimateA)
	    a->state = AnimateB;
	else
	    a->state = AnimateA;
	a->timeout = XtAddTimeOut (a->delay, DoAnimate, (XtPointer) a);
    }
    PaintEntry (a->rw, a->x, a->y);
}

XawReversiAnimateSpot (w, x, y, A, B, delay, repeat)
    Widget	    w;
    int		    x, y;
    ReversiStone    A, B;
    unsigned long   delay;
    int		    repeat;
{
    ReversiWidget   rw = (ReversiWidget) w;
    Animate	    *a;
    XtIntervalId    timeout;

    a = &rw->reversi.animate[x][y];
    if (a->state != AnimateNone)
    {
	XtRemoveTimeOut (a->timeout);
    }
    a->state = AnimateNone;
    if (repeat == 0)
    {
	PaintEntry (rw, x, y);
	return;
    }
    a->delay = delay;
    a->togo = repeat - 1;
    a->x = x;
    a->y = y;
    a->rw = rw;
    a->A = A;
    a->B = B;
    a->state = AnimateA;
    if (!(a->timeout = XtAddTimeOut (delay, DoAnimate, (XtPointer) a)))
	a->state = AnimateNone;
    PaintEntry (rw, x, y);
}

static void
Select (w, event, params, num_params)
    Widget	w;
    XEvent	*event;
    String *params;		/* unused */
    Cardinal *num_params;	/* unused */
{
    ReversiWidget   rw = (ReversiWidget) w;
    int	    x, y;
    double  dx, dy;
    int	    rx, ry;
    ReversiMove	move;

    switch (event->type)
    {
    case ButtonPress:
    case ButtonRelease:
	x = event->xbutton.x;
	y = event->xbutton.y;
	break;
    case MotionNotify:
	x = event->xmotion.x;
	y = event->xmotion.y;
	break;
    default:
	return;
    }
    dx = Tx (x, y, &rw->reversi.t);
    dy = Ty (x, y, &rw->reversi.t);
    move.x = rx = dx + 0.5;
    move.y = ry = dy + 0.5;
    if (0 <= rx && rx < BOARD_WIDTH && 0 <= ry && ry < BOARD_HEIGHT)
	XtCallCallbackList ((Widget) rw, rw->reversi.callbacks, (XtPointer) &move);
}

static XtActionsRec actions[] =
{
    {"select",	    Select},
};

static char defaultTranslations[] =
    "<BtnDown>:	select()";

ReversiClassRec reversiClassRec = {
  {
/* core_class fields */	
    /* superclass	  	*/	(WidgetClass) superclass,
    /* class_name	  	*/	"Reversi",
    /* widget_size	  	*/	sizeof(ReversiRec),
    /* class_initialize   	*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	Realize,
    /* actions		  	*/	actions,
    /* num_actions	  	*/	XtNumber (actions),
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	Destroy,
    /* resize		  	*/	Resize,
    /* expose		  	*/	Redisplay,
    /* set_values	  	*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	defaultTranslations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  }, {
/* simple class */
    /* change_sensitive		*/	XtInheritChangeSensitive,
  }
};

WidgetClass reversiWidgetClass = (WidgetClass)&reversiClassRec;
