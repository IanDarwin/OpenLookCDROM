/**
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:	Greg Renda <greg@ncd.com>
 * 		Network Computing Devices, Inc.
 * 		350 North Bernardo Ave.
 * 		Mountain View, CA  94043
 *
 * $NCDId: @(#)Graph.c,v 1.10 1994/04/07 18:19:00 greg Exp $
 */

#include <values.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "GraphP.h"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))
#define abs(a) ((a) < 0 ? -(a) : (a))

#define offset(field) XtOffsetOf(GraphRec, graph.field)
/* {name, class, type, size, offset, default_type, default_addr}, */
static XtResource resources[] =
{
    XtNgraphColor, XtCColor, XtRPixel, sizeof(Pixel), offset(graphColor),
    XtRString, XtDefaultForeground,

    XtNmarkerColor, XtCColor, XtRPixel, sizeof(Pixel), offset(markerColor),
    XtRString, "red",

    XtNpositionColor, XtCColor, XtRPixel, sizeof(Pixel), offset(positionColor),
    XtRString, "blue",

    XtNdata, XtCData, XtRPointer, sizeof(int *), offset(data), XtRImmediate,
    NULL,

    XtNnumSamples, XtCNumSamples, XtRInt, sizeof(int), offset(numSamples),
    XtRImmediate, 0,

    XtNnumTracks, XtCNumTracks, XtRInt, sizeof(int), offset(numTracks),
    XtRImmediate, (XtPointer) 1,

    XtNstart, XtCStart, XtRInt, sizeof(int), offset(start), XtRImmediate, 0,

    XtNend, XtCEnd, XtRInt, sizeof(int), offset(end), XtRImmediate, 0,

    XtNleftMarker, XtCLeftMarker, XtRInt, sizeof(int), offset(leftMarker),
    XtRImmediate, 0,

    XtNrightMarker, XtCRightMarker, XtRInt, sizeof(int), offset(rightMarker),
    XtRImmediate, 0,

    XtNposition, XtCPosition, XtRInt, sizeof(int), offset(position),
    XtRImmediate, 0,

    XtNleftProc, XtCCallback, XtRCallback, sizeof(XtPointer), offset(leftProc),
    XtRCallback, NULL,

    XtNrightProc, XtCCallback, XtRCallback, sizeof(XtPointer),
    offset(rightProc), XtRCallback, NULL,
};
#undef offset

static void     selectMarker(), moveMarker();

static XtActionsRec actions[] =
{
    /* {name, procedure}, */
    {"selectMarker", selectMarker},
    {"moveMarker", moveMarker},
};

static char     translations[] =
"\
<BtnDown>:      selectMarker()\n\
<BtnMotion>:    moveMarker()\n\
";

static void     Initialize(), Exposure(), Destroy(), Resize();
static Boolean  SetValues(), redrawPending;

GraphClassRec   graphClassRec =
{
    {						/* core fields */
	(WidgetClass) & widgetClassRec,		/* superclass */
	"Graph",				/* class_name */
	sizeof(GraphRec),			/* widget_size */
	NULL,					/* class_initialize */
	NULL,					/* class_part_initialize */
	FALSE,					/* class_inited */
	Initialize,				/* initialize */
	NULL,					/* initialize_hook */
	XtInheritRealize,			/* realize */
	actions,				/* actions */
	XtNumber(actions),			/* num_actions */
	resources,				/* resources */
	XtNumber(resources),			/* num_resources */
	NULLQUARK,				/* xrm_class */
	TRUE,					/* compress_motion */
	TRUE,					/* compress_exposure */
	TRUE,					/* compress_enterleave */
	FALSE,					/* visible_interest */
	Destroy,				/* destroy */
	Resize,					/* resize */
	Exposure,				/* expose */
	SetValues,				/* set_values */
	NULL,					/* set_values_hook */
	XtInheritSetValuesAlmost,		/* set_values_almost */
	NULL,					/* get_values_hook */
	NULL,					/* accept_focus */
	XtVersion,				/* version */
	NULL,					/* callback_private */
	translations,				/* tm_table */
	XtInheritQueryGeometry,			/* query_geometry */
	XtInheritDisplayAccelerator,		/* display_accelerator */
	NULL,					/* extension */
    },
    {						/* graph fields */
	0					/* empty */
    }
};

WidgetClass     graphWidgetClass = (WidgetClass) & graphClassRec;

static void
CreateGC(w, which)
GraphWidget     w;
unsigned int    which;
{
    XGCValues       gcv;
#ifndef __alpha
    unsigned long   commonMask;
#else /* __alpha */
    unsigned int  commonMask;
#endif /* __alpha */

    gcv.graphics_exposures = FALSE;
    commonMask = GCGraphicsExposures;

    if (which & GRAPH_GC)
    {
	gcv.foreground = w->graph.graphColor;
	w->graph.graphGC = XtGetGC((Widget) w, commonMask | GCForeground, &gcv);
    }

    if (which & POSITION_GC)
    {
	gcv.foreground = w->graph.positionColor ^ w->core.background_pixel;
	gcv.function = GXxor;
	gcv.line_style = LineOnOffDash;
	w->graph.positionGC = XtGetGC((Widget) w,
				   commonMask | GCForeground | GCLineStyle |
				      GCFunction, &gcv);
    }

    if (which & MARKER_GC)
    {
	gcv.foreground = w->graph.markerColor ^ w->core.background_pixel;
	gcv.function = GXxor;
	w->graph.markerGC = XtGetGC((Widget) w,
				    commonMask | GCForeground | GCFunction,
				    &gcv);
    }
}

static void
DestroyGC(w, which)
GraphWidget     w;
unsigned int    which;
{
    if (which & GRAPH_GC)
	XtReleaseGC((Widget) w, w->graph.graphGC);

    if (which & POSITION_GC)
	XtReleaseGC((Widget) w, w->graph.positionGC);

    if (which & MARKER_GC)
	XtReleaseGC((Widget) w, w->graph.markerGC);
}

/* ARGSUSED */
static void
Initialize(req, new)
Widget          req,
                new;
{
    CreateGC(new, (unsigned int) ALL_GCS);
    redrawPending = FALSE;
}

static void
Destroy(w)
Widget          w;
{
    DestroyGC(w, (unsigned int) ALL_GCS);
}

static void
drawPosition(w)
GraphWidget     w;
{
    int             x;

    if (w->graph.position >= w->graph.end)
	w->graph.position = w->graph.end - 1;

    if (w->graph.position < w->graph.start)
	w->graph.position = w->graph.start;

    x = (w->graph.position - w->graph.start) * w->graph.hscale;

    XDrawLine(XtDisplay(w), XtWindow(w), w->graph.positionGC, x, 0, x,
	      w->core.height - 1);
}

static void
drawMarker(w, m)
GraphWidget     w;
int             m;
{
    int            *x,
                    marker;

    if (m == GraphLeftMarker)
    {
	marker = w->graph.leftMarker;

	if (marker >= w->graph.rightMarker)
	    marker = w->graph.rightMarker - 1;

	if (marker < w->graph.start)
	    marker = w->graph.start;

	w->graph.leftMarker = marker;
	x = &w->graph.leftMarkerX;
    }
    else
    {				/* right marker */
	marker = w->graph.rightMarker;

	if (marker <= w->graph.leftMarker)
	    marker = w->graph.leftMarker + 1;

	if (marker > w->graph.end)
	    marker = w->graph.end;

	w->graph.rightMarker = marker;
	x = &w->graph.rightMarkerX;
    }

    *x = (marker - w->graph.start) * w->graph.hscale;

    if (w->graph.leftMarkerX == w->graph.rightMarkerX)
	*x += m == GraphLeftMarker ? -1 : 1;

    XDrawLine(XtDisplay(w), XtWindow(w), w->graph.markerGC, *x, 0, *x,
	      w->core.height - 1);
}

static void
paintWindow(w)
GraphWidget     w;
{
    Display        *dpy = XtDisplay(w);
    Window          win = XtWindow(w);
    int             x,
                    t,
                    center;
    float           k;
    GraphDataType  *p,
                   *end;

    redrawPending = FALSE;
    XClearWindow(dpy, win);

    if (w->graph.numSamples)
	for (t = 0; t < w->graph.numTracks; t++)
	{
	    center = (int) w->core.height / (int) w->graph.numTracks / 2 *
		((t + 1) * 2 - 1);

	    /* draw center line */
	    XDrawLine(dpy, win, w->graph.graphGC, 0, center, w->core.width,
		      center);

	    /* draw the data */
	    p = w->graph.data + (w->graph.start * w->graph.numTracks) + t;
	    end = w->graph.data + (w->graph.end * w->graph.numTracks);
	    k = w->graph.hscale;

	    if (k < 1)
	    {			/* multiple samples per pixel */
		GraphDataType   minY,
		                maxY,
		                pminY,
		                pmaxY,
		                v;

		pminY = pmaxY = 0;

		for (x = 0; x < (int) w->core.width; x++)
		{
		    minY = MAXSHORT;
		    maxY = -MAXSHORT;

		    for (; (int) k == x && p < end; k += w->graph.hscale)
		    {
			v = *p;
			p += w->graph.numTracks;

			minY = min(v, minY);
			maxY = max(v, maxY);
		    }

		    minY = min(pmaxY, minY);
		    maxY = max(pminY, maxY);

		    pminY = minY;
		    pmaxY = maxY;

		    XDrawLine(dpy, win, w->graph.graphGC,
			      x, (int) (-maxY * w->graph.vscale + center),
			      x, (int) (-minY * w->graph.vscale + center));
		}
	    }
	    else
	    {			/* multiple pixels per sample */
		int             px,
		                py,
		                y;

		px = 0;
		py = -*p * w->graph.vscale + center;
		p += w->graph.numTracks;
		x = k;
		k += w->graph.hscale;

		for (; p < end; k += w->graph.hscale, x = k)
		{
		    y = -*p * w->graph.vscale + center;
		    p += w->graph.numTracks;
		    XDrawLine(dpy, win, w->graph.graphGC, px, py, x, y);
		    px = x;
		    py = y;
		}
	    }
	}

    /* draw markers */
    w->graph.rightMarkerX = w->core.width;

    drawMarker(w, GraphLeftMarker);
    drawMarker(w, GraphRightMarker);
    drawPosition(w);
}

static void
Exposure(w, event, region)
Widget          w;
XEvent         *event;
Region          region;
{
    if (XtIsRealized((Widget) w))
	paintWindow(w);
}

static void
recalc(w)
GraphWidget     w;
{
    w->graph.vscale = (float) w->core.height / w->graph.numTracks /
	(MAXSHORT - -MAXSHORT + 1);

    w->graph.hscale = (float) ((int) w->core.width - 1) /
	(w->graph.end - w->graph.start);
}

static void
validate(g)
GraphPart      *g;
{
    if (g->start < 0)
	g->start = 0;
    else
	g->start = min(g->start, g->numSamples - 1);

    if (g->end <= g->start)
	g->end = g->start + 1;
    else
	g->end = min(g->end, g->numSamples);

    if (g->leftMarker >= g->rightMarker)
	g->leftMarker = g->rightMarker - 1;

    if (g->leftMarker < g->start)
	g->leftMarker = g->start;

    if (g->rightMarker <= g->leftMarker)
	g->rightMarker = g->leftMarker + 1;

    if (g->rightMarker > g->end)
	g->rightMarker = g->end;
}

/* ARGSUSED */
static          Boolean
SetValues(old, req, w)
GraphWidget     old,
                req,
                w;
{
    Boolean         newGC = NO_GCS,
                    redraw = FALSE;

    validate(&w->graph);

    if (w->graph.leftMarker != old->graph.leftMarker)
    {
	XtCallCallbacks((Widget) w, XtNleftProc,
			(XtPointer) w->graph.leftMarker);
	redraw = TRUE;
    }

    if (w->graph.rightMarker != old->graph.rightMarker)
    {
	XtCallCallbacks((Widget) w, XtNrightProc,
			(XtPointer) w->graph.rightMarker);
	redraw = TRUE;
    }

    if (w->graph.position != old->graph.position ||
	w->graph.data != old->graph.data)
	redraw = TRUE;

    if (w->graph.numSamples != old->graph.numSamples ||
	w->graph.start != old->graph.start ||
	w->graph.end != old->graph.end)
    {
	recalc(w);
	redraw = TRUE;
    }

    if (w->graph.graphColor != old->graph.graphColor)
	newGC |= GRAPH_GC;

    if (w->graph.positionColor != old->graph.positionColor)
	newGC |= POSITION_GC;

    if (w->graph.markerColor != old->graph.markerColor)
	newGC |= MARKER_GC;

    if (w->core.background_pixel != old->core.background_pixel)
	newGC |= MARKER_GC;

    if (newGC)
    {
	DestroyGC(old, newGC);
	CreateGC(w, newGC);
	redraw = TRUE;
    }

    if (redrawPending)
	return FALSE;
    else
	return redrawPending = redraw;
}

static void
Resize(w)
Widget          w;
{
    recalc(w);
}

static void
selectMarker(w, event)
GraphWidget     w;
XButtonEvent   *event;
{
    w->graph.marker = abs(event->x - w->graph.leftMarkerX) <
	abs(event->x - w->graph.rightMarkerX) ? GraphLeftMarker :
	GraphRightMarker;

    moveMarker(w, event);
}

static void
moveMarker(w, event)
GraphWidget     w;
XButtonEvent   *event;
{
    int             marker = event->x / w->graph.hscale + w->graph.start;

    /* erase the old marker */
    drawMarker(w, w->graph.marker);

    if (w->graph.marker == GraphLeftMarker)
	w->graph.leftMarker = marker;
    else
	w->graph.rightMarker = marker;

    /* draw the new marker */
    drawMarker(w, w->graph.marker);

    if (w->graph.marker == GraphLeftMarker)
	XtCallCallbacks((Widget) w, XtNleftProc,
			(XtPointer) w->graph.leftMarker);
    else
	XtCallCallbacks((Widget) w, XtNrightProc,
			(XtPointer) w->graph.rightMarker);
}

/* public functions */

void
GraphSetPosition(w, p)
GraphWidget     w;
int             p;
{
    /* erase the old position */
    drawPosition(w);

    w->graph.position = p;

    /* draw the new position */
    drawPosition(w);
}

void
GraphRedraw(w)
GraphWidget     w;
{
    paintWindow(w);
}
