#ifndef lint
static char *rcsid = "$Id: Canvas.c,v 1.3 1991/09/23 03:57:20 ishisone Rel $";
#endif
/*
 * Copyright (c) 1990  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "CanvasP.h"

static XtResource resources[] = {
#define offset(field) XtOffset(CanvasWidget, canvas.field)
    { XtNexposeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(exposecallback), XtRCallback, (XtPointer)NULL },
    { XtNresizeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(resizecallback), XtRCallback, (XtPointer)NULL },
#undef offset
};

static void Redisplay();
static void Resize();

CanvasClassRec canvasClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"Canvas",
    /* widget_size		*/	sizeof(CanvasRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	NULL,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* canvas fields */
    /* empty			*/	0
  }
};

WidgetClass canvasWidgetClass = (WidgetClass)&canvasClassRec;

/* ARGSUSED */
static void
Redisplay(w, event, region)
Widget w;
XEvent *event;
Region region;
{
    CanvasWidget csw = (CanvasWidget)w;

    XtCallCallbackList(w, csw->canvas.exposecallback, (XtPointer)event);
}

static void
Resize(w)
Widget w;
{
    CanvasWidget csw = (CanvasWidget)w;

    XtCallCallbackList(w, csw->canvas.resizecallback, (XtPointer)NULL);
}
