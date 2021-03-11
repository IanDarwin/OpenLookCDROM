#ifndef lint
static char *rcsid = "$Id: CanvasShel.c,v 1.7 1991/09/23 04:03:55 ishisone Rel $";
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
#include "CanvasSheP.h"

static XtResource resources[] = {
#define offset(field) XtOffset(CanvasShellWidget, canvasshell.field)
    { XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
	offset(cursor), XtRImmediate, (XtPointer)None },
    { XtNexposeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(exposecallback), XtRCallback, (XtPointer)NULL },
    { XtNresizeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(resizecallback), XtRCallback, (XtPointer)NULL },
#undef offset
};

static void Redisplay();
static void Realize();
static void Resize();
static Boolean SetValues();

CanvasShellClassRec canvasShellClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &adoptedShellClassRec,
    /* class_name		*/	"CanvasShell",
    /* widget_size		*/	sizeof(CanvasShellRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	NULL,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
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
    /* set_values		*/	SetValues,
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
  { /* Composite */
    /* geometry_manager		*/	XtInheritGeometryManager,
    /* change_managed		*/	XtInheritChangeManaged,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	NULL
  },
  { /* Shell */
    /* extension		*/	NULL
  },
  { /* overrideShell fields */
    /* extension		*/	NULL
  },
  { /* AdoptedShell fields */
    /* empty			*/	0
  },
  { /* canvasShell fields */
    /* empty			*/	0
  }
};

WidgetClass canvasShellWidgetClass = (WidgetClass)&canvasShellClassRec;

/* ARGSUSED */
static void
Redisplay(w, event, region)
Widget w;
XEvent *event;
Region region;
{
    CanvasShellWidget csw = (CanvasShellWidget)w;

    XtCallCallbackList(w, csw->canvasshell.exposecallback, (XtPointer)event);
}

static void
Resize(w)
Widget w;
{
    CanvasShellWidget csw = (CanvasShellWidget)w;

    XtCallCallbackList(w, csw->canvasshell.resizecallback, (XtPointer)NULL);
}

static void
Realize(w, maskp, attr)
Widget w;
XtValueMask *maskp;
XSetWindowAttributes *attr;
{
    CanvasShellWidget csw = (CanvasShellWidget)w;

    if (csw->canvasshell.cursor != None) {
	*maskp |= CWCursor;
	attr->cursor = csw->canvasshell.cursor;
    }
    *maskp |= CWBitGravity;
    attr->bit_gravity = NorthWestGravity;
    (*canvasShellWidgetClass->core_class.superclass->core_class.realize)(w, maskp, attr);
}

/* ARGSUSED */
static Boolean
SetValues(cur, req, new, args, num_args)
Widget cur;
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    CanvasShellWidget csw = (CanvasShellWidget)new;
    CanvasShellWidget old = (CanvasShellWidget)cur;

    if (csw->canvasshell.cursor != old->canvasshell.cursor &&
	XtIsRealized(new)) {
	XDefineCursor(XtDisplay(new), XtWindow(new), csw->canvasshell.cursor);
    }

    return False;
}
