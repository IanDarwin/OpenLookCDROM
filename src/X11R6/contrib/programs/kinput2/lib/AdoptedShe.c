#ifndef lint
static char *rcsid = "$Id: AdoptedShe.c,v 1.6 1991/09/23 04:03:31 ishisone Rel $";
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
#include "AdoptedShP.h"

static XtResource resources[] = {
#define offset(field) XtOffset(AdoptedShellWidget, adoptedshell.field)
    { XtNparentWindow, XtCParentWindow, XtRWindow, sizeof(Window),
	offset(parent), XtRImmediate, None },
    { XtNdisableGeometryManagement, XtCDisableGeometryManagement,
	XtRBoolean, sizeof(Boolean),
	offset(disable_geometry), XtRString, "False" },
#undef offset
};

static void Initialize();
static void Realize();
static Boolean SetValues();
static XtGeometryResult GeometryManager();

static void GetParentInfo();

AdoptedShellClassRec adoptedShellClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &overrideShellClassRec,
    /* class_name		*/	"AdoptedShell",
    /* widget_size		*/	sizeof(AdoptedShellRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
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
    /* resize			*/	XtInheritResize,
    /* expose			*/	XtInheritExpose,
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
    /* geometry_manager		*/	GeometryManager,
    /* change_managed		*/	XtInheritChangeManaged,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	NULL
  },
  { /* shell fields */
    /* extension		*/	NULL
  },
  { /* overrideShell fields */
    /* extension		*/	NULL
  },
  { /* AdoptedShell fields */
    /* empty			*/	0
  }
};

WidgetClass adoptedShellWidgetClass = (WidgetClass)&adoptedShellClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    AdoptedShellWidget asw = (AdoptedShellWidget)new;
    Cardinal i;

    asw->adoptedshell.colormap_specified = False;
    for (i = 0; i < *num_args; i++) {
	if (!strcmp(args[i].name, XtNcolormap)) {
	    asw->adoptedshell.colormap_specified = True;
	    break;
	}
    }
}

static void
Realize(w, maskp, attr)
Widget w;
XtValueMask *maskp;
XSetWindowAttributes *attr;
{
    AdoptedShellWidget asw = (AdoptedShellWidget)w;
    Window savedroot;

    if (asw->adoptedshell.parent == None) {
	asw->adoptedshell.parent = w->core.screen->root;
    } else {
	GetParentInfo(asw);
    }
    asw->adoptedshell.colormap_specified = False;

    /* cheat the super-class's realize function */
    savedroot = w->core.screen->root;
    w->core.screen->root = asw->adoptedshell.parent;

    /* call super class's realize function */
    (*adoptedShellWidgetClass->core_class.superclass->core_class.realize)(w, maskp, attr);

    /* restore savedroot */
    w->core.screen->root = savedroot;
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
    AdoptedShellWidget asw = (AdoptedShellWidget)new;
    AdoptedShellWidget old = (AdoptedShellWidget)cur;

    if (asw->adoptedshell.parent != old->adoptedshell.parent) {
	if (XtIsRealized(new)) {
	    XtAppError(XtWidgetToApplicationContext(new),
		       "AdoptedShell: can't change parent window after realization");
	}
    }
    if (asw->core.colormap != old->core.colormap && !XtIsRealized(new)) {
	asw->adoptedshell.colormap_specified = True;
    }
    return False;
}

static XtGeometryResult
GeometryManager(w, reqp, repp)
Widget w;
XtWidgetGeometry *reqp;
XtWidgetGeometry *repp;
{
    AdoptedShellWidget asw = (AdoptedShellWidget)XtParent(w);
    ShellWidgetClass super;

    if (asw->adoptedshell.disable_geometry) return XtGeometryYes;

    super = (ShellWidgetClass)(adoptedShellWidgetClass->core_class.superclass);
    return (*super->composite_class.geometry_manager)(w, reqp, repp);
}

static void
GetParentInfo(w)
AdoptedShellWidget w;
{
    Window parent = w->adoptedshell.parent;
    XWindowAttributes attr;

    (void)XGetWindowAttributes(XtDisplay((Widget)w), parent, &attr);
    w->core.screen = attr.screen;
    if (!w->adoptedshell.colormap_specified) w->core.colormap = attr.colormap;
    /* may be I should honor user-specified visual, etc. */
    w->core.depth = attr.depth;
    w->shell.visual = attr.visual;
}
