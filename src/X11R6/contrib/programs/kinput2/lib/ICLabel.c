#ifndef lint
static char *rcsid = "$Id: ICLabel.c,v 1.11 1991/09/23 04:01:20 ishisone Rel $";
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
#include <X11/Xmu/Converters.h>
#if XtSpecificationRelease > 4
#include <X11/Xfuncs.h>
#endif
#include "ICLabelP.h"
#include "ConvDisp.h"

static XtResource resources[] = {
#define offset(field) XtOffset(ICLabelWidget, iclabel.field)
    { XtNhorizontalSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	offset(hspace), XtRString, "1" },
    { XtNverticalSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	offset(vspace), XtRString, "1" },
    { XtNlabel, XtCLabel, XtRPointer, sizeof(ICString*),
	offset(label), XtRImmediate, NULL },
    { XtNjustify, XtCJustify, XtRJustify, sizeof(XtJustify),
	offset(justify), XtRImmediate, (XtPointer)XtJustifyCenter },
    { XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
	offset(cursor), XtRImmediate, (XtPointer)None },
#undef offset
};

static void ClassInitialize();
static void Initialize(), Destroy();
static void Realize();
static void Redisplay();
static Boolean SetValues();
static XtGeometryResult QueryGeometry();
static void InsertChild();

static void computeSize();
static ICString *copyString();
static void freeString();

static CompositeClassExtensionRec CompositeExtension = {
    /* next_extension		*/	NULL,
    /* record_type		*/	NULLQUARK,
    /* version			*/	XtCompositeExtensionVersion,
    /* record_size		*/	sizeof(CompositeClassExtensionRec),
    /* accept_objects		*/	True,
};

ICLabelClassRec icLabelClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &compositeClassRec,
    /* class_name		*/	"ICLabel",
    /* widget_size		*/	sizeof(ICLabelRec),
    /* class_initialize		*/	ClassInitialize,
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
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	QueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* composite fields */
    /* geometry_manager		*/	NULL,
    /* change_managed		*/	NULL,
    /* insert_child		*/	InsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	(XtPointer)&CompositeExtension,
  },
  { /* iclabel fields */
    /* empty			*/	0
  }
};

WidgetClass icLabelWidgetClass = (WidgetClass)&icLabelClassRec;

static void
ClassInitialize()
{
    XtAddConverter(XtRString, XtRJustify, XmuCvtStringToJustify, NULL, 0);
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    ICLabelWidget iclw = (ICLabelWidget)new;

    if (iclw->iclabel.label != NULL) {
	iclw->iclabel.label = copyString(iclw->iclabel.label);
    }
    iclw->iclabel.width = 0;
}

static void
Destroy(w)
Widget w;
{
    ICLabelWidget iclw = (ICLabelWidget)w;

    if (iclw->iclabel.label != NULL) freeString(iclw->iclabel.label);
}

static void
Realize(w, maskp, attr)
Widget w;
XtValueMask *maskp;
XSetWindowAttributes *attr;
{
    ICLabelWidget iclw = (ICLabelWidget)w;

    if (iclw->iclabel.cursor != None) {
	*maskp |= CWCursor;
	attr->cursor = iclw->iclabel.cursor;
    }
    (*icLabelWidgetClass->core_class.superclass->core_class.realize)(w, maskp, attr);
}

/* ARGSUSED */
static void
Redisplay(w, ev, region)
Widget w;
XEvent *ev;
Region region;
{
    ICLabelWidget iclw = (ICLabelWidget)w;
    Widget dispobj = iclw->iclabel.displayobj;
    int x, y;

    if (iclw->iclabel.label == NULL || iclw->iclabel.displayobj == NULL) return;

    switch (iclw->iclabel.justify) {
    case XtJustifyLeft:
	x = iclw->iclabel.hspace;
	break;
    case XtJustifyRight:
	x = iclw->core.width - (iclw->iclabel.vspace + iclw->iclabel.width);
	break;
    case XtJustifyCenter:
    default:
	x = (iclw->core.width - iclw->iclabel.width) / 2;
	break;
    }
    y = (iclw->core.height - iclw->iclabel.fontheight) / 2;

    CDDrawString(dispobj, w, iclw->iclabel.label, 0, -1, x, y);
}

/* ARGSUSED */
static Boolean
SetValues(cur, req, wid, args, num_args)
Widget cur;
Widget req;
Widget wid;
ArgList args;
Cardinal *num_args;
{
    ICLabelWidget old = (ICLabelWidget)cur;
    ICLabelWidget new = (ICLabelWidget)wid;
    Boolean widthspecified = False;
    Boolean heightspecified = False;
    Boolean redisplay = False;
    Boolean label_changed = False;
    int i;

    for (i = 0; i < *num_args; i++) {
	if (!strcmp(args->name, XtNwidth)) widthspecified = True;
	else if (!strcmp(args->name, XtNheight)) heightspecified = True;
    }

    if (new->iclabel.label != old->iclabel.label) {
	/* compute maximum pixel width of the list items */
	if (old->iclabel.label != NULL) freeString(old->iclabel.label);
	if (new->iclabel.label != NULL) {
	    new->iclabel.label = copyString(new->iclabel.label);
	    if (new->iclabel.displayobj != NULL) {
		new->iclabel.width = CDStringWidth(new->iclabel.displayobj,
						   new->iclabel.label, 0, -1);
	    }
	} else {
	    new->iclabel.width = 0;
	}
	label_changed = True;
	redisplay = True;
    }

    if (new->iclabel.hspace != old->iclabel.hspace ||
	new->iclabel.vspace != old->iclabel.vspace ||
	label_changed) {
	computeSize(new, widthspecified, heightspecified);
	redisplay = True;
    } else if (new->iclabel.justify != old->iclabel.justify) {
	redisplay = True;
    }

    if (new->iclabel.cursor != old->iclabel.cursor && XtIsRealized(wid)) {
	XDefineCursor(XtDisplay(wid), XtWindow(wid), new->iclabel.cursor);
    }

    return redisplay;
}

static XtGeometryResult
QueryGeometry(w, req, ret)
Widget w;
XtWidgetGeometry *req;
XtWidgetGeometry *ret;
{
    ICLabelWidget iclw = (ICLabelWidget)w;

    ret->request_mode = CWWidth | CWHeight;

    ret->width = iclw->iclabel.width + iclw->iclabel.hspace * 2;
    ret->height = iclw->iclabel.fontheight + iclw->iclabel.vspace * 2;

    if ((!(req->request_mode & CWWidth) || ret->width == req->width) &&
	(!(req->request_mode & CWHeight) || ret->height == req->height)) {
	return XtGeometryYes;
    } else if (ret->width == iclw->core.width &&
	       ret->height == iclw->core.height) {
	return XtGeometryNo;
    }
    return XtGeometryAlmost;
}

static void
InsertChild(w)
Widget w;
{
    ICLabelWidget iclw = (ICLabelWidget)XtParent(w);
    CompositeWidgetClass super = (CompositeWidgetClass)XtClass(iclw)->core_class.superclass;
    String params[1];
    Cardinal num_params;

    if (!XtIsSubclass(w, convDisplayObjectClass)) {
	params[0] = XtClass(iclw)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext(w),
		      "childError", "class", "WidgetError",
		      "%s: child must be subclass of ConvDisplayObject",
		      params, &num_params);
    }
    if (iclw->composite.num_children != 0) {
	params[0] = XtClass(iclw)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext(w),
		      "childError", "number", "WidgetError",
		      "%s: can only take one child",
		      params, &num_params);
    }

    (*super->composite_class.insert_child)(w);

    iclw->iclabel.displayobj = w;
    iclw->iclabel.fontheight = CDLineHeight(w, (Position *)NULL);

    if (iclw->iclabel.label != NULL) {
	iclw->iclabel.width = CDStringWidth(iclw->iclabel.displayobj,
					    iclw->iclabel.label, 0, -1);
    }
    computeSize(iclw, iclw->core.width != 0, iclw->core.height != 0);
}

static void
computeSize(iclw, fixwidth, fixheight)
ICLabelWidget iclw;
Boolean fixwidth;
Boolean fixheight;
{
    if (!fixwidth) {
	iclw->core.width = iclw->iclabel.width + iclw->iclabel.hspace * 2;
    }
    if (!fixheight) {
	iclw->core.height = iclw->iclabel.fontheight + iclw->iclabel.vspace * 2;
    }
}

static ICString *
copyString(from)
ICString *from;
{
    ICString *to = XtNew(ICString);

    to->nbytes = from->nbytes;
    to->nchars = from->nchars;
    to->attr = from->attr;
    to->data = XtMalloc(from->nbytes);
    (void)bcopy(from->data, to->data, from->nbytes);
    return to;
}

static void
freeString(s)
ICString *s;
{
    XtFree(s->data);
    XtFree((char *)s);
}


/*
 * public function
 */

void
ICLRecomputeSize(w)
Widget w;
{
    ICLabelWidget iclw = (ICLabelWidget)w;
    Dimension width, height;

    if (iclw->iclabel.displayobj != NULL) {
	iclw->iclabel.fontheight = CDLineHeight(iclw->iclabel.displayobj,
						(Position *)NULL);
	if (iclw->iclabel.label != NULL) {
	    iclw->iclabel.width = CDStringWidth(iclw->iclabel.displayobj,
						iclw->iclabel.label, 0, -1);
	}
	width = iclw->iclabel.width + iclw->iclabel.hspace * 2;
	height = iclw->iclabel.fontheight + iclw->iclabel.vspace * 2;
	XtMakeResizeRequest(w, width, height,
			    (Dimension *)NULL, (Dimension *)NULL);
    } /* else do nothing */
}
