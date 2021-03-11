#ifndef lint
static char *rcsid = "$Id: CandPanel.c,v 1.11 1994/04/22 04:26:22 ishisone Rel $";
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
#include "CandPanelP.h"
#include "ConvDisp.h"

static XtResource resources[] = {
#define offset(field) XtOffset(CandidatePanelWidget, cpanel.field)
    { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
	offset(foreground), XtRString, XtDefaultForeground },
    { XtNhorizontalSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	offset(hspace), XtRString, "6" },
    { XtNverticalSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	offset(vspace), XtRString, "4" },
    { XtNlist, XtCList, XtRPointer, sizeof(ICString*),
	offset(list), XtRImmediate, NULL },
    { XtNnumStrings, XtCNumStrings, XtRInt, sizeof(int),
	offset(nstrings), XtRImmediate, 0 },
    { XtNdefaultWidth, XtCDefaultWidth, XtRDimension, sizeof(Dimension),
	offset(defaultwidth), XtRString, "400" },
    { XtNcurrentItem, XtCCurrentItem, XtRInt, sizeof(int),
	offset(current), XtRImmediate, 0 },
    { XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
	offset(cursor), XtRImmediate, (XtPointer)None },
    { XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	offset(callback), XtRCallback, NULL },
#undef offset
};

static void Move(/* Widget, XEvent*, String*, Cardinal* */);
static void Set(/* Widget, XEvent*, String*, Cardinal* */);
static void Notify(/* Widget, XEvent*, String*, Cardinal* */);

static XtActionsRec actions[] = {
    { "move",	Move },
    { "set",	Set },
    { "select",	Notify },
};

static char translations[] =
    "<Btn1Down>: set() select()\n\
     <Key>Up:    move(up)\n\
     <Key>Down:  move(down)\n\
     <Key>Left:  move(left)\n\
     <Key>Right: move(right)";

static void Initialize(), Destroy();
static void Realize();
static void Redisplay();
static void Resize();
static Boolean SetValues();
static XtGeometryResult QueryGeometry();
static void InsertChild();

static void GetInvGC();
static int MaxWidth();
static void ComputeSize();
static void Layout();
static void ToggleHighlight();

static CompositeClassExtensionRec CompositeExtension = {
    /* next_extension		*/	NULL,
    /* record_type		*/	NULLQUARK,
    /* version			*/	XtCompositeExtensionVersion,
    /* record_size		*/	sizeof(CompositeClassExtensionRec),
    /* accept_objects		*/	True,
};

CandidatePanelClassRec candidatePanelClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &compositeClassRec,
    /* class_name		*/	"CandidatePanel",
    /* widget_size		*/	sizeof(CandidatePanelRec),
    /* class_initialize		*/	NULL,
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
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
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
  { /* candidatepanel fields */
    /* empty			*/	0
  }
};

WidgetClass candidatePanelWidgetClass = (WidgetClass)&candidatePanelClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)new;

    cpw->cpanel.displayobj = NULL;
    GetInvGC(cpw);
}

static void
Destroy(w)
Widget w;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;

    if (cpw->cpanel.invgc != NULL) XtReleaseGC(w, cpw->cpanel.invgc);
}

static void
Realize(w, mask, value)
Widget w;
XtValueMask *mask;
XSetWindowAttributes *value;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;
    CompositeWidgetClass super = (CompositeWidgetClass)XtClass(w)->core_class.superclass;
    String params[1];
    Cardinal num_params;

    if (cpw->cpanel.displayobj == NULL) {
	params[0] = XtClass(w)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext(w),
		      "childError", "number", "WidgetError",
		      "%s: has no child (ConvDisplayObject)",
		      params, &num_params);
    }

    if (cpw->cpanel.cursor != None) {
	*mask |= CWCursor;
	value->cursor = cpw->cpanel.cursor;
    }

    (*super->core_class.realize)(w, mask, value);
}

/* ARGSUSED */
static void
Redisplay(w, ev, region)
Widget w;
XEvent *ev;
Region region;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;
    Widget dispobj = cpw->cpanel.displayobj;
    XExposeEvent *event = (XExposeEvent *)ev;
    ICString *list = cpw->cpanel.list;
    int cwidth, cheight, hspace, vspace;
    int c0, c1, r0, r1;
    int row, col;
    int idx;
    int x, y;

    if (list == NULL || dispobj == NULL) return;

    cwidth = cpw->cpanel.maxwidth;
    cheight = cpw->cpanel.fontheight;
    hspace = cpw->cpanel.hspace;
    vspace = cpw->cpanel.vspace;

    c0 = (event->x + hspace - hspace / 2) / (cwidth + hspace);
    c1 = (event->x + event->width - 1 - hspace / 2) / (cwidth + hspace) + 1;
    if (c1 > cpw->cpanel.ncolumns) c1 = cpw->cpanel.ncolumns;

    r0 = (event->y + vspace - vspace / 2) / (cheight + vspace);
    r1 = (event->y + event->height - 1 - vspace / 2) / (cheight + vspace) + 1;
    if (r1 > cpw->cpanel.nrows) r1 = cpw->cpanel.nrows;

    for (row = r0; row < r1; row++) {
	y = (cheight + vspace) * row + vspace / 2;
	for (col = c0; col < c1; col++) {
	    x = (cwidth + hspace) * col + hspace / 2;
	    idx = row * cpw->cpanel.ncolumns + col;
	    if (idx >= cpw->cpanel.nstrings) return;
	    if (idx == cpw->cpanel.current) {
		XClearArea(XtDisplay(w), XtWindow(w), x, y,
			   (unsigned int)cwidth, (unsigned int)cheight,
			   False);
	    }
	    CDDrawString(dispobj, w, list + idx, 0, -1, x, y);
	    if (idx == cpw->cpanel.current) {
		ToggleHighlight(cpw, idx);
	    }
	}
    }
}

static void
Resize(w)
Widget w;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;

    Layout(cpw, False, False);
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
    CandidatePanelWidget old = (CandidatePanelWidget)cur;
    CandidatePanelWidget new = (CandidatePanelWidget)wid;
    Boolean redisplay = False;
    Boolean listspecified = False;
    int i;

    if (new->cpanel.displayobj == NULL) return False;

    for (i = 0; i < *num_args; i++) {
	if (!strcmp(args[i].name, XtNlist)) {
	    listspecified = True;
	    break;
	}
    }

    if (new->cpanel.foreground != old->cpanel.foreground ||
	new->core.background_pixel != old->core.background_pixel) {
	XtVaSetValues(new->cpanel.displayobj,
		      XtNforeground, new->cpanel.foreground,
		      XtNbackground, new->core.background_pixel,
		      NULL);
	redisplay = True;
    }
		      
    if (listspecified ||
	new->cpanel.list != old->cpanel.list ||
	new->cpanel.nstrings != old->cpanel.nstrings) {
	/* compute maximum pixel width of the list items */
	new->cpanel.maxwidth = MaxWidth(new);
    }

    if (listspecified ||
	new->cpanel.list != old->cpanel.list ||
	new->cpanel.nstrings != old->cpanel.nstrings ||
	new->cpanel.hspace != old->cpanel.hspace ||
	new->cpanel.vspace != old->cpanel.vspace ||
	new->cpanel.defaultwidth != old->cpanel.defaultwidth) {
	Layout(new, True, True);
	redisplay = True;
    }

    if (new->cpanel.current != old->cpanel.current &&
	!redisplay && XtIsRealized(wid)) {
	ToggleHighlight(new, old->cpanel.current);
	ToggleHighlight(new, new->cpanel.current);
    }

    if (new->cpanel.cursor != old->cpanel.cursor && XtIsRealized(wid)) {
	XDefineCursor(XtDisplay(wid), XtWindow(wid), new->cpanel.cursor);
    }

    return redisplay;
}

static XtGeometryResult
QueryGeometry(w, req, ret)
Widget w;
XtWidgetGeometry *req;
XtWidgetGeometry *ret;
{
    Dimension width, height;
    Dimension owidth, oheight;

    ret->request_mode = 0;

    if ((req->request_mode & (CWWidth | CWHeight)) == 0) return XtGeometryYes;

    width = (req->request_mode & CWWidth) ? req->width : w->core.width;
    height = (req->request_mode & CWHeight) ? req->height : w->core.height;

    owidth = width;
    oheight = height;
    ComputeSize((CandidatePanelWidget)w,
		(req->request_mode & CWWidth) != 0,
		(req->request_mode & CWHeight) != 0,
		&width, &height);
    ret->request_mode = CWWidth | CWHeight;
    ret->width = width;
    ret->height = height;

    if (width != owidth || height != oheight) return XtGeometryAlmost;

    return XtGeometryYes;
}

static void
InsertChild(w)
Widget w;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)XtParent(w);
    CompositeWidgetClass super = (CompositeWidgetClass)XtClass(cpw)->core_class.superclass;
    String params[1];
    Cardinal num_params;

    if (!XtIsSubclass(w, convDisplayObjectClass)) {
	params[0] = XtClass(cpw)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext(w),
		      "childError", "class", "WidgetError",
		      "%s: child must be subclass of ConvDisplayObject",
		      params, &num_params);
    }
    if (cpw->composite.num_children != 0) {
	params[0] = XtClass(cpw)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext(w),
		      "childError", "number", "WidgetError",
		      "%s: can only take one child",
		      params, &num_params);
    }

    (*super->composite_class.insert_child)(w);

    cpw->cpanel.displayobj = w;
    cpw->cpanel.fontheight = CDLineHeight(w, (Position *)NULL);

#ifdef notdef
    {
    Pixel fg, bg;
    XtVaGetValues(w, XtNforeground, &fg, XtNbackground, &bg, NULL);
    GetInvGC(cpw, fg, bg);
    }
#endif

    if (cpw->cpanel.list != NULL) {
	cpw->cpanel.maxwidth = MaxWidth(cpw);
	Layout(cpw, cpw->core.width == 0, cpw->core.height == 0);
    }
}

static void
GetInvGC(cpw)
CandidatePanelWidget cpw;
{
    XGCValues values;

    values.function = GXinvert;
    values.plane_mask = cpw->cpanel.foreground ^ cpw->core.background_pixel;
    cpw->cpanel.invgc = XtGetGC((Widget)cpw, GCFunction|GCPlaneMask, &values);
}

/* ARGSUSED */
static void
Move(w, ev, args, num_args)
Widget w;
XEvent *ev;
String *args;
Cardinal *num_args;
{
    int dir;

    if (*num_args < 1) return;

    if (!strcmp(*args, "left") || !strcmp(*args, "Left")) {
	dir = ICMoveLeft;
    } else if (!strcmp(*args, "right") || !strcmp(*args, "Right")) {
	dir = ICMoveRight;
    } else if (!strcmp(*args, "up") || !strcmp(*args, "Up")) {
	dir = ICMoveUp;
    } else if (!strcmp(*args, "down") || !strcmp(*args, "Down")) {
	dir = ICMoveDown;
    } else {
	XtAppWarning(XtWidgetToApplicationContext(w),
		     "CandidatePanel: unknown direction");
	return;
    }

    CPanelMoveCurrent(w, dir);
}

/* ARGSUSED */
static void
Set(w, ev, args, num_args)
Widget w;
XEvent *ev;
String *args;
Cardinal *num_args;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;
    XButtonEvent *event = (XButtonEvent *)ev;
    int cwidth, cheight, hspace, vspace;
    int x, y;
    int row, col;
    int newidx;

    cwidth = cpw->cpanel.maxwidth;
    cheight = cpw->cpanel.fontheight;
    hspace = cpw->cpanel.hspace;
    vspace = cpw->cpanel.vspace;

    x = event->x - hspace / 2;
    col = x / (cwidth + hspace);
    if (col >= cpw->cpanel.ncolumns || (x % (cwidth + hspace)) > cwidth) return;

    y = event->y - vspace / 2;
    row = y / (cheight + vspace);
    if (row >= cpw->cpanel.nrows || (y % (cheight + vspace)) > cheight) return;

    newidx = col + cpw->cpanel.ncolumns * row;
    if (newidx >= cpw->cpanel.nstrings) return;

    CPanelSetCurrent(w, newidx);
}

/* ARGSUSED */
static void
Notify(w, ev, args, num_args)
Widget w;
XEvent *ev;
String *args;
Cardinal *num_args;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;

    XtCallCallbackList(w, cpw->cpanel.callback,
		       (XtPointer)cpw->cpanel.current);
}

static int
MaxWidth(cpw)
CandidatePanelWidget cpw;
{
    Widget dispobj = cpw->cpanel.displayobj;
    ICString *list = cpw->cpanel.list;
    int maxwidth;
    int i;

    maxwidth = 0;
    for (i = 0; i < cpw->cpanel.nstrings; i++) {
	int w = CDStringWidth(dispobj, list + i, 0, -1);
	if (w > maxwidth) maxwidth = w;
    }

    return maxwidth;
}

static void
ComputeSize(cpw, resizex, resizey, width_inout, height_inout)
CandidatePanelWidget cpw;
int resizex;
int resizey;
Dimension *width_inout;
Dimension *height_inout;
{
    int nrows, ncolumns;
    int width, height;

    if (cpw->cpanel.displayobj == NULL || cpw->cpanel.nstrings == 0) return;

    width = *width_inout;
    height = *height_inout;

    if (resizex) {
	if (resizey) {
	    int maxheight = HeightOfScreen(XtScreen((Widget)cpw));

	    /* use defaultwidth */
	    ncolumns = cpw->cpanel.defaultwidth /
			(cpw->cpanel.maxwidth + cpw->cpanel.hspace);
	    if (ncolumns > cpw->cpanel.nstrings) ncolumns = cpw->cpanel.nstrings;
	    if (ncolumns <= 0) ncolumns = 1;
	    nrows = (cpw->cpanel.nstrings + ncolumns - 1) / ncolumns;
	    width = (cpw->cpanel.maxwidth + cpw->cpanel.hspace) * ncolumns;
	    height = (cpw->cpanel.fontheight + cpw->cpanel.vspace) * nrows;
	    /*
	     * If the computed height exceeds display height,
	     * expand width so that the entire window fits into the display.
	     */
	    if (height > maxheight) {
		/* compute maximum number of rows */
		nrows = maxheight / (cpw->cpanel.fontheight + cpw->cpanel.vspace);
		if (nrows <= 0) nrows = 1;
		ncolumns = (cpw->cpanel.nstrings + nrows - 1) / nrows;
		if (ncolumns > cpw->cpanel.nstrings) {
		    ncolumns = cpw->cpanel.nstrings;
		}
		/* re-compute correct number of rows */
		nrows = (cpw->cpanel.nstrings + ncolumns - 1) / ncolumns;
		width = (cpw->cpanel.maxwidth + cpw->cpanel.hspace) * ncolumns;
		height = (cpw->cpanel.fontheight + cpw->cpanel.vspace) * nrows;
	    }
	} else {
	    /* use specified height */
	    nrows = height / (cpw->cpanel.fontheight + cpw->cpanel.vspace);
	    if (nrows <= 0) nrows = 1;
	    ncolumns = (cpw->cpanel.nstrings + nrows - 1) / nrows;
	    if (ncolumns > cpw->cpanel.nstrings) ncolumns = cpw->cpanel.nstrings;
	    width = (cpw->cpanel.maxwidth + cpw->cpanel.hspace) * ncolumns;
	}
    } else {
	ncolumns = width / (cpw->cpanel.maxwidth + cpw->cpanel.hspace);
	if (ncolumns <= 0) ncolumns = 1;
	nrows = (cpw->cpanel.nstrings + ncolumns - 1) / ncolumns;
	if (resizey) {
	    height = (cpw->cpanel.fontheight + cpw->cpanel.vspace) * nrows;
	}
    }
    cpw->cpanel.ncolumns = ncolumns;
    cpw->cpanel.nrows = nrows;
    *width_inout = width;
    *height_inout = height;
}

static void
Layout(cpw, resizex, resizey)
CandidatePanelWidget cpw;
int resizex;
int resizey;
{
    Dimension width, height;
    Dimension owidth, oheight;
    XtGeometryResult re;

    if (cpw->cpanel.displayobj == NULL) return;

    width = cpw->core.width;
    height = cpw->core.height;
    ComputeSize(cpw, resizex, resizey, &width, &height);

    if (width != cpw->core.width || height != cpw->core.height) {
	owidth = width;
	oheight = height;
	re = XtMakeResizeRequest((Widget)cpw, owidth, oheight, &width, &height);
	switch (re) {
	case XtGeometryYes:
	    /* no problem */
	    break;
	case XtGeometryNo:
	    width = cpw->core.width;
	    height = cpw->core.height;
	    ComputeSize(cpw, False, False, &width, &height);
	    break;
	case XtGeometryAlmost:
	    ComputeSize(cpw,
			width != owidth,
			height != oheight,
			&width, &height);
	    re = XtMakeResizeRequest((Widget)cpw, width, height, &width, &height);
	    switch (re) {
	    case XtGeometryYes:
		break;
	    case XtGeometryNo:
		width = cpw->core.width;
		height = cpw->core.height;
		ComputeSize(cpw, False, False, &width, &height);
		break;
	    case XtGeometryAlmost:
		ComputeSize(cpw, False, False, &width, &height);
		(void)XtMakeResizeRequest((Widget)cpw, width, height, &width, &height);
	    }
	}
    }
}

static void
ToggleHighlight(cpw, idx)
CandidatePanelWidget cpw;
int idx;
{
    int row, col;
    int x, y;
    int width;

    if (idx < 0 || cpw->cpanel.nstrings <= idx) return;

    col = idx % cpw->cpanel.ncolumns;
    row = idx / cpw->cpanel.ncolumns;
    x = (cpw->cpanel.maxwidth + cpw->cpanel.hspace) * col +
      cpw->cpanel.hspace / 2;
    y = (cpw->cpanel.fontheight + cpw->cpanel.vspace) * row +
      cpw->cpanel.vspace / 2;
    width = CDStringWidth(cpw->cpanel.displayobj, cpw->cpanel.list + idx,
			  0, -1);

    XFillRectangle(XtDisplay(cpw), XtWindow(cpw), cpw->cpanel.invgc, x, y,
		   (unsigned int)width, (unsigned int)cpw->cpanel.fontheight);
}


/*
 * Public Functions
 */

void
CPanelSetList(w, list, nstrings, current, resize)
Widget w;
ICString *list;
int nstrings;
int current;
int resize;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;

    if (list != NULL) {
	cpw->cpanel.list = list;
	cpw->cpanel.nstrings = nstrings;
	if (current < 0) current = 0;
	if (current >= nstrings) current = nstrings - 1;
	cpw->cpanel.current = current;
    }

    if (cpw->cpanel.displayobj == NULL) return;

    cpw->cpanel.fontheight = CDLineHeight(cpw->cpanel.displayobj,
					  (Position *)NULL);
    /* compute maximum pixel width of the list items */
    cpw->cpanel.maxwidth = MaxWidth(cpw);
    Layout(cpw, resize, resize);

    if (XtIsRealized(w)) XClearWindow(XtDisplay(w), XtWindow(w));
}

void
CPanelSetCurrent(w, idx)
Widget w;
int idx;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;

    if (idx < 0 || cpw->cpanel.nstrings <= idx) return;

    if (idx == cpw->cpanel.current) return;

    if (cpw->cpanel.displayobj != NULL && XtIsRealized(w)) {
	ToggleHighlight(cpw, cpw->cpanel.current);
	ToggleHighlight(cpw, idx);
    }
    cpw->cpanel.current = idx;
}

void
CPanelMoveCurrent(w, dir)
Widget w;
int dir;
{
    CandidatePanelWidget cpw = (CandidatePanelWidget)w;
    int newidx;
    int row, col;
    int nstrings = cpw->cpanel.nstrings;

    if (nstrings <= 0) return;

    col = cpw->cpanel.current % cpw->cpanel.ncolumns;
    row = cpw->cpanel.current / cpw->cpanel.ncolumns;

    switch (dir) {
    case ICMoveLeft:
	if ((newidx = cpw->cpanel.current - 1) < 0) newidx = nstrings - 1;
	break;
    case ICMoveRight:
	if ((newidx = cpw->cpanel.current + 1) >= nstrings) newidx = 0;
	break;
    case ICMoveUp:
    case ICMovePrevPage:
	if (--row < 0) row = cpw->cpanel.nrows - 1;
	newidx = row * cpw->cpanel.ncolumns + col;
	if (newidx >= nstrings) newidx -= cpw->cpanel.ncolumns;
	break;
    case ICMoveDown:
    case ICMoveNextPage:
	if (++row >= cpw->cpanel.nrows) row = 0;
	newidx = row * cpw->cpanel.ncolumns + col;
	if (newidx >= nstrings) newidx = col;
	break;
    case ICMoveLeftMost:
	newidx = row * cpw->cpanel.ncolumns;
	break;
    case ICMoveRightMost:
	newidx = (row + 1) * cpw->cpanel.ncolumns - 1;
	if (newidx >= cpw->cpanel.nstrings) newidx = cpw->cpanel.nstrings - 1;
	break;
    case ICMoveFirst:
	newidx = 0;
	break;
    case ICMoveLast:
	newidx = cpw->cpanel.nstrings - 1;
	break;
    }

    CPanelSetCurrent(w, newidx);
}
