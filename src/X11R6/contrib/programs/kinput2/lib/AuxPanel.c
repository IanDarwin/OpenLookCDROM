#ifndef lint
static char *rcsid = "$Id: AuxPanel.c,v 1.22 1994/06/02 02:29:12 ishisone Rel $";
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

/* Copyright 1991 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of NEC Corporation
 * not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.  NEC 
 * Corporation makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express
 * or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 *
 * Author: Akira Kon, NEC Corporation.  (kon@d1.bs2.mt.nec.co.jp)
 */

/*
  問題点
  ・文字描画が全部書き直し処理になっている。
  ・だれも Destroy を呼んでくれないのでちょっとごみが残る。
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#if XtSpecificationRelease > 4
#include <X11/Xfuncs.h>
#endif
#include "AuxPanelP.h"
#include "ConvDisp.h"
#include "WStr.h"

#define DEBUG_VAR debug_AuxPanel
#include "DebugPrint.h"

static XtResource resources[] = {
#define offset(field) XtOffset(AuxPanelWidget, cpanel.field)
    { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
	offset(foreground), XtRString, XtDefaultForeground },
    { XtNhorizontalSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	offset(hspace), XtRString, "6" },
    { XtNverticalSpacing, XtCSpacing, XtRDimension, sizeof(Dimension),
	offset(vspace), XtRString, "4" },
    { XtNdefaultWidth, XtCDefaultWidth, XtRDimension, sizeof(Dimension),
	offset(defaultwidth), XtRString, "400" },
    { XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
	offset(cursor), XtRImmediate, (XtPointer)None },
    { XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	offset(callback), XtRCallback, NULL },
#undef offset
};

static void Initialize(), Destroy();
static void Realize();
static void Redisplay();
static void Resize();
static Boolean SetValues();
static XtGeometryResult QueryGeometry();
static void InsertChild();

static void GetInvGC();
static void ComputeSize();
static void Layout();

static void freeDisplaySegments();
static void UpdateAuxDisplay();

static CompositeClassExtensionRec CompositeExtension = {
    /* next_extension		*/	NULL,
    /* record_type		*/	NULLQUARK,
    /* version			*/	XtCompositeExtensionVersion,
    /* record_size		*/	sizeof(CompositeClassExtensionRec),
    /* accept_objects		*/	True,
};

AuxPanelClassRec auxPanelClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &compositeClassRec,
    /* class_name		*/	"AuxPanel",
    /* widget_size		*/	sizeof(AuxPanelRec),
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
  { /* auxpanel fields */
    /* empty			*/	0
  }
};

WidgetClass auxPanelWidgetClass = (WidgetClass)&auxPanelClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    AuxPanelWidget cpw = (AuxPanelWidget)new;

    TRACE(("AuxPanel:Initialize()\n"));

    cpw->cpanel.displayobj = NULL;
    cpw->cpanel.dispauxsegments = NULL;
    cpw->cpanel.dispauxsegmentsize = 0;
    cpw->cpanel.numauxsegments = 0;
    GetInvGC(cpw);
}

static void
Destroy(w)
Widget w;
{
    AuxPanelWidget cpw = (AuxPanelWidget)w;

    TRACE(("AuxPanel:Destroy()\n"));

    freeDisplaySegments(cpw);
    if (cpw->cpanel.invgc != NULL) XtReleaseGC(w, cpw->cpanel.invgc);
}

static void
Realize(w, mask, value)
Widget w;
XtValueMask *mask;
XSetWindowAttributes *value;
{
    AuxPanelWidget cpw = (AuxPanelWidget)w;
    CompositeWidgetClass super = (CompositeWidgetClass)XtClass(w)->core_class.superclass;
    String params[1];
    Cardinal num_params;

    TRACE(("AuxPanel:Realize()\n"));

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
    AuxPanelWidget cpw = (AuxPanelWidget)w;
    XExposeEvent *event = (XExposeEvent *)ev;
    DisplaySegment *dseg;
    int cheight, hspace, vspace;
    int x, i;

    TRACE(("AuxPanel:Redisplay()\n"));

    cheight = cpw->cpanel.fontheight;
    hspace = cpw->cpanel.hspace;
    vspace = cpw->cpanel.vspace;

    if (event->y + event->height < vspace / 2
	|| cheight + vspace / 2 < event->y)
      return;

    for (i = 0, x = hspace / 2, dseg = cpw->cpanel.dispauxsegments;
	 i < cpw->cpanel.numauxsegments ; i++, dseg++) {
      if (x < event->x + event->width && event->x < x + dseg->width) {
	dseg->redrawpos = 0;
      }
      else {
	dseg->redrawpos = -1; /* 書き直す必要がない */
      }
      x += dseg->width;
    }

    UpdateAuxDisplay(cpw);
}

static void
Resize(w)
Widget w;
{
    AuxPanelWidget cpw = (AuxPanelWidget)w;

    TRACE(("AuxPanel:Resize()\n"));

    Layout(cpw, True, True);
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
    AuxPanelWidget old = (AuxPanelWidget)cur;
    AuxPanelWidget new = (AuxPanelWidget)wid;
    Boolean redisplay = False;

    TRACE(("AuxPanel:SetValues()\n"));

    if (new->cpanel.displayobj == NULL) return False;

    if (new->cpanel.foreground != old->cpanel.foreground ||
	new->core.background_pixel != old->core.background_pixel) {
	XtVaSetValues(new->cpanel.displayobj,
		      XtNforeground, new->cpanel.foreground,
		      XtNbackground, new->core.background_pixel,
		      NULL);
	redisplay = True;
    }
		      
    if (new->cpanel.numauxsegments > 0 ||
	new->cpanel.hspace != old->cpanel.hspace ||
	new->cpanel.vspace != old->cpanel.vspace ||
	new->cpanel.defaultwidth != old->cpanel.defaultwidth) {
	Layout(new, True, True);
	redisplay = True;
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

    TRACE(("AuxPanel:QueryGeometry()\n"));

    ret->request_mode = 0;

    if ((req->request_mode & (CWWidth | CWHeight)) == 0) return XtGeometryYes;

    width = (req->request_mode & CWWidth) ? req->width : w->core.width;
    height = (req->request_mode & CWHeight) ? req->height : w->core.height;

    owidth = width;
    oheight = height;
    ComputeSize((AuxPanelWidget)w,
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
    AuxPanelWidget cpw = (AuxPanelWidget)XtParent(w);
    CompositeWidgetClass super = (CompositeWidgetClass)XtClass(cpw)->core_class.superclass;
    String params[1];
    Cardinal num_params;

    TRACE(("AuxPanel:InsertChild()\n"));

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

    if (cpw->cpanel.numauxsegments > 0) {
	Layout(cpw, cpw->core.width == 0, cpw->core.height == 0);
    }
}

static void
GetInvGC(cpw)
AuxPanelWidget cpw;
{
    XGCValues values;

    TRACE(("AuxPanel:GetInvGC()\n"));

    values.function = GXinvert;
    values.plane_mask = cpw->cpanel.foreground ^ cpw->core.background_pixel;
    cpw->cpanel.invgc = XtGetGC((Widget)cpw, GCFunction|GCPlaneMask, &values);
}

/* ARGSUSED */
static void
ComputeSize(cpw, resizex, resizey, width_inout, height_inout)
AuxPanelWidget cpw;
int resizex;
int resizey;
Dimension *width_inout;
Dimension *height_inout;
{
    int width, height;
    int i, minwidth = 0;
    DisplaySegment *dseg = cpw->cpanel.dispauxsegments;


    TRACE(("AuxPanel:ComputeSize()\n"));

    if (cpw->cpanel.displayobj == NULL) return;

    width = *width_inout;
    height = *height_inout;

    for (i = 0 ; i < cpw->cpanel.numauxsegments ; i++, dseg++) {
      minwidth += CDStringWidth(cpw->cpanel.displayobj, &dseg->seg, 0, -1);
    }
    minwidth += cpw->cpanel.hspace * 2;
    if (resizex) {
      width = minwidth;
    } else {
      width = (cpw->core.width < minwidth) ? minwidth : cpw->core.width;
    }
    height = cpw->cpanel.fontheight + cpw->cpanel.vspace;
    *width_inout = width;
    *height_inout = height;
}

static void
Layout(cpw, resizex, resizey)
AuxPanelWidget cpw;
int resizex;
int resizey;
{
    Dimension width, height;
    Dimension owidth, oheight;
    XtGeometryResult re;

    TRACE(("AuxPanel:Layout()\n"));

    if (cpw->cpanel.displayobj == NULL) return;

    width = cpw->core.width;
    height = cpw->core.height;
    ComputeSize(cpw, resizex, resizey, &width, &height);
    if (width != cpw->core.width || height != cpw->core.height) {
	owidth = width;
	oheight = height;
	re = XtMakeResizeRequest((Widget)cpw, owidth, oheight,
				 &width, &height);
	switch (re) {
	case XtGeometryYes:
	  /* no problem */
	  break;
	}
    }
}

/*- allocDisplaySegments: prepare specified number of display segments -*/
static void
allocDisplaySegments(cpw, n)
AuxPanelWidget cpw;
Cardinal n;
{
  if (cpw->cpanel.dispauxsegmentsize > n) return;
  n = ((n + 3) / 4) * 4 ;
  if (cpw->cpanel.dispauxsegments == NULL) {
    cpw->cpanel.dispauxsegments = 
      (DisplaySegment *)XtCalloc(n, sizeof(DisplaySegment));
  } else {
    cpw->cpanel.dispauxsegments = 
      (DisplaySegment *)XtRealloc((char *)cpw->cpanel.dispauxsegments,
				  n * sizeof(DisplaySegment));
    bzero((char *)(cpw->cpanel.dispauxsegments + cpw->cpanel.numauxsegments),
	  (n - cpw->cpanel.numauxsegments) * sizeof(DisplaySegment));
  }
  cpw->cpanel.dispauxsegmentsize = n;
}

/*- freeDisplaySegments: free display segments -*/
static void
freeDisplaySegments(cpw)
AuxPanelWidget cpw;
{
  Cardinal i;
  DisplaySegment *dseg = cpw->cpanel.dispauxsegments;

  for (i = 0 ; i < cpw->cpanel.numauxsegments ; i++, dseg++) {
    XtFree(dseg->seg.data);
  }
  XtFree((char *)cpw->cpanel.dispauxsegments);
  cpw->cpanel.dispauxsegments = (DisplaySegment *)0;
  cpw->cpanel.dispauxsegmentsize = 0;
}

/*- copyString: copy ICString -*/
static void
copyString(from, to)
ICString *from;
ICString *to;
{
  XtFree(to->data);
  *to = *from;
  to->data = XtMalloc(to->nbytes);
  (void)bcopy(from->data, to->data, to->nbytes);
}

static void
UpdateAuxDisplay(cpw)
AuxPanelWidget cpw;
{
  Widget dispobj = cpw->cpanel.displayobj;
  DisplaySegment *dseg;
  int hspace, vspace;
  int x, y;
  Cardinal i;

  if (dispobj == NULL) return;

  hspace = cpw->cpanel.hspace;
  vspace = cpw->cpanel.vspace;

  x = hspace / 2;
  y = vspace / 2;

  for (i = 0, dseg = cpw->cpanel.dispauxsegments;
       i < cpw->cpanel.numauxsegments ; i++, dseg++) {
    if (dseg->redrawpos >= 0) {
      int offset = 0;
      int idx = dseg->redrawpos;

      if (dseg->width == 0) {
	dseg->width = CDStringWidth(dispobj, &dseg->seg, 0, -1);
      }
      if (idx > 0) {
	offset = CDStringWidth(dispobj, &dseg->seg, 0, idx);
      }
      CDDrawString(dispobj, (Widget)cpw, &dseg->seg, 0, -1, x + offset, y);
    }
    x += dseg->width;
  }
  return;
}

/*
 * Public Functions
 */

/* ARGSUSED */
void
APanelStart(w, segments, nseg, curseg, cursorpos)
Widget w;
ICString *segments;
Cardinal nseg, curseg, cursorpos;
{
    AuxPanelWidget cpw = (AuxPanelWidget)w;
    Cardinal i;
    DisplaySegment *dseg;

    cpw->cpanel.numauxsegments = nseg;
    allocDisplaySegments(cpw, nseg);
    for (i = 0, dseg = cpw->cpanel.dispauxsegments ; i < nseg ; i++, dseg++) {
      copyString(segments +i, &dseg->seg);
      dseg->redrawpos = 0;
      dseg->width = 0;
    }
    Layout(cpw, True, True);
    if (XtIsRealized(w)) XClearWindow(XtDisplay(w), XtWindow(w));
    return;
}

void
APanelEnd(w)
Widget w;
{
  AuxPanelWidget cpw = (AuxPanelWidget)w;

  cpw->cpanel.numauxsegments = 0;
  return;
}

/* ARGSUSED */
void
APanelChange(w, segments, nseg, curseg, cursorpos)
Widget w;
ICString *segments;
Cardinal nseg, curseg, cursorpos;
{
  AuxPanelWidget cpw = (AuxPanelWidget)w;
  Cardinal i;
  DisplaySegment *dseg;

  cpw->cpanel.numauxsegments = nseg;
  allocDisplaySegments(cpw, nseg);
  for (i = 0, dseg = cpw->cpanel.dispauxsegments ; i < nseg ; i++, dseg++) {
    /* 全書き換えになってしまう。ICCompareSegment が使えるなら
       使いたいのだが。 */
    copyString(segments +i, &dseg->seg);
    dseg->redrawpos = 0;
    dseg->width = 0;
  }
  Layout(cpw, False, False);
  if (XtIsRealized(w)) XClearWindow(XtDisplay(w), XtWindow(w));
  UpdateAuxDisplay(cpw);
  return;
}
