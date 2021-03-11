#ifndef lint
static char *rcsid = "$Id: OffConv.c,v 1.38 1994/06/06 01:00:54 ishisone Rel $";
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

/*
 * Note: This file contains TWO widget classes, OverTheSpotConversionWidget
 *       and its subclass SeparateConversionWidget.
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#if XtSpecificationRelease > 4
#include <X11/Xfuncs.h>
#endif
#include "CachedAtom.h"
#include "AsyncErr.h"
#include "OffConvP.h"
#include <X11/Xaw/Form.h>
#include "InputConv.h"
#include "ConvDisp.h"
#include "CandPanel.h"
#include "AuxPanel.h"
#include "Canvas.h"
#include "AdoptedShe.h"
#include "CanvasShel.h"
#include "ICLabel.h"

#define DEBUG_VAR debug_OffTheSpotConversion
#include "DebugPrint.h"

/*- resource table for OffTheSpotConversion (SeparateConversion has no resources -*/
static XtResource off_resources[] = {
#define offset(field) XtOffset(OffTheSpotConversionWidget, offthespot.field)
    { XtNleftMargin, XtCMargin, XtRDimension, sizeof(Dimension),
	offset(leftmargin), XtRImmediate, (XtPointer)2 },
    { XtNrightMargin, XtCMargin, XtRDimension, sizeof(Dimension),
	offset(rightmargin), XtRImmediate, (XtPointer)2 },
#undef offset
};

/*- default translations -- same as superclass's -*/
static char off_translations[] = "<Key>: to-inputobj()";
static char sep_translations[] = "<Key>: to-inputobj()";

/*- declarations of static functions -*/
static void OffTheSpot_Initialize();
static void OffTheSpot_Destroy();
static Boolean OffTheSpot_SetValues();
static void OffTheSpot_Startup();
static void OffTheSpot_ConversionFinish();
static void OffTheSpot_ChangeAttributes();
static void OffTheSpot_ChangeFocus();

static void Separate_Initialize();
static void Separate_Startup();
static void Separate_ConversionFinish();
static void Separate_ChangeAttributes();
static void Separate_ChangeFocus();

static Widget CreateDisplayObject();
static Widget CreateSelectionWidget();

static void SetupDisplayObject();
static Boolean ResetDisplayObject();
static void SetupModeWidget();
static Boolean ResetModeWidget();
static void SetupCanvasWidget();
static Boolean ResetCanvasWidget();

static void UpdateText();
static void UpdateMode();

static void SelectionControl();
static void SelectionStart();
static void LocateSelectionPopup();
static void SelectionEnd();
static void SelectionSet();
static void SelectionMove();

static Widget CreateAuxWidget();
static void AuxControl();
static void AuxStart();
static void AuxEnd();
static void AuxChange();
static void LocateAuxPopup();

static void TextRedisplay();
static void TextResize();

static void SelectionSelected();

static void computeDisplaySegments();
static void recomputeDisplaySegments();
static void computeLastPosition();
static DisplayFragment * computeDisplayFragments();
static int widthAvailable();
static void initialLocation();
static void nextLocation();
static DisplayLocation * findLocation();
static void reconfigureDisplay();
static void updateDisplay();
static void updateDisplaySegment();
static void redrawSegments();

static void eraseCursor();
static void showCursor();
static Boolean exposeCursor();
static void computeCursor();

static void MoveShell();
static Window getToplevelWindow();
static void setTransientFor();
static Boolean intersectRect();
static void unionRect();
static DisplayFragment * allocDisplayFragment();
static void freeDisplayFragments();
static void destroyDisplayFragments();
static void allocDisplaySegments();
static void freeDisplaySegment();
static void clearAllDisplaySegments();
static void copyString();
static void freeString();

/*- composite-extension rec: for enabling non-widget children -*/
static CompositeClassExtensionRec CompositeExtension = {
    /* next_extension		*/	NULL,
    /* record_type		*/	NULLQUARK,
    /* version			*/	XtCompositeExtensionVersion,
    /* record_size		*/	sizeof(CompositeClassExtensionRec),
    /* accept_objects		*/	True,
};

/*- offTheSpotConversionClass record -*/
OffTheSpotConversionClassRec offTheSpotConversionClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass)&conversionControlClassRec,
    /* class_name		*/	"OffTheSpotConversion",
    /* widget_size		*/	sizeof(OffTheSpotConversionRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	OffTheSpot_Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	off_resources,
    /* num_resources		*/	XtNumber(off_resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	OffTheSpot_Destroy,
    /* resize			*/	XtInheritResize,
    /* expose			*/	NULL,
    /* set_values		*/	OffTheSpot_SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	off_translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* composite fields */
    /* geometry_manager		*/	XtInheritGeometryManager,
    /* change_managed		*/	XtInheritChangeManaged,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	(XtPointer)&CompositeExtension,
  },
  { /* shell fields */
    /* extension		*/	NULL
  },
  { /* wm_shell fields */
    /* extension		*/	NULL
  },
  { /* vendor_shell fields */
    /* extension		*/	NULL
  },
  { /* transient_shell fields */
    /* extension		*/	NULL
  },
  { /* conversionControl fields */
    /* Startup			*/	OffTheSpot_Startup,
    /* Finish			*/	OffTheSpot_ConversionFinish,
    /* ChangeAttributes		*/	OffTheSpot_ChangeAttributes,
    /* ChangeFocus		*/	OffTheSpot_ChangeFocus,
    /* TextChange		*/	UpdateText,
    /* ModeChange		*/	UpdateMode,
    /* SelectionControl		*/	SelectionControl,
    /* AuxControl		*/	AuxControl,
  },
  { /* offTheSpotConversion fields */
    /* empty			*/	0
  },
};

WidgetClass offTheSpotConversionWidgetClass = (WidgetClass)&offTheSpotConversionClassRec;

/*- separateConversionClass record -*/
SeparateConversionClassRec separateConversionClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass)&offTheSpotConversionClassRec,
    /* class_name		*/	"SeparateConversion",
    /* widget_size		*/	sizeof(SeparateConversionRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Separate_Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	NULL,
    /* num_resources		*/	0,
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	XtInheritResize,
    /* expose			*/	NULL,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	sep_translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* composite fields */
    /* geometry_manager		*/	XtInheritGeometryManager,
    /* change_managed		*/	XtInheritChangeManaged,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	(XtPointer)&CompositeExtension,
  },
  { /* shell fields */
    /* extension		*/	NULL
  },
  { /* wm_shell fields */
    /* extension		*/	NULL
  },
  { /* vendor_shell fields */
    /* extension		*/	NULL
  },
  { /* transient_shell fields */
    /* extension		*/	NULL
  },
  { /* conversionControl fields */
    /* Startup			*/	Separate_Startup,
    /* Finish			*/	Separate_ConversionFinish,
    /* ChangeAttributes		*/	Separate_ChangeAttributes,
    /* ChangeFocus		*/	Separate_ChangeFocus,
    /* TextChange		*/	XtInheritTextChange,
    /* ModeChange		*/	XtInheritModeChange,
    /* SelectionControl		*/	XtInheritSelectionControl,
    /* AuxControl		*/	XtInheritAuxControl,
  },
  { /* offTheSpotConversion fields */
    /* empty			*/	0
  },
  { /* separateConversion fields */
    /* empty			*/	0
  },
};

WidgetClass separateConversionWidgetClass = (WidgetClass)&separateConversionClassRec;

/*
 *+ OffTheSpot -- Core class method
 */

/*- OffTheSpot_Initialize: initalize method for OffTheSpotCoversion -*/
/* ARGSUSED */
static void
OffTheSpot_Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)new;

    (void)CreateDisplayObject(ocw);
    (void)CreateSelectionWidget(ocw);
    (void)CreateAuxWidget(ocw);

    ocw->offthespot.canvaswidget = NULL;
    ocw->offthespot.modewidget = NULL;
    ocw->offthespot.modeshell = NULL;

    ocw->offthespot.dispsegments = NULL;
    ocw->offthespot.numsegments = 0;
    ocw->offthespot.dispsegmentsize = 0;
    ocw->offthespot.candlist = NULL;
    ocw->offthespot.numcands = 0;
    ocw->offthespot.selectionpoppedup = False;
    ocw->offthespot.auxpoppedup = False;
}

/*- OffTheSpot_Destroy: destroy method for OffTheSpotCoversion -*/
static void
OffTheSpot_Destroy(w)
Widget w;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;

    /* ディスプレイセグメントの領域を解放 */
    if (ocw->offthespot.dispsegments) {
	DisplaySegment *dsp = ocw->offthespot.dispsegments;
	int i;

	for (i = 0; i < ocw->offthespot.numsegments; i++) {
	    freeString(&dsp[i].seg);
	    destroyDisplayFragments(dsp->fragments);
	}
	XtFree((char *)dsp);
    }
}

/*- OffTheSpot_SetValues: setvalues method for OffTheSpotCoversion -*/
/* ARGSUSED */
static Boolean
OffTheSpot_SetValues(cur, req, new, args, num_args)
Widget cur;
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    /* OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)new; */
    return False;
}

/*
 *+ OffTheSpot -- ConversionControl class method
 */

/*- OffTheSpot_Startup: OffTheSpot conversion startup -*/
static void
OffTheSpot_Startup(widget, valuemask, value)
Widget widget;
unsigned long valuemask;
ConversionAttributes *value;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)widget;
    Window toplevel;

    if (!(valuemask & CAStatusArea)) {
	String params[1];
	Cardinal num_params = 1;

	params[0] = XtClass(widget)->core_class.class_name;
	XtAppErrorMsg(XtWidgetToApplicationContext(widget),
		      "conversionAttributeError", "statusArea", "WidgetError",
		      "%s: status area must be specified",
		      params, &num_params);
    }

    SetupDisplayObject(ocw, valuemask, value);
    SetupModeWidget(ocw, valuemask, value);
    SetupCanvasWidget(ocw, valuemask, value);

    /* 内部のバッファをクリアする */
    clearAllDisplaySegments(ocw);

    /* カーソルの設定 */
    ocw->offthespot.cursorvisible = True;
    eraseCursor(ocw);
    ocw->offthespot.cursorvisible = True;
    initialLocation(ocw, &(ocw->offthespot.cursorlocation));
    ocw->offthespot.cursorlocation.y += ocw->offthespot.ascent;

    /* WM_TRANSIENT_FOR プロパティを正しくセットする */
    toplevel = getToplevelWindow(XtDisplay(widget),
				 ocw->ccontrol.clientwindow);
    setTransientFor(ocw->offthespot.selectionshell, toplevel);
    setTransientFor(ocw->offthespot.auxshell, toplevel);

    /*
     * OffTheSpotConvesion の widget 自体はポップアップさせないが、
     * バックエンドタイプの時にはクライアントがこの widget の
     * ウィンドウに対してイベントを送るので Realize だけしておく
     */
    if (!XtIsRealized(widget)) {
	Arg args[2];

	XtSetArg(args[0], XtNwidth, 1);
	XtSetArg(args[1], XtNheight, 1);
	XtSetValues(widget, args, 2);
	XtRealizeWidget(widget);
    }

    /* ポップアップする */
    XtPopup(ocw->offthespot.modeshell, XtGrabNone);
    XtPopup(ocw->offthespot.canvaswidget, XtGrabNone);
}

/*- OffTheSpot_ConversionFinish: OffTheSpot conversion finish -*/
/* ARGSUSED */
static void
OffTheSpot_ConversionFinish(w)
Widget w;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;
    XAEHandle h;

    /* Popdown and unrealize textcanvas and mode widget
     *	we must be careful here. if clientwindow are destroyed,
     *	the text canvas and mode widget are also destroyed.
     *	we have to popdown and unrealize those widgets, but if
     *	they are destroyed, BadWindow error will be generated.
     *	so we must set own error handler that ignores errors.
     */
    h = XAESetIgnoreErrors(XtDisplay(w));
    XtPopdown(ocw->offthespot.modeshell);
    XtUnrealizeWidget(ocw->offthespot.modeshell);
    XtPopdown(ocw->offthespot.canvaswidget);
    XtUnrealizeWidget(ocw->offthespot.canvaswidget);
    XAEUnset(h);

    if (ocw->offthespot.selectionpoppedup) {
	XtPopdown(ocw->offthespot.selectionshell);
	ocw->offthespot.selectionpoppedup = False;
    }
    if (ocw->offthespot.auxpoppedup) {
	XtPopdown(ocw->offthespot.auxshell);
	ocw->offthespot.auxpoppedup = False;
    }
}

/*- OffTheSpot_ChangeAttributes: OffTheSpot conversion attribute change -*/
/* ARGSUSED */
static void
OffTheSpot_ChangeAttributes(w, valuemask, value)
Widget w;
unsigned long valuemask;
ConversionAttributes *value;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;
    Boolean redraw, reconfig;

    if (ResetModeWidget(ocw, valuemask, value) &&
	XtIsRealized(ocw->offthespot.modewidget)) {
	XClearArea(XtDisplay(w), XtWindow((Widget)ocw->offthespot.modewidget),
		   0, 0, 0, 0, True);
    }

    redraw = ResetDisplayObject(ocw, valuemask, value);
    reconfig = ResetCanvasWidget(ocw, valuemask, value);

    if (reconfig) {
	TextResize(ocw->offthespot.canvaswidget, (XtPointer)w, (XtPointer)NULL);
    } else if (redraw && XtIsRealized(ocw->offthespot.canvaswidget)) {
	XClearArea(XtDisplay(w), XtWindow(ocw->offthespot.canvaswidget),
		   0, 0, 0, 0, True);
    }
}

/*- OffTheSpot_ChangeFocus: OffTheSpot focus change -*/
static void
OffTheSpot_ChangeFocus(w, set)
Widget w;
int set;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;

    if (set) {
	XtPopup(ocw->offthespot.modeshell, XtGrabNone);
	XtPopup(ocw->offthespot.canvaswidget, XtGrabNone);
    } else {
	XtPopdown(ocw->offthespot.modeshell);
	XtPopdown(ocw->offthespot.canvaswidget);
    }
}

/*
 *+ Separate -- Core class method
 */

/*- Separate_Initialize: initialize method for SeparateConversion -*/
/* ARGSUSED */
static void
Separate_Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    SeparateConversionWidget scw = (SeparateConversionWidget)new;
    Widget inputobj = scw->ccontrol.inputobj;
    Widget form, canvas, mode, tmp[2];

    form = XtCreateManagedWidget("form", formWidgetClass, (Widget)scw,
				 NULL, 0);
    tmp[0] = mode = XtVaCreateWidget("mode", icLabelWidgetClass, form,
				     XtNlabel, ICGetMode(inputobj),
				     NULL);
    (void)XtCreateWidget("display", scw->ccontrol.displayobjclass, mode,
			 NULL, 0);

    tmp[1] = canvas = XtCreateWidget("text", canvasWidgetClass, form, NULL, 0);
    XtManageChildren(tmp, 2);

    XtAddCallback(canvas, XtNexposeCallback, TextRedisplay, (XtPointer)scw);
    XtAddCallback(canvas, XtNresizeCallback, TextResize, (XtPointer)scw);

    XtInstallAccelerators(canvas, (Widget)scw);
    XtInstallAccelerators(mode, (Widget)scw);
    XtInstallAccelerators(form, (Widget)scw);

    scw->separate.formwidget = form;
    scw->offthespot.canvaswidget = canvas;
    scw->offthespot.modewidget = mode;

}

/*
 *+ Separate -- ConversionControl class method
 */

/*- Separate_Startup: Separate conversion startup -*/
/* ARGSUSED */
static void
Separate_Startup(widget, valuemask, value)
Widget widget;
unsigned long valuemask;
ConversionAttributes *value;
{
    SeparateConversionWidget scw = (SeparateConversionWidget)widget;
    Position clx, cly;
    Dimension clw, clh;
    Position x, y;
    Dimension w, h;
    Dimension dpyWidth, dpyHeight;
    Window toplevel;

    /* 内部のバッファをクリアする */
    clearAllDisplaySegments((OffTheSpotConversionWidget)scw);

    /* カーソルの設定 */
    scw->offthespot.cursorvisible = True;
    eraseCursor((OffTheSpotConversionWidget)scw);
    scw->offthespot.cursorvisible = True;
    initialLocation((OffTheSpotConversionWidget)scw,
		    &(scw->offthespot.cursorlocation));
    scw->offthespot.cursorlocation.y += scw->offthespot.ascent;

    /* 初期モードを設定する */
    XtVaSetValues(scw->offthespot.modewidget,
		  XtNlabel, ICGetMode(scw->ccontrol.inputobj),
		  NULL);

    /* 表示位置を決める */
    clx = scw->ccontrol.client_rootx;
    cly = scw->ccontrol.client_rooty;
    clw = scw->ccontrol.client_attr.width;
    clh = scw->ccontrol.client_attr.height;

    XtRealizeWidget((Widget)scw);
    w =  scw->core.width;
    h =  scw->core.height;
    DPRINT(("Off_the_spot_Startup(): widget size = %dx%d\n", w, h));
    x =  clx + clw / 2 - w / 2;
    y =  cly + clh + 8;

    dpyWidth = WidthOfScreen(XtScreen(widget));
    dpyHeight = HeightOfScreen(XtScreen(widget));

    if (x + w > dpyWidth) x = dpyWidth - w;
    if (x < 0) x = 0;
    if (y + h > dpyHeight) {
	y = cly - h;
	if (y < 0) y = dpyHeight - h;
    }
    MoveShell(widget, x, y);

    /* WM_TRANSIENT_FOR プロパティを正しくセットする */
    toplevel = getToplevelWindow(XtDisplay(widget),
				 scw->ccontrol.clientwindow);
    setTransientFor((Widget)scw, toplevel);

    /*
     * This is a kind of a magic word... I don't know why, but without this
     * the selection popup will appear in wrong size (1x1) at the first time.
     */
    XtRealizeWidget(scw->offthespot.selectionshell);
    XtRealizeWidget(scw->offthespot.auxshell);

    /* ポップアップする */
    XtPopup(widget, XtGrabNone);
}

/*- Separate_ConversionFinish: Separate conversion finish -*/
/* ARGSUSED */
static void
Separate_ConversionFinish(w)
Widget w;
{
    SeparateConversionWidget scw = (SeparateConversionWidget)w;

    if (scw->offthespot.selectionpoppedup) {
	XtPopdown(scw->offthespot.selectionshell);
	scw->offthespot.selectionpoppedup = False;
    }
    if (scw->offthespot.auxpoppedup) {
	XtPopdown(scw->offthespot.auxshell);
	scw->offthespot.auxpoppedup = False;
    }
    XtPopdown(w);
}

/*- Separate_ChangeAttributes: Separate conversion attribute change -*/
/* ARGSUSED */
static void
Separate_ChangeAttributes(w, mask, value)
Widget w;
unsigned long mask;
ConversionAttributes *value;
{
    /* do nothing */
}

/*- Separate_ChangeFocus: Separate focus change -*/
/* ARGSUSED */
static void
Separate_ChangeFocus(w, set)
Widget w;
int set;
{
    /* do nothing */
}

/*
 *+ sub-widget creation
 */

/*- CreateDisplayObject: create display object for text drawing -*/
static Widget
CreateDisplayObject(ocw)
OffTheSpotConversionWidget ocw;
{
    Widget dispobj;

    dispobj = XtCreateWidget("displayObj", ocw->ccontrol.displayobjclass,
			     (Widget)ocw, NULL, 0);

    ocw->offthespot.displayobj = dispobj;
    ocw->offthespot.lineheight = CDLineHeight(dispobj, &ocw->offthespot.ascent);

    return dispobj;
}

/*- CreateSelectionWidget: create selection widget for selecting candidates -*/
static Widget
CreateSelectionWidget(ocw)
OffTheSpotConversionWidget ocw;
{
    Widget shell, sel;

    /* set width/height so that XtRealizeWidget() doesn't cause error */
    shell = XtVaCreatePopupShell("selectionShell",
				 transientShellWidgetClass,
				 (Widget)ocw,
				 XtNwidth, 1,
				 XtNheight, 1,
				 NULL);
    ocw->offthespot.selectionshell = shell;

    sel = XtCreateManagedWidget("selection", candidatePanelWidgetClass,
				shell, NULL, 0);
    (void)XtCreateWidget("display", ocw->ccontrol.displayobjclass, sel,
			 NULL, 0);
    XtAddCallback(sel, XtNcallback, SelectionSelected, (XtPointer)ocw);
    XtInstallAccelerators(sel, (Widget)ocw);

    ocw->offthespot.selectionwidget = sel;

    return shell;
}

/*- CreateAuxWidget: create aux widget for display auxiliary data -*/
static Widget
CreateAuxWidget(ocw)
OffTheSpotConversionWidget ocw;
{
    Widget shell, sel;

    /* set width/height so that XtRealizeWidget() doesn't cause error */
    shell = XtVaCreatePopupShell("auxShell",
				 transientShellWidgetClass,
				 (Widget)ocw,
				 XtNwidth, 1,
				 XtNheight, 1,
				 XtNallowShellResize, True,
				 NULL);
    ocw->offthespot.auxshell = shell;

    sel = XtCreateManagedWidget("aux", auxPanelWidgetClass,
				shell, NULL, 0);
    (void)XtCreateWidget("display", ocw->ccontrol.displayobjclass, sel,
			 NULL, 0);
    XtAddCallback(sel, XtNcallback, SelectionSelected, (XtPointer)ocw);
    XtInstallAccelerators(sel, (Widget)ocw);

    ocw->offthespot.auxwidget = sel;

    return shell;
}

/*
 *+ subwidget configuration
 */

/*- SetupDisplayObject: do display objetct configuration on conversion startup -*/
static void
SetupDisplayObject(ocw, mask, value)
OffTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    /*
     * order is important. we must set fonts BEFORE anything else,
     * because it is possible that the fonts previously set in the
     * display object no longer exist, and if so, that causes BadFont
     * error when changing GCs.
     */

    if (mask & CAFonts) {
	CDSetFonts(ocw->offthespot.displayobj,
		   value->fonts, value->num_fonts);
    } else {
	CDSetFonts(ocw->offthespot.displayobj, (XFontStruct **)NULL, 0);
    }
    if (mask & CAColor) {
	XtVaSetValues(ocw->offthespot.displayobj,
		      XtNforeground, value->foreground,
		      XtNbackground, value->background,
		      NULL);
    }

    ocw->offthespot.lineheight = CDLineHeight(ocw->offthespot.displayobj,
					      &ocw->offthespot.ascent);
}

/*- ResetDisplayObject: do display objetct reconfiguration on attribute change -*/
static Boolean
ResetDisplayObject(ocw, mask, value)
OffTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Boolean redraw = False;

    if (mask & CAColor) {
	XtVaSetValues(ocw->offthespot.displayobj,
		      XtNforeground, value->foreground,
		      XtNbackground, value->background,
		      NULL);
	redraw = True;
    }
    if (mask & CAFonts) {
	CDSetFonts(ocw->offthespot.displayobj,
		   value->fonts, value->num_fonts);
	redraw = True;
	ocw->offthespot.lineheight = CDLineHeight(ocw->offthespot.displayobj,
						  &ocw->offthespot.ascent);
    }
    return redraw;
}

/*- SetupModeWidget: do mode widget configuration (OffTheSpot only) -*/
static void
SetupModeWidget(ocw, mask, value)
OffTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Window clwin = ocw->ccontrol.clientwindow;
    Widget inputobj = ocw->ccontrol.inputobj;
    Arg shellarg[10], modearg[10], objarg[10];
    Cardinal i = 0, j = 0, k = 0;

    XtSetArg(shellarg[i], XtNparentWindow, clwin); i++;
    XtSetArg(shellarg[i], XtNx, value->statusarea.x); i++;
    XtSetArg(shellarg[i], XtNy, value->statusarea.y); i++;
    XtSetArg(shellarg[i], XtNwidth, value->statusarea.width); i++;
    XtSetArg(shellarg[i], XtNheight, value->statusarea.height); i++;

    XtSetArg(modearg[j], XtNlabel, ICGetMode(inputobj)); j++;

    if (mask & CAColormap) {
	XtSetArg(modearg[j], XtNcolormap, value->colormap); j++;
    } else {
	XtSetArg(modearg[j], XtNcolormap,
		 DefaultColormapOfScreen(XtScreen((Widget)ocw))); j++;
    }
    if (mask & CAColor) {
	XtSetArg(modearg[j], XtNbackground, value->background); j++;
	XtSetArg(objarg[k], XtNforeground, value->foreground); k++;
	XtSetArg(objarg[k], XtNbackground, value->background); k++;
    } else {
	XtSetArg(modearg[j], XtNbackground, ocw->core.background_pixel); j++;
    }

    if (ocw->offthespot.modewidget == NULL) {
	Widget shell, mode, disp;
	shell = XtCreatePopupShell("modeShell", adoptedShellWidgetClass,
				   (Widget)ocw, shellarg, i);
	mode = XtCreateManagedWidget("mode", icLabelWidgetClass, shell,
				     modearg, j);
	disp = XtCreateWidget("display", ocw->ccontrol.displayobjclass, mode,
			      objarg, k);
	if (mask & CAFonts) CDSetFonts(disp, value->fonts, value->num_fonts);

	ocw->offthespot.modeshell = shell;
	ocw->offthespot.modewidget = mode;
	ocw->offthespot.modedisplayobj = disp;
	XtInstallAccelerators(mode, (Widget)ocw);
    } else {
	if (mask & CAFonts) {
	    CDSetFonts(ocw->offthespot.modedisplayobj,
		       value->fonts, value->num_fonts);
	} else {
	    CDSetFonts(ocw->offthespot.modedisplayobj,
		       (XFontStruct **)NULL, 0);
	}
	XtSetValues(ocw->offthespot.modeshell, shellarg, i);
	XtSetValues(ocw->offthespot.modewidget, modearg, j);
	XtSetValues(ocw->offthespot.modedisplayobj, objarg, k);
    }

    ICLRecomputeSize(ocw->offthespot.modewidget);
}

/*- ResetModeWidget: do mode widget reconfiguration (OffTheSpot only) -*/
static Boolean
ResetModeWidget(ocw, mask, value)
OffTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Arg shellarg[10], modearg[10], objarg[10];
    Cardinal i = 0, j = 0, k = 0;
    Boolean redraw = False;

    if (mask & CAStatusArea) {
	XtSetArg(shellarg[i], XtNx, value->statusarea.x); i++;
	XtSetArg(shellarg[i], XtNy, value->statusarea.y); i++;
	XtSetArg(shellarg[i], XtNwidth, value->statusarea.width); i++;
	XtSetArg(shellarg[i], XtNheight, value->statusarea.height); i++;
    }

    if (mask & CAColormap) {
	XtSetArg(modearg[j], XtNcolormap, value->colormap); j++;
	redraw = True;
    }
    if (mask & CAColor) {
	XtSetArg(modearg[j], XtNbackground, value->background); j++;
	XtSetArg(objarg[k], XtNforeground, value->foreground); k++;
	XtSetArg(objarg[k], XtNbackground, value->background); k++;
	redraw = True;
    }
    XtSetValues(ocw->offthespot.modeshell, shellarg, i);
    XtSetValues(ocw->offthespot.modewidget, modearg, j);
    XtSetValues(ocw->offthespot.modedisplayobj, objarg, k);

    if (mask & CAFonts) {
	CDSetFonts(ocw->offthespot.modedisplayobj,
		   value->fonts, value->num_fonts);
	ICLRecomputeSize(ocw->offthespot.modewidget);
	redraw = True;
    }

    return redraw;
}

/*- SetupCanvasWidget: do text canvas configuration on conversion startup -*/
static void
SetupCanvasWidget(ocw, mask, value)
OffTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Window clwin = ocw->ccontrol.clientwindow;
    Arg arg[10];
    Cardinal i = 0;

    XtSetArg(arg[i], XtNparentWindow, clwin); i++;

    if (mask & CAClientArea) {
	XtSetArg(arg[i], XtNx, value->clientarea.x); i++;
	XtSetArg(arg[i], XtNy, value->clientarea.y); i++;
	XtSetArg(arg[i], XtNwidth, value->clientarea.width); i++;
	XtSetArg(arg[i], XtNheight, value->clientarea.height); i++;
    } else {
	XtSetArg(arg[i], XtNx, 0); i++;
	XtSetArg(arg[i], XtNy, 0); i++;
	XtSetArg(arg[i], XtNwidth, ocw->ccontrol.client_attr.width); i++;
	XtSetArg(arg[i], XtNheight, ocw->ccontrol.client_attr.height); i++;
    }

    /* if (mask & CALineSpacing) ... */
    if (mask & CAColormap) {
	XtSetArg(arg[i], XtNcolormap, value->colormap); i++;
    } else {
	XtSetArg(arg[i], XtNcolormap,
		 DefaultColormapOfScreen(XtScreen((Widget)ocw))); i++;
    }
    if (mask & CAColor) {
	XtSetArg(arg[i], XtNbackground, value->background); i++;
    } else {
	XtSetArg(arg[i], XtNbackground, ocw->core.background_pixel); i++;
    }
    if (mask & CACursor) {
	XtSetArg(arg[i], XtNcursor, value->cursor); i++;
    } else {
	XtSetArg(arg[i], XtNcursor, None); i++;
    }

    if (ocw->offthespot.canvaswidget == NULL) {
	Widget canvas;
	canvas = XtCreatePopupShell("text", canvasShellWidgetClass,
				    (Widget)ocw, arg, i);
	XtAddCallback(canvas, XtNexposeCallback, TextRedisplay, (XtPointer)ocw);
	XtAddCallback(canvas, XtNresizeCallback, TextResize, (XtPointer)ocw);
	XtInstallAccelerators(canvas, (Widget)ocw);
	ocw->offthespot.canvaswidget = canvas;
    } else {
	XtSetValues(ocw->offthespot.canvaswidget, arg, i);
    }
}

/*- ResetCanvasWidget: do text canvas reconfiguration on attribute change (OfftheSpot only) -*/
static Boolean
ResetCanvasWidget(ocw, mask, value)
OffTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Arg arg[10];
    Cardinal i = 0;
    Boolean redraw = False;

    if (mask & CAClientArea) {
	XtSetArg(arg[i], XtNx, value->clientarea.x); i++;
	XtSetArg(arg[i], XtNy, value->clientarea.y); i++;
	XtSetArg(arg[i], XtNwidth, value->clientarea.width); i++;
	XtSetArg(arg[i], XtNheight, value->clientarea.height); i++;
	redraw = True;
    }

    if (mask & CAColormap) {
	XtSetArg(arg[i], XtNcolormap, value->colormap); i++;
	redraw = True;
    }
    if (mask & CAColor) {
	XtSetArg(arg[i], XtNbackground, value->background); i++;
	redraw = True;
    }
    if (mask & CACursor) {
	XtSetArg(arg[i], XtNcursor, value->cursor); i++;
    }

    XtSetValues(ocw->offthespot.canvaswidget, arg, i);

    return redraw;
}

static void
UpdateText(w)
Widget w;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;

    TRACE(("OffTheSpotConversion:UpdateText()\n"));
    eraseCursor(ocw);
    computeDisplaySegments(ocw);
    computeCursor(ocw);
    reconfigureDisplay(ocw);
    updateDisplay(ocw);
    showCursor(ocw);
}

static void
UpdateMode(w)
Widget w;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;

    TRACE(("OffTheSpotConversion:UpdateMode()\n"));
    XtVaSetValues(ocw->offthespot.modewidget,
		  XtNlabel, ICGetMode(ocw->ccontrol.inputobj),
		  NULL);
}

static void
SelectionControl(w, arg)
Widget w;
ICSelectionControlArg *arg;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;
    String params[1];
    Cardinal num_params;

    switch (arg->command) {
    case ICSelectionStart:
	SelectionStart(ocw, arg->u.selection_kind);
	break;
    case ICSelectionEnd:
	SelectionEnd(ocw, &arg->u.current_item);
	break;
    case ICSelectionSet:
	SelectionSet(ocw, arg->u.current_item);
	break;
    case ICSelectionMove:
	SelectionMove(ocw, arg->u.dir);
	break;
    default:
	params[0] = XtClass(w)->core_class.class_name;
	num_params = 1;
	XtAppWarningMsg(XtWidgetToApplicationContext(w),
			"parameterError", "SelectionControl", "WidgetError",
			"%s: unknown selection control command",
			params, &num_params);
	break;
    }
}

/* ARGSUSED */
static void
SelectionStart(ocw, kind)
OffTheSpotConversionWidget ocw;
int kind;
{
    Cardinal ncand;

    TRACE(("OffTheSpotConversion:SelectionStart()\n"));
    if (ocw->offthespot.selectionpoppedup) {
	TRACE(("\tselection already started -- ignored\n"));
	return;
    }

    ocw->offthespot.candlist = ICGetItemList(ocw->ccontrol.inputobj, &ncand);
    ocw->offthespot.numcands = ncand;

    TRACE(("\tnumcands=%d\n", ocw->offthespot.numcands));
    CPanelSetList(ocw->offthespot.selectionwidget,
		  ocw->offthespot.candlist,
		  ocw->offthespot.numcands, 0, True);

    /* ポップアップする場所を決める */
    LocateSelectionPopup(ocw);

    XtPopup(ocw->offthespot.selectionshell, XtGrabNone);
    ocw->offthespot.selectionpoppedup = True;
}

static void
LocateSelectionPopup(ocw)
OffTheSpotConversionWidget ocw;
{
    Position x, y;
    DisplayLocation lastp;
    Dimension dpyWidth, dpyHeight;
    Widget canvas = ocw->offthespot.canvaswidget;
    Widget panel = ocw->offthespot.selectionwidget;
    Widget shell = ocw->offthespot.selectionshell;
    int clx, cly;
    Window junk;

    if (ocw->offthespot.numsegments > 0) {
	computeLastPosition(ocw->offthespot.dispsegments[ocw->offthespot.numsegments - 1].fragments, &lastp);
    } else {
	lastp.y = 0;
    }
    lastp.x = canvas->core.width / 2;
    lastp.y += ocw->offthespot.lineheight;
    (void)XTranslateCoordinates(XtDisplay(canvas), XtWindow(canvas),
				RootWindowOfScreen(XtScreen(canvas)),
				0, 0, &clx, &cly, &junk);

    x = clx + lastp.x - panel->core.width / 2;
    y = cly + lastp.y + 8;	/* XXX */

    dpyWidth = WidthOfScreen(XtScreen(canvas));
    dpyHeight = HeightOfScreen(XtScreen(canvas));

    if (x + panel->core.width > dpyWidth) x = dpyWidth - panel->core.width;
    if (x < 0) x = 0;
    if (y + panel->core.height > dpyHeight) {
	y = cly - panel->core.height - 8;	/* XXX */
	if (y < 0) y = dpyHeight - panel->core.height - 4; /* XXX */
    }
    MoveShell(shell, x, y);
}

static void
SelectionEnd(ocw, current)
OffTheSpotConversionWidget ocw;
int *current;
{
    TRACE(("OverTheSpotConversion:SelectionEnd()\n"));
    if (!ocw->offthespot.selectionpoppedup) {	/* for safe */
	TRACE(("\tnot in selection mode -- ignored\n"));
	return;
    }

    XtVaGetValues(ocw->offthespot.selectionwidget,
		  XtNcurrentItem, current,
		  NULL);

    XtPopdown(ocw->offthespot.selectionshell);

    ocw->offthespot.selectionpoppedup = False;
}

static void
SelectionSet(ocw, current)
OffTheSpotConversionWidget ocw;
int current;
{
    TRACE(("OverTheSpotConversion:SelectionSet()\n"));
    if (!ocw->offthespot.selectionpoppedup) {	/* for safe */
	TRACE(("\tnot in selection mode -- ignored\n"));
	return;
    }

    XtVaSetValues(ocw->offthespot.selectionwidget,
		  XtNcurrentItem, current,
		  NULL);
}

static void
SelectionMove(ocw, dir)
OffTheSpotConversionWidget ocw;
int dir;
{
    TRACE(("OverTheSpotConversion:SelectionMove()\n"));
    if (!ocw->offthespot.selectionpoppedup) {	/* for safe */
	TRACE(("\tnot in selection mode -- ignored\n"));
	return;
    }

    CPanelMoveCurrent(ocw->offthespot.selectionwidget, dir);
}

/*
 * Aux Callback
 */

static void
AuxControl(w, arg)
Widget w;
ICAuxControlArg *arg;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)w;
    String params[1];
    Cardinal num_params;

    switch (arg->command) {
    case ICAuxStart:
	AuxStart(ocw);
	break;
    case ICAuxEnd:
	AuxEnd(ocw);
	break;
    case ICAuxChange:
	AuxChange(ocw);
	break;
    default:
	params[0] = XtClass(w)->core_class.class_name;
	num_params = 1;
	XtAppWarningMsg(XtWidgetToApplicationContext(w),
			"parameterError", "AuxControl", "WidgetError",
			"%s: unknown aux control command",
			params, &num_params);
	break;
    }
}

/* ARGSUSED */
static void
AuxStart(ocw)
OffTheSpotConversionWidget ocw;
{
  ICString *auxstr;
  Cardinal ncand, curseg, cursorpos;
  
  if (ocw->offthespot.auxpoppedup) return;
  
  /* テキストコールバックの時のような処理をする
     のは AuxPanel.c にまかせよう */

  auxstr = ICGetAuxSegments(ocw->ccontrol.inputobj,
			    &ncand, &curseg, &cursorpos);

  APanelStart(ocw->offthespot.auxwidget, auxstr, ncand, curseg, cursorpos);
    
  /* ポップアップする場所を決める */
  LocateAuxPopup(ocw);

  XtPopup(ocw->offthespot.auxshell, XtGrabNone);
  ocw->offthespot.auxpoppedup = True;
}

/* ARGSUSED */
static void
AuxEnd(ocw)
OffTheSpotConversionWidget ocw;
{
  if (!ocw->offthespot.auxpoppedup) return;	/* for safe */

/*  APanelEnd(ocw->offthespot.auxwidget); */

  XtPopdown(ocw->offthespot.auxshell);

  ocw->offthespot.auxpoppedup = False;
}

/* ARGSUSED */
static void
AuxChange(ocw)
OffTheSpotConversionWidget ocw;
{
  Cardinal ncand, curseg, cursorpos;
  ICString *auxstr;

  if (!ocw->offthespot.auxpoppedup) return;	/* for safe */

  auxstr = ICGetAuxSegments(ocw->ccontrol.inputobj,
			    &ncand, &curseg, &cursorpos);

  APanelChange(ocw->offthespot.auxwidget, auxstr, ncand, curseg, cursorpos);
}

static void
LocateAuxPopup(ocw)
OffTheSpotConversionWidget ocw;
{
    Position x, y;
    DisplayLocation lastp;
    Dimension dpyWidth, dpyHeight;
    Widget canvas = ocw->offthespot.canvaswidget;
    Widget panel = ocw->offthespot.auxwidget;
    Widget shell = ocw->offthespot.auxshell;
    int clx, cly;
    Window junk;

    if (ocw->offthespot.numsegments > 0) {
	computeLastPosition(ocw->offthespot.dispsegments[ocw->offthespot.numsegments - 1].fragments, &lastp);
    } else {
	lastp.y = 0;
    }
    lastp.x = canvas->core.width / 2;
    lastp.y += ocw->offthespot.lineheight;
    (void)XTranslateCoordinates(XtDisplay(canvas), XtWindow(canvas),
				RootWindowOfScreen(XtScreen(canvas)),
				0, 0, &clx, &cly, &junk);

    x = clx + lastp.x - panel->core.width / 2;
    y = cly + lastp.y + 8;	/* XXX */

    dpyWidth = WidthOfScreen(XtScreen(canvas));
    dpyHeight = HeightOfScreen(XtScreen(canvas));

    if (x + panel->core.width > dpyWidth) x = dpyWidth - panel->core.width;
    if (x < 0) x = 0;
    if (y + panel->core.height > dpyHeight) {
	y = cly - panel->core.height - 8;	/* XXX */
	if (y < 0) y = dpyHeight - panel->core.height - 4; /* XXX */
    }
    MoveShell(shell, x, y);
}


/*
 *+ TextCanvas callback
 */

/*- TextRedisplay: redraw text canvas -*/
static void
TextRedisplay(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)client_data;
    XExposeEvent *event = (XExposeEvent *)call_data;
    XRectangle region;
    Boolean cursorredraw;

    TRACE(("OffTheSpotConversion:TextRedisplay()\n"));
    region.x = event->x;
    region.y = event->y;
    region.width = event->width;
    region.height = event->height;

    cursorredraw = exposeCursor(ocw, w, &region);
    redrawSegments(ocw, &region);
    if (cursorredraw) showCursor(ocw);
}

/*- TextResize: do reconfiguration (and redraw) of text canvas when resized -*/
/* ARGSUSED */
static void
TextResize(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;	/* unused */
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)client_data;

    TRACE(("OffTheSpotConversion:TextResize()\n"));
    recomputeDisplaySegments(ocw);
    computeCursor(ocw);
    if (XtIsRealized(w)) {
	/* redraw it */
	XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, True);
    }
}


/*
 *+ Selection Widget callback
 */

/*- SelectionSelected: selection selected callback -*/
/* ARGSUSED */
static void
SelectionSelected(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    OffTheSpotConversionWidget ocw = (OffTheSpotConversionWidget)client_data;
    int current = (int)call_data;

    TRACE(("OffTheSpotConversion:SelectionSelected()\n"));
    XtPopdown(ocw->offthespot.selectionshell);
    ocw->offthespot.selectionpoppedup = False;
    ICSelectItem(ocw->ccontrol.inputobj, current);
}


/*
 *+ text drawing functions
 */

/*- computeDisplaySegments: compare old&new text and update segments/fragments -*/
static void
computeDisplaySegments(ocw)
OffTheSpotConversionWidget ocw;
{
    Widget inputobj = ocw->ccontrol.inputobj;
    Cardinal nnew = ICNumSegments(inputobj);
    Cardinal nold = ocw->offthespot.numsegments;
    ICString *newseg;
    DisplaySegment *dseg;
    DisplayLocation disploc;
    Cardinal nsame;
    int diff;
    Cardinal i;

    TRACE(("OffTheSpotConversion:computeDisplaySegments() nnew=%d\n", nnew));
    allocDisplaySegments(ocw, nnew);

    initialLocation(ocw, &disploc);

    for (i = 0, dseg = ocw->offthespot.dispsegments; i < nnew; i++, dseg++) {
	newseg = ICGetSegment(ocw->ccontrol.inputobj, i);
	if (i >= nold) {
	    copyString(newseg, &dseg->seg);
	    dseg->redrawpos = 0;
	    dseg->fragments = computeDisplayFragments(ocw, newseg, &disploc);
	} else {
	    DisplayFragment *oldfragments, *newfragments;

	    dseg->redrawpos = -1;
	    diff = ICCompareSegment(inputobj, newseg, &dseg->seg, &nsame);
	    if (diff != ICSame ||
		disploc.x != dseg->fragments->region.x ||
		disploc.y != dseg->fragments->region.y) {
		oldfragments = dseg->fragments;
		newfragments = computeDisplayFragments(ocw, newseg, &disploc);
		dseg->fragments = newfragments;
	    } else {
		oldfragments = NULL;
		newfragments = dseg->fragments;
		computeLastPosition(newfragments, &disploc);
	    }

	    switch (diff) {
	    case ICSame:
		if (oldfragments == NULL ||
		    oldfragments->region.x == newfragments->region.x &&
		    oldfragments->region.y == newfragments->region.y) {
		    dseg->redrawpos = -1;
		} else {
		    dseg->redrawpos = 0;
		}
		break;
	    case ICAttrChanged:
		dseg->redrawpos = 0;
		dseg->seg.attr = newseg->attr;
		break;
	    case ICStringChanged:
		if (oldfragments == NULL ||
		    oldfragments->region.x == newfragments->region.x &&
		    oldfragments->region.y == newfragments->region.y) {
		    dseg->redrawpos = nsame;
		} else {
		    dseg->redrawpos = 0;
		}
		freeString(&dseg->seg);
		copyString(newseg, &dseg->seg);
		break;
	    default:
		dseg->redrawpos = 0;
		freeString(&dseg->seg);
		copyString(newseg, &dseg->seg);
		break;
	    }
	    if (oldfragments) freeDisplayFragments(oldfragments);
	}
    }

    for (; i < nold; i++, dseg++) freeDisplaySegment(dseg);

    ocw->offthespot.numsegments = nnew;
}

/*- recomputeDisplaySegments: recompute segments/fragments -*/
static void
recomputeDisplaySegments(ocw)
OffTheSpotConversionWidget ocw;
{
    Cardinal nsegs = ocw->offthespot.numsegments;
    DisplaySegment *dseg;
    DisplayLocation disploc;
    Cardinal i;

    initialLocation(ocw, &disploc);

    for (i = 0, dseg = ocw->offthespot.dispsegments; i < nsegs; i++, dseg++) {
	freeDisplayFragments(dseg->fragments);
	dseg->redrawpos = 0;
	dseg->fragments = computeDisplayFragments(ocw, &dseg->seg, &disploc);
    }
}

/*- computeLastPosition: get last position of the specified fragment list -*/
static void
computeLastPosition(fragments, disploc)
DisplayFragment *fragments;
DisplayLocation *disploc;
{
    while (fragments->next != NULL) fragments = fragments->next;
    disploc->x = fragments->region.x + fragments->region.width;
    disploc->y = fragments->region.y;
}

/*- computeDisplayFragments: compute fragment(s) of the specified segment -*/
static DisplayFragment *
computeDisplayFragments(ocw, newseg, disploc)
OffTheSpotConversionWidget ocw;
ICString *newseg;
DisplayLocation *disploc;
{
    int start;
    int nchars;
    Widget dispobj = ocw->offthespot.displayobj;
    DisplayFragment *fragments, *dfp;
    int widthavailable;

    TRACE(("computeDisplayFragments()\n"));
    start = 0;
    fragments = NULL;
    while (start < newseg->nchars) {
	widthavailable = widthAvailable(ocw, disploc);
	nchars = CDMaxChar(dispobj, newseg, start, widthavailable);
	if (nchars == 0 && disploc->x <= ocw->offthespot.leftmargin) {
	    /*
	     * avoiding infinite loop
	     * we display at least one character per line
	     */
	    nchars = 1;
	}
	TRACE(("\twidthavailable=%d, start=%d, maxchar=%d\n", widthavailable, start, nchars));
	if (nchars > 0) {
	    if (fragments == NULL) {
		fragments = dfp = allocDisplayFragment();
	    } else {
		dfp->next = allocDisplayFragment();
		dfp = dfp->next;
	    }
	    dfp->from = start;
	    dfp->nchars = nchars;
	    dfp->region.x = disploc->x;
	    dfp->region.y = disploc->y;
	    dfp->region.width = CDStringWidth(dispobj, newseg, start,
						    start + nchars);
	    dfp->region.height = ocw->offthespot.lineheight;
	    dfp->next = NULL;

	    disploc->x += dfp->region.width;
	}
	start += nchars;

	if (start < newseg->nchars) nextLocation(ocw, disploc);
    }

    return fragments;
}

/*- widthAvailable: return the width of the current line left for drawing -*/
static int
widthAvailable(ocw, disploc)
OffTheSpotConversionWidget ocw;
DisplayLocation *disploc;
{
    return ocw->offthespot.canvaswidget->core.width - 
           ocw->offthespot.rightmargin - disploc->x;
}

/*- initialLocation: return the initial text drawing position -*/
static void
initialLocation(ocw, disploc)
OffTheSpotConversionWidget ocw;
DisplayLocation *disploc;
{
    int cheight = ocw->offthespot.canvaswidget->core.height;
    int lheight = ocw->offthespot.lineheight;

    disploc->x = ocw->offthespot.leftmargin;
    if (cheight / lheight == 1) {
	/* if there's space for a single line, locate it in the center */
	disploc->y = (cheight - lheight) / 2;
    } else {
	disploc->y = 0;
    }
}

/*- nextLocation: return the position of the next line -*/
static void
nextLocation(ocw, disploc)
OffTheSpotConversionWidget ocw;
DisplayLocation *disploc;
{
    disploc->x = ocw->offthespot.leftmargin;
    disploc->y += ocw->offthespot.lineheight;
}

/*- findLocation: compute the display position of specific character -*/
static DisplayLocation *
findLocation(ocw, dsp, offset, disploc)
OffTheSpotConversionWidget ocw;
DisplaySegment *dsp;
Cardinal offset;
DisplayLocation *disploc;
{
    DisplayFragment *dfp = dsp->fragments;

    while (dfp != NULL) {
	if (dfp->nchars > offset ||
	    dfp->next == NULL && dfp->nchars == offset) {
	    break;
	}
	offset -= dfp->nchars;
	dfp = dfp->next;
    }
    if (dfp == NULL) return NULL;

    disploc->x = dfp->region.x + CDStringWidth(ocw->offthespot.displayobj,
					       &dsp->seg, dfp->from,
					       dfp->from + offset);
    disploc->y = dfp->region.y;

    return disploc;
}

/*- reconfigureDisplay: do reconfiguration of text canvas (resize/popup/popdown) -*/
static void
reconfigureDisplay(ocw)
OffTheSpotConversionWidget ocw;
{
    DisplaySegment *dsp;
    DisplayFragment *dfp;
    Widget canvas = ocw->offthespot.canvaswidget;
    Position lastx, lasty;
    Dimension height = ocw->offthespot.lineheight;
    int i;

    lastx = 0;
    dsp = ocw->offthespot.dispsegments;
    if (ocw->offthespot.numsegments > 0 && dsp->fragments != NULL) {
	lasty = dsp->fragments->region.y;
    } else {
	lasty = 0;
    }
    for (i = 0, dsp = ocw->offthespot.dispsegments; i < ocw->offthespot.numsegments; i++, dsp++) {
	for (dfp = dsp->fragments; dfp != NULL; dfp = dfp->next) {
	    if (lasty != dfp->region.y) {
		XClearArea(XtDisplay(canvas), XtWindow(canvas),
			   lastx, lasty, 0, height, False);
	    }
	    lastx = dfp->region.x + dfp->region.width;
	    lasty = dfp->region.y;
	}
    }

    XClearArea(XtDisplay(canvas), XtWindow(canvas),
	       lastx, lasty, 0, 0, False);
    if (lasty + height < canvas->core.height) {
	XClearArea(XtDisplay(canvas), XtWindow(canvas),
		   0, lasty + height, 0, 0, False);
    }
}

/*- updateDisplay: redraw text (if needed) -*/
static void
updateDisplay(ocw)
OffTheSpotConversionWidget ocw;
{
    DisplaySegment *dsp = ocw->offthespot.dispsegments;
    int i;

    for (i = 0; i < ocw->offthespot.numsegments; i++, dsp++) {
	if (dsp->redrawpos >= 0) {
	    TRACE(("updateDisplaySegment(seg#=%d)\n", i));
	    updateDisplaySegment(ocw, dsp);
	}
    }
}

/*- updateDisplaySegment: redraw specified segment (if needed) -*/
static void
updateDisplaySegment(ocw, dsp)
OffTheSpotConversionWidget ocw;
DisplaySegment *dsp;
{
    DisplayFragment *dfp = dsp->fragments;
    Widget dispobj = ocw->offthespot.displayobj;
    Widget canvas = ocw->offthespot.canvaswidget;
    int from;
    int x;

    while (dfp != NULL) {
	if (dsp->redrawpos < dfp->from + dfp->nchars) {
	    from = (dsp->redrawpos > dfp->from) ? dsp->redrawpos : dfp->from;
	    x = dfp->region.x;
	    if (from > dfp->from) {
		x += CDStringWidth(dispobj, &dsp->seg, dfp->from, from);
	    } 
	    CDDrawString(dispobj, canvas, &dsp->seg,
			 from, dfp->from + dfp->nchars,
			 x, dfp->region.y);
	}
	dfp = dfp->next;
    }
}

/*- redrawSegments: redraw segments in specified area -*/
static void
redrawSegments(ocw, region)
OffTheSpotConversionWidget ocw;
XRectangle *region;
{
    DisplaySegment *dsp = ocw->offthespot.dispsegments;
    DisplayFragment *dfp;
    Widget dispobj = ocw->offthespot.displayobj;
    Widget canvas = ocw->offthespot.canvaswidget;
    int i;

    for (i = 0; i < ocw->offthespot.numsegments; i++, dsp++) {
	for (dfp = dsp->fragments; dfp != NULL; dfp = dfp->next) {
	    if (intersectRect(&dfp->region, region)) {
		CDDrawString(dispobj, canvas, &dsp->seg,
			      dfp->from, dfp->from + dfp->nchars,
			      dfp->region.x, dfp->region.y);
	    }
	}
    }
}

/*
 *+ insert cursor handling
 */

/*- eraseCursor: erase insert cursor -*/
static void
eraseCursor(ocw)
OffTheSpotConversionWidget ocw;
{
    if (!ocw->offthespot.cursorvisible) return;

    TRACE(("eraseCursor() at (%d,%d)\n",
	    ocw->offthespot.cursorlocation.x,
	    ocw->offthespot.cursorlocation.y));
    CDDrawCursor(ocw->offthespot.displayobj,
		 ocw->offthespot.canvaswidget,
		 ocw->offthespot.cursorlocation.x,
		 ocw->offthespot.cursorlocation.y,
		 False);
    ocw->offthespot.cursorvisible = False;
}

/*- showCursor: draw insert cursor -*/
static void
showCursor(ocw)
OffTheSpotConversionWidget ocw;
{
    if (!ocw->offthespot.cursorvisible) return;

    TRACE(("showCursor at (%d,%d)\n",
	    ocw->offthespot.cursorlocation.x,
	    ocw->offthespot.cursorlocation.y));
    CDDrawCursor(ocw->offthespot.displayobj,
		 ocw->offthespot.canvaswidget,
		 ocw->offthespot.cursorlocation.x,
		 ocw->offthespot.cursorlocation.y,
		 True);
}

/*- exposeCursor: make the insert cursor redraw correctly when exposing -*/
static Boolean
exposeCursor(ocw, w, region)
OffTheSpotConversionWidget ocw;
Widget w;
XRectangle *region;
{
    XRectangle bounds;

    if (!ocw->offthespot.cursorvisible) return False;

    TRACE(("exposeCursor(region=%d,%d-%d,%d)\n",
	    region->x, region->y, region->width, region->height));
    CDGetCursorBounds(ocw->offthespot.displayobj, &bounds);
    bounds.x += ocw->offthespot.cursorlocation.x;
    bounds.y += ocw->offthespot.cursorlocation.y;
    if (intersectRect(region, &bounds)) {
	eraseCursor(ocw);
	XClearArea(XtDisplay(w), XtWindow(w),
		   bounds.x, bounds.y, bounds.width, bounds.height, False);
	unionRect(region, &bounds, region);
    }
    ocw->offthespot.cursorvisible = True;
    return True;
}

/*- computeCursor: compute cursor position -*/
static void
computeCursor(ocw)
OffTheSpotConversionWidget ocw;
{
    DisplaySegment *dsp;
    DisplayLocation disploc;
    Cardinal seg, offset;

    if (ocw->offthespot.numsegments == 0) {
	/* special case */
	initialLocation(ocw, &(ocw->offthespot.cursorlocation));
	ocw->offthespot.cursorlocation.y += ocw->offthespot.ascent;
	ocw->offthespot.cursorvisible = True;
	return;
    }

    if (!ICCursorPos(ocw->ccontrol.inputobj, &seg, &offset)) return;

    /* sanity check */
    if (seg >= ocw->offthespot.numsegments) return;
    dsp = ocw->offthespot.dispsegments + seg;
    if (offset > dsp->seg.nchars) return;

    if (findLocation(ocw, dsp, offset, &disploc) == NULL) return;

    disploc.y += ocw->offthespot.ascent;

    ocw->offthespot.cursorvisible = True;
    ocw->offthespot.cursorlocation = disploc;
}

/*
 *+ miscelaneous functions
 */

/*- MoveShell: move shell widget -*/
static void
MoveShell(w, x, y)
Widget w;
Position x;
Position y;
{
    XtWidgetGeometry req;

    /*
     * calling XtMoveWidget() is NOT enough to move shell widgets.
     * we must use XtMakeGeometryRequest() or XtSetValues() to
     * invoke root-geometry-manager which modifies the size hint
     * appropriately.
     */
    req.request_mode = CWX | CWY;
    req.x = x;
    req.y = y;
    (void)XtMakeGeometryRequest(w, &req, (XtWidgetGeometry *)NULL);
}

/*- getToplevelWindow: get top-level window of a given window -*/
static Window
getToplevelWindow(dpy, win)
Display *dpy;
Window win;
{
    Atom wm_state;
    Atom type;
    int format;
    unsigned long nitems, bytesafter;
    unsigned char *data;
    Window root, parent;
    Window *children;
    unsigned int nchildren;

    /*
     * find toplevel window which has WM_STATE property or if no exists,
     * direct subwindow of the root window. (ie I assume that if a
     * window manager is running, that is a ICCCM compliant one)
     */
    wm_state = CachedInternAtom(dpy, "WM_STATE", True);
    for (;;) {
	type = None;
	if (wm_state != None) {
	    data = NULL;
	    XGetWindowProperty(dpy, win, wm_state, 0L, 0L, False,
			       AnyPropertyType, &type, &format,
			       &nitems, &bytesafter, &data);
	    if (data != NULL) XtFree((char *)data);
	    if (type != None) break;
	}
	if (!XQueryTree(dpy, win, &root, &parent, &children, &nchildren)) break;
	if (nchildren > 0) XtFree((char *)children);
	if (root == parent) break;
	win = parent;
    }
    return win;
}

/*- setTransientFor: set WM_TRANSIENT_FOR property to specified widget -*/
static void
setTransientFor(w, win)
Widget w;
Window win;
{
    if (!XtIsRealized(w)) XtRealizeWidget(w);
    XSetTransientForHint(XtDisplay(w), XtWindow(w), win);
}

/*- intersectRect: return whether given two rectangles have a intersection -*/
static Boolean
intersectRect(rect1, rect2)
register XRectangle *rect1;
register XRectangle *rect2;
{
    return (rect1->x + rect1->width <= rect2->x ||
	    rect1->x >= rect2->x + rect2->width ||
	    rect1->y + rect1->height <= rect2->y ||
	    rect1->y >= rect2->y + rect2->height) ? False : True;
}

/*- unionRect: returns a minimum rectangle that covers given two rectangles -*/
static void
unionRect(rect1, rect2, rect_ret)
register XRectangle *rect1;
register XRectangle *rect2;
XRectangle *rect_ret;
{
    int x0, x1, y0, y1;

    x0 = (rect1->x > rect2->x) ? rect2->x : rect1->x;
    y0 = (rect1->y > rect2->y) ? rect2->y : rect1->y;
    x1 = (rect1->x + rect1->width > rect2->x + rect2->width) ?
		rect1->x + rect1->width : rect2->x + rect2->width;
    y1 = (rect1->y + rect1->height > rect2->y + rect2->height) ?
		rect1->y + rect1->height : rect2->y + rect2->height;

    rect_ret->x = x0;
    rect_ret->y = y0;
    rect_ret->width = x1 - x0;
    rect_ret->height = y1 - y0;
}

static DisplayFragment *free_fragments = NULL;

/*- allocDisplayFragment: get a new fragment -*/
static DisplayFragment *
allocDisplayFragment()
{
    if (free_fragments == NULL) {
	return XtNew(DisplayFragment);
    } else {
	DisplayFragment *dfp = free_fragments;
	free_fragments = dfp->next;
	return dfp;
    }
}

/*- freeDisplayFragments: add specified fragment list to the free-list -*/
static void
freeDisplayFragments(fragments)
DisplayFragment *fragments;
{
    DisplayFragment *dfp = fragments;

    if (dfp == NULL) return;
    while (dfp->next != NULL) dfp = dfp->next;
    dfp->next = free_fragments;
    free_fragments = fragments;
}

/*- destroyDisplayFragments: do 'free()' specified fragment list -*/
static void
destroyDisplayFragments(fragments)
DisplayFragment *fragments;
{
    DisplayFragment *dfp;

    while (fragments != NULL) {
	dfp = fragments->next;
	XtFree((char *)fragments);
	fragments = dfp;
    }
}

/*- allocDisplaySegments: prepare specified number of display segments -*/
static void
allocDisplaySegments(ocw, n)
OffTheSpotConversionWidget ocw;
Cardinal n;
{
    if (ocw->offthespot.dispsegmentsize > n) return;
    n = ((n + 3) / 4) * 4 ;
    if (ocw->offthespot.dispsegments == NULL) {
	ocw->offthespot.dispsegments = (DisplaySegment *)XtMalloc(n * sizeof(DisplaySegment));
    } else {
	ocw->offthespot.dispsegments = (DisplaySegment *)XtRealloc((char *)ocw->offthespot.dispsegments, n * sizeof(DisplaySegment));
    }
    ocw->offthespot.dispsegmentsize = n;
}

/*- freeDisplaySegment: free display segment's contents -*/
static void
freeDisplaySegment(dsp)
DisplaySegment *dsp;
{
    freeString(&dsp->seg);
    freeDisplayFragments(dsp->fragments);
    dsp->fragments = NULL;
}

/*- clearAllDisplaySegments: clear all display segment's -*/
static void
clearAllDisplaySegments(ocw)
OffTheSpotConversionWidget ocw;
{
    DisplaySegment *dsp = ocw->offthespot.dispsegments;
    int i;

    for (i = 0; i < ocw->offthespot.numsegments; i++) {
	freeDisplaySegment(dsp++);
    }
    ocw->offthespot.numsegments = 0;
}

/*- copyString: copy ICString -*/
static void
copyString(from, to)
ICString *from;
ICString *to;
{
    *to = *from;
    to->data = XtMalloc(to->nbytes);
    (void)bcopy(from->data, to->data, to->nbytes);
}

/*- freeString: free ICString -*/
static void
freeString(seg)
ICString *seg;
{
    XtFree(seg->data);
    seg->data = NULL;
    seg->nbytes = 0;
}
