#ifndef lint
static char *rcsid = "$Id: OverConv.c,v 1.60 1994/06/02 04:43:10 ishisone Rel $";
#endif
/*-
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
#include <X11/Xmu/CharSet.h>
#if XtSpecificationRelease > 4
#include <X11/Xfuncs.h>
#endif
#include "CachedAtom.h"
#include "AsyncErr.h"
#include "OverConvP.h"
#include "InputConv.h"
#include "ConvDisp.h"
#include "CandPanel.h"
#include "AuxPanel.h"
#include "CanvasShel.h"
#include "ICLabel.h"

#define DEBUG_VAR debug_OverTheSpotConversion
#include "DebugPrint.h"

typedef enum { NeedNone, NeedRedraw, NeedReconfig } ResetStatus;

/*- resource table -*/
static XtResource resources[] = {
#define offset(field) XtOffset(OverTheSpotConversionWidget, overthespot.field)
    { XtNspotX, XtCPosition, XtRPosition, sizeof(Position),
	offset(spotx), XtRImmediate, (XtPointer)0 },
    { XtNspotY, XtCPosition, XtRPosition, sizeof(Position),
	offset(spoty), XtRImmediate, (XtPointer)0 },
    { XtNautoSpotForwarding, XtCAutoSpotForwarding, XtRBoolean, sizeof(Boolean),
	offset(spotforwarding), XtRImmediate, (XtPointer)False },
    { XtNlineSpacing, XtCLineSpacing, XtRDimension, sizeof(Dimension),
	offset(linespacing), XtRImmediate, (XtPointer)0 },
    { XtNmodeLocation, XtCModeLocation, XtRModeLocation, sizeof(ModeLocation),
	offset(modelocation), XtRString, "BottomLeft" },
    { XtNshrinkWindow, XtCShrinkWindow, XtRBoolean, sizeof(Boolean),
	offset(shrinkwindow), XtRImmediate, (XtPointer)False },
    { XtNignoreStatusAreaSpec, XtCIgnoreStatusAreaSpec,
	XtRBoolean, sizeof(Boolean),
	offset(ignorestatusarea), XtRImmediate, (XtPointer)False },
    { XtNmodeBorderForeground, XtCModeBorderForeground,
	XtRBoolean, sizeof(Boolean),
	offset(borderforeground), XtRImmediate, (XtPointer)False },
    /* changes superclass's default */
    { XtNmappedWhenManaged, XtCMappedWhenManaged, XtRBoolean, sizeof(Boolean),
	XtOffset(OverTheSpotConversionWidget, core.mapped_when_managed),
	XtRImmediate, (XtPointer)False },
#undef offset
};

/*- default translation table -*/
static char translations[] = "<Key>: to-inputobj()";	/* same as superclass's */

/*- declarations of static functions -*/
static void ClassInitialize();
static void Initialize();
static void Destroy();
static Boolean SetValues();

static void ConversionStartup();
static void ChangeAttributes();
static void ChangeFocus();
static void ConversionFinish();

static void CreateDisplayObject();
static void CreateSelectionWidget();
static void CreateModeWidget();
static TextCanvas * CreateTextCanvas();

static void setupTextCanvas();
static ResetStatus resetTextCanvas();
static void setupDisplayObject();
static ResetStatus resetDisplayObject();
static void setupModeWidget();
static ResetStatus resetModeWidget();
static void locateTextCanvasInitial();
static void locateModeWidget();
static void locateTrackingModeWidget();
static void redrawAndReconfigureTextCanvas();

static void UpdateText();
static void UpdateMode();
static void SelectionControl();

static void SelectionStart();
static void locateSelectionPopup();
static void SelectionEnd();
static void SelectionSet();
static void SelectionMove();
static void ForwardSpot();

static void CreateAuxWidget();
static void AuxControl();
static void AuxStart();
static void locateAuxPopup();
static void AuxEnd();
static void AuxChange();


static void TextRedisplay();

static void SelectionSelected();

static void computeDisplaySegments();
static void recomputeDisplaySegments();
static void computeLastPosition();
static DisplayFragment *computeDisplayFragments();
static int computeWidthAvailable();
static void nextLocation();
static DisplayLocation *findLocation();
static void reconfigureDisplay();
static void updateDisplay();
static void updateDisplaySegment();
static void redrawSegments();

static void adjustDisplay();
static Boolean getAttributeSegmentRange();
static Boolean getInsertingSegmentRange();
static void adjustOffset();

static void eraseCursor();
static void showCursor();
static Boolean exposeCursor();
static void computeCursor();

static void StringToModeLocation();

static void MoveShell();
static Window getToplevelWindow();
static void setTransientFor();
static void getFocusOffset();
static Boolean intersectRect();
static void unionRect();
static int enoughSpaceForStatus();
static DisplayFragment *allocDisplayFragment();
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

/*- overTheSpotConversionClass record -*/
OverTheSpotConversionClassRec overTheSpotConversionClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass)&conversionControlClassRec,
    /* class_name		*/	"OverTheSpotConversion",
    /* widget_size		*/	sizeof(OverTheSpotConversionRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
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
    /* destroy			*/	Destroy,
    /* resize			*/	XtInheritResize,
    /* expose			*/	NULL,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
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
    /* Startup			*/	ConversionStartup,
    /* Finish			*/	ConversionFinish,
    /* ChangeAttributes		*/	ChangeAttributes,
    /* ChangeFocus		*/	ChangeFocus,
    /* TextChange		*/	UpdateText,
    /* ModeChange		*/	UpdateMode,
    /* SelectionControl		*/	SelectionControl,
    /* SelectionControl		*/	AuxControl,
  },
  { /* overTheSpotConversion fields */
    /* empty			*/	0
  },
};

WidgetClass overTheSpotConversionWidgetClass = (WidgetClass)&overTheSpotConversionClassRec;

/*
 *+ Convenience macros
 */
#define SPOTX(w)	((w)->overthespot.spotx)
#define SPOTY(w)	((w)->overthespot.spoty)
#define CLAREA(w)	((w)->overthespot.clientarea)
#define FOCUSOFFX(w)	((w)->overthespot.focusoffsetx)
#define FOCUSOFFY(w)	((w)->overthespot.focusoffsety)

/*
 *+ Core class methods
 */

/*- ClassInitialize: add resource converter (string->modelocation) -*/
/* ARGSUSED */
static void
ClassInitialize()
{
    XtAddConverter(XtRString, XtRModeLocation, StringToModeLocation,
		   NULL, 0);
}

/*- Initialize: initialize method -*/
/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)new;

    ocw->overthespot.background = ocw->core.background_pixel;
    ocw->overthespot.canvaslist = NULL;
    ocw->overthespot.overflowcanvas = NULL;
    ocw->overthespot.dispsegments = NULL;
    ocw->overthespot.numsegments = 0;
    ocw->overthespot.dispsegmentsize = 0;
    ocw->overthespot.candlist = NULL;
    ocw->overthespot.numcands = 0;
    ocw->overthespot.selectionpoppedup = False;
    ocw->overthespot.cursorvisible = False;
    ocw->overthespot.canvascursor = None;
    ocw->overthespot.auxpoppedup = False;

    ocw->overthespot.wm_state =
      CachedInternAtom(XtDisplay(new), "WM_STATE", False);

    /* テキスト表示の widget は最初の変換開始時に作る */
    CreateDisplayObject(ocw);
    CreateSelectionWidget(ocw);
    CreateAuxWidget(ocw);
    CreateModeWidget(ocw);
}

/*- Destroy: destroy method -*/
static void
Destroy(w)
Widget w;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;

    /* ディスプレイセグメントの領域を解放 */
    if (ocw->overthespot.dispsegments) {
	DisplaySegment *dsp = ocw->overthespot.dispsegments;
	int i;

	for (i = 0; i < ocw->overthespot.numsegments; i++) {
	    freeString(&dsp[i].seg);
	    destroyDisplayFragments(dsp->fragments);
	}
	XtFree((char *)dsp);
    }

    /* canvaslist の領域を解放 (canvas widget は 自動的に destroy される */
    if (ocw->overthespot.canvaslist) {
	TextCanvas *p = ocw->overthespot.canvaslist;
	while (p) {
	    TextCanvas *q = p->next;
	    XtFree((char *)p);
	    p = q;
	}
    }
}

/*- SetValues: setvalues method -*/
/* ARGSUSED */
static Boolean
SetValues(cur, req, new, args, num_args)
Widget cur;
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    /* OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)new; */
    return False;
}

/*
 *+ ConversionControl cass methods
 */

/*- ConversionStartup: class specific conversion startup -*/
static void
ConversionStartup(w, mask, value)
Widget w;
unsigned long mask;
ConversionAttributes *value;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;
    Widget inputobj = ocw->ccontrol.inputobj;
    Window toplevel;

    TRACE(("OverTheSpot:ConversionStartup()\n"));

    /* 内部のバッファをクリアする */
    clearAllDisplaySegments(ocw);

    /* 変換オブジェクトにコールバックを設定する */
    XtAddCallback(inputobj, XtNfixNotify, ForwardSpot, (XtPointer)w);

    if (ocw->overthespot.ignorestatusarea) mask &= ~CAStatusArea;

    setupDisplayObject(ocw, mask, value);
    setupTextCanvas(ocw, mask, value);
    setupModeWidget(ocw, mask, value);

    /* WM_TRANSIENT_FOR プロパティを正しくセットする */
    toplevel = getToplevelWindow(XtDisplay(w),
				 ocw->ccontrol.clientwindow,
				 ocw->overthespot.wm_state);
    setTransientFor(ocw->overthespot.modeshell, toplevel);
    setTransientFor(ocw->overthespot.selectionshell, toplevel);
    setTransientFor(ocw->overthespot.auxshell, toplevel);

    /* テキストキャンバスの表示位置を決める */
    locateTextCanvasInitial(ocw);

    /* モードの表示位置を決める */
    if (!ocw->overthespot.modelocationspecified) locateModeWidget(ocw);

    /*
     * OverTheSpotConvesion の場合、自分自身はポップアップしないが、
     * バックエンドタイプの時にはクライアントがこの widget の
     * ウィンドウに対してイベントを送るので、Realize だけしておく
     * その際、大きさを指定しないとサイズが 0 になってしまうので注意する
     */
    if (!XtIsRealized(w)) {
	Arg args[2];

	XtSetArg(args[0], XtNwidth, 1);
	XtSetArg(args[1], XtNheight, 1);
	XtSetValues(w, args, 2);
	XtRealizeWidget(w);
    }

    /* モード表示キャンバスをポップアップする */
    XtPopup(ocw->overthespot.modeshell, XtGrabNone);
}

/*- ChangeAttributes: class specific conversion attribute change routine -*/
/* ARGSUSED */
static void
ChangeAttributes(w, mask, value)
Widget w;
unsigned long mask;
ConversionAttributes *value;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;
    ResetStatus dispres, tcres;

    TRACE(("OverTheSpot:ChangeAttributes()\n"));

    if (ocw->overthespot.ignorestatusarea) mask &= ~CAStatusArea;

    dispres = resetDisplayObject(ocw, mask, value);
    tcres = resetTextCanvas(ocw, mask, value);
    if (dispres == NeedReconfig || tcres == NeedReconfig) {
	redrawAndReconfigureTextCanvas(ocw);
    } else if (dispres == NeedRedraw || tcres == NeedRedraw) {
	TextCanvas *tcp = ocw->overthespot.canvaslist;

	while (tcp != NULL) {
	    if (XtIsRealized(tcp->canvas)) {
		XClearArea(XtDisplay(tcp->canvas), XtWindow(tcp->canvas),
			   0, 0, 0, 0, True);
	    }
	    tcp = tcp->next;
	}
    }

    if (resetModeWidget(ocw, mask, value) != NeedNone &&
	XtIsRealized(ocw->overthespot.modewidget)) {
	XClearArea(XtDisplay(w), XtWindow((Widget)ocw->overthespot.modewidget),
		   0, 0, 0, 0, True);
    }
}

/*- ChangeFocus: class specific conversion attribute change routine -*/
static void
ChangeFocus(w, set)
Widget w;
int set;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;

    TRACE(("OverTheSpot:ChangeFocus()\n"));

    if (set) {
	XtPopup(ocw->overthespot.modeshell, XtGrabNone);
    } else {
	XtPopdown(ocw->overthespot.modeshell);
    }
}

/*- ConversionFinish: class specific conversion finish -*/
/* ARGSUSED */
static void
ConversionFinish(w)
Widget w;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;
    Widget inputobj = ocw->ccontrol.inputobj;
    TextCanvas *tcp = ocw->overthespot.canvaslist;
    XAEHandle h;

    /* 変換オブジェクトのコールバックを消す */
    XtRemoveCallback(inputobj, XtNfixNotify, ForwardSpot, (XtPointer)w);

    /* Popdown and unrealize textcanvases
     *	we must be careful here. if clientwindow are destroyed,
     *	the text canvases are also destroyed.
     *	we have to popdown and unrealize canvas widgets, but if
     *	they are destroyed, BadWindow error will be generated.
     *	so we must set own error handler that ignores errors.
     */
    h = XAESetIgnoreErrors(XtDisplay(w));
    while (tcp != NULL) {
	if (tcp->poppedup) XtPopdown(tcp->canvas);
	XtUnrealizeWidget(tcp->canvas);
	/* XtVaSetValues(tcp->canvas, XtNcursor, None, NULL); */
	tcp->poppedup = False;
	tcp = tcp->next;
    }
    /* Popdown mode widget */
    XtPopdown(ocw->overthespot.modeshell);
    if (ocw->overthespot.modeshell == ocw->overthespot.modeshell_fix) {
	XtUnrealizeWidget(ocw->overthespot.modeshell);
    }
    XAEUnset(h);

    /* Popdown selection popup (if popped-up) */
    if (ocw->overthespot.selectionpoppedup) {
	XtPopdown(ocw->overthespot.selectionshell);
	ocw->overthespot.selectionpoppedup = False;
    }
    if (ocw->overthespot.auxpoppedup) {
	XtPopdown(ocw->overthespot.auxshell);
	ocw->overthespot.auxpoppedup = False;
    }
}

/*
 *+ sub-widget creation
 */

/*- CreateDisplayObject: create display object for text drawing -*/
static void
CreateDisplayObject(ocw)
OverTheSpotConversionWidget ocw;
{
    Widget dispobj;

    dispobj = XtCreateWidget("displayObj", ocw->ccontrol.displayobjclass,
			     (Widget)ocw, NULL, 0);

    ocw->overthespot.displayobj = dispobj;
    ocw->overthespot.lineheight = CDLineHeight(dispobj,
					       &ocw->overthespot.ascent);

}

/*- CreateSelectionWidget: create selection widget for selecting candidates -*/
static void
CreateSelectionWidget(ocw)
OverTheSpotConversionWidget ocw;
{
    Widget shell, sel, obj;

    shell = XtVaCreatePopupShell("selectionShell",
				 transientShellWidgetClass,
				 (Widget)ocw,
				 XtNwidth, 1, XtNheight, 1,
				 NULL);
    ocw->overthespot.selectionshell = shell;

    sel = XtCreateManagedWidget("selection", candidatePanelWidgetClass,
				shell, NULL, 0);
    obj = XtCreateWidget("display", ocw->ccontrol.displayobjclass,
			 sel, NULL, 0);
    XtAddCallback(sel, XtNcallback, SelectionSelected, (Widget)ocw);
    XtInstallAccelerators(sel, (Widget)ocw);

    ocw->overthespot.selectionwidget = sel;
    ocw->overthespot.selectiondisplayobj = obj;
}

/*- CreateAuxWidget: create auxiliary widget for displaying auxiliary data -*/
static void
CreateAuxWidget(ocw)
OverTheSpotConversionWidget ocw;
{
    Widget shell, sel, obj;

    shell = XtVaCreatePopupShell("auxShell",
				 transientShellWidgetClass,
				 (Widget)ocw,
				 XtNwidth, 1, XtNheight, 1,
				 XtNallowShellResize, True,
				 NULL);
    ocw->overthespot.auxshell = shell;

    sel = XtCreateManagedWidget("aux", auxPanelWidgetClass,
				shell, NULL, 0);
    obj = XtCreateWidget("display", ocw->ccontrol.displayobjclass,
			 sel, NULL, 0);
    XtAddCallback(sel, XtNcallback, SelectionSelected, (Widget)ocw);
    XtInstallAccelerators(sel, (Widget)ocw);

    ocw->overthespot.auxwidget = sel;
    ocw->overthespot.auxdisplayobj = obj;
}

/*- CreateModeWidget: create mode displaying widget -*/
static void
CreateModeWidget(ocw)
OverTheSpotConversionWidget ocw;
{
    Widget shell, w, obj;

    TRACE(("CreateModeWidget()\n"));

    /* create fixed widget */
    shell = XtCreatePopupShell("modeShell", adoptedShellWidgetClass,
			       (Widget)ocw, NULL, 0);
    XtVaGetValues(shell, XtNborderWidth, &ocw->overthespot.saved_bw, NULL);
    w = XtCreateManagedWidget("mode", icLabelWidgetClass, shell, NULL, 0);
    obj = XtCreateWidget("display", ocw->ccontrol.displayobjclass, w,
			 NULL, 0);
    XtInstallAccelerators(shell, (Widget)ocw);
    XtInstallAccelerators(w, (Widget)ocw);
    ocw->overthespot.modeshell_fix = shell;
    ocw->overthespot.modewidget_fix = w;
    ocw->overthespot.modedisplayobj_fix = obj;

    /* create floating widget */
    shell = XtCreatePopupShell("modeShell", transientShellWidgetClass,
			       (Widget)ocw, NULL, 0);
    w = XtCreateManagedWidget("mode", icLabelWidgetClass, shell, NULL, 0);
    obj = XtCreateWidget("display", ocw->ccontrol.displayobjclass, w,
			 NULL, 0);
    XtInstallAccelerators(shell, (Widget)ocw);
    XtInstallAccelerators(w, (Widget)ocw);
    ocw->overthespot.modeshell_float = shell;
    ocw->overthespot.modewidget_float = w;
    ocw->overthespot.modedisplayobj_float = obj;
}

/*- CreateTextCanvas: create a text canvas -*/
static TextCanvas *
CreateTextCanvas(ocw)
OverTheSpotConversionWidget ocw;
{
    TextCanvas *tcp;

    tcp = XtNew(TextCanvas);
    tcp->x = 0;
    tcp->y = 0;
    tcp->poppedup = False;
    tcp->next = NULL;
    tcp->canvas = XtVaCreatePopupShell("text",
				       canvasShellWidgetClass,
				       (Widget)ocw,
				       XtNcolormap,
				           ocw->overthespot.colormap,
				       XtNbackground,
				           ocw->overthespot.background,
				       XtNparentWindow,
				           ocw->ccontrol.clientwindow,
				       XtNoverrideRedirect, True,
				       NULL);	/* XXX for now XXX */
    XtAddCallback(tcp->canvas, XtNexposeCallback, TextRedisplay, (XtPointer)ocw);
    XtInstallAccelerators(tcp->canvas, (Widget)ocw);

    return tcp;
}


/*
 *+ subwidget configuration
 */

/*- setupTextCanvas: do text canvas configuration on conversion startup -*/
static void
setupTextCanvas(ocw, mask, value)
OverTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    TRACE(("setupTextCanvas(mask=0x%lx)\n", mask));

    getFocusOffset(ocw);

    if (mask & CAClientArea) {
	CLAREA(ocw) = value->clientarea;
    } else {
	/* default */
	CLAREA(ocw).x = 0;
	CLAREA(ocw).y = 0;
	CLAREA(ocw).width = ocw->ccontrol.focus_attr.width;
	CLAREA(ocw).height = ocw->ccontrol.focus_attr.height;
    }
    CLAREA(ocw).x += FOCUSOFFX(ocw);
    CLAREA(ocw).y += FOCUSOFFY(ocw);

    TRACE(("\tclientarea: (%d,%d)-(%d,%d)\n",CLAREA(ocw).x,CLAREA(ocw).y,CLAREA(ocw).width,CLAREA(ocw).height));
    if (mask & CASpotLocation) {
	SPOTX(ocw) = value->spotx + FOCUSOFFX(ocw);
	SPOTY(ocw) = value->spoty + FOCUSOFFY(ocw);
    } else {
	/* default */
	SPOTX(ocw) = CLAREA(ocw).x;
	SPOTY(ocw) = CLAREA(ocw).y + ocw->overthespot.ascent;
    }
    TRACE(("\tspotlocation: (%d,%d)\n",SPOTX(ocw),SPOTY(ocw)));

    if (mask & CALineSpacing) {
	ocw->overthespot.linespacing = value->linespacing;
	if (ocw->overthespot.linespacing == 0) {
	    DPRINT(("\tspecified linespacing is 0. reset to default\n"));
	    ocw->overthespot.linespacing = ocw->overthespot.lineheight;
	}
    } else {
	ocw->overthespot.linespacing = ocw->overthespot.lineheight;
    }
    if (mask & CAColormap) {
	ocw->overthespot.colormap = value->colormap;
    } else {
	ocw->overthespot.colormap = DefaultColormapOfScreen(XtScreen((Widget)ocw));
    }
    if (mask & CAColor) {
	ocw->overthespot.background = value->background;
    } else {
	/* default */
	ocw->overthespot.background = ocw->core.background_pixel;
    }
    if (mask & CACursor) {
	ocw->overthespot.canvascursor = value->cursor;
    } else {
	ocw->overthespot.canvascursor = None;
    }

    if (ocw->overthespot.canvaslist == NULL) {
	ocw->overthespot.canvaslist = CreateTextCanvas(ocw);
    } else {
	TextCanvas *tcp = ocw->overthespot.canvaslist;
	while (tcp != NULL) {
	    XtVaSetValues(tcp->canvas,
			  XtNcolormap, ocw->overthespot.colormap,
			  XtNbackground, ocw->overthespot.background,
			  XtNparentWindow, ocw->ccontrol.clientwindow,
			  XtNcursor, ocw->overthespot.canvascursor,
			  NULL);
	    tcp = tcp->next;
	}
    }
}

/*- resetTextCanvas: do text canvas reconfiguration on attribute change -*/
static ResetStatus
resetTextCanvas(ocw, mask, value)
OverTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    ResetStatus redraw = NeedNone;

    if (mask & (CAColormap|CAColor|CACursor)) {
	Arg args[3];
	Cardinal i = 0;
	if (mask & CAColormap &&
	    value->colormap != ocw->overthespot.colormap) {
	    ocw->overthespot.colormap = value->colormap;
	    XtSetArg(args[i], XtNcolormap, value->colormap); i++;
	}
	if (mask & CAColor &&
	    value->background != ocw->overthespot.background) {
	    ocw->overthespot.background = value->background;
	    XtSetArg(args[i], XtNbackground, value->background); i++;
	}
	if (mask & CACursor &&
	    value->cursor != ocw->overthespot.canvascursor) {
	    ocw->overthespot.canvascursor = value->cursor;
	    XtSetArg(args[i], XtNcursor, value->cursor); i++;
	}
	if (i > 0) {
	    TextCanvas *tcp = ocw->overthespot.canvaslist;

	    while (tcp != NULL) {
		XtSetValues(tcp->canvas, args, i);
		tcp = tcp->next;
	    }
	    redraw = NeedRedraw;
	}
    }
    if (mask & CAFocusWindow) {
	getFocusOffset(ocw);
	redraw = NeedReconfig;
    }
    if (mask & CAClientArea) {
	if (value->clientarea.x + FOCUSOFFX(ocw) != CLAREA(ocw).x ||
	    value->clientarea.y + FOCUSOFFY(ocw) != CLAREA(ocw).y ||
	    value->clientarea.width != CLAREA(ocw).width ||
	    value->clientarea.height != CLAREA(ocw).height) {
	    CLAREA(ocw) = value->clientarea;
	    CLAREA(ocw).x += FOCUSOFFX(ocw);
	    CLAREA(ocw).y += FOCUSOFFY(ocw);
	    redraw = NeedReconfig;
	}
    } else if (mask & CAFocusWindow) {
	CLAREA(ocw).x = FOCUSOFFX(ocw);
	CLAREA(ocw).y = FOCUSOFFY(ocw);
	CLAREA(ocw).width = ocw->ccontrol.focus_attr.width;
	CLAREA(ocw).height = ocw->ccontrol.focus_attr.height;
    }
    if (mask & CASpotLocation) {
	if (value->spotx + FOCUSOFFX(ocw) != SPOTX(ocw) ||
	    value->spoty + FOCUSOFFY(ocw) != SPOTY(ocw)) {
	    SPOTX(ocw) = value->spotx + FOCUSOFFX(ocw);
	    SPOTY(ocw) = value->spoty + FOCUSOFFY(ocw);
	    redraw = NeedReconfig;
	}
    } else if (mask & CAFocusWindow) {
	SPOTX(ocw) = CLAREA(ocw).x;
	SPOTY(ocw) = CLAREA(ocw).y + ocw->overthespot.ascent;
    }
    if (mask & CALineSpacing) {
	if (value->linespacing != ocw->overthespot.linespacing &&
	    value->linespacing != 0) {
	    ocw->overthespot.linespacing = value->linespacing;
	    redraw = NeedReconfig;
	}
    } else if (mask & CAFonts) {
	ocw->overthespot.linespacing = ocw->overthespot.lineheight;
	redraw = NeedReconfig;
    }

    return redraw;
}

/*- setupDisplayObject: do displayobj configuration on conversion startup -*/
static void
setupDisplayObject(ocw, mask, value)
OverTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Widget dispobj = ocw->overthespot.displayobj;

    TRACE(("setupDisplayObject()\n"));

    /*
     * order is important. we must set fonts BEFORE anything else,
     * because it is possible that the fonts previously set in the
     * display object no longer exist, and if so, that causes BadFont
     * error when changing GCs.
     */

    if (mask & CAFonts) {
	TRACE(("\tchanging fonts...\n"));
	CDSetFonts(dispobj, value->fonts, value->num_fonts);
    } else {
	/* reset to default */
	CDSetFonts(dispobj, (XFontStruct **)NULL, 0);
    }
    if (mask & CAColor) {
	TRACE(("\tchanging colors...\n"));
	XtVaSetValues(dispobj,
		      XtNforeground, value->foreground,
		      XtNbackground, value->background,
		      NULL);
    }
    ocw->overthespot.lineheight = CDLineHeight(dispobj,
					       &ocw->overthespot.ascent);
}

/*- resetDisplayObject: do displayobj reconfiguration on attribute change -*/
static ResetStatus
resetDisplayObject(ocw, mask, value)
OverTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Widget dispobj = ocw->overthespot.displayobj;
    ResetStatus redraw = NeedNone;

    TRACE(("resetDisplayObject()\n"));

    if (mask & CAColor) {
	TRACE(("\tchanging colors...\n"));
	XtVaSetValues(dispobj,
		      XtNforeground, value->foreground,
		      XtNbackground, value->background,
		      NULL);
	redraw = NeedRedraw;
    }
    if (mask & CAFonts) {
	TRACE(("\tchanging fonts...\n"));
	CDSetFonts(dispobj, value->fonts, value->num_fonts);
	ocw->overthespot.lineheight = CDLineHeight(dispobj,
						   &ocw->overthespot.ascent);
	redraw = NeedReconfig;
    }

    return redraw;
}

/*- setupModeWidget: do mode widget configuration on conversion startup -*/
static void
setupModeWidget(ocw, mask, value)
OverTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Widget inputobj = ocw->ccontrol.inputobj;
    Widget dispobj;
    Widget mode;
    Arg modeargs[10];
    Arg shellargs[15];
    Cardinal i = 0, j = 0;

    TRACE(("setupModeWidget()\n"));

    /* choose appropriate widgets */
    if (mask & CAStatusArea) {
	/* use fixed modedisplay */
	ocw->overthespot.modeshell = ocw->overthespot.modeshell_fix;
	ocw->overthespot.modewidget = ocw->overthespot.modewidget_fix;
	ocw->overthespot.modedisplayobj = ocw->overthespot.modedisplayobj_fix;
	XtSetArg(shellargs[j], XtNparentWindow, ocw->ccontrol.clientwindow); j++;
	XtSetArg(shellargs[j], XtNborderWidth, 0); j++;
	XtSetArg(shellargs[j], XtNallowShellResize, False); j++;
	XtSetArg(shellargs[j], XtNx, value->statusarea.x); j++;
	XtSetArg(shellargs[j], XtNy, value->statusarea.y); j++;
	XtSetArg(shellargs[j], XtNwidth, value->statusarea.width); j++;
	XtSetArg(shellargs[j], XtNheight, value->statusarea.height); j++;
	ocw->overthespot.modelocationspecified = True;
    } else if (ocw->overthespot.modelocation == ModeTrackText &&
	       enoughSpaceForStatus(ocw)) {
	ocw->overthespot.modeshell = ocw->overthespot.modeshell_fix;
	ocw->overthespot.modewidget = ocw->overthespot.modewidget_fix;
	ocw->overthespot.modedisplayobj = ocw->overthespot.modedisplayobj_fix;
	ocw->overthespot.modelocationspecified = False;
	XtSetArg(shellargs[j], XtNparentWindow, ocw->ccontrol.clientwindow); j++;
	XtSetArg(shellargs[j], XtNallowShellResize, True); j++;
	XtSetArg(shellargs[j], XtNborderWidth, ocw->overthespot.saved_bw); j++;
    } else {
	/* use floating modedisplay */
	ocw->overthespot.modeshell = ocw->overthespot.modeshell_float;
	ocw->overthespot.modewidget = ocw->overthespot.modewidget_float;
	ocw->overthespot.modedisplayobj = ocw->overthespot.modedisplayobj_float;
	ocw->overthespot.modelocationspecified = False;
    }

    mode = ocw->overthespot.modewidget;
    dispobj = ocw->overthespot.modedisplayobj;

    XtSetArg(modeargs[i], XtNlabel, ICGetMode(inputobj)); i++;
    if (mask & CAColormap) {
	XtSetArg(modeargs[i], XtNcolormap, value->colormap); i++;
    }
    /* ignore background_pixmap... */

    /*
     * order of changing display object resources is important.
     * see comment in setupDisplayObject() for details.
     */
    if (mask & CAFonts) {
	TRACE(("\tchanging fonts...\n"));
	CDSetFonts(dispobj, value->fonts, value->num_fonts);
    } else {
	/* reset to default */
	CDSetFonts(dispobj, (XFontStruct **)NULL, 0);
    }
    if (mask & CAColor) {
	TRACE(("\tchanging colors...\n"));
	XtVaSetValues(dispobj,
		      XtNforeground, value->foreground,
		      XtNbackground, value->background,
		      NULL);
	XtSetArg(modeargs[i], XtNbackground, value->background); i++;
	if (ocw->overthespot.borderforeground) {
	    XtSetArg(shellargs[j], XtNborderColor, value->foreground); j++;
	}
    } else {
	XtSetArg(modeargs[i], XtNbackground, ocw->overthespot.background); i++;
    }

    XtSetValues(mode, modeargs, i);
    ICLRecomputeSize(mode);

    if (!(mask & CAStatusArea)) {
	/*
	 * force shell to resize.
	 * it is because Shell doesn't honor its child's dimension
	 * at second (or later)  realization.
	 */
	XtSetArg(shellargs[j], XtNwidth, mode->core.width); j++;
	XtSetArg(shellargs[j], XtNheight, mode->core.height); j++;
    }
    XtSetValues(ocw->overthespot.modeshell, shellargs, j);
}

/*- resetModeWidget: do mode widget reconfiguration on attribute change -*/
static ResetStatus
resetModeWidget(ocw, mask, value)
OverTheSpotConversionWidget ocw;
unsigned long mask;
ConversionAttributes *value;
{
    Widget mode = ocw->overthespot.modewidget;
    Widget dispobj = ocw->overthespot.modedisplayobj;
    ResetStatus redraw = NeedNone;

    TRACE(("resetModeWidget()\n"));

    if (mask & CAStatusArea) {
	if (ocw->overthespot.modelocationspecified &&
	    ocw->overthespot.modeshell == ocw->overthespot.modeshell_fix) {
	    XtVaSetValues(ocw->overthespot.modeshell,
			  XtNx, value->statusarea.x,
			  XtNy, value->statusarea.y,
			  XtNwidth, value->statusarea.width,
			  XtNheight, value->statusarea.height,
			  NULL);
	} /* else ignore... */
    }

    if (mask & CAColormap) {
	XtVaSetValues(mode, XtNcolormap, value->colormap, NULL);
    }

    if (mask & CAColor) {
	TRACE(("\tchanging colors...\n"));
	XtVaSetValues(dispobj,
		      XtNforeground, value->foreground,
		      XtNbackground, value->background,
		      NULL);
	XtVaSetValues(mode, XtNbackground, value->background, NULL);
	if (ocw->overthespot.borderforeground) {
	    XtVaSetValues(ocw->overthespot.modeshell,
			  XtNborderColor, value->foreground,
			  NULL);
	}
	redraw = NeedRedraw;
    }
    if (mask & CAFonts) {
	TRACE(("\tchanging fonts...\n"));
	CDSetFonts(dispobj, value->fonts, value->num_fonts);
	ICLRecomputeSize(mode);
	redraw = NeedRedraw;
    }

    return redraw;
}

/*- locateTextCanvasInitial: put the text canvas at the initial position -*/
static void
locateTextCanvasInitial(ocw)
OverTheSpotConversionWidget ocw;
{
    TextCanvas *tcp = ocw->overthespot.canvaslist;

    tcp->x = SPOTX(ocw);
    tcp->y = SPOTY(ocw) - ocw->overthespot.ascent;
}

/*- locateModeWidget: put the mode widget at the initial position -*/
static void
locateModeWidget(ocw)
OverTheSpotConversionWidget ocw;
{
    Position x, y;
    Widget modewidget = ocw->overthespot.modewidget;
    Widget modeshell = ocw->overthespot.modeshell;
    int rootx, rooty;
    Window child;

    if (modeshell == ocw->overthespot.modeshell_fix) {
	/* must be tracking text type */
	locateTrackingModeWidget(ocw, True, 0, 0);
	return;
    }

    switch (ocw->overthespot.modelocation) {
    case ModeTopLeft:
	x = 0;
	y = -(modewidget->core.height + modeshell->core.border_width * 2);
	break;
    case ModeTopRight:
	x = ocw->ccontrol.client_attr.width - modewidget->core.width + modeshell->core.border_width * 2;
	y = -(modewidget->core.height + modeshell->core.border_width * 2);
	break;
    case ModeBottomRight:
	x = ocw->ccontrol.client_attr.width - modewidget->core.width + modeshell->core.border_width * 2;
	y = ocw->ccontrol.client_attr.height;
	break;
    case ModeTrackText:	/* in case of insufficient space in the client area */
	x = CLAREA(ocw).x;
	y = CLAREA(ocw).y + CLAREA(ocw).height + 2;
	break;
    default:
	/* ModeBottomLeft */
	x = 0;
	y = ocw->ccontrol.client_attr.height;
	break;
    }

    (void)XTranslateCoordinates(XtDisplay(ocw), ocw->ccontrol.clientwindow,
				RootWindowOfScreen(XtScreen(ocw)),
				x, y, &rootx, &rooty, &child);
    MoveShell(ocw->overthespot.modeshell, rootx, rooty);
}

/*- locateTrackingModeWidget: put the tracking text type mode widget at appropriate position */
static void
locateTrackingModeWidget(ocw, initial, x, y)
OverTheSpotConversionWidget ocw;
Boolean initial;
Position x;
Position y;
{
    Widget modewidget = ocw->overthespot.modewidget;
    Widget modeshell = ocw->overthespot.modeshell;
    Dimension width, height;
    XRectangle *clarea = &CLAREA(ocw);
    static Position lastx, lasty;

    if (initial) {
	x = SPOTX(ocw);
	y = SPOTY(ocw) - ocw->overthespot.ascent
		+ ocw->overthespot.lineheight;
    } else if (x == lastx && y == lasty) {
	return;
    }

    lastx = x;
    lasty = y;

    width = modewidget->core.width + modeshell->core.border_width * 2;
    height = modewidget->core.height + modeshell->core.border_width * 2;

    /* adjust x */
    if (x + width > clarea->x + clarea->width) {
	x = clarea->x + clarea->width - width;
    }
    if (x < clarea->x) x = clarea->x;

    /* adjust y */
    if (y + height + 2 <= clarea->y + clarea->height) {
	y += 2;	/* make some (2pixels high) space between text and mode */
    } else if (y + height > clarea->y + clarea->height) {
	Position initx, inity;

	if (initial) {
	    initx = SPOTX(ocw);
	    inity = SPOTY(ocw) - ocw->overthespot.ascent;
	} else {
	    TextCanvas *tcp = ocw->overthespot.canvaslist;
	    initx = tcp->x;
	    inity = tcp->y;
	}
	if (inity - height > clarea->y) {
	    y = inity - height;
	} else if (x + width < initx) {
	    y = inity - modeshell->core.border_width * 2;
	} else if (clarea->x + width < initx) {
	    x = initx - width;
	    y = inity - modeshell->core.border_width * 2;
	} else {
	    x = initx - width;
	    y = inity - height;
	}
	if (y < clarea->y) y = clarea->y;
    }
    XtMoveWidget(modeshell, x, y);
}

/*- redrawAndReconfigureTextCanvas: redraw & reconfigure text canvas -*/
static void
redrawAndReconfigureTextCanvas(ocw)
OverTheSpotConversionWidget ocw;
{
    TextCanvas *tcp = ocw->overthespot.canvaslist;

    TRACE(("OverTheSpotConversion:redrawAndReconfigureTextCanvas()\n"));

    /* popdown and clear all canvases */
    while (tcp != NULL) {
	if (tcp->poppedup) XtPopdown(tcp->canvas);
	tcp->poppedup = False;
	if (XtIsRealized(tcp->canvas)) {
	    XClearArea(XtDisplay(tcp->canvas), XtWindow(tcp->canvas),
		       0, 0, 0, 0, True);
	}
	tcp = tcp->next;
    }
    locateTextCanvasInitial(ocw);
    recomputeDisplaySegments(ocw);
    computeCursor(ocw);
    reconfigureDisplay(ocw);
}

/*
 *+ inputobject callback
 */

/*- UpdateText: update text -*/
static void
UpdateText(w)
Widget w;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;

    TRACE(("OverTheSpotConversion:UpdateText()\n"));
    eraseCursor(ocw);
    computeDisplaySegments(ocw);
    computeCursor(ocw);
    reconfigureDisplay(ocw);
    updateDisplay(ocw);
    showCursor(ocw);
}

/*- UpdateMode: update mode -*/
static void
UpdateMode(w)
Widget w;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;

    XtVaSetValues(ocw->overthespot.modewidget,
		  XtNlabel, ICGetMode(ocw->ccontrol.inputobj),
		  NULL);
#ifdef notdef
    /* a hack... */
    if (ocw->overthespot.modeshell == ocw->overthespot.modeshell_float &&
	XtIsRealized(ocw->overthespot.modeshell)) {
	XRaiseWindow(XtDisplay(ocw->overthespot.modeshell),
		     XtWindow(ocw->overthespot.modeshell));
    }
#endif
}

/*- SelectionControl: selection control -*/
static void
SelectionControl(w, arg)
Widget w;
ICSelectionControlArg *arg;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;

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
	XtAppWarning(XtWidgetToApplicationContext(w),
		     "OverTheSpotConversion: unknown selection control command");
	break;
    }
}

/*- SelectionStart: selection startup -*/
/* ARGSUSED */
static void
SelectionStart(ocw, kind)
OverTheSpotConversionWidget ocw;
int kind;
{
    Cardinal ncand;

    TRACE(("OverTheSpotConversion:SelectionStart()\n"));
    if (ocw->overthespot.selectionpoppedup) {
	DPRINT(("\tselection already started -- ignored\n"));
	return;
    }

    ocw->overthespot.candlist = ICGetItemList(ocw->ccontrol.inputobj, &ncand);
    ocw->overthespot.numcands = ncand;

    TRACE(("\tnumcands=%d\n", ocw->overthespot.numcands));
    CPanelSetList(ocw->overthespot.selectionwidget,
		  ocw->overthespot.candlist,
		  ocw->overthespot.numcands, 0, True);

    locateSelectionPopup(ocw);
    XtPopup(ocw->overthespot.selectionshell, XtGrabNone);
    ocw->overthespot.selectionpoppedup = True;
}

/*- locateSelectionPopup: put selection popup at an appropriate position -*/
static void
locateSelectionPopup(ocw)
OverTheSpotConversionWidget ocw;
{
    Position x, y;
    int clx, cly;
    Dimension dpyWidth, dpyHeight;
    Widget panel = ocw->overthespot.selectionwidget;
    Widget shell = ocw->overthespot.selectionshell;
    Window junk;

    (void)XTranslateCoordinates(XtDisplay(ocw),
				ocw->ccontrol.clientwindow,
				RootWindowOfScreen(XtScreen(ocw)),
				0, 0, &clx, &cly, &junk);

    if (ocw->overthespot.numsegments > 0) {
	DisplayLocation lastp;
	DisplaySegment *dsp = ocw->overthespot.dispsegments;
	int i;
	int offset = 0;

	/* find current segment. if not found, use last segment */
	for (i = 0; i < ocw->overthespot.numsegments - 1; i++) {
	    if (dsp[i].seg.attr & ICAttrCurrentSegment) break;
	}

	computeLastPosition(dsp[i].fragments, &lastp);
	if (lastp.canvas == ocw->overthespot.overflowcanvas) {
	    offset = ocw->overthespot.overflowoffset;
	}
	x = clx + lastp.canvas->x + lastp.x
	  - panel->core.width / 2 + offset;
	y = cly + lastp.canvas->y + lastp.y + ocw->overthespot.lineheight;
    } else {
	x = clx + SPOTX(ocw) - panel->core.width / 2;
	y = cly + SPOTY(ocw);
    }

    dpyWidth = WidthOfScreen(XtScreen(shell));
    dpyHeight = HeightOfScreen(XtScreen(shell));

    if (x + panel->core.width > (int)dpyWidth) x = dpyWidth - panel->core.width;
    if (x < 0) x = 0;
    if (y + panel->core.height > (int)dpyHeight) {
	y = cly + SPOTY(ocw) - panel->core.height/*- ascent XXX */;
	if (y < 0) y = dpyHeight - panel->core.height;
    }
    MoveShell(shell, x, y);
}

/*- SelectionEnd: selection finish -*/
static void
SelectionEnd(ocw, current)
OverTheSpotConversionWidget ocw;
int *current;
{
    TRACE(("OverTheSpotConversion:SelectionEnd()\n"));

    if (!ocw->overthespot.selectionpoppedup) {	/* for safe */
	TRACE(("\tnot in selection mode -- ignored\n"));
	return;
    }

    XtVaGetValues(ocw->overthespot.selectionwidget,
		  XtNcurrentItem, current,
		  NULL);

    XtPopdown(ocw->overthespot.selectionshell);

    ocw->overthespot.selectionpoppedup = False;
}

/*- SelectionSet: set current selection item -*/
static void
SelectionSet(ocw, current)
OverTheSpotConversionWidget ocw;
int current;
{
    TRACE(("OverTheSpotConversion:SelectionSet()\n"));

    if (!ocw->overthespot.selectionpoppedup) {	/* for safe */
	TRACE(("\tnot in selection mode -- ignored\n"));
	return;
    }

    XtVaSetValues(ocw->overthespot.selectionwidget,
		  XtNcurrentItem, current,
		  NULL);
}

/*- SelectionMove: move crrent selection to specified direction -*/
static void
SelectionMove(ocw, dir)
OverTheSpotConversionWidget ocw;
int dir;
{
    TRACE(("OverTheSpotConversion:SelectionMove()\n"));

    if (!ocw->overthespot.selectionpoppedup) {	/* for safe */
	TRACE(("\tnot in selection mode -- ignored\n"));
	return;
    }

    CPanelMoveCurrent(ocw->overthespot.selectionwidget, dir);
}

/*- ForwardSpot: forward spot location when text is fixed -*/
/* ARGSUSED */
static void
ForwardSpot(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)client_data;
    DisplaySegment *dsp = ocw->overthespot.dispsegments;
    Cardinal nsegs = ocw->overthespot.numsegments;
    DisplayLocation disploc;

    if (!ocw->overthespot.spotforwarding || nsegs == 0) return;

    /* get next spot location */
    computeLastPosition(dsp[nsegs - 1].fragments, &disploc);

    SPOTX(ocw) = disploc.canvas->x + disploc.x;
    SPOTY(ocw) = disploc.canvas->y + disploc.y + ocw->overthespot.ascent;
    locateTextCanvasInitial(ocw);
}

/*
 * Aux Callback
 */

static void
AuxControl(w, arg)
Widget w;
ICAuxControlArg *arg;
{
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)w;
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
OverTheSpotConversionWidget ocw;
{
  ICString *auxstr;
  Cardinal ncand, curseg, cursorpos;
  
  if (ocw->overthespot.auxpoppedup) return;
  
  /* テキストコールバックの時のような処理をする
     のは AuxPanel.c にまかせよう */

  auxstr = ICGetAuxSegments(ocw->ccontrol.inputobj,
			    &ncand, &curseg, &cursorpos);

  APanelStart(ocw->overthespot.auxwidget, auxstr, ncand, curseg, cursorpos);

  /* ポップアップする場所を決める */
  locateAuxPopup(ocw);
  
  XtPopup(ocw->overthespot.auxshell, XtGrabNone);
  ocw->overthespot.auxpoppedup = True;
}

/* ARGSUSED */
static void
AuxEnd(ocw)
OverTheSpotConversionWidget ocw;
{
  if (!ocw->overthespot.auxpoppedup) return;	/* for safe */

/*  APanelEnd(ocw->overthespot.auxwidget); */

  XtPopdown(ocw->overthespot.auxshell);

  ocw->overthespot.auxpoppedup = False;
}

/* ARGSUSED */
static void
AuxChange(ocw)
OverTheSpotConversionWidget ocw;
{
  Cardinal ncand, curseg, cursorpos;
  ICString *auxstr;

  if (!ocw->overthespot.auxpoppedup) return;	/* for safe */

  auxstr = ICGetAuxSegments(ocw->ccontrol.inputobj,
			    &ncand, &curseg, &cursorpos);

  APanelChange(ocw->overthespot.auxwidget, auxstr, ncand, curseg, cursorpos);
}

/*- locateAuxPopup: put aux popup at an appropriate position -*/
static void
locateAuxPopup(ocw)
OverTheSpotConversionWidget ocw;
{
    int x, y;
    int clx, cly;
    int dpyWidth, dpyHeight;
    Widget panel = ocw->overthespot.auxwidget;
    Widget shell = ocw->overthespot.auxshell;
    Window junk;

    (void)XTranslateCoordinates(XtDisplay(ocw),
				ocw->ccontrol.clientwindow,
				RootWindowOfScreen(XtScreen(ocw)),
				0, 0, &clx, &cly, &junk);

    if (ocw->overthespot.numsegments > 0) {
	DisplayLocation lastp;
	DisplaySegment *dsp = ocw->overthespot.dispsegments;
	int i;
	int offset;

	/* find current segment. if not found, use last segment */
	for (i = 0; i < ocw->overthespot.numsegments - 1; i++) {
	    if (dsp[i].seg.attr & ICAttrCurrentSegment) break;
	}

	computeLastPosition(dsp[i].fragments, &lastp);
	if (lastp.canvas == ocw->overthespot.overflowcanvas)
	    offset = ocw->overthespot.overflowoffset;
	else
	    offset = 0;
	x = clx + lastp.canvas->x + lastp.x
	  - panel->core.width / 2 + offset;
	y = cly + lastp.canvas->y + lastp.y + ocw->overthespot.lineheight;
    } else {
	x = clx + ocw->overthespot.spotx - panel->core.width / 2;
	y = cly + ocw->overthespot.spoty;
    }

    dpyWidth = (int)WidthOfScreen(XtScreen(shell));
    dpyHeight = (int)HeightOfScreen(XtScreen(shell));

    if ((int)(x + panel->core.width) > dpyWidth) x = dpyWidth - panel->core.width;
    if (x < 0) x = 0;
    if ((int)(y + panel->core.height) > dpyHeight) {
	y = cly + ocw->overthespot.spoty - panel->core.height/*- ascent XXX */;
	if (y < 0) y = dpyHeight - panel->core.height;
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
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)client_data;
    XExposeEvent *event = (XExposeEvent *)call_data;
    XRectangle region;
    Boolean cursorredraw;

    TRACE(("OverTheSpotConversion:TextRedisplay()\n"));
    region.x = event->x;
    region.y = event->y;
    region.width = event->width;
    region.height = event->height;

    cursorredraw = exposeCursor(ocw, w, &region);
    redrawSegments(ocw, w, &region);
    if (cursorredraw) showCursor(ocw);
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
    OverTheSpotConversionWidget ocw = (OverTheSpotConversionWidget)client_data;
    int current = (int)call_data;

    TRACE(("OverTheSpotConversion:SelectionSelected()\n"));
    XtPopdown(ocw->overthespot.selectionshell);
    ocw->overthespot.selectionpoppedup = False;
    ICSelectItem(ocw->ccontrol.inputobj, current);
}


/*
 *+ text drawing functions
 */

/*- computeDisplaySegments: compare old&new text and update segments/fragments -*/
static void
computeDisplaySegments(ocw)
OverTheSpotConversionWidget ocw;
{
    Widget inputobj = ocw->ccontrol.inputobj;
    int nnew = ICNumSegments(inputobj);
    int nold = ocw->overthespot.numsegments;
    ICString *newseg;
    DisplaySegment *dseg;
    DisplayLocation disploc;
    Cardinal nsame;
    int diff;
    int i;

    TRACE(("OverTheSpotConversion:computeDisplaySegments() nnew=%d\n", nnew));
    allocDisplaySegments(ocw, nnew);

    ocw->overthespot.overflowcanvas = NULL;

    disploc.x = disploc.y = 0;
    if (ocw->overthespot.canvaslist == NULL) {
	ocw->overthespot.canvaslist = CreateTextCanvas(ocw);
    }
    disploc.canvas = ocw->overthespot.canvaslist;

    for (i = 0, dseg = ocw->overthespot.dispsegments; i < nnew; i++, dseg++) {
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
		disploc.canvas != dseg->fragments->canvas ||
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
		    oldfragments->canvas == newfragments->canvas &&
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
		    oldfragments->canvas == newfragments->canvas &&
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

    ocw->overthespot.numsegments = nnew;
}

/*- recomputeDisplaySegments: recompute segments/fragments -*/
static void
recomputeDisplaySegments(ocw)
OverTheSpotConversionWidget ocw;
{
    int nseg = ocw->overthespot.numsegments;
    DisplaySegment *dseg;
    DisplayLocation disploc;
    int i;

    ocw->overthespot.overflowcanvas = NULL;

    disploc.x = disploc.y = 0;
    if (ocw->overthespot.canvaslist == NULL) {
	ocw->overthespot.canvaslist = CreateTextCanvas(ocw);
    }
    disploc.canvas = ocw->overthespot.canvaslist;

    for (i = 0, dseg = ocw->overthespot.dispsegments; i < nseg; i++, dseg++) {
	freeDisplayFragments(dseg->fragments);
	dseg->redrawpos = 0;
	dseg->fragments = computeDisplayFragments(ocw, &dseg->seg, &disploc);
    }
}

/*- computeLastPosition: get last position of the specified fragment -*/
static void
computeLastPosition(fragments, disploc)
DisplayFragment *fragments;
DisplayLocation *disploc;
{
    while (fragments->next != NULL) fragments = fragments->next;
    disploc->canvas = fragments->canvas;
    disploc->x = fragments->region.x + fragments->region.width;
    disploc->y = fragments->region.y;
}

/*- computeDisplayFragments: compute fragment(s) of the specified segment -*/
static DisplayFragment *
computeDisplayFragments(ocw, newseg, disploc)
OverTheSpotConversionWidget ocw;
ICString *newseg;
DisplayLocation *disploc;
{
    int start;
    int nchars;
    Widget dispobj = ocw->overthespot.displayobj;
    DisplayFragment *fragments, *dfp;
    int widthavailable;

    TRACE(("computeDisplayFragments()\n"));
    start = 0;
    fragments = NULL;
    while (start < newseg->nchars) {
	widthavailable = computeWidthAvailable(ocw, disploc);
	nchars = CDMaxChar(dispobj, newseg, start, widthavailable);
	if (nchars == 0) {
	    if (disploc->canvas->x <= CLAREA(ocw).x &&
		disploc->x == 0) {
		/*
		 * specified width is too narrow to display a character.
		 * we force to display at least one character per line.
		 */
		nchars = 1;
	    }
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
	    dfp->canvas = disploc->canvas;
	    dfp->region.x = disploc->x;
	    dfp->region.y = disploc->y;
	    dfp->region.width = CDStringWidth(dispobj, newseg, start,
						    start + nchars);
	    dfp->region.height = ocw->overthespot.lineheight;
	    dfp->next = NULL;

	    disploc->x += dfp->region.width;
	}
	start += nchars;
	if (start < newseg->nchars) nextLocation(ocw, disploc);
    }

    return fragments;
}

/*- computeWidthAvailable: return the width of the current line left for drawing -*/
/* ARGSUSED */
static int
computeWidthAvailable(ocw, disploc)
OverTheSpotConversionWidget ocw;
DisplayLocation *disploc;
{
    XRectangle *cregion = &CLAREA(ocw);

    if (disploc->canvas == ocw->overthespot.overflowcanvas) {
	 /* we pretend this canvas is veeeeeeery wide */
	return 9999;
    }
    return (cregion->x + cregion->width) - (disploc->canvas->x + disploc->x);
}

/*- nextLocation: return the position of the next line -*/
/* ARGSUSED */
static void
nextLocation(ocw, disploc)
OverTheSpotConversionWidget ocw;
DisplayLocation *disploc;
{
    XRectangle *cregion = &CLAREA(ocw);
    Position x, y;

    if (disploc->canvas->y + ocw->overthespot.linespacing * 2 >
	cregion->y + cregion->height) {
	/* no new canvas can create underneath this canvas */
	ocw->overthespot.overflowcanvas = disploc->canvas;
	return;
    }

    if (disploc->canvas->next == NULL) {
	disploc->canvas->next = CreateTextCanvas(ocw);
    }
    x = CLAREA(ocw).x;
    y = disploc->canvas->y + ocw->overthespot.linespacing;
    disploc->canvas = disploc->canvas->next;
    disploc->x = disploc->y = 0;

    disploc->canvas->x = x;
    disploc->canvas->y = y;
}

/*- findLocation: compute the display position of specified char -*/
static DisplayLocation *
findLocation(ocw, dsp, offset, disploc)
OverTheSpotConversionWidget ocw;
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

    disploc->canvas = dfp->canvas;
    disploc->x = dfp->region.x + CDStringWidth(ocw->overthespot.displayobj,
					       &dsp->seg, dfp->from,
					       dfp->from + offset);
    disploc->y = dfp->region.y;

    return disploc;
}

/*- reconfigureDisplay: do reconfiguration of text canvas (resize/popup/popdown) -*/
static void
reconfigureDisplay(ocw)
OverTheSpotConversionWidget ocw;
{
    DisplaySegment *dsp;
    DisplayFragment *dfp;
    TextCanvas *tcp, *lasttcp;
    Boolean shrink = ocw->overthespot.shrinkwindow;
    XRectangle *areap = &CLAREA(ocw);
    int i;

    for (tcp = ocw->overthespot.canvaslist; tcp != NULL; tcp = tcp->next) {
	tcp->maxx = tcp->maxy = 0;
	tcp->shouldpopup = False;
    }

    for (i = 0, dsp = ocw->overthespot.dispsegments; i < ocw->overthespot.numsegments; i++, dsp++) {
	for (dfp = dsp->fragments; dfp != NULL; dfp = dfp->next) {
	    tcp = dfp->canvas;
	    tcp->maxx = dfp->region.x + dfp->region.width;
	    tcp->maxy = dfp->region.y + dfp->region.height;
	    tcp->shouldpopup = True;
	}
    }

    lasttcp = NULL;
    for (tcp = ocw->overthespot.canvaslist; tcp != NULL; tcp = tcp->next) {
	if (tcp->maxx < tcp->canvas->core.width && XtIsRealized(tcp->canvas)) {
	    XClearArea(XtDisplay(tcp->canvas), XtWindow(tcp->canvas),
		       tcp->maxx, 0, 0, 0, False);
	}
	if (tcp->shouldpopup) lasttcp = tcp;
    }

    if (!ocw->overthespot.modelocationspecified &&
	ocw->overthespot.modeshell == ocw->overthespot.modeshell_fix) {
	/* ModeTrackText */
	if (lasttcp == NULL) {
	    locateTrackingModeWidget(ocw, True, 0, 0);
	} else {
	    locateTrackingModeWidget(ocw, False, lasttcp->x,
				     lasttcp->y + lasttcp->maxy);
	}
    }

    if (ocw->overthespot.cursorvisible) {
	DisplayLocation *dlp = &ocw->overthespot.cursorlocation;
	XRectangle cbounds;
	int x;

	tcp = dlp->canvas;
	CDGetCursorBounds(ocw->overthespot.displayobj, &cbounds);
	x = dlp->x +  cbounds.x + cbounds.width;
	if (x > tcp->maxx) tcp->maxx = x;
    }

    if (lasttcp != NULL &&
	lasttcp->x + lasttcp->maxx > areap->x + areap->width) {
	ocw->overthespot.overflowcanvas = lasttcp;
	adjustDisplay(ocw);
    }

    for (tcp = ocw->overthespot.canvaslist; tcp != NULL; tcp = tcp->next) {
	Arg args[2];
	int nargs = 0;
	if (tcp->shouldpopup && tcp->maxx > 0 && tcp->maxy > 0) {
	    if (tcp == ocw->overthespot.overflowcanvas) {
		XtMoveWidget(tcp->canvas,
			     tcp->x + ocw->overthespot.overflowoffset, tcp->y);
	    } else if (tcp->x != tcp->canvas->core.x ||
		       tcp->y != tcp->canvas->core.y) {
		XtMoveWidget(tcp->canvas, tcp->x, tcp->y);
	    }
	    if (shrink || !tcp->poppedup ||
		tcp->maxx > tcp->canvas->core.width) {
		XtSetArg(args[nargs], XtNwidth, tcp->maxx); nargs++;
	    }
	    if (!tcp->poppedup || tcp->maxy > tcp->canvas->core.height) {
		XtSetArg(args[nargs], XtNheight, tcp->maxy); nargs++;
	    }
	    if (nargs > 0) XtSetValues(tcp->canvas, args, nargs);

	    if (!tcp->poppedup) {
		TRACE(("reconfigureDisplay(): canvas popup\n"));
		XtPopup(tcp->canvas, XtGrabNone);
		tcp->poppedup = True;
	    }
	} else {
	    if (tcp->poppedup) {
		TRACE(("reconfigureDisplay(): canvas popdown\n"));
		XtPopdown(tcp->canvas);
		tcp->poppedup = False;
	    }
	}
    }
}

/*- updateDisplay: redraw text (if needed) -*/
static void
updateDisplay(ocw)
OverTheSpotConversionWidget ocw;
{
    Widget dispobj = ocw->overthespot.displayobj;
    DisplaySegment *dsp = ocw->overthespot.dispsegments;
    int i;

    for (i = 0; i < ocw->overthespot.numsegments; i++, dsp++) {
	if (dsp->redrawpos >= 0) {
	    TRACE(("updateDisplaySegment(seg#=%d)\n", i));
	    updateDisplaySegment(dispobj, dsp);
	}
    }
}

/*- updateDisplaySegment: redraw specified segment (if needed) -*/
static void
updateDisplaySegment(dispobj, dsp)
Widget dispobj;
DisplaySegment *dsp;
{
    DisplayFragment *dfp = dsp->fragments;
    int from;
    int x;

    while (dfp != NULL) {
	if (dsp->redrawpos < dfp->from + dfp->nchars) {
	    from = (dsp->redrawpos > dfp->from) ? dsp->redrawpos : dfp->from;
	    x = dfp->region.x;
	    if (from > dfp->from) {
		x += CDStringWidth(dispobj, &dsp->seg, dfp->from, from);
	    } 
	    CDDrawString(dispobj, dfp->canvas->canvas, &dsp->seg,
			 from, dfp->from + dfp->nchars,
			 x, dfp->region.y);
	}
	dfp = dfp->next;
    }
}

/*- redrawSegments: redraw segments in specified area -*/
static void
redrawSegments(ocw, canvas, region)
OverTheSpotConversionWidget ocw;
Widget canvas;
XRectangle *region;
{
    DisplaySegment *dsp = ocw->overthespot.dispsegments;
    DisplayFragment *dfp;
    Widget dispobj = ocw->overthespot.displayobj;
    int i;

    for (i = 0; i < ocw->overthespot.numsegments; i++, dsp++) {
	for (dfp = dsp->fragments; dfp != NULL; dfp = dfp->next) {
	    if (dfp->canvas->canvas == canvas &&
		intersectRect(&dfp->region, region)) {
		CDDrawString(dispobj, canvas, &dsp->seg,
			      dfp->from, dfp->from + dfp->nchars,
			      dfp->region.x, dfp->region.y);
	    }
	}
    }
}

/*
 *+ handle overflow canvas functions
 */

/*- adjustDisplay: compute appropriate offset for the overflow canvas -*/
static void
adjustDisplay(ocw)
OverTheSpotConversionWidget ocw;
{
    Position outerleft, outerright, innerleft, innerright;
    TextCanvas *overflowcanvas = ocw->overthespot.overflowcanvas;
    Cardinal curseg;
    Cardinal curoffset;
    XRectangle *areap;
    Position offset;

    TRACE(("adjustDisplay()\n"));
    ocw->overthespot.overflowoffset = 0;

    /*
     * ストラテジとしては
     * カレントセグメント・カレントサブセグメント・インサートカーソルのある
     * セグメントのどれもなければ気にしない
     * インサートカーソルがあればそれを最優先する。つまりインサートカーソル
     * は何があっても表示するようにする。
     * できればインサートカーソルのあるセグメントはすべて表示する。
     */

    outerleft = innerleft = 9999;
    outerright = innerright = 0;

    if (ICCursorPos(ocw->ccontrol.inputobj, &curseg, &curoffset) == 1) {
	(void)getInsertingSegmentRange(ocw, overflowcanvas,
				       curseg, curoffset,
				       &outerleft, &outerright, &innerleft);
	if (outerleft <= outerright) innerright = innerleft + 2; /* XXX */
    } else {
	(void)getAttributeSegmentRange(ocw, overflowcanvas,
				       ICAttrCurrentSegment,
				       &innerleft, &innerright);
	(void)getAttributeSegmentRange(ocw, overflowcanvas,
				       ICAttrCurrentSubSegment,
				       &outerleft, &outerright);
    }

    if (outerleft > outerright && innerleft > innerright) {
	/* no important segments is on the overflow canvas */
	return;
    }

    if (outerleft > innerleft) outerleft = innerleft;
    if (outerright < innerright) outerright = innerright;

    areap = &CLAREA(ocw);

    if (areap->x <= outerleft && outerright <= areap->x + areap->width) {
	/* important part fits in the visible area */
	return;
    }

    offset = 0;
    adjustOffset(areap, outerleft, outerright, &offset, False);
    adjustOffset(areap, innerleft, innerright, &offset, True);
    ocw->overthespot.overflowoffset = offset;
}

/*- getAttributeSegmentRange: compute span of segments which has the specified attributes -*/
static Boolean
getAttributeSegmentRange(ocw, canvas, attr, leftp, rightp)
OverTheSpotConversionWidget ocw;
TextCanvas *canvas;
int attr;
Position *leftp;
Position *rightp;
{
    int nsegs = ocw->overthespot.numsegments;
    DisplaySegment *dseg = ocw->overthespot.dispsegments;
    DisplayFragment *dfp;
    Position left, right;

    left = 32767;
    right = 0;

    while (nsegs-- > 0) {
	if (dseg->seg.attr & attr) {
	    dfp = dseg->fragments;

	    while (dfp != NULL) {
		if (dfp->canvas == canvas) {
		    if (dfp->region.x < left) left = dfp->region.x;
		    if (right < dfp->region.x + dfp->region.width) {
			right = dfp->region.x + dfp->region.width;
		    }
		}
		dfp = dfp->next;
	    }
	}
	dseg++;
    }

    if (left > right) return False;

    *leftp = left + canvas->x;
    *rightp = right + canvas->x;
    return True;
}

/*- getInsertingSegmentRange: compute span of segments which has insert cursor -*/
static Boolean
getInsertingSegmentRange(ocw, canvas, curseg, offset, leftp, rightp, posp)
OverTheSpotConversionWidget ocw;
TextCanvas *canvas;
Cardinal curseg;
Cardinal offset;
Position *leftp;
Position *rightp;
Position *posp;
{
    DisplaySegment *dseg = ocw->overthespot.dispsegments + curseg;
    DisplayFragment *dfp;
    Position left, right, insert;

    left = 32767;
    right = 0;

    dfp = dseg->fragments;

    while (dfp != NULL) {
	if (dfp->canvas == canvas &&
	    dfp->from <= offset && offset <= dfp->from + dfp->nchars) {
	    if (dfp->region.x < left) left = dfp->region.x;
	    if (right < dfp->region.x + dfp->region.width) {
		right = dfp->region.x + dfp->region.width;
	    }

	    if (offset == dfp->from) {
		insert = dfp->region.x;
	    } else if (offset == dfp->from + dfp->nchars) {
		insert = dfp->region.x + dfp->region.width;
	    } else {
		insert = dfp->region.x +
		         CDStringWidth(ocw->overthespot.displayobj,
				       &dseg->seg, dfp->from,
				       offset);
	    }
	    break;
	}
	dfp = dfp->next;
    }

    if (left > right) return False;

    *leftp = left + canvas->x;
    *rightp = right + canvas->x;
    *posp = insert + canvas->x;
    return True;
}

/*- adjustOffset: make the span fit within the specified area -*/
static void
adjustOffset(rectp, left, right, offsetp, force)
XRectangle *rectp;
Position left;
Position right;
Position *offsetp;
Boolean force;
{
    Position offset = *offsetp;

    if (rectp->x <= left + offset &&
	right + offset <= rectp->x + rectp->width) return;

    if (right - left > rectp->width) {
	if (!force) return;
	/* centering */
	offset = (rectp->x + rectp->width / 2) - (right - left) / 2;
    } else {
	if (left + offset < rectp->x) {
	    offset = rectp->x - left;
	} else if (rectp->x + rectp->width < right + offset) {
	    offset = rectp->x + rectp->width - right;
	}
    }
    *offsetp = offset;
    return;
}


/*
 *+ insert cursor handling functions
 */

/*- eraseCursor: erase insert cursor -*/
static void
eraseCursor(ocw)
OverTheSpotConversionWidget ocw;
{
    if (!ocw->overthespot.cursorvisible) return;

    TRACE(("eraseCursor() at (%d,%d)\n",
	    ocw->overthespot.cursorlocation.x,
	    ocw->overthespot.cursorlocation.y));
    CDDrawCursor(ocw->overthespot.displayobj,
		 ocw->overthespot.cursorlocation.canvas->canvas,
		 ocw->overthespot.cursorlocation.x,
		 ocw->overthespot.cursorlocation.y,
		 False);
    ocw->overthespot.cursorvisible = False;
}

/*- showCursor: draw insert cursor -*/
static void
showCursor(ocw)
OverTheSpotConversionWidget ocw;
{
    if (!ocw->overthespot.cursorvisible) return;

    TRACE(("showCursor at (%d,%d)\n",
	    ocw->overthespot.cursorlocation.x,
	    ocw->overthespot.cursorlocation.y));
    CDDrawCursor(ocw->overthespot.displayobj,
		 ocw->overthespot.cursorlocation.canvas->canvas,
		 ocw->overthespot.cursorlocation.x,
		 ocw->overthespot.cursorlocation.y,
		 True);
}

/*- exposeCursor: make the insert cursor redraw correctly when exposing -*/
static Boolean
exposeCursor(ocw, w, region)
OverTheSpotConversionWidget ocw;
Widget w;
XRectangle *region;
{
    XRectangle bounds;

    if (!ocw->overthespot.cursorvisible ||
	w != ocw->overthespot.cursorlocation.canvas->canvas) return False;

    TRACE(("exposeCursor(region=%d,%d-%d,%d)\n",
	    region->x, region->y, region->width, region->height));
    /*
     * if a part of the insert cursor is in the exposing region,
     * clear the entire cursor before redraw, since the cursor is
     * drawn with xor mode.
     */
    CDGetCursorBounds(ocw->overthespot.displayobj, &bounds);
    bounds.x += ocw->overthespot.cursorlocation.x;
    bounds.y += ocw->overthespot.cursorlocation.y;
    if (intersectRect(region, &bounds)) {
	eraseCursor(ocw);
	XClearArea(XtDisplay(w), XtWindow(w),
		   bounds.x, bounds.y, bounds.width, bounds.height, False);
	unionRect(region, &bounds, region);
    }
    ocw->overthespot.cursorvisible = True;
    return True;
}

/*- computeCursor: compute insert cursor position if visible -*/
static void
computeCursor(ocw)
OverTheSpotConversionWidget ocw;
{
    DisplaySegment *dsp;
    DisplayLocation disploc;
    Cardinal seg, offset;

    if (!ICCursorPos(ocw->ccontrol.inputobj, &seg, &offset)) {
	ocw->overthespot.cursorvisible = False;
	return;
    }

    /* sanity check */
    if (seg >= ocw->overthespot.numsegments) return;
    dsp = ocw->overthespot.dispsegments + seg;
    if (offset > dsp->seg.nchars) return;

    if (findLocation(ocw, dsp, offset, &disploc) == NULL) return;

    disploc.y += ocw->overthespot.ascent;

    ocw->overthespot.cursorvisible = True;
    ocw->overthespot.cursorlocation = disploc;
}

/*
 *+ resource converter
 */

/*- StringToModeLocation: string->mode-location resource converter -*/
/* ARGSUSED */
static void
StringToModeLocation(args, num_args, from, to)
XrmValue *args;
Cardinal *num_args;
XrmValue *from;
XrmValue *to;
{
    char *s = (char *)from->addr;
    static ModeLocation ml = ModeBottomLeft;

    if (!XmuCompareISOLatin1(s, "topleft")) ml = ModeTopLeft;
    else if (!XmuCompareISOLatin1(s, "topright")) ml = ModeTopRight;
    else if (!XmuCompareISOLatin1(s, "bottomleft")) ml = ModeBottomLeft;
    else if (!XmuCompareISOLatin1(s, "bottomright")) ml = ModeBottomRight;
    else if (!XmuCompareISOLatin1(s, "tracktext")) ml = ModeTrackText;
    else {
	XtStringConversionWarning(s, XtRModeLocation);
    }

    to->size = sizeof(ModeLocation);
    to->addr = (caddr_t)&ml;
}

/*
 *+ miscellaneous functions
 */

/*- MoveShell: move shell widget -*/
static void
MoveShell(w, x, y)
Widget w;
Position x;
Position y;
{
    ShellWidget shell = (ShellWidget)w;

    TRACE(("MoveShell(%s,x=%d,y=%d,core.x=%d,core.y=%d)\n",XtName(w),x,y,w->core.x,w->core.y));
    XtCheckSubclass(w, shellWidgetClass,
		    "MoveShell: specified widget is not a shell");
    /*
     * calling XtMoveWidget() is NOT enough to move shell widgets when
     * they are not mapped. we must use XtMakeGeometryRequest() or
     * XtSetValues() to invoke root-geometry-manager which modifies
     * the size hint appropriately.
     */
    if (shell->shell.popped_up) {
	XtMoveWidget(w, x, y);
    } else {
	XtWidgetGeometry req;

	req.request_mode = CWX | CWY;
	req.x = x;
	req.y = y;
	(void)XtMakeGeometryRequest(w, &req, (XtWidgetGeometry *)NULL);
    }
}

/*- getToplevelWindow: get top-level window of a given window -*/
static Window
getToplevelWindow(dpy, win, wm_state)
Display *dpy;
Window win;
Atom wm_state;
{
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

/*- getFocusOffset: get the focus window's position relative to the client window -*/
static void
getFocusOffset(ocw)
OverTheSpotConversionWidget ocw;
{
    int offx, offy;
    Window junkw;

    if (ocw->ccontrol.focuswindow == ocw->ccontrol.clientwindow) {
	FOCUSOFFX(ocw) = 0;
	FOCUSOFFY(ocw) = 0;
	return;
    }
    XTranslateCoordinates(XtDisplay((Widget)ocw),
			  ocw->ccontrol.focuswindow,
			  ocw->ccontrol.clientwindow,
			  0, 0, &offx, &offy, &junkw);
    FOCUSOFFX(ocw) = offx;
    FOCUSOFFY(ocw) = offy;
}
   
/*- intersectRect: returns given rectangles have a intersection -*/
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

/*- unionRect: returns minimum rectangle that covers given rectangles -*/
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

/*- enoughSpaceForStatus: checks if there's enough space for the status display -*/
static int
enoughSpaceForStatus(ocw)
OverTheSpotConversionWidget ocw;
{
    Widget modew = ocw->overthespot.modewidget_fix;
    int modespace;
    int ascent = ocw->overthespot.ascent;
    int descent = ocw->overthespot.lineheight - ascent;
    int lspace = ocw->overthespot.linespacing;
    int areatop = CLAREA(ocw).y;
    int areabottom = CLAREA(ocw).y + CLAREA(ocw).height;
    int top, bottom, y;

    if (lspace == 0) lspace = 1;	/* avoid "divide by zero" error */

    /*
     * tracking ステータスが表示できるかどうかチェックするには、クライ
     * アントエリアに表示できる最初と最後の行についてその上か下にステー
     * タスが表示できることを調べればよい。
     */

    modespace = modew->core.height + modew->core.border_width * 2 + 2;

    /* 最初の行の上下の Y 座標を計算してステータスが表示できるか調べる */
    y = SPOTY(ocw) - ascent;
    top = y - ((y - areatop) / lspace) * lspace;
    bottom = top + ascent + descent;
    if (top - areatop < modespace && areabottom - bottom < modespace) {
	return 0;
    }

    /* 最後の行の上下の Y 座標を計算してステータスが表示できるか調べる */
    y = SPOTY(ocw) + descent;
    bottom = y + ((areabottom - y) / lspace) * lspace;
    top = bottom - (ascent + descent);
    if (top - areatop < modespace && areabottom - bottom < modespace) {
	return 0;
    }

    return 1;
}

static DisplayFragment *free_fragments = NULL;

/*- allocDisplayFragment: get new fragment -*/
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

/*- freeDisplayFragments: add specified fragment list to free-list -*/
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

/*- destroyDisplayFragments: free specified fragment list -*/
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
OverTheSpotConversionWidget ocw;
int n;
{
    if (ocw->overthespot.dispsegmentsize > n) return;
    n = ((n + 3) / 4) * 4 ;
    if (ocw->overthespot.dispsegments == NULL) {
	ocw->overthespot.dispsegments = (DisplaySegment *)XtMalloc(n * sizeof(DisplaySegment));
    } else {
	ocw->overthespot.dispsegments = (DisplaySegment *)XtRealloc((char *)ocw->overthespot.dispsegments, n * sizeof(DisplaySegment));
    }
    ocw->overthespot.dispsegmentsize = n;
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
OverTheSpotConversionWidget ocw;
{
    DisplaySegment *dsp = ocw->overthespot.dispsegments;
    int i;

    for (i = 0; i < ocw->overthespot.numsegments; i++) {
	freeDisplaySegment(dsp++);
    }
    ocw->overthespot.numsegments = 0;
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
