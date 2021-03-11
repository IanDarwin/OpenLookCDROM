#ifndef lint
static char *rcsid = "$Id: ConvCtrl.c,v 1.49 1994/06/02 02:21:33 ishisone Rel $";
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
#include <X11/Xmu/CharSet.h>
#include "ConvCtrlP.h"
#include "InputConv.h"
#include "ConvDisp.h"
#include "MyDispatch.h"
#include "AsyncErr.h"

#define DEBUG_VAR debug_ConversionControl
#include "DebugPrint.h"

static XtResource resources[] = {
#define offset(field) XtOffset(ConversionControlWidget, ccontrol.field)
    { XtNinputObject, XtCInputObject, XtRWidget, sizeof(Widget),
	offset(inputobj), XtRImmediate, (XtPointer)NULL },
    { XtNinputObjectClass, XtCInputObjectClass,
	XtRPointer, sizeof(WidgetClass),
	offset(inputobjclass), XtRImmediate, (XtPointer)NULL },
    { XtNdisplayObjectClass, XtCDisplayObjectClass,
	XtRPointer, sizeof(WidgetClass),
	offset(displayobjclass), XtRImmediate, (XtPointer)NULL },
    { XtNclientWindow, XtCWindow, XtRWindow, sizeof(Window),
	offset(clientwindow), XtRImmediate, (XtPointer)None },
    { XtNfocusWindow, XtCWindow, XtRWindow, sizeof(Window),
	offset(focuswindow), XtRImmediate, (XtPointer)None },
    { XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
	offset(cursor), XtRImmediate, (XtPointer)None },
    { XtNeventSelectMethod, XtCEventSelectMethod,
	XtREventSelectMethod, sizeof(EventSelectMethod),
	offset(eventselectmethod), XtRString, (XtPointer)"none" },
    { XtNtextEncoding, XtCTextEncoding, XtRAtom, sizeof(Atom),
	offset(textencoding), XtRString, "COMPOUND_TEXT" },
    { XtNtextCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(textcallback), XtRCallback, (XtPointer)NULL },
    { XtNnewTextCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(newtextcallback), XtRCallback, (XtPointer)NULL },
    { XtNendCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(endcallback), XtRCallback, (XtPointer)NULL },
    { XtNunusedEventCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(unusedeventcallback), XtRCallback, (XtPointer)NULL },
    { XtNsendbackKeyPress, XtCSendbackEvent, XtRBoolean, sizeof(Boolean),
	offset(sendbackKeyPress), XtRString, (XtPointer)"False" },
#undef offset
};

static void EventToInputObject();

static XtActionsRec actions[] = {
    {"to-inputobj",	EventToInputObject },
};

static char translations[] = "<Key>: to-inputobj()";

static void ClassInitialize();
static void StringToESM();
static void ClassPartInitialize();
static void Initialize(), Destroy();
static void Realize();
static void Resize();
static Boolean SetValues();

static void ConversionStartup();
static void ConversionFinish();
static void ChangeAttributes();
static void ChangeFocus();
static void TextChange();
static void ModeChange();
static void SelectionControl();
static void AuxControl();

static void GetClientCoordinates();

static Widget CreateInputObject();

static Boolean ClassIsSubClassOf();

static void CaptureClientDead();
static void InterceptClientKeyEvent();
static void SelectFocusKeyEvent();
static void UnselectFocusKeyEvent();
static void ClientKey();
static void ClientDead();

static Boolean SafeGetWindowAttributes();
static void CheckAttributes();
static void CheckCoordinates();
static Boolean clipRectangle();

static void FixCallback();
static void ConversionEndCallback();
static void TextChangeCallback();
static void ModeChangeCallback();
static void SelectionControlCallback();
static void AuxControlCallback();

static void WidgetError(), WidgetWarning();

static CompositeClassExtensionRec CompositeExtension = {
    /* next_extension		*/	NULL,
    /* record_type		*/	NULLQUARK,
    /* version			*/	XtCompositeExtensionVersion,
    /* record_size		*/	sizeof(CompositeClassExtensionRec),
    /* accept_objects		*/	True,
};

ConversionControlClassRec conversionControlClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &transientShellClassRec,
    /* class_name		*/	"ConversionControl",
    /* widget_size		*/	sizeof(ConversionControlRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	ClassPartInitialize,
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
    /* TextChange		*/	TextChange,
    /* ModeChange		*/	ModeChange,
    /* SelectionControl		*/	SelectionControl,
    /* AuxControl		*/	AuxControl,
  }
};

WidgetClass conversionControlWidgetClass = (WidgetClass)&conversionControlClassRec;

/* ARGSUSED */
static void
ClassInitialize()
{
    /* add String -> EventSelectionMethod converter */
    XtAddConverter(XtRString, XtREventSelectMethod, StringToESM,
		   (XtConvertArgList)NULL, (Cardinal)0);
}

/* ARGSUSED */
static void
StringToESM(args, num_args, from, to)
XrmValue *args;
Cardinal *num_args;
XrmValue *from;
XrmValue *to;
{
    char *s = (char *)from->addr;
    static EventSelectMethod esm = ESMethodNone;

    if (!XmuCompareISOLatin1(s, "inputonly")) {
	esm = ESMethodInputOnly;
    } else if (!XmuCompareISOLatin1(s, "selectfocus")) {
	esm = ESMethodSelectFocus;
    } else if (!XmuCompareISOLatin1(s, "none")) {
	esm = ESMethodNone;
    } else {
	XtStringConversionWarning(s, XtREventSelectMethod);
    }

    to->size = sizeof(EventSelectMethod);
    to->addr = (caddr_t)&esm;
}

static void
ClassPartInitialize(cl)
WidgetClass cl;
{
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)cl;
    ConversionControlWidgetClass super = (ConversionControlWidgetClass)class->core_class.superclass;

#define ccclass conversionControl_class
    if (class->ccclass.Startup == XtInheritStartup) {
	class->ccclass.Startup = super->ccclass.Startup;
    }
    if (class->ccclass.Finish == XtInheritFinish) {
	class->ccclass.Finish = super->ccclass.Finish;
    }
    if (class->ccclass.ChangeAttributes == XtInheritChangeAttributes) {
	class->ccclass.ChangeAttributes = super->ccclass.ChangeAttributes;
    }
    if (class->ccclass.ChangeFocus == XtInheritChangeFocus) {
	class->ccclass.ChangeFocus = super->ccclass.ChangeFocus;
    }
    if (class->ccclass.TextChange == XtInheritTextChange) {
	class->ccclass.TextChange = super->ccclass.TextChange;
    }
    if (class->ccclass.ModeChange == XtInheritModeChange) {
	class->ccclass.ModeChange = super->ccclass.ModeChange;
    }
    if (class->ccclass.SelectionControl == XtInheritSelectionControl) {
	class->ccclass.SelectionControl = super->ccclass.SelectionControl;
    }
    if (class->ccclass.AuxControl == XtInheritAuxControl) {
	class->ccclass.AuxControl = super->ccclass.AuxControl;
    }
#undef ccclass
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    ConversionControlWidget ccw = (ConversionControlWidget)new;

    /*
     * check inputobj/inputobjclass resource
     */

    if (ccw->ccontrol.inputobj == NULL) {
	/* if inputobj not specified, inputobjclass must be specified */
	if (ccw->ccontrol.inputobjclass == NULL) {
	    WidgetError(new, "noResourceError", "inputObjectClass",
			"either inputObject or inputObjectClass must be specified at creation time");
	} else if (!ClassIsSubClassOf(ccw->ccontrol.inputobjclass,
				      inputConvObjectClass)) {
	    WidgetError(new, "classError", "inputObjectClass",
			"inputObjectClass must be subclass of inputConvObjectClass");
	}
	(void)CreateInputObject(ccw);
	ccw->ccontrol.createinputobj = True;

    } else if (!XtIsSubclass(ccw->ccontrol.inputobj, inputConvObjectClass)) {
	WidgetError(new, "classError", "inputObject",
		    "inputObject must be subclass of inputConvObjectClass");
    }

    if (ccw->ccontrol.displayobjclass == NULL) {
	WidgetError(new, "noResourceError", "displayObjectClass",
		    "displayObjectClass must be specified");
    } else if (!ClassIsSubClassOf(ccw->ccontrol.displayobjclass,
				  convDisplayObjectClass)) {
	WidgetError(new, "classError", "displayObjectClass",
		    "displayObjectClass must be subclass of convDisplayObjectClass");
    }

    ccw->ccontrol.active = False;
    ccw->ccontrol.oldclientwindow = None;
    ccw->ccontrol.probewindow = None;
}

static void
Destroy(w)
Widget w;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;
    Display *dpy = XtDisplay(w);

    if (ccw->ccontrol.active == False) return;

    if (ccw->ccontrol.clientwindow != None) {
	MyRemoveAllEventHandler(dpy, ccw->ccontrol.clientwindow);
    }

    if (ccw->ccontrol.probewindow != None) {
	MyRemoveAllEventHandler(dpy, ccw->ccontrol.probewindow);
	XDestroyWindow(dpy, ccw->ccontrol.probewindow);
    }
}

static void
Realize(w, maskp, attr)
Widget w;
XtValueMask *maskp;
XSetWindowAttributes *attr;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;

    if (ccw->ccontrol.cursor != None) {
	attr->cursor = ccw->ccontrol.cursor;
	*maskp |= CWCursor;
    }

    /* call super class's realize function */
    (*conversionControlWidgetClass->core_class.superclass->core_class.realize)(w, maskp, attr);
}

static void
Resize(w)
Widget w;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;
    Widget child;
    int i;

    TRACE(("ConversionControl:Resize()\n"));

    /* ignore non-widgets */
    for (i = 0; i < ccw->composite.num_children; i++) {
	child = ccw->composite.children[i];
	if (XtIsWidget(child) && child->core.managed) {
	    XtResizeWidget(child, ccw->core.width, ccw->core.height,
			   child->core.border_width);
	}
    }
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
    ConversionControlWidget old = (ConversionControlWidget)cur;
    ConversionControlWidget new = (ConversionControlWidget)wid;

    if (new->ccontrol.active) {
	if (old->ccontrol.inputobj != new->ccontrol.inputobj) {
	    WidgetWarning(wid, "setValuesError", "inputObject",
			  "inputObject resource can't be changed during conversion");
	    new->ccontrol.inputobj = old->ccontrol.inputobj;	/* restore */
	}
	if (old->ccontrol.eventselectmethod != new->ccontrol.eventselectmethod) {
	    WidgetWarning(wid, "setValuesError", "eventSelectionMethod",
			  "eventSelectionMethod resource can't be changed during conversion");
	    new->ccontrol.eventselectmethod = old->ccontrol.eventselectmethod; /* restore */
	}
    }

    if (new->ccontrol.clientwindow != old->ccontrol.clientwindow ||
	new->ccontrol.focuswindow != old->ccontrol.focuswindow) {
	WidgetWarning(wid, "setValuesError", "clientWindow",
		      "clientWindow and focusWindow resources are read-only");
	new->ccontrol.clientwindow = old->ccontrol.clientwindow; /* restore */
	new->ccontrol.focuswindow = old->ccontrol.focuswindow; /* restore */
    }

    if (new->ccontrol.cursor != old->ccontrol.cursor && XtIsRealized(wid)) {
	XDefineCursor(XtDisplay(wid), XtWindow(wid), new->ccontrol.cursor);
    }

    return False;
}

/* ARGSUSED */
static void
ConversionStartup(w, mask, value)
Widget w;
unsigned long mask;
ConversionAttributes *value;
{
    /* do nothing */
}

/* ARGSUSED */
static void
ConversionFinish(w)
Widget w;
{
    /* do nothing */
}

/* ARGSUSED */
static void
ChangeAttributes(w, mask, value)
Widget w;
unsigned long mask;
ConversionAttributes *value;
{
    /* do nothing */
}

/* ARGSUSED */
static void
ChangeFocus(w, set)
Widget w;
int set;
{
    /* do nothing */
}

/* ARGSUSED */
static void
TextChange(w)
Widget w;
{
    /* do nothing */
}

/* ARGSUSED */
static void
ModeChange(w)
Widget w;
{
    /* do nothing */
}

/* ARGSUSED */
static void
SelectionControl(w, controlarg)
Widget w;
ICSelectionControlArg *controlarg;
{
    /* do nothing */
}

/* ARGSUSED */
static void
AuxControl(w, controlarg)
Widget w;
ICAuxControlArg *controlarg;
{
    /* do nothing */
}

/*
 * public functions
 */

void
CControlStartConversion(w, clientwindow, valuemask, value)
Widget w;
Window clientwindow;
unsigned long valuemask;
ConversionAttributes *value;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)w->core.widget_class;

    TRACE(("CControlStartConversion(clientwindow=%lx)\n", clientwindow));
    if (ccw->ccontrol.active) {
	WidgetWarning(w, "busyError", "CControlStartConversion",
		      "is busy. can't start conversion");
	return;
    }
    if (clientwindow == None) {
	/* ouch */
	WidgetWarning(w, "dataError", "cControlStartConversion",
		     "clientWindow not specified. can't start conversion.");
	return;
    }

    /* check clientWindow's existance */
    if (!SafeGetWindowAttributes(XtDisplay(w), clientwindow,
				 &(ccw->ccontrol.client_attr))) {
	WidgetWarning(w, "badWindowError", "clientWindow",
		      "clientWindow does not exist. can't start conversion.");
	return;
    }

    ICClearConversion(ccw->ccontrol.inputobj);
    ccw->ccontrol.notext = ICNumSegments(ccw->ccontrol.inputobj) == 0;

    ccw->ccontrol.active = True;
    ccw->ccontrol.clientwindow = clientwindow;

    /* check given attributes */
    CheckAttributes(ccw, &valuemask, value);

    if (valuemask & CAFocusWindow) {
	ccw->ccontrol.focuswindow = value->focuswindow;
    } else {
	ccw->ccontrol.focuswindow = clientwindow;
	ccw->ccontrol.focus_attr = ccw->ccontrol.client_attr;
    }

    if (ccw->ccontrol.eventselectmethod == ESMethodInputOnly) {
	InterceptClientKeyEvent(ccw);
    } else if (ccw->ccontrol.eventselectmethod == ESMethodSelectFocus) {
	SelectFocusKeyEvent(ccw);
    }

    CheckCoordinates(ccw, &valuemask, value, 1);

    GetClientCoordinates(ccw);

    CaptureClientDead(ccw);

    XtAddCallback(ccw->ccontrol.inputobj,
		  XtNfixNotify, FixCallback, (XtPointer)ccw);
    XtAddCallback(ccw->ccontrol.inputobj,
		  XtNendNotify, ConversionEndCallback, (XtPointer)ccw);
    XtAddCallback(ccw->ccontrol.inputobj,
		  XtNtextChangeNotify, TextChangeCallback, (XtPointer)ccw);
    XtAddCallback(ccw->ccontrol.inputobj,
		  XtNmodeChangeNotify, ModeChangeCallback, (XtPointer)ccw);
    XtAddCallback(ccw->ccontrol.inputobj,
		  XtNselectionControl, SelectionControlCallback,
		  (XtPointer)ccw);
    XtAddCallback(ccw->ccontrol.inputobj,
		  XtNauxControl, AuxControlCallback,
		  (XtPointer)ccw);

    /* call input style dependent startup */
    (*class->conversionControl_class.Startup)(w, valuemask, value);
}

void
CControlEndConversion(w)
Widget w;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)w->core.widget_class;
    Display *dpy = XtDisplay(w);

    if (!ccw->ccontrol.active) {
	WidgetWarning(w, "busyError", "cControlEndConversion",
		      "is not active. can't stop conversion");
	return;
    }

    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNfixNotify, FixCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNendNotify, ConversionEndCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNtextChangeNotify, TextChangeCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNmodeChangeNotify, ModeChangeCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNselectionControl, SelectionControlCallback,
		     (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNauxControl, AuxControlCallback,
		     (XtPointer)ccw);

    ICClearConversion(ccw->ccontrol.inputobj);

    MyRemoveEventHandler(dpy, ccw->ccontrol.clientwindow, DestroyNotify,
			 ClientDead, (XtPointer)ccw);

    if (ccw->ccontrol.probewindow != None) {
	MyRemoveAllEventHandler(dpy, ccw->ccontrol.probewindow);
	XDestroyWindow(dpy, ccw->ccontrol.probewindow);
	ccw->ccontrol.probewindow = None;
    }

    /* unselect focuswindow events */
    if (ccw->ccontrol.eventselectmethod == ESMethodSelectFocus) {
	UnselectFocusKeyEvent(ccw);
    }

    ccw->ccontrol.active = False;

    /* call input style dependent finish */
    (*class->conversionControl_class.Finish)(w);

    ccw->ccontrol.oldclientwindow = ccw->ccontrol.clientwindow;
    ccw->ccontrol.clientwindow = None;
}

void
CControlChangeAttributes(w, valuemask, value)
Widget w;
unsigned long valuemask;
ConversionAttributes *value;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)w->core.widget_class;

    if (!ccw->ccontrol.active) {
	WidgetWarning(w, "busyError", "cControlChangeAttributes",
		      "is not active. can't change attributes");
	return;
    }

    CheckAttributes(ccw, &valuemask, value);
    CheckCoordinates(ccw, &valuemask, value, 0);

    if (valuemask == 0L) return;

    if (valuemask & CAFocusWindow) {
	if (ccw->ccontrol.eventselectmethod == ESMethodSelectFocus) {
	    UnselectFocusKeyEvent(ccw);
	}
	ccw->ccontrol.focuswindow = value->focuswindow;
	if (ccw->ccontrol.eventselectmethod == ESMethodSelectFocus) {
	    SelectFocusKeyEvent(ccw);
	}
    }

    (*class->conversionControl_class.ChangeAttributes)(w, valuemask, value);
}

void
CControlChangeFocus(w, set)
Widget w;
int set;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)w->core.widget_class;

    if (!ccw->ccontrol.active) {
	WidgetWarning(w, "busyError", "cControlChangeFocus",
		      "is not active. can't change focus");
	return;
    }

    (*class->conversionControl_class.ChangeFocus)(w, set);
}

static Boolean
SafeGetWindowAttributes(dpy, w, attr)
Display *dpy;
Window w;
XWindowAttributes *attr;
{
    XAEHandle h;
    unsigned long errbits = 0;

    h = XAESetRecordErrors(dpy, &errbits);
    (void)XGetWindowAttributes(dpy, w, attr);
    XAEUnset(h);

    return (errbits == 0);
}

static void
CheckAttributes(ccw, valuemaskp, value)
ConversionControlWidget ccw;
unsigned long *valuemaskp;
ConversionAttributes *value;
{
    Display *dpy = XtDisplay((Widget)ccw);
    XAEHandle h;
    unsigned long ebits = 0;

#define CHECKATTRS (CAFocusWindow|CAColormap|CACursor|CABackgroundPixmap)
    if ((*valuemaskp & CHECKATTRS) == 0) return;
#undef CHECKATTRS

#define XERROR(e)	(ebits & (1 << (e)))

    h = XAESetRecordErrors(dpy, &ebits);

    if (*valuemaskp & (CAColormap|CACursor|CABackgroundPixmap)) {
	XSetWindowAttributes attr;
	Window w;

	w = XCreateSimpleWindow(dpy, ccw->ccontrol.clientwindow,
				0, 0, 1, 1, 0, 0L, 0L);

	if (*valuemaskp & CACursor) {
	    /* BadCursor */
	    attr.cursor = value->cursor;
	    XChangeWindowAttributes(dpy, w, CWCursor, &attr);
	}
	if (*valuemaskp & CAColormap) {
	    /* BadMatch or BadColormap */
	    attr.colormap = value->colormap;
	    XChangeWindowAttributes(dpy, w, CWColormap, &attr);
	}
	if (*valuemaskp & CABackgroundPixmap) {
	    /* BadMatch or BadPixmap */
	    attr.background_pixmap = value->background_pixmap;
	    XChangeWindowAttributes(dpy, w, CWBackPixmap, &attr);
	}

	XDestroyWindow(dpy, w);
    }
    if (*valuemaskp & CAFocusWindow) {
	(void)XGetWindowAttributes(dpy, value->focuswindow,
				   &(ccw->ccontrol.focus_attr));
    } else {
	XSync(dpy, False);
    }
    XAEUnset(h);

    if ((*valuemaskp & CAFocusWindow) && XERROR(BadWindow)) {
	WidgetWarning((Widget)ccw, "badWindowError", "focusWindow",
		      "focusWindow does not exist.");
	*valuemaskp &= ~CAFocusWindow;
    }
    if ((*valuemaskp & CAColormap) && XERROR(BadColor)) {
	WidgetWarning((Widget)ccw, "badColorError", "colormap",
		      "invalid colormap ID.");
	*valuemaskp &= ~CAColormap;
    }
    if ((*valuemaskp & CAColormap) && XERROR(BadMatch)) {
	WidgetWarning((Widget)ccw, "badMatchError", "colormap",
		      "invalid colormap.");
	*valuemaskp &= ~CAColormap;
    }
    if ((*valuemaskp & CABackgroundPixmap) && XERROR(BadPixmap)) {
	WidgetWarning((Widget)ccw, "badPixmapError", "backgroundPixmap",
		      "invalid pixmap ID.");
	*valuemaskp &= ~CABackgroundPixmap;
    }
    if ((*valuemaskp & CABackgroundPixmap) && XERROR(BadMatch)) {
	WidgetWarning((Widget)ccw, "badMatchError", "backgroundPixmap",
		      "invalid pixmap for background.");
	*valuemaskp &= ~CABackgroundPixmap;
    }
    if ((*valuemaskp & CACursor) && XERROR(BadCursor)) {
	WidgetWarning((Widget)ccw, "badCursorError", "cursor",
		      "invalid cursor ID.");
	*valuemaskp &= ~CACursor;
    }
#undef XERROR
}

static void
CheckCoordinates(ccw, valuemaskp, value, clip)
ConversionControlWidget ccw;
unsigned long *valuemaskp;
ConversionAttributes *value;
int clip;
{
#define INVALIDRECT(r) (r.width == 0 || r.height == 0)
    if ((*valuemaskp & CAClientArea) &&
	(INVALIDRECT(value->clientarea) ||
	 (clip && !clipRectangle(&value->clientarea, &ccw->ccontrol.client_attr)))) {
	DPRINT(("CheckCoordinates: invalid ClientArea\n"));
	*valuemaskp &= ~CAClientArea;
    }
    if ((*valuemaskp & CAStatusArea) &&
	(INVALIDRECT(value->statusarea) ||
	 (clip && !clipRectangle(&value->statusarea, &ccw->ccontrol.client_attr)))) {
	DPRINT(("CheckCoordinates: invalid StatusArea\n"));
	*valuemaskp &= ~CAStatusArea;
    }
#undef INVALIDRECT
}

static Boolean
clipRectangle(rectp, attrp)
register XRectangle *rectp;
register XWindowAttributes *attrp;
{
    register int z0, z1, e;

    z0 = rectp->x; z1 = z0 + rectp->width; e = attrp->width;
    if (z0 == z1) z1 = e;		/* if (rectp->width == 0)... */
    if (z0 >= e || z1 <= 0) return False;
    if (z0 < 0) z0 = 0;
    if (z1 > e) z1 = e;
    rectp->x = z0; rectp->width = z1 - z0;

    z0 = rectp->y; z1 = z0 + rectp->height; e = attrp->height;
    if (z0 == z1) z1 = e;		/* if (rectp->height == 0)... */
    if (z0 >= e || z1 <= 0) return False;
    if (z0 < 0) z0 = 0;
    if (z1 > e) z1 = e;
    rectp->y = z0; rectp->height = z1 - z0;

    return True;
}

static void
GetClientCoordinates(ccw)
ConversionControlWidget ccw;
{
    Display *dpy = XtDisplay(ccw);
    Window root = RootWindowOfScreen(XtScreen(ccw));
    Window win = ccw->ccontrol.clientwindow;
    Window junk;
    int rootx, rooty;

    if (!XTranslateCoordinates(dpy, win, root, 0, 0, &rootx, &rooty, &junk)) {
	WidgetWarning((Widget)ccw, "windowError", "differentRoot",
		      "clientWindow and conversion widget have different root. this should not happen!");
	rootx = rooty = 0;
    }

    ccw->ccontrol.client_rootx = rootx;
    ccw->ccontrol.client_rooty = rooty;
}

/* ARGSUSED */
static void
EventToInputObject(w, event, args, num_args)
Widget w;
XEvent *event;
String *args;
Cardinal *num_args;
{
    ConversionControlWidget ccw = (ConversionControlWidget)w;
    Boolean hascallback = False;
#ifdef OBSOLETE_FEATURE
    Boolean sendback = False;
#endif

    if (ccw->ccontrol.inputobj == NULL) return;

    ccw->ccontrol.endnotify = False;

    if (ccw->ccontrol.unusedeventcallback != NULL &&
	XtHasCallbacks(w, XtNunusedEventCallback) == XtCallbackHasSome) {
	hascallback = True;
    }

#ifdef OBSOLETE_FEATURE
    /*
     * a cheap little hack -- sending back unused events
     *
     * if user set some callback on XtNunusedEventCallback, we call
     * the callback functions on 'unused' KeyPress events.
     * 
     * otherwise, if the resource XtNsendbackKeyPress is true, we
     * attempt to send 'unused' KeyPress events back to the client.
     *
     * we call callbacks  or send an event back to the client when
     * following conditions are satisfied:
     *    + it is a KeyPress event AND
     *    + the number of segments is 0 before the conversion object
     *      processes it AND
     *    + the number of segments remains 0 after processing AND
     *    + none of the modeChangeNotify, selectionControl, endNotify and
     *      fixNofity callbacks are called during processing
     * it is intentional to exclude textChangeNotify callback from
     * above condition, because this callback is often called even if
     * the text doesn't change actually. so we use the condition that
     * the number of segments remains 0 instead.
     */

    if ((hascallback ||ccw->ccontrol.sendbackKeyPress) &&
	event->type == KeyPress &&
	ICNumSegments(ccw->ccontrol.inputobj) == 0) {
	sendback = True;
    }

    ccw->ccontrol.eventused = False;

    if (ICInputEvent(ccw->ccontrol.inputobj, event) == 1 ||
	(sendback && !ccw->ccontrol.eventused &&
	 ICNumSegments(ccw->ccontrol.inputobj) == 0)) {
#else
    /*
     * Above feature is obsolete.  Now it is the input object's
     * responsibility to decide whether the event should be
     * sent back to the client or not.
     */
    if (ICInputEvent(ccw->ccontrol.inputobj, event) == 1) {
#endif

	/* event isn't used */
	if (hascallback) {
	    TRACE(("call XtNunusedEventCallback\n"));
	    XtCallCallbackList(w, ccw->ccontrol.unusedeventcallback,
			       (XtPointer)event);
	} else if (ccw->ccontrol.sendbackKeyPress) {
	    Window savewin;
	    Window savesubwin;
	    
	    TRACE(("sendback event to window 0x%lx\n", ccw->ccontrol.focuswindow));
	    savewin = event->xkey.window;
	    savesubwin = event->xkey.subwindow;
	    event->xkey.window = ccw->ccontrol.focuswindow;
	    event->xkey.subwindow = None;

	    /*
	     * here we use NoEventMask as the eventmask, not
	     * KeyPressMask.  that means the event will be sent only
	     * to the client who created the destination window.
	     */
	    XSendEvent(XtDisplay(w), event->xkey.window,
		       False, NoEventMask, event);

	    event->xkey.window = savewin;	/* restore */
	    event->xkey.subwindow = savesubwin;	/* restore */
	}
    }

    if (ccw->ccontrol.endnotify) {
	CControlEndConversion(w);
	XtCallCallbackList(w, ccw->ccontrol.endcallback, (XtPointer)False);
    }
}


/*
 * sub-widget creation
 */

static Widget
CreateInputObject(ccw)
ConversionControlWidget ccw;
{
    Widget inputobj;
    Arg args[1];

    XtSetArg(args[0], XtNdisplayObjectClass, ccw->ccontrol.displayobjclass);
    inputobj = XtCreateWidget("inputObj", ccw->ccontrol.inputobjclass,
			      (Widget)ccw, args, 1);
    ccw->ccontrol.inputobj = inputobj;

    return inputobj;
}

static Boolean
ClassIsSubClassOf(class, reference)
WidgetClass class;
WidgetClass reference;
{
    while (class != NULL) {
	if (class == reference) return True;
	class = class->core_class.superclass;
    }
    return False;
}

static void
CaptureClientDead(ccw)
ConversionControlWidget ccw;
{
    Display *dpy = XtDisplay(ccw);
    Window win = ccw->ccontrol.clientwindow;

    MyAddEventHandler(dpy, win, DestroyNotify, StructureNotifyMask,
		      ClientDead, (XtPointer)ccw);
}

static void
InterceptClientKeyEvent(ccw)
ConversionControlWidget ccw;
{
    Display *dpy = XtDisplay(ccw);
    Window win = ccw->ccontrol.clientwindow;
    Window probe;

    TRACE(("InterceptClientKeyEvent()\n"));
    probe = XCreateWindow(dpy, win, 0, 0, 9999, 9999, 0, 0,
			  InputOnly, (Visual *)CopyFromParent,
			  0L, (XSetWindowAttributes *)NULL);
    TRACE(("\tprobewindow = %lx\n", probe));

    MyAddEventHandler(dpy, probe, KeyPress, KeyPressMask,
		      ClientKey, (XtPointer)ccw);
    MyAddEventHandler(dpy, probe, KeyRelease, KeyReleaseMask,
		      ClientKey, (XtPointer)ccw);

    ccw->ccontrol.probewindow = probe;

    XMapWindow(dpy, probe);
}

static void
SelectFocusKeyEvent(ccw)
ConversionControlWidget ccw;
{
    Display *dpy = XtDisplay(ccw);
    Window win = ccw->ccontrol.focuswindow;

    MyAddEventHandler(dpy, win, KeyPress, KeyPressMask,
		      ClientKey, (XtPointer)ccw);
    MyAddEventHandler(dpy, win, KeyRelease, KeyReleaseMask,
		      ClientKey, (XtPointer)ccw);
}

static void
UnselectFocusKeyEvent(ccw)
ConversionControlWidget ccw;
{
    Display *dpy = XtDisplay(ccw);
    Window win = ccw->ccontrol.focuswindow;

    MyRemoveEventHandler(dpy, win, KeyPress, ClientKey, (XtPointer)ccw);
    MyRemoveEventHandler(dpy, win, KeyRelease, ClientKey, (XtPointer)ccw);
}

static void
ClientKey(ev, data)
XEvent *ev;
XtPointer data;
{
    Widget w = (Widget)data;
    Cardinal num_params = 0;

    EventToInputObject(w, ev, (String *)NULL, &num_params);
}

static void
ClientDead(ev, data)
XEvent *ev;
XtPointer data;
{
    ConversionControlWidget ccw = (ConversionControlWidget)data;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)ccw->core.widget_class;

    if (ev->type != DestroyNotify ||
	ev->xdestroywindow.window != ccw->ccontrol.clientwindow) return;

    /*
     * Client window is destroyed.
     */

    /* remove all event handlers */
    MyRemoveAllEventHandler(XtDisplay(ccw), ccw->ccontrol.clientwindow);
    if (ccw->ccontrol.probewindow != None) {
	/* no need to destroy probewindow. it's already destroyed. */
	MyRemoveAllEventHandler(XtDisplay(ccw), ccw->ccontrol.probewindow);
    }
    if (ccw->ccontrol.eventselectmethod == ESMethodSelectFocus &&
	ccw->ccontrol.focuswindow != ccw->ccontrol.clientwindow) {
	MyRemoveAllEventHandler(XtDisplay(ccw), ccw->ccontrol.focuswindow);
    }

    ccw->ccontrol.oldclientwindow = ccw->ccontrol.clientwindow;
    ccw->ccontrol.clientwindow = None;
    ccw->ccontrol.focuswindow = None;
    ccw->ccontrol.probewindow = None;
    ccw->ccontrol.active = False;

    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNfixNotify, FixCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNendNotify, ConversionEndCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNtextChangeNotify, TextChangeCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNmodeChangeNotify, ModeChangeCallback, (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNselectionControl, SelectionControlCallback,
		     (XtPointer)ccw);
    XtRemoveCallback(ccw->ccontrol.inputobj,
		     XtNauxControl, AuxControlCallback,
		     (XtPointer)ccw);

    /* call input style dependent finish */
    (*class->conversionControl_class.Finish)((Widget)ccw);

    /* clear conversion object */
    ICClearConversion(ccw->ccontrol.inputobj);

    ccw->ccontrol.oldclientwindow = None;
    XtCallCallbackList((Widget)ccw, ccw->ccontrol.endcallback, (XtPointer)True);
}

/* ARGSUSED */
static void
FixCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    ConversionControlWidget ccw = (ConversionControlWidget)client_data;
    Atom encoding = ccw->ccontrol.textencoding;
    int format;
    int length;
    XtPointer text;
    CCTextCallbackArg arg;

    ccw->ccontrol.eventused = True;

    if (ICGetConvertedString(w, &encoding, &format, &length, &text) < 0) {
	return;
    }
    arg.encoding = encoding;
    arg.format = format;
    arg.length = length;
    arg.text = text;

    XtCallCallbackList((Widget)ccw, ccw->ccontrol.textcallback,
		       (XtPointer)&arg);
    XtFree(text);
}

/* ARGSUSED */
static void
ConversionEndCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    ConversionControlWidget ccw = (ConversionControlWidget)client_data;

    ccw->ccontrol.eventused = True;
    ccw->ccontrol.endnotify = True;
}

/* ARGSUSED */
static void
TextChangeCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget widget = (Widget)client_data;
    ConversionControlWidget ccw = (ConversionControlWidget)widget;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)widget->core.widget_class;
    int nsegs = ICNumSegments(ccw->ccontrol.inputobj);

    /*
     * don't do:
     *	ccw->ccontrol.eventused = True;
     * because this callback is often called even if
     * the text didn't change actually.
     */

    /*
     * when num-segments changed from 0 to 1 (or more),
     * call new-text calllback
     */
    if (ccw->ccontrol.notext && nsegs > 0) {
	XtCallCallbackList((Widget)ccw, ccw->ccontrol.newtextcallback,
			   (XtPointer)w);
    }
    ccw->ccontrol.notext = nsegs == 0;

    (*class->conversionControl_class.TextChange)(widget);
}

/* ARGSUSED */
static void
ModeChangeCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget widget = (Widget)client_data;
    ConversionControlWidget ccw = (ConversionControlWidget)widget;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)widget->core.widget_class;

    ccw->ccontrol.eventused = True;

    (*class->conversionControl_class.ModeChange)(widget);
}

/* ARGSUSED */
static void
SelectionControlCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget widget = (Widget)client_data;
    ConversionControlWidget ccw = (ConversionControlWidget)widget;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)widget->core.widget_class;
    ICSelectionControlArg *arg = (ICSelectionControlArg *)call_data;

    ccw->ccontrol.eventused = True;

    (*class->conversionControl_class.SelectionControl)(widget, arg);
}

/* ARGSUSED */
static void
AuxControlCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget widget = (Widget)client_data;
    ConversionControlWidget ccw = (ConversionControlWidget)widget;
    ConversionControlWidgetClass class = (ConversionControlWidgetClass)widget->core.widget_class;
    ICAuxControlArg *arg = (ICAuxControlArg *)call_data;

    ccw->ccontrol.eventused = True;

    (*class->conversionControl_class.AuxControl)(widget, arg);
}

static void
WidgetError(w, name, type, msg)
Widget w;
String name;
String type;
String msg;
{
    char buf[512];
    String params[1];
    Cardinal num_params;

    params[0] = XtClass(w)->core_class.class_name;
    num_params = 1;

    (void)sprintf(buf, "%%s: %s", msg);

    XtAppErrorMsg(XtWidgetToApplicationContext(w),
		  name, type, "WidgetError", buf, params, &num_params);
}

static void
WidgetWarning(w, name, type, msg)
Widget w;
String name;
String type;
String msg;
{
    char buf[512];
    String params[1];
    Cardinal num_params;

    params[0] = XtClass(w)->core_class.class_name;
    num_params = 1;

    (void)sprintf(buf, "%%s: %s", msg);

    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    name, type, "WidgetError", buf, params, &num_params);
}
