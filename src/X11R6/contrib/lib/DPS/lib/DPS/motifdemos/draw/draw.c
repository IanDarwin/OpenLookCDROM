/* draw.c - The draw application
 *
 * (c) Copyright 1990-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <DPS/XDPS.h>
#include <DPS/XDPSlib.h>
#include <DPS/dpsXclient.h>
#include <DPS/DPSScrollW.h>

#include "draw.h"
#include "objects.h"
#include "drawwraps.h"
#include "textmgr.h"
#include "colormgr.h"
#include <X11/Xlib.h>

ObjTypeDesc objDescriptors[CountTypes];

static ObjTypeDesc *curObjDesc = NULL;
static float gridSize = 0;
static Boolean gridOn = False;
static Widget scrollWidget, drawingArea, textField;
static GenericObject *objects = NULL;
static GenericObject *lastObj = NULL;

#ifdef _NO_PROTO
#define ARGCAST int
#else
#define ARGCAST void *
#endif

static void RefreshObjects()
{
    GenericObject *obj;
    
    for (obj = objects; obj != NULL; obj = obj->next) {
	PSsetrgbcolor(obj->red, obj->green, obj->blue);
	(*objDescriptors[obj->objectType].drawObject)(obj);
    }
}

static void RefreshGrid()
{
    PSWDrawGrid(gridSize, DRAW_WIDTH, DRAW_HEIGHT, 0.0);
}

char *GetTextValue()
{
    return XmTextGetString(textField);
}

/* ARGSUSED */

static void GridChange(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    float r[4];

    gridOn = toggle->set;

    r[0] = r[1] = 0;
    r[2] = r[3] = -1;
    DSWAddToDirtyArea(scrollWidget, r, 1);
}

/* ARGSUSED */

static void ChangeGridSize(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmScaleCallbackStruct *scale = (XmScaleCallbackStruct *) callData;
    float r[4];

    gridSize = scale->value;

    if (gridOn) {
	r[0] = r[1] = 0;
	r[2] = r[3] = -1;
	DSWAddToDirtyArea(scrollWidget, r, 1);
    }
}

static void CreateGridControls(parent)
    Widget parent;
{
    Widget controls, w;
    int value;

    controls = XtVaCreateManagedWidget("gridControls", xmRowColumnWidgetClass,
				       parent, NULL);

    w = XtVaCreateManagedWidget("gridScale", xmScaleWidgetClass,
				controls, NULL);
    XtAddCallback(w, XmNvalueChangedCallback, ChangeGridSize, NULL);
    XtAddCallback(w, XmNdragCallback, ChangeGridSize, NULL);
  
    XmScaleGetValue(w, &value);

    w = XtVaCreateManagedWidget("grid", xmToggleButtonWidgetClass,
				controls, NULL);
    XtAddCallback(w, XmNvalueChangedCallback, GridChange, NULL);

    gridOn = XmToggleButtonGetState(w);
    gridSize = value;
}

static void AddObject(obj)
    GenericObject *obj;
{
    if (objects == NULL) {
	objects = lastObj = obj;
	obj->next = obj->prev = NULL;
    } else {
	lastObj->next = obj;
	obj->prev = lastObj;
	obj->next = NULL;
	lastObj = obj;
    }
}

/* ARGSUSED */

static void RefreshCanvas(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    DSWExposeCallbackRec *cb = (DSWExposeCallbackRec *) callData;

    if (cb->directions == DSWAbort) {
	cb->results = DSWAborted;
	return;
    }

    PSWClearPage();
    if (gridOn) RefreshGrid();
    RefreshObjects();

    cb->results = DSWFinished;
}

static float SnapToGrid(n, size)
    float n, size;
{
    return (int)((n + size/2) / size) * size;
}

/* ARGSUSED */

static void MouseMoved(w, clientData, event, goon)
    Widget w;
    XtPointer clientData;
    XEvent *event;
    Boolean *goon;
{
    XMotionEvent *m = (XMotionEvent *) event;
    Display *dpy = XtDisplay(w);
    Boolean buttonUp;
    GenericObject *obj = (GenericObject *) clientData;
    Dimension width, height;
    int scrollx = 0, scrolly = 0;

    if (XPending(dpy) != 0) {
        XEvent e;
        XPeekEvent(dpy, &e);
        if (e.xany.window == event->xany.window &&
            e.type == event->type) return;
    }

    if (curObjDesc == NULL) return;

    buttonUp = (event->type == ButtonRelease);

    if (!buttonUp) {
	XtVaGetValues(drawingArea, XtNwidth, &width, XtNheight, &height, NULL);
	if (m->x < 0) scrollx = m->x;
	else if (m->x > (int) width) scrollx = m->x - width;
	if (m->y < 0) scrolly = m->y;
	else if (m->y > (int) height) scrolly = m->y - height;
	if (scrollx != 0 || scrolly != 0) {
	    DSWScrollBy(scrollWidget, scrollx, scrolly);
	}
    }

    DSWConvertXToPS(scrollWidget, m->x, m->y, &obj->x2, &obj->y2);

    if (gridOn) {
	obj->x2 = SnapToGrid(obj->x2, gridSize);
	obj->y2 = SnapToGrid(obj->y2, gridSize);
    }

    (*(curObjDesc->computeBBox))(obj, obj->bbox);

    DSWSetFeedbackDirtyArea(scrollWidget, obj->bbox, 1, (XtPointer) buttonUp);
}

/* ARGSUSED */

static void ButtonDown(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmDrawingAreaCallbackStruct *dac =
            (XmDrawingAreaCallbackStruct *) callData;
    XButtonEvent *be = (XButtonEvent *) dac->event;
    GenericObject *obj;

    if (be->type != ButtonPress) return;

    if (curObjDesc == NULL) return;
    obj = (*curObjDesc->createObject)(curObjDesc->type);

    GetColors(&obj->red, &obj->green, &obj->blue);
    DSWConvertXToPS(scrollWidget, be->x, be->y, &obj->x1, &obj->y1);

    if (gridOn) {
	obj->x1 = SnapToGrid(obj->x1, gridSize);
	obj->y1 = SnapToGrid(obj->y1, gridSize);
    }

    obj->x2 = obj->x1;
    obj->y2 = obj->y1;

    (*(curObjDesc->computeBBox))(obj, obj->bbox);

    XtAddEventHandler(drawingArea, PointerMotionMask | ButtonReleaseMask,
		      False, MouseMoved, (XtPointer) obj);
    DSWStartFeedbackDrawing(scrollWidget, (XtPointer) obj);

    DSWSetFeedbackDirtyArea(scrollWidget, obj->bbox, 1, (XtPointer) False);
}

/* ARGSUSED */

static void Feedback(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    DSWFeedbackCallbackRec *f = (DSWFeedbackCallbackRec *) callData;
    Boolean buttonUp = (Boolean) f->continue_feedback_data;
    GenericObject *obj = (GenericObject *) f->start_feedback_data;

    PSsetrgbcolor(obj->red, obj->green, obj->blue);
    (*(curObjDesc->drawObject))(obj);

    if (buttonUp) {
	AddObject(obj);
	DSWEndFeedbackDrawing(scrollWidget, False);
	XtRemoveEventHandler(drawingArea,
			     PointerMotionMask | ButtonReleaseMask, False,
			     MouseMoved, obj);

	DSWAddToDirtyArea(scrollWidget, obj->bbox, 1);
    }
}

/* ARGSUSED */

static void Erase(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    GenericObject *obj;
    float r[4];

    obj = objects;
    while (obj != NULL) {
	GenericObject *next = obj->next;
	if (obj->objectType == TextType) {
	    /*
	      Do something with all of those font name
	      and text strings. How are they to be freed?
	      */
	}
	free((char *)obj);
	obj = next;
    }
    objects = lastObj = NULL;

    r[0] = r[1] = 0;
    r[2] = r[3] = -1;
    DSWAddToDirtyArea(scrollWidget, r, 1);
}

/* ARGSUSED */

static void CanvasSetup(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    DSWSetupCallbackRec *setup = (DSWSetupCallbackRec *) callData;

    DPSSetContext(setup->context);
    PSWLoadProcs();
}

static void CreateCanvas(parent, context)
    Widget parent;
    DPSContext context;
{
    Widget w;
  
    w = XtVaCreateManagedWidget("canvas", dpsScrolledWindowWidgetClass,
				parent, XtNcontext, context, NULL);
    XtAddCallback(w, XtNexposeCallback, RefreshCanvas, NULL);
    XtAddCallback(w, XtNsetupCallback, CanvasSetup, NULL);
    XtAddCallback(w, XtNfeedbackCallback, Feedback, NULL);
  
    scrollWidget = w;
    drawingArea = XtNameToWidget(w, "*drawingArea");
    XtAddCallback(drawingArea, XmNinputCallback, ButtonDown, NULL);
}

/* ARGSUSED */

void ObjectSelected(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *tb =
	    (XmToggleButtonCallbackStruct *) callData;
    ObjTypeDesc *objDesc;

    XtVaGetValues(w, XmNuserData, &objDesc, NULL);

    if (tb->set) curObjDesc = objDesc;
}

static void CreateDrawingCommands(parent)
    Widget parent;
{
    int i;
    Arg wargs[1];
    ObjTypeDesc *cur;
    Widget w, commands;

    InitStdObjects();
    InitTextObject();

    XtSetArg(wargs[0], XmNentryClass, xmToggleButtonGadgetClass);
    commands = XmCreateRadioBox(parent, "commands", wargs, 1);
    XtManageChild(commands);

    for (i = CountTypes - 1, cur = &objDescriptors[i]; i >= 0; i--, cur--) {
	XtSetArg(wargs[0], XmNuserData, cur);
	w = XmCreateToggleButtonGadget(commands, cur->label, wargs, 1);
	XtManageChild(w);
	XtAddCallback(w, XmNvalueChangedCallback, ObjectSelected, NULL);
    }
}

/* ARGSUSED */

static void Quit(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XCloseDisplay(XtDisplay(w));
    exit(0);
}

/* ARGSUSED */

static void PopupColorEditor(w, clientData, callData)
    Widget w;
    XtPointer clientData,  callData;
{
    Widget popup = (Widget) clientData;

    XtPopup(XtParent(popup), XtGrabNone);
    XRaiseWindow(XtDisplay(popup), XtWindow(XtParent(popup)));
}

/* ARGSUSED */

static void PopupFontPanel(w, clientData, callData)
    Widget w;
    XtPointer clientData,  callData;
{
    Widget popup = (Widget) clientData;

    XtPopup(XtParent(popup), XtGrabNone);
    XRaiseWindow(XtDisplay(popup), XtWindow(XtParent(popup)));
}

static String fallback_resources[] = {
  "*XmScale.Orientation:			HORIZONTAL",
  "*XmScale.ProcessingDirection:		MAX_ON_RIGHT",
  "*framework.width:				600",
  "*framework.height:				512",
  NULL
};

main(argc, argv)
    int argc;
    char *argv[];
{
    Arg wargs[5];
    Display *dpy;
    XtAppContext app;
    Widget topLevel, framework, command, w, popup;
    DPSContext context;

    curObjDesc = NULL;
    objects = lastObj = NULL;

    topLevel = XtAppInitialize(&app, "Draw", NULL, 0, &argc, argv,
			       fallback_resources, (ArgList)NULL, 0);

    /* Make it possible for this client to start a DPS NX agent,
       if "dpsnx.agent" is on the executable search path. */

    (void) XDPSNXSetClientArg(XDPSNX_AUTO_LAUNCH, (ARGCAST) True);

    dpy = XtDisplay(topLevel);
    context = XDPSCreateSimpleContext(dpy, 0, 0, 0, 0,
				      DPSDefaultTextBackstop,
				      DPSDefaultErrorProc, NULL);
    if (context == NULL) {
	fprintf (stderr, "\ndraw: DPS is not available\n");
	fprintf (stderr,
          "You need an X server with the DPS extension, or a DPS NX agent.\n");
	exit (1);
    }

#ifdef DEBUG
    {
	DPSContext textCtxt = 
		DPSCreateTextContext(DPSDefaultTextBackstop,
				     DPSDefaultErrorProc);
	DPSChainContext(context, textCtxt);
    }
#endif

    framework = XtCreateManagedWidget("framework", xmFormWidgetClass,
				      topLevel, NULL, 0);

    /* Create column to hold commands... */
    XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
    XtSetArg(wargs[1], XmNbottomAttachment, XmATTACH_FORM);
    XtSetArg(wargs[2], XmNleftAttachment, XmATTACH_FORM);
    command = XtCreateManagedWidget("command", xmRowColumnWidgetClass,
				    framework, wargs, 3);

    w = XtCreateManagedWidget("quit", xmPushButtonWidgetClass, command,
			      NULL, 0);
    XtAddCallback(w, XmNactivateCallback, Quit, (XtPointer) NULL);

    w = XtCreateManagedWidget("erase", xmPushButtonWidgetClass, command,
			      NULL, 0);
    XtAddCallback(w, XmNactivateCallback, Erase, NULL);

    w = XtCreateManagedWidget("color", xmPushButtonWidgetClass, command,
			      NULL, 0);
    popup = CreateColorEditor(w, (float)InitRed,
			      (float)InitGreen, (float)InitBlue);
    XtAddCallback(w, XmNactivateCallback, PopupColorEditor, (XtPointer) popup);

    w = XtCreateManagedWidget("fonts", xmPushButtonWidgetClass, command,
			      NULL, 0);
    popup = CreateFontPanel(w);
    XtAddCallback(w, XmNactivateCallback, PopupFontPanel, (XtPointer) popup);

    w = XtCreateManagedWidget("textString", xmLabelWidgetClass,
			      command, NULL, 0);
    textField = XtCreateManagedWidget("textField", xmTextWidgetClass,
					   command, NULL, 0);
    CreateDrawingCommands(command);
    CreateGridControls(command);
    CreateCanvas(framework, context);
    XtSetArg(wargs[0], XmNtopAttachment, XmATTACH_FORM);
    XtSetArg(wargs[1], XmNbottomAttachment, XmATTACH_FORM);
    XtSetArg(wargs[2], XmNrightAttachment, XmATTACH_FORM);
    XtSetArg(wargs[3], XmNleftAttachment, XmATTACH_WIDGET);
    XtSetArg(wargs[4], XmNleftWidget, command);
    XtSetValues(scrollWidget, wargs, 5);

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app);
}
