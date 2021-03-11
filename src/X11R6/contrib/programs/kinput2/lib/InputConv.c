#ifndef lint
static char *rcsid = "$Id: InputConv.c,v 1.14 1991/09/26 00:35:59 ishisone Rel $";
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
#include "InputConvP.h"

static XtResource resources[] = {
#define offset(field) XtOffset(InputConvObject, inputConv.field)
    {XtNselectionControl, XtCCallback, XtRCallback, sizeof(XtCallbackList), 
       offset(selectioncallback), XtRCallback, (XtPointer)NULL},
    {XtNtextChangeNotify, XtCCallback, XtRCallback, sizeof(XtCallbackList), 
       offset(textchangecallback), XtRCallback, (XtPointer)NULL},
    {XtNmodeChangeNotify, XtCCallback, XtRCallback, sizeof(XtCallbackList), 
       offset(modechangecallback), XtRCallback, (XtPointer)NULL},
    {XtNendNotify, XtCCallback, XtRCallback, sizeof(XtCallbackList), 
       offset(endcallback), XtRCallback, (XtPointer)NULL},
    {XtNfixNotify, XtCCallback, XtRCallback, sizeof(XtCallbackList), 
       offset(fixcallback), XtRCallback, (XtPointer)NULL},
    {XtNauxControl, XtCCallback, XtRCallback, sizeof(XtCallbackList), 
       offset(auxcallback), XtRCallback, (XtPointer)NULL},
    {XtNdisplayObjectClass, XtCClass, XtRPointer, sizeof(WidgetClass),
	offset(displayObjClass), XtRImmediate, (XtPointer)NULL },
#undef offset
};

static void ClassPartInitialize();
static void Initialize();
static void Destroy();

static int InputEvent();
static ICString *GetMode();
static int CursorPos();
static int NumSegments();
static ICString *GetSegment();
static int CompareSegment();
static ICString *GetItemList();
static int SelectItem();
static int ConvertedString();
static int Clear();
static ICString *GetAuxSegments();

InputConvClassRec inputConvClassRec = {
  { /* object fields */
    /* superclass		*/	(WidgetClass) &objectClassRec,
    /* class_name		*/	"InputConv",
    /* widget_size		*/	sizeof(InputConvRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	ClassPartInitialize,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* obj1			*/	NULL,
    /* obj2			*/	NULL,
    /* obj3			*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* obj4			*/	FALSE,
    /* obj5			*/	FALSE,
    /* obj6			*/	FALSE,
    /* obj7			*/	FALSE,
    /* destroy			*/	Destroy,
    /* obj8			*/	NULL,
    /* obj9			*/	NULL,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* obj10			*/	NULL,
    /* get_values_hook		*/	NULL,
    /* obj11			*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* obj12			*/	NULL,
    /* obj13			*/	NULL,
    /* obj14			*/	NULL,
    /* extension		*/	NULL,
  },
  { /* inputConv fields */
    /* InputEvent		*/	InputEvent,
    /* GetMode			*/	GetMode,
    /* CursorPos		*/	CursorPos,
    /* NumSegments		*/	NumSegments,
    /* GetSegment		*/	GetSegment,
    /* CompareSegment		*/	CompareSegment,
    /* GetItemList		*/	GetItemList,
    /* SelectItem		*/	SelectItem,
    /* GetConvertedString	*/	ConvertedString,
    /* ClearConversion		*/	Clear,
    /* GetSegments		*/	GetAuxSegments,
    /* SupportMultipleObjects	*/	False,
    /* NoMoreObjects		*/	False,
  }
};

WidgetClass inputConvObjectClass = (WidgetClass)&inputConvClassRec;

static void
ClassPartInitialize(cl)
WidgetClass cl;
{
    InputConvObjectClass class = (InputConvObjectClass)cl;
    InputConvObjectClass super = (InputConvObjectClass)class->object_class.superclass;

    if (class->inputConv_class.InputEvent == XtInheritInputEvent)
	class->inputConv_class.InputEvent = super->inputConv_class.InputEvent;
    if (class->inputConv_class.GetMode == XtInheritGetMode)
	class->inputConv_class.GetMode = super->inputConv_class.GetMode;
    if (class->inputConv_class.CursorPos == XtInheritCursorPos)
	class->inputConv_class.CursorPos = super->inputConv_class.CursorPos;
    if (class->inputConv_class.NumSegments == XtInheritNumSegments)
	class->inputConv_class.NumSegments = super->inputConv_class.NumSegments;
    if (class->inputConv_class.GetSegment == XtInheritGetSegment)
	class->inputConv_class.GetSegment = super->inputConv_class.GetSegment;
    if (class->inputConv_class.CompareSegment == XtInheritCompareSegment)
	class->inputConv_class.CompareSegment = super->inputConv_class.CompareSegment;
    if (class->inputConv_class.GetItemList == XtInheritGetItemList)
	class->inputConv_class.GetItemList = super->inputConv_class.GetItemList;
    if (class->inputConv_class.SelectItem == XtInheritSelectItem)
	class->inputConv_class.SelectItem = super->inputConv_class.SelectItem;
    if (class->inputConv_class.GetConvertedString == XtInheritGetConvertedString)
	class->inputConv_class.GetConvertedString = super->inputConv_class.GetConvertedString;
    if (class->inputConv_class.ClearConversion == XtInheritClearConversion)
	class->inputConv_class.ClearConversion = super->inputConv_class.ClearConversion;
    if (class->inputConv_class.GetAuxSegments == XtInheritGetAuxSegments)
	class->inputConv_class.GetAuxSegments = super->inputConv_class.GetAuxSegments;

    class->inputConv_class.NoMoreObjects = False;
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    InputConvObjectClass class = (InputConvObjectClass)new->core.widget_class;
    String params[1];
    Cardinal num_params;

    if (class->inputConv_class.NoMoreObjects) {
	params[0] = XtClass(new)->core_class.class_name;
	num_params = 1;
	XtAppErrorMsg(XtWidgetToApplicationContext(new),
		      "creationError", "widget", "WidgetError",
		      "Class %s cannot have multiple instances",
		      params, &num_params);
    }
    if (!class->inputConv_class.SupportMultipleObjects) {
	class->inputConv_class.NoMoreObjects = True;
    }
}

static void
Destroy(w)
Widget w;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    class->inputConv_class.NoMoreObjects = False;
}

/* ARGSUSED */
static int
InputEvent(w, event)
Widget w;
XEvent *event;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: InputEvent function isn't defined.");
    return -1;	/* for lint */
}

/* ARGSUSED */
static ICString *
GetMode(w)
Widget w;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: GetMode function isn't defined.");
    return NULL;	/* for lint */
}

/* ARGSUSED */
static int
CursorPos(w, segidx, offset)
Widget w;
Cardinal *segidx;
Cardinal *offset;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: CursorPos function isn't defined.");
    return 0;	/* for lint */
}

/* ARGSUSED */
static int
NumSegments(w)
Widget w;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: NumSegments function isn't defined.");
    return 0;	/* for lint */
}

/* ARGSUSED */
static ICString *
GetSegment(w, n)
Widget w;
Cardinal n;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: GetSegment function isn't defined.");
    return NULL;	/* for lint */
}

/* ARGSUSED */
static int
CompareSegment(w, seg1, seg2, n)
Widget w;
ICString *seg1;
ICString *seg2;
Cardinal *n;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: CompareSegment function isn't defined.");
    return 0;	/* for lint */
}

/* ARGSUSED */
static ICString *
GetItemList(w, n)
Widget w;
Cardinal *n;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: GetItem function isn't defined.");
    return NULL;	/* for lint */
}

/* ARGSUSED */
static int
SelectItem(w, n)
Widget w;
int n;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: SelectItem function isn't defined.");
    return -1;	/* for lint */
}

/* ARGSUSED */
static int
ConvertedString(w, encoding, format, length, string)
Widget w;
Atom *encoding;
int *format;
int *length;
XtPointer *string;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: GetConvertedString function isn't defined.");
    return -1;	/* for lint */
}

/* ARGSUSED */
static int
Clear(w)
Widget w;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: ClearConversion function isn't defined.");
    return -1;	/* for lint */
}

/* ARGSUSED */
static ICString *
GetAuxSegments(w, n, ns, nc)
Widget w;
Cardinal *n, *ns, *nc;
{
    XtAppError(XtWidgetToApplicationContext(w),
	       "InputConv Object: GetAuxSegments function isn't defined.");
    return NULL;	/* for lint */
}


/*
 * public functions
 */

Boolean
ICSupportMultipleObjects(cl)
WidgetClass cl;
{
    InputConvObjectClass class = (InputConvObjectClass)cl;

    /* check if specified class is a subclass of InputConvObjectClass */
    while (cl != NULL) {
	if (cl == inputConvObjectClass) {
	    /* OK */
	    XtInitializeWidgetClass(cl);
	    return class->inputConv_class.SupportMultipleObjects;
	}
	cl = cl->core_class.superclass;
    }
    return True;	/* for almost all of other classes, it's True */
}

int
ICInputEvent(w, event)
Widget w;
XEvent *event;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICInputEvent()");
    return (*class->inputConv_class.InputEvent)(w, event);
}

ICString *
ICGetMode(w)
Widget w;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICGetMode()");
    return (*class->inputConv_class.GetMode)(w);
}

int
ICCursorPos(w, segidx, offset)
Widget w;
Cardinal *segidx;
Cardinal *offset;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICCursorPos()");
    return (*class->inputConv_class.CursorPos)(w, segidx, offset);
}

int
ICNumSegments(w)
Widget w;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICNumSegments()");
    return (*class->inputConv_class.NumSegments)(w);
}

ICString *
ICGetSegment(w, n)
Widget w;
Cardinal n;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICGetSegments()");
    return (*class->inputConv_class.GetSegment)(w, n);
}

int
ICCompareSegment(w, seg1, seg2, n)
Widget w;
ICString *seg1;
ICString *seg2;
Cardinal *n;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICCompareSegments()");
    return (*class->inputConv_class.CompareSegment)(w, seg1, seg2, n);
}

ICString *
ICGetItemList(w, n)
Widget w;
Cardinal *n;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICGetItemList()");
    return (*class->inputConv_class.GetItemList)(w, n);
}

int
ICSelectItem(w, n)
Widget w;
int n;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICSelectItem()");
    return (*class->inputConv_class.SelectItem)(w, n);
}

int
ICGetConvertedString(w, encoding, format, length, string)
Widget w;
Atom *encoding;
int *format;
int *length;
XtPointer *string;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICGetConvertedString()");
    return (*class->inputConv_class.GetConvertedString)(w, encoding, format,
							length, string);
}

int
ICClearConversion(w)
Widget w;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICClearConversion()");
    return (*class->inputConv_class.ClearConversion)(w);
}

ICString *
ICGetAuxSegments(w, n, ns, nc)
Widget w;
Cardinal *n, *ns, *nc;
{
    InputConvObjectClass class = (InputConvObjectClass)w->core.widget_class;

    XtCheckSubclass(w, inputConvObjectClass, "ICGetAuxSegments()");
    return (*class->inputConv_class.GetAuxSegments)(w, n, ns, nc);
}
