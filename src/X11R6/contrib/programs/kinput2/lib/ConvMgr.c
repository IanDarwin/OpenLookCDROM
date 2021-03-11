#ifndef lint
static char *rcsid = "$Id: ConvMgr.c,v 1.8 1991/10/14 06:40:01 ishisone Rel $";
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
#include "ConvMgrP.h"
#include "InputConv.h"
#include "ConvCtrl.h"

static void Initialize(), Destroy();

static Screen *getScreen();
static ConverterRec *newConverter();
static InputObjRec *getInputObjRec();
static InputObjRec *newInputObj();
static Boolean isSomeoneBusy();

static CompositeClassExtensionRec CompositeExtension = {
    /* next_extension		*/	NULL,
    /* record_type		*/	NULLQUARK,
    /* version			*/	XtCompositeExtensionVersion,
    /* record_size		*/	sizeof(CompositeClassExtensionRec),
    /* accept_objects		*/	True,
};

ConversionManagerClassRec conversionManagerClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &compositeClassRec,
    /* class_name		*/	"ConversionManager",
    /* widget_size		*/	sizeof(ConversionManagerRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
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
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	NULL,
    /* set_values		*/	NULL,
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
  { /* composite fields */
    /* geometry_manager		*/	NULL,
    /* change_managed		*/	NULL,
    /* insert_child		*/	XtInheritInsertChild,
    /* delete_child		*/	XtInheritDeleteChild,
    /* extension		*/	(XtPointer)&CompositeExtension,
  },
  { /* conversionmanager fields */
    /* empty			*/	0
  }
};

WidgetClass conversionManagerWidgetClass = (WidgetClass)&conversionManagerClassRec;

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
Widget req;
Widget new;
ArgList args;
Cardinal *num_args;
{
    ConversionManagerWidget cmw = (ConversionManagerWidget)new;

    cmw->convmgr.converterlist = NULL;
    cmw->convmgr.inputobjlist = NULL;
}

static void
Destroy(w)
Widget w;
{
    ConversionManagerWidget cmw = (ConversionManagerWidget)w;
    ConverterRec *clist = cmw->convmgr.converterlist;
    ConverterRec *crp;
    InputObjRec *ilist = cmw->convmgr.inputobjlist;
    InputObjRec *iorp;

    while (clist != NULL) {
	crp = clist->next;
	XtFree((char *)clist);
	clist = crp;
    }
    while (ilist != NULL) {
	if (ilist->inputobj != NULL) XtDestroyWidget(ilist->inputobj);
	iorp = ilist->next;
	XtFree((char *)ilist);
	ilist = iorp;
    }
}

static Screen *
getScreen(dpy, win)
Display *dpy;
Window win;
{
    Window root;
    int reqx, reqy;
    unsigned int reqwidth, reqheight, reqborder, junk;
    int i;

    /* get root window */
    XGetGeometry(dpy, win, &root, &reqx, &reqy,
		 &reqwidth, &reqheight, &reqborder, &junk);

    /* get screen of the root window */
    for (i = 0; i < ScreenCount(dpy); i++) {
	if (root == RootWindow(dpy, i)) return ScreenOfDisplay(dpy, i);
    }

    return NULL;
}

static ConverterRec *
newConverter(cmw, screen, converterclass, inputobjclass, displayobjclass)
ConversionManagerWidget cmw;
Screen *screen;
WidgetClass converterclass;
WidgetClass inputobjclass;
WidgetClass displayobjclass;
{
    Arg args[5];
    int i;
    ConverterRec *crp;
    InputObjRec *iorp;

    crp = XtNew(ConverterRec);
    crp->busy = False;
    crp->converterclass = converterclass;
    crp->inputobjclass = inputobjclass;
    crp->displayobjclass = displayobjclass;
    crp->screen = screen;

    if ((iorp = getInputObjRec(cmw, inputobjclass)) == NULL) {
	iorp = newInputObj(cmw, inputobjclass, displayobjclass);
    }

    i = 0;
    if (screen != XtScreen((Widget)cmw)) {
	XtSetArg(args[i], XtNscreen, screen); i++;
	XtSetArg(args[i], XtNdepth, DefaultDepthOfScreen(screen)); i++;
	XtSetArg(args[i], XtNcolormap, DefaultColormapOfScreen(screen)); i++;
    }
    if (iorp->inputobj != NULL) {
	XtSetArg(args[i], XtNinputObject, iorp->inputobj); i++;
    } else {
	XtSetArg(args[i], XtNinputObjectClass, inputobjclass); i++;
    }
    XtSetArg(args[i], XtNdisplayObjectClass, displayobjclass); i++;

    crp->converter = XtCreatePopupShell("converter", converterclass,
					(Widget)cmw, args, i);

    /* insert it to the list */
    crp->next = cmw->convmgr.converterlist;
    cmw->convmgr.converterlist = crp;

    return crp;
}

static InputObjRec *
getInputObjRec(cmw, objclass)
ConversionManagerWidget cmw;
WidgetClass objclass;
{
    InputObjRec *iorp = cmw->convmgr.inputobjlist;

    while (iorp != NULL) {
	if (iorp->inputobjclass == objclass) return iorp;
	iorp = iorp->next;
    }
    return NULL;
}

static InputObjRec *
newInputObj(cmw, objclass, dispobjclass)
ConversionManagerWidget cmw;
WidgetClass objclass;
WidgetClass dispobjclass;
{
    InputObjRec *iorp;

    iorp = XtNew(InputObjRec);
    iorp->inputobjclass = objclass;
    if (!ICSupportMultipleObjects(objclass)) {
	Arg args[1];

	XtSetArg(args[0], XtNdisplayObjectClass, dispobjclass);
	iorp->inputobj = XtCreateWidget("convObject", objclass,
					(Widget)cmw, args, 1);
    } else {
	iorp->inputobj = NULL;
    }

    iorp->next = cmw->convmgr.inputobjlist;
    cmw->convmgr.inputobjlist = iorp;
    return iorp;
}

static Boolean
isSomeoneBusy(clist, objclass)
ConverterRec *clist;
WidgetClass objclass;
{
    while (clist != NULL) {
	if (clist->inputobjclass == objclass && clist->busy) return True;
	clist = clist->next;
    }
    return False;
}


/*
 * public functions
 */

void
CMPrepareConverter(w, screen, converterclass, inputobjclass, displayobjclass)
Widget w;
Screen *screen;
WidgetClass converterclass;
WidgetClass inputobjclass;
WidgetClass displayobjclass;
{
    XtCheckSubclass(w, conversionManagerWidgetClass, "CMPrepareConverter()");
    (void)newConverter((ConversionManagerWidget)w, screen,
		       converterclass, inputobjclass, displayobjclass);
}

Widget
CMGetConverter(w, client, converterclass, inputobjclass, displayobjclass)
Widget w;
Window client;
WidgetClass converterclass;
WidgetClass inputobjclass;
WidgetClass displayobjclass;
{
    ConversionManagerWidget cmw = (ConversionManagerWidget)w;
    ConverterRec *clist = cmw->convmgr.converterlist;
    ConverterRec *crp;
    InputObjRec *iorp;
    Screen *scr;

    XtCheckSubclass(w, conversionManagerWidgetClass, "CMGetConverter()");

    if ((iorp = getInputObjRec(cmw, inputobjclass)) == NULL) {
	iorp = newInputObj(cmw, inputobjclass, displayobjclass);
    } else if (iorp->inputobj != NULL && isSomeoneBusy(clist, inputobjclass)) {
	return NULL;
    }
    
    if ((scr = getScreen(XtDisplay(w), client)) == NULL) {
	String params[1];
	Cardinal num_params;

	params[0] = XtClass(w)->core_class.class_name;
	num_params = 1;
	XtAppWarningMsg(XtWidgetToApplicationContext(w),
			"parameterError", "cannotGetScreen", "WidgetError",
			"%s: CMGetConverter() cannot get screen of specified client window",
			params, &num_params);
	return NULL;
    }

    while (clist != NULL) {
	if (!clist->busy &&
	    clist->screen == scr &&
	    clist->converterclass == converterclass &&
	    clist->inputobjclass == inputobjclass &&
	    clist->displayobjclass == displayobjclass) {
	    /* found */
	    clist->busy = True;
	    return clist->converter;
	}
	clist = clist->next;
    }

    crp = newConverter(cmw, scr, converterclass,
		       inputobjclass, displayobjclass);
    crp->busy = True;
    return crp->converter;
}

void
CMReleaseConverter(w, converter)
Widget w;
Widget converter;
{
    ConversionManagerWidget cmw = (ConversionManagerWidget)w;
    ConverterRec *clist = cmw->convmgr.converterlist;
    String params[1];
    Cardinal num_params;

    XtCheckSubclass(w, conversionManagerWidgetClass, "CMReleaseConverter()");

    while (clist != NULL) {
	if (clist->converter == converter) {
	    if (!clist->busy) {
		params[0] = XtClass(w)->core_class.class_name;
		num_params = 1;
		XtAppWarningMsg(XtWidgetToApplicationContext(w),
				"parameterError", "converterNotBusy",
				"WidgetError",
				"%s: CMReleaseConverter() converter isn't used",
				params, &num_params);
	    }
	    clist->busy = False;
	    return;
	}
	clist = clist->next;
    }

    params[0] = XtClass(w)->core_class.class_name;
    num_params = 1;
    XtAppWarningMsg(XtWidgetToApplicationContext(w),
		    "parameterError", "noSuchConverter", "WidgetError",
		    "%s: CMReleaseConverter() no such converter",
		    params, &num_params);
}
