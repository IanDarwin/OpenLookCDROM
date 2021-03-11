/*
 * $RCSfile: HitMain.c,v $
 *
 * (c) Copyright 1992-1994 Adobe Systems Incorporated.
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


/***************************************************************
**
**	INCLUDES
**
***************************************************************/

#include "Hit.h"
#include "zoom.xbm"
#include "zoom_mask.xbm"

/***************************************************************
**
**	DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application data block
*/
AppDataType     AppData;

/*
** Global resource management data
*/
MrmHierarchy         SMrmHierarchy;	/* MRM database hierarchy ID */
static MrmType       DummyClass;	/* and class variable. */
static char         *DbFilenameVec [] =     /* Mrm.heirarchy file list. */
{
    "Hit.uid"
};

static void resizeWindow(), refreshWindow(), createProc(), quitApp(),
	traceProc(), showBufferProc(), bufferExposeProc(), refreshMouse(),
	gridSel(), copyAllSel(), mouseSel(), magnificationSel(),
	initApplication(), mouseDown();

/* Names and addresses for Mrm to bind */
static MrmRegisterArg RegList [] =
{
    {"resizeWindow"    , (XtPointer) resizeWindow    },
    {"refreshWindow"   , (XtPointer) refreshWindow   },
    {"graphicExpose"   , (XtPointer) graphicExpose   },
    {"createProc"      , (XtPointer) createProc      },
    {"quitApp"         , (XtPointer) quitApp         },
    {"scrollProc"      , (XtPointer) scrollProc      },
    {"traceProc"       , (XtPointer) traceProc       },
    {"showBufferProc"  , (XtPointer) showBufferProc  },
    {"bufferExposeProc", (XtPointer) bufferExposeProc},
    {"refreshMouse"    , (XtPointer) refreshMouse    },
    {"gridSel"         , (XtPointer) gridSel         },
    {"copyAllSel"      , (XtPointer) copyAllSel	     },
    {"mouseSel"        , (XtPointer) mouseSel        },
    {"magnificationSel", (XtPointer) magnificationSel}
};

static XrmOptionDescRec CommandLineOptions[] = {
  { "-draw", ".drawTrace", XrmoptionNoArg, (caddr_t)"True",},
  { "-hit", ".hitTrace", XrmoptionNoArg, (caddr_t) "True",},
  { "-zoom", ".zoomTrace", XrmoptionNoArg, (caddr_t) "True",},
};

static XtResource Resources[] = {
  {
    "drawTrace",
    "DrawTrace",
    XtRBoolean,
    sizeof (Boolean),
    XtOffset (AppDataTypePtr, drawTrace),
    XtRImmediate,
    (caddr_t) False
    },
  {
    "hitTrace",
    "hitTrace",
    XtRBoolean,
    sizeof (Boolean),
    XtOffset (AppDataTypePtr, hitTrace),
    XtRImmediate,
    (caddr_t) False
    },
  {
    "zoomTrace",
    "zoomTrace",
    XtRBoolean,
    sizeof (Boolean),
    XtOffset (AppDataTypePtr, zoomTrace),
    XtRImmediate,
    (caddr_t) False
    },
};

static 	Cursor zoomCursor;     /* cursor for zooming window */

/***************************************************************
**
** FUNCTION:	main
**
** DESCRIPTION:	Main procedure for the Hit Detection Application.
**		This procedure sets up the X Window environment
**		and then enters event dispatching loop.
**
** PARAMETERS:	argc	argument count of command line call
**		argv	array of command line arguments
**
** RETURN:		N/A
**
***************************************************************/

main(argc, argv)
    int	 argc;
    char *argv[];
{
    XtAppContext appContext;
    Widget toplevel;
    Widget mainWindow;
    Widget optionBox;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize();

    /*
    ** Initialize the X Toolkit. We get back a top level shell widget.
    */
    toplevel = XtAppInitialize (
	&appContext, "Hit", CommandLineOptions,
	XtNumber(CommandLineOptions), &argc, argv,
	(String *) NULL, (ArgList) NULL, 0);
    
    XtGetApplicationResources (
	toplevel, (XtPointer) &AppData, (XtResourceList) Resources,
	XtNumber(Resources), (ArgList) NULL, 0
	);

    if (!XDPSExtensionPresent(XtDisplay(toplevel))) {
        fprintf (stderr, "%s:  DPS extension not in server\n", argv [0]);
        exit (1);
    }

    /*
    ** Open the UID files (the output of the UIL compiler)
    */
    if (MrmOpenHierarchy (XtNumber(DbFilenameVec), DbFilenameVec, NULL,
        &SMrmHierarchy)  !=  MrmSUCCESS) {
        fprintf (stderr, "Can't open heirarchy\n");
        exit (1);
    }

    /*
    ** Register the items MRM needs to bind for us.
    */
    MrmRegisterNames (RegList, XtNumber(RegList));

    /*
    ** Get the main window for the application.
    */
    if (MrmFetchWidget (SMrmHierarchy, "MainWindow", toplevel,
        &mainWindow, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf (stderr, "Can't fetch main window\n");
        exit (0);
    }

    /*
    ** Get the option box widget
    */
    if (MrmFetchWidget(SMrmHierarchy, "OptionBox", mainWindow,
	&optionBox, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf(stderr, "Can't fetch option window\n");
        exit(0);
    }

    /*
    ** Get the show buffer box widget
    */
    if (MrmFetchWidget(SMrmHierarchy, "BufferBox", mainWindow,
	&AppData.bufferBox, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf(stderr, "Can't fetch show buffer window\n");
        exit(0);
    }

    /*
    ** Manage the main window and option box and realize everything.
    ** The interface comes up on the display now.
    */
    XtManageChild(mainWindow);
    XtManageChild(optionBox);
    XtVaSetValues(
	XtParent(optionBox),
    	XmNmwmFunctions,
        MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE,
        XmNmwmDecorations,
        MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_RESIZEH,
        NULL
    );
    XtRealizeWidget(toplevel);

    /* Perform onetime initialization of application data structures */
    initApplication();

    /*
    ** Do all the post-realization DPS/X processing here
    */
    initDPSContext();

    /*
    ** Sit around forever waiting to process X-events.
    ** From here on, we only execute our callback routines.
    */
    while (1) {
	XEvent event;
	XtAppNextEvent(appContext, &event);
	if (!XDPSDispatchEvent(&event)) (void) XtDispatchEvent(&event);
    }
}

/***************************************************************
**
** FUNCTION:    initApplication
**
** DESCRIPTION: One-time initialization of application data
**              structures.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/

static void initApplication()
{
    XGCValues values;
    Pixmap cursor, mask;
    Display *dpy = XtDisplay(AppData.drawingArea);
    Window win =  XtWindow(AppData.drawingArea);
    XColor fore, back;

    /*
    ** Initialize the booleans
    */
    AppData.showBuffer = False;
    AppData.gridOn = False;
    AppData.copyAll = False;
    AppData.selected = False;
    AppData.zooming = False;
    AppData.scrolling = False;

    /*
    ** Initialize the initial radio button selection
    */
    AppData.magnify   = 100;    
    AppData.hitSize = 4;
    AppData.scale = 1.0;		

    /* 
    ** Create a GC for copying
    */
    AppData.gc = XCreateGC(dpy, win, 0, &values);

    /*
    ** Create a zoom pixmap
    */
    cursor = XCreateBitmapFromData(dpy, win, (char *) zoom_bits,
				   zoom_width, zoom_height);
    mask = XCreateBitmapFromData(dpy, win, (char *) zoom_mask_bits,
				 zoom_mask_width, zoom_mask_height);
    if (cursor != None && mask != None) {
	fore.red = fore.green = fore.blue = 0;
	back.red = back.green = back.blue = 65535;
	fore.flags = back.flags = DoRed | DoGreen | DoBlue;

	zoomCursor = XCreatePixmapCursor(dpy, cursor, mask, &fore, &back,
					 zoom_x_hot, zoom_y_hot);
	XFreePixmap(dpy, cursor);
	XFreePixmap(dpy, mask);
    } else zoomCursor = None;
} /* end initApplication() */

/***************************************************************
**
** FUNCTION:    resizeWindow
**
** DESCRIPTION: Callback routine to handle resize events.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void resizeWindow(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int depth;
    Display *dpy = XtDisplay(w);
    XPoint xpoint;
    Point point;

    if (!XtIsRealized(w)) return;

    /*
    ** Convert lower left corner into PS units so we can keep it fixed
    */
    xpoint.x = 0;
    xpoint.y = (int) AppData.drawingHeight;
    convertToDPS(&xpoint, &point);

    /*
    ** Get new size of drawing area
    */
    XtVaGetValues(AppData.drawingArea, XtNheight, &AppData.drawingHeight,
		  XtNwidth, &AppData.drawingWidth,
		  XtNdepth, &depth, NULL);

    /*
    ** Resizing automatically moved the y offset
    */
    AppData.yOffset = AppData.drawingHeight;

    /*
    ** Change the sizes of the buffer windows
    */
    XtVaSetValues(AppData.bufOrig, XtNheight, AppData.drawingHeight,
		  XtNwidth, AppData.drawingWidth, NULL);
    XtVaSetValues(AppData.bufComp,  XtNheight, AppData.drawingHeight,
		  XtNwidth, AppData.drawingWidth, NULL);
	
    /*
    ** Create new pixmaps to match the drawable
    */
    XFreePixmap(dpy, AppData.original);
    XFreePixmap(dpy, AppData.composite);

    AppData.original = XCreatePixmap(dpy, XtWindow(w), AppData.drawingWidth,
				     AppData.drawingHeight, depth);
    AppData.composite = XCreatePixmap(dpy, XtWindow(w), AppData.drawingWidth,
				      AppData.drawingHeight, depth);
	
    /*
    ** Clear pixmaps
    */
    XFillRectangle(dpy, AppData.original, AppData.gc, 0, 0, 
		   AppData.drawingWidth, AppData.drawingHeight);
    XFillRectangle(dpy, AppData.composite, AppData.gc, 0, 0,
		   AppData.drawingWidth, AppData.drawingHeight);

    /*
    ** Update the gstates for the two buffers to reflect the new pixmaps
    */
    XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
    XDPSSetContextDrawable(AppData.dpsCtxt, AppData.composite,
			   AppData.drawingHeight);
    XDPSUpdateContextGState(AppData.dpsCtxt, AppData.compGState);
    
    XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    XDPSSetContextDrawable(AppData.dpsCtxt, AppData.original,
			   AppData.drawingHeight);
    XDPSUpdateContextGState(AppData.dpsCtxt, AppData.origGState);

    /*
    ** Move the drawing area so the bottom left corner
    ** of the image remains located at the bottom left corner
    */
    positionDrawingArea(point.x, point.y, 0, AppData.drawingHeight);

    drawSelfAndUpdate();
} /* end resizeWindow() */

/***************************************************************
**
** FUNCTION:    refreshWindow
**
** DESCRIPTION: Callback routine to handle regular expose events
**		to the main window.  Updates the window by copying
**		from the original pixmap
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void refreshWindow(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmDrawingAreaCallbackStruct *callback =
	    (XmDrawingAreaCallbackStruct *) callData;
    XExposeEvent *e = &callback->event->xexpose;

    /*
    ** Only update exposed areas
    */
    XCopyArea(XtDisplay(w), AppData.original, XtWindow(w), AppData.gc,
	      e->x, e->y, e->width, e->height, e->x, e->y);
} /* end refreshWindow() */

/***************************************************************
**
** FUNCTION:    refreshMouse
**
** DESCRIPTION: Callback routine to handle expose events on the 
**		mouse hit drawing area.  Refreshes the window by
**		redrawing the circle
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void refreshMouse(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    register Display *dpy = XtDisplay(w);
    register Window window = XtWindow(w);
    XEvent event;

    /*
    ** Pseudo Exposure event compression for the Drawing Area Widget
    */
    if (XPending(dpy) > 0) {
	XPeekEvent(dpy, &event);
	if (event.type == Expose && event.xany.window == window) return;
    }

    /*
    ** Redraw mouse sensitivity area
    */
    drawSensitivityCircle();
} /* end refreshMouse() */

/***************************************************************
**
** FUNCTION:    createProc
**
** DESCRIPTION: Callback routine for widget creation.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void createProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int widgetNum = *(int *) clientData;

    /*
    ** Save widget ID
    */
    switch (widgetNum) {
        case cMainDrawArea:
	    AppData.drawingArea= w;
            /*
	    ** Add event handlers for main window
	    */
            XtAddRawEventHandler(w, 0, True, graphicExpose, NULL);
	    XtAddEventHandler(w, ButtonPressMask, False, mouseDown, NULL);
            break;

        case cBufferDrawArea0:	AppData.bufOrig = w;            break;
	case cBufferDrawArea1:	AppData.bufComp = w;            break;
	case cMainHorzSBar:	AppData.hScroll = w;		break;
	case cMainVertSBar:	AppData.vScroll = w;		break;
	case cMouseDrawArea:	AppData.mouseArea = w;		break;
	
        case cDrawingToggle:
	    /* If trace turned on by command line, set button */
            if (AppData.drawTrace) XtVaSetValues(w, XmNset, True, NULL);
	    break;

        case cHitDetToggle:
            if (AppData.hitTrace) XtVaSetValues(w, XmNset, True, NULL);
            break;

        case cZoomingToggle:
            if (AppData.zoomTrace) XtVaSetValues(w, XmNset, True, NULL);
            break;
    } /* end switch */
} /* end createProc() */
 
/***************************************************************
**
** FUNCTION:    quitApp
**
** DESCRIPTION: Callback routine for "quit" command menu
**              selection.  Exits from the application.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      Returns to OS.
**
***************************************************************/

/* ARGSUSED */

static void quitApp(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
    exit(0);
} /* end quitApp() */

/***************************************************************
**
** FUNCTION:    traceProc
**
** DESCRIPTION: Callback routine for the trace toggle buttons. 
**		Sets or resets the appropriate tracing flags.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void traceProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    switch (num) {
        case 0:				/* Trace - Drawing */
            AppData.drawTrace = toggle->set;
            break;

        case 1:     			/* Trace - Hit Detection */
            AppData.hitTrace = toggle->set;
            break;

	case 2:     			/* Trace - Zooming */
            AppData.zoomTrace = toggle->set;
            break;
    }
} /* end traceProc() */
 
/***************************************************************
**
** FUNCTION:    showBufferProc
**
** DESCRIPTION: Callback routine for show buffer toggle button 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void showBufferProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    AppData.showBuffer = toggle->set;

    if (AppData.showBuffer) {
	/*
	** Resize buffers to match current window size and pop up
	*/
	XtVaSetValues(AppData.bufOrig, XtNheight, AppData.drawingHeight,
		      XtNwidth, AppData.drawingWidth, NULL);
	XtVaSetValues(AppData.bufComp, XtNheight, AppData.drawingHeight,
		      XtNwidth, AppData.drawingWidth, NULL);
	XtManageChild(AppData.bufferBox);
    } else XtUnmanageChild(AppData.bufferBox);
} /* end showBufferProc() */

/***************************************************************
**
** FUNCTION:    bufferExposeProc
**
** DESCRIPTION: Callback routine for buffer exposure
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void bufferExposeProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmDrawingAreaCallbackStruct *callback =
	    (XmDrawingAreaCallbackStruct *) callData;
    XExposeEvent *e = &callback->event->xexpose;

    /*
    ** Only update exposed areas
    */
    if (w == AppData.bufOrig) {
	XCopyArea(XtDisplay(w), AppData.original, XtWindow(w), AppData.gc,
		  e->x, e->y, e->width, e->height, e->x, e->y);
    } else {
	XCopyArea(XtDisplay(w), AppData.composite, XtWindow(w), AppData.gc,
		  e->x, e->y, e->width, e->height, e->x, e->y);
    }
} /* end bufferExposeProc() */

/***************************************************************
**
** FUNCTION:    gridSel
**
** DESCRIPTION: Callback routine for grid togglebutton selection.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void gridSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the state of the flag
    */
    AppData.gridOn = toggle->set;
    drawSelfAndUpdate();
} /* end gridSel() */

/***************************************************************
**
** FUNCTION:    copyAllSel
**
** DESCRIPTION: Callback routine for copyAll togglebutton selection.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void copyAllSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the state of the flag
    */
    AppData.copyAll = toggle->set;
} /* end copyAllSel() */

/***************************************************************
**
** FUNCTION:    mouseSel
**
** DESCRIPTION: Callback routine for mouse sensitivity selection.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void mouseSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** If setting, change stored hit size and redraw sensitivity
    */
    if (toggle->set) {
        AppData.hitSize = num;
	drawSensitivityCircle();
    }
} /* end mouseSel() */

/***************************************************************
**
** FUNCTION:    magnificationSel
**
** DESCRIPTION: Callback routine for magnification selection.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void magnificationSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** If set, start zooming by setting zoom cursor
    */
    if (toggle->set && AppData.magnify != num) {
	AppData.zooming = True;
	AppData.magnify = num;
	if (zoomCursor != None) {
	    XDefineCursor(XtDisplay(AppData.drawingArea),
			  XtWindow(AppData.drawingArea), zoomCursor);
	}
    }
} /* end magnificationSel() */
 
/***************************************************************
**
** FUNCTION:	mouseDown
**
** DESCRIPTION:	This function acts as the event handler for the
**		ButtonDown event.  If the magnification is being
**		changed (zooming), then scale and position the 
**		drawing view. Else check for hit detection on the 
**		Bezier or the control points.
**
** PARAMETERS:	w		window widget
**		clientData	clientdata
**		event		event information
**		goOn		continue to dispatch
**
** RETURN:	None
**
***************************************************************/

/* ARGSUSED */

static void mouseDown(w, clientData, event, goOn)
    Widget	w;
    XtPointer clientData;
    XEvent	*event;
    Boolean	*goOn;
{
    XPoint	xpoint;
    Point	point;
    int		ptNum;
    XButtonPressedEvent *bp = (XButtonPressedEvent *) event;
 
    if (event->xany.window != XtWindow(AppData.drawingArea)) return;

    xpoint.x = bp->x;
    xpoint.y = bp->y;

    /*
    ** If zooming, rescale so clicked-on point remains fixed
    */
    if (AppData.zooming) {
	XDPSChainTextContext(AppData.dpsCtxt, AppData.zoomTrace);
	
        /*
        ** Convert point to PS units and scale
        */
	convertToDPS(&xpoint, &point);
	scaleDrawingArea();
	positionDrawingArea(point.x, point.y, xpoint.x, xpoint.y);

	drawSelfAndUpdate();

	AppData.zooming = False;
	if (zoomCursor != None) {
	    XUndefineCursor(XtDisplay(w), XtWindow(w));
	}
	if (AppData.zoomTrace) XDPSChainTextContext(AppData.dpsCtxt, False);

    } else {
	/*
	** Not zooming; do hit detection.  If not already selected, see if
	** the user hit it; redraw showing control points
	*/
	if (!AppData.selected) {
	    if (hitObject(&xpoint)) {
		AppData.selected = True;
		drawSelfAndUpdate();
	    }
	} else {
	    /*
	    ** Already selected; see if the user hit a control point
	    ** or the curve itself.  If so, reshape or move; if not,
	    ** redraw without control points
	    */
	    if (hitControl(&xpoint, &ptNum)) reshapeObject(&xpoint, ptNum);
	    else if (hitObject(&xpoint)) moveObject(&xpoint);	
	    else {
		AppData.selected = False;
		drawSelfAndUpdate();
	    }
	}
    }
} /* end mouseDown() */
