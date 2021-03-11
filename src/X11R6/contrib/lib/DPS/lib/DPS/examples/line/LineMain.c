/*
 * $RCSfile: LineMain.c,v $
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
** INCLUDE FILES
**
***************************************************************/

#include "Line.h"

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
AppDataType     AppData;

/*
** Global resource management data
*/
static MrmHierarchy SMrmHierarchy;	/* MRM database hierarchy ID */
static MrmType	DummyClass;		/* and class variable. */
static char	*DbFilenameVec [] =	/* Mrm.heirarchy file list. */
{
    "Line.uid"
};

static void initApplication(), resetProc(), scaleValueChanged(),
	textValueChanged(), refreshWindow(), createProc(), quitApp(),
	traceProc(), drawProc(), colorWidthProc(), numLineSel(),
	markStartTime();

static long getElapsedTime();

static XtResource Resources[] = {
	{
		"trace",
		"Trace",
		XtRBoolean,
		sizeof(Boolean),
		XtOffset(AppDataTypePtr,trace),
		XtRImmediate,
		(XtPointer)False
	},
};

static XrmOptionDescRec CommandLineOptions[] = {
	{
		"-trace",	/* PostScript trace command line option */
		".trace",	/* resource identifier */
		XrmoptionNoArg,
		(XtPointer)"true"
	},
};

static String FallbackResources[] = {
	NULL
};

/* Names and addresses for Mrm to bind */
static MrmRegisterArg RegList [] =
{
	{"resetProc",		(caddr_t) resetProc	  	},
	{"textValueChanged",	(caddr_t) textValueChanged	},
	{"refreshWindow",	(caddr_t) refreshWindow   	},
	{"createProc", 		(caddr_t) createProc      	},
	{"quitApp", 		(caddr_t) quitApp         	},
	{"traceProc",		(caddr_t) traceProc		},
	{"colorWidthProc",	(caddr_t) colorWidthProc	},
	{"drawProc",		(caddr_t) drawProc     		},
	{"numLineSel",		(caddr_t) numLineSel      	},
	{"scaleValueChanged",	(caddr_t) scaleValueChanged 	}
};

/***************************************************************
**
** FUNCTION:    main
**
** DESCRIPTION: OS transfer point.  The main routine does all
**              the one-time setup and then goes into its dispatching
**		loop.
**
** PARAMETERS:  argc    command line argument count
**              argv    array of pointers to command line args.
**
** RETURN:      None.
**
***************************************************************/

void main (argc, argv)
    int argc;
    char *argv [];
{
    Widget toplevel;
    Widget mainWindow;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    /*
    ** Initialize the X Toolkit. We get back a top level shell widget.
    */
    toplevel = XtAppInitialize (
	&AppData.appContext, "Line", (XrmOptionDescList)CommandLineOptions,
	(Cardinal)XtNumber(CommandLineOptions), &argc, argv,
	(String *)FallbackResources, (ArgList)NULL, 0
	);

    XtGetApplicationResources (
	toplevel, (XtPointer)&AppData, (XtResourceList)Resources,
	XtNumber(Resources), (ArgList)NULL, 0
	);

    if (!XDPSExtensionPresent(XtDisplay(toplevel)))
    {
        fprintf (stderr, "%s:  DPS extension not in server\n", argv [0]);
        exit (1);
    }

    /*
    ** Open the UID files (the output of the UIL compiler)
    */
    if (MrmOpenHierarchy (XtNumber(DbFilenameVec), DbFilenameVec, NULL,
        &SMrmHierarchy)  !=  MrmSUCCESS)
    {
        fprintf (stderr, "Can't open heirarchy\n");
        exit (1);
    }

    /*
    ** Perform onetime initialization of application data structures.
    */
    initApplication ();

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
        exit (1);
    }

    /*
    ** Manage the main window and option box and realize everything.
    ** The interface comes up on the display now.
    */
    XtManageChild (mainWindow);
    XtVaSetValues(
	toplevel,
    	XmNmwmFunctions,
        MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE,
        XmNmwmDecorations,
        MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_RESIZEH,
        NULL
    );
    XtRealizeWidget (toplevel);

    /*
    ** Do all the post-realization DPSX processing here
    */
    initDPSContext (toplevel);

    /*
    ** Sit around forever waiting to process X-events.
    ** From here on, we only execute our callback routines.
    */
    while (1) {
	XEvent event;
	XtAppNextEvent(AppData.appContext, &event);
	if (!XDPSDispatchEvent(&event)) (void) XtDispatchEvent(&event);
    }
}

/***************************************************************
**
** FUNCTION:    initApplication
**
** DESCRIPTION: One-time initialization of application data
**				structures.
**
** PARAMETERS:	None.
**
** RETURN:      None.
**
***************************************************************/

static void initApplication ()
{
    /*
    ** initialize the random number generator seed
    */
    srand(time((time_t *) NULL));

    /*
    ** Initialize application state variables
    */
    AppData.numberChanged = False;
    AppData.numberOfLines = 0;
} /* end initApplication () */

/***************************************************************
**
** FUNCTION:    refreshWindow
**
** DESCRIPTION: Callback routine to handle expose events.
**              Causes the window to be refreshed.
**
** PARAMETERS:
**	w		callback widget ID
**	clientData	callback client data
**	callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void refreshWindow (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int i = 7;
    register Display *dpy = XtDisplay(w);
    XEvent event;

    /*
    ** Pseudo Exposure event compression for the Drawing Area Widget
    */
    if (XPending(dpy) > 0) {
	XPeekEvent(dpy, &event);
	if (event.type == Expose && event.xany.window == XtWindow(w)) return;
    }

    drawProc (w, (XtPointer) &i, NULL);
} /* end refreshWindow () */

/***************************************************************
**
** FUNCTION:    createProc
**
** DESCRIPTION: Callback routine for widget creation.
**              Saves the widget id in an array.
**
** PARAMETERS:	w		callback widget ID
**		clientData	callback client data
**		callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void createProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int widgetNum = *((int *)clientData);

    switch (widgetNum) {
	case cMainDrawArea:	AppData.drawingArea = w;	break;
	case cTimingText0:	AppData.timing0 = w; 		break;
	case cTimingText1:	AppData.timing1 = w;		break;
	case cTimingText2:	AppData.timing2 = w;		break;
	case cTimingText3:	AppData.timing3 = w; 		break;
	case cTimingText4:	AppData.timing4 = w;		break;
	case cTimingText5:	AppData.timing5 = w;		break;
	case cScaleForm:	AppData.scaleForm = w;		break;
	case cTotalText:	AppData.totalText = w;		break;
	case cColorWidthButton:	AppData.colorWidthButton = w;	break;
	case cColorScale:	AppData.colorScale = w; 	break;
	case cWidthScale:	AppData.widthScale = w;		break;

	case cTraceToggle:
	    if (AppData.trace) XtVaSetValues (w, XmNset, True, NULL);
            break;

    } /* end switch */

} /* end createProc () */

/***************************************************************
**
** FUNCTION:    quitApp
**
** DESCRIPTION: Callback routine for "quit" command menu
**              selection.  Exits from the application.
**
** PARAMETERS: 	w		callback widget ID
**		clientData	callback client data
**		callData	callback Motif data structure
**
** RETURN:      Returns to OS.
**
***************************************************************/

/* ARGSUSED */

static void quitApp (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
    exit (0);
} /* end quitApp () */

/***************************************************************
**
** FUNCTION:    traceProc
**
** DESCRIPTION: Callback routine for the trace toggle button.
**		Enables or disables text trace.
**
** PARAMETERS:  w           callback widget ID
**		clientData	callback client data
**		callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void traceProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    /*
    ** Change the state of the toggle button
    */
    XDPSChainTextContext (AppData.dpsCtxt, toggle->set);
} /* end traceProc () */

/***************************************************************
**
** FUNCTION:    colorWidthProc
**
** DESCRIPTION: Callback routine for the color/width toggle button.
**		Enables or disables sliders and fills color and
**		width arrays.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void colorWidthProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    XtSetSensitive (AppData.scaleForm, (Boolean) toggle->set);
    makeColorWidth(AppData.numberOfLines);
} /* end colorWidthProc () */


/***************************************************************
**
** FUNCTION:    scaleValueChanged
**
** DESCRIPTION: Callback routine when either the line width or color
**		scale widget values have been changed.  Fills color
**		and width arrays.
**
** PARAMETERS:	w		callback widget ID
**      	clientData	callback client data
**      	callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void scaleValueChanged (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    makeColorWidth(AppData.numberOfLines);
}

/***************************************************************
**
** FUNCTION:    setTimingValue
**
** DESCRIPTION: Routine to set timing value in options box.
**
** PARAMETERS:
**	w	label widget to change
**	iTime	timing value
**
** RETURN:      None.
**
***************************************************************/

void setTimingValue (w, iTime)
    Widget w;
    long iTime;
{
    char cTime [15];

    if (iTime  ==  0) strcpy (cTime, " ");
    else sprintf (cTime, "%d", iTime);

    XtVaSetValues (w,
		   XmNlabelString, XmStringCreateSimple(cTime),
		   NULL);
}

/***************************************************************
**
** FUNCTION:    doDraw
**
** DESCRIPTION: Draw using a method and update timing window
**
** PARAMETERS:	timing		timing window to update
**		proc		drawingProcedure
**
** RETURN:      None.
**
***************************************************************/
	
static void doDraw(timing, proc)
    Widget timing;
    void (*proc)();
{
    struct timeval start;

    setTimingValue (timing, 0);
    DPSWaitContext(AppData.dpsCtxt);
    markStartTime (&start);
    (*proc) (AppData.numberOfLines);
    DPSWaitContext(AppData.dpsCtxt);
    setTimingValue (timing, getElapsedTime(&start));
}

/***************************************************************
**
** FUNCTION:    flushTiming
**
** DESCRIPTION: Forces an update of the timing windows by syncing and
**		dispatching the resulting Expose events.  Yuck
**
** PARAMETERS:	dpy	Display to flush
**
** RETURN:      None.
**
***************************************************************/
static void flushTiming(dpy)
    Display *dpy;
{
    XEvent event;

    XSync(dpy, False);
    while (XtAppPending(AppData.appContext)) {
	XtAppNextEvent(AppData.appContext, &event);
	if (!XDPSDispatchEvent(&event)) (void) XtDispatchEvent (&event);
    }
    XFlush(dpy);
}

/***************************************************************
**
** FUNCTION:    drawProc
**
** DESCRIPTION: Callback routine for redraw button pushed.
**              Redraws the lines and displays the time.
**
** PARAMETERS:
**	w		callback widget ID
**	clientData	callback client data
**	callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/
static void drawProc (w, clientData, callData)
    Widget w;
    XtPointer clientData;
    XtPointer callData;
{
    int i;
    int buttonID = *(int *)clientData;

    if (AppData.numberOfLines == 0) {
	erasePage ();
	return;
    }

    if (AppData.numberChanged) {
	makeLines (AppData.numberOfLines);
	makeColorWidth (AppData.numberOfLines);
	AppData.numberChanged = False;
    }

    /*
    ** Redraw the lines
    */
    switch (buttonID) {
	case 0:		/* DPSPrintf */
	    doDraw(AppData.timing0, drawDPSPrintf);
	    break;

	case 1:		/* single operations */
	    doDraw(AppData.timing1, drawSingleOps);
	    break;

	case 2:		/* wraps */
	    doDraw(AppData.timing2, drawSimpleWraps);
	    break;

	case 3:		/* wraps w/binding */
	    doDraw(AppData.timing3, drawWrapsBind);
	    break;

	case 4:		/* wraps w/repeat */
	    doDraw(AppData.timing4, drawWrapsRepeat);
	    break;

	case 5:		/* wraps optimized */
	    doDraw(AppData.timing5, drawOptimizedStroke);
	    break;


	case 6:		/* draw all */
	    for (i = 0; i < 6; i++) {
		drawProc (w, (XtPointer) &i, callData);
		flushTiming (XtDisplay(w));
	    }
	    break;

	case 7:		/* special for expose events */
	    drawOptimizedStroke(AppData.numberOfLines);
	    break;

	default:
	    erasePage();
	    break;
    }
} /* end drawProc () */

/***************************************************************
**
** FUNCTION:    numLineSel
**
** DESCRIPTION: Callback routine for number of lines buttons.
**		Sets the number in the text field; the callback
**		routine for the field actually does the work.
**
** PARAMETERS:	w		callback widget ID
**		clientData	callback client data
**		callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void numLineSel (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    static int numLinesArray[] = { 10, 100, 500, 1000, 0 };
    int num = *((int *) clientData);
    char cCount [15];
    int numLines;

    if ((num >= 0) && (num < 5)) numLines = numLinesArray[num];
    else numLines = 0;

    sprintf (cCount, "%d", numLines);
    XmTextSetString (AppData.totalText, cCount);
} /* end numLineSel () */

/***************************************************************
**
** FUNCTION:    markStartTime
**
** DESCRIPTION: routine to set the start time of the DPS drawing method
**
** PARAMETERS:
**	startTime - pointer to struct timeval where current time is stored
**
** RETURN:      None.
**
***************************************************************/
static void markStartTime (startTime)
    struct timeval *startTime;
{
    struct timezone timeZone;

    gettimeofday (startTime, &timeZone);
}

/***************************************************************
**
** FUNCTION:    getElapsedTime
**
** DESCRIPTION: Returns milliseconds since startTime
**
** PARAMETERS:
**	startTime - pointer to struct timeval where the start time is kept
**
** RETURN:
**	long - the elapsed time since the start in milliseconds
**
***************************************************************/
static long getElapsedTime (startTime)
    struct timeval *startTime;
{
    struct timezone timeZone;
    struct timeval finishTime;
    long elapsedSeconds, elapsedMicroseconds;

    gettimeofday (&finishTime, &timeZone);
    elapsedSeconds = finishTime.tv_sec - startTime->tv_sec;
    elapsedMicroseconds = finishTime.tv_usec - startTime->tv_usec;

    return ((long)(elapsedSeconds * 1000 + (elapsedMicroseconds/1000)));
}

/***************************************************************
**
** FUNCTION:    textValueChanged
**
** DESCRIPTION: Callback routine for any value change in the text widget
**		where the total number of lines to draw is kept.
**
** PARAMETERS:
**	w		callback widget ID
**	clientData	callback client data
**	callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void textValueChanged (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    char newText[256];
    String string;
    int numLines;
    static Boolean recurring;

    /*
    ** Handle bad numeric input by converting string to an integer and
    ** printing the result in the text field.  This will cause a recursive
    ** call, which we ignore.
    */

    if (recurring) return;

    string = XmTextGetString (AppData.totalText);
    numLines = atoi (string);
    XtFree (string);
    if (numLines > MAXARRAY) numLines = MAXARRAY;
    if (numLines < 0) numLines = 0;

    recurring = True;
    (void) sprintf (newText, "%d", numLines);
    XmTextSetString (AppData.totalText, newText);
    XmTextSetInsertionPosition (AppData.totalText, strlen(newText));
    recurring = False;

    AppData.numberChanged = True;
    AppData.numberOfLines = numLines;
}

/***************************************************************
**
** FUNCTION:    resetProc
**
** DESCRIPTION: Callback routine for the reset button activation
**
** PARAMETERS:
**	w		callback widget ID
**	clientData	callback client data
**	callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void resetProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmTextSetString (AppData.totalText, "0");
    XmScaleSetValue (AppData.colorScale, 0);
    XmScaleSetValue (AppData.widthScale, 200);
    XmToggleButtonSetState (AppData.colorWidthButton, True, True);

    setTimingValue(AppData.timing0, 0);
    setTimingValue(AppData.timing1, 0);
    setTimingValue(AppData.timing2, 0);
    setTimingValue(AppData.timing3, 0);
    setTimingValue(AppData.timing4, 0);
    
    erasePage();
}

