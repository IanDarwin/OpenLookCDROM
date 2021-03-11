/*
 * $RCSfile: ControlMain.c,v $
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

#include "Control.h"

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
    "Control.uid"
};

static void refreshWindow(), createProc(), quitApp(),
	traceProc(), drawProc(), devIndepSel(),
	markStartTime(), numPointsSel(), pointTypeSel();

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

/* Names and addresses for Mrm to bind */
static MrmRegisterArg RegList [] =
{
    {"refreshWindow"   , (XtPointer) refreshWindow   },
    {"createProc"      , (XtPointer) createProc      },
    {"quitApp"         , (XtPointer) quitApp         },
    {"traceProc"       , (XtPointer) traceProc       },
    {"drawProc"	       , (XtPointer) drawProc	     },
    {"numPointsSel"    , (XtPointer) numPointsSel    },
    {"pointTypeSel"    , (XtPointer) pointTypeSel    },
    {"devIndepSel"     , (XtPointer) devIndepSel     },
};

/***************************************************************
**
** FUNCTION:    main
**
** DESCRIPTION: OS transfer point.  The main routine does all
**              the one-time setup and then calls dispatching loop
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
	&AppData.appContext, "Control", CommandLineOptions,
	(Cardinal) XtNumber(CommandLineOptions), &argc, argv,
	(String *) NULL, (ArgList) NULL, 0
	);

    XtGetApplicationResources (
	toplevel, (XtPointer) &AppData, (XtResourceList) Resources,
	XtNumber(Resources), (ArgList) NULL, 0
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
    ** Do all the post-realization DPS/X processing here
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
** FUNCTION:    refreshWindow
**
** DESCRIPTION: Callback routine to handle expose events.
**              Causes the window to be refreshed.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void refreshWindow (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int i = 8;
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
**              Saves the widget id
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void createProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int     widgetNum = *(int *) clientData;

    switch (widgetNum)
    {
	case cMainDrawArea:	AppData.drawingArea = w;	break;
	case cTimingText0:	AppData.timing0 = w; 		break;
	case cTimingText1:	AppData.timing1 = w;		break;
	case cTimingText2:	AppData.timing2 = w;		break;
	case cTimingText3:	AppData.timing3 = w; 		break;
	case cTimingText4:	AppData.timing4 = w;		break;
	case cTimingText5:	AppData.timing5 = w;		break;
	case cTimingText6:	AppData.timing6 = w;		break;
        case cButton3:		AppData.button3 = w;		break;

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
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
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
**
** PARAMETERS:  w          	callback widget ID
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
    erasePage();
    DPSWaitContext(AppData.dpsCtxt);
    markStartTime (&start);
    (*proc) ();
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
**              Redraws the patterns and displays the time.
**
** PARAMETERS:  w           callback widget ID
**		clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/
static void drawProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int buttonNum = *(int *) clientData;
    int i;

    if (AppData.numPoints == 0) return;

    /*
    ** Redraw the points
    */
    switch (buttonNum)
    {
        case 0:     /* draw basic points */
	    doDraw(AppData.timing0, drawBasic);
            break;

        case 1:     /* draw cached user path */
            doDraw(AppData.timing1, drawUserCache);
            break;

        case 2:     /* draw user path */
            doDraw(AppData.timing2, drawUserPath);
            break;

        case 3:     /* draw using rectangle operators */
            doDraw(AppData.timing3, drawRectOp);
            break;

        case 4:     /* draw with pixmaps */
            doDraw(AppData.timing4, drawPixmaps);
            break;

        case 5:     /* draw with show */
            doDraw(AppData.timing5, drawShow);
            break;

        case 6:     /* draw with xyshow */
            doDraw(AppData.timing6, drawXYShow);
            break;

        case 7:     /* draw all */
	    for (i = 0; i < 7; i++) {
		if (i != 3 || XtIsSensitive(AppData.button3)) {
		    drawProc (w, (XtPointer) &i, callData);
		} else setTimingValue(AppData.timing3, 0);
		flushTiming (XtDisplay(w));
	    }
	    break;

	case 8:		/* special for expose events */
	    erasePage();
	    drawXYShow();
	    break;
    }
} /* end methodsProc () */

/***************************************************************
**
** FUNCTION:    numPointsSel
**
** DESCRIPTION: Callback routine for number of points selection.
**
** PARAMETERS:  w           	callback widget ID
**		clientData  	callback client data
**		callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void numPointsSel (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the lines to draw count
    */
    if (toggle->set) AppData.numPoints = num;
    else AppData.numPoints = 0;

    /*
    ** Choose a new starting index to instill a little change
    ** in the display
    */
    AppData.index = (rand() % (MAX_POINTS - AppData.numPoints + 1)) * 2;
} /* end numPointsSel () */

/***************************************************************
**
** FUNCTION:    pointTypeSel
**

** DESCRIPTION: Callback routine for number of points selection.
**
** PARAMETERS:  w		callback widget ID
**		clientData	callback client data
**		callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void pointTypeSel (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the lines to draw count
    */
    if (toggle->set) {
        switch (num) {
            case 0:     /* selected filled rectangles */
                setRectFill ();
		XtSetSensitive(AppData.button3, True);
                break;

            case 1:     /* selected X's */
                setX ();
		XtSetSensitive(AppData.button3, False);
                break;

            case 2:     /* selected empty rectangles */
                setRectOpen ();
		XtSetSensitive(AppData.button3, True);
                break;

            case 3:     /* selected crosses */
                setCross ();
		XtSetSensitive(AppData.button3, False);
                break;
        }
    }

} /* end pointTypeSel () */

/***************************************************************
**
** FUNCTION:    devIndepSel
**
** DESCRIPTION: Callback routine for device independence selection.
**
** PARAMETERS:  w           	callback widget ID
**		clientData	callback client data
**		callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void devIndepSel (w, clientData, callData) 
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the device independence toggle button setting
    */
    AppData.devIndependent = toggle->set;
} /* end devIndepSel () */

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
