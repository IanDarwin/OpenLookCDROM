/*
 * $RCSfile: DialMain.c,v $
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

#include "Dial.h"

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
AppDataType     AppData;

static XrmOptionDescRec CommandLineOptions[] = {
  {
    "-trace", "*trace", XrmoptionNoArg, "true"
    },
};

static XtResource Resources[] = {
  {
    "trace",
    "Trace",
    XtRBoolean,
    sizeof(Boolean),
    XtOffset (AppDataTypePtr, trace),
    XtRImmediate,
    (caddr_t) False
    },
};

static String FallbackResources [] = {
  NULL,
};

/*
** Global resource management data
*/
static MrmHierarchy SMrmHierarchy;          /* MRM database hierarchy ID */
static MrmType      DummyClass;             /* and class variable. */
static char         *DbFilenameVec [] =     /* Mrm.heirarchy file list. */
{
    "Dial.uid"
};

static void refreshWindow(), createProc(), quitApp(), traceProc(),
	drawProc(), marksSel(), markStartTime(), initApplication(),
	setTimingValue(); 

static long getElapsedTime();

/* Names and addresses for Mrm to bind */
static MrmRegisterArg RegList [] =
{
    {"refreshWindow"   , (XtPointer) refreshWindow   },
    {"createProc"      , (XtPointer) createProc      },
    {"quitApp"         , (XtPointer) quitApp         },
    {"traceProc"       , (XtPointer) traceProc       },
    {"drawProc"        , (XtPointer) drawProc 	     },
    {"marksSel"        , (XtPointer) marksSel        },
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
    Widget topLevel, mainWindow;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    /*
    ** Initialize the X Toolkit. We get back a top level shell widget.
    */
    topLevel = XtVaAppInitialize (&AppData.appContext,
				  "Dial",
				  CommandLineOptions,
				  XtNumber(CommandLineOptions),
				  &argc, argv,
				  FallbackResources,
				  NULL);

    if (! XDPSExtensionPresent(XtDisplay(topLevel))) {
        fprintf (stderr, "%s:  DPS extension not in server\n", argv [0]);
        exit (0);
    }

    XtVaGetApplicationResources (topLevel,
				 (XtPointer) &AppData,
				 (XtResourceList) Resources,
				 (Cardinal) XtNumber(Resources),
				 NULL);
    /*
    ** Open the UID files (the output of the UIL compiler)
    */
    if (MrmOpenHierarchy (XtNumber(DbFilenameVec), DbFilenameVec,
			  NULL, &SMrmHierarchy)  !=  MrmSUCCESS)
    {
        fputs ("Can't open heirarchy\n", stderr);
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
    if (MrmFetchWidget (SMrmHierarchy, "MainWindow", topLevel,
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
	topLevel,
	XmNallowShellResize, True,
    	XmNmwmFunctions,
        MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE,
        XmNmwmDecorations,
        MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_RESIZEH,
        NULL);
    XtRealizeWidget (topLevel);

    /*
    ** Do all the post-realization DPSX processing here
    */
    initDPSContext ();

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
**              structures.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/

static void initApplication ()
{
    /*
    ** Initialize the marks to draw selection toggle button states
    */
    AppData.ninety    = TRUE;
    AppData.fortyFive = TRUE;
    AppData.ten       = TRUE;
    AppData.one       = FALSE;
} /* end initApplication () */

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
    int i = 5;
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
	/*
	 ** Save widget ID in application data structure
	 */
        case cMainDrawArea:	AppData.drawArea = w;		break;
	case cTimingText0:	AppData.timing0 = w; 		break;
	case cTimingText1:	AppData.timing1 = w;		break;
	case cTimingText2:	AppData.timing2 = w;		break;
	case cTimingText3:	AppData.timing3 = w; 		break;

        case cTraceToggle:

            /*
            ** If trace turned on by command line, set button
            */
	    XmToggleButtonGadgetSetState (w, AppData.trace, False);
            break;

        default:
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
** DESCRIPTION: Callback routine for trace toggle button.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
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
** FUNCTION:    doDraw
**
** DESCRIPTION: Draw using a method and update timing window.
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

    drawDialBackground();

    setTimingValue (timing, 0);
    DPSWaitContext(AppData.dpsCtxt);
    markStartTime (&start);
    (*proc) ();
    DPSWaitContext(AppData.dpsCtxt);
    setTimingValue (timing, getElapsedTime(&start));
    drawDialBorder();
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
**              Redraws the dial and displays the time.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
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

    /*
    ** Redraw the lines
    */
    switch (buttonNum)
    {
        case 0:     /* wraps/rotate */
            doDraw(AppData.timing0, drawRotate);
            break;

        case 1:     /* wraps/trig */
            doDraw(AppData.timing1, drawTrig);
            break;

        case 2:     /* upaths/trig */
	    doDraw(AppData.timing2, drawTrigUserPaths);
            break;

        case 3:     /* upaths/server */
            doDraw(AppData.timing3, drawTrigUserPathsServer);
            break;

        case 4:     /* draw all */
	    for (i = 0; i < 4; i++) {
		drawProc(w, (XtPointer) &i, callData);
		flushTiming(XtDisplay(w));
	    }
            break;

	case 5:		/* DRAW_EXPOSE */
	    drawDialBackground();
	    drawTrigUserPathsServer();
	    drawDialBorder();
	    break;
    }
} /* end drawProc () */

/***************************************************************
**
** FUNCTION:    setTimingValue
**
** DESCRIPTION: Routine to set timing value in label.
**
** PARAMETERS:  widget      timing label
**              iTime       timing value
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void setTimingValue (widget, iTime)
    Widget widget;
    int iTime;
{
    char cTime [15];

    sprintf (cTime, "%d", iTime);

    XtVaSetValues (widget,
		   XmNlabelString, XmStringCreateSimple (cTime),
		   NULL);
} /* end setTimingValue () */

/***************************************************************
**
** FUNCTION:    marksSel
**
** DESCRIPTION: Callback routine for marks to draw selection.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void marksSel (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    int num = *(int *) clientData;

    /*
    ** Change the state of the toggle button
    */

    switch (num)
    {
        case 0:     /* 90 degrees */
            AppData.ninety = toggle->set;
            break;

        case 1:     /* 45 degrees */
            AppData.fortyFive = toggle->set;
            break;

        case 2:     /* 10 degrees */
            AppData.ten = toggle->set;
            break;

        case 3:     /* 1 degree */
            AppData.one = toggle->set;
            break;
    }

} /* end marksSel () */

/***************************************************************
**
** FUNCTION:    markStartTime
**
** DESCRIPTION: this procedure calls gettimeofday (3s) to mark
**              the start of the elapsed time.
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
** DESCRIPTION: Callback routine for number of lines selection.
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
