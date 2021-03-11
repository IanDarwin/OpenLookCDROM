/*
 * $RCSfile: StrokeMain.c,v $
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

#include "Stroke.h"

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/

/*
** Global pointers to the application name and data block
*/
AppDataType     AppData;

static Widget   WidgetArray [MAX_WIDGETS];  /* Place to keep all */
                                            /* widget IDs */

static void initApplication(), refreshWindow(), createProc(),
	    quitApp(), traceProc(), drawProc();

/*
** Global resource management data
*/
static MrmHierarchy SMrmHierarchy;          /* MRM database hierarchy ID */
static MrmType      DummyClass;             /* and class variable. */
static char         *DbFilenameVec [] =     /* Mrm.heirarchy file list. */
{
    "Stroke.uid"
};

/* Names and addresses for Mrm to bind */
static MrmRegisterArg RegList [] =
{
    {"refreshWindow"   , (caddr_t) refreshWindow   },
    {"createProc"      , (caddr_t) createProc      },
    {"quitApp"         , (caddr_t) quitApp         },
    {"traceProc"       , (caddr_t) traceProc       },
    {"drawProc"        , (caddr_t) drawProc        },
};

static XrmOptionDescRec CommandLineOptions[] = {
  {
    "-trace", ".trace", XrmoptionNoArg, (XtPointer)"true"
    },
};

static XtResource ApplicationResources[] = {
  {
    "trace",
    "Trace",
    XtRBoolean,
    sizeof(Boolean),
    XtOffset (AppDataTypePtr, traceToggle),
    XtRImmediate,
    (XtPointer)False
    },
};

static String FallbackResources[] = {
  NULL
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
    Widget TopLevelShell, MainWindowWidget;
    XtAppContext appContext;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    TopLevelShell = XtVaAppInitialize (
					&appContext,
					"Stroke",
					(XrmOptionDescList) CommandLineOptions,
					(Cardinal)XtNumber(CommandLineOptions),
					&argc,
					argv,
					(String *)FallbackResources,
					XmNallowShellResize, True,
					XmNmwmFunctions, MWM_FUNC_ALL |
				                         MWM_FUNC_RESIZE |
				                         MWM_FUNC_MAXIMIZE |
				                         MWM_FUNC_CLOSE,
					XmNmwmDecorations, MWM_DECOR_ALL |
				                           MWM_DECOR_MAXIMIZE |
				                           MWM_DECOR_RESIZEH,
					NULL
					);

    if (!XDPSExtensionPresent(XtDisplay(TopLevelShell)))
    {
        fprintf (stderr, "%s:  DPS extension not in server\n", argv [0]);
        exit (1);
    }
    XtVaGetApplicationResources (
				 TopLevelShell,
				 (XtPointer)&AppData,
				 (XtResourceList) ApplicationResources,
				 (Cardinal) XtNumber(ApplicationResources),
				 NULL
				 );

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
    if (MrmFetchWidget (SMrmHierarchy, "MainWindow", TopLevelShell,
			&MainWindowWidget, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf (stderr, "Can't fetch main window\n");
        exit (1);
    }

    /*
    ** Manage the main window and option box and realize everything.
    ** The interface comes up on the display now.
    */
    XtManageChild (MainWindowWidget);
    XtRealizeWidget (TopLevelShell);

    /*
    ** Do all the post-realization DPSX processing here
    */
    initDPSContext (XtDisplay(TopLevelShell));

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

static void initApplication ()
{
    /*
    ** Initialize line type toggle buttons
    */
    AppData.horizontalT = False;
    AppData.verticalT   = False;
    AppData.diagonalT   = False;
    AppData.arcT        = False;

    /*
    ** Initialize line width value
    */
    AppData.lineWidth = 0.0;

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
    Dimension height;
    XEvent event;

    /*
    ** Pseudo Exposure event compression for the Drawing Area Widget
    */
    if (XPending(XtDisplay(w)) > 0) {
	XPeekEvent(XtDisplay(w), &event);
	if (event.type == Expose && event.xany.window == XtWindow(w)) return;
    }

    XtVaGetValues(w, XtNheight, &height, NULL);

    if (w == AppData.window0) {
	XDPSSetContextGState(AppData.dpsCtxt, AppData.gs0);
    } else XDPSSetContextGState(AppData.dpsCtxt, AppData.gs1);

    PSsetlinewidth (AppData.lineWidth);
    PSerasepage();
    if (AppData.horizontalT) makeHorizLines();
    if (AppData.verticalT) makeVertLines();
    if (AppData.arcT) makeArcs ();
    if (AppData.diagonalT) makeDiagLines();
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

    /*
    ** Save widget ID
    */
    WidgetArray [widgetNum] = w;

    /*
    ** Save widget ID in application data structure
    */
    switch (widgetNum)
    {
        case cMainDrawArea0:
	    AppData.window0 = w;
	    break;

        case cMainDrawArea1:
	    AppData.window1 = w;
            break;

        case cTraceToggle:
            /*
            ** If trace turned on by command line, set button
            */
	    XmToggleButtonGadgetSetState (w, AppData.traceToggle, False);
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
** FUNCTION:    drawProc
**
** DESCRIPTION: Callback routine for draw button pushed.
**              Reads the toggle states and line width
**              scale value.  If necessary, calls drawIt ()
**              to redraw the lines and display the times.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void drawProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int temp;

    /*
    ** Get the state of the toggle buttons
    */
    AppData.horizontalT = XmToggleButtonGetState (WidgetArray [cTypeButton0]);
    AppData.diagonalT   = XmToggleButtonGetState (WidgetArray [cTypeButton1]);
    AppData.verticalT   = XmToggleButtonGetState (WidgetArray [cTypeButton2]);
    AppData.arcT        = XmToggleButtonGetState (WidgetArray [cTypeButton3]);

    /*
    ** Read the scale value for line width
    */
    XmScaleGetValue(WidgetArray [cWidthScale], &temp);
    AppData.lineWidth = (float) temp / 100;

    /*
    ** See if there are any lines to draw
    */
    if (AppData.horizontalT || AppData.diagonalT ||
	AppData.verticalT || AppData.arcT) {
        drawIt ();
    } else {
	setTimingValue (cTimingText0, 0);
	setTimingValue (cTimingText1, 0);
    }
} /* end drawProc () */

/***************************************************************
**
** FUNCTION:    setTimingValue
**
** DESCRIPTION: Routine to set timing value in options box.
**
** PARAMETERS:  num         index in WidgetArray of XmText widget
**              iTime       timing value
**
** RETURN:      None.
**
***************************************************************/

void setTimingValue (num, iTime)
    int num;
    int iTime;
{
    char cTime [15];

    if (iTime == 0) cTime [0] = '\0';
    else sprintf (cTime, "%d", iTime);

    XtVaSetValues (WidgetArray [num],
		   XmNlabelString, XmStringCreateSimple(cTime),
		   NULL);
} /* end setTimingValue () */

