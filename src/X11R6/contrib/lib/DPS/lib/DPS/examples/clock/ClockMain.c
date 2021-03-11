/*
 * $RCSfile: ClockMain.c,v $
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

#include "Clock.h"

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
** Global program state variables
*/

/*
** Global resource management data
*/
static MrmHierarchy SMrmHierarchy;          /* MRM database hierarchy ID */
static MrmType      DummyClass;            /* and class variable. */
static char         *DbFilenameVec [] =     /* Mrm.heirarchy file list. */
{
    "Clock.uid"
};

static XrmOptionDescRec CommandLineOptions[] =
{
  { "-trace", ".trace", XrmoptionNoArg, "true" },
  { "-pixmap", ".pixmapBackground", XrmoptionNoArg, "true", },
  { "-double", ".doubleBuffering", XrmoptionNoArg, "true", },
  { "-gstates", ".graphicStates", XrmoptionNoArg, "true", },
  { "-paths", ".serverPaths", XrmoptionNoArg, "true", },
  { "-alarm", ".alarmOn", XrmoptionNoArg, "true", }
};

XtResource ApplicationResources[] =
{
   {		/* PostScript Text Context Trace Resource */
       "trace", "Trace", XtRBoolean, sizeof(Boolean),
       XtOffset (AppDataTypePtr, trace), XtRImmediate, (caddr_t) False
    },{		/* Clock Alarm Resource */
	"alarm", "Alarm", XtRBoolean, sizeof(Boolean),
	XtOffset (AppDataTypePtr, alarmOn), XtRImmediate, (caddr_t) False
    },{		/* Graphic States Resource */
	"graphicStates", "GraphicStates", XtRBoolean, sizeof (Boolean),
	XtOffset (AppDataTypePtr, graphicStates), XtRImmediate, (caddr_t) False
    },{		/* User Paths in Server Resource */
	"serverPaths", "ServerPaths", XtRBoolean, sizeof (Boolean),
	XtOffset (AppDataTypePtr, serverPaths), XtRImmediate, (caddr_t) False
    },{		/* Face Pixmap Background Resource */
	"pixmapBackground", "PixmapBackground", XtRBoolean, sizeof (Boolean),
	XtOffset (AppDataTypePtr, pixmapBackground),
	XtRImmediate, (caddr_t) False
    },{		/* Double Buffering Resource */
	"doubleBuffering", "DoubleBuffering", XtRBoolean, sizeof (Boolean),
	XtOffset (AppDataTypePtr, doubleBuffering),
	XtRImmediate, (caddr_t) False
    }
};

static void resizeWindow(), createProc(), quitApp(), traceProc(), optionSel(),
	timingProc(), alarmProc(), dismissTimingBox(), mapHandler(),
	refreshWindow(), mouseDown();

/* Names and addresses for Mrm to bind */
static MrmRegisterArg RegList [] =
{
    {"resizeWindow"    , (XtPointer) resizeWindow    },
    {"refreshWindow"   , (XtPointer) refreshWindow    },
    {"createProc"      , (XtPointer) createProc      },
    {"quitApp"         , (XtPointer) quitApp         },
    {"traceProc"       , (XtPointer) traceProc       },
    {"optionSel"       , (XtPointer) optionSel       },
    {"timingProc"      , (XtPointer) timingProc      },
    {"alarmProc"       , (XtPointer) alarmProc       },
    {"dismissTimingBox", (XtPointer) dismissTimingBox},
};

static int ringingAlarmBell = 0;    /* alarm ringing countdown */

/***************************************************************
**
** FUNCTION:    main
**
** DESCRIPTION: OS transfer point.  The main routine does all
**              the one-time setup and then calls the dispatch loop
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
    Widget topLevel, mainWindow, timingShell;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    topLevel = XtVaAppInitialize (&AppData.appContext,
				  "Clock",
				  CommandLineOptions,
				  XtNumber(CommandLineOptions),
				  &argc,
				  argv,
				  NULL,
				  XmNallowShellResize, True,
				  NULL);
    XtAddEventHandler (topLevel, StructureNotifyMask,
		       FALSE, mapHandler, NULL);

    XtVaGetApplicationResources (topLevel,
				 &AppData,
				 ApplicationResources,
				 XtNumber(ApplicationResources),
				 NULL);

    /*
    ** Open the UID files (the output of the UIL compiler)
    */
    if (MrmOpenHierarchy (XtNumber(DbFilenameVec), DbFilenameVec,
			  NULL, &SMrmHierarchy)  !=  MrmSUCCESS) {
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
    if (MrmFetchWidget (SMrmHierarchy, "MainWindow", topLevel,
			&mainWindow, &DummyClass)  !=  MrmSUCCESS) {
        fprintf (stderr, "Can't fetch main window\n");
        exit (1);
    }

    /*
    ** Get the timing box widget
    */
    timingShell = XtVaCreatePopupShell ("ClockTiming",
					xmDialogShellWidgetClass,
					topLevel,
					XmNallowShellResize, True,
					NULL);

    if (MrmFetchWidget (SMrmHierarchy, "TimingBox", timingShell,
			&AppData.timingBox, &DummyClass)  !=  MrmSUCCESS) {
        fprintf (stderr, "Can't fetch timing window\n");
        exit (1);
    }

    /*
    ** Manage and realize the clock (main window)
    */
    XtManageChild (mainWindow);
    XtRealizeWidget (topLevel);

    /*
    ** Do all the post-realization DPSX processing here
    */
    initDPSContext ();

    if (AppData.graphicStates) setGStates();

    AppData.clockPixmap = AppData.facePixmap = None;

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
** FUNCTION:    resizeWindow
**
** DESCRIPTION: Callback routine to handle resize events.
**              The new size is obtained and the face is drawn.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void resizeWindow (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Dimension width, height;

    /*
    ** The window cannot be sized if it is not yet realized
    */
    if (!XtIsRealized (w)) return;

    XtVaGetValues (w, XtNheight, &height, XtNwidth, &width, NULL);

    handleWindowResize(w, width, height);
    drawClockTime();
    AppData.redrewAlready = True;
} /* end resizeWindow () */

/***************************************************************
**
** FUNCTION:    refreshWindow
**
** DESCRIPTION: Callback routine to handle expose events.
**              
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
    register Display *dpy = XtDisplay(w);
    XEvent event;

    /*
    ** Pseudo Exposure event compression for the Drawing Area Widget
    */
    if (XPending(dpy) > 0) {
	XPeekEvent(dpy, &event);
	if (event.type == Expose && event.xany.window == XtWindow(w)) return;
    }

    if (AppData.redrewAlready) return;
    drawClockTime();
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
    Widget          w;
    XtPointer clientData, callData;
{
    int     widgetNum = *(int *) clientData;

    /*
    ** Save widget IDs
    */

    switch (widgetNum)
    {
        case cMainDrawArea:
            AppData.widget = w;
            /*
            ** Add event handler for the mouse button down event
            */
            XtAddEventHandler (w, ButtonPressMask, FALSE, mouseDown, NULL);
            break;

	case cTimingText:
	    AppData.timingText = w;
	    break;

	case cTimingButton:
	    AppData.timingButton = w;
	    break;

        case cTraceToggle:
	    AppData.traceToggle = w;
	    XmToggleButtonSetState (w, AppData.trace, False);
            break;

            /*
	    ** If option turned on by command line, set button
            */
        case cOptionButton0:
            XmToggleButtonSetState (w, AppData.graphicStates, False);
	    break;

        case cOptionButton1:
            XmToggleButtonSetState (w, AppData.serverPaths, False);
	    break;

        case cOptionButton2:
            XmToggleButtonSetState (w, AppData.doubleBuffering, False);
	    break;

        case cOptionButton3:
            XmToggleButtonSetState (w, AppData.pixmapBackground, False);
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
    XtPointer clientData;
    XtPointer callData;
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
    ** Chain text context for trace option if trace is on
    */
    XDPSChainTextContext (AppData.dpsCtxt, toggle->set);
} /* end traceProc () */

/***************************************************************
**
** FUNCTION:    setTimingValue
**
** DESCRIPTION: Routine to set timing value in options box.
**
** PARAMETERS:	iTime       timing value
**
** RETURN:      None.
**
***************************************************************/

void setTimingValue (iTime)
    int iTime;
{
    char cTime [15];

    if (iTime  ==  0) strcpy (cTime, " - ");
    else sprintf (cTime, "%d", iTime);

    XtVaSetValues (AppData.timingText,
		   XmNlabelString, XmStringCreateSimple(cTime), NULL);
} /* end setTimingValue () */

/***************************************************************
**
** FUNCTION:    timingProc
**
** DESCRIPTION: Callback routine for the timing window push button.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void timingProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    int total, i;

    /*
    ** If timing window toggle is on, map the window to make it visible
    ** and update the timing value in the text widget
    */
    if (toggle->set) {
        XtManageChild (AppData.timingBox);

	/*
	** Update timing box with average of last N timings
	*/

        if (AppData.numIterations) {
	    total = 0;
	    for (i = 0; i < AppData.numIterations; i++) {
		total += AppData.lastTimes[i];
	    }
            setTimingValue (total / AppData.numIterations);

        } else setTimingValue (0);

    /*
    ** If timing window toggle is off, unmap the window to make it invisible
    */
    } else XtUnmanageChild (AppData.timingBox);

} /* end timingProc () */

/***************************************************************
**
** FUNCTION:    alarmProc
**
** DESCRIPTION: Callback routine for the alarm on push button.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void alarmProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Mark the alarm on/off state
    */
    AppData.alarmOn = toggle->set;

    /*
    ** Reset the alarm bell count which will turn off a ringing alarm
    */
    ringingAlarmBell = 0;

} /* end alarmProc () */

/***************************************************************
**
** FUNCTION:    optionSel
**
** DESCRIPTION: Callback routine for the option selection buttons.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void optionSel (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    int num = *(int *) clientData;

    switch (num) {
	case 0:     /* graphic states */
            AppData.graphicStates = toggle->set;
	    setGStates();
            break;

        case 1:     /* user paths in server */
            AppData.serverPaths = toggle->set;
            break;

	case 2:     /* double buffering */
	    AppData.doubleBuffering = toggle->set;
	    setBufferRendering();
	    break;

	case 3:     /* Pixmap background */
	    AppData.pixmapBackground = toggle->set;
	    break;
    }

    /*
    ** Reset the number of iterations and total time for drawing
    ** the clock time in the current mode
    */
    AppData.numIterations = 0;
    AppData.redrewAlready = False;

    /*
    ** Reset the timing value in the display
    */
    setTimingValue (0);

} /* end optionSel () */

/***************************************************************
**
** FUNCTION:    clockTimeout
**
** DESCRIPTION: Callback routine for timer events.  Causes the
**              clock to be redrawn.  Check if the clock needs
**              to ring the alarm bell and rings it.
**
** PARAMETERS:  clientData	client data
**		id		timer id
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void clockTimeout (clientData, id)
    XtPointer clientData;
    XtIntervalId *id;
{
    register int i;

    /*
    ** Check to see if the draw area window is mapped (visible)
    */
    if (! AppData.mapped) return;

    /*
    ** If just redrew because of expose event, don't redraw this time
    */

    AppData.redrewAlready = False;
    drawClockTime ();

    /*
    ** Check to see if alarm is turned on
    */
    if (AppData.alarmOn)
    {
        /*
        ** If the alarm ringing counter is set, ring the bell and
        ** decrement the counter
        */
        if (ringingAlarmBell) {
	    for (i = 0; i < 4; i++)
                XBell (XtDisplay(AppData.widget), ALARM_LEVEL);
            ringingAlarmBell --;

        /*
        ** Otherwise, if the hour hand has moved into the alarm hand
        ** interval, start the alarm ringing on the next one second update
        */
        } else if (checkAlarm ()) ringingAlarmBell = ALARM_REPEATS;
    }

    /*
    ** Reset the timer for next one second update
    */
    (void) XtAppAddTimeOut (AppData.appContext,
			    (AppData.milliSecs > 900) ?
			         1000 : 1000 - AppData.milliSecs,
			    clockTimeout, (XtPointer) NULL);

} /* end clockTimeout () */

/***************************************************************
**
** FUNCTION:    mouseDown
**
** DESCRIPTION: This function acts as the event handler for the
**              ButtonDown event.  If the alarm is ringing it is
**              turned off.  The mouse point is checked to see
**              if it hits the alarm hand and if so the alarm
**              hand is moved appropriately.
**
** PARAMETERS:  w       	window widget
**              clientData	client data
**              event   	event information
**		goOn		whether to continue processing	
**
** RETURN:      None
**
***************************************************************/

/* ARGSUSED */

static void mouseDown (w, clientData, event, goOn)
    Widget  w;
    XtPointer clientData;
    XEvent  *event;
    Boolean *goOn;
{
    XButtonEvent *bp;
 
    ringingAlarmBell = 0;

    bp = &(event->xbutton);

    if (isHit (bp->x, bp->y)) setAlarm ();

}   /* end mouseDown() */

/***************************************************************
**
** FUNCTION:    mapHandler
**
** DESCRIPTION: Call back routine for map notify and unmap
**              notify events.  A unmap notify indicates the
**              window has been iconified at which time clock
**              drawing is turned off.  The map notify indicates
**              the window has been made visible and clock
**              drawing is turned back on.  This results in
**              a processing time savings since the clock is
**              normally updated once a second.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              event  	    event information
**		goOn	    whether to continue processing	
**
** RETURN:      None
**
***************************************************************/

/* ARGSUSED */

static void mapHandler (w, clientData, event, goOn)
    Widget w;
    XtPointer clientData;
    XEvent *event;
    Boolean *goOn;
{
    switch (event -> type)
    {
        case MapNotify:
            AppData.mapped = TRUE;
            clockTimeout (NULL, 0);
            break;

        case UnmapNotify:
            AppData.mapped = FALSE;
            break;
    }
} /* end mapHandler () */

/***************************************************************
**
** FUNCTION:    dismissTimingBox
**
** DESCRIPTION: callback routine to dismiss the timing box.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      N/A
**
***************************************************************/

/* ARGSUSED */

static void dismissTimingBox (w, clientData, callData)
     Widget w;
     XtPointer clientData, callData;
{
    XtUnmanageChild (AppData.timingBox);
    XmToggleButtonGadgetSetState(AppData.timingButton, False, False);
}

