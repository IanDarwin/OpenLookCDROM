/*
 * $RCSfile: HelloMain.c,v $
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

#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Mrm/MrmAppl.h>        /* Motif Toolkit and MRM */

#include <DPS/dpsXclient.h>
#include <DPS/dpsXshare.h>
#include <DPS/psops.h>

#include "HelloWraps.h"

/***************************************************************
**
** CONSTANT DEFINITIONS
**
***************************************************************/

/*
** These numbers are matched with corresponding numbers in Hello.uil
*/

#define cMainDrawArea       1
#define cTraceToggle        2
#define cWriteTextToggle    3

/***************************************************************
**
** TYPE DECLARATIONS
**
***************************************************************/

typedef struct
{
    Widget      widget;         /* drawing area widget ID     */
    DPSContext  dpsCtxt;        /* drawing DPS context        */
    String      message;        /* message to display */
    Boolean	trace;		/* Send PostScript trace to stdout */
    Boolean	writeText;	/* Toggle state to write the text */
} AppDataType, *AppDataTypePtr;

/***************************************************************
**
** FUNCTION DECLARATIONS
**
***************************************************************/

void
	createProc(),
	resizeWindow(),
	refreshWindow(),
	quitApp(),
	traceProc(),
	writeProc();

void
	initDPSContext();

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/

/*
** Global data block
*/
AppDataType AppData;

/*
** Global resource management data
*/
static MrmHierarchy SMrmHierarchy;	/* MRM database hierarchy ID */
static MrmType DummyClass;
static char *DbFilenameVec [] =		/* Mrm.hierarchy file list. */
{
    "Hello.uid"
};

/* Names and addresses for Mrm to bind */
static MrmRegisterArg RegList [] =
{
    {"refreshWindow"   , (caddr_t) refreshWindow   },
    {"resizeWindow"    , (caddr_t) resizeWindow   },
    {"createProc"      , (caddr_t) createProc      },
    {"quitApp"         , (caddr_t) quitApp         },
    {"traceProc"       , (caddr_t) traceProc       },
    {"writeProc"       , (caddr_t) writeProc       }
};

static XrmOptionDescRec CommandLineOptions[] = {
	{
		"-trace",		/* turn PostScript tracing on */
		".trace",		/* resource specifier for tracing */
		XrmoptionNoArg,
		(XtPointer) "true"
	},
	{
		"-message",		/* message to display */
		".message",		/* message resource specifier */
		XrmoptionSepArg,	/* -message <string> */
		(XtPointer) NULL	/* message to display */
	},
	{
		"-write",		/* write text option */
		".writeText",		/* resource specifier for write text */
		XrmoptionNoArg,
		(XtPointer) "false"
	}
};

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
	{
		"message",
		"Message",
		XtRString,
		sizeof(String),
		XtOffset(AppDataTypePtr,message),
		XtRString,
		(XtPointer)"Resource File Not Found"
	},
	{
		"writeText",
		"WriteText",
		XtRBoolean,
		sizeof(Boolean),
		XtOffset(AppDataTypePtr,writeText),
		XtRImmediate,
		(XtPointer)True
	}
};

static String FallbackResources[] = {
	"Hello.mMainForm.height: 400",
	"Hello.mMainForm.width: 900",
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
    char **argv;
{
    XtAppContext appContext;
    Widget appShellWidget;
    Widget mainWindowWidget;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    /*
    ** Initialize the X Toolkit. Get back an application level shell widget.
    */
    appShellWidget = XtAppInitialize (&appContext, "Hello",
        (XrmOptionDescRec *)CommandLineOptions, XtNumber(CommandLineOptions),
        &argc, argv, (String *)FallbackResources, (ArgList)NULL, 0);

    XtGetApplicationResources (
        appShellWidget, (XtPointer)&AppData, Resources, XtNumber(Resources),
        (ArgList)NULL, 0);

    /*
    ** Verify that the DPS extension is present in the X server.
    */
    if (!XDPSExtensionPresent (XtDisplay(appShellWidget)))
    {
        fprintf (stderr, "%s:  DPS extension not in server\n", argv[0]);
        exit (1);
    }

    /*
    ** Open the UID files (the output of the UIL compiler)
    */
    if (MrmOpenHierarchy (XtNumber(DbFilenameVec), DbFilenameVec,
        (MrmOsOpenParamPtr *) NULL, &SMrmHierarchy)  !=  MrmSUCCESS)
    {
        fprintf (stderr, "Can't open hierarchy\n");
        exit (1);
    }

    /*
    ** Register the items MRM needs to bind for us.
    */
    MrmRegisterNames (RegList, XtNumber(RegList));

    /*
    ** Get the main window for the application.
    */
    if (MrmFetchWidget (SMrmHierarchy, "MainWindow", appShellWidget,
        &mainWindowWidget, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf (stderr, "Can't fetch main window\n");
        exit (1);
    }

    /*
    ** Manage and realize the main window.
    ** The interface comes up on the display now.
    */
    XtManageChild (mainWindowWidget);
    XtRealizeWidget (appShellWidget);

    /*
    ** Do all the post-realization DPSX processing here
    */
    initDPSContext (appShellWidget);

    /*
    ** Sit around forever waiting to process X and DPS events.
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

void refreshWindow (w, clientData, callData)
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
    ** Redraw if text is being shown
    */
    PSerasepage ();
    if (AppData.writeText) PSWDisplayText (AppData.message);
} /* end refreshWindow () */

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

void resizeWindow (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    if (!XtIsRealized(w)) return;

    PSinitclip();
    PSinitviewclip();
} /* end resizeWindow () */



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

void createProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int widgetNum = *(int *) clientData;

    switch (widgetNum)
    {
        case cMainDrawArea:
            /*
            ** Save widget ID in application data structure
            */
            AppData.widget = w;
            break;

        case cTraceToggle:
            /*
            ** If trace turned on by command line, set button
            */
            if (AppData.trace) XtVaSetValues (w, XmNset, True, NULL);
            break;

	case cWriteTextToggle:
            /*
            ** If write text turned on by resource
            */
            if (AppData.writeText) XtVaSetValues (w, XmNset, True, NULL);
            break;

    } /* end switch */

} /* end createProc () */

/***************************************************************
**
** FUNCTION:    quitApp
**
** DESCRIPTION: Callback routine for "quit" command menu
**              selection.  The space allocated in the DPS
**              server is freed.  The X display is closed.
**              Exits from the application.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      Returns to OS.
**
***************************************************************/

/* ARGSUSED */

void quitApp (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    if (AppData.dpsCtxt != NULL) XDPSDestroySharedContext (AppData.dpsCtxt);

    /*
    ** Close the X display and free all X resources.
    */
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));

    /*
    ** Exit the application.
    */
    exit (0);

} /* end quitApp () */

/***************************************************************
**
** FUNCTION:    traceProc
**
** DESCRIPTION: Callback routine for the trace toggle button.
**
** PARAMETERS:  w		callback widget ID
**              clientData	callback client data
**              callData	callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

void traceProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
        (XmToggleButtonCallbackStruct *)callData;
    /*
    ** Change the state of the toggle flag
    */
    AppData.trace = toggle->set;

    /*
    ** Chain text context for trace option if trace is on
    */
    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

} /* end traceProc () */

/***************************************************************
**
** FUNCTION:    writeProc
**
** DESCRIPTION: Callback routine for the write push button.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

void writeProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
        (XmToggleButtonCallbackStruct *)callData;

    AppData.writeText = toggle->set;
    /*
    ** Draw the text to the draw area window
    */
    PSerasepage ();
    if (AppData.writeText) PSWDisplayText (AppData.message);
} /* end writeProc () */

/***************************************************************
**
** FUNCTION:    initDPSContext
**
** DESCRIPTION: Post-Realization initialization of DPS context and such.
**
** PARAMETERS:  None.
**
** RETURN:      None.
**
***************************************************************/

void initDPSContext (shell)
    Widget shell;
{
    XSetWindowAttributes attr;
    Dimension   height, width;

    /*
    ** Create the shared DPS context in which rendering will occur
    */
    AppData.dpsCtxt = XDPSGetSharedContext (XtDisplay(shell));
    if (AppData.dpsCtxt == NULL)
    {
        printf ("Couldn't create a Display PostScript context.\n");
        exit (1);
    }

    /*
    ** Set the default DPS context
    */
    DPSSetContext (AppData.dpsCtxt);
    (void) XDPSSetEventDelivery(XtDisplay(shell), dps_event_pass_through);

    XtVaGetValues (
        AppData.widget,
        XmNwidth, &width,
        XmNheight, &height,
        NULL
    );

    if (XDPSSetContextDrawable(AppData.dpsCtxt, XtWindow(AppData.widget),
			       height) != dps_status_success)
    {
        printf ("Couldn't set Display PostScript context drawable.\n");
        exit (1);
    }

    /*
    ** Change DPS origin to center of window and keep it there
    */
    PSsetXoffset(width/2, height/2);

    attr.bit_gravity = CenterGravity;
    XChangeWindowAttributes(XtDisplay(shell), XtWindow(AppData.widget),
        CWBitGravity, &attr);

    PSinitclip();
    PSinitviewclip();

    /*
    ** Create text context for trace option and chain it if trace is on
    */
    XDPSChainTextContext (AppData.dpsCtxt, AppData.trace);

} /* end initDPSContext () */
