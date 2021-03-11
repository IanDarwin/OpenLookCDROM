/*
 * $RCSfile: ScrollMain.c,v $
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

#include "Scroll.h"
#include "zoom.xbm"
#include "zoom_mask.xbm"
#include <X11/cursorfont.h>
#include <Xm/FileSB.h>
#include <Xm/DialogS.h>
#include <DPS/DPSScrollW.h>

/*
** Global pointers to the application data block
*/

AppDataType     AppData;

/*
** Global resource management data
*/
static MrmHierarchy SMrmHierarchy;	/* MRM database hierarchy ID */
static MrmType      DummyClass; 	/* and class variable. */
static char         *DbFilenameVec[] =	/* Mrm.heirarchy file list. */
{
    "Scrolling2.uid"
};

static void initApplication(), mouseDown(), traceProc(), quitProc(),
	exposeProc(), redrawProc(), createProc(), selectProc(),
	optionProc(), zoomProc(), forceRedraw(), backgroundProc(),
	drawStrategyProc(), clippingProc(), gstateProc(), strokeProc(),
	appendProc(), appendProc(), optionProc(), abortProc();

static MrmRegisterArg RegList [] =
{
	{"traceProc"		, (caddr_t) traceProc	},
	{"exposeProc"		, (caddr_t) exposeProc	},
	{"redrawProc"		, (caddr_t) redrawProc	},
	{"optionProc"		, (caddr_t) optionProc	},
	{"zoomProc"		, (caddr_t) zoomProc	},
	{"drawStrategyProc"	, (caddr_t) drawStrategyProc	},
	{"clippingProc"		, (caddr_t) clippingProc	},
	{"gstateProc"		, (caddr_t) gstateProc	},
	{"strokeProc"		, (caddr_t) strokeProc	},
	{"appendProc"		, (caddr_t) appendProc	},
	{"redrawProc"		, (caddr_t) redrawProc	},
	{"selectProc"		, (caddr_t) selectProc	},
	{"quitProc"		, (caddr_t) quitProc	},
	{"createProc"		, (caddr_t) createProc	},
	{"abortProc"		, (caddr_t) abortProc	},
};

static XrmOptionDescRec CommandLineOptions[] = {
  { "-trace", ".trace", XrmoptionNoArg, (caddr_t)"True",},
};

static XtResource Resources[] = {
  { "trace", "Trace", XtRBoolean, sizeof (Boolean),
    XtOffset (AppDataTypePtr, trace), XtRImmediate, (caddr_t) False
  },
  { "noInputFileMessage", "NoInputFileMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, noInputFileMessage),
    XtRString, (caddr_t) "Could not open file for input"
  },
  { "noOutputFileMessage", "NoOutputFileMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, noOutputFileMessage),
    XtRString, (caddr_t) "Could not open file for output"
  },
  { "noDistillFileMessage", "NoDistillFileMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, noDistillFileMessage),
    XtRString, (caddr_t) "Could not distillery file for input"
  },
  { "noMemoryMessage", "NoMemoryMessage", XmRXmString, sizeof (XmString),
    XtOffset (AppDataTypePtr, noMemoryMessage), XtRString,
    (caddr_t) "Not enough memory to parse or distill file"
  },
  { "badReadMessage", "BadReadMessage", XmRXmString, sizeof (XmString),
    XtOffset (AppDataTypePtr, badReadMessage), XtRString,
    (caddr_t) "Read error trying to parse file"
  },
  { "badWriteMessage", "BadWriteMessage", XmRXmString, sizeof (XmString),
    XtOffset (AppDataTypePtr, badWriteMessage), XtRString,
    (caddr_t) "Write error trying to distill file"
  },
  { "badFileMessage", "BadFileMessage", XmRXmString, sizeof (XmString),
    XtOffset (AppDataTypePtr, badFileMessage), XtRString,
    (caddr_t) "File is not a PostScript language file, or\n\
it was not created by the distillery"
  },
  { "parserErrorMessage", "ParserErrorMessage", XmRXmString, sizeof (XmString),
    XtOffset (AppDataTypePtr, parserErrorMessage), XtRString,
    (caddr_t) "Parser error:\n"
  },
  { "noDistillContextMessage", "noDistillContextMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, noDistillContextMessage),
    XtRString, (caddr_t) "Could not create context for distillery"
  },
  { "distillErrorMessage", "DistillErrorMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, distillErrorMessage),
    XtRString, (caddr_t) "Error prevented file distillation"
  },
  { "distillCompleteMessage", "DistillCompleteMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, distillCompleteMessage),
    XtRString, (caddr_t) "File successfully distilled.\n\
Distilled file has extension .dst"
  }
};

static Cursor zoomCursor;

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

void main(argc, argv)
    int argc;
    char *argv [];
{
    XtAppContext appContext;
    Widget toplevel, popup;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    /*
    ** Initialize the X Toolkit. We get back a top level shell widget.
    */
    toplevel = XtAppInitialize (
	&appContext, "Scrolling2", CommandLineOptions,
	XtNumber(CommandLineOptions), &argc, argv,
	(String *) NULL, (ArgList) NULL, 0);
    
    XtGetApplicationResources (
	toplevel, (XtPointer) &AppData, Resources,
	XtNumber(Resources), (ArgList) NULL, 0
	);

    if (!XDPSExtensionPresent(XtDisplay(toplevel)))
    {
        fprintf (stderr, "%s:  DPS extension not in server\n", argv[0]);
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
    if (MrmFetchWidget (SMrmHierarchy, "ControlDialog", toplevel,
        &AppData.mainWindow, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf (stderr, "Can't fetch main window\n");
        exit (1);
    }

    /*
    ** Create the DPSScrollingWidget.  Wouldn't it be nice if UIL let
    ** you use your own widget types?
    */
    popup = XmCreateDialogShell(toplevel, "scrollShell", (ArgList) NULL, 0);
    AppData.scroller = XtVaCreateWidget("scrollWindow",
					dpsScrolledWindowWidgetClass,
					popup, NULL);
    XtAddCallback(AppData.scroller, XtNexposeCallback, exposeProc, NULL);
    XtAddCallback(AppData.scroller, XtNbackgroundCallback,
		  backgroundProc, NULL);

    AppData.drawingArea = XtNameToWidget(AppData.scroller, "*drawingArea");
    XtAddCallback(AppData.drawingArea, XmNinputCallback, mouseDown, NULL);

    /*
    ** Important to do this here!  The widget has created the context,
    ** but since it's unrealized none of our procedures have been called yet
    */
    initDPSContext ();
    initApplication();

    /*
    ** Manage the option box and realize everything.
    ** The interface comes up on the display now.
    */
    XtManageChild(AppData.mainWindow);
    XtVaSetValues(
	toplevel,
    	XmNmwmFunctions,
        MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE,
        XmNmwmDecorations,
        MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_RESIZEH,
        NULL
    );
    XtRealizeWidget(toplevel);

    XtManageChild(AppData.scroller);

    /*
    ** Sit around forever waiting to process X-events.
    ** From here on, we only execute our callback routines.
    */
    while (1) {
	XEvent event;
	XtAppNextEvent(appContext, &event);
	if (!XDPSDispatchEvent(&event)) (void) XtDispatchEvent(&event);
    }
} /* end main () */


/***************************************************************
**
**			PRIVATE FUNCTIONS
**
***************************************************************/

/***************************************************************
**
** FUNCTION:    initApplication
**
** DESCRIPTION: One-time initialization of GlobalData struct
**
** PARAMETERS:	None.
**
** RETURN:      None.
**
***************************************************************/

static void initApplication()
{
    Display *dpy = XtDisplay(AppData.time);
    Window win = RootWindowOfScreen(XtScreen(AppData.time));
    Pixmap cursor, mask;
    XColor fore, back;

    /*
    ** Initialize the non-zero application control values
    */
    AppData.scale = 1.0;			
    AppData.openDrawing = False;
    AppData.resetTime = True;
    AppData.usePixmap = True;
    AppData.bigPixmap = False;
    AppData.watchProgress = False;
    AppData.minimalDrawing = False;
    AppData.incrementalDrawing = False;
    AppData.drawStrategy = draw_userpaths;
    AppData.clientClipping = True;
    AppData.optimizeChanges = True;
    AppData.wireFrame = False;
    AppData.consolidate = True;

    /*
    ** Create a zoom cursor
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

    /*
    ** Create wait cursor
    */
    AppData.waitCursor = XCreateFontCursor(dpy, XC_watch);
} /* end initApplication() */

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

static void createProc (w, clientData, callData)
    Widget	w;
    XtPointer 	clientData, callData;
{
    int 	widgetNum = *((int *) clientData);

    /*
    ** Save widget ID in application data structure; sometimes do more...
    */
    switch (widgetNum) {
	case cTimingText:	AppData.time = w;		break;
	case cTotalTimingText:	AppData.totalTime = w;		break;
        case cIncrementSelector:AppData.incrementText = w;	break;

        case cTraceToggle:
            if (AppData.trace) XtVaSetValues(w, XmNset, True, NULL);
	    break;
        case cBackgroundToggle:
	    AppData.usePixmap = XmToggleButtonGetState(w);
	    break;
        case cBigPixmapToggle:
	    AppData.bigPixmap = XmToggleButtonGetState(w);
	    break;
        case cWatchProgressToggle:
	    AppData.watchProgress = XmToggleButtonGetState(w);
	    break;
        case cMinimalDrawingToggle:
	    AppData.minimalDrawing = XmToggleButtonGetState(w);
	    break;
        case cIncrementalDrawingToggle:
	    AppData.incrementalDrawing = XmToggleButtonGetState(w);
	    break;
        case cAbortDrawingToggle:
	    AppData.abortPartialDrawing = XmToggleButtonGetState(w);
	    break;
    }
} /* end createProc () */

/***************************************************************
**
** FUNCTION:    traceProc
**
** DESCRIPTION: Callback routine for TraceToggle manipulation.
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
    Widget 	w;
    XtPointer 	clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    XDPSChainTextContext (AppData.dpsCtxt, toggle->set);
} /* end traceProc () */

/***************************************************************
**
** FUNCTION:    exposeProc
**
** DESCRIPTION: Callback routine to handle expose events on the
**              main graphics display.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void exposeProc (w, clientData, callData)
    Widget 	w;
    XtPointer 	clientData, callData;
{
    DSWExposeCallbackRec *cb = (DSWExposeCallbackRec *) callData;

    if (cb->directions == DSWAbort ||
	(cb->directions == DSWAbortOrFinish && AppData.abortPartialDrawing)) {
	cb->results = DSWAborted;
	abortDrawing();
	return;
    }

    if (drawSelf(cb->rects, cb->rect_count, cb->first,
		 cb->directions == DSWDrawSome && AppData.incrementalDrawing)){
	cb->results = DSWFinished;
    } else cb->results = DSWCallAgain;
} /* end of exposeProc () */

/***************************************************************
**
** FUNCTION:    backgroundProc
**
** DESCRIPTION: Callback routine to handle expose events on the
**              background of the main graphics display.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void backgroundProc (w, clientData, callData)
    Widget 	w;
    XtPointer 	clientData, callData;
{
    DSWExposeCallbackRec *cb = (DSWExposeCallbackRec *) callData;

    PSWDrawFrame(0, 0, (int) PAGE_WIDTH, (int) PAGE_HEIGHT);
    cb->results = DSWFinished;
} /* end of backgroundProc () */

/***************************************************************
**
** FUNCTION:    cancelFileCallback
**
** DESCRIPTION: Unmanages file dialog when cancelled
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void cancelFileCallback(w, clientData, callData)
    Widget 	w;
    XtPointer	clientData, callData;
{
    XtUnmanageChild(AppData.fileDialog);
}

/***************************************************************
**
** FUNCTION:    makeNormalString
**
** DESCRIPTION: Extracts a normal string from a compound string.
**		Caller should free string with XtFree
**
** PARAMETERS:	xmstring	compound string
**
** RETURN:      The normal string
**
***************************************************************/

static char *makeNormalString(xmstring)
    XmString xmstring;
{
    String answer;

    answer = (String) XtMalloc(XmStringLength (xmstring) + 1);
    XmStringGetLtoR(xmstring, XmSTRING_DEFAULT_CHARSET, &answer);
    return answer;
}

/***************************************************************
**
** FUNCTION:    setWaitCursor
**
** DESCRIPTION: Puts up the wait cursor in the main window and the scroller
**
** PARAMETERS:	None.
**
** RETURN:      None.
**
***************************************************************/

void setWaitCursor()
{
    if (AppData.waitCursor != None) {
	XDefineCursor(XtDisplay(AppData.scroller),
		      XtWindow(AppData.scroller), AppData.waitCursor);
	XDefineCursor(XtDisplay(AppData.mainWindow),
		      XtWindow(AppData.mainWindow), AppData.waitCursor);
	/*
	** Flush connection to make cursor change happen
	*/
	XFlush(XtDisplay(AppData.mainWindow));
    }
}

/***************************************************************
**
** FUNCTION:    clearWaitCursor
**
** DESCRIPTION: Takes down the wait cursor
**
** PARAMETERS:	None.
**
** RETURN:      None.
**
***************************************************************/

void clearWaitCursor()
{
    if (AppData.waitCursor != None) {
	XUndefineCursor(XtDisplay(AppData.scroller),
			XtWindow(AppData.scroller));
	XUndefineCursor(XtDisplay(AppData.mainWindow),
			XtWindow(AppData.mainWindow));
	/*
	** Flush connection to make cursor change happen
	*/
	XFlush(XtDisplay(AppData.mainWindow));
    }
}

/***************************************************************
**
** FUNCTION:    openFileCallback
**
** DESCRIPTION: Callback procedure to open a file for input or
**		distillation
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void openFileCallback(w, clientData, callData)
    Widget 	w;
    XtPointer	clientData, callData;
{
    XmFileSelectionBoxCallbackStruct *fdata =
	    (XmFileSelectionBoxCallbackStruct *) callData;
    FILE 	*f;
    String 	str = makeNormalString(fdata->value);
    Action 	*action = (Action *) clientData;
    Boolean 	success;

    /*
    ** Open the selected filename for read, then unmanage the file dialog
    */
    f = fopen(str, "r");

    if (f == NULL) {
	putUpInfoDialog(AppData.noInputFileMessage);
	XtFree(str);
	return;
    }
    XtUnmanageChild(AppData.fileDialog);

    /* If loading a file, try to parse, and if successful redraw. */
    if (*action == action_load) {
	setWaitCursor();
	success = parseFile(f);
	clearWaitCursor();

	XtFree(str);
	fclose(f);
	if (success) {
	    AppData.openDrawing = True;
	    forceRedraw();
	}

    } else {			/* distilling the file */
	setWaitCursor();
	distillFile(f, str);
	clearWaitCursor();
	XtFree(str);
	fclose(f);
    }
}

/***************************************************************
**
** FUNCTION:	selectProc
**
** DESCRIPTION:	Callback for the Open and Distill buttons; creates
**		and manages the file selection dialog
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:	None.
**
***************************************************************/

/* ARGSUSED */

static void selectProc (w, clientData, callData)
    Widget 	w;
    XtPointer	clientData, callData;
{
    static XmString 	eps, dst;
    static Action 	action;

    action =  *((Action *)clientData);

    /*
    ** If there is no file dialog, create one and unmanage the help button
    */
    if (AppData.fileDialog == NULL) {
	AppData.fileDialog =
		XmCreateFileSelectionDialog(AppData.time, "fileDialog",
					    (ArgList) NULL, 0);
	w = XmFileSelectionBoxGetChild(AppData.fileDialog,
				       XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(w);
	XtAddCallback(AppData.fileDialog, XmNcancelCallback,
		      cancelFileCallback, (XtPointer) NULL);
	XtAddCallback(AppData.fileDialog, XmNokCallback,
		      openFileCallback, (XtPointer) &action);

	/*
	** Create compound strings for the extensions
	*/
	eps = XmStringCreate("*.eps", XmSTRING_DEFAULT_CHARSET);
	dst = XmStringCreate("*.dst", XmSTRING_DEFAULT_CHARSET);
    }

    /*
    ** Set the appropriate file extension and manage the file dialog.
    ** The file dialog callback will do the real work.
    */
    if (action == action_load) {
	XtVaSetValues(AppData.fileDialog, XmNpattern, dst, NULL);
    } else { 				/* action_distill */
	XtVaSetValues(AppData.fileDialog, XmNpattern, eps, NULL);
    }
    XtManageChild(AppData.fileDialog);
} /* end of selectProc () */

/***************************************************************
**
** FUNCTION:    quitProc
**
** DESCRIPTION: Callback routine for "quit" command menu
**              selection.  Exits from the application.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None; terminates process.
**
***************************************************************/

/* ARGSUSED */

static void quitProc (w, clientData, callData)
    Widget	w;
    XtPointer	clientData, callData;
{
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
    exit(0);
} /* end quitProc () */

/***************************************************************
**
** FUNCTION:    optionProc
**
** DESCRIPTION: Callback routine for scrolling control toggles
**              selection.  Sets resources on the scroller widget
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None
**
***************************************************************/

/* ARGSUSED */

static void optionProc (w, clientData, callData)
    Widget 		w;
    XtPointer 	clientData;
    XtPointer	callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    char *option = NULL;
    
    switch (val) {
	case cBackgroundToggle:
	    option = XtNuseBackingPixmap;
	    break;
        case cBigPixmapToggle:		
	    option = XtNdocumentSizePixmaps;
	    break;
	case cWatchProgressToggle:
	    option = XtNwatchProgress;
	    break;
	case cMinimalDrawingToggle:
	    option = XtNminimalDrawing;
	    break;
	case cIncrementalDrawingToggle:
	    AppData.incrementalDrawing = toggle->set;
	    break;
	default:
	    return;
    }

    if (option != NULL) {
	XtVaSetValues(AppData.scroller, option, toggle->set, NULL);
    }

    if (AppData.openDrawing) forceRedraw();
}

/***************************************************************
**
** FUNCTION:    abortProc
**
** DESCRIPTION: Callback routine for "abort partial drawing" toggle
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None
**
***************************************************************/

/* ARGSUSED */

static void abortProc (w, clientData, callData)
    Widget 		w;
    XtPointer 	clientData;
    XtPointer	callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    
    AppData.abortPartialDrawing = toggle->set;

    if (AppData.openDrawing) forceRedraw();
}

/***************************************************************
**
** FUNCTION:    zoomProc
**
** DESCRIPTION: Callback for zoom toggle buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void zoomProc (w, clientData, callData)
    Widget 		w;
    XtPointer 	clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the selected radio button if toggle state is set
    */
    if (toggle->set && AppData.scale != (float) num / 100.0) {
	if (AppData.openDrawing) {
	    /*
	    ** Set zooming flag and cursor.  Actual work takes place
	    ** in mouse event handler when the user clicks
	    */
	    AppData.newScale = (float) num / 100.0;
	    AppData.zooming = True;
	    if (zoomCursor != None) {
		XDefineCursor(XtDisplay(AppData.drawingArea),
			      XtWindow(AppData.drawingArea), zoomCursor);
	    }
	} else {
	    Dimension width, height;

	    XtVaGetValues(AppData.drawingArea, XmNwidth, &width,
			  XmNheight, &height, NULL);
	    AppData.scale = (float) num / 100.0;
	    DSWSetScale(AppData.scroller, AppData.scale, width/2, height/2);
	}
    }
} /* end of zoomProc () */

/***************************************************************
**
** FUNCTION:    forceRedraw
**
** DESCRIPTION: Forces a redraw of the picture
**
** PARAMETERS:  None
**
** RETURN:      None.
**
***************************************************************/

static void forceRedraw()
{
    float area[4];

    AppData.resetTime = True;

    area[0] = area[1] = 0;
    area[2] = area[3] = -1;

    DSWAddToDirtyArea(AppData.scroller, area, 1);
}    

/***************************************************************
**
** FUNCTION:    drawStrategyProc
**
** DESCRIPTION: Callback for draw strategy toggle buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void drawStrategyProc (w, clientData, callData)
    Widget	w;
    XtPointer 	clientData, callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Ignore callbacks from button unsets
    */
    if (!toggle->set || val == (int) AppData.drawStrategy) return;

    /*
    ** Update the global flag
    */
    AppData.drawStrategy = val;

    if (AppData.openDrawing) forceRedraw();
} /* end drawStrategyProc () */

/***************************************************************
**
** FUNCTION:    clippingProc
**
** DESCRIPTION: Callback for bounds toggle buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void clippingProc (w, clientData, callData)
    Widget	w;
    XtPointer 	clientData, callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Ignore callbacks from button unsets
    */
    if (!toggle->set || val == AppData.clientClipping) return;

    /*
    ** Update the global flag
    */
    AppData.clientClipping = val;

    if (AppData.openDrawing) forceRedraw();
} /* end of clippingProc () */

/***************************************************************
**
** FUNCTION:    gstateProc
**
** DESCRIPTION: Callback for gstate toggle buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void gstateProc (w, clientData, callData)
    Widget 	w;
    XtPointer 	clientData, callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Ignore callbacks from button unsets
    */
    if (!toggle->set || val == AppData.optimizeChanges) return;

    /*
    ** Update the global flag
    */
    AppData.optimizeChanges = val;

    if (AppData.openDrawing) forceRedraw();
} /* end gstateProc () */

/***************************************************************
**
** FUNCTION:    strokeProc
**
** DESCRIPTION: Callback for stroke toggle buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void strokeProc (w, clientData, callData)
    Widget 	w;
    XtPointer 	clientData, callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Ignore callbacks from button unsets
    */
    if (!toggle->set || val == AppData.wireFrame) return;

    /*
    ** Update the global flag
    */
    AppData.wireFrame = val;

    if (AppData.openDrawing) forceRedraw();
} /* end strokeProc () */


/***************************************************************
**
** FUNCTION:    appendProc
**
** DESCRIPTION: Callback for append toggle buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void appendProc (w, clientData, callData)
    Widget	w;
    XtPointer 	clientData, callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Ignore callbacks from button unsets
    */
    if (!toggle->set || val == AppData.consolidate) return;

    /*
    ** Update the global flag
    */
    AppData.consolidate = val;

    if (AppData.openDrawing) forceRedraw();
} /* end appendProc () */

/***************************************************************
**
** FUNCTION:	mouseDown
**
** DESCRIPTION:	This function acts as the event handler for the
**		ButtonDown event.  If the magnification is being
**		changed (zooming), then scale and position the 
**		drawing view.
**
** PARAMETERS:	w		window widget
**		clientData	client data
**		callData	Motif call data
**
** RETURN:	None
**
***************************************************************/

/* ARGSUSED */

static void mouseDown(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmDrawingAreaCallbackStruct *dac =
            (XmDrawingAreaCallbackStruct *) callData;
    XButtonEvent *bp = (XButtonEvent *) dac->event;

    if (dac->event->xany.window != XtWindow(AppData.drawingArea)) return;

    /*
    ** If zooming, rescale so clicked-on point remains fixed
    */
    if (AppData.zooming) {
	DSWSetScale(AppData.scroller, AppData.newScale, bp->x, bp->y);

	AppData.scale = AppData.newScale;
	AppData.zooming = False;
	if (zoomCursor != None) {
	    XUndefineCursor(XtDisplay(w), XtWindow(w));
	}
    }
} /* end mouseDown() */

/***************************************************************
**
** FUNCTION:    redrawProc
**
** DESCRIPTION: Callback routine for redraw button pushed.
**              Redraws the patterns and displays the time.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void redrawProc (w, clientData, callData)
    Widget	w;
    XtPointer	clientData, callData;
{
    forceRedraw();
} /* end redrawProc () */

/***************************************************************
**
** FUNCTION:    putUpInfoDialog
**
** DESCRIPTION: Creates and manages the information dialog with
**		the specified compound string as the message
**
** PARAMETERS:	message		String to display
**
** RETURN:      None.
**
***************************************************************/

void putUpInfoDialog(message)
    XmString message;
{
    Widget w;
    static Widget infoDialog;

    /*
    ** If this is the first time, create the info dialog
    */
    if (infoDialog == NULL) {
	infoDialog =
		XmCreateInformationDialog(AppData.time, "infoDialog",
					  (ArgList) NULL, 0);
	XtVaSetValues(infoDialog,
		      XmNdialogStyle, XmDIALOG_APPLICATION_MODAL, NULL);
	w = XmMessageBoxGetChild(infoDialog, XmDIALOG_CANCEL_BUTTON);
	XtUnmanageChild(w);
	w = XmMessageBoxGetChild(infoDialog, XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(w);
    }

    /*
    ** Set the message and pop it up
    */
    XtVaSetValues(infoDialog, XmNmessageString, message, NULL);
    XtManageChild(infoDialog);
} /* end putUpInfoDialog() */

/***************************************************************
**
** FUNCTION:    showTime
**
** DESCRIPTION: Routine to set timing value in options box.
**
** PARAMETERS:	eTime		elapsed time
**		tTime		total time
**
** RETURN:      None.
**
***************************************************************/

void showTime(eTime, tTime)
    unsigned long eTime, tTime;
{
    char cTime [15];

    if (eTime  ==  0) strcpy (cTime, " ");
    else sprintf (cTime, "%d", eTime);

    XtVaSetValues (AppData.time,
		   XmNlabelString, XmStringCreateSimple(cTime), NULL);

    if (tTime  ==  0) strcpy (cTime, " ");
    else sprintf (cTime, "%d", tTime);

    XtVaSetValues (AppData.totalTime,
		   XmNlabelString, XmStringCreateSimple(cTime), NULL);
} /* end setTimingValue () */

