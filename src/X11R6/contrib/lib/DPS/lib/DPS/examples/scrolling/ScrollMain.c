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

/***************************************************************
**
** 		FUNCTION DECLARATIONS
**
***************************************************************/

/***************************************************************
**
**		DATA DECLARATIONS
**
***************************************************************/
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
    "Scrolling.uid"
};

static void initApplication(), mouseDown(), traceProc(), quitProc(),
	exposeProc(), selfResizeProc(), autoResizeProc(), redrawProc(),
	createProc(), selectProc(), scrollStrategyProc(), zoomProc(),
	drawStrategyProc(), clippingProc(), gstateProc(), strokeProc(),
	appendProc(), appendProc(), bufferProc();

static MrmRegisterArg RegList [] =
{
	{"traceProc"		, (caddr_t) traceProc	},
	{"scrollProc"		, (caddr_t) scrollProc	},
	{"exposeProc"		, (caddr_t) exposeProc	},
	{"resizeProc"		, (caddr_t) selfResizeProc	},
	{"redrawProc"		, (caddr_t) redrawProc	},
	{"scrollStrategyProc"	, (caddr_t) scrollStrategyProc	},
	{"bufferProc"		, (caddr_t) bufferProc  },
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
};

static XrmOptionDescRec CommandLineOptions[] = {
  { "-trace", ".trace", XrmoptionNoArg, (caddr_t)"True",},
};

static XtResource Resources[] = {
  { "trace", "Trace", XtRBoolean, sizeof (Boolean),
    XtOffset (AppDataTypePtr, trace), XtRImmediate, (caddr_t) False
  },
  { "noAutoPixmapMessage", "NoAutoPixmapMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, noAutoPixmapMessage),
    XtRString, (caddr_t) "Auto scrolling pixmap could not be allocated\n\
Switching to auto-scroll unbuffered."
  },
  { "noSelfPixmapMessage", "NoSelfPixmapMessage", XmRXmString,
    sizeof (XmString), XtOffset (AppDataTypePtr, noSelfPixmapMessage),
    XtRString, (caddr_t) "Self scrolling pixmap could not be allocated\n\
Switching to self-scroll unbuffered."
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
    (caddr_t) "Write error trying to distillfile"
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
  },
  { "pixmapMaxSize", "PixmapMaxSize", XtRInt, sizeof (int),
    XtOffset (AppDataTypePtr, pixmapMaxSize), XtRImmediate,
    (caddr_t) 2097152 /* Two megabytes */
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
    Widget toplevel;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    /*
    ** Initialize the X Toolkit. We get back a top level shell widget.
    */
    toplevel = XtAppInitialize (
	&appContext, "Scrolling", CommandLineOptions,
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
    ** Get the auto drawing widget
    */
    if (MrmFetchWidget(SMrmHierarchy, "AutoScroller", AppData.mainWindow,
	&AppData.autoScrolling, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf(stderr, "Can't fetch auto-scrolling window\n");
        exit(1);
    }

    /*
    ** Get the auto drawing widget
    */
    if (MrmFetchWidget(SMrmHierarchy, "SelfScroller", AppData.mainWindow,
	&AppData.selfScrolling, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf(stderr, "Can't fetch self-scrolling window\n");
        exit(1);
    }

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

    initApplication();

    initDPSContext ();

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
    Window win = XtWindow(AppData.time);
    XGCValues values;
    Pixmap cursor, mask;
    XColor fore, back;

    /*
    ** Initialize the non-zero application control values
    */
    AppData.magnify = 100.0;
    AppData.scale = 1.0;			
    AppData.scrollStrategy = scroll_background;
    AppData.showDrawing = False;
    AppData.drawStrategy = draw_userpaths;
    AppData.clientClipping = True;
    AppData.optimizeChanges = True;
    AppData.wireFrame = False;
    AppData.consolidate = True;

    /*
    ** Create a GC for copying
    */
    AppData.gc = XCreateGC(dpy, win, 0, &values);

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
	case cSelfDrawArea:
	    AppData.selfDrawingArea = w;

	    /*
	    ** Add event handlers to handle mouse press and GraphicsExpose
	    */
	    XtAddEventHandler(w, ButtonPressMask, False, mouseDown, NULL);
            XtAddRawEventHandler(w, 0, True, graphicsExpose, NULL);
            break;

	case cAutoDrawArea:
	    AppData.autoDrawingArea = w;

	    /*
	    ** Add event handler to handle mouse press to parent (clip window)
	    */
	    XtAddEventHandler(w, ButtonPressMask, False,
			      mouseDown, NULL);
	    /*
	    ** Add resize callback to parent (the clip window)
	    */
	    XtAddCallback(XtParent(w), XmNresizeCallback,
			  autoResizeProc, (XtPointer) NULL);
            break;

        case cTraceToggle:
            if (AppData.trace) XtVaSetValues(w, XmNset, True, NULL);
	    break;
	
	case cAutoScrollWin:
	    /*
	    ** Find out the horizontal and vertical scroll bar id's
	    */
	    XtVaGetValues(w,
		      XmNhorizontalScrollBar, &AppData.autoHScroll,
		      XmNverticalScrollBar, &AppData.autoVScroll, NULL);
	    break;

	case cSelfHsb:		AppData.selfHScroll = w;	break;
	case cSelfVsb:		AppData.selfVScroll = w;	break;
	case cTimingText:	AppData.time = w;		break;
	case cBackgroundToggle: AppData.currentStrategy = w;	break;
	case cAutoRedrawToggle: AppData.autoRedraw = w;		break;
	case cSelfRedrawToggle: AppData.selfRedraw = w;		break;
	case cWatchFrame:	AppData.watchFrame = w;		break;
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
** FUNCTION:    selfResizeProc
**
** DESCRIPTION: Callback routine to handle resize events on the
**		self-scrolling drawing window
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void selfResizeProc (w, clientData, callData)
    Widget 	w;
    XtPointer 	clientData, callData;
{
    XPoint xpoint;
    Point point;

    if (!XtIsRealized(w)) return;

    /*
    ** Convert upper left corner into PS units so we can keep it fixed
    */
    xpoint.x = 0;
    xpoint.y = 0;
    convertToDPS(&xpoint, &point);

    /*
    ** Get new size of drawing area
    */
    XtVaGetValues(w, XtNheight, &AppData.drawingHeight,
		  XtNwidth, &AppData.drawingWidth, NULL);

    /*
    ** Move the drawing area so the top left corner
    ** of the image remains located at the top left corner
    */
    setupAndDraw(False, point.x, point.y, xpoint.x, xpoint.y);
} /* end of selfResizeProc () */

/***************************************************************
**
** FUNCTION:    autoResizeProc
**
** DESCRIPTION: Callback routine to handle resize events on the
**		auto-scrolling clip window
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void autoResizeProc(w, clientData, callData)
    Widget 	w;
    XtPointer 	clientData, callData;
{
    XPoint xpoint;
    Point point;
    Position x, y;
    Boolean setup;

    if (!XtIsRealized(w)) return;

    /*
    ** Convert upper left corner into PS units so we can keep it fixed
    */
    XtVaGetValues(AppData.autoDrawingArea, XtNx, &x, XtNy, &y, NULL);
    xpoint.x = -x;
    xpoint.y = -y;
    convertToDPS(&xpoint, &point);

    /*
    ** We have to redraw if the old or new window size is larger than the
    ** scaled size; this means that we're showing some gray background
    ** and have to recenter
    */
    setup = (int) AppData.drawingHeight > AppData.scaledHeight ||
	    (int) AppData.drawingWidth > AppData.scaledWidth;

    /*
    ** Get new size of drawing area
    */
    XtVaGetValues(w, XtNheight, &AppData.drawingHeight,
		  XtNwidth, &AppData.drawingWidth, NULL);

    setup = setup || (int) AppData.drawingHeight > AppData.scaledHeight ||
	    (int) AppData.drawingWidth > AppData.scaledWidth;

    /*
    ** Make the window size match the scaled size
    */
    setWindowSize(AppData.scaledWidth, AppData.scaledHeight);

    /*
    ** Move the drawing area so the top left corner
    ** of the image remains located at the top left corner
    */
    if (setup) setupAndDraw(False, point.x, point.y, 0, 0);
} /* end of autoResizeProc () */

/***************************************************************
**
** FUNCTION:    addExposureToBBox
**
** DESCRIPTION: Adds the exposed rectangle from the expose event
**		to the bbox list
**
** PARAMETERS:	bbox	pointer to bbox data list
**		len	pointer to total number of entries in bbox list
**		count	pointer to current number of entries in bbox list
**		e	pointer to expose event
**
** RETURN:      None.  (bbox, count, and len are updated)
**
***************************************************************/

void addExposureToBBox(box, len, count, e)
    int **box;
    int *len, *count;
    XExposeEvent *e;
{
    /*
    ** Grow bounding box list if necessary
    */
    if (*count + 4 > *len) {
	*box = (int *) XtRealloc((XtPointer) *box, (*len + 12) * sizeof(int));
	*len += 12;
    }

    /*
    ** Store rectangle in list
    */
    (*box)[(*count)++] = e->x;
    (*box)[(*count)++] = e->y + e->height;
    (*box)[(*count)++] = e->width;
    (*box)[(*count)++] = e->height;
}

/***************************************************************
**
** FUNCTION:    checkExposeEvent
**
** DESCRIPTION: Utility procedure for XCheckEvent to see of there
**		are any more expose events for the window.  This always
**		returns False, since we don't actually want to remove the
**		event from the queue; the answer is returned in the "found"
**		parameter of CheckData.
**
** PARAMETERS:	dpy	Display pointer
**		e	Event pointer
**		arg	Pointer to CheckData structure
**
** RETURN:      Always False
**
***************************************************************/

typedef struct {
    Window w;
    Boolean found;
} CheckData;

/* ARGSUSED */

static Bool checkExposeEvent(dpy, e, arg)
    Display *dpy;
    XEvent *e;
    char *arg;
{
    CheckData *d = (CheckData *) arg;

    if (!d->found && e->type == Expose &&
	d->w == e->xany.window) d->found = True;
    return False;
}

/***************************************************************
**
** FUNCTION:    exposeProc
**
** DESCRIPTION: Callback routine to handle expose events on the
**              main graphics display.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void exposeProc (w, clientData, callData)
    Widget 	w;
    XtPointer 	clientData, callData;
{
    XmDrawingAreaCallbackStruct *callback =
	    (XmDrawingAreaCallbackStruct *) callData;
    XExposeEvent *e = &callback->event->xexpose;
    static int *bboxList = NULL;
    static int bboxLen = 0, bboxCount = 0;
    XEvent event;
    Display *dpy = XtDisplay(w);
    CheckData cd;

    /*
    ** If we are in the midst of scrolling, the exposed area must be offset
    ** by the most recent scroll
    */
    if (AppData.scrolling) {
	e->x -= AppData.lastXdelta;
	e->y -= AppData.lastYdelta;
    }

    switch (AppData.scrollStrategy) {
	/*
	** If using window background, nothing to do
	*/
	case scroll_background:
	    break;

	/*
	** If using a buffered window, copy the exposed area from the buffer
	*/
	case scroll_auto_buffer:
	case scroll_self_buffer:
	    XCopyArea(dpy, AppData.buf, XtWindow(w), AppData.gc,
		      e->x, e->y, e->width, e->height, e->x, e->y);
	    break;

	/*
	** If using a redraw window, add the exposure to the list of
	** bounding boxes.  If there are no more expose
	** events, set the view clip to the bounding box list and redraw
	*/
	case scroll_auto_redraw:
	case scroll_self_redraw:
	    addExposureToBBox(&bboxList, &bboxLen, &bboxCount, e);
	    if (e->count == 0) {
		/*
		** Check the event queue for more expose events; if there
		** are any don't redraw just yet
		*/
		if (XPending(dpy) > 0) {
		    cd.w = XtWindow(w);
		    cd.found = False;
		    (void) XCheckIfEvent(dpy, &event, checkExposeEvent,
					 (char *) &cd);
		    if (cd.found) return;
		}
		drawSelf(bboxList, bboxCount/4);
		bboxCount = 0;
	    }
	    break;
    }
} /* end of exposeProc () */

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
** DESCRIPTION: Puts up the wait cursor in the main window and, if mapped,
**		the current display window
**
** PARAMETERS:	None.
**
** RETURN:      None.
**
***************************************************************/

void setWaitCursor()
{
    if (AppData.waitCursor != None) {
	if (AppData.currentDraw != NULL) {
	    XDefineCursor(XtDisplay(AppData.currentDraw),
			  XtWindow(AppData.currentDraw), AppData.waitCursor);
	}
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
	if (AppData.currentDraw != NULL) {
	    XUndefineCursor(XtDisplay(AppData.currentDraw),
			    XtWindow(AppData.currentDraw));
	}
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

    /*
    ** If loading a file, try to parse, and if successful redraw.  If this
    ** is the first time, pass True for the center parameter so that the
    ** picture is centered.  If this isn't the first time, call
    ** setupAndDrawUnmoving so the current position is maintained.
    */
    if (*action == action_load) {
	setWaitCursor();
	success = parseFile(f);
	clearWaitCursor();

	XtFree(str);
	fclose(f);
	if (success) {
	    if (AppData.currentDraw == NULL) {
		setupAndDraw(True, 0.0, 0.0, 0, 0);
	    } else setupAndDrawUnmoving();
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
** FUNCTION:    scrollStrategyProc
**
** DESCRIPTION: Callback for scroll strategy buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

static void scrollStrategyProc (w, clientData, callData)
    Widget 		w;
    XtPointer 	clientData;
    XtPointer	callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Ignore callbacks from button unsets
    */
    if (!toggle->set || val == (int) AppData.scrollStrategy) return;

    /*
    ** If changing away from background scrolling, reset the window
    ** background to be white
    */
    if (AppData.scrollStrategy == scroll_background &&
	AppData.currentDraw != NULL) {
	XSetWindowBackground(XtDisplay(w), XtWindow(AppData.currentDraw),
			     WhitePixelOfScreen(XtScreen(w)));
    }

    /*
    ** If selecting a redraw method, make watch progress box insensitive
    */
    if (val & SCROLL_REDRAW) {
	XtSetSensitive(AppData.watchFrame, False);
    } else XtSetSensitive(AppData.watchFrame, True);

    /*
    ** Update the global data flag
    */
    AppData.scrollStrategy = val;
    AppData.currentStrategy = w;

    if (AppData.currentDraw != NULL) setupAndDrawUnmoving();
} /* end scrollStrategyProc () */

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
    if (toggle->set && AppData.magnify != num) {
	AppData.magnify = num;
	if (AppData.currentDraw == NULL) {
	    /*
	    ** No current drawing, but compute new scale and size
	    ** for when we decide to draw
	    */
	    AppData.scale = (float) num / 100.0;
	    AppData.scaledWidth = PAGE_WIDTH * AppData.origXScale *
		    AppData.scale;
	    AppData.scaledHeight = PAGE_HEIGHT * AppData.origYScale *
		    AppData.scale;
	} else {
	    /*
	    ** Set zooming flag and cursor.  Actual work takes place
	    ** in mouse event handler when the user clicks
	    */
	    AppData.zooming = True;
	    if (zoomCursor != None) {
		XDefineCursor(XtDisplay(AppData.currentDraw),
			      XtWindow(AppData.currentDraw), zoomCursor);
	    }
	}
    }
} /* end of zoomProc () */

/***************************************************************
**
** FUNCTION:    flushAndClear
**
** DESCRIPTION: Deletes all expose events for the window, then clears it
**
** PARAMETERS:  w	widget to flush and clear
**
** RETURN:      None.
**
***************************************************************/

void flushAndClear(w)
    Widget w;
{
    XEvent e;

    /*
    ** Make sure everything is here that's a'comin'
    */
    XSync(XtDisplay(w), False);

    /*
    ** Check for Expose events and remove them from the queue
    */
    while (XCheckTypedWindowEvent(XtDisplay(w), XtWindow(w), Expose, &e)) {}

    /*
    ** Clear the window, generating expose events for the visible areas
    */
    XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, True);    
}

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
    int area[4];
    Widget w = AppData.currentDraw;

    switch (AppData.scrollStrategy) {
	/*
	** Auto-scrolling buffered methods have the entire page as
	** a clipping area.  Redraw.
	*/
	case scroll_background:
	case scroll_auto_buffer:
	    area[0] = 0;
	    area[1] = AppData.pixmapHeight;
	    area[2] = AppData.pixmapWidth;
	    area[3] = AppData.pixmapHeight;
	    drawSelf(area, 1);
	    /*
	    ** If using background, install it.  Otherwise clear the window;
	    ** the expose event will cause a copy from the buffer
	    */
	    if (AppData.scrollStrategy == scroll_background) {
		XSetWindowBackgroundPixmap(XtDisplay(w), XtWindow(w),
					   AppData.buf);
		XClearWindow(XtDisplay(w), XtWindow(w));
	    } else if (!AppData.showDrawing) flushAndClear(w);
	    break;

	/*
	** Self-scrolling buffered requires finding the bounding box
	** of the actual buffer area
	*/
	case scroll_self_buffer:
	    area[0] = 0;
	    area[1] = AppData.drawingHeight;
	    area[2] = AppData.drawingWidth;
	    area[3] = AppData.drawingHeight;
	    drawSelf(area, 1);

	    /*
	    ** Clear drawing window to force copy from redrawn pixmap
	    */
	    if (!AppData.showDrawing) flushAndClear(w);
	    break;

	/*
	** If redrawing, just clear and wait for expose events
	*/
	case scroll_auto_redraw:
	case scroll_self_redraw:
	    flushAndClear(w);
	    break;
    }
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

    if (AppData.currentDraw != NULL) forceRedraw();
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

    if (AppData.currentDraw != NULL) forceRedraw();
} /* end of clippingProc () */

/***************************************************************
**
** FUNCTION:    bufferProc
**
** DESCRIPTION: Callback for show progress buttons on ctrl panel. 
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void bufferProc (w, clientData, callData)
    Widget	w;
    XtPointer 	clientData, callData;
{
    int	val = *((int *) clientData); 
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Ignore callbacks from button unsets
    */
    if (!toggle->set || val == AppData.showDrawing) return;

    /*
    ** Update the global flag
    */
    AppData.showDrawing = val;

    if (AppData.currentDraw != NULL) setupAndDrawUnmoving();
} /* end of bufferProc () */

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

    if (AppData.currentDraw != NULL) forceRedraw();
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

    if (AppData.currentDraw != NULL) forceRedraw();
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

    if (AppData.currentDraw != NULL) forceRedraw();
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
    XButtonPressedEvent *bp = (XButtonPressedEvent *) event;
    Position	x, y;

    if (event->xany.window != XtWindow(AppData.currentDraw)) return;

    /*
    ** If zooming, rescale so clicked-on point remains fixed
    */
    if (AppData.zooming) {
        /*
        ** Convert point to PS units and scale
        */
	xpoint.x = bp->x;
	xpoint.y = bp->y;

	convertToDPS(&xpoint, &point);

	/*
	** If auto-drawing, find out how where the drawing window is
	** compared to the clip window, and add the position to find
	** the mouse coordinates in the clip window
	*/
	if (AppData.scrollStrategy & SCROLL_AUTO) {
	    XtVaGetValues(AppData.autoDrawingArea, XtNx, &x, XtNy, &y, NULL);
	    xpoint.x += x;
	    xpoint.y += y;
	}

	scaleDrawingArea();
	setupAndDraw(False, point.x, point.y, xpoint.x, xpoint.y);

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
** PARAMETERS:	iTime       timing value
**
** RETURN:      None.
**
***************************************************************/

void showTime(iTime)
    int iTime;
{
    char cTime [15];

    if (iTime  ==  0) strcpy (cTime, " ");
    else sprintf (cTime, "%d", iTime);

    XtVaSetValues (AppData.time,
		   XmNlabelString, XmStringCreateSimple(cTime), NULL);
} /* end setTimingValue () */

