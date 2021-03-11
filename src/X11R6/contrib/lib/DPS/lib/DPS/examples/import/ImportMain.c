/*
 * $RCSfile: ImportMain.c,v $
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

#include "Import.h"
#include <X11/cursorfont.h>
#include <sys/stat.h>
#include <X11/Xatom.h>

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
    "Import.uid"
};

static void resizeWindow(), refreshWindow(), createProc(), quitApp(),
	traceProc(), showBufferProc(), bufferExposeProc(), 
	initApplication(), mouseDown(), fileProc(), 
	cutProc(), copyProc(), pasteProc(), deleteProc(),
	rotateProc(), scaleProc(), frontProc(), backProc(),
	origSizeProc(), origRatioProc(), useBoxProc(), setPreview();

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
    {"fileProc"	       , (XtPointer) fileProc	     },
    {"cutProc"	       , (XtPointer) cutProc	     },
    {"copyProc"	       , (XtPointer) copyProc	     },
    {"pasteProc"       , (XtPointer) pasteProc	     },
    {"deleteProc"      , (XtPointer) deleteProc	     },
    {"rotateProc"      , (XtPointer) rotateProc	     },
    {"scaleProc"       , (XtPointer) scaleProc	     },
    {"frontProc"       , (XtPointer) frontProc	     },
    {"backProc"	       , (XtPointer) backProc	     },
    {"origSizeProc"    , (XtPointer) origSizeProc    },
    {"origRatioProc"   , (XtPointer) origRatioProc   },
    {"useBoxProc"      , (XtPointer) useBoxProc	     },
    {"setPreview"      , (XtPointer) setPreview	     }
};

static XrmOptionDescRec CommandLineOptions[] = {
  { "-trace", ".trace", XrmoptionNoArg, (caddr_t) "True",},
};

static XtResource Resources[] = {
  {
    "trace",
    "Trace",
    XtRBoolean,
    sizeof (Boolean),
    XtOffset (AppDataTypePtr, trace),
    XtRImmediate,
    (caddr_t) False
  },
  {
    "pixmapMaxSize",
    "PixmapMaxSize",
    XtRInt,
    sizeof (int),
    XtOffset (AppDataTypePtr, pixmapMaxSize),
    XtRImmediate,
    (caddr_t) 2097152 /* Two megabytes */
  }
};

static Atom XA_CLIPBOARD, XA_TARGETS, XA_ADOBE_EPS,
	XA_ADOBE_EPSI, XA_FILE_NAME;

/***************************************************************
**
** FUNCTION:	main
**
** DESCRIPTION:	Main procedure for the Import Application.
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

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize();

    /*
    ** Initialize the X Toolkit. We get back a top level shell widget.
    */
    toplevel = XtAppInitialize (
	&appContext, "Import", CommandLineOptions,
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
} /* end main() */

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
    Display *dpy = XtDisplay(AppData.drawingArea);
    Window win =  XtWindow(AppData.drawingArea);
    Pixmap p;
    /*
    ** Initialize the booleans
    */
    AppData.showBuffer = False;
    AppData.scrolling = False;
    AppData.includePreview = True;
    AppData.deepPreview = False;

    AppData.moveElement = NULL;

    /* 
    ** Create GCs
    */
    values.foreground = WhitePixelOfScreen(XtScreen(AppData.drawingArea));
    AppData.gc = XCreateGC(dpy, win, GCForeground, &values);
    values.foreground = BlackPixelOfScreen(XtScreen(AppData.drawingArea));
    AppData.blackgc = XCreateGC(dpy, win, GCForeground, &values);

    p = XCreatePixmap(dpy, win, 1, 1, 1);
    values.foreground = 0;
    AppData.bitmapgc = XCreateGC(dpy, p, GCForeground, &values);
    XFreePixmap(dpy, p);

    /*
    ** Find depth of drawing area
    */
    XtVaGetValues(AppData.drawingArea, XtNdepth, &AppData.depth, NULL);

    /*
    ** Create cursors
    */
    AppData.crosshairCursor = XCreateFontCursor(dpy, XC_crosshair);
    AppData.busyCursor = XCreateFontCursor(dpy, XC_watch);

    /*
    ** Initialize atoms
    */
    XA_CLIPBOARD = XInternAtom(dpy, "CLIPBOARD", False);
    XA_TARGETS = XInternAtom(dpy, "TARGETS", False);
    XA_ADOBE_EPS = XInternAtom(dpy, "_ADOBE_EPS", False);
    XA_ADOBE_EPSI = XInternAtom(dpy, "_ADOBE_EPSI", False);
    XA_FILE_NAME = XInternAtom(dpy, "FILE_NAME", False);
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
    ** Convert upper left corner into PS units so we can keep it fixed
    */
    xpoint.x = 0;
    xpoint.y = 0;
    convertToDPS(&xpoint, &point);

    /*
    ** Get new size of drawing area
    */
    XtVaGetValues(AppData.drawingArea, XtNheight, &AppData.drawingHeight,
		  XtNwidth, &AppData.drawingWidth,
		  XtNdepth, &depth, NULL);

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
    ** Update the gstates for the buffers to reflect the new pixmaps
    */
    XDPSSetContextGState(AppData.dpsCtxt, AppData.origGState);
    XDPSSetContextDrawable(AppData.dpsCtxt, AppData.original,
                           AppData.drawingHeight);
    XDPSUpdateContextGState(AppData.dpsCtxt, AppData.origGState);

    XDPSSetContextGState(AppData.dpsCtxt, AppData.compGState);
    XDPSSetContextDrawable(AppData.dpsCtxt, AppData.composite,
                           AppData.drawingHeight);
    XDPSUpdateContextGState(AppData.dpsCtxt, AppData.compGState);

    /*
    ** Update the gstate for the window to reflect the new origin
    */
    XDPSSetContextGState(AppData.dpsCtxt, AppData.winGState);
    AppData.yOffset = AppData.drawingHeight;
    PSsetXoffset(AppData.xOffset, AppData.yOffset);
    XDPSUpdateContextGState(AppData.dpsCtxt, AppData.winGState);

    /*
    ** Move the drawing area so the upper left corner
    ** of the image remains located at the upper left corner
    */
    positionDrawingArea(point.x, point.y, 0, 0);

    drawSelfAndUpdate(NULL);
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
    if (AppData.selected) drawSelectionMarks();
} /* end refreshWindow() */

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

        case cTraceToggle:
            if (AppData.trace) XtVaSetValues(w, XmNset, True, NULL);
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
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    XDPSChainTextContext (AppData.dpsCtxt, toggle->set);
    XDPSChainTextContext (AppData.imageCtxt, toggle->set);
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
} /* end cancelFileCallback() */

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
} /* end makeNormalString() */

/*************************************************************
**
** FUNCTION:	doOpen
**
** DESCRIPTION:	Opens a file and adds it to the drawing
**
** PARAMETERS:	name	name of file
**		f	pointer to open file
**
** RETURN:	None
**
*************************************************************/

static void doOpen(name, f)
    String name;
    FILE *f;
{
    Element *e;
    struct stat buf;

    /*
    ** Create a new element structure, or reuse the previous one
    */
    if (AppData.adding != NULL) {
	e = AppData.adding;
	XtFree(e->filename);
	freeResourceList(e->resources);
	fclose(e->f);
    } else {
	e = XtNew(Element);
	e->tx = e->ty = e->sx = e->sy = e->rotation = 0.0;
	e->origBBox.ll.x = e->origBBox.ll.y =
		e->origBBox.ur.x = e->origBBox.ur.y = 0.0;
    }

    /*
    ** Fill element structure with info about file
    */
    (void) fstat(fileno(f), &buf);
    e->filename = name;
    e->length = buf.st_size;
    e->f = f;
    e->image = e->mask = None;
    e->next = NULL;
    e->resources = NULL;

    /*
    ** Set cursor to busy while we parse the file for comments
    */
    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), AppData.busyCursor);
    XFlush(XtDisplay(AppData.drawingArea));

    if (parseFileHeader(e)) {
	/*
	** Get ready to add file.  We don't really do anything here; instead
	** it gets added in the mouseDown event handler
	*/
	AppData.adding = e;
	XDefineCursor(XtDisplay(AppData.drawingArea),
		      XtWindow(AppData.drawingArea), AppData.crosshairCursor);
    } else {
	/*
	** Parse error, back out
	*/
	XDefineCursor(XtDisplay(AppData.drawingArea),
		      XtWindow(AppData.drawingArea), None);
	XtFree(e->filename);
	fclose(e->f);
	XtFree((XtPointer) e);
	AppData.adding = NULL;
    }

    if (AppData.selected != NULL) unselect();
} /* end doOpen() */

/*************************************************************
**
** FUNCTION:	writeToFileFunc
**
** DESCRIPTION:	Function to write its data out to a file.  When
**		passed to writePictureToFile, this makes the output
**		go to a file
**
** PARAMETERS:	buf		Null-terminated string to write
**		clientData	Pointer to file
**
** RETURN:	None
**
*************************************************************/

static void writeToFileFunc(buf, clientData)
    char *buf;
    char *clientData;
{
    FILE *f = (FILE *) clientData;
    fputs(buf, f);
} /* end writeToFileFunc() */

/*************************************************************
**
** FUNCTION:	doSave
**
** DESCRIPTION:	Write the picture out to a file
**
** PARAMETERS:	f	File pointer to output file
**
** RETURN:	None
**
*************************************************************/

static void doSave(f)
    FILE *f;
{
    /*
    ** If nothing in picture, don't write anything
    */
    if (AppData.elements == NULL) {
	fclose(f);
	return;
    }

    writePictureToFile(writeToFileFunc, (char *) f,
		       (Element *) NULL, AppData.includePreview);
    fclose(f);
} /* end doSave() */

/***************************************************************
**
** FUNCTION:    openFileCallback
**
** DESCRIPTION: Callback procedure to open a file for input or for saving
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
    String 	str = makeNormalString(fdata->value);
    int 	*action = (int *) clientData;
    FILE	*f;

    /*
    ** Open the file for reading (if input) or writing (if output)
    */
    if (*action == 0) f = fopen(str, "r");
    else f = fopen(str, "w");

    if (f == NULL) {
	fprintf(stderr, "Error:  Could not open file %f\n", str);
    } else {
	XtUnmanageChild(AppData.fileDialog);
	if (*action == 0) doOpen(str, f);
	else doSave(f);
    }
} /* end openFileCallback() */

/***************************************************************
**
** FUNCTION:    fileProc
**
** DESCRIPTION: Callback routine to open or save a file
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void fileProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    static int action;

    action = *(int *) clientData;

    /*
    ** If there is no file dialog, create one and unmanage the help button
    */
    if (AppData.fileDialog == NULL) {
	AppData.fileDialog =
		XmCreateFileSelectionDialog(AppData.drawingArea, "fileDialog",
					    (ArgList) NULL, 0);
	w = XmFileSelectionBoxGetChild(AppData.fileDialog,
				       XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(w);
	XtAddCallback(AppData.fileDialog, XmNcancelCallback,
		      cancelFileCallback, (XtPointer) NULL);
	XtAddCallback(AppData.fileDialog, XmNokCallback,
		      openFileCallback, (XtPointer) &action);
    }

    XtManageChild(AppData.fileDialog);
} /* end fileProc() */

/*************************************************************
**
** FUNCTION:	convertSelection
**
** DESCRIPTION:	Callback procedure to deliver selection value
**
** PARAMETERS:	w		Widget owning selection
**		selection	Selection desired
**		target		Type of information required
**
** RETURN:	type		Data type of selection
**		value		Selection data
**		length		Length of selection data
**		format		Size of selection data units
**
*************************************************************/

/* ARGSUSED */

static Boolean convertSelection(w, selection, target, type, value,
				length, format)
    Widget w;
    Atom *selection, *target;
    Atom *type;
    XtPointer *value;
    unsigned long *length;
    int *format;
{
    Element *e;
    char *buf;

    /*
    ** Type of TARGETS means a request for supported types
    */
    if (*target == XA_TARGETS) {
	Atom *targets = (Atom *) XtMalloc(4 * sizeof(Atom));
	targets[0] = XA_ADOBE_EPS;
	targets[1] = XA_ADOBE_EPSI;
	targets[2] = XA_FILE_NAME;
	targets[3] = XA_PIXMAP;
	*value = (XtPointer) targets;
	*type = XA_ATOM;
	*length = 4;
	*format = 32;
	return True;
    }

    /*
    ** If there is a pending cut, deliver it; otherwise deliver copied
    ** element.  If nothing is selected, no data to return
    */
    if (AppData.pendingCut != NULL) e = AppData.pendingCut;
    else if (AppData.copiedElement != NULL) e = AppData.copiedElement;
    else return False;
    
    /*
    ** Return data in EPS format either with or without a preview image
    */
    if (*target == XA_ADOBE_EPS || *target == XA_ADOBE_EPSI) {
	XDefineCursor(XtDisplay(AppData.drawingArea),
		      XtWindow(AppData.drawingArea), AppData.busyCursor);
	buf = convertToEPS(e, (*target == XA_ADOBE_EPSI));
	XDefineCursor(XtDisplay(AppData.drawingArea),
		      XtWindow(AppData.drawingArea), None);
	*value = (XtPointer) buf;
	*length = strlen(buf) + 1;
	*type = XA_STRING;
	*format = 8;
	return True;
    }
    /*
    ** Return name of selected file
    */
    if (*target == XA_FILE_NAME) {
	char *name = XtNewString(e->filename);
	*value = (XtPointer) name;
	*length = strlen(name) + 1;
	*type = XA_STRING;
	*format = 8;
	return True;
    }
    /*
    ** Return pixmap id of rendered image 
    */
    if (*target == XA_PIXMAP) {
	Pixmap *p;
	if (e->image == None) renderElement(e);
	if (e->image == None) return False;
	p = XtNew(Pixmap);
	*p = e->image;
	*value = (XtPointer) p;
	*length = 1;
	*type = XA_DRAWABLE;
	*format = 32;
	return True;
    }

    /*
    ** Unrecognized target type
    */
    return False;
} /* end convertSelection() */

/*************************************************************
**
** FUNCTION:	loseSelection
**
** DESCRIPTION:	Callback procedure for losing selection.  If there
**		is a pending cut selection, free it
**
** PARAMETERS:	w		Widget owning selection
**		selection	Selection being lost
**
** RETURN:	None
**
*************************************************************/

/* ARGSUSED */

static void loseSelection(w, selection)
    Widget w;
    Atom *selection;
{
    if (AppData.pendingCut != NULL) {
	freeElement(AppData.pendingCut);
	AppData.pendingCut = NULL;
    }
    AppData.copiedElement = NULL;

} /* end loseSelection() */

/***************************************************************
**
** FUNCTION:    cutProc
**
** DESCRIPTION: Callback routine to cut selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void cutProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;

    /*
    ** If nothing selected, return.  If there's already a pending cut,
    ** delete it
    */
    if (e == NULL) return;
    if (AppData.pendingCut != NULL) freeElement(AppData.pendingCut);
    AppData.copiedElement = NULL;

    /*
    ** Remove selected element from the list of element and move to pending cut
    */
    if (e->next == NULL) AppData.lastElement = e->prev;
    else e->next->prev = e->prev;
    if (e->prev == NULL) AppData.elements = e->next;
    else e->prev->next = e->next;

    AppData.pendingCut = e;
    AppData.selected = NULL;
    drawSelfAndUpdate(NULL);

    /*
    ** Assert ownership of the selection
    */
    XtOwnSelection(AppData.drawingArea, XA_CLIPBOARD,
		   XtLastTimestampProcessed(XtDisplay(w)),
		   convertSelection, loseSelection, NULL);
} /* end cutProc() */

/***************************************************************
**
** FUNCTION:    copyProc
**
** DESCRIPTION: Callback routine to copy selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void copyProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    if (AppData.selected == NULL) return;

   /*
    ** If there's a pending cut, delete it
    */
    if (AppData.pendingCut != NULL) {
	freeElement(AppData.pendingCut);
	AppData.pendingCut = NULL;
    }

    AppData.copiedElement = AppData.selected;

    /*
    ** Assert ownership of the selection
    */
    (void) XtOwnSelection(AppData.drawingArea, XA_CLIPBOARD,
		   XtLastTimestampProcessed(XtDisplay(w)),
		   convertSelection, loseSelection, NULL);
} /* end copyProc() */

/*************************************************************
**
** FUNCTION:	gotSelection
**
** DESCRIPTION:	
**
** PARAMETERS:	
**
** RETURN:	None
**
*************************************************************/

/* ARGSUSED */

static void gotSelection(w, data, selection, type, value, length, format)
    Widget w;
    XtPointer data;
    Atom *selection, *type;
    XtPointer value;
    unsigned long *length;
    int *format;
{
    /*
    ** Check that we got what we expected
    */
    if (*selection != XA_CLIPBOARD || *type != XA_STRING ||
	*format != 8 || value == NULL) return;

    /*
    ** Paste the selection value into the picture
    */
    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), AppData.busyCursor);
    pasteEPS((char *) value, *length);
    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), None);
} /* end gotSelection() */

/***************************************************************
**
** FUNCTION:    pasteProc
**
** DESCRIPTION: Callback routine to paste selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void pasteProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    /*
    ** Fetch the value of the clipboard selection
    */
    XtGetSelectionValue(AppData.drawingArea, XA_CLIPBOARD, XA_ADOBE_EPS,
			gotSelection, NULL,
			XtLastTimestampProcessed(XtDisplay(w)));
} /* end pasteProc() */

/***************************************************************
**
** FUNCTION:    deleteProc
**
** DESCRIPTION: Callback routine to delete selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void deleteProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;

    if (e == NULL) return;

    /*
    ** Remove selected element from the chain
    */
    if (e->next == NULL) AppData.lastElement = e->prev;
    else e->next->prev = e->prev;
    if (e->prev == NULL) AppData.elements = e->next;
    else e->prev->next = e->next;

    if (e == AppData.copiedElement) {
	AppData.pendingCut = e;
	AppData.copiedElement = NULL;
    } else freeElement(e);

    AppData.selected = NULL;
    drawSelfAndUpdate(NULL);
} /* end deleteProc() */

/***************************************************************
**
** FUNCTION:    rotateProc
**
** DESCRIPTION: Callback routine to rotate selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void rotateProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;

    if (e == NULL) return;

    /*
    ** Just redefine cursor.  Real work takes place in mouseDown procedure
    */
    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), AppData.crosshairCursor);
    AppData.rotating = True;
} /* end rotateProc() */

/***************************************************************
**
** FUNCTION:    scaleProc
**
** DESCRIPTION: Callback routine to scale selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void scaleProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;

    if (e == NULL) return;

    /*
    ** Just redefine cursor.  Real work takes place in mouseDown procedure
    */
    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), AppData.crosshairCursor);
    AppData.scaling = True;
} /* end scaleProc() */

/***************************************************************
**
** FUNCTION:    frontProc
**
** DESCRIPTION: Callback routine to raise selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void frontProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;

    /*
    ** If nothing selected or selected element is already in front, return
    */
    if (e == NULL || e == AppData.elements) return;

    /*
    ** Remove from list and re-insert at the front
    */
    if (e->next == NULL) AppData.lastElement = e->prev;
    else e->next->prev = e->prev;
    if (e->prev == NULL) AppData.elements = e->next;
    else e->prev->next = e->next;

    e->next = AppData.elements;
    e->prev = NULL;
    AppData.elements->prev = e;
    AppData.elements = e;
    drawSelfAndUpdate(e);
} /* end frontProc() */

/***************************************************************
**
** FUNCTION:    backProc
**
** DESCRIPTION: Callback routine to lower selection
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void backProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;

    /*
    ** If nothing selected or selected element is already in back, return
    */
    if (e == NULL || e == AppData.lastElement) return;

    /*
    ** Remove from list and re-insert at the end
    */
    if (e->next == NULL) AppData.lastElement = e->prev;
    else e->next->prev = e->prev;
    if (e->prev == NULL) AppData.elements = e->next;
    else e->prev->next = e->next;

    e->prev = AppData.lastElement;
    e->next = NULL;
    AppData.lastElement->next = e;
    AppData.lastElement = e;
    drawSelfAndUpdate(NULL);
} /* end backProc() */

/***************************************************************
**
** FUNCTION:    origSizeProc
**
** DESCRIPTION: Callback routine to restore selection to original size
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void origSizeProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;

    if (e == NULL || (e->sx == 1.0 && e->sy == 1.0)) return;

    /*
    ** Set the x and y scale factors to 1 and redraw
    */
    e->sx = e->sy = 1.0;
    updateElement(e);
    drawSelfAndUpdate(NULL);
} /* end origSizeProc() */

/***************************************************************
**
** FUNCTION:    origRatioProc
**
** DESCRIPTION: Callback routine to restore selection to original ratio
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void origRatioProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    Element *e = AppData.selected;
    float s;

    if (e == NULL || ABS(e->sx) == ABS(e->sy)) return;

    /*
    ** Set the x and y scale factors to the minimum of the two, but keep the
    ** signs unchanged.  This makes the new size fit within the previous size
    */
    s = MIN(ABS(e->sx), ABS(e->sy));
    e->sx = SIGN(e->sx) * s;
    e->sy = SIGN(e->sy) * s;
    updateElement(e);
    drawSelfAndUpdate(NULL);
} /* end origRatioProc() */

/***************************************************************
**
** FUNCTION:    useBoxProc
**
** DESCRIPTION: Callback routine to use boxes instead of pictures
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void useBoxProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    Element *e;

    AppData.useBoxes = toggle->set;

    /*
    ** If turning off boxes, go through elements and render those that have
    ** no pixmaps
    */
    if (!toggle->set) {
	for (e = AppData.elements; e != NULL; e = e->next) {
	    if (e->image == None) renderElement(e);
	}
    }
    drawSelfAndUpdate(NULL);
} /* end useBoxProc() */

/***************************************************************
**
** FUNCTION:    setPreview
**
** DESCRIPTION: Callback routine to set preview options
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void setPreview(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;
    int action = *(int *) clientData;

    if (action == 0) AppData.includePreview = toggle->set;
    else AppData.deepPreview = toggle->set;
} /* end setPreview() */

/***************************************************************
**
** FUNCTION:	mouseDown
**
** DESCRIPTION:	This function acts as the event handler for the
**		ButtonDown event.  If we're adding something, start
**		sweeping out an area.  If we're scaling, start scaling box
**		If rotating, start rotating box.  If none of the above,
**		see if we can select or move something
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
    XRect	rect;
    XButtonPressedEvent *bp = (XButtonPressedEvent *) event;
    Element	*e;
 
    if (event->xany.window != XtWindow(AppData.drawingArea)) return;

    xpoint.x = bp->x;
    xpoint.y = bp->y;

    /*
    ** If adding an element, sweep out a rectangle and add
    */
    if (AppData.adding != NULL) {
	sweepRectangle(&xpoint, &AppData.adding->origBBox, &rect);
	XDefineCursor(XtDisplay(AppData.drawingArea),
		      XtWindow(AppData.drawingArea), AppData.busyCursor);
	addElement(&rect);
	XDefineCursor(XtDisplay(AppData.drawingArea),
		      XtWindow(AppData.drawingArea), None); 
    /*
    ** If rotating, start rotation
    */
    } else if (AppData.rotating) {
	e = AppData.selected;
	unselect();
	rotateElement(&xpoint, e);
	AppData.rotating = False;
	selectElement(e);
    /*
    ** If scaling, start scale operation
    */
    } else if (AppData.scaling) {
	e = AppData.selected;
	unselect();
	scaleElement(&xpoint, e);
	AppData.scaling = False;
	selectElement(e);
   } else {
	/*
	** Try to select something.  Convert mouse point into document
	** coordinates
	*/
	xpoint.x -= AppData.originX;
	xpoint.y += AppData.scaledHeight - AppData.originY;
	for (e = AppData.elements; e != NULL; e = e->next) {
	    /*
	    ** Check if mouse point is in element
	    */
	    if (pointInElement(&xpoint, e)) {
		if (e == AppData.selected) {
		    /*
		    ** If the newly selected element is the same as the
		    ** current one, start move
		    */
		    unselect();
		    xpoint.x += AppData.originX;
		    xpoint.y -= AppData.scaledHeight - AppData.originY;
		    moveElement(&xpoint, e);
		} else unselect();
		selectElement(e);
		return;
	    }
	}
	unselect();
    }
} /* end mouseDown() */
