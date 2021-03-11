/*
 * $RCSfile: TextMain.c,v $
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

#include "Text.h"
#include "zoom.xbm"
#include "zoom_mask.xbm"
#include <X11/cursorfont.h>
#include <DPS/PSres.h>

/***************************************************************
**
** DATA DECLARATIONS
**
***************************************************************/
/*
** Global pointers to the application data block
*/

AppDataType     AppData;

/*
** Global resource management data
*/
static MrmHierarchy SMrmHierarchy;          /* MRM database hierarchy ID */
static MrmType      DummyClass;            /* and class variable. */
static char         *DbFilenameVec[] =     /* Mrm.heirarchy file list. */
{
    "Text.uid"
};

static void mouseDown(), traceProc(), reshowProc(), justifySel(), spacingSel(),
	showSel(), sizeSel(), issuesSel(), compSel(), magSel(),
	resizeWindow(), refreshWindow(), createProc(), quitApp();

/*
** Names and addresses for Mrm to bind
*/
static MrmRegisterArg RegList[] =
{
	{"resizeWindow"    , (XtPointer) resizeWindow    },
	{"refreshWindow"   , (XtPointer) refreshWindow   },
	{"createProc"      , (XtPointer) createProc      },
	{"quitApp"         , (XtPointer) quitApp         },
	
	{"traceProc"       , (XtPointer) traceProc       },
	{"reshowProc"      , (XtPointer) reshowProc      },
	
	{"justifySel"      , (XtPointer) justifySel      },
	{"spacingSel"      , (XtPointer) spacingSel      },
	{"showSel"         , (XtPointer) showSel         },
	{"sizeSel"         , (XtPointer) sizeSel         },
	{"issuesSel"       , (XtPointer) issuesSel       },
	{"compSel"         , (XtPointer) compSel         },
	{"magSel"          , (XtPointer) magSel          },
	{"scrollProc"      , (XtPointer) scrollProc      },
};

/*
** Font sizes supported in text application
*/
float	FontSizes[NUM_SIZES] = { 10, 11, 12 };

static XrmOptionDescRec CommandLineOptions[] = {
  { "-trace", ".trace", XrmoptionNoArg, (caddr_t)"True",},
};

static XtResource Resources[] = {
  { "trace",
    "Trace",
    XtRBoolean,
    sizeof (Boolean),
    XtOffset (AppDataTypePtr, trace),
    XtRImmediate,
    (caddr_t) False
  }
};

static 	Cursor zoomCursor;     /* cursor for zooming window */
static void initApplication();

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

main (argc, argv)
    int argc;
    char *argv[];
{
    XtAppContext appContext;
    Widget toplevel;
    Widget mainWindow;
    Widget optionShell;

    /*
    ** Initialize MRM before initializing the X Toolkit.
    */
    MrmInitialize ();

    /*
    ** Initialize the X Toolkit. We get back a top level shell widget.
    */
    toplevel = XtAppInitialize (
	&appContext, "Text", CommandLineOptions,
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
    if (MrmFetchWidget (SMrmHierarchy, "MainWindow", toplevel,
        &mainWindow, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf (stderr, "Can't fetch main window\n");
        exit (1);
    }

    /*
    ** Create a shell for the option box
    */
    optionShell = XtVaCreatePopupShell("TextOptions",
				       xmDialogShellWidgetClass,
				       toplevel,
				       XmNallowShellResize, True,
				       XmNmappedWhenManaged, False,
				       NULL);

    /*
    ** Get the option box widget
    */
    if (MrmFetchWidget(SMrmHierarchy, "OptionBox", optionShell,
	&AppData.optionBox, &DummyClass)  !=  MrmSUCCESS)
    {
        fprintf(stderr, "Can't fetch option window\n");
        exit(1);
    }

    /*
    ** Manage the main window and option box and realize everything.
    ** The interface comes up on the display now.
    */
    XtManageChild(mainWindow);
    XtManageChild(AppData.optionBox);
    XtVaSetValues(
	optionShell,
    	XmNmwmFunctions,
        MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE | MWM_FUNC_CLOSE,
        XmNmwmDecorations,
        MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_RESIZEH,
        NULL
    );
    XtRealizeWidget(toplevel);

    /*
    ** Perform onetime initialization of application data structures.
    */
    initApplication();

    /*
    ** Do all the post-realization DPSX processing here
    */
    initDPSContext();

    /*
    ** Draw the initial page into the buffer
    */
    drawSelf();

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

#ifdef __STDC__

static int pkdCmp (a1, b1)
    const void *a1, *b1;
{
    AFMPairKernData *a = (AFMPairKernData *) a1,
		    *b = (AFMPairKernData *) b1;
    int	rc = strcmp ( a->name1, b->name1 );

    return rc ? rc : strcmp ( a->name2, b->name2 );
}

static int cmiCmp (a1, b1)
    const void *a1, *b1;
{
    AFMCharMetricInfo *a = (AFMCharMetricInfo *) a1,
		      *b = (AFMCharMetricInfo *) b1;
    return strcmp ( a->name, b->name );
}

#else /* __STDC__ */

static int pkdCmp (a, b)
    AFMPairKernData *a, *b;
{
    int	rc = strcmp ( a->name1, b->name1 );

    return rc ? rc : strcmp ( a->name2, b->name2 );
}

static int cmiCmp (a, b)
    AFMCharMetricInfo *a, *b;
{
    return strcmp ( a->name, b->name );
}

#endif /* __STDC__ */

static AFMFontInfo *parseAFMFile()
{
    AFMFontInfo		*fi;
    FILE		*AFMfp;
    char		**afmNames, **afmFiles;
    int			numFiles;

    SetPSResourcePolicy(PSSaveReturnValues, 0, NULL);
    numFiles = ListPSResourceFiles(NULL, ".", PSResFontAFM, "Times-Roman",
				   &afmNames, &afmFiles);
    if (numFiles == 0) {
	fprintf(stderr, "Can't locate Times-Roman AFM file\n");
        exit(1);
    }
    AFMfp = fopen (afmFiles[0], "r");
    if (AFMfp == NULL) {
	fprintf(stderr, "Can't open Times-Roman AFM file\n");
        exit(1);
    }
    AFMParseFile (AFMfp, &fi, AFM_GMP);
    fclose (AFMfp);
    free(afmFiles);
    free(afmNames);
    FreePSResourceStorage(1);

    return fi;
}

static void initFontMetrics()
{
    int			s, n, c, code, kernIndex, numOfChars, numOfPairs;
    AFMCharMetricInfo	*cmi;
    AFMPairKernData	*pkd;
    AFMFontInfo		*fi;
    int			maxChar;

    /*
    ** Parse the font metrics file
    */
    fi = parseAFMFile();

    /*
    ** Re-index the font metrics data structure so it is easier to find
    ** character widths and kern pairs
    */
    numOfChars = fi->numOfChars;
    numOfPairs = fi->numOfPairs;

    AppData.metrics.numOfChars = numOfChars;
    AppData.metrics.numOfPairs = numOfPairs;

    /*
    ** Quicksort the pair kern data and the char metrics data for easy access
    ** The cmi array is sorted by character name (a string)
    ** The pkd array is sorted primarily on name1, and secondarily on name2
    */
    qsort (fi->cmi, numOfChars, sizeof(AFMCharMetricInfo), cmiCmp);
    qsort (fi->pkd, numOfPairs, sizeof(AFMPairKernData),   pkdCmp);

    maxChar = 0;
    for (c = 0; c < numOfChars; c++) maxChar = MAX(fi->cmi[c].code, maxChar);

    AppData.metrics.maxChar = maxChar;

    /*
     ** Allocate space for the widths and kern-pairs
     */
    AppData.metrics.widths = (float *) XtCalloc (maxChar+1, sizeof (float));
    AppData.metrics.kernIndex = (int *) XtCalloc (maxChar+1, sizeof (int));
    AppData.metrics.numKernPairs = (int *) XtCalloc (maxChar+1, sizeof (int));
    AppData.metrics.kernPairs =
	    (KernPair *) XtCalloc (numOfPairs, sizeof (KernPair));

    /*
    ** Step through the array of characters
    */
    for (c = kernIndex = 0, pkd = fi->pkd; c < numOfChars; c++) {
	/*
	** Get the character metrics for this character
	*/
	cmi = &(fi->cmi[c]);

	/*
	** Get its ascii code
	*/
	code = cmi->code;

	/*
	** This application does not deal with unencoded characters
	*/
	if (code == -1) continue;

	/*
	** Store its width in the widths array indexed by ascii value
	*/
	AppData.metrics.widths[code] = (fi->cmi[c].wx) / 1000.0;

	/*
	** Assume its kern pairs start at the next kern pair index
	*/
	AppData.metrics.kernIndex[code] = kernIndex;

	/*
	** Step down the kern pair array sent back from the parser
	** until the first character no longer matches the current
	** character. (note the characters are stored as strings)
	** (note that the loop may have 0 iterations)
	*/
	for (n = 0, s = 0; 
	     kernIndex < numOfPairs && !strcmp (cmi->name, pkd->name1); 
	     pkd++, kernIndex++, n++) {
	    /*
	    ** Step down the character metrics array looking for the
	    ** second character in the kern pair for its ascii code
	    */
	    for (; s < numOfChars; s++) {
		if (!strcmp (fi->cmi[s].name, pkd->name2)) {
		    /*
		    ** Found it in the character metrics, put the ascii
		    ** code into the kern pair array, along with the
		    ** amount to kern by
		    */
		    AppData.metrics.kernPairs[kernIndex].code
			    = fi->cmi[s].code;
		    AppData.metrics.kernPairs[kernIndex].dx
			    = pkd->xamt / 1000.0;
		    break;
		}
	    }
	} /* end for */
	/*
	** The number of iterations in the loop on n is the number of
	** kern pairs for this character. (could be 0)
	*/
	AppData.metrics.numKernPairs[code] = n;

    } /* end for */
}

/***************************************************************
**
** FUNCTION:    initApplication
**
** DESCRIPTION: One-time initialization of application data
**		structures.
**
** PARAMETERS:	None.
**
** RETURN:      None.
**
***************************************************************/

static void initApplication ()
{
    Display *dpy = XtDisplay(AppData.drawingArea);
    Window win = XtWindow(AppData.drawingArea);
    XGCValues values;
    Pixmap cursor, mask;
    XColor fore, back;

    /*
    ** Allocate space for the show struct used in show varients
    */
    AllocShowStruct(&AppData.s);

    /*
    ** Allocate space for the list of coordinates used in xshow
    */
    AppData.charspace = (float*) XtCalloc (sizeof (float), MAX_XSHOW);

    /*
    ** Initialize the initial button selections
    */
    AppData.justify	= False;	/* No justification */
    AppData.scrolling	= False;	/* Not in a scroll redraw */
    AppData.zooming	= False;
    AppData.spacing	= 0;		/* No kerning or tracking */
    AppData.show	= show_xshow;	/* use xshow */
    AppData.fontNum	= NUM_SIZES - 1; /* largest font */
    AppData.fontSize	= FontSizes[NUM_SIZES - 1];
    AppData.issues	= CACHE;	/* Font cache on */
    AppData.comp	= 0;		/* No comparisons */
    AppData.scale	= 1.0;		/* 100 magnificaton */
    AppData.magnify	= 100;		/* 100 magnificaton */
    AppData.screen	= True;		/* Use screen widths */

    initFontMetrics();

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
} /* end initApplication () */

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
    ** Resizing automatically moved the y offset
    */
    AppData.yOffset = AppData.drawingHeight;

    /*
    ** Create new pixmap to match the drawable
    */
    XFreePixmap(dpy, AppData.buf);

    AppData.buf = XCreatePixmap(dpy, XtWindow(w), AppData.drawingWidth,
				AppData.drawingHeight, depth);

    /*
    ** Clear pixmaps
    */
    XFillRectangle(dpy, AppData.buf, AppData.gc, 0, 0, 
		   AppData.drawingWidth, AppData.drawingHeight);

    /*
    ** Update the context to reflect the new pixmap
    */
    XDPSSetContextDrawable(AppData.dpsCtxt, AppData.buf,
			   AppData.drawingHeight);
    PSscale(AppData.scale, AppData.scale);

    /*
    ** Move the drawing area so the top left corner
    ** of the image remains located at the top left corner
    */
    positionDrawingArea(point.x, point.y, 0, 0);

    drawSelfAndUpdate();
} /* end resizeWindow() */

/***************************************************************
**
** FUNCTION:    refreshWindow
**
** DESCRIPTION: Callback routine to handle regular expose events
**		to the main window.  Causes the window to be 
**		refreshed.
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
    XCopyArea(XtDisplay(w), AppData.buf, XtWindow(w), AppData.gc,
	      e->x, e->y, e->width, e->height, e->x, e->y);
} /* end refreshWindow() */

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

static void createProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int 		widgetNum = *(int *) clientData;

    switch (widgetNum) {
	case cMainDrawArea:
	    /*
            ** Save widget ID in application data structure
            */
	    AppData.drawingArea = w;

	    XtAddEventHandler(w, ButtonPressMask, False, mouseDown, NULL);
            XtAddRawEventHandler(w, 0, True, graphicExpose, NULL);
            break;

        case cTraceToggle:
            if (AppData.trace) XtVaSetValues(w, XmNset, True, NULL);
	    break;
	
	case cTimingText0:	AppData.time = w;		break;
	case cTimingText1:	AppData.numChars = w;		break;
	case cTimingText2:	AppData.kernPairs = w;		break;

	case cStatusText0:	AppData.cacheStatus[0] = w;	break;
	case cStatusText1:	AppData.cacheStatus[1] = w;	break;
	case cStatusText2:	AppData.cacheStatus[2] = w;	break;
	case cStatusText3:	AppData.cacheStatus[3] = w;	break;
	case cStatusText4:	AppData.cacheStatus[4] = w;	break;
	case cStatusText5:	AppData.cacheStatus[5] = w;	break;
	case cStatusText6:	AppData.cacheStatus[6] = w;	break;

	case cHsb:		AppData.hScroll = w;		break;
	case cVsb:		AppData.vScroll = w;		break;
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
    exit(0);
} /* end quitApp () */

/***************************************************************
**
** FUNCTION:    traceProc
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

static void traceProc(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    XDPSChainTextContext (AppData.dpsCtxt, toggle->set);
} /* end traceProc() */

/***************************************************************
**
** FUNCTION:    reshowProc
**
** DESCRIPTION: Callback routine for redraw button pushed.
**              Redraws the text and displays the time.
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void reshowProc (w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    /*
    ** Redraw the page and time fields
    */
    drawSelfAndUpdate();
} /* end reshowProc () */

/***************************************************************
**
** FUNCTION:    justifySel
**
** DESCRIPTION: Callback routine for activating justify buttons
**              Sets the justify button number
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

static void justifySel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the selected radio button if toggle state is set
    */
    if (toggle->set && AppData.justify != (num != 0)) {
	AppData.justify = (num != 0);
	reshowProc(w, NULL, NULL);
    }
} /* end justifySel () */

/***************************************************************
**
** FUNCTION:    spacingSel
**
** DESCRIPTION: Callback routine for activating spacing buttons
**              Sets the spacing bits
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

static void spacingSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the state of the toggle button
    */
    if ( toggle->set ) AppData.spacing |=  (1 << num);
    else AppData.spacing &= ~(1 << num);

    reshowProc(w, NULL, NULL);
} /* end spacingSel () */

/***************************************************************
**
** FUNCTION:    showSel
**
** DESCRIPTION: Callback routine for activating show buttons
**              Sets the show manner
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

static void showSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the selected radio button if toggle state is set
    */
    if (toggle->set && AppData.show != num) {
	AppData.show = num;
	reshowProc(w, NULL, NULL);
    }
} /* end showSel () */

/***************************************************************
**
** FUNCTION:    sizeSel
**
** DESCRIPTION: Callback routine for picking a new font size
**              Sets the font size
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void sizeSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;

    /*
    ** Set the new font size
    */
    if (AppData.fontSize != FontSizes[num]) {
	AppData.fontNum  = num;
	AppData.fontSize = FontSizes[num];
	reshowProc(w, NULL, NULL);
    }
} /* end sizeSel () */

/***************************************************************
**
** FUNCTION:    issuesSel
**
** DESCRIPTION: Callback routine for activating issues buttons
**              Sets the font issues
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

static void issuesSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the state of the toggle button
    */
    if (toggle->set) AppData.issues |=  (1 << num);
    else AppData.issues &= ~(1 << num);

    /*
    ** Set the cache low if it is not in use
    */
    if (num == 0) {
	if (toggle->set) {
	    PSWSetcacheparams(AppData.size, AppData.lower, AppData.upper);
	} else PSWSetcacheparams(0, 0, 0);
	
    /*
    ** Set flag indicating use of outline widths or bitmap widths
    */
    } else AppData.screen = !toggle->set;

    reshowProc(w, NULL, NULL);
} /* end issuesSel () */

/***************************************************************
**
** FUNCTION:    compSel
**
** DESCRIPTION: Callback routine for activating comparisons buttons
**              Sets the comparison bits
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

static void compSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the state of the toggle push button
    */
    AppData.comp = (toggle->set) ? (num + 1) : 0;

    reshowProc(w, NULL, NULL);
} /* end compSel () */

/***************************************************************
**
** FUNCTION:    magSel
**
** DESCRIPTION: Callback routine for activating magnify buttons
**              Sets the magnification, makes the cursor the zoom
**		cursor, and sets the zooming flag for the later click
**
** PARAMETERS:  w           callback widget ID
**              clientData  callback client data
**              callData    callback Motif data structure
**
** RETURN:      None.
**
***************************************************************/

/* ARGSUSED */

static void magSel(w, clientData, callData)
    Widget w;
    XtPointer clientData, callData;
{
    int num = *(int *) clientData;
    XmToggleButtonCallbackStruct *toggle =
	    (XmToggleButtonCallbackStruct *) callData;

    /*
    ** Change the selected radio button if toggle state is set
    */
    if (toggle->set && AppData.magnify != num) {
	AppData.zooming = True;
	AppData.magnify = num;
	if (zoomCursor != None) {
	    XDefineCursor(XtDisplay(AppData.drawingArea),
			  XtWindow(AppData.drawingArea), zoomCursor);
	}
    }
} /* end magSel () */

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
 
    if (event->xany.window != XtWindow(AppData.drawingArea)) return;

    xpoint.x = bp->x;
    xpoint.y = bp->y;

    /*
    ** If zooming, rescale so clicked-on point remains fixed
    */
    if (AppData.zooming) {
        /*
        ** Convert point to PS units and scale
        */
	convertToDPS(&xpoint, &point);
	scaleDrawingArea(point.x, point.y, xpoint.x, xpoint.y);

	drawSelfAndUpdate();

	AppData.zooming = False;
	if (zoomCursor != None) {
	    XUndefineCursor(XtDisplay(w), XtWindow(w));
	}
    }
} /* end mouseDown() */

static XmString blank = NULL;

/***************************************************************
**
** FUNCTION:    eraseFields 
**
** DESCRIPTION: Routine to clear the XmText widgets for timing and status
**
** PARAMETERS: None
**
** RETURN:      None.
**
***************************************************************/
void eraseFields ()
{
    int	i;

    if (blank == NULL) blank = XmStringCreateSimple("");

    AppData.timingInfo.chars = 0;
    AppData.timingInfo.kerns = 0;
    AppData.timingInfo.time = 0;

    XtVaSetValues(AppData.time, XmNlabelString, blank, NULL);
    XtVaSetValues(AppData.numChars, XmNlabelString, blank, NULL);
    XtVaSetValues(AppData.kernPairs, XmNlabelString, blank, NULL);

    for (i = 0; i < 7; i++) {
	XtVaSetValues(AppData.cacheStatus[i], XmNlabelString, blank, NULL);
    }
} /* end eraseFields() */

/***************************************************************
**
** FUNCTION:    displayFields 
**
** DESCRIPTION: Routine to set the XmText widgets for timing and status
**
** PARAMETERS:
**
** RETURN:      None.
**
***************************************************************/

void displayFields ()
{
    int     i, cvalues[7];
    char    field[7];
    Timing *t = &AppData.timingInfo;

    if (t->time != 0) {
	sprintf (field, "%d", t->time);
	XtVaSetValues(AppData.time,
		      XmNlabelString, XmStringCreateSimple(field), NULL);

	sprintf (field, "%d", t->chars);
	XtVaSetValues(AppData.numChars,
		      XmNlabelString, XmStringCreateSimple(field), NULL);

	sprintf (field, "%d", t->kerns);
	XtVaSetValues(AppData.kernPairs,
		      XmNlabelString, XmStringCreateSimple(field), NULL);
    }

    /*
    ** Get the cache status from PS
    */
    PSWCachestatus(cvalues);

    /*
    ** Fill the 7 status fields
    */
    for (i = 0; i < 7; i++) {
	sprintf (field, "%d", cvalues[i]);
	XtVaSetValues(AppData.cacheStatus[i],
		      XmNlabelString, XmStringCreateSimple(field), NULL);
    }
} /* end displayFields() */
