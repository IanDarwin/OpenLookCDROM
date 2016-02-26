/*
 * Copyright 1989 O'Reilly and Associates, Inc.

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */


/*
 * xbitmap3.c
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

#include "BitmapEdit.h"

#include <stdio.h>

#define DRAWN 1
#define UNDRAWN 0

struct {
    GC draw_gc, undraw_gc;
    Pixmap normal_bitmap, reverse_bitmap;
    Widget showNormalBitmap, showReverseBitmap;
    Dimension pixmap_width_in_cells, pixmap_height_in_cells;
    String filename;	/* filename to read and write */
} bitmap_stuff;

static void CellToggled();

/* ARGSUSED */
static void 
Printout(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;	/* unused */
{
    XWriteBitmapFile(XtDisplay(widget), bitmap_stuff.filename, bitmap_stuff.normal_bitmap,
	    bitmap_stuff.pixmap_width_in_cells, bitmap_stuff.pixmap_height_in_cells, 0, 0);
}

/*ARGSUSED*/
static void
RedrawSmallPicture(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
	Pixmap pixmap;

	if (w == bitmap_stuff.showNormalBitmap)
		pixmap = bitmap_stuff.normal_bitmap;
	else
		pixmap = bitmap_stuff.reverse_bitmap;

    if (DefaultDepthOfScreen(XtScreen(w)) == 1)
 	    XCopyArea(XtDisplay(w), pixmap, XtWindow(w),
			DefaultGCOfScreen(XtScreen(w)), 0, 0, 
			bitmap_stuff.pixmap_width_in_cells, bitmap_stuff.pixmap_height_in_cells, 0, 0);
    else
 	    XCopyPlane(XtDisplay(w), pixmap, XtWindow(w),
    		DefaultGCOfScreen(XtScreen(w)), 0, 0, 
			bitmap_stuff.pixmap_width_in_cells, bitmap_stuff.pixmap_height_in_cells, 0, 0, 1);
}

main(argc, argv)
int argc;
char *argv[];
{
	XtAppContext app_context;
    Widget topLevel, form, bigBitmap, buttonbox, quit, output;
    extern void exit();
    static XtActionsRec window_actions[] = {
	    {"redraw_small_picture", RedrawSmallPicture}
    };

    String trans = "<Expose>:	redraw_small_picture()";

    static XrmOptionDescRec table[] = 
    {
        {"-pw",            "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
        {"-pixmapwidth",   "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
        {"-ph",            "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
        {"-pixmapheight",  "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
        {"-cellsize",      "*cellSizeInPixels",           XrmoptionSepArg, NULL},
    };
    
        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XBitmap3",         /* Application class */
        table, XtNumber(table),   /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    if (argv[1] != NULL)
         bitmap_stuff.filename = argv[1];
    else {
         fprintf(stderr, "xbitmap: must specify filename on command line\n");
         exit(1);
    }

    form = XtCreateManagedWidget("form", formWidgetClass, topLevel, NULL, 0);

    buttonbox = XtCreateManagedWidget("buttonbox", boxWidgetClass, form, NULL, 0);

    output = XtCreateManagedWidget("output", commandWidgetClass, buttonbox, NULL, 0);

    XtAddCallback(output, XtNcallback, Printout, NULL);

    quit = XtCreateManagedWidget("quit", commandWidgetClass, buttonbox, NULL, 0);

    XtAddCallback(quit, XtNcallback, exit, NULL);

    XtAppAddActions(app_context, window_actions, XtNumber(window_actions));

    bigBitmap = XtCreateManagedWidget("bigBitmap", bitmapEditWidgetClass, 
			form, NULL, 0);

    XtAddCallback(bigBitmap, XtNcallback, CellToggled, NULL);

    XtVaGetValues(bigBitmap, 
		XtNpixmapHeightInCells, &bitmap_stuff.pixmap_height_in_cells,
		XtNpixmapWidthInCells, &bitmap_stuff.pixmap_width_in_cells,
		NULL);

    SetUpThings(topLevel);

    bitmap_stuff.showNormalBitmap = XtVaCreateManagedWidget("showNormalBitmap", 
		coreWidgetClass, buttonbox, 
		XtNwidth, bitmap_stuff.pixmap_width_in_cells,
		XtNheight, bitmap_stuff.pixmap_height_in_cells,
		XtNtranslations, XtParseTranslationTable(trans),
		NULL);

    bitmap_stuff.showReverseBitmap = XtVaCreateManagedWidget("showReverseBitmap", 
		coreWidgetClass, buttonbox, 
		XtNwidth, bitmap_stuff.pixmap_width_in_cells,
		XtNheight, bitmap_stuff.pixmap_height_in_cells,
		XtNtranslations, XtParseTranslationTable(trans),
		NULL);

    XtRealizeWidget(topLevel);
	XtAppMainLoop(app_context);
}

SetUpThings(w)
Widget w;
{
	XGCValues values;

	bitmap_stuff.normal_bitmap = XCreatePixmap(XtDisplay(w), 
	    RootWindowOfScreen(XtScreen(w)),
	    bitmap_stuff.pixmap_width_in_cells, bitmap_stuff.pixmap_height_in_cells, 1);

	bitmap_stuff.reverse_bitmap = XCreatePixmap(XtDisplay(w), 
	    RootWindowOfScreen(XtScreen(w)),
	    bitmap_stuff.pixmap_width_in_cells, bitmap_stuff.pixmap_height_in_cells, 1);

	values.foreground = 1;
	values.background = 0;
	/* note that normal_bitmap is used as the drawable because it
	 * is one bit deep.  The root window may not be one bit deep */
	bitmap_stuff.draw_gc = XCreateGC(XtDisplay(w),  bitmap_stuff.normal_bitmap,
	        GCForeground | GCBackground, &values);

	values.foreground = 0;
	values.background = 1;
	bitmap_stuff.undraw_gc = XCreateGC(XtDisplay(w), bitmap_stuff.normal_bitmap,
	        GCForeground | GCBackground, &values);

	XFillRectangle(XtDisplay(w), bitmap_stuff.reverse_bitmap, bitmap_stuff.draw_gc,
   	    	0, 0, bitmap_stuff.pixmap_width_in_cells + 1, bitmap_stuff.pixmap_height_in_cells + 1);
	XFillRectangle(XtDisplay(w), bitmap_stuff.normal_bitmap, bitmap_stuff.undraw_gc,
   	    	0, 0, bitmap_stuff.pixmap_width_in_cells + 1, bitmap_stuff.pixmap_height_in_cells + 1);
}

/* ARGSUSED */
static void
CellToggled(w, client_data, call_data)
Widget w;
XtPointer client_data;	/* unused */
XtPointer call_data;	/* will be cast to cur_info */
{
    /* cast pointer to needed type: */
    BitmapEditPointInfo *cur_info = (BitmapEditPointInfo *) call_data;
    /* 
     * Note, BitmapEditPointInfo is defined in BitmapEdit.h 
     */

    XDrawPoint(XtDisplay(w), bitmap_stuff.normal_bitmap, ((cur_info->mode == DRAWN) ? bitmap_stuff.draw_gc : bitmap_stuff.undraw_gc), cur_info->newx, cur_info->newy);
    XDrawPoint(XtDisplay(w), bitmap_stuff.reverse_bitmap, ((cur_info->mode == DRAWN) ? bitmap_stuff.undraw_gc : bitmap_stuff.draw_gc), cur_info->newx, cur_info->newy); 

    RedrawSmallPicture(bitmap_stuff.showNormalBitmap);
    RedrawSmallPicture(bitmap_stuff.showReverseBitmap);
}
