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
 * xbitmap6.c
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

#include "BitmapEdit.h"

#include <stdio.h>

#define DRAWN 1
#define UNDRAWN 0

GC draw_gc, undraw_gc;
Pixmap normal_bitmap, reverse_bitmap;
Widget bigBitmap, showNormalBitmap, showReverseBitmap;
Dimension pixmap_width_in_cells, pixmap_height_in_cells;

static void cell_toggled();

/* ARGSUSED */
static void 
Printout(widget, client_data, call_data)
Widget widget;
XtPointer client_data;	/* cast to bigBitmap */
XtPointer call_data;	/* unused */
{
	Widget bigBitmap = (Widget) client_data;
    int x, y;
    char *cell;
    cell = BitmapEditGetArrayString(bigBitmap);

    (void) putchar('\n');
    for (y = 0; y < pixmap_height_in_cells; y++) {
	for (x = 0; x < pixmap_width_in_cells; x++)
	    (void) putchar(cell[x + y * pixmap_width_in_cells] ? '1' : '0');
	(void) putchar('\n');
    }
    (void) putchar('\n');
}

static void
redraw_small_picture(w)
Widget w;
{
	/* Tell Label widget to redraw itself */
 	XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 10000, 10000, True);


	/* or, to eliminate flashing, send synthetic Expose event: 
	XExposeEvent event;
	event.type = Expose;
	event.serial = 0;
	event.send_event = True;
	event.display = XtDisplay(w);
	event.window = XtWindow(w);
	event.x = 0;
	event.y = 0;
	event.width = 1000;
	event.height = 1000;
	event.count = 0;

	XSendEvent(XtDisplay(w), XtWindow(w), True, ExposureMask, &event);
	*/
}

main(argc, argv)
int argc;
char *argv[];
{
	XtAppContext app_context;
    Widget topLevel, form, buttonbox, quit, output;
    extern void exit();

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
        "XBitmap6",         /* Application class */
        table, XtNumber(table),   /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    form = XtCreateManagedWidget("form", formWidgetClass, topLevel, NULL, 0);

    buttonbox = XtCreateManagedWidget("buttonbox", boxWidgetClass, form, NULL, 0);

    output = XtCreateManagedWidget("output", commandWidgetClass, buttonbox, NULL, 0);

    quit = XtCreateManagedWidget("quit", commandWidgetClass, buttonbox, NULL, 0);

    XtAddCallback(quit, XtNcallback, exit, NULL);

    bigBitmap = XtCreateManagedWidget("bigBitmap", bitmapEditWidgetClass, 
			form, NULL, 0);

    XtAddCallback(output, XtNcallback, Printout, bigBitmap);

    XtAddCallback(bigBitmap, XtNcallback, cell_toggled, NULL);

    XtVaGetValues(bigBitmap, 
			XtNpixmapHeightInCells, &pixmap_height_in_cells,
			XtNpixmapWidthInCells, &pixmap_width_in_cells,
			NULL);

    set_up_things(topLevel);

    showNormalBitmap = XtVaCreateManagedWidget("showNormalBitmap", 
			labelWidgetClass, buttonbox, 
			XtNbitmap, normal_bitmap,
			NULL);

    showReverseBitmap = XtVaCreateManagedWidget("showReverseBitmap", 
			labelWidgetClass, buttonbox, 
			XtNbitmap, reverse_bitmap,
			NULL);

    XtRealizeWidget(topLevel);
	XtAppMainLoop(app_context);
}

set_up_things(w)
Widget w;
{
	XGCValues values;

	normal_bitmap = XCreatePixmap(XtDisplay(w), 
	    RootWindowOfScreen(XtScreen(w)),
	    pixmap_width_in_cells, pixmap_height_in_cells, 1);

	reverse_bitmap = XCreatePixmap(XtDisplay(w), 
	    RootWindowOfScreen(XtScreen(w)),
	    pixmap_width_in_cells, pixmap_height_in_cells, 1);

	values.foreground = 1;
	values.background = 0;
	/* note that normal_bitmap is used as the drawable because it
	 * is one bit deep.  The root window may not be one bit deep */
	draw_gc = XCreateGC(XtDisplay(w),  normal_bitmap,
	        GCForeground | GCBackground, &values);

	XFillRectangle(XtDisplay(w), normal_bitmap, draw_gc,
   	    	0, 0, pixmap_width_in_cells + 1, pixmap_height_in_cells + 1);

	values.foreground = 0;
	values.background = 1;
	undraw_gc = XCreateGC(XtDisplay(w), reverse_bitmap,
	        GCForeground | GCBackground, &values);

	XFillRectangle(XtDisplay(w), reverse_bitmap, undraw_gc,
   	    	0, 0, pixmap_width_in_cells + 1, pixmap_height_in_cells + 1);
}

/* ARGSUSED */
static void
cell_toggled(w, client_data, info)
Widget w;
XtPointer client_data;   /* unused */
XtPointer info;
{
    BitmapEditPointInfo *cur_info = (BitmapEditPointInfo *) info;
	/* 
	 * Note, BitmapEditPointInfo is defined in BitmapEdit.h 
	 */

    XDrawPoint(XtDisplay(w), normal_bitmap, ((cur_info->mode == UNDRAWN) ? draw_gc : undraw_gc), cur_info->newx, cur_info->newy);
    XDrawPoint(XtDisplay(w), reverse_bitmap, ((cur_info->mode == UNDRAWN) ? undraw_gc : draw_gc), cur_info->newx, cur_info->newy); 

    redraw_small_picture(showNormalBitmap);
    redraw_small_picture(showReverseBitmap);
}
