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
 * xselectbit.c
 */
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>

#include "BitmapEdit.h"

Dimension pixmap_width_in_cells, pixmap_height_in_cells;

/*
 * The Printout routine prints an array of 1s and 0s representing the
 * contents of the bitmap.  This data can be processed into any
 * desired form, including standard X11 bitmap file format.
 */
/* ARGSUSED */
static void 
Printout(widget, client_data, call_data)
Widget widget;
XtPointer client_data;   /* cast to bigBitmap */
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

main(argc, argv)
int argc;
char *argv[];
{
	XtAppContext app_context;
	Widget topLevel, form, buttonbox, quit, output, bigBitmap;

	extern void exit();

	static XrmOptionDescRec table[] = {
		{"-pw",            "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
		{"-pixmapwidth",   "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
		{"-ph",            "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
		{"-pixmapheight",  "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
		{"-cellsize",      "*cellSizeInPixels",           XrmoptionSepArg, NULL},
	};
    

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XSelectbit",         /* Application class */
        table, XtNumber(table),  /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

	form = XtCreateManagedWidget("form", formWidgetClass, topLevel, NULL, 0);

	buttonbox = XtCreateManagedWidget("buttonbox", boxWidgetClass, form, NULL, 0);

	output = XtCreateManagedWidget("output", commandWidgetClass, buttonbox, NULL, 0);

	/* output callback added below, after bitmap widget created */

	quit = XtCreateManagedWidget("quit", commandWidgetClass, buttonbox, NULL, 0);

	XtAddCallback(quit, XtNcallback, exit, NULL);

	bigBitmap = XtCreateManagedWidget("bigBitmap", bitmapEditWidgetClass, form, NULL, 0);

	XtAddCallback(output, XtNcallback, Printout, bigBitmap);

	/* need the following values for the Printout routine. */
	XtVaGetValues(bigBitmap, 
			XtNpixmapWidthInCells, &pixmap_width_in_cells,
			XtNpixmapHeightInCells, &pixmap_height_in_cells,
			NULL);

	XtRealizeWidget(topLevel);
	XtAppMainLoop(app_context);
}
