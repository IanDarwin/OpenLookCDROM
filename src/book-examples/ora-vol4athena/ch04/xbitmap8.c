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
 * xbitmap8.c
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

GC draw_gc, undraw_gc, invert_gc;
Pixmap normal_bitmap;
Widget bigBitmap, showNormalBitmap, showReverseBitmap;
Dimension pixmap_width_in_cells, pixmap_height_in_cells;

static void cell_toggled();

String filename;	/* filename to read and write */
static Boolean file_contained_good_data = False;

/* ARGSUSED */
static void 
Printout(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;	/* unused */
{
	XWriteBitmapFile(XtDisplay(widget), filename, normal_bitmap,
			 pixmap_width_in_cells, pixmap_height_in_cells, 0, 0);
}


/*ARGSUSED*/
static void
Redraw_small_picture(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
	GC gc;

	if (w == showNormalBitmap)
		gc = DefaultGCOfScreen(XtScreen(w));
	else
		gc = invert_gc;

	if (DefaultDepthOfScreen(XtScreen(w)) == 1)
		XCopyArea(XtDisplay(w), normal_bitmap, XtWindow(w),
				 gc, 0, 0, pixmap_width_in_cells,  pixmap_height_in_cells,
				 0, 0);
	else
		XCopyPlane(XtDisplay(w), normal_bitmap, XtWindow(w),
				 gc, 0, 0, pixmap_width_in_cells,  pixmap_height_in_cells,
				 0, 0, 1);
}


String
FillCell(w)
Widget w;
{
	String cell;
	int x, y;
	XImage * image;
	cell = XtCalloc(pixmap_width_in_cells * pixmap_height_in_cells,
			 sizeof(char));
	/* Convert pixmap into image, so that we can 
	 * read indiviual pixels */
	image = XGetImage(XtDisplay(w), normal_bitmap, 0, 0, 
			 pixmap_width_in_cells, pixmap_height_in_cells, AllPlanes,
			 XYPixmap);

	for (x = 0; x < pixmap_width_in_cells; x++) {
		for (y = 0; y < pixmap_height_in_cells; y++) {
			cell[x + (y * pixmap_width_in_cells)] =
					 XGetPixel(image, x, y);
		}
	}
	return(cell);
}


main(argc, argv)
int argc;
char *argv[];
{
	XtAppContext app_context;
	Widget topLevel, form, buttonbox, quit, output;
	Arg args[5];
	int i;
	extern void exit();
	unsigned int width, height;	/* NOT Dimension: used in Xlib calls */
	int junk;
	String cell;
	static XtActionsRec window_actions[] = {
		{ "redraw_small_picture", Redraw_small_picture }
	};    


	String trans =  "<Expose>:	redraw_small_picture()";

	static XrmOptionDescRec table[] = 
	 {
		{ "-pw",            "*pixmapWidthInCells",        XrmoptionSepArg,
				 NULL },
		{ "-pixmapwidth",   "*pixmapWidthInCells",        XrmoptionSepArg,
				 NULL },
		{ "-ph",            "*pixmapHeightInCells",       XrmoptionSepArg,
				 NULL },
		{ "-pixmapheight",  "*pixmapHeightInCells",       XrmoptionSepArg,
				 NULL },
		{ "-cellsize",      "*cellSizeInPixels",           XrmoptionSepArg,
				 NULL },

	};

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

	topLevel = XtVaAppInitialize( &app_context,       /* Application context */
			"XBitmap8",         /* Application class */
			table, XtNumber(table),  /* command line option list */
			&argc, argv,        /* command line args */
			NULL,               /* for missing app-defaults file */
			NULL);              /* terminate varargs list */

	if (argv[1] != NULL)
		filename = argv[1];
	else {
		fprintf(stderr, "xbitmap: must specify filename on command line.\n");
		exit(1);
	}

	form = XtCreateManagedWidget("form", formWidgetClass,
			 topLevel, NULL, 0);

	buttonbox = XtCreateManagedWidget("buttonbox", boxWidgetClass,
			 form, NULL, 0);

	output = XtCreateManagedWidget("output", commandWidgetClass,
			 buttonbox, NULL, 0);

	XtAddCallback(output, XtNcallback, Printout, NULL);

	quit = XtCreateManagedWidget("quit", commandWidgetClass,
			 buttonbox, NULL, 0);

	XtAddCallback(quit, XtNcallback, exit, (XtPointer) NULL);

	XtAppAddActions(app_context, window_actions, XtNumber(window_actions));

	switch (XReadBitmapFile(XtDisplay(quit), RootWindowOfScreen(XtScreen(quit)),
			 filename, &width, &height, &normal_bitmap, &junk, &junk)) {
	case BitmapSuccess:
		file_contained_good_data = True;
		if ((pixmap_width_in_cells != width) ||  (pixmap_height_in_cells !=
				 height)) {
			fprintf(stderr, "xbitmap: bitmap file dimensions do not match resource database, ignoring database.\n");
			i = 0;
			XtSetArg(args[i], XtNpixmapWidthInCells, width);   i++;
			XtSetArg(args[i], XtNpixmapHeightInCells, height);   i++;
			pixmap_width_in_cells = width;
			pixmap_height_in_cells = height;
			cell = FillCell(quit);
			XtSetArg(args[i], XtNcellArray, cell);   
			i++;
		}
		break;
	case BitmapOpenFailed:
		fprintf(stderr, "xbitmap: could not open bitmap file, using fresh bitmap.\n");
		file_contained_good_data = False;
		i = 0;
		break;
	case BitmapFileInvalid:
		fprintf(stderr, "xbitmap: bitmap file invalid.\n");
		exit(1);
	case BitmapNoMemory:
		fprintf(stderr, "xbitmap: insufficient server memory to create bitmap.\n");
		exit(1);
	default:
		fprintf(stderr, "xbitmap: programming error.\n");
		exit(1);
	}

	/* args are set in if and switch above if file was read */
	bigBitmap = XtCreateManagedWidget("bigBitmap",  bitmapEditWidgetClass,
			 form, args, i);

	XtAddCallback(bigBitmap, XtNcallback, cell_toggled, NULL);

	if (!file_contained_good_data) {
		XtVaGetValues(bigBitmap,  
				XtNpixmapHeightInCells, &pixmap_height_in_cells, 
				XtNpixmapWidthInCells, &pixmap_width_in_cells, NULL);

		normal_bitmap = XCreatePixmap(XtDisplay(quit),
				RootWindowOfScreen(XtScreen(quit)), 
				pixmap_width_in_cells,
				pixmap_height_in_cells, 1);

	}

	set_up_things(topLevel);

	if (!file_contained_good_data) {
		XFillRectangle(XtDisplay(quit),
				normal_bitmap, 
				undraw_gc, 0, 0, 10000, 10000);
	}

	showNormalBitmap = XtVaCreateManagedWidget("showNormalBitmap",
			coreWidgetClass, buttonbox,  
			XtNwidth, pixmap_width_in_cells,
			XtNheight, pixmap_height_in_cells, 
			XtNtranslations, XtParseTranslationTable(trans),
			NULL);

	showReverseBitmap = XtVaCreateManagedWidget("showReverseBitmap",
			coreWidgetClass, buttonbox,  
			XtNwidth, pixmap_width_in_cells,
			XtNheight, pixmap_height_in_cells, 
			XtNtranslations, XtParseTranslationTable(trans),
			NULL);

	XtRealizeWidget(topLevel);
	XtAppMainLoop(app_context);
}


set_up_things(w)
Widget w;
{
	XGCValues values;

	values.foreground = 1;
	values.background = 0;

	/* note that normal_bitmap is used as the drawable because it
	 * is one bit deep.  The root window may not be one bit deep. */
	draw_gc = XCreateGC(XtDisplay(w), normal_bitmap, GCForeground |
			 GCBackground, &values);

	values.foreground = 0;
	values.background = 1;
	undraw_gc = XCreateGC(XtDisplay(w), normal_bitmap, GCForeground |
			 GCBackground, &values);

	/* this GC is for copying from the bitmap
	 * to the small reverse widget */
	values.foreground = WhitePixelOfScreen(XtScreen(w));
	values.background = BlackPixelOfScreen(XtScreen(w));
	invert_gc = XtGetGC(w, GCForeground | GCBackground, &values);
}


/* ARGSUSED */
static void
cell_toggled(w, client_data, info)
Widget w;
XtPointer client_data;	/* unused */
XtPointer info;	/* call_data (from widget) */
{
	BitmapEditPointInfo * cur_info = (BitmapEditPointInfo *
			 ) info;
	/* 
	 * Note: BitmapEditPointInfo is defined in BitmapEdit.h 
	 */

	XDrawPoint(XtDisplay(w), normal_bitmap, ((cur_info->mode ==
			 DRAWN) ? draw_gc : undraw_gc), cur_info->newx, cur_info->newy);

	Redraw_small_picture(showNormalBitmap);
	Redraw_small_picture(showReverseBitmap);
}


