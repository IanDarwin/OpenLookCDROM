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
 * xscrollbox.c -- a sample application that uses ScrollBox widget
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Paned.h>	/* now V or H paned */
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Scrollbar.h>

#include "../ch04/BitmapEdit.h"
#include "ScrollBox.h"

#include <stdio.h>

#define DRAWN 1
#define UNDRAWN 0

#define MAXSIZE 300
#define BUTTONBOXWIDTH 300

Widget bigBitmap, buttonbox;

Dimension pixmap_width_in_cells, pixmap_height_in_cells;
Dimension pixmap_width_in_pixels, pixmap_height_in_pixels;
Dimension window_width, window_height;

/* ARGSUSED */
static void 
Printout(widget)
Widget widget;
{
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
    Widget topLevel, vpane;

    static XrmOptionDescRec table[] = 
    {
        {"-pw",            "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
        {"-pixmapwidth",   "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
        {"-ph",            "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
        {"-pixmapheight",  "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
        {"-c",             "*cellSizeInPixels",           XrmoptionSepArg, NULL},
        {"-cellsize",      "*cellSizeInPixels",           XrmoptionSepArg, NULL},
    };
    
        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XScrollBox",       /* Application class */
        table, XtNumber(table),   /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    vpane = XtCreateManagedWidget("vpane", panedWidgetClass, topLevel, NULL, 0);

    create_buttonbox(vpane);

    create_viewport(vpane, app_context);

    XtRealizeWidget(topLevel);
	XtAppMainLoop(app_context);
}

/* ARGSUSED */
static void
Resize_thumbs(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
	String *orientation = (String *) params;
    Dimension width, height;
    int cur_x, cur_y;

    XtVaGetValues(bigBitmap, 
			XtNheight, &height,
			XtNwidth, &width,
			XtNcurX, &cur_x,
			XtNcurY, &cur_y,
			NULL);

    if (*orientation[0] == 'h')
        XawScrollbarSetThumb(w,
				(float)cur_x/pixmap_width_in_pixels,
			        (float)width/pixmap_width_in_pixels);
    else 
        XawScrollbarSetThumb(w,
				(float)cur_y/pixmap_height_in_pixels,
			        (float)height/pixmap_height_in_pixels);
}

/* ARGSUSED */
static void
Scroll_up_down(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;
{
	int pixels = (int) call_data;
    Dimension height;
    int cur_y;

    XtVaGetValues(bigBitmap, 
			XtNheight, &height,
			XtNcurY, &cur_y,
			NULL);

    /* When pixels is negative, right button pressed, move data down,
     * thumb moves up.  Otherwise, left button pressed, pixels 
     * positive, move data up, thumb down. 
     */
    cur_y += pixels;

    /* limit panning to size of bitmap */
    if (cur_y < 0)
	cur_y = 0;
    else if (cur_y > pixmap_height_in_pixels - height )
	cur_y = pixmap_height_in_pixels - height;

    XtVaSetValues(bigBitmap, 
			XtNcurY, cur_y,
			NULL);
    
    XawScrollbarSetThumb(w,
			(float)cur_y/pixmap_height_in_pixels,
			(float)height/pixmap_height_in_pixels);
}

/* ARGSUSED */
static void
Scroll_left_right(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;
{
	int pixels = (int) call_data;
    Dimension width;
    int cur_x;

    XtVaGetValues(bigBitmap, 
			XtNwidth, &width,
			XtNcurX, &cur_x,
			NULL);

    cur_x += pixels;

    /* limit panning to size of bitmap */
    if (cur_x < 0)
	cur_x = 0;
    else if (cur_x > pixmap_width_in_pixels - width)
	cur_x = pixmap_width_in_pixels - width;

    XtVaSetValues(bigBitmap, 
			XtNcurX, cur_x,
			NULL);

    XawScrollbarSetThumb(w,
			(float)cur_x/pixmap_width_in_pixels,
			(float) width/pixmap_width_in_pixels);
}

/* ARGSUSED */
static void
Thumb_up_down(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;  /* cast to float */
{
    float percent = *(float *) call_data;
    Dimension height;
    int cur_y;

    XtVaGetValues(bigBitmap, 
			XtNheight, &height,
			XtNcurY, &cur_y,
			NULL);

    if ((cur_y = (int)(pixmap_height_in_pixels * percent)) >=
		       pixmap_height_in_pixels - height)
	cur_y = pixmap_height_in_pixels - height;

    XtVaSetValues(bigBitmap, 
			XtNcurY, cur_y,
			NULL);

    XawScrollbarSetThumb(w,
				(float)cur_y/pixmap_height_in_pixels,
				(float)height/pixmap_height_in_pixels);
}

/* ARGSUSED */
static void
Thumb_left_right(w, unused, call_data)
Widget w;
XtPointer unused;
XtPointer call_data;  /* cast to float */
{
    float percent = *(float *) call_data;
    Dimension width;
    int cur_x;

    XtVaGetValues(bigBitmap, 
			XtNwidth, &width,
			XtNcurX, &cur_x,
			NULL);

    if ((cur_x = (int)(pixmap_width_in_pixels * percent)) >=
		       pixmap_width_in_pixels - width)
	cur_x = pixmap_width_in_pixels - width;

    XtVaSetValues(bigBitmap, 
			XtNcurX, cur_x,
			NULL);

    XawScrollbarSetThumb(w,
			(float)cur_x/pixmap_width_in_pixels,
			(float)width/pixmap_width_in_pixels);
}

create_buttonbox(parent)
Widget parent;
{
    Widget output, quit;
    extern void exit();

    /* 
     * Setup buttonbox with four children
     */
    buttonbox = XtVaCreateManagedWidget("buttonbox", boxWidgetClass, 
			parent, 
			XtNwidth, BUTTONBOXWIDTH,
			NULL);

    output = XtCreateManagedWidget("output", commandWidgetClass, buttonbox, NULL, 0);
    XtAddCallback(output, XtNcallback, Printout, NULL);

    quit = XtCreateManagedWidget("quit", commandWidgetClass, buttonbox, NULL, 0);
    XtAddCallback(quit, XtNcallback, exit, NULL);

}

create_viewport(parent, app_context)
Widget parent;
XtAppContext app_context;
{
    Widget scrollHoriz, scrollVert, scrollBox;
    int cell_size;
    static XtActionsRec window_actions[] = {
		{"resize_thumbs", Resize_thumbs}
	};

    XtAppAddActions(app_context, window_actions, 1);

    scrollBox = XtCreateManagedWidget("scrollBox", scrollBoxWidgetClass, parent, NULL, 0);

    bigBitmap = XtCreateManagedWidget("bigBitmap", bitmapEditWidgetClass, scrollBox, NULL, 0);

    XtVaGetValues(bigBitmap, 
			XtNpixmapHeightInCells, &pixmap_height_in_cells,
			XtNpixmapWidthInCells, &pixmap_width_in_cells,
			XtNcellSizeInPixels, &cell_size,
			NULL);

    pixmap_height_in_pixels = pixmap_height_in_cells * cell_size;
    pixmap_width_in_pixels = pixmap_width_in_cells * cell_size;

    window_width = ((pixmap_width_in_pixels > MAXSIZE) ? MAXSIZE : pixmap_width_in_pixels);
    window_height = ((pixmap_height_in_pixels > MAXSIZE) ? MAXSIZE : pixmap_height_in_pixels);

    XtVaSetValues(bigBitmap,
			XtNwidth, window_width,
			XtNheight, window_height,
			NULL);

    scrollVert = XtVaCreateManagedWidget("scrollVert", scrollbarWidgetClass,
			scrollBox, 
			XtNorientation, XtorientVertical,
			XtNheight, window_height,
			XtNwidth, 15,
			NULL);

    XtAddCallback(scrollVert, XtNscrollProc, Scroll_up_down, bigBitmap);
    XtAddCallback(scrollVert, XtNjumpProc, Thumb_up_down, bigBitmap);

    XtOverrideTranslations(scrollVert,
		XtParseTranslationTable("<Configure>: resize_thumbs(v)\n\
				<Expose>: resize_thumbs(v)"));

    scrollHoriz = XtVaCreateManagedWidget("scrollHoriz", 
			scrollbarWidgetClass, scrollBox, 
			XtNorientation, XtorientHorizontal,
			XtNwidth, window_width,
			XtNheight, 15,
			NULL);

    XtAddCallback(scrollHoriz, XtNscrollProc, Scroll_left_right, bigBitmap);
    XtAddCallback(scrollHoriz, XtNjumpProc, Thumb_left_right, bigBitmap);
    XtOverrideTranslations(scrollHoriz,
		XtParseTranslationTable("<Configure>: resize_thumbs(h)\n\
				<Expose>: resize_thumbs(h)"));
}
