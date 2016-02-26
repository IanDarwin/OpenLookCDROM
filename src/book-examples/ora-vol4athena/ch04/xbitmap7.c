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
 * xbitmap7.c
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Paned.h>	/* now V or H paned */
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Scrollbar.h>

#include "BitmapEdit.h"

#include <stdio.h>

#define DRAWN 1
#define UNDRAWN 0
#define MAXSIZE 300
#define BUTTONBOXWIDTH 300

GC draw_gc, undraw_gc;

Pixmap normal_bitmap, reverse_bitmap;

Widget bigBitmap, showNormalBitmap, showReverseBitmap, buttonbox, showNormalBitmap, showReverseBitmap;

Dimension pixmap_width_in_cells, pixmap_height_in_cells;
Dimension pixmap_width_in_pixels, pixmap_height_in_pixels;
Dimension window_width, window_height;

int cell_size;

static void cell_toggled();

/* ARGSUSED */
static void 
Printout(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
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

/*ARGSUSED*/
static void
Redraw_small_picture(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
	Pixmap pixmap;

	if (w == showNormalBitmap)
		pixmap = normal_bitmap;
	else
		pixmap = reverse_bitmap;

	if (DefaultDepthOfScreen(XtScreen(w)) == 1)
		XCopyArea(XtDisplay(w), pixmap, XtWindow(w),
				DefaultGCOfScreen(XtScreen(w)), 0, 0, 
				pixmap_width_in_cells, pixmap_height_in_cells, 0, 0);
	else
		XCopyPlane(XtDisplay(w), pixmap, XtWindow(w),
				DefaultGCOfScreen(XtScreen(w)), 0, 0, 
				pixmap_width_in_cells, pixmap_height_in_cells, 0, 0, 1);
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
        {"-cellsize",      "*cellSizeInPixels",           XrmoptionSepArg, NULL},
    };
    

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XBitmap7",         /* Application class */
        table, XtNumber(table),  /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    vpane = XtCreateManagedWidget("vpane", panedWidgetClass, topLevel, NULL, 0);

    create_viewport(vpane, app_context);

    create_buttonbox(vpane, app_context);

    create_gcs_and_pixmaps(topLevel);

    XtRealizeWidget(topLevel);
	XtAppMainLoop(app_context);
}

/* ARGSUSED */
static void
Resize_scrollbars(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
	String *orientation = (String *) params;
    Dimension width, height;
    int cur_x, cur_y;

    XtVaGetValues(w, 
			XtNheight, &height,
			XtNwidth, &width,
			NULL);

    XtVaGetValues(bigBitmap, 
			XtNcurX, &cur_x,
			XtNcurY, &cur_y,
			NULL);

    if (*orientation[0] == 'h')
        XawScrollbarSetThumb(w, (float)cur_x/(pixmap_width_in_pixels),
			       (float)width/(pixmap_width_in_pixels));
    else 
        XawScrollbarSetThumb(w, (float)cur_y/(pixmap_height_in_pixels),
			       (float)height/(pixmap_height_in_pixels));
}

create_gcs_and_pixmaps(w)
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
	draw_gc = XCreateGC(XtDisplay(w),  normal_bitmap,
	        GCForeground | GCBackground, &values);

	values.background = 1;
	values.foreground = 0;
	undraw_gc = XCreateGC(XtDisplay(w), normal_bitmap,
	        GCForeground | GCBackground, &values);

	XFillRectangle(XtDisplay(w), normal_bitmap, draw_gc,
   	    	0, 0, pixmap_width_in_cells, pixmap_height_in_cells);

	XFillRectangle(XtDisplay(w), reverse_bitmap, undraw_gc,
   	    	0, 0, pixmap_width_in_cells, pixmap_height_in_cells);
}

/* ARGSUSED */
static void
scroll_up_down(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;  /* cast to pixels */
{
	int pixels = (int) call_data;
    Dimension height;
    int cur_y;

    XtVaGetValues(w, 
			XtNheight, &height,
			NULL);

    XtVaGetValues(bigBitmap, 
			XtNcurY, &cur_y,
			NULL);

    /* When pixels is negative, right button pressed, move data down,
     * thumb moves up.  Otherwise, left button pressed, pixels 
     * positive, move data up,
     * thumb down. 
     */
    cur_y += pixels;

    if (cur_y < 0)
	cur_y = 0;
    else if (cur_y > pixmap_height_in_pixels - height )
	cur_y = pixmap_height_in_pixels - height;

    XtVaSetValues(bigBitmap, 
			XtNcurY, cur_y,
			NULL);
    
    XawScrollbarSetThumb(w, (float)cur_y/pixmap_height_in_pixels,
			   (float)height/pixmap_height_in_pixels);
}

/* ARGSUSED */
static void
scroll_left_right(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;  /* cast to pixels */
{
	int pixels = (int) call_data;
    Dimension width;
    int cur_x;

    XtVaGetValues(w, 
			XtNwidth, &width,
			NULL);

    XtVaGetValues(bigBitmap, 
			XtNcurX, &cur_x,
			NULL);

    cur_x += pixels;

    if (cur_x < 0)
	cur_x = 0;
    else if (cur_x > pixmap_width_in_pixels - width)
	cur_x = pixmap_width_in_pixels - width;

    XtVaSetValues(bigBitmap, 
			XtNcurX, cur_x,
			NULL);

    XawScrollbarSetThumb(w, (float)cur_x/pixmap_width_in_pixels,
			   (float) width/pixmap_width_in_pixels);
}

/* ARGSUSED */
static void
jump_up_down(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;
{
	int percent = (int) call_data;
    Dimension height;
    int cur_y;

    XtVaGetValues(w, 
			XtNheight, &height,
			NULL);

    XtVaGetValues(bigBitmap, 
			XtNcurY, &cur_y,
			NULL);

    if ((cur_y = (int)(pixmap_height_in_pixels * percent)) >=
		       pixmap_height_in_pixels - height)
	cur_y = pixmap_height_in_pixels - height;

    XtVaSetValues(bigBitmap, 
			XtNcurY, cur_y,
			NULL);

    XawScrollbarSetThumb(w, (float)cur_y/pixmap_height_in_pixels,
			   (float)height/pixmap_height_in_pixels);
}

/* ARGSUSED */
static void
jump_left_right(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;  /* cast to percent */
{
	int percent = (int) call_data;
    Dimension width;
    int cur_x;

    XtVaGetValues(w, 
			XtNwidth, &width,
			NULL);

    XtVaGetValues(bigBitmap, 
			XtNcurX, &cur_x,
			NULL);

    if ((cur_x = (int)(pixmap_width_in_pixels * percent)) >=
		       pixmap_width_in_pixels - width)
	cur_x = pixmap_width_in_pixels - width;

    XtVaSetValues(bigBitmap, 
			XtNcurX, cur_x,
			NULL);

    XawScrollbarSetThumb(w, (float)cur_x/pixmap_width_in_pixels,
			   (float)width/pixmap_width_in_pixels);
}

/* ARGSUSED */
static void
cell_toggled(w, client_data, info)
Widget w;
XtPointer client_data;  /* unused */
XtPointer info;  /* call_data */
{
    BitmapEditPointInfo *cur_info = (BitmapEditPointInfo *) info;

    XDrawPoint(XtDisplay(w), normal_bitmap, ((cur_info->mode == UNDRAWN) ? draw_gc : undraw_gc), cur_info->newx, cur_info->newy);
    XDrawPoint(XtDisplay(w), reverse_bitmap, ((cur_info->mode == UNDRAWN) ? undraw_gc : draw_gc), cur_info->newx, cur_info->newy); 

    Redraw_small_picture(showNormalBitmap);
    Redraw_small_picture(showReverseBitmap);
}

create_buttonbox(parent, app_context)
Widget parent;
XtAppContext app_context;
{
    Widget output, quit;
    extern void exit();
    XtActionsRec window_actions[2];
    String trans =
	"<Expose>:	redraw_small_picture()";
    /* 
     * Setup buttonbox with four children
     */
    buttonbox = XtVaCreateManagedWidget("buttonbox", boxWidgetClass, parent, 
			XtNwidth, BUTTONBOXWIDTH,
			NULL);

    output = XtCreateManagedWidget("output", commandWidgetClass, buttonbox, NULL, 0);
    XtAddCallback(output, XtNcallback, Printout, NULL);

    quit = XtCreateManagedWidget("quit", commandWidgetClass, buttonbox, NULL, 0);
    XtAddCallback(quit, XtNcallback, exit, NULL);

    showNormalBitmap = XtVaCreateManagedWidget("showNormalBitmap", coreWidgetClass, buttonbox, 
			XtNwidth,	pixmap_width_in_cells,
			XtNheight, pixmap_height_in_cells,
			XtNtranslations, XtParseTranslationTable(trans),
			NULL);

    showReverseBitmap = XtVaCreateManagedWidget("showReverseBitmap", coreWidgetClass, buttonbox, 
			XtNwidth, pixmap_width_in_cells,
			XtNheight, pixmap_height_in_cells,
			XtNtranslations, XtParseTranslationTable(trans),
			NULL);

    /* exposure action for showNormalBitmap and showReverseBitmap */
    window_actions[0].proc = Redraw_small_picture;
    window_actions[0].string = "redraw_small_picture";
    XtAppAddActions(app_context, window_actions, 2);
}

create_viewport(parent, app_context)
Widget parent;
XtAppContext app_context;
{
    Widget scrollHoriz, scrollVert, drawingForm;
    static XtActionsRec window_actions[] = {
		{"resize_scrollbars", Resize_scrollbars}
	};

    XtAppAddActions(app_context, window_actions, 1);

    drawingForm = XtCreateManagedWidget("drawingForm", formWidgetClass, parent, NULL, 0);

    bigBitmap = XtCreateManagedWidget("bigBitmap", bitmapEditWidgetClass, drawingForm, NULL, 0);

    XtAddCallback(bigBitmap, XtNcallback, cell_toggled, NULL);

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

    scrollVert = XtVaCreateManagedWidget("scrollVert", 
			scrollbarWidgetClass, drawingForm, 
			XtNorientation, XtorientVertical,
			XtNheight, window_height,
			XtNwidth, 15,
			XtNshown, window_height/pixmap_height_in_pixels,
			NULL);

    XtAddCallback(scrollVert, XtNscrollProc, scroll_up_down, bigBitmap);
    XtAddCallback(scrollVert, XtNjumpProc, jump_up_down, bigBitmap);

    XtOverrideTranslations(scrollVert,
		XtParseTranslationTable("<Configure>: resize_scrollbars(v)\n\
				<Expose>: resize_scrollbars(v)"));

    scrollHoriz = XtVaCreateManagedWidget("scrollHoriz", 
			scrollbarWidgetClass, drawingForm, 
			XtNorientation, XtorientHorizontal,
			XtNwidth, window_width,
			XtNheight, 15,
			XtNshown, window_height/pixmap_height_in_pixels,
			NULL);

    XtAddCallback(scrollHoriz, XtNscrollProc, scroll_left_right, bigBitmap);
    XtAddCallback(scrollHoriz, XtNjumpProc, jump_left_right, bigBitmap);
    XtOverrideTranslations(scrollHoriz,
		XtParseTranslationTable("<Configure>: resize_scrollbars(h)\n\
				<Expose>: resize_scrollbars(h)"));
}

