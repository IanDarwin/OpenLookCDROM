/*
 * Copyright 1989, 1992 O'Reilly and Associates, Inc.

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
#include <Xm/Xm.h>

#include <Xm/RowColumn.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/ScrollBar.h>

#include "BitmapEdit.h"

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

    vpane = XtCreateManagedWidget("vpane", xmPanedWindowWidgetClass, topLevel, NULL, 0);

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
            XmNheight, &height,
            XmNwidth, &width,
            XtNcurX, &cur_x,
            XtNcurY, &cur_y,
            NULL);

    if (*orientation[0] == 'h')
        XtVaSetValues(w,
                XmNminimum,      0,
                XmNmaximum,      width,
                XmNsliderSize,      width * (int)width/pixmap_width_in_pixels,
                XmNvalue,  cur_x * (int)width/pixmap_width_in_pixels,
                NULL);
    else 
        XtVaSetValues(w,
                XmNminimum,      0,
                XmNmaximum,      height,
                XmNsliderSize,      height * (int)height/pixmap_height_in_pixels,
                XmNvalue,  cur_y * (int)height/pixmap_height_in_pixels,
                NULL);
}

/* ARGSUSED */
static void
Scroll_up_down(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;
{
    XmScrollBarCallbackStruct *sbstruct = (XmScrollBarCallbackStruct *)call_data;
    int position;
    Dimension height;
    int cur_y;

    position = sbstruct->value;

    XtVaGetValues(bigBitmap, 
            XmNheight, &height,
            XtNcurY, &cur_y,
            NULL);

	if (sbstruct->reason == XmCR_PAGE_INCREMENT)
    	cur_y += position;
	else
    	cur_y -= position;

    /* limit panning to size of bitmap */
    if (cur_y < 0)
        cur_y = 0;
    else if (cur_y > pixmap_height_in_pixels - height )
        cur_y = pixmap_height_in_pixels - height;

    XtVaSetValues(bigBitmap, 
            XtNcurY, cur_y,
            NULL);
    
    XtVaSetValues(w,
            XmNvalue,  cur_y * (int)height/pixmap_height_in_pixels,
            NULL);
}

/* ARGSUSED */
static void
Scroll_left_right(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;
{
    XmScrollBarCallbackStruct *sbstruct = (XmScrollBarCallbackStruct *)call_data;
    int position;
    Dimension width;
    int cur_x;

    position = sbstruct->value;

    XtVaGetValues(bigBitmap, 
            XmNwidth, &width,
            XtNcurX, &cur_x,
            NULL);

	if (sbstruct->reason == XmCR_PAGE_INCREMENT)
    	cur_x += position;
	else
    	cur_x -= position;

    /* limit panning to size of bitmap */
    if (cur_x < 0)
        cur_x = 0;
    else if (cur_x > pixmap_width_in_pixels - width)
        cur_x = pixmap_width_in_pixels - width;

    XtVaSetValues(bigBitmap, 
            XtNcurX, cur_x,
            NULL);

    XtVaSetValues(w,
           XmNvalue,  cur_x * (int)width/pixmap_width_in_pixels,
           NULL);
}

/* ARGSUSED */
create_buttonbox(parent)
Widget parent;
{
    Widget output, quit;
    extern exit();

    /* 
     * Setup buttonbox with four children
     */
    buttonbox = XtVaCreateManagedWidget("buttonbox", 
            xmRowColumnWidgetClass, parent, 
            XmNwidth, BUTTONBOXWIDTH,
            NULL);

    output = XtCreateManagedWidget("output", xmPushButtonWidgetClass, buttonbox, NULL, 0);
    XtAddCallback(output, XmNactivateCallback, Printout, NULL);

    quit = XtCreateManagedWidget("quit", xmPushButtonWidgetClass, buttonbox, NULL, 0);
    XtAddCallback(quit, XmNactivateCallback, exit, NULL);

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
            XmNwidth, window_width,
            XmNheight, window_height,
            NULL);

    scrollVert = XtVaCreateManagedWidget("scrollVert", 
            xmScrollBarWidgetClass,
            scrollBox, 
            XmNorientation, XmVERTICAL,
            XmNheight, window_height,
            XmNwidth, 15,
            NULL);

    XtAddCallback(scrollVert, XmNpageIncrementCallback, Scroll_up_down, bigBitmap);
    XtAddCallback(scrollVert, XmNpageDecrementCallback, Scroll_up_down, bigBitmap);
    XtAddCallback(scrollVert, XmNdragCallback, Scroll_up_down, bigBitmap);

    XtOverrideTranslations(scrollVert,
        XtParseTranslationTable("<Configure>: resize_thumbs(v)\n\
                <Expose>: resize_thumbs(v)"));

    scrollHoriz = XtVaCreateManagedWidget("scrollHoriz", 
            xmScrollBarWidgetClass, scrollBox, 
            XmNorientation, XmHORIZONTAL,
            XmNwidth, window_width,
            XmNheight, 15,
            NULL);

    XtAddCallback(scrollHoriz, XmNpageIncrementCallback, Scroll_left_right, bigBitmap);
    XtAddCallback(scrollHoriz, XmNpageDecrementCallback, Scroll_left_right, bigBitmap);
    XtAddCallback(scrollHoriz, XmNdragCallback, Scroll_left_right, bigBitmap);
    XtOverrideTranslations(scrollHoriz,
        XtParseTranslationTable("<Configure>: resize_thumbs(h)\n\
                <Expose>: resize_thumbs(h)"));
}
