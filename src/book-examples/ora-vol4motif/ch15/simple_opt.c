/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 *
 *   The X Consortium, and any party obtaining a copy of these files from
 *   the X Consortium, directly or indirectly, is granted, free of charge, a
 *   full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *   nonexclusive right and license to deal in this software and
 *   documentation files (the "Software"), including without limitation the
 *   rights to use, copy, modify, merge, publish, distribute, sublicense,
 *   and/or sell copies of the Software, and to permit persons who receive
 *   copies from any such party to do so.  This license includes without
 *   limitation a license to do the foregoing actions under any patents of
 *   the party supplying this software to the X Consortium.
 */

/* simple_option.c -- demonstrate how to use a simple option menu.
 * Display a drawing area.  The user selects the drawing style from 
 * the option menu.  
 */
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>

main(argc, argv)
int argc;
char *argv[];
{
    XmString draw_shape, line, square, circle;
    Widget toplevel, main_w, rc, sw, drawing_a, option_menu, pb;
    void option_cb(), exit();
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a RowColumn
     * widget as its work window.
     */
    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel, NULL);
    rc = XtVaCreateWidget ("rowcol", xmRowColumnWidgetClass, main_w, NULL);

    /* Inside RowColumn is the Exit pushbutton, the option menu and the
     * scrolled window that contains the drawing area.
     */
    pb = XtVaCreateManagedWidget ("Exit", xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback (pb, XmNactivateCallback, exit, NULL);

    draw_shape = XmStringCreateLocalized ("Draw Mode:");
    line = XmStringCreateLocalized ("Line");
    square = XmStringCreateLocalized ("Square");
    circle = XmStringCreateLocalized ("Circle");
    option_menu = XmVaCreateSimpleOptionMenu (rc, "option_menu",
        draw_shape, 'D', 0 /*initial menu selection*/, option_cb,
        XmVaPUSHBUTTON, line, 'L', NULL, NULL,
        XmVaPUSHBUTTON, square, 'S', NULL, NULL,
        XmVaPUSHBUTTON, circle, 'C', NULL, NULL,
        NULL);
    XmStringFree (line);
    XmStringFree (square);
    XmStringFree (circle);
    XmStringFree (draw_shape);

    XtManageChild (option_menu);

    /* Create a DrawingArea inside a ScrolledWindow */
    sw = XtVaCreateManagedWidget ("sw", 
        xmScrolledWindowWidgetClass, rc,
        XmNscrollingPolicy, XmAUTOMATIC,
        NULL);
    drawing_a = XtVaCreateManagedWidget ("drawing_area",
        xmDrawingAreaWidgetClass, sw,
        XmNwidth,  500,
        XmNheight, 500,
        NULL);
    
    XtManageChild (rc);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* option_cb() -- invoked when the user selects an item in the 
 * option menu 
 */
void
option_cb(menu_item, client_data, call_data)
Widget menu_item;
XtPointer client_data;
XtPointer call_data;
{
    int item_no = (int) client_data;

    puts (XtName (menu_item));
}
