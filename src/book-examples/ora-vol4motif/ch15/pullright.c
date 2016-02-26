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

/* simple_pullright.c -- demonstrate how to make a pullright menu
 * using simple menu creation routines.  Create a main window that 
 * contains a DrawingArea widget that displays a popup menu when the 
 * user presses the third mouse button.
 */
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>

main(argc, argv)
int argc;
char *argv[];
{
    XmString line, square, circle, weight, exit, exit_acc;
    XmString w_one, w_two, w_four, w_eight;
    Widget toplevel, main_w, drawing_a, cascade, popup_menu, pullright;
    void popup_cb(), set_width(), input();
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* Create a MainWindow widget that contains a DrawingArea in
     * its work window. 
     */
    main_w = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,      XmAUTOMATIC,
        NULL);
    /* Create a DrawingArea -- no actual drawing will be done. */
    drawing_a = XtVaCreateManagedWidget ("drawing_a",
        xmDrawingAreaWidgetClass, main_w,
        XmNwidth,  500,
        XmNheight, 500,
        NULL);

    line = XmStringCreateLocalized ("Line");
    square = XmStringCreateLocalized ("Square");
    circle = XmStringCreateLocalized ("Circle");
    weight = XmStringCreateLocalized ("Line Width");
    exit = XmStringCreateLocalized ("Exit");
    exit_acc = XmStringCreateLocalized ("Ctrl+C");
    popup_menu = XmVaCreateSimplePopupMenu (drawing_a, "popup", popup_cb,
        XmVaPUSHBUTTON, line, 'L', NULL, NULL,
        XmVaPUSHBUTTON, square, 'S', NULL, NULL,
        XmVaPUSHBUTTON, circle, 'C', NULL, NULL,
        XmVaCASCADEBUTTON, weight, 'W',
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, exit, 'x', "Ctrl<Key>c", exit_acc,
        NULL);
    XmStringFree (line);
    XmStringFree (square);
    XmStringFree (circle);
    XmStringFree (weight);
    XmStringFree (exit);

    /* create pullright for "Line Width" button -- this is the 4th item! */
    w_one = XmStringCreateLocalized (" 1 ");
    w_two = XmStringCreateLocalized (" 2 ");
    w_four = XmStringCreateLocalized (" 4 ");
    w_eight = XmStringCreateLocalized (" 8 ");
    pullright = XmVaCreateSimplePulldownMenu (popup_menu,
        "pullright", 3 /* menu item offset */, set_width,
        XmVaPUSHBUTTON, w_one, '1', NULL, NULL,
        XmVaPUSHBUTTON, w_two, '2', NULL, NULL,
        XmVaPUSHBUTTON, w_four, '4', NULL, NULL,
        XmVaPUSHBUTTON, w_eight, '8', NULL, NULL,
        NULL);
    XmStringFree (w_one);
    XmStringFree (w_two);
    XmStringFree (w_four);
    XmStringFree (w_eight);

    /* after popup menu is created, add callback for all input events */
    XtAddCallback (drawing_a, XmNinputCallback, input, popup_menu);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* input() -- called in responses to events in the DrawingArea; 
 * button-3 pops up menu. 
 */
void
input(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Widget popup = (Widget) client_data;
    XmDrawingAreaCallbackStruct *cbs = 
        (XmDrawingAreaCallbackStruct *) call_data;

    if (cbs->event->xany.type != ButtonPress ||
            cbs->event->xbutton.button != 3)
        return;

    /* Position the menu where the event occurred */
    XmMenuPosition (popup, (XButtonPressedEvent *) (cbs->event));
    XtManageChild (popup);
}

/* popup_cb() -- invoked when the user selects an item in the popup menu */
void
popup_cb(menu_item, client_data, call_data)
Widget menu_item;
XtPointer client_data;
XtPointer call_data;
{
    int item_no = (int) client_data;

    if (item_no == 4) /* Exit was selected -- exit */
        exit (0);
    puts (XtName (menu_item)); /* Otherwise, just print the selection */
}

/* set_width() -- called when items in the Line Width pullright menu 
 * are selected. 
 */
void
set_width(menu_item, client_data, call_data)
Widget menu_item;
XtPointer client_data;
XtPointer call_data;
{
    int item_no = (int) client_data;
    
    printf ("Line weight = %d\n", 1 << item_no);
}
