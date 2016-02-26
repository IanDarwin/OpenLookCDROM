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

/* proc_traversal.c -- demonstrate how to process keyboard traversal
 * from a PushButton's callback routine.  This simple demo contains
 * a RowColumn (a tab group) and three PushButtons.  If any of the
 * PushButtons are activated (selected), the input focus traverses
 * to the "home" item.
 */ 
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, pb;
    XtAppContext app;
    void do_it();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateManagedWidget ("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    (void) XtVaCreateManagedWidget ("OK",
        xmPushButtonWidgetClass, rowcol, NULL);

    pb = XtVaCreateManagedWidget ("Cancel",
        xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback (pb, XmNactivateCallback, do_it, NULL);

    pb = XtVaCreateManagedWidget ("Help",
        xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback (pb, XmNactivateCallback, do_it, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* callback for pushbuttons */
void
do_it(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    /* do stuff here for PushButton widget */
    (void) XmProcessTraversal(widget, XmTRAVERSE_HOME);
}
