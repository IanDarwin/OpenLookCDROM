/* Written by Paula Ferguson.  
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

/* traversal.c -- demonstrate keyboard traversal in a ScrolledWindow 
 * using the XmNtraverseObscuredCallback.
 */
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/ScrolledW.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, sw, rc;
    XtAppContext app;
    void traverse();
    int i;
    char name[10];

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    sw = XtVaCreateManagedWidget ("scrolled_w",
        xmScrolledWindowWidgetClass, toplevel,
        XmNscrollingPolicy, XmAUTOMATIC,
        NULL);

    XtAddCallback (sw, XmNtraverseObscuredCallback, traverse, NULL);

    /* RowColumn is the work window for the widget */
    rc = XtVaCreateWidget ("rc", 
        xmRowColumnWidgetClass, sw, 
	XmNorientation, XmHORIZONTAL,
        XmNpacking, XmPACK_COLUMN,
	XmNnumColumns, 10,
        NULL);

    for ( i = 0; i < 10; i++ ) {
        sprintf (name, "Toggle %d", i);
        XtVaCreateManagedWidget (name, xmToggleButtonWidgetClass, rc, NULL);
        sprintf (name, "Button %d", i);
        XtVaCreateManagedWidget (name, xmPushButtonWidgetClass, rc, NULL);
    }
    XtManageChild (rc);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
traverse(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    XmTraverseObscuredCallbackStruct *cbs = 
        (XmTraverseObscuredCallbackStruct *) call_data;

    XmScrollVisible (widget, cbs->traversal_destination, 10, 10);
}

