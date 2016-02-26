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

/* entry_cb.c -- demonstrate how the XmNentryCallback resource works
 * in RowColumn widgets.  When a callback function is set for this
 * resource, all the callbacks for the RowColumn's children are reset
 * to point to this function.  Their original functions are no longer
 * called had they been set in favor of the entry-callback function.
 */
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>

char *strings[] = {
    "One", "Two", "Three", "Four", "Five",
    "Six", "Seven", "Eight", "Nine", "Ten",
};

void
called(widget, client_data, call_data)
Widget widget;
XtPointer client_data; 
XtPointer call_data; 
{
    XmRowColumnCallbackStruct *cbs = 
        (XmRowColumnCallbackStruct *) call_data;
    Widget pb = cbs->widget;

    printf ("%s: %d\n", XtName (pb), cbs->data);
}

static void
never_called(widget, client_data, call_data)
Widget widget;
XtPointer client_data; 
XtPointer call_data; 
{
    puts ("This function is never called");
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, parent, w;
    XtAppContext app;
    int i;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget ("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        NULL);
    XtAddCallback (parent, XmNentryCallback, called, NULL);
 
    /* simply loop thru the strings creating a widget for each one */
    for (i = 0; i < XtNumber (strings); i++) {
        w = XtVaCreateManagedWidget (strings[i],
            xmPushButtonGadgetClass, parent, NULL);
        /* Call XtAddCallback() to install client_data only! */
        XtAddCallback (w, XmNactivateCallback, never_called, i+1);
    }

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
