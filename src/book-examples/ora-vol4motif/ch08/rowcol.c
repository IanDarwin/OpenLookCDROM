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

/* rowcol.c -- demonstrate a simple RowColumn widget.  Create one
 * with 3 pushbutton gadgets.  Once created, resize the thing in
 * all sorts of contortions to get a feel for what RowColumns can
 * do with its children.
 */
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol;
    XtAppContext app;
    
    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0, 
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateManagedWidget ("rowcolumn",
        xmRowColumnWidgetClass, toplevel, NULL);

    (void) XtVaCreateManagedWidget ("One",
        xmPushButtonWidgetClass, rowcol, NULL);

    (void) XtVaCreateManagedWidget ("Two",
        xmPushButtonWidgetClass, rowcol, NULL);

    (void) XtVaCreateManagedWidget ("Three",
        xmPushButtonWidgetClass, rowcol, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
