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

/* main_list.c -- Use the ScrolledList window as the feature
 * component of a MainWindow widget.
 */
#include <Xm/MainW.h>
#include <Xm/List.h>

main(argc, argv)
char *argv[];
{
    Widget toplevel, main_w, list_w;
    XtAppContext app;
    Pixmap pixmap;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    main_w = XtVaCreateManagedWidget ("main_window",
        xmMainWindowWidgetClass,   toplevel,
        NULL);

    list_w = XmCreateScrolledList (main_w, "main_list", NULL, 0);
    XtVaSetValues (list_w,
        XtVaTypedArg, XmNitems, XmRString,
            "Red, Green, Blue, Orange, Maroon, Grey, Black, White", 53,
        XmNitemCount,           8,
        XmNvisibleItemCount,    5,
        NULL);
    XtManageChild (list_w);

    /* set the list_w as the "work area" of the main window */
    XtVaSetValues (main_w, XmNworkWindow, XtParent (list_w), NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
