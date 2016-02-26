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

/* string.c  -- create a compound string with the "MY_TAG" tag.
 * The tag defaults to the "9x15" font.  Create three pushbuttons: 
 * pb1, pb2, and pb3.  The user can specify resources so that each of the
 * widgets has a different font associated with the "MY_TAG" tag
 * specified in the compound string.
 */
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>

String fallbacks[] = { "*fontList:9x15=MY_TAG", NULL };

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, rowcol;
    XtAppContext  app;
    XmString      text;
    Display      *dpy;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "String", NULL, 0,
        &argc, argv, fallbacks, NULL);

    text = XmStringCreate ("Testing, testing...", "MY_TAG");

    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel,
        NULL);

    XtVaCreateManagedWidget ("pb1", 
        xmPushButtonGadgetClass, rowcol,
        XmNlabelString, text,
        NULL);

    XtVaCreateManagedWidget ("pb2", 
        xmPushButtonGadgetClass, rowcol,
        XmNlabelString, text,
        NULL);

    XtVaCreateManagedWidget ("pb3", 
        xmPushButtonGadgetClass, rowcol,
        XmNlabelString, text,
        NULL);

    XmStringFree (text);
    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
