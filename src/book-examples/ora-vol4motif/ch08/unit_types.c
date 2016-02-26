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

/* unit_types.c --the same as paned_win1.c except that the
 * Labels' minimum and maximum sizes are set to 1/4 inch and
 * 1/2 inch respectively.  These measurements are retained
 * regardless of the pixels-per-inch resolution of the user's
 * display.
 */
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Text.h>

main(argc, argv)
char *argv[];
{
    Widget        toplevel, pane;
    XtAppContext  app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    pane = XtVaCreateWidget ("pane",
        xmPanedWindowWidgetClass, toplevel,
        XmNunitType, Xm1000TH_INCHES,
        NULL);

    XtVaCreateManagedWidget ("Hello", xmLabelWidgetClass, pane,
        XmNpaneMinimum,    250, /* quarter inch */
        XmNpaneMaximum,    500, /* half inch */
        NULL);

    XtVaCreateManagedWidget ("text", xmTextWidgetClass, pane,
        XmNrows,           5,
        XmNcolumns,        80,
        XmNpaneMinimum,    250,
        XmNeditMode,       XmMULTI_LINE_EDIT,
        XmNvalue,   "This is a test of the paned window widget.",
        NULL);

    XtVaCreateManagedWidget ("Goodbye", xmLabelWidgetClass, pane,
        XmNpaneMinimum,    250, /* quarter inch */
        XmNpaneMaximum,    500, /* half inch */
        NULL);

    XtManageChild (pane);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
