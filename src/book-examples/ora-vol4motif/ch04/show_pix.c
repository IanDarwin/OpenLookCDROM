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

/* show_pix.c -- A minimal example of a MainWindow.  Use a Label as the 
 * workWindow to display a bitmap specified on the command line.
 */
#include <Xm/MainW.h>
#include <Xm/Label.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, main_w, label;
    XtAppContext app;
    Pixmap pixmap;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    if (!argv[1]) {
        printf ("usage: %s bitmap-file\n", *argv);
        exit (1);
    }

    main_w = XtVaCreateManagedWidget ("main_window",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollBarDisplayPolicy, XmAS_NEEDED,
        XmNscrollingPolicy,        XmAUTOMATIC,
        NULL);

    /* Load bitmap given in argv[1] */
    pixmap = XmGetPixmap (XtScreen (toplevel), argv[1],
        BlackPixelOfScreen (XtScreen (toplevel)),
        WhitePixelOfScreen (XtScreen (toplevel)));

    if (pixmap == XmUNSPECIFIED_PIXMAP) {
        printf ("can't create pixmap from %s\n", argv[1]);
        exit (1);
    }

    /* Now create label using pixmap */
    label = XtVaCreateManagedWidget ("label", xmLabelWidgetClass, main_w,
        XmNlabelType,   XmPIXMAP,
        XmNlabelPixmap, pixmap,
        NULL);

    /* set the label as the "work area" of the main window */
    XtVaSetValues (main_w,
        XmNworkWindow, label,
        NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
