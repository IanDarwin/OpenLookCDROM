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

/* pixmaps.c -- Demonstrate simple label gadgets in a row column.
 * Each command line argument represents a bitmap filename.  Try
 * to load the corresponding pixmap and store in a RowColumn.
 */
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Pixel fg, bg;
    Widget toplevel, rowcol;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    if (argc < 2) {
        puts ("Specify bitmap filenames.");
        exit (1);
    }
    /* create a RowColumn that has an equal number of rows and
     * columns based on the number of pixmaps it is going to
     * display (this value is in "argc").
     */
    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel,
        XmNnumColumns,  int_sqrt (argc),
        XmNpacking,     XmPACK_COLUMN,
        NULL);

    /* Get the foreground and background colors of the rowcol to make
     * all the pixmaps appear using a consistent color.
     */
    XtVaGetValues (rowcol,
        XmNforeground, &fg,
        XmNbackground, &bg,
        NULL);

    while (*++argv) {
        Pixmap pixmap = XmGetPixmap (XtScreen (rowcol), *argv, fg, bg);
        if (pixmap == XmUNSPECIFIED_PIXMAP)
            printf ("Couldn't load %s\n", *argv);
        else
            XtVaCreateManagedWidget (*argv, xmLabelGadgetClass, rowcol,
                XmNlabelType, XmPIXMAP,
                XmNlabelPixmap, pixmap,
                NULL);
    }

    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* get the integer square root of n -- used to determine the number
 * of rows and columns of pixmaps to use in the RowColumn widget.
 */
int_sqrt(n)
register int n;
{
    register int i, s = 0, t;
    for (i = 15; i >= 0; i--) {
        t = (s | (1 << i));
        if (t * t <= n)
            s = t;
    }
    return s;
}
