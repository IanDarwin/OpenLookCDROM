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

/* text_entry.c -- This demo shows how the RowColumn widget can be
 * configured to build a text entry form.  It displays a table of
 * right-justified Labels and Text widgets that extend to the right
 * edge of the Form.
 */
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>

char *text_labels[] = {
    "Name:", "Phone:", "Address:", "City:", "State:", "Zip Code:",
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol;
    XtAppContext app;
    char buf[8];
    int i;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget ("rowcolumn",
        xmRowColumnWidgetClass, toplevel,
        XmNpacking,        XmPACK_COLUMN,
        XmNnumColumns,     XtNumber (text_labels),
        XmNorientation,    XmHORIZONTAL,
        XmNisAligned,      True,
        XmNentryAlignment, XmALIGNMENT_END,
        NULL);

    /* simply loop thru the strings creating a widget for each one */
    for (i = 0; i < XtNumber (text_labels); i++) {
        XtVaCreateManagedWidget (text_labels[i],
            xmLabelGadgetClass, rowcol,
            NULL);
        sprintf (buf, "text_%d", i);
        XtVaCreateManagedWidget (buf,
            xmTextWidgetClass, rowcol,
            NULL);
    }

    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
