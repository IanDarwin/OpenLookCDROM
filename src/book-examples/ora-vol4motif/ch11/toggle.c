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

/* toggle.c -- demonstrate a simple toggle button.  */
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>

void
toggled(widget, client_data, call_data)
Widget widget;
XtPointer client_data; 
XtPointer call_data;
{
    XmToggleButtonCallbackStruct *state = 
        (XmToggleButtonCallbackStruct *) call_data;
    
    printf ("%s: %s\n", XtName (widget), state->set? "on" : "off");
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, toggle;
    XtAppContext app;
    Pixmap on, off;
    Pixel fg, bg;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget ("_rowcol",
        xmRowColumnWidgetClass, toplevel, 
        XmNorientation, XmHORIZONTAL,
        NULL);

    XtVaGetValues (rowcol,
        XmNforeground, &fg,
        XmNbackground, &bg,
        NULL);
    on = XmGetPixmap (XtScreen (rowcol), "switch_on", fg, bg);
    off = XmGetPixmap (XtScreen (rowcol), "switch_off", fg, bg);
    if (on == XmUNSPECIFIED_PIXMAP || off == XmUNSPECIFIED_PIXMAP) {
        puts ("Couldn't load pixmaps");
        exit (1);
    }

    toggle = XtVaCreateManagedWidget ("toggle",
        xmToggleButtonWidgetClass, rowcol, 
        XmNlabelType,    XmPIXMAP,
        XmNlabelPixmap,  off,
        XmNselectPixmap, on,
        NULL);
    XtAddCallback (toggle, XmNvalueChangedCallback, toggled, NULL);

    toggle = XtVaCreateManagedWidget ("toggle",
        xmToggleButtonWidgetClass, rowcol, 
        XmNlabelType,    XmPIXMAP,
        XmNlabelPixmap,  off,
        XmNselectPixmap, on,
        NULL);
    XtAddCallback (toggle, XmNvalueChangedCallback, toggled, NULL);

    XtManageChild (rowcol);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
