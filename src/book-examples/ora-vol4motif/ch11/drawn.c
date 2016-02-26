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

/* drawn.c -- demonstrate the DrawnButton widget by drawing a
 * common X logo into its window.  This is hardly much different
 * from a PushButton widget, but the DrawnButton isn't much
 * different, except for a couple more callback routines...
 */
#include <Xm/DrawnB.h>
#include <Xm/BulletinB.h>

Pixmap pixmap;

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, bb, button;
    Pixel fg, bg;
    Dimension ht, st;
    void my_callback();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    bb = XtVaCreateManagedWidget ("bb",
        xmBulletinBoardWidgetClass, toplevel, NULL);

    XtVaGetValues (bb,
        XmNforeground, &fg,
        XmNbackground, &bg,
        NULL);
    pixmap = XmGetPixmap (XtScreen (bb), "xlogo64", fg, bg);

    button = XtVaCreateManagedWidget ("button",
        xmDrawnButtonWidgetClass, bb,
        NULL);

    XtVaGetValues (button, 
        XmNhighlightThickness, &ht,
        XmNshadowThickness, &st,
        NULL);

    XtVaSetValues (button,
        XmNwidth, 2 * ht + 2 * st + 64,
        XmNheight, 2 * ht + 2 * st + 64,
        NULL);

    XtAddCallback (button, XmNactivateCallback, my_callback, NULL);
    XtAddCallback (button, XmNexposeCallback, my_callback, NULL);
    XtAddCallback (button, XmNresizeCallback, my_callback, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
my_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    XmDrawnButtonCallbackStruct *cbs = 
        (XmDrawnButtonCallbackStruct *) call_data;
    if (cbs->reason == XmCR_ACTIVATE)
        printf ("%s: pushed %d times\n", XtName(w), cbs->click_count);
    else if (cbs->reason == XmCR_EXPOSE) {
        Dimension ht, st;

        XtVaGetValues (w, 
            XmNhighlightThickness, &ht,
            XmNshadowThickness, &st,
            NULL);

        XtVaSetValues (w,
            XmNwidth, 2 * ht + 2 * st + 64,
            XmNheight, 2 * ht + 2 * st + 64,
            NULL);

        XCopyArea (XtDisplay (w), pixmap, XtWindow (w), 
            XDefaultGCOfScreen (XtScreen (w)), 0, 0, 64, 64, 
            ht + st, ht + st);
    }
    else /* XmCR_RESIZE */
        puts ("Resize");
}

