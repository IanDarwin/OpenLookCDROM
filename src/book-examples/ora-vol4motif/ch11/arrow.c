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

/* arrow.c -- demonstrate the ArrowButton widget.
 * Have a Form widget display 4 ArrowButtons in a
 * familiar arrangement.
 */
#include <Xm/ArrowBG.h>
#include <Xm/Form.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, form;
    Display *dpy;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    dpy = XtDisplay (toplevel);
    /* Rather than listing all these resources in an app-defaults file,
     * add them directly to the database for this application only. This
     * would be virtually equivalent to hard-coding values, since these
     * resources will override any other specified external to this file.
    XrmPutStringResource 
        (&dpy->db, "*form*topAttachment", "attach_position");
    XrmPutStringResource 
        (&dpy->db, "*form*leftAttachment", "attach_position");
    XrmPutStringResource 
        (&dpy->db, "*form*rightAttachment", "attach_position");
    XrmPutStringResource 
        (&dpy->db, "*form*bottomAttachment", "attach_position");

    I commented this out since it didn't compile.  Adrian Nye.
     */

    form = XtVaCreateWidget ("form", xmFormWidgetClass, toplevel,
        XmNfractionBase,     3,
        NULL);

    XtVaCreateManagedWidget ("arrow1",
        xmArrowButtonGadgetClass, form,
        XmNtopPosition,      0,
        XmNbottomPosition,   1,
        XmNleftPosition,     1,
        XmNrightPosition,    2,
        XmNarrowDirection,   XmARROW_UP,
        NULL);

    XtVaCreateManagedWidget ("arrow2",
        xmArrowButtonGadgetClass, form,
        XmNtopPosition,      1,
        XmNbottomPosition,   2,
        XmNleftPosition,     0,
        XmNrightPosition,    1,
        XmNarrowDirection,   XmARROW_LEFT,
        NULL);

    XtVaCreateManagedWidget ("arrow3",
        xmArrowButtonGadgetClass, form,
        XmNtopPosition,      1,
        XmNbottomPosition,   2,
        XmNleftPosition,     2,
        XmNrightPosition,    3,
        XmNarrowDirection,   XmARROW_RIGHT,
        NULL);

    XtVaCreateManagedWidget ("arrow4",
        xmArrowButtonGadgetClass, form,
        XmNtopPosition,      2,
        XmNbottomPosition,   3,
        XmNleftPosition,     1,
        XmNrightPosition,    2,
        XmNarrowDirection,   XmARROW_DOWN,
        NULL);

    XtManageChild (form);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
