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

/* attach.c -- demonstrate how attachments work in Form widgets. */

#include <Xm/PushB.h>
#include <Xm/Form.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, parent, one, two, three;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget ("form",
        xmFormWidgetClass, toplevel, NULL);
    one = XtVaCreateManagedWidget ("One",
        xmPushButtonWidgetClass, parent,
        XmNtopAttachment,   XmATTACH_FORM,
        XmNleftAttachment,  XmATTACH_FORM,
        NULL);
    two = XtVaCreateManagedWidget ("Two",
        xmPushButtonWidgetClass, parent,
        XmNleftAttachment,  XmATTACH_WIDGET,
        XmNleftWidget,      one,
        /* attach top of widget to same y coordinate as top of "one" */
        XmNtopAttachment,   XmATTACH_OPPOSITE_WIDGET,
        XmNtopWidget,       one,
        NULL);
    three = XtVaCreateManagedWidget ("Three",
        xmPushButtonWidgetClass, parent,
        XmNtopAttachment,   XmATTACH_WIDGET,
        XmNtopWidget,       one,
        /* attach left of widget to same x coordinate as left side of "one" */
        XmNleftAttachment,  XmATTACH_OPPOSITE_WIDGET,
        XmNleftWidget,      one,
        NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
