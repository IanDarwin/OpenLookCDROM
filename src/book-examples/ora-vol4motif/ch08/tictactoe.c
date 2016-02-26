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

/* tictactoe.c -- demonstrate how fractionBase and XmATTACH_POSITIONs
 * work in Form widgets.
 */
#include <Xm/PushB.h>
#include <Xm/Form.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, parent, w;
    int x, y;
    extern void pushed();  /* callback for each PushButton */

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget ("form", 
        xmFormWidgetClass, toplevel,
        XmNfractionBase,    3,
        NULL);

    for (x = 0; x < 3; x++)
        for (y = 0; y < 3; y++) {
            w = XtVaCreateManagedWidget ("   ",
                xmPushButtonWidgetClass, parent,
                XmNtopAttachment,    XmATTACH_POSITION,
                XmNtopPosition,      y,
                XmNleftAttachment,   XmATTACH_POSITION,
                XmNleftPosition,     x,
                XmNrightAttachment,  XmATTACH_POSITION,
                XmNrightPosition,    x+1,
                XmNbottomAttachment, XmATTACH_POSITION,
                XmNbottomPosition,   y+1,
                NULL);
            XtAddCallback (w, XmNactivateCallback, pushed, NULL);
        }

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
pushed(w, client_data, call_data)
Widget     w;           /* The PushButton that got activated */
XtPointer  client_data; /* unused -- NULL was passed to XtAddCallback() */
XtPointer  call_data;
{
    char buf[2];
    XmString str;
    XmPushButtonCallbackStruct *cbs = 
        (XmPushButtonCallbackStruct *) call_data;

    /* Shift key gets an O.  (xbutton and xkey happen to be similar) */
    if (cbs->event->xbutton.state & ShiftMask)
        buf[0] = '0';
    else
        buf[0] = 'X';
    buf[1] = 0;
    str = XmStringCreateLocalized (buf);
    XtVaSetValues (w, XmNlabelString, str, NULL);
    XmStringFree (str);
}
