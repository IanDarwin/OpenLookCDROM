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

/* pushb.c -- demonstrate the pushbutton widget.  Display one
 * PushButton with a single callback routine.  Print the name
 * of the widget and the number of "multiple clicks".  This
 * value is maintained by the toolkit.
 */
#include <Xm/PushB.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, button;
    void my_callback();
    XmString btn_text;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    btn_text = XmStringCreateLocalized ("Push Here");
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString, btn_text,
        NULL);
    XmStringFree (btn_text);
    XtAddCallback (button, XmNarmCallback, my_callback, NULL);
    XtAddCallback (button, XmNactivateCallback, my_callback, NULL);
    XtAddCallback (button, XmNdisarmCallback, my_callback, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
my_callback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    XmPushButtonCallbackStruct *cbs = 
        (XmPushButtonCallbackStruct *) call_data;

    if (cbs->reason == XmCR_ARM)
        printf ("%s: armed\n", XtName (w));
    else if (cbs->reason == XmCR_DISARM)
        printf ("%s: disarmed\n", XtName (w));
    else
        printf ("%s: pushed %d times\n", XtName (w), cbs->click_count);
}
