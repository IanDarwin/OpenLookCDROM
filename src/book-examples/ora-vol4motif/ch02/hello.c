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

/* hello.c -- initialize the toolkit using an application context and a 
 * toplevel shell widget, then create a pushbutton that says Hello using
 * the varargs interface.
 */
#include <Xm/PushB.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, button;
    XtAppContext  app;
    void          button_pushed();
    XmString 	  label;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Hello", NULL, 0,
        &argc, argv, NULL, NULL);

    label = XmStringCreateLocalized ("Push here to say hello"); 
    button = XtVaCreateManagedWidget ("pushme",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString, label,
        NULL);
    XmStringFree (label);
    XtAddCallback (button, XmNactivateCallback, button_pushed, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
button_pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    printf ("Hello Yourself!\n");
}

