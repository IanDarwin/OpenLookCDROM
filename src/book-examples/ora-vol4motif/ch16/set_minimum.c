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

/* set_minimum.c -- demonstrate how to set the minimum size of a
 * window to its initial size.  This method is useful if your program
 * is initially displayed at its minimum size, but it would be too
 * difficult to try to calculate ahead of time what the initial size
 * would be.
 */
#include <Xm/PushB.h>

void getsize(), configure();

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL,
        XmNmaxWidth,     150,
        XmNmaxHeight,    100,
        XmNbaseWidth,    5,
        XmNbaseHeight,   5,
        XmNwidthInc,     5,
        XmNheightInc,    5,
        NULL);

    /* Add an event handler to trap the first configure event */
    XtAddEventHandler (toplevel, StructureNotifyMask, False, configure, NULL);

    /* Pushbutton's callback prints the dimensions of the shell. */
    button = XtVaCreateManagedWidget ("Print Size",
        xmPushButtonWidgetClass, toplevel, NULL);
    XtAddCallback (button, XmNactivateCallback, getsize, toplevel);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
getsize(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Widget shell = (Widget) client_data;
    Dimension width, height;

    XtVaGetValues (shell, 
        XmNwidth, &width, 
        XmNheight, &height, 
        NULL);
    printf ("Width = %d, Height = %d\n", width, height);
}

void
configure(shell, client_data, event)
Widget shell;
XtPointer client_data;
XEvent *event;
{
    XConfigureEvent *cevent = (XConfigureEvent *) event;

    if (cevent->type != ConfigureNotify)
        return;
    printf ("Width = %d, Height = %d\n", cevent->width, cevent->height);
    XtVaSetValues (shell,
        XmNminWidth, cevent->width,
        XmNminHeight, cevent->height,
        NULL);
    XtRemoveEventHandler (shell, StructureNotifyMask, False, configure, NULL);
}
