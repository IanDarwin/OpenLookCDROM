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

/* traverse.c -- demonstrate how keyboard traversal can be
 * manipulated among primitive widgets.  Create a tic-tac-toe
 * board of PushButtons.  As each item is selected, mark it
 * with an X (unless the Shift key is down) and change the
 * PushButton's XmNtraversalOn to False so user can't traverse
 * to it anymore.
 */
#include <Xm/PushBG.h>
#include <Xm/Form.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, parent, w;
    int x, y;
    extern void pushed();  /* callback for the PushButton */

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    parent = XtVaCreateManagedWidget ("form", 
        xmFormWidgetClass, toplevel,
        XmNfractionBase,    3,
        NULL);
    /* create nine pushbutton widgets in tic-tac-toe format */
    for (x = 0; x < 3; x++)
        for (y = 0; y < 3; y++) {
            w = XtVaCreateManagedWidget ("   ",
                xmPushButtonGadgetClass, parent,
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
Widget      w;      
XtPointer   client_data;
XtPointer   call_data;
{
    char buf[2];
    XmString str;
    int letter;
    XmPushButtonCallbackStruct *cbs =
        (XmPushButtonCallbackStruct *) call_data;

    XtVaGetValues (w, XmNuserData, &letter, NULL);
    if (letter) {
        XBell (XtDisplayOfObject (w), 50);
        return;
    }
    /* Shift key gets an O.  (xbutton and xkey happen to be similar) */
    if (cbs->event->xbutton.state & ShiftMask)
        letter = buf[0] = '0';
    else
        letter = buf[0] = 'X';
    buf[1] = 0;
    str = XmStringCreateLocalized (buf);
    XtVaSetValues (w,
        XmNlabelString,     str,
        XmNuserData,        letter,
        XmNshadowThickness, 0,
        XmNtraversalOn,     False,
        NULL);
    XmStringFree (str);
}


