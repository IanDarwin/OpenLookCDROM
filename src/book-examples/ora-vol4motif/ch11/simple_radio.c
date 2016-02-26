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

/* simple_radio.c -- demonstrate a simple radio box by using
 * XmVaCreateSimpleRadioBox().  Create a box with 3 toggles:
 * "one", "two" and "three".  The callback routine prints
 * the most recently selected choice.
 */
#include <Xm/RowColumn.h>

void
toggled(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int which = (int) client_data;
    XmToggleButtonCallbackStruct *state = 
        (XmToggleButtonCallbackStruct *) call_data;

    printf ("%s: %s\n", XtName (widget), state->set? "on" : "off");
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, radio_box;
    XtAppContext app;
    XmString one, two, three;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    one  = XmStringCreateLocalized ("One");
    two  = XmStringCreateLocalized ("Two");
    three  = XmStringCreateLocalized ("Three");
    radio_box = XmVaCreateSimpleRadioBox (toplevel, "radio_box",
        0,  /* the inital choice */
        toggled, /* the callback routine */
        XmVaRADIOBUTTON, one,  NULL, NULL, NULL,
        XmVaRADIOBUTTON, two,  NULL, NULL, NULL,
        XmVaRADIOBUTTON, three, NULL, NULL, NULL,
        NULL);
    XmStringFree (one);
    XmStringFree (two);
    XmStringFree (three);

    XtManageChild (radio_box);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
