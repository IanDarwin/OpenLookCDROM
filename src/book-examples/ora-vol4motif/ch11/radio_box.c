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

/* simple_radio.c -- demonstrate a simple radio box.  Create a
 * box with 3 toggles: "one", "two" and "three".  The callback
 * routine prints the most recently selected choice.  Maintain
 * a global variable that stores the most recently selected.
 */
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>

int toggle_item_set;

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
    if (state->set)
        toggle_item_set = which;
    else
        toggle_item_set = 0;
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, radio_box, one, two, three;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    radio_box = XmCreateRadioBox (toplevel, "radio_box", NULL, 0);

    one = XtVaCreateManagedWidget ("One",
        xmToggleButtonGadgetClass, radio_box, NULL);
    XtAddCallback (one, XmNvalueChangedCallback, toggled, 1);

    two = XtVaCreateManagedWidget ("Two",
        xmToggleButtonGadgetClass, radio_box, NULL);
    XtAddCallback (two, XmNvalueChangedCallback, toggled, 2);

    three = XtVaCreateManagedWidget ("Three",
        xmToggleButtonGadgetClass, radio_box, NULL);
    XtAddCallback (three, XmNvalueChangedCallback, toggled, 3);

    XtManageChild (radio_box);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
