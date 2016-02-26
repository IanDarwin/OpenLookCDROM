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

/* warning.c -- show a use of the WarningMsg() function.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

/*
 * WarningMsg() -- Inform the user that she is about to embark on a
 * dangerous mission and give her the opportunity to back out.
 */
void
WarningMsg(parent, client_data, call_data)
Widget parent;
XtPointer client_data;
XtPointer call_data;
{
    static Widget dialog;
    XmString text, ok_str, cancel_str;
    char *msg = (char *) client_data;

    if (!dialog)
        dialog = XmCreateWarningDialog (parent, "warning", NULL, 0);
    text = XmStringCreateLtoR (msg, XmFONTLIST_DEFAULT_TAG);
    ok_str = XmStringCreateLocalized ("Yes");
    cancel_str = XmStringCreateLocalized ("No");
    XtVaSetValues (dialog,
        XmNmessageString,     text,
        XmNokLabelString,     ok_str,
        XmNcancelLabelString, cancel_str,
        XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON,
        NULL);
    XmStringFree (text);
    XmStringFree (ok_str);
    XmStringFree (cancel_str);

    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}


main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, pb;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);
    pb = XtVaCreateManagedWidget ("Button",
        xmPushButtonWidgetClass, toplevel, NULL);
    XtAddCallback (pb, XmNactivateCallback, WarningMsg, 
        "Do you really want to delete all files?");

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
