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

/* simple_help.c -- create a PushButton that posts a dialog box
 * that entices the user to press the help button.  The callback
 * for this button displays a new dialog that gives help.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, button;
    XtAppContext app;
    XmString label;
    void pushed();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    label = XmStringCreateLocalized ("Push Me");
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XtAddCallback (button, XmNactivateCallback,
        pushed, "You probably need help for this item.");
    XmStringFree (label);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

#define HELP_TEXT "This is the help information.\nNow press 'OK'"

/* pushed() -- the callback routine for the main app's pushbutton. */
void
pushed(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    char *text = (char *) client_data;
    Widget dialog;
    XmString t = XmStringCreateLocalized (text);
    Arg args[5];
    int n;
    void help_callback(), help_done();

    n = 0;
    XtSetArg (args[n], XmNautoUnmanage, False); n++;
    XtSetArg (args[n], XmNmessageString, t); n++;
    dialog = XmCreateMessageDialog (XtParent(w), "notice", args, n);
    XmStringFree (t);

    XtUnmanageChild (
        XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));

    XtAddCallback (dialog, XmNokCallback, help_done, NULL);
    XtAddCallback (dialog, XmNhelpCallback, help_callback, HELP_TEXT);

    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}

/*
 * help_callback() -- callback routine for the Help button in the 
 * original dialog box that displays an InformationDialog based on the 
 * help_text parameter.
 */
void
help_callback(parent, client_data, call_data)
Widget parent;
XtPointer client_data;
XtPointer call_data;
{
    char *help_text = (char *) client_data;
    Widget dialog;
    XmString text;
    void help_done();
    Arg args[5];
    int n;

    n = 0;
    text = XmStringCreateLtoR (help_text, XmFONTLIST_DEFAULT_TAG);
    XtSetArg (args[n], XmNmessageString, text); n++;
    XtSetArg (args[n], XmNautoUnmanage, False); n++;
    dialog = XmCreateInformationDialog (parent, "help", args, n);
    XmStringFree (text);

    XtUnmanageChild (  /* no need for the cancel button */
        XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
    XtSetSensitive (   /* no more help is available. */
        XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    /* the OK button will call help_done() below */
    XtAddCallback (dialog, XmNokCallback, help_done, NULL);

    /* display the help text */
    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}

/* help_done() -- called when user presses "OK" in dialogs. */
void
help_done(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    XtDestroyWidget (dialog);
}
