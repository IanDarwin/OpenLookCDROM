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

/* more_help.c -- create a pushbutton that posts a dialog box
 * that entices the user to press the help button.  The callback
 * for this button displays a new dialog that gives help.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <stdio.h>

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
    XtAddCallback (dialog, XmNhelpCallback, help_callback, 0);

    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}

#define MAX_HELP_STAGES 3
char *help_text[3][5] = {
    {
        "You have reached the first stage of the help system.",
        "If you need additional help, select the 'More Help' button.",
        "You may exit help at any time by pressing 'Done'.",
        NULL,
    },
    {
        "This is the second stage of the help system.  There is",
        "more help available.  Press 'More Help' to see more.",
        "Press 'Previous' to return to the previous help message,",
        "or press 'Done' to exit the help system.",
        NULL,
    },
    {
        "This is the last help message you will see on this topic.",
        "You may either press 'Previous' to return to the previous",
        "help level, or press 'Done' to exit the help system.",
        NULL,
    }
};

/* help_callback() -- callback routine for the Help button in the
 * original dialog box.  The routine also serves as its own help
 * callback for displaying multiple levels of help messages.
 */
void
help_callback(parent, client_data, call_data)
Widget parent;
XtPointer client_data;
XtPointer call_data;
{
    static Widget dialog; /* prevent multiple help dialogs */
    XmString text;
    char buf[BUFSIZ], *p;
    static int index;
    int i;
    void help_done();
    int index_incr = (int) client_data;

    if (dialog && index_incr == 0) {
        /* user pressed Help button in MesageDialog again.  We're
         * already up, so just make sure we're visible and return. */
        XtPopup (XtParent (dialog), XtGrabNone);
        XMapRaised (XtDisplay (dialog), XtWindow (XtParent (dialog)));
        return;
    }

    if (dialog)
        index += index_incr; /* more/previous help; change index */
    else {
        /* We're not up, so create new Help Dialog */
        Arg args[5];
	int n;

        /* Action area button labels. */
        XmString done = XmStringCreateLocalized ("Done");
        XmString cancel = XmStringCreateLocalized ("Previous");
        XmString more = XmStringCreateLocalized ("More Help");

	n = 0;
        XtSetArg (args[n], XmNautoUnmanage, False); n++;
        XtSetArg (args[n], XmNokLabelString, done); n++;
        XtSetArg (args[n], XmNcancelLabelString, cancel); n++;
        XtSetArg (args[n], XmNhelpLabelString, more); n++;
        dialog = XmCreateInformationDialog (parent, "help", args, n);

        /* pass help_done() the address of "dialog" so it can reset */
        XtAddCallback (dialog, XmNokCallback, help_done, &dialog);

        /* if more/previous help, recall ourselves with increment */
        XtAddCallback (dialog, XmNcancelCallback, help_callback, -1);
        XtAddCallback (dialog, XmNhelpCallback, help_callback, 1);

        /* If our parent dies, we must reset "dialog" to NULL! */
        XtAddCallback (dialog, XmNdestroyCallback, help_done, &dialog);

        XmStringFree (done);   /* once dialog is created, these */
        XmStringFree (cancel); /* strings are no longer needed. */
        XmStringFree (more);

        index = 0; /* initialize index--needed for each new help stuff */
    }

    /* concatenate help text into a single string with newlines */
    for (p = buf, i = 0; help_text[index][i]; i++) {
        p += strlen (strcpy (p, help_text[index][i]));
        *p++ = '\n';
	*p = 0;
    }

    text = XmStringCreateLtoR (buf, XmFONTLIST_DEFAULT_TAG);
    XtVaSetValues (dialog, XmNmessageString, text, NULL);
    XmStringFree (text); /* after set-values, free unneeded memory */

    /* If no previous help msg, set "Previous" to insensitive. */
    XtSetSensitive (
        XmMessageBoxGetChild (dialog,XmDIALOG_CANCEL_BUTTON), index > 0);
    /* If no more help, set "More Help" insensitive. */
    XtSetSensitive (XmMessageBoxGetChild (
        dialog, XmDIALOG_HELP_BUTTON), index < MAX_HELP_STAGES-1);

    /* display the dialog */
    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}

/* help_done () -- callback used to set the dialog pointer
 * to NULL so it can't be referenced again by help_callback().
 * This function is called from the Done button in the help dialog.
 * It is also our XmNdestroyCallback, so reset our dialog_ptr to NULL.
 */
void
help_done(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    Widget *dialog_ptr;

    if (!client_data) { /* destroy original MessageDialog */
        XtDestroyWidget (dialog);
	return;
    }

    dialog_ptr = (Widget *) client_data;
    if (!*dialog_ptr) /* prevent unnecessarily destroying twice */
        return;
    XtDestroyWidget (dialog); /* this might call ourselves.. */
    *dialog_ptr = NULL;
}
