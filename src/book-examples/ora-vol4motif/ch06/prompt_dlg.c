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

/* prompt_dlg.c -- prompt the user for a string.  Two PushButtons
 * are displayed.  When one is selected, a PromptDialog is displayed
 * allowing the user to type a string.  When done, the PushButton's
 * label changes to the string.
 */
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

main(argc, argv)
char *argv[];
{
    XtAppContext app;
    Widget toplevel, rc, button;
    void pushed();

    XtSetLanguageProc (NULL, NULL, NULL);

    /* Initialize toolkit and create toplevel shell */
    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* RowColumn managed both PushButtons */
    rc = XtVaCreateWidget ("rowcol", xmRowColumnWidgetClass, toplevel,
        NULL);
    /* Create two pushbuttons -- both have the same callback */
    button = XtVaCreateManagedWidget ("PushMe 1",
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback (button, XmNactivateCallback, pushed, NULL);
    button = XtVaCreateManagedWidget ("PushMe 2",
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback (button, XmNactivateCallback, pushed, NULL);

    XtManageChild (rc);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* pushed() --the callback routine for the main app's pushbuttons.
 * Create a dialog that prompts for a new button name.
 */
void
pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Widget dialog;
    XmString t = XmStringCreateLocalized ("Enter New Button Name:");
    extern void read_name();
    Arg args[5];
    int n = 0;

    /* Create the dialog -- the PushButton acts as the DialogShell's
     * parent (not the parent of the PromptDialog).
     */
    XtSetArg (args[n], XmNselectionLabelString, t); n++;
    XtSetArg (args[n], XmNautoUnmanage, False); n++;
    dialog = XmCreatePromptDialog (widget, "prompt", args, n);
    XmStringFree (t); /* always destroy compound strings when done */

    /* When the user types the name, call read_name() ... */
    XtAddCallback (dialog, XmNokCallback, read_name, widget);

    /* If the user selects cancel, just destroy the dialog */
    XtAddCallback (dialog, XmNcancelCallback, XtDestroyWidget, NULL);

    /* No help is available... */
    XtSetSensitive (
        XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    XtManageChild (dialog);

    XtPopup (XtParent (dialog), XtGrabNone);
}

/* read_name() --the text field has been filled in. */
void
read_name(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Widget push_button = (Widget) client_data;  
    XmSelectionBoxCallbackStruct *cbs = 
        (XmSelectionBoxCallbackStruct *) call_data;

    XtVaSetValues (push_button, XmNlabelString, cbs->value, NULL);
    /* Name's fine -- go ahead and enter it */
    XtDestroyWidget(widget);
}
