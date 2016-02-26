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

/* hello_dialog.c -- your typical Hello World program using
 * an InformationDialog.
 */
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, rc, pb;
    extern void popup(); /* callback for the pushbuttons -- pops up dialog */
    extern void exit();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rc = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);
    pb = XtVaCreateManagedWidget ("Hello", 
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback (pb, XmNactivateCallback, popup, "Hello World");
    pb = XtVaCreateManagedWidget ("Goodbye", 
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback (pb, XmNactivateCallback, exit, NULL);

    XtManageChild (rc);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* callback for the PushButtons.  Popup an InformationDialog displaying
 * the text passed as the client data parameter.
 */
void
popup(button, client_data, call_data)
Widget button;
XtPointer client_data;
XtPointer call_data;
{
    Widget dialog;
    XmString xm_string;
    extern void activate();
    Arg args[5];
    int n = 0;
    char *text = (char *) client_data;

    /* set the label for the dialog */
    xm_string = XmStringCreateLocalized (text);
    XtSetArg (args[n], XmNmessageString, xm_string); n++;

    /* Create the InformationDialog as child of button */
    dialog = XmCreateInformationDialog (button, "info", args, n);

    /* no longer need the compound string, free it */
    XmStringFree (xm_string);

    /* add the callback routine */
    XtAddCallback (dialog, XmNokCallback, activate, NULL);

    /* manage the dialog */
    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}

/* callback routine for when the user presses the OK button.
 * Yes, despite the fact that the OK button was pressed, the
 * widget passed to this callback routine is the dialog!
 */
void
activate(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    puts ("OK was pressed.");
}
