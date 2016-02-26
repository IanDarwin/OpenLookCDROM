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

/* dialog.c -- your typical Hello World program using
 * an InformationDialog.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

void
pushed(w)
Widget w;
{
    Widget dialog;
    extern void ok_pushed(), cancel_pushed(), help_pushed();
    Arg args[5];
    int n = 0;
    XmString m = XmStringCreateLocalized ("This is a message.");
    XmString t = XmStringCreateLocalized ("Message");

    XtSetArg (args[n], XmNautoUnmanage, False); n++;
    XtSetArg (args[n], XmNmessageString, m); n++;
    XtSetArg (args[n], XmNdialogTitle, t); n++;
    dialog = XmCreateMessageDialog (w, "notice", args, n);
    XtAddCallback (dialog, XmNokCallback, ok_pushed, "Hi");
    XtAddCallback (dialog, XmNcancelCallback, cancel_pushed, "Foo");
    XtAddCallback (dialog, XmNhelpCallback, help_pushed, NULL);
    XmStringFree (m);
    XmStringFree (t);
    
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
    XtAddCallback (pb, XmNactivateCallback, pushed, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* ok_pushed() --the OK button was selected.  */
void
ok_pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    char *message = (char *) client_data;

    printf ("OK was selected: %s\n", message);
    XtDestroyWidget (widget);
}

/* cancel_pushed() --the Cancel button was selected.  */
void
cancel_pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    char *message = (char *) client_data;

    printf ("Cancel was selected: %s\n", message);
    XtDestroyWidget (widget);
}

/* help_pushed() --the Help button was selected.  */
void
help_pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    printf ("Help was selected\n");
}
