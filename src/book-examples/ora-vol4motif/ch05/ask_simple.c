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

/* ask_user_simple.c -- create a pushbutton that posts a dialog box
 * that asks the user a question that requires an immediate
 * response.  The function that asks the question actually
 * posts the dialog that displays the question, waits for and
 * returns the result.
 */
#include <X11/Intrinsic.h>
#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>

XtAppContext app;

#define YES 1
#define NO  2

/* main() --create a pushbutton whose callback pops up a dialog box */
main(argc, argv)
int argc;
char *argv[];
{
    Widget parent, button, toplevel;
    XmString label;
    void pushed();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    label = XmStringCreateLocalized ("/bin/rm *");
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XtAddCallback (button, XmNactivateCallback,
        pushed, "Remove Everything?");
    XmStringFree (label);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* pushed() --the callback routine for the main app's pushbutton. */
void
pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    char *question = (char *) client_data;

    if (AskUser (widget, question) == YES)
        puts ("Yes");
    else
        puts ("No");
}

/*
 * AskUser() -- a generalized routine that asks the user a question
 * and returns the Yes/No response.
 */
int
AskUser(parent, question)
Widget parent;
char *question;
{
    static Widget dialog;
    XmString text, yes, no;
    static int answer;
    extern void response();
    extern XtAppContext app;

    if (!dialog) {
        dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
        yes = XmStringCreateLocalized ("Yes");
        no = XmStringCreateLocalized ("No");
        XtVaSetValues (dialog,
            XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
            XmNokLabelString,      yes,
            XmNcancelLabelString,  no,
            NULL);
        XtSetSensitive (
            XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON),
            False);
        XtAddCallback (dialog, XmNokCallback, response, &answer);
        XtAddCallback (dialog, XmNcancelCallback, response, &answer);
        XmStringFree (yes);
        XmStringFree (no);
    }
    answer = 0;
    text = XmStringCreateLocalized (question);
    XtVaSetValues (dialog,
        XmNmessageString,      text,
        NULL);
    XmStringFree (text);
    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);

    /* while the user hasn't provided an answer, simulate main loop.
     * The answer changes as soon as the user selects one of the
     * buttons and the callback routine changes its value.
     */
    while (answer == 0)
        XtAppProcessEvent (app, XtIMAll);

    XtPopdown (XtParent (dialog));
    return answer;
}

/* response() --The user made some sort of response to the
 * question posed in AskUser().  Set the answer (client_data)
 * accordingly and destroy the dialog.
 */
void
response(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int *answer = (int *) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;

    switch (cbs->reason) {
        case XmCR_OK:
            *answer = YES;
            break;
        case XmCR_CANCEL:
            *answer = NO;
            break;
        default:
            return;
    }
}
