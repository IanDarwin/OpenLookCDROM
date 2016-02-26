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

/* ask_user.c -- the user is presented with two pushbuttons.  
 * The first creates a file (/tmp/foo) and the second removes it.  
 * In each case, a dialog pops up asking for verification of the action.
 *
 * This program is intended to demonstrate an advanced implementation
 * of the AskUser() function.  This time, the function is passed the
 * strings to use for the OK button and the Cancel button as well as
 * the button to use as the default value.
 */
#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

#define YES 1
#define NO  2

/* Generalize the question/answer process by creating a data structure
 * that has the necessary labels, questions and everything needed to
 * execute a command.
 */
typedef struct {
    char *label;    /* label for pushbutton used to invoke cmd */
    char *question; /* question for dialog box to confirm cmd */
    char *yes;      /* what the "OK" button says */
    char *no;       /* what the "Cancel" button says */
    int   dflt;     /* which should be the default answer */
    char *cmd;      /* actual command to execute (using system()) */
} QandA;

QandA touch_foo = {
    "Create", "Create /tmp/foo?", "Yes", "No", YES, "touch /tmp/foo"
};
QandA rm_foo = {
    "Remove", "Remove /tmp/foo?", "Yes", "No", NO, "rm /tmp/foo"
};

XtAppContext app;

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, button, rowcolumn;
    XmString label;
    void pushed();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcolumn = XtVaCreateManagedWidget ("rowcolumn",
        xmRowColumnWidgetClass, toplevel, NULL);

    label = XmStringCreateLocalized (touch_foo.label);
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, rowcolumn,
        XmNlabelString,          label,
        NULL);
    XtAddCallback (button, XmNactivateCallback, pushed, &touch_foo);
    XmStringFree (label);

    label = XmStringCreateLocalized (rm_foo.label);
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, rowcolumn,
        XmNlabelString,          label,
        NULL);
    XtAddCallback (button, XmNactivateCallback, pushed, &rm_foo);
    XmStringFree (label);

    XtManageChild (rowcolumn);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* pushed() --when a button is pressed, ask the question described
 * by the QandA parameter (client_data).  Execute the cmd if YES.
 */
void
pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    QandA *quest = (QandA *) client_data;

    if (AskUser (widget, quest->question, quest->yes, quest->no,
            quest->dflt) == YES) {
        printf ("Executing: %s\n", quest->cmd);
        system (quest->cmd);
    } else
        printf ("Not executing: %s\n", quest->cmd);
}

/*
 * AskUser() -- a generalized routine that asks the user a question
 * and returns a response.  Parameters are: the question, the labels
 * for the "Yes" and "No" buttons, and the default selection to use.
 */
AskUser(parent, question, ans1, ans2, default_ans)
Widget parent;
char *question, *ans1, *ans2;
int default_ans;
{
    static Widget dialog; /* static to avoid multiple creation */
    XmString text, yes, no;
    static int answer;
    extern void response();

    if (!dialog) {
        dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
        XtVaSetValues (dialog,
            XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
            NULL);
        XtSetSensitive (
            XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON),
            False);
        XtAddCallback (dialog, XmNokCallback, response, &answer);
        XtAddCallback (dialog, XmNcancelCallback, response, &answer);
    }
    answer = 0;
    text = XmStringCreateLocalized (question);
    yes = XmStringCreateLocalized (ans1);
    no = XmStringCreateLocalized (ans2);
    XtVaSetValues (dialog,
        XmNmessageString,      text,
        XmNokLabelString,      yes,
        XmNcancelLabelString,  no,
        XmNdefaultButtonType,  default_ans == YES ?
            XmDIALOG_OK_BUTTON : XmDIALOG_CANCEL_BUTTON,
        NULL);
    XmStringFree (text);
    XmStringFree (yes);
    XmStringFree (no);
    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);

    while (answer == 0)
        XtAppProcessEvent (app, XtIMAll);

    XtPopdown (XtParent (dialog));
    /* make sure the dialog goes away before returning. Sync with server
     * and update the display.
     */
    XSync (XtDisplay (dialog), 0);
    XmUpdateDisplay (parent);

    return answer;
}

/* response() --The user made some sort of response to the
 * question posed in AskUser().  Set the answer (client_data)
 * accordingly.
 */
void
response(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int *answer = (int *) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_OK)
        *answer = YES;
    else if (cbs->reason == XmCR_CANCEL)
        *answer = NO;
}

