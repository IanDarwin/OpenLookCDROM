#ifndef lint
static char rcsid[] = "modal.c,v 2.0 1994/05/19 02:01:20 dan Exp";
#endif

/*
 * Copyright (c) 1994    Daniel Williams
 * 
 * The X Consortium, and any party obtaining a copy of these files from
 * the X Consortium, directly or indirectly, is granted, free of charge,
 * a full and unrestricted irrevocable, world-wide, paid up,
 * royalty-free, nonexclusive right and license to deal in this software
 * and documentation files (the "Software"), including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons
 * who receive copies from any such party to do so.  This license
 * includes without limitation a license to do the foregoing actions
 * under any patents of the party supplying this software to the X
 * Consortium.  The following conditions apply:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DANIEL WILLIAMS OR SYSTEMS & SCIENTIFIC SOFTWARE BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * this module is derived from code in "Motif Programming Manual" 
 * which bears the following notice:
 * 
 * Written by Dan Heller.  Copyright 1991, O'Reilly & Associates.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

#include <stdio.h>
#include <Xm/MessageB.h>
#include "mgdiff.h"
#include "externs.h"

static void ok_cb (Widget w, XtPointer closure, XtPointer call_data);
static void cancel_cb (Widget w, XtPointer closure, XtPointer call_data);

typedef enum {
    Busy, Ok, Cancel
} State;

/* ARGSUSED */
static void ok_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    *((State *) closure) = Ok;
    XtDestroyWidget (get_top_shell (w));
}

/* ARGSUSED */
static void cancel_cb (Widget w, XtPointer closure, XtPointer call_data)
{
    *((State *) closure) = Cancel;
    XtDestroyWidget (get_top_shell (w));
}

/* 
 * pop up a modal QuestionDialog and return True for user's choice of 
 * "OK" and False for "Cancel"
 */
int modal_question (Widget parent, char *title, char *question)
{
    static Arg args[] = {{XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL}};
    XtAppContext app = XtWidgetToApplicationContext (parent);
    State answer = Busy;
    Widget dialog;

    dialog = XmCreateQuestionDialog (parent, "question", args, XtNumber (args));
    XtAddCallback (dialog, XmNokCallback, ok_cb, &answer);
    XtAddCallback (dialog, XmNcancelCallback, cancel_cb, &answer);
    XtVaSetValues (dialog, 
		   XtVaTypedArg, XmNmessageString, XmRString, question, strlen (question)+1,
		   XtVaTypedArg, XmNdialogTitle, XmRString, title, strlen (title)+1,
		   NULL);

    XtSetSensitive (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    XtManageChild (dialog);

    while ((answer == Busy) || XtAppPending (app))
	XtAppProcessEvent (app, XtIMAll);
    return (answer == Ok);
}
