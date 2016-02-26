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

/* working.c -- represent a complicated, time-consuming task by
 * counting from 0 to 20000 and provide feedback to the user about
 * how far we are in the process.  The user may terminate the process
 * at any time by selecting the Stop button in the WorkingDialog.
 * This demonstrates how a WorkingDialog can be used to allow the
 * user to interrupt lengthy procedures.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

#define MAXNUM 20000

void done();

/* Global variables */
static int           i = 0;
static XtWorkProcId  work_id;

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext  app;
    Widget        toplevel, button;
    XmString      label;
    void          pushed();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    label = XmStringCreateLocalized ("Press Here To Start A Long Task");
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XtAddCallback (button, XmNactivateCallback, pushed, app);
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
    XtAppContext  app = (XtAppContext) client_data;
    Widget        dialog;
    XmString      stop_txt;
    Arg           args[5];
    int           n;
    Boolean       count();

    /* Create the dialog -- the "cancel" button says "Stop" */
    n = 0;
    stop_txt = XmStringCreateLocalized ("Stop");
    XtSetArg(args[n], XmNcancelLabelString, stop_txt); n++;
    dialog = XmCreateWorkingDialog (w, "working", args, n);
    XmStringFree (stop_txt);

    work_id = XtAppAddWorkProc (app, count, dialog);

    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_OK_BUTTON));
    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));

    /* Use cancel button to stop counting. True = remove work proc */
    XtAddCallback (dialog, XmNcancelCallback, done, True);

    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}

/* count() -- work procedure that counts to MAXNUM.  When we get there,
 * change the "Stop" button to say "Done".
 */
Boolean
count(client_data)
XtPointer client_data;
{
    Widget dialog = (Widget) client_data;
    char buf[64];
    XmString str, button;
    Boolean finished = False;

    /* If we printed every number, the flicker is too fast to read.
     * Therefore, just print every 1000 ticks for smoother feedback.
     */
    if (++i % 1000 != 0)
        return finished;

    /* display where we are in the counter. */
    sprintf (buf, "Counter: %d", i);
    str = XmStringCreateLocalized (buf);
    XtVaSetValues (dialog, XmNmessageString, str, NULL);
    XmStringFree (str);

    if (i == MAXNUM) {
        i = 0;
        finished = True;
        button = XmStringCreateLocalized ("Done");
        XtVaSetValues (dialog, XmNcancelLabelString, button, NULL);
        XmStringFree (button);
        XtRemoveCallback (dialog, XmNcancelCallback, done, True);
        XtAddCallback (dialog, XmNcancelCallback, done, False);
        XMapRaised (XtDisplay (dialog), XtWindow (XtParent (dialog)));
    }

    /* Return either True, meaning we're done and remove the work proc,
     * or False, meaning continue working by calling this function. 
     */
    return finished;
}

/* done () -- user pressed "Stop" or "Done" in WorkingDialog. */
void
done(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    Boolean remove_work_proc = (Boolean) client_data;

    if (remove_work_proc) {
        i = 0;
        XtRemoveWorkProc (work_id);
    }
    XtDestroyWidget (dialog);
}
