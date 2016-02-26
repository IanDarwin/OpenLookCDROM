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

/* busy.c -- demonstrate how to use a WorkingDialog and to process
 * only important events.  e.g., those that may interrupt the
 * task or to repaint widgets for exposure.  Set up a simple shell
 * and a widget that, when pressed, immediately goes into its own
 * loop.  Set a timeout cursor on the shell and pop up a WorkingDialog.  
 * Then enter loop and sleep for one second ten times, checking between 
 * each interval to see if the user clicked the Stop button or if 
 * any widgets need to be refreshed.  Ignore all other events.
 *
 * main() and get_busy() are stubs that would be replaced by a real
 * application; all other functions can be used as is.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <X11/cursorfont.h>

Widget shell;
void TimeoutCursors();
Boolean CheckForInterrupt();

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget button;
    XmString label;
    void get_busy();

    XtSetLanguageProc (NULL, NULL, NULL);

    shell = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    label = XmStringCreateLocalized ("Press Here To Start A Long Task");
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, shell,
        XmNlabelString,          label,
        NULL);
    XmStringFree (label);
    XtAddCallback (button, XmNactivateCallback, get_busy, NULL);

    XtRealizeWidget (shell);
    XtAppMainLoop (app);
}

void
get_busy(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int n;

    TimeoutCursors (True, True);
    for (n = 0; n < 10; n++) {
        sleep (1);
        if (CheckForInterrupt ()) {
            puts ("Interrupt!");
            break;
        }
    }
    if (n == 10)
        puts ("Done");
    TimeoutCursors (False, False);
}

/* The interesting part of the program -- extract and use at will */

static Boolean stopped;  /* True when user wants to stop task */
static Widget dialog;    /* WorkingDialog displayed */

/* TimeoutCursors() -- turns on the watch cursor over the application
 * to provide feedback for the user that she's going to be waiting
 * a while before she can interact with the application again.
 */
void
TimeoutCursors(on, interruptable)
Boolean on, interruptable;
{
    static int locked;
    static Cursor cursor;
    extern Widget shell;
    XSetWindowAttributes attrs;
    Display *dpy = XtDisplay (shell);
    XEvent event;
    Arg args[5];
    int n;
    XmString str;
    extern void stop();

    /* "locked" keeps track if we've already called the function.
     * This allows recursion and is necessary for most situations.
     */
    if (on) 
        locked++;
    else 
        locked--;
    if (locked > 1 || locked == 1 && on == 0)
        return; /* already locked and we're not unlocking */

    stopped = False;
    if (!cursor) 
        cursor = XCreateFontCursor (dpy, XC_watch);

    /* if on is true, then turn on watch cursor, otherwise, return
     * the shell's cursor to normal.
     */
    attrs.cursor = on ? cursor : None;

    /* change the main application shell's cursor to be the timeout
     * cursor or to reset it to normal.  If other shells exist in
     * this application, they will have to be listed here in order
     * for them to have timeout cursors too.
     */
    XChangeWindowAttributes (dpy, XtWindow (shell), CWCursor, &attrs);

    XFlush (dpy);

    if (on) {
        /* we're timing out, put up a WorkingDialog.  If the process
         * is interruptable, allow a "Stop" button.  Otherwise, remove
         * all actions so the user can't stop the processing.
         */
        n = 0;
        str = XmStringCreateLocalized ("Busy -- Please Wait.");
        XtSetArg (args[n], XmNmessageString, str); n++;
        dialog = XmCreateWorkingDialog (shell, "busy", args, n);
        XmStringFree (str);
        XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_OK_BUTTON));
        XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
        if (interruptable) {
            str = XmStringCreateLocalized ("Stop");
            XtVaSetValues (dialog, XmNcancelLabelString, str, NULL);
            XmStringFree (str);
            XtAddCallback (dialog, XmNcancelCallback, stop, NULL);
        } 
        else
            XtUnmanageChild (XmMessageBoxGetChild 
                (dialog, XmDIALOG_CANCEL_BUTTON));
        XtManageChild (dialog);
    } 
    else {
        /* get rid of all button and keyboard events that occured
         * during the time out.  The user shouldn't have done anything
         * during this time, so flush for button and keypress events.
         * KeyRelease events are not discarded because accelerators
         * require the corresponding release event before normal input
         * can continue.
         */
        while (XCheckMaskEvent (dpy,
                ButtonPressMask | ButtonReleaseMask | ButtonMotionMask
                | PointerMotionMask | KeyPressMask, &event)) {
            /* do nothing */;
        }
        XtDestroyWidget (dialog);
    }
}

/* stop() -- user pressed the "Stop" button in dialog. */
void
stop(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    stopped = True;
}

/* CheckForInterrupt() -- check events in event queue and process
 * the interesting ones.
 */
Boolean
CheckForInterrupt()
{
    extern Widget shell;
    Display *dpy = XtDisplay (shell);
    Window win = XtWindow (dialog);
    XEvent event;

    /* Make sure all our requests get to the server */
    XFlush (dpy);

    /* Let Motif process all pending exposure events for us. */
    XmUpdateDisplay (shell);

    /* Check the event loop for events in the dialog ("Stop"?) */
    while (XCheckMaskEvent (dpy,
           ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
           PointerMotionMask | KeyPressMask, &event)) {
        /* got an "interesting" event. */
        if (event.xany.window == win)
            XtDispatchEvent (&event); /* it's in our dialog.. */
        else /* uninteresting event--throw it away and sound bell */
            XBell (dpy, 50);
    }
    return stopped;
}
