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

/* warn_msg.c -- display a very urgent warning message.
 * Really catch the user's attention by flashing an urgent-
 * looking pixmap every 250 milliseconds.
 * The program demonstrates how to set the XmNsymbolPixmap
 * resource, how to destroy the pixmap and how to use timers.
 */
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

#include "bang0.symbol"
#include "bang1.symbol"

#define TEXT "Alert!\n\
The computer room is ON FIRE!\n\
All of your e-mail will be lost."

/* define the data structure we need to implement flashing effect */
typedef struct {
    XtIntervalId   id;
    int            which;
    Pixmap         pix1, pix2;
    Widget         dialog;
    XtAppContext   app;
} TimeOutClientData;

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget toplevel, button;
    XmString label;
    void warning();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    label = XmStringCreateLocalized (
        "Don't Even Think About Pressing This Button");
    button = XtVaCreateManagedWidget ("button",
        xmPushButtonWidgetClass, toplevel,
        XmNlabelString,          label,
        NULL);
    XmStringFree (label);

    /* set up callback to popup warning */
    XtAddCallback (button, XmNactivateCallback, warning, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* warning() -- callback routine for the button.  Create a message
 * dialog and set the message string.  Allocate an instance of
 * the TimeOutClientData structure and set a timer to alternate
 * between the two pixmaps.  The data is passed to the timeout
 * routine and the callback for when the user presses "OK".
 */
void
warning(parent, client_data, call_data)
Widget parent;
XtPointer client_data;
XtPointer call_data;
{
    Widget        dialog;
    XtAppContext  app = XtWidgetToApplicationContext (parent);
    XmString      text;
    extern void   done(), destroy_it(), blink();
    Display      *dpy = XtDisplay (parent);
    Screen       *screen = XtScreen (parent);
    Pixel         fg, bg;
    Arg           args[5];
    int           n, depth;
    TimeOutClientData *data = XtNew (TimeOutClientData);

    /* Create the dialog */
    n = 0;
    XtSetArg (args[n], XmNdeleteResponse, XmDESTROY); n++;
    dialog = XmCreateMessageDialog (parent, "danger", args, n);

    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));

    XtAddCallback (dialog, XmNokCallback, done, NULL);
    XtAddCallback (dialog, XmNdestroyCallback, destroy_it, data);

    /* now that dialog has been created, it's colors are initialized */
    XtVaGetValues (dialog,
        XmNforeground, &fg,
        XmNbackground, &bg,
        XmNdepth, &depth,
        NULL);

    /* Create pixmaps that are going to be used as symbolPixmaps.
     * Use the foreground and background colors of the dialog.
     */
    data->pix1 = XCreatePixmapFromBitmapData (dpy, XtWindow (parent),
        bang0_bits, bang0_width, bang0_height, fg, bg, depth);
    data->pix2 = XCreatePixmapFromBitmapData (dpy, XtWindow (parent),
        bang1_bits, bang1_width, bang1_height, fg, bg, depth);
    /* complete the timeout client data */
    data->dialog = dialog;
    data->app = app;

    /* Add the timeout for blinking effect */
    data->id = XtAppAddTimeOut (app, 1000L, blink, data);

    /* display the help text and the appropriate pixmap */
    text = XmStringCreateLtoR (TEXT, XmFONTLIST_DEFAULT_TAG);
    XtVaSetValues (dialog,
        XmNmessageString,      text,
        XmNsymbolPixmap,       data->pix2,
        NULL);
    XmStringFree (text);

    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
}

/* blink() -- visual blinking effect for dialog's symbol.  Displays
 * flashing ! symbol, restarts timer and saves timer id.
 */
void
blink(client_data, id)
XtPointer client_data;
XtIntervalId *id;
{
    TimeOutClientData *data = (TimeOutClientData *) client_data;

    data->id = XtAppAddTimeOut (data->app, 250L, blink, data);
    XtVaSetValues (data->dialog,
        XmNsymbolPixmap,  (data->which = !data->which) ?
            data->pix1 : data->pix2,
        NULL);
}

/* done() -- called when user presses "OK" in dialog or
 * if the user picked the Close button in system menu.
 * Remove the timeout id stored in data, free pixmaps and
 * make sure the widget is destroyed (which is only when
 * the user presses the "OK" button.
 */
void
done(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    XtDestroyWidget(dialog);
}

/* destroy_it() -- called when dialog is destroyed.  Removes
 * timer and frees allocated data.
 */
void
destroy_it(dialog, client_data, call_data)
Widget dialog;
XtPointer client_data;
XtPointer call_data;
{
    TimeOutClientData *data = (TimeOutClientData *) client_data;
    Pixmap symbol;

    XtRemoveTimeOut (data->id);
    XFreePixmap (XtDisplay (data->dialog), data->pix1);
    XFreePixmap (XtDisplay (data->dialog), data->pix2);
    XtFree (data);
}
