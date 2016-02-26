/* Written by Dan Heller.  Copyright 1991, 1994, Z-Code Software Corp.
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

/* zcard.c -- a postcard interface for zmail.
 */
#include <stdio.h>
#include <Xm/List.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Text.h>

#include "zcard.icon"

/* redefine to "mush" or "Mail" if you don't have Z-Mail */
#define MAIL_CMD     "mush"

extern char *strcpy();
Widget list_w, text_w, to_w, subj_w;
Widget CreateLabeledTextForm();
void add_user(), send_it(), add_to_to(), move();

/* These only take effect if the app-defaults file is not found */
String fallback_resources[] = {
    "*XmText.fontList: -*-courier-medium-r-*--12-*",
    "*XmText.translations: #override \
        Ctrl<Key>D: activate() \n\
        Ctrl<Key>U: kill-to-start-of-line() \n\
        Ctrl<Key>W: delete-previous-word() \n\
        <Key>osfDelete: delete-previous-character()",
    "*msg-text.rows: 15",
    "*msg-text.columns: 35",
    "*XmPushButton.fontList: -*-new century schoolbook-bold-r-*--12-*",
    "*XmPushButtonGadget.fontList: -*-new century schoolbook-bold-r-*--12-*",
    "*XmLabelGadget.fontList: -*-new century schoolbook-bold-r-*--12-*",
    "*XmList.fontList: -*-courier-medium-r-*--12-*",
    "*zcard.labelString: Z-Card",
    "*title.labelString: Quick Message Sender",
    "*actions*leftAttachment: attach_position",
    "*actions*rightAttachment: attach_position",
    "*to-label.labelString: To:",
    "*to-list.visibleItemCount: 6",
    "*subject-label.labelString: Subject:",
    "*add-btn.labelString: Add",
    "*delete-btn.labelString: Delete",
    "*send-btn.labelString: Send",
    "*quit-btn.labelString: Quit",
    "*error.messageString: You must provide at least one message recipient.",
    NULL
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, label, left, heading, icon, titles;
    Widget actions, rc, w, send_w;
    XtAppContext app;
    Arg args[5];
    int n;
    Pixel fg, bg;
    Pixmap pixmap;
    extern void exit();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Zcard", NULL, 0,
        &argc, argv, fallback_resources,
        XmNallowShellResize,  True,
        NULL);

    /* The form is the general layout manager for the application.
     * It contains two main widgets: a rowcolumn and a scrolled text.
     */
    rc = XtVaCreateWidget ("rc", 
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    /* left side is a RowColumn -- a child of the bigger RowColumn */
    left = XtVaCreateWidget ("left", xmRowColumnWidgetClass, rc, NULL);

    /* start the left side with a Form to hold the heading */
    heading = XtVaCreateWidget ("heading", xmFormWidgetClass, left, NULL);

    /* create an icon to make things pretty */
    XtVaGetValues (heading, 
        XmNforeground, &fg, 
        XmNbackground, &bg, 
        NULL);
    pixmap = XCreatePixmapFromBitmapData (XtDisplay (heading),
        RootWindowOfScreen (XtScreen (heading)),
        /* these values are defined in "zcard.icon" */
        zcard_logo_bits, zcard_logo_width, zcard_logo_height,
        fg, bg, DefaultDepthOfScreen (XtScreen (heading)));
    icon = XtVaCreateManagedWidget ("zcard_icon", 
        xmLabelGadgetClass, heading,
        XmNleftAttachment,  XmATTACH_FORM,
        XmNlabelType,       XmPIXMAP,
        XmNlabelPixmap,     pixmap,
        XmNalignment,       XmALIGNMENT_END,
        NULL);

    /* identify the program */
    titles = XtVaCreateWidget ("titles",
        xmRowColumnWidgetClass, heading,
        XmNrightAttachment,  XmATTACH_FORM,
        XmNleftAttachment,   XmATTACH_WIDGET,
        XmNleftWidget,       icon,
        XmNtopAttachment,    XmATTACH_FORM,
        XmNbottomAttachment, XmATTACH_FORM,
        NULL);
    XtVaCreateManagedWidget ("zcard", xmLabelGadgetClass, titles, NULL);
    XtVaCreateManagedWidget ("title", xmLabelGadgetClass, titles, NULL);
    XtManageChild (titles);
    XtManageChild (heading);

    /* provide the "To:" prompt (see the resources above) */
    to_w = CreateLabeledTextForm (left, "to-label", "to");

    /* prompt for the subject (see the resources above) */
    subj_w = CreateLabeledTextForm (left, "subject-label", "subject-text");

    /* when user hits <Return>, advance caret to next input item */
    XtAddCallback (subj_w, XmNactivateCallback, move, NULL);

    /* right side is a scrolled text region for letter input. */
    n = 0;
    XtSetArg (args[n], XmNeditMode,          XmMULTI_LINE_EDIT); n++;
    XtSetArg (args[n], XmNscrollVertical,    True); n++;
    XtSetArg (args[n], XmNscrollHorizontal,  True); n++;
    text_w = XmCreateScrolledText (rc, "msg-text", args, n);
    XtManageChild (text_w);

    /* Ctrl-D in text_w causes activate() which calls send_it() */
    XtAddCallback (text_w, XmNactivateCallback, send_it, send_w);

    /* Create a ScrolledList of all the recipients entered in To: */
    n = 0;
    XtSetArg (args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
    XtSetArg (args[n], XmNselectionPolicy, XmEXTENDED_SELECT); n++;
    XtSetArg (args[n], XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE); n++;
    list_w = XmCreateScrolledList (left, "to-list", args, n);
    XtAddCallback (list_w, XmNdefaultActionCallback, add_to_to, to_w);
    XtManageChild (list_w);

    /* Any command line args are recipients */
    while (argc-- > 1) {
        XmString str = XmStringCreateLocalized (*++argv);
        XmListAddItemUnselected (list_w, str, 0);
        XmStringFree (str);
    }

    /* Add, Delete, Send and Quit buttons -- space equally */
    actions = XtVaCreateWidget ("actions", xmFormWidgetClass, left, NULL);

    send_w = XtVaCreateManagedWidget ("send-btn",
        xmPushButtonWidgetClass, actions,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, 0,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, 23,
        NULL);
    XtAddCallback (send_w, XmNactivateCallback, send_it, NULL);

    w = XtVaCreateManagedWidget ("add-btn",
        xmPushButtonWidgetClass, actions,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, 26,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, 46,
        NULL);
    /* clicking on Add user adds user to scrolled list */
    XtAddCallback (w, XmNactivateCallback, add_user, (XtPointer) 1);

    /* Make it appear as tho hitting return in To: text widget
     * is just like clicking on the Add button.
     */
    XtAddCallback (to_w, XmNactivateCallback, add_user, w);

    w = XtVaCreateManagedWidget ("delete-btn",
        xmPushButtonWidgetClass, actions,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, 49,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, 75,
        NULL);
    /* clicking on delete calls add_user() with a 0 client_data */
    XtAddCallback (w, XmNactivateCallback, add_user, (XtPointer) 0);

    w = XtVaCreateManagedWidget ("quit-btn",
        xmPushButtonWidgetClass, actions,
        XmNleftAttachment, XmATTACH_POSITION,
        XmNleftPosition, 78,
        XmNrightAttachment, XmATTACH_POSITION,
        XmNrightPosition, 100,
        NULL);
    XtAddCallback (w, XmNactivateCallback, exit, NULL);
    XtManageChild (actions);

    XtManageChild (left);
    XtManageChild (rc);

    /* specify tab groups in the order we'd like tabbing to follow */
    XtVaSetValues (to_w, XmNnavigationType, XmEXCLUSIVE_TAB_GROUP, NULL);
    XtVaSetValues (subj_w, XmNnavigationType, XmEXCLUSIVE_TAB_GROUP, NULL);
    XtVaSetValues (text_w, XmNnavigationType, XmEXCLUSIVE_TAB_GROUP, NULL);
    XtVaSetValues (actions, XmNnavigationType, XmEXCLUSIVE_TAB_GROUP, NULL);
    XtVaSetValues (list_w, XmNnavigationType, XmEXCLUSIVE_TAB_GROUP, NULL);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* add_user() -- add an address to the list of recipients.
 * The user clicked on either Add or Delete buttons, or he hit return in
 * the To: text field.  In the latter case, client data is the add_btn,
 * so call that widget's ArmAndActivate() action proc.
 */
void
add_user(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int data = (int) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;

    if (w == to_w) {
        /* User hit return... make it look as tho he clicked on Add */
        XtCallActionProc (data, "ArmAndActivate", cbs->event, NULL, 0);
        return;
    }

    /* User clicked on Add if data == 1, or delete otherwise */
    if (data) {
        /* get the value of the To: text widget */
        char *text = XmTextGetString (to_w);
        XmString str = XmStringCreateLocalized (text);
        if (text && *text) /* if not a null string, add to List */
            XmListAddItemUnselected (list_w, str, 0);
        XmStringFree (str);
        XtFree (text);
        XmTextSetString (to_w, NULL); /* reset so user can add more */
    } 
    else {
        /* user clicked on Delete; delete all selected names */
        int *sel, n;
        if (!XmListGetSelectedPos (list_w, &sel, &n))
            return;
        /* Must delete in reverse order or positions get messed up! */
        while (n--)
            XmListDeletePos (list_w, sel[n]);
        XtFree (sel);
    }
}

/* add_to_to() -- callback for double-clicking a list item that 
 * causes the selected item to be added to To: text.  Now
 * the user can edit the address. 
 */
void
add_to_to(list_w, client_data, call_data)
Widget list_w;
XtPointer client_data;
XtPointer call_data;
{
    Widget to_w = (Widget) client_data;
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
    char *text;

    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &text);
    XmTextSetString (to_w, text);
    XmTextSetInsertionPosition (to_w, strlen(text));
    XtFree (text);
    XmListDeletePos (list_w, cbs->item_position);
    /* it's a long way, but traverse to To: text field */
    XmProcessTraversal (list_w, XmTRAVERSE_NEXT_TAB_GROUP);
}

/* send_it() -- callback for when user clicked on Send.  Build 
 * a command line, use popen() to open pipe to mail command, send 
 * text data to it and then exit.  The message is sent to all
 * of the addresses that have been specified and are shown in the
 * list.
 */
void
send_it(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget send_w; 
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
    char *text, *subj, cmd[BUFSIZ], *p, *dummy, *getenv();
    int n, i, status;
    XmString *list;
    FILE *pp, *popen();

    if (w == text_w) {
        send_w = (Widget) client_data;
        XtCallActionProc (send_w, "ArmAndActivate", cbs->event, NULL, 0);
        return;
    }
    
    /* if something was left in the To: field, grab it */
    text = XmTextGetString (to_w);
    if (text != 0 && *text != 0) {
        XmString str = XmStringCreateLocalized (text);
        XmListAddItemUnselected (list_w, str, 0);
        XmTextSetString (to_w, "");
        XmStringFree (str);
        XtFree (text);
    }

    /* Get the list of users entered */
    XtVaGetValues (list_w,
        XmNitems, &list,
        XmNitemCount, &n,
        NULL);
    if (n == 0) {
        static Widget dialog;
        /* user goofed -- must provide at least one recipient */
        if (!dialog) {
            Arg args[5];
            n = 0;
            XtSetArg (args[n], XmNdialogStyle, 
                XmDIALOG_APPLICATION_MODAL); n++;
            dialog = XmCreateErrorDialog (to_w, "error", args, n);
            XtUnmanageChild (
                XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
            XtUnmanageChild (
                XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
        }
        XtManageChild (dialog);
        return;
    }

    /* get the subject (may be empty) */
    subj = XmTextGetString (subj_w);

    /* build command line */
    if (!(p = getenv ("MAIL_CMD")))
        p = MAIL_CMD;
    p = strcpy (cmd, p);
    p += strlen (cmd);
    *p++ = ' ';
    if (subj && *subj) {
        /* if subject not empty, add to mail command */
        sprintf (p, "-s \"%s\" ", subj);
        p += strlen (p);
    }

    /* Add each user in the List to the command line */
    for (i = 0; i < n; i++) {
        XmStringGetLtoR (list[i], XmFONTLIST_DEFAULT_TAG, &dummy);
        p += strlen (strcpy (p, dummy));
        if (i < n-1) /* more to come yet... */
            *p++ = ',', *p++ = ' '; /* separate addresses w/commas */
    }

    /* open pipe to mail command */
    if (!(pp = popen (cmd, "w"))) {
        fprintf (stderr, "Can't execute");
        perror (cmd);
        return;
    }
    /* give it the text user typed (may be empty) */
    text = XmTextGetString (text_w);
    fputs (text, pp);
    fputc ('\n', pp); /* make sure there's a terminating newline */
    status = pclose (pp); /* close mail program */

    XtFree (text);
    XtFree (subj);
    if (status == 0) {
        XmTextSetString (to_w, NULL);
        XmTextSetString (text_w, NULL);
        XmTextSetString (subj_w, NULL);
        XmListDeleteAllItems (list_w);
    }
    /* send complete -- start back at beginning */
    XmProcessTraversal (w, XmTRAVERSE_HOME);
}

/* move() -- callback for when the user hits return in the Text widget */
void
move(text_w, client_data, call_data)
Widget text_w;
XtPointer client_data;
XtPointer call_data;
{
    XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
}

/* CreateLabeledTextForm() -- create a Form widget that has a label on 
 * the left and a Text widget to the right.  Attach perimeter edges to 
 * form.  We use it twice in the program, so make a function out of it.
 */
Widget
CreateLabeledTextForm(parent, label_name, text_name)
Widget parent;
char *label_name, *text_name;
{
    Widget form, label, ret;

    form = XtVaCreateWidget ("form", 
        xmFormWidgetClass, parent,
        XmNorientation,      XmHORIZONTAL,
        NULL);
    label = XtVaCreateManagedWidget (label_name, 
        xmLabelGadgetClass, form,
        XmNleftAttachment,   XmATTACH_FORM,
        XmNtopAttachment,    XmATTACH_FORM,
        XmNbottomAttachment, XmATTACH_FORM,
        NULL);
    ret = XtVaCreateManagedWidget (text_name, 
        xmTextWidgetClass, form,
        XmNleftAttachment,   XmATTACH_WIDGET,
        XmNleftWidget,       label,
        XmNtopAttachment,    XmATTACH_FORM,
        XmNrightAttachment,  XmATTACH_FORM,
        XmNbottomAttachment, XmATTACH_FORM,
        NULL);
    XtManageChild (form);

    return ret;
}
