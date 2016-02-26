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

/* select_dlg.c -- display two pushbuttons: days and months.
 * When the user selections one of them, post a selection
 * dialog that displays the actual days or months accordingly.
 * When the user selects or types a selection, post a dialog
 * the identifies which item was selected and whether or not
 * the item is in the list.
 *
 * This program demonstrates how to use selection boxes,
 * methods for creating generic callbacks for action area
 * selections, abstraction of data structures, and a generic
 * MessageDialog posting routine.
 */
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>

Widget PostDialog();

char *days[] = {
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
};
char *months[] = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};
typedef struct {
    char *label;
    char **strings;
    int size;
} ListItem;

ListItem month_items = { "Months", months, XtNumber (months) };
ListItem days_items = { "Days", days, XtNumber (days) };

/* main() --create two pushbuttons whose callbacks pop up a dialog */
main(argc, argv)
char *argv[];
{
    Widget toplevel, button, rc;
    XtAppContext app;
    void pushed();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    rc = XtVaCreateWidget ("rowcolumn", 
        xmRowColumnWidgetClass, toplevel, NULL);

    button = XtVaCreateManagedWidget (month_items.label,
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback (button, XmNactivateCallback, pushed, &month_items);

    button = XtVaCreateManagedWidget (days_items.label,
        xmPushButtonWidgetClass, rc, NULL);
    XtAddCallback (button, XmNactivateCallback, pushed, &days_items);

    XtManageChild (rc);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* pushed() --the callback routine for the main app's pushbutton.
 * Create a dialog containing the list in the items parameter.
 */
void
pushed(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Widget dialog;
    XmString t, *str;
    int i;
    extern void dialog_callback();
    ListItem *items = (ListItem *) client_data;

    str = (XmString *) XtMalloc (items->size * sizeof (XmString));
    t = XmStringCreateLocalized (items->label);
    for (i = 0; i < items->size; i++)
        str[i] = XmStringCreateLocalized (items->strings[i]);
    dialog = XmCreateSelectionDialog (widget, "selection", NULL, 0);
    XtVaSetValues (dialog,
        XmNlistLabelString, t,
        XmNlistItems,       str,
        XmNlistItemCount,   items->size,
        XmNmustMatch,       True,
        NULL);
    XtSetSensitive (
        XmSelectionBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    XtAddCallback (dialog, XmNokCallback, dialog_callback, NULL);
    XtAddCallback (dialog, XmNnoMatchCallback, dialog_callback, NULL);
    XmStringFree (t);
    while (--i >= 0)
        XmStringFree (str[i]); /* free elements of array */
    XtFree (str); /* now free array pointer */
    XtManageChild (dialog);

    XtPopup (XtParent (dialog), XtGrabNone);
}

/* dialog_callback() --The OK button was selected or the user 
 * input a name by himself.  Determine whether the result is
 * a valid name by looking at the "reason" field.
 */
void
dialog_callback(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    char msg[256], *prompt, *value;
    int dialog_type;
    XmSelectionBoxCallbackStruct *cbs = 
        (XmSelectionBoxCallbackStruct *) call_data;

    switch (cbs->reason) {
        case XmCR_OK:
            prompt = "Selection: ";
            dialog_type = XmDIALOG_MESSAGE;
            break;
        case XmCR_NO_MATCH:
            prompt = "Not a valid selection: ";
            dialog_type = XmDIALOG_ERROR;
            break;
        default:
            prompt = "Unknown selection: ";
            dialog_type = XmDIALOG_ERROR;
    }
    XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
    sprintf (msg, "%s%s", prompt, value);
    XtFree (value);
    (void) PostDialog (XtParent (XtParent (widget)), dialog_type, msg);
    if (cbs->reason != XmCR_NO_MATCH) {
        XtPopdown (XtParent (widget));
        XtDestroyWidget (widget); 
    }
}

/*
 * PostDialog() -- a generalized routine that allows the programmer
 * to specify a dialog type (message, information, error, help,
 * etc..), and the message to show.
 */
Widget
PostDialog(parent, dialog_type, msg)
Widget parent;
int dialog_type;
char *msg;
{
    Widget dialog;
    XmString text;

    dialog = XmCreateMessageDialog (parent, "dialog", NULL, 0);
    text = XmStringCreateLocalized (msg);
    XtVaSetValues (dialog,
        XmNdialogType,    dialog_type,
        XmNmessageString, text,
        NULL);
    XmStringFree (text);
    XtUnmanageChild (
        XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
    XtSetSensitive (
        XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON), False);
    XtAddCallback (dialog, XmNokCallback, XtDestroyWidget, NULL);
    XtManageChild (dialog);
    return dialog;
}
