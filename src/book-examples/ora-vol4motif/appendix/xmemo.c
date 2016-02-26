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

/* xmemo.c -- a memo calendar program that creates a calendar on the 
 * left and a list of months on the right.  Selecting a month changes 
 * the calendar.  Selecting a day causes that date to become activated 
 * and a popup window is displayed that contains a text widget.  This
 * widget is presumably used to keep memos for that day.  You can pop 
 * up and down the window by continuing to select the date on that month.
 */
#include <stdio.h>
#include <X11/Xos.h>
#include <Xm/List.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Text.h>

int year;
void XmStringFreeTable(), date_dialog(), set_month();
Widget list_w, month_label;

typedef struct _month {
    char *name;
    Widget form, dates[6][7];
} Month;

Month months[] = { /* only initialize "known" data */
    { "January" }, { "February" }, { "March" }, { "April" },
    { "May" }, { "June" }, { "July" }, { "August" }, { "September" },
    { "October" }, { "November" }, { "December" }
};

/* These only take effect if the app-defaults file is not found */
String fallback_resources[] = {
    "*XmPushButton.fontList: -*-courier-bold-r-*--18-*",
    "*XmLabelGadget.fontList: -*-courier-bold-r-*--18-*",
    "*XmList.fontList: -*-courier-medium-r-*--18-*",
    NULL
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, frame, rowcol, rowcol2;
    XtAppContext app;
    int month;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "XMemo", NULL, 0,
        &argc, argv, fallback_resources, NULL);

    /* The form is the general layout manager for the application.
     * It will contain two widgets (the calendary and the list of months).
     * These widgets are laid out horizontally.
     */
    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    /* Place a frame around the calendar... */
    frame = XtVaCreateManagedWidget ("frame1",
        xmFrameWidgetClass, rowcol, NULL);
    /* the calendar is placed inside of a RowColumn widget */
    rowcol2 = XtVaCreateManagedWidget ("rowcol2",
        xmRowColumnWidgetClass, frame, NULL);
    /* the month label changes dynamically as each month is selected */
    month_label = XtVaCreateManagedWidget ("month_label",
        xmLabelGadgetClass, rowcol2, NULL);
    XtVaCreateManagedWidget (" Su Mo Tu  We Th  Fr Sa",
        xmLabelGadgetClass, rowcol2, NULL);

    /* Create a ScrolledText that contains the months.  You probably won't
     * see the ScrollBar unless the list is resized so that not all of
     * the month names are visible.
     */
    {
        XmString strs[XtNumber (months)];
        for (month = 0; month < XtNumber (months); month++)
            strs[month] = XmStringCreateLocalized (months[month].name);
        list_w = XmCreateScrolledList (rowcol, "list", NULL, 0);
        XtVaSetValues (list_w,
            XmNitems,      strs,
            XmNitemCount,  XtNumber (months),
            NULL);
        for (month = 0; month < XtNumber (months); month++)
            XmStringFree (strs[month]);
        XtAddCallback (list_w, XmNbrowseSelectionCallback, set_month, NULL);
        XtManageChild (list_w);
    }

    /* Determine the year we're dealing with and establish today's month */
    if (argc > 1)
        year = atoi (argv[1]);
    else {
        long time(), t = time (0);
        struct tm *today = localtime (&t);
        year = 1900 + today->tm_year;
        month = today->tm_mon + 1;
    }
    XmListSelectPos (list_w, month, True);

    XtManageChild (rowcol);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* set_month() -- callback routine for when a month is selected.
 * Each month is a separate, self-contained widget that contains the
 * dates as PushButton widgets.  New months do not overwrite old ones,
 * so the old month must be "unmanaged" before the new month is managed.
 * If the month has not yet been created, then figure out the dates and
 * which days of the week they fall on using clever math computations...
 */
void
set_month(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    XmListCallbackStruct *list_cbs = (XmListCallbackStruct *) call_data;
    char text[BUFSIZ];
    register char *p;
    int i, j, m, tot, day;
    static int month = -1;

    if (list_cbs->item_position == month + 1)
        return; /* same month, don't bother redrawing */

    if (month >= 0 && months[month].form)
        XtUnmanageChild (months[month].form); /* unmanage last month */
    month = list_cbs->item_position - 1; /* set new month */
    sprintf (text, "%s  %d", months[month].name, year);
    XtVaSetValues (month_label,
        XtVaTypedArg, XmNlabelString, XmRString, text, strlen (text) + 1,
        NULL);
    if (months[month].form) {
        /* it's already been created -- just manage and return */
        XtManageChild (months[month].form);
        return;
    }

    /* Create the month Form widget and dates PushButton widgets */
    months[month].form = XtVaCreateWidget ("month_form",
        xmRowColumnWidgetClass, XtParent (month_label),
        XmNorientation,    XmHORIZONTAL,
        XmNnumColumns,     6,
        XmNpacking,        XmPACK_COLUMN,
        NULL);

    /* calculate the dates of the month using science */
    /* day_number() takes day-of-month (1-31), returns day-of-week (0-6) */
    m = day_number (year, month + 1, 1);
    tot = days_in_month (year, month + 1);

    /* We are creating a whole bunch of PushButtons, but not all of 
     * them have dates associated with them.  The buttons that have 
     * dates get the number sprintf'ed into it.  All others get two blanks.
     */
    for (day = i = 0; i < 6; i++) {
        for (j = 0; j < 7; j++, m += (j > m && --tot > 0)) {
            char *name;
            if (j != m || tot < 1)
                name = "  ";
            else {
                sprintf(text, "%2d", ++day);
                name = text;
            }
            months[month].dates[i][j] =
                XtVaCreateManagedWidget (name,
                    xmPushButtonWidgetClass, months[month].form,
                    /* this is where we will hold the dialog later. */
                    XmNuserData,      NULL,
                    XmNsensitive,     (j % 7 == m && tot > 0),
                    XmNshadowThickness, 0,
                    NULL);
            XtAddCallback (months[month].dates[i][j],
                XmNactivateCallback, date_dialog, day);
        }
        m = 0;
    }
    XtManageChild (months[month].form);

    /* The RowColumn widget creates equally sized boxes for each child 
     * it manages.  If one child is bigger than the rest, all children 
     * are that big.  If we create all the PushButtons with a 0 shadow 
     * thickness, as soon as one PushButton is selected and its thickness 
     * is set to 2, the entire RowColumn resizes itself.  To compensate 
     * for the problem, we need to set the shadow thickness of at least 
     * one of the buttons to 2, so that the entire RowColumn is 
     * initialized to the right size.  But this will cause the button to
     * have a visible border and make it appear preselected, so, we have 
     * to make it appear invisible.  If it is invisible then it cannot be 
     * selected, but it just so happens that the last 5 days in
     * the month will never have selectable dates, so we can use any one
     * of those.  To make the button invisible, we need to unmap the 
     * widget.  We can't simply unmanage it or the parent won't consider 
     * its size, which defeats the whole purpose.  We can't create the 
     * widget and then unmap it because it has not been realized, so it 
     * does not have a window yet.  We don't want to realize and manage 
     * the entire application just to realize this one widget, so we 
     * set XmNmappedWhenManaged to False along with the shadow thickness
     * being set to 2.  Now the RowColumn is the right size.
     */
    XtVaSetValues (months[month].dates[5][6],
        XmNshadowThickness, 2,
        XmNmappedWhenManaged, False,
        NULL);
}

/* date_dialog() -- when a date is selected, this function is called.  
 * Create a dialog (toplevel shell) that contains a multiline text 
 * widget for memos about this date.
 */
void
date_dialog(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int date = (int) client_data;
    Widget dialog;
    XWindowAttributes xwa;

    /* the dialog is stored in the PushButton's XmNuserData */
    XtVaGetValues (w, XmNuserData, &dialog, NULL);
    if (!dialog) {
        /* it doesn't exist yet, create it. */
        char buf[32];
        Arg args[5];
        int n, n_pos, *list;

        /* get the month that was selected -- we just need it for its name */
        if (!XmListGetSelectedPos (list_w, &list, &n_pos))
            return;
        sprintf (buf, "%s %d %d", months[list[0]-1].name, date, year);
        XtFree (list);
        dialog = XtVaCreatePopupShell ("popup",
            topLevelShellWidgetClass, XtParent (w),
            XmNtitle,            buf,
            XmNallowShellResize, True,
            XmNdeleteResponse,   XmUNMAP,
            NULL);
        n = 0;
        XtSetArg (args[n], XmNrows,     10); n++;
        XtSetArg (args[n], XmNcolumns,  40); n++;
        XtSetArg (args[n], XmNeditMode, XmMULTI_LINE_EDIT); n++;
        XtManageChild (XmCreateScrolledText (dialog, "text", args, n));
        /* set the shadow thickness to 2 so user knows there is a memo
         * attached to this date.
         */
        XtVaSetValues (w,
            XmNuserData, dialog,
            XmNshadowThickness, 2,
            NULL);
    }
    /* See if the dialog is realized and is visible.  If so, pop it down */
    if (XtIsRealized (dialog) && XGetWindowAttributes 
            (XtDisplay (dialog), XtWindow (dialog), &xwa) &&
            xwa.map_state == IsViewable)
        XtPopdown (dialog);
    else
        XtPopup (dialog, XtGrabNone);
}

/* the rest of the file is junk to support finding the current date. */

static int mtbl[] = { 0,31,59,90,120,151,181,212,243,273,304,334,365 };

int
days_in_month(year, month)
int year, month;
{
    int days;

    days = mtbl[month] - mtbl[month - 1];
    if (month == 2 && year % 4 == 0 && (year % 100 != 0 || year % 400 == 0))
        days++;
    return days;
}

int
day_number(year, month, day)
int year, month, day;
{
    /* Lots of foolishness with casts for Xenix-286 16-bit ints */

    long days_ctr;      /* 16-bit ints overflowed Sept 12, 1989 */

    year -= 1900;
    days_ctr = ((long)year * 365L) + ((year + 3) / 4);
    days_ctr += mtbl[month - 1] + day + 6;
    if (month > 2 && (year % 4 == 0))
        days_ctr++;
    return (int) (days_ctr % 7L);
}
