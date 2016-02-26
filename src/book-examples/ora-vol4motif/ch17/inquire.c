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

/* inquire.c -- inquire about the data format of the data on the
 * clipboard.
 */
#include <Xm/Xm.h>
#include <Xm/CutPaste.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

static void check_fmts(), add_fmt();

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, rowcol, button;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    /* Initialize toolkit, application context and toplevel shell */
    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    /* manage two buttons in a RowColumn widget */
    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel,
        NULL);

    /* button1 copies to the clipboard */
    button = XtVaCreateManagedWidget ("button1",
        xmPushButtonWidgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Add Format", sizeof (char *),
        NULL);
    XtAddCallback (button, XmNactivateCallback, add_fmt, NULL);

    /* button2 checks the formats known */
    button = XtVaCreateManagedWidget ("button2",
        xmPushButtonWidgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Check Known Formats", sizeof (char *),
        NULL);
    XtAddCallback (button, XmNactivateCallback, check_fmts, NULL);

    /* manage RowColumn, realize toplevel shell and start main loop */
    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

static void
add_fmt(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    unsigned long item_id, data_id;
    Display *dpy = XtDisplay (widget);
    Window window = XtWindow (widget);
    XmString label = XmStringCreateLocalized ("integer");
    int status, ten = 10;

    /* register "INTEGER" with clipboard. */
    if (XmClipboardRegisterFormat (dpy, "INTEGER", 32) == ClipboardFail) {
        XtWarning ("Can't register INTEGER");
        return;
    }

    do
	status = XmClipboardStartCopy (dpy, window, label, CurrentTime,
	    NULL, NULL, &item_id);
    while (status == ClipboardLocked);

    XmStringFree (label);

    do
        status = XmClipboardCopy (dpy, window, item_id, "INTEGER",
	    (char *) &ten, sizeof (int), NULL, &data_id);
    while (status == ClipboardLocked);

    do
        status = XmClipboardEndCopy (dpy, window, item_id);
    while (status == ClipboardLocked);
}

static void
check_fmts(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Display *dpy = XtDisplay (widget);
    Window window = XtWindow (widget);
    char *fmt_str;
    int status, len = 1, i, count, maxlen;

    do 
        status = XmClipboardInquireCount (dpy, window, &count, &maxlen);
    while (status == ClipboardLocked);

    printf ("#-formats: %d, strlen of longest format name: %d\n",
	count, maxlen);

    if (count == 0 || status != ClipboardSuccess ||
	!(fmt_str = XtMalloc (maxlen+1))) {
	XtWarning ("Can't get clipboard formats");
	return;
    }

    for (i = 1; i <= count; i++) {
	do status = XmClipboardInquireFormat (dpy, window, i,
	    fmt_str, maxlen, &len);
	while (status == ClipboardLocked);
	fmt_str[len] = 0;
	printf ("Format %d: %s\n", i, fmt_str);
    }
    XtFree (fmt_str);
}

