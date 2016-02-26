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

/* query_retrieve.c -- simple copy and retrieve program.  Two
 * pushbuttons: the first places text in the clipboard, the other
 * receives text from the clipboard.  This just demonstrates the
 * API involved.
 */
#include <Xm/CutPaste.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

static void to_clipbd(), from_clipbd_query();

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
    rowcol = XtVaCreateWidget ("rowcol", xmRowColumnWidgetClass, 
        toplevel, NULL);

    /* button1 copies to the clipboard */
    button = XtVaCreateManagedWidget ("button1",
        xmPushButtonWidgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Copy To Clipboard", 18, /* strlen() + 1 */
        NULL);
    XtAddCallback (button, XmNactivateCallback, to_clipbd, 
        "This chunk of text is retrieved in one fell swoop");

    /* button2 retrieves text stored in the clipboard */
    button = XtVaCreateManagedWidget ("button2",
        xmPushButtonWidgetClass, rowcol,
        XtVaTypedArg, XmNlabelString, XmRString,
            "Retrieve From Clipboard", 24, /* strlen() + 1 */
        NULL);
    XtAddCallback (button, XmNactivateCallback, from_clipbd_query, NULL);

    /* manage RowColumn, realize toplevel shell and start main loop */
    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* copy data to clipboard. */
static void
to_clipbd(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    unsigned long item_id = 0;  /* clipboard item id */
    int           status;
    XmString      clip_label;
    static int    cnt;
    Display      *dpy = XtDisplayOfObject (widget);
    Window        window = XtWindowOfObject (widget);
    char         *data = (char *) client_data;

    clip_label = XmStringCreateLocalized ("to_clipbd");

    /* start a copy -- retry till unlocked */
    do
        status = XmClipboardStartCopy (dpy, window,
            clip_label, CurrentTime, NULL, NULL, &item_id);
    while (status == ClipboardLocked);

    XmStringFree (clip_label);

    /* copy the data -- pass "cnt" as private id for kicks */
    do
        status = XmClipboardCopy (dpy, window, item_id, "STRING",
            data, (long) strlen (data)+1, cnt, NULL);
    while (status == ClipboardLocked);

    /* end the copy */
    do
        status = XmClipboardEndCopy (dpy, window, item_id);
    while (status == ClipboardLocked);

    printf ("Copied \"%s\" to clipboard.\n", data);
}

static void
from_clipbd_query(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    int         status, recvd, length;
    char       *data;
    Display    *dpy = XtDisplayOfObject (widget);
    Window      window = XtWindowOfObject (widget);

    do 
        status = XmClipboardInquireLength (dpy, window, "STRING", &length);
    while (status == ClipboardLocked);

    if (length == 0)
        printf ("No data on clipboard in specified format.\n");

    data = XtMalloc (length+1);

    do
        status = XmClipboardRetrieve (dpy, window,
            "STRING", data, length+1, &recvd, NULL);
    while (status == ClipboardLocked);

    if (status != ClipboardSuccess || recvd != length) {
        printf ("Failed to receive all clipboard data\n");
	XtFree (data);
    } 
    else 
        printf ("Retrieved \"%s\" from clipboard.\n", data);
}





