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

/* error_test.c -- test the error handlers and wprint() routine
 */
#include <Xm/Text.h>
#include <Xm/MainW.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <stdio.h>
#include <varargs.h> 

void wprint();
Widget text_output;

static void
x_error(dpy, err_event)
Display      *dpy;
XErrorEvent  *err_event;
{
    char                buf[256];

    XGetErrorText (dpy, err_event->error_code, buf, (sizeof buf));

    wprint("X Error: <%s>\n", buf);
}

static void
xt_error(message)
char *message;
{
    wprint ("Xt Error: %s\n", message);
}

static void
make_error(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int which = (int) client_data;

    switch (which) {
	case 0 : 
	    XLookupColor (XtDisplay (text_output), NULL, "", NULL, NULL);
	    break;
	case 2 : 
	    XtError ("This is an XtError call!"); 
	    break;
	case 3 : 
	    XtWarning ("This is an XtWarning call."); 
	    break;
    }
}

main(argc, argv)
int argc;
char *argv[];
{
    XtAppContext app;
    Widget       toplevel, main_window, rowcol, pb;
    Arg          args[10];
    int          n;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
	NULL, 0, &argc, argv, NULL, NULL);

    main_window = XtVaCreateWidget ("main_window", 
	xmMainWindowWidgetClass, toplevel, NULL);

    rowcol = XtVaCreateWidget ("rowcol", 
	xmRowColumnWidgetClass, main_window, NULL);

    pb = XtVaCreateManagedWidget ("XLib Error",
        xmPushButtonGadgetClass, rowcol,
        NULL);
    XtAddCallback (pb, XmNactivateCallback, make_error, 0);

    pb = XtVaCreateManagedWidget ("Xt Error",
        xmPushButtonGadgetClass, rowcol,
        NULL);
    XtAddCallback (pb, XmNactivateCallback, make_error, 2);

    pb = XtVaCreateManagedWidget ("Xt Warning",
        xmPushButtonGadgetClass, rowcol,
        NULL);
    XtAddCallback (pb, XmNactivateCallback, make_error, 3);

    /* Create output_text as a ScrolledText window */
    n = 0;
    XtSetArg(args[n], XmNrows,             6); n++;
    XtSetArg(args[n], XmNcolumns,          80); n++;
    XtSetArg(args[n], XmNeditable,         False); n++;
    XtSetArg(args[n], XmNeditMode,         XmMULTI_LINE_EDIT); n++;
    XtSetArg(args[n], XmNwordWrap,         True); n++;
    XtSetArg(args[n], XmNscrollHorizontal, False); n++;
    XtSetArg(args[n], XmNcursorPositionVisible, False); n++;
    text_output = XmCreateScrolledText(rowcol, "text_output", args, n);
    XtManageChild (text_output);

    /* catch Xt errors */
    XtAppSetErrorHandler (app, xt_error);
    XtAppSetWarningHandler (app, xt_error);

    /* and Xlib errors */
    XSetErrorHandler (x_error);

    XmMainWindowSetAreas (main_window, NULL, NULL, NULL, NULL, rowcol);

    XtManageChild (rowcol);
    XtManageChild (main_window);
    XtRealizeWidget (toplevel);

    XtAppMainLoop (app);
}

/*VARARGS*/
void
wprint(va_alist)
va_dcl
{
    char msgbuf[256];
    char *fmt;
    static XmTextPosition wpr_position;
    va_list args;

    va_start (args);
    fmt = va_arg (args, char *);
#ifndef NO_VPRINTF
    (void) vsprintf (msgbuf, fmt, args);
#else /* !NO_VPRINTF */
    {
        FILE foo;
        foo._cnt = 256;
        foo._base = foo._ptr = msgbuf; /* (unsigned char *) ?? */
        foo._flag = _IOWRT+_IOSTRG;
        (void) _doprnt (fmt, args, &foo);
        *foo._ptr = '\0'; /* plant terminating null character */
    }
#endif /* NO_VPRINTF */
    va_end (args);

    XmTextInsert (text_output, wpr_position, msgbuf);
    wpr_position = wpr_position + strlen (msgbuf);
    XtVaSetValues (text_output, XmNcursorPosition, wpr_position, NULL);
    XmTextShowPosition (text_output, wpr_position);
}
