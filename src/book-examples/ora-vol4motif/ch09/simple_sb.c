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

/* simple_sb.c -- demonstrate the Scrollbar resource values from
 * a ScrolledText object.  This is used as an introductory examination
 * of the resources used by Scrollbars.
 */
#include <Xm/ScrolledW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushBG.h>
#include <Xm/Text.h>

/* print the "interesting" resource values of a scrollbar */
void
get_sb(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    Widget scrollbar = (Widget) client_data;
    int increment=0, maximum=0, minimum=0;
    int page_incr=0, slider_size=0, value=0;

    XtVaGetValues (scrollbar,
        XmNincrement,     &increment,
        XmNmaximum,       &maximum,
        XmNminimum,       &minimum,
        XmNpageIncrement, &page_incr,
        XmNsliderSize,    &slider_size,
        XmNvalue,         &value,
        NULL);
    printf ("increment=%d, max=%d, min=%d, page=%d, slider=%d, value=%d\n",
        increment, maximum, minimum, page_incr, slider_size, value);
}

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, rowcol, text_w, pb, sb;
    XtAppContext  app;
    Arg           args[10];
    int           n = 0;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    /* RowColumn contains ScrolledText and PushButton */
    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    XtSetArg (args[n], XmNrows,      10); n++;
    XtSetArg (args[n], XmNcolumns,   80); n++;
    XtSetArg (args[n], XmNeditMode,  XmMULTI_LINE_EDIT); n++;
    XtSetArg (args[n], XmNscrollHorizontal,  False); n++;
    XtSetArg (args[n], XmNwordWrap,  True); n++;
    text_w = XmCreateScrolledText (rowcol, "text_w", args, n);
    XtManageChild (text_w);

    /* get the scrollbar from ScrolledWindow associated with Text widget */
    XtVaGetValues (XtParent (text_w), XmNverticalScrollBar, &sb, NULL);

    /* provide a pushbutton to obtain the scrollbar's resource values */
    pb = XtVaCreateManagedWidget ("Print ScrollBar Values",
        xmPushButtonGadgetClass, rowcol, NULL);
    XtAddCallback (pb, XmNactivateCallback, get_sb, sb);

    XtManageChild (rowcol);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
