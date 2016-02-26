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

/* cut_paste.c -- demonstrate the text functions that handle
 * clipboard operations.  These functions are convenience routines
 * that relieve the programmer of the need to use clipboard functions.
 * The functionality of these routines already exists in the Text
 * widget, yet it is common to place such features in the interface
 * via the MenuBar's "Edit" pulldown menu.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>

Widget text_w, text_output;

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, main_w, menubar, rowcol_v;
    XtAppContext  app;
    void          cut_paste();
    XmString      label, cut, clear, copy, paste;
    Arg           args[10];
    int           n;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    main_w = XtVaCreateWidget ("main_w",
        xmMainWindowWidgetClass, toplevel, NULL);

    /* Create a simple MenuBar that contains a single menu */
    label = XmStringCreateLocalized ("Edit");
    menubar = XmVaCreateSimpleMenuBar (main_w, "menubar",
        XmVaCASCADEBUTTON, label, 'E',
        NULL);
    XmStringFree (label);

    cut = XmStringCreateLocalized ("Cut");      /* create a simple    */
    copy = XmStringCreateLocalized ("Copy");    /* pulldown menu that */
    clear = XmStringCreateLocalized ("Clear");  /* has these menu     */
    paste = XmStringCreateLocalized ("Paste");  /* items in it.       */
    XmVaCreateSimplePulldownMenu (menubar, "edit_menu", 0, cut_paste,
        XmVaPUSHBUTTON, cut, 't', NULL, NULL,
        XmVaPUSHBUTTON, copy, 'C', NULL, NULL,
        XmVaPUSHBUTTON, paste, 'P', NULL, NULL,
        XmVaSEPARATOR,
        XmVaPUSHBUTTON, clear, 'l', NULL, NULL,
        NULL);
    XmStringFree (cut);
    XmStringFree (clear);
    XmStringFree (copy);
    XmStringFree (paste);

    XtManageChild (menubar);

    /* create a standard vertical RowColumn... */
    rowcol_v = XtVaCreateWidget ("rowcol_v",
        xmRowColumnWidgetClass, main_w, NULL);

    text_output = XtVaCreateManagedWidget ("text_output",
        xmTextWidgetClass, rowcol_v,
        XmNeditable,              False,
        XmNcursorPositionVisible, False,
        XmNshadowThickness,       0,
        XmNhighlightThickness,    0,
        NULL);

    n = 0;
    XtSetArg (args[n], XmNrows,      10); n++;
    XtSetArg (args[n], XmNcolumns,   80); n++;
    XtSetArg (args[n], XmNeditMode,  XmMULTI_LINE_EDIT); n++;
    XtSetArg (args[n], XmNscrollHorizontal,  False); n++;
    XtSetArg (args[n], XmNwordWrap,  True); n++;
    text_w = XmCreateScrolledText (rowcol_v, "text_w", args, n);
    XtManageChild (text_w);

    XtManageChild (rowcol_v);
    XtManageChild (main_w);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* cut_paste() -- the callback routine for the items in the edit menu */
void
cut_paste(widget, client_data, call_data)
Widget widget;  
XtPointer client_data;
XtPointer call_data;
{
    Boolean result = True;
    int reason = (int) client_data;
    XEvent *event = ((XmPushButtonCallbackStruct *) call_data)->event;
    Time when;

    XmTextSetString (text_output, NULL);   /* clear message area */

    if (event != NULL) {
        switch (event->type) {
            case ButtonRelease :
                when = event->xbutton.time;
                break;
            case KeyRelease :
                when = event->xkey.time;
                break;
            default:
                when = CurrentTime;
                break;
        }
    }

    switch (reason) {
        case 0 : 
            result = XmTextCut (text_w, when); 
            break;
        case 1 : 
            result = XmTextCopy (text_w, when); 
            break;
        case 2 : 
            result = XmTextPaste (text_w);
        case 3 : 
            XmTextClearSelection (text_w, when); 
            break;
    }
    if (result == False)
        XmTextSetString (text_output, "There is no selection.");
    else
        XmTextSetString (text_output, NULL);
}
