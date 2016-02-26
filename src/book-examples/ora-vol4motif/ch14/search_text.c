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

/* search_text.c -- demonstrate how to position a cursor at a
 * particular location.  The position is determined by a pattern
 * match search.
 */
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <X11/Xos.h>   /* for the index() function */

Widget text_w, search_w, text_output;

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, rowcol_v, rowcol_h;
    XtAppContext  app;
    int           i, n;
    void          search_text();
    Arg           args[10];

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol_v = XtVaCreateWidget ("rowcol_v",
        xmRowColumnWidgetClass, toplevel, NULL);

    rowcol_h = XtVaCreateWidget ("rowcol_h",
        xmRowColumnWidgetClass, rowcol_v,
        XmNorientation,  XmHORIZONTAL,
        NULL);
    XtVaCreateManagedWidget ("Search Pattern:",
        xmLabelGadgetClass, rowcol_h, NULL);
    search_w = XtVaCreateManagedWidget ("search_text",
        xmTextFieldWidgetClass, rowcol_h, NULL);
    XtManageChild (rowcol_h);

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

    XtAddCallback (search_w, XmNactivateCallback, search_text, NULL);

    XtManageChild (rowcol_v);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* search_text() -- called when the user activates the TextField. */
void
search_text(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
    char *search_pat, *p, *string, buf[32];
    XmTextPosition pos;
    int len;
    Boolean found = False;

    /* get the text that is about to be searched */
    if (!(string = XmTextGetString (text_w)) || !*string) {
        XmTextSetString (text_output, "No text to search.");
        XtFree (string); /* may have been ""; free it */
        return;
    }
    /* get the pattern we're going to search for in the text. */
    if (!(search_pat = XmTextGetString (search_w)) || !*search_pat) {
        XmTextSetString (text_output, "Specify a search pattern.");
        XtFree (string); /* this we know is a string; free it */
        XtFree (search_pat); /* this may be "", XtFree() checks.. */
        return;
    }
    len = strlen (search_pat);

    /* start searching at current cursor position + 1 to find
     * the -next- occurrance of string.  we may be sitting on it.
     */
    pos = XmTextGetCursorPosition (text_w);
    for (p = &string[pos+1]; p = index (p, *search_pat); p++)
        if (!strncmp (p, search_pat, len)) {
            found = True;
            break;
        }
    if (!found) { /* didn't find pattern? */
        /* search from beginning till we've passed "pos" */
        for (p = string;
                (p = index (p, *search_pat)) && p - string <= pos; p++)
            if (!strncmp (p, search_pat, len)) {
                found = True;
                break;
            }
    }
    if (!found)
        XmTextSetString (text_output, "Pattern not found.");
    else {
        pos = (XmTextPosition)(p - string);
        sprintf (buf, "Pattern found at position %ld.", pos);
        XmTextSetString (text_output, buf);
        XmTextSetInsertionPosition (text_w, pos);
    }
    XtFree (string);
    XtFree (search_pat);
}
