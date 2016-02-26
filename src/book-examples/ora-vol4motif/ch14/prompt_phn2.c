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

/* prompt_phone.c -- a complex problem for XmNmodifyVerifyCallback.
 * prompt for a phone number by filtering digits only from input.
 * Don't allow paste operations and handle backspacing.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <ctype.h>
#include <stdio.h>

void check_phone();

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, text_w, rowcol;
    XtAppContext  app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel,
        XmNorientation, XmHORIZONTAL,
        NULL);

    XtVaCreateManagedWidget ("Phone Number:",
        xmLabelGadgetClass, rowcol, NULL);
    text_w = XtVaCreateManagedWidget ("text_w",
        xmTextWidgetClass, rowcol, NULL);

    XtAddCallback (text_w, XmNmodifyVerifyCallback, check_phone, NULL);
    XtAddCallback (text_w, XmNmotionVerifyCallback, check_phone, NULL);

    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
check_phone(text_w, client_data, call_data)
Widget     text_w;
XtPointer  client_data;
XtPointer  call_data;
{
    char c;
    int len = XmTextGetLastPosition (text_w);
    XmTextVerifyCallbackStruct *cbs = 
        (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_MOVING_INSERT_CURSOR) {
        if (cbs->newInsert != len)
	    cbs->doit = False;
        return;
    }

    /* no backspacing, typing or stuffing in middle of string */
    if (cbs->currInsert < len) {
        cbs->doit = False;
        return;
    }

    if (cbs->text->length == 0) { /* backspace */
        if (cbs->startPos == 3 || cbs->startPos == 7)
            cbs->startPos--;      /* delete the hyphen too */
        return;
    }

    if (cbs->text->length > 1) { /* don't allow clipboard copies */
        cbs->doit = False;
        return;
    }

    /* don't allow non-digits or let the input exceed 12 chars */
    if (!isdigit (c = cbs->text->ptr[0]) || len >= 12)
        cbs->doit = False;
    else if (len == 2 || len == 6) {
        cbs->text->ptr = XtRealloc (cbs->text->ptr, 2);
        cbs->text->length = 2;
        cbs->text->ptr[0] = c;
        cbs->text->ptr[1] = '-';
    }
}
