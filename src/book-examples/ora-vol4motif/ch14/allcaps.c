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

/* allcaps.c -- demonstrate the XmNmodifyVerifyCallback for
 * Text widgets by using one to convert all typed input to
 * capital letters.
 */
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <ctype.h>

void allcaps();

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

    XtVaCreateManagedWidget ("Enter Text:",
        xmLabelGadgetClass, rowcol, NULL);
    text_w = XtVaCreateManagedWidget ("text_w",
        xmTextWidgetClass, rowcol, NULL);

    XtAddCallback (text_w, XmNmodifyVerifyCallback, allcaps, NULL);

    XtManageChild (rowcol);
    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

/* allcaps() -- convert inserted text to capital letters. */
void
allcaps(text_w, client_data, call_data)
Widget      text_w;
XtPointer   client_data;
XtPointer   call_data;
{
    int len;
    XmTextVerifyCallbackStruct *cbs = 
        (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->text->ptr == NULL)  
        return;

    /* convert all input to upper-case if necessary */
    for (len = 0; len < cbs->text->length; len++)
        if (islower (cbs->text->ptr[len]))
            cbs->text->ptr[len] = toupper (cbs->text->ptr[len]);
}
