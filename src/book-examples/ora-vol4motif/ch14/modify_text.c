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

/* modify_text.c -- demonstrate the XmNmodifyVerifyCallback for
 * Text widgets.  Display several different type-in fields prompting
 * for name, phone, state, zip and a password.  Each widget has a
 * separate modify-callback associated with it to check that the
 * right type of input is being given.  zipcode requires digits
 * only, but the phone number also inserts -'s at the right place.
 * The state converts everything to upper case and the password only
 * outputs *'s.
 */
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <ctype.h>

void check_phone(), allcaps(), check_zip(),
    check_passwd(), value_changed(), traverse();
char *passwd;
char *labels[] = { "Name:", "Phone:", "State:", "ZIP:", "Password:" };

typedef void (*VoidProc)();
VoidProc modify_funcs[] = {
    NULL, check_phone, allcaps, check_zip, check_passwd
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel, text_w, form, rowcol;
    XtAppContext  app;
    int           i;
    void          print_result();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos",
        NULL, 0, &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget ("rowcol",
        xmRowColumnWidgetClass, toplevel, NULL);

    for (i = 0; i < XtNumber (labels); i++) {
        form = XtVaCreateWidget ("form", xmFormWidgetClass, rowcol,
            XmNfractionBase,  10,
            NULL);
        XtVaCreateManagedWidget (labels[i],
            xmLabelGadgetClass, form,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_FORM,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    3,
            XmNalignment,        XmALIGNMENT_END,
            NULL);
        text_w = XtVaCreateManagedWidget ("text_w",
            xmTextWidgetClass, form,
            XmNtraversalOn,      True,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     4,
            NULL);

        if (modify_funcs[i]) {
            /* When user types, monitor input */
            XtAddCallback (text_w, XmNactivateCallback,
                modify_funcs[i], labels[i]);
            XtAddCallback (text_w, XmNmodifyVerifyCallback,
                modify_funcs[i], labels[i]);
            XtAddCallback (text_w, XmNvalueChangedCallback,
                value_changed, NULL);
        } 
	else
            XtAddCallback (text_w, XmNactivateCallback, traverse, labels[i]);
        XtManageChild (form);
    }
    XtManageChild (rowcol);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}

void
traverse(text_w, client_data, call_data)
Widget text_w;
XtPointer client_data;
XtPointer call_data;
{
    char *label = (char *) client_data;

    char *string = XmTextGetString (text_w);
    printf ("%s %s\n", label, string);
    XtFree (string);
    XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
}

void
value_changed(text_w, client_data, call_data)
Widget text_w;
XtPointer client_data;
XtPointer call_data;
{
    XmTextSetInsertionPosition (text_w, XmTextGetLastPosition (text_w));
}

/* check_phone() -- handle phone number input. */
void
check_phone(text_w, client_data, call_data)
Widget     text_w;
XtPointer  client_data;
XtPointer  call_data;
{
    char c;
    int len = XmTextGetLastPosition (text_w);
    char *label = (char *) client_data;
    XmTextVerifyCallbackStruct *cbs = 
        (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_ACTIVATE) {
        if (len == 12) {
            char *string = XmTextGetString (text_w);
            printf ("%s %s\n", label, string);
            XtFree (string);
            XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        }
        return;
    }

    /* no backspacing or typing in the middle of string */
    if (cbs->currInsert < len) {
        cbs->doit = False;
        return;
    }

    if (cbs->text->length == 0) {  /* backspace */
        if (cbs->startPos == 3 || cbs->startPos == 7)
            cbs->startPos--;       /* delete the hyphen too */
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

/* allcaps() -- convert inserted text to capital letters. */
void
allcaps(text_w, client_data, call_data)
Widget      text_w;
XtPointer   client_data;
XtPointer   call_data;
{
    int len;
    char *label = (char *) client_data;
    XmTextVerifyCallbackStruct *cbs = 
        (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_ACTIVATE) {
        char *string = XmTextGetString (text_w);
	printf ("%s %s\n", label, string);
	XtFree (string);
        XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        return;
    }

    if (cbs->text->ptr == NULL)  
        return;

    /* convert all input to upper-case if necessary */
    for (len = 0; len < cbs->text->length; len++)
        if (islower (cbs->text->ptr[len]))
            cbs->text->ptr[len] = toupper (cbs->text->ptr[len]);
}

/* check_zip() -- limit the user to entering a ZIP code. */
void
check_zip(text_w, client_data, call_data)
Widget text_w;
XtPointer client_data;
XtPointer call_data;
{
    int len = XmTextGetLastPosition (text_w);
    char *label = (char *) client_data;
    XmTextVerifyCallbackStruct *cbs = 
        (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_ACTIVATE) {
        if (len == 5) {
            char *string = XmTextGetString (text_w);
            printf ("%s %s\n", label, string);
            XtFree (string);
            XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        }
        return;
    }

    if (cbs->startPos < cbs->currInsert) /* backspace */
        return;

    if (len == 5) {
        cbs->doit = False;
        return;
    }
    /* check that the new additions won't put us over 5 */
    if (len + cbs->text->length > 5) {
        cbs->text->ptr[5 - len] = 0;
        cbs->text->length = strlen (cbs->text->ptr);
    }
    for (len = 0; len < cbs->text->length; len++) {
        /* make sure all additions are digits. */
        if (!isdigit (cbs->text->ptr[len])) {
            /* not a digit-- move all chars down one and
             * decrement cbs->text->length.
             */
            int i;
            for (i = len; (i+1) < cbs->text->length; i++)
                cbs->text->ptr[i] = cbs->text->ptr[i+1];
            cbs->text->length--;
            len--;
        }
    }
    if (cbs->text->length == 0)
        cbs->doit = False;
}

/* check_passwd() -- handle the input of a password. */
void
check_passwd(text_w, client_data, call_data)
Widget        text_w;
XtPointer     client_data;
XtPointer     call_data;
{
    char *new;
    int len;
    char *label = (char *) client_data;
    XmTextVerifyCallbackStruct *cbs = 
        (XmTextVerifyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_ACTIVATE) {
        printf("%s %s\n", label, passwd);
        XmProcessTraversal(text_w, XmTRAVERSE_NEXT_TAB_GROUP);
        return;
    }

    if (cbs->startPos < cbs->currInsert) {   /* backspace */
        cbs->endPos = strlen (passwd);       /* delete from here to end */
        passwd[cbs->startPos] = 0;           /* backspace--terminate */
        return;
    }

    if (cbs->text->length > 1) {
        cbs->doit = False;  /* don't allow "paste" operations */
        return;             /* make the user *type* the password! */
    }

    new = XtMalloc (cbs->endPos + 2); /* new char + NULL terminator */
    if (passwd) {
        strcpy (new, passwd);
        XtFree (passwd);
    } else
        new[0] = NULL;
    passwd = new;
    strncat (passwd, cbs->text->ptr, cbs->text->length);
    passwd[cbs->endPos + cbs->text->length] = 0;

    for (len = 0; len < cbs->text->length; len++)
        cbs->text->ptr[len] = '*';
}
