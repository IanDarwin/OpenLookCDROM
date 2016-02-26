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

/* browse.c -- specify a browse selection callback for a simple List.
 */
#include <Xm/List.h>

char *months[] = {
    "January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December"
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget           toplevel, list_w;
    XtAppContext     app;
    int              i, n = XtNumber (months);
    XmStringTable    str_list;
    void             sel_callback();

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));

    for (i = 0; i < n; i++)
        str_list[i] = XmStringCreateLocalized (months[i]);

    list_w = XmCreateScrolledList (toplevel, "months", NULL, 0);
    XtVaSetValues (list_w,
        XmNvisibleItemCount,   n,
        XmNitemCount,          n,
        XmNitems,              str_list,
        NULL);
    XtManageChild (list_w);

    XtAddCallback (list_w, XmNdefaultActionCallback, sel_callback, NULL);
    XtAddCallback (list_w, XmNbrowseSelectionCallback, sel_callback, NULL);

    for (i = 0; i < n; i++)
        XmStringFree (str_list[i]);
    XtFree (str_list);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}


void
sel_callback(list_w, client_data, call_data)
Widget list_w;
XtPointer client_data;
XtPointer call_data;
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) call_data;
    char *choice;

    if (cbs->reason == XmCR_BROWSE_SELECT)
        printf ("Browse selection -- ");
    else
        printf ("Default action -- ");

    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
    printf ("selected item: %s (%d)\n", choice, cbs->item_position);
    XtFree (choice);
}

