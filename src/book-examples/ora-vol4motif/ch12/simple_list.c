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

/* simple_list.c -- introduce the List widget.  Lists present
 * a number of comound strings as choices.  Therefore, strings
 * must be converted before set in lists.  Also, the number of
 * visible items must be set or the List defaults to 1 item.
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
    Widget           toplevel;
    XtAppContext     app;
    int              i, n = XtNumber (months);
    XmStringTable    str_list;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));

    for (i = 0; i < n; i++)
        str_list[i] = XmStringCreateLocalized (months[i]);

    XtVaCreateManagedWidget ("Hello",
        xmListWidgetClass,     toplevel,
        XmNvisibleItemCount,   n,
        XmNitemCount,          n,
        XmNitems,              str_list,
        NULL);

    for (i = 0; i < n; i++)
        XmStringFree (str_list[i]);
    XtFree (str_list);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
