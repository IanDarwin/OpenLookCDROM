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

/* multi_font.c -- create three compound strings using 12, 14 and 18 
 * point fonts.  The user can specify resources so that each of the strings
 * use different fonts by setting resources similar to that shown
 * by the fallback resources.
 */
#include <Xm/Label.h>

String fallbacks[] = {
    "multi_font*fontList:\
-*-courier-*-r-*--12-*=TAG1,\
-*-courier-bold-o-*--14-*=TAG2,\
-*-courier-medium-r-*--18-*=TAG3",
    NULL
};

main(argc, argv)
int argc;
char *argv[];
{
    Widget        toplevel;
    XtAppContext  app;
    XmString      s1, s2, s3, text, tmp;
    String        string1 = "This is a string ",
                  string2 = "that contains three ",
                  string3 = "separate fonts.";

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "String", NULL, 0,
        &argc, argv, fallbacks, NULL);

    s1 = XmStringCreate (string1, "TAG1");
    s2 = XmStringCreate (string2, "TAG2");
    s3 = XmStringCreate (string3, "TAG3");

    /* concatenate the 3 strings on top of each other, but we can only
     * do two at a time.  So do s1 and s2 onto tmp and then do s3.
     */
    tmp = XmStringConcat (s1, s2);
    text = XmStringConcat (tmp, s3);

    XtVaCreateManagedWidget ("widget_name",
        xmLabelWidgetClass, toplevel,
        XmNlabelString,     text,
        NULL);

    XmStringFree (s1);
    XmStringFree (s2);
    XmStringFree (s3);
    XmStringFree (tmp);
    XmStringFree (text);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
