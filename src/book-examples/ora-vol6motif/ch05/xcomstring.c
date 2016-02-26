/*
 * Copyright 1989, 1992 O'Reilly and Associates, Inc.

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */

/*
 * xcomstring.c - simple test of compound strings, modifying fonts
 */

/*
 * Header files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>     /* Intrinsics definitions */
#include <Xm/Xm.h>    /* Standard Motif definitions */

/*
 * Public header file for widgets we actually use in this file.
 */
#include <Xm/PushB.h>     /* Motif PushButton Widget */


main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel;
    XmString text;
    XmString s1, s2, s3, s4;
    Widget hello;
    static String string1 = "Specify the ",
                  string2 = "character set ",
                  string3 = "in the code.";

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
            &app_context,       /* Application context */
            "XComstring",       /* Application class */
            NULL, 0,            /* command line option list */
            &argc, argv,        /* command line args */
            NULL,               /* for missing app-defaults file */
            NULL);              /* terminate varargs list */

    s1 = XmStringCreate(string1, "tag1");
    s2 = XmStringCreate(string2, "tag2");
    s3 = XmStringCreate(string3, "tag1");

    s4 = XmStringConcat(s1, s2);
    XmStringFree(s1);
    XmStringFree(s2);

    text = XmStringConcat(s4, s3);
    XmStringFree(s3);
    XmStringFree(s4);

    hello = XtVaCreateManagedWidget(
	    "hello",			/* arbitrary widget name */
	    xmPushButtonWidgetClass,	/* widget class from PushButton.h */
	    topLevel,			/* parent widget */
            XmNlabelString, text,
	    NULL);              	/* terminate varargs list */

    XmStringFree(text);

    XtRealizeWidget(topLevel);
    
    XtAppMainLoop(app_context);
}

