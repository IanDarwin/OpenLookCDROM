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
 * xgetstring.c - simple program to put up a banner on the display
 *                Demonstrates querying a compound string.
 */

#include <stdio.h>
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
    Widget topLevel, hello;
    String string;
    XmString text;

    /*
     * Register the default language procedure
     */
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
            &app_context,       /* Application context */
            "XHello",         /* Application class */
            NULL, 0,            /* command line option list */
            &argc, argv,        /* command line args */
            NULL,               /* for missing app-defaults file */
            NULL);              /* terminate varargs list */

    hello = XtVaCreateManagedWidget(
	    "hello",			/* arbitrary widget name */
	     xmPushButtonWidgetClass,	/* widget class from PushButton.h */
	     topLevel,			/* parent widget */
             NULL);                     /* terminate varargs list */

    XtVaGetValues(hello,
		  XmNlabelString, &text,
		  NULL);

    (void) XmStringGetLtoR(text, XmFONTLIST_DEFAULT_TAG, &string);
    printf("The string set in the app-defaults file is: %s\n", string);
    XtFree(string);
    XmStringFree(text);

    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel);
    
    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_context);
  }

