/*
 * Copyright 1989,1992 O'Reilly and Associates, Inc.

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
 * xfallback.c - simple program to demonstrate fallback resources
 */

/*
 * Include files required for all Toolkit programs
 */
#include <Xm/Xm.h>     /* Intrinsics Definitions*/

/*
 * Public include file for widgets we actually use in this file.
 */
#include <Xm/Label.h>     /* Motif Label Widget */

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, hello;

    static String fallback_resources[] = {
      "*hello.labelString: App defaults file not installed",
      "*hello.fontList: *courier-bold*18*iso8859-1",
      NULL,		/* MUST BE NULL TERMINATED */
    };

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "NoFile",           /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        fallback_resources, /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    hello = XtVaCreateManagedWidget(
	"hello",		/* arbitrary widget name */
	xmLabelWidgetClass,	/* widget class from Label.h */
	topLevel,		/* parent widget */
        NULL);              	/* terminate varargs list */

    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel);
    
    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_context);
}
