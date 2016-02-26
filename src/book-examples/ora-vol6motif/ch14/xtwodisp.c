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
 * xtwodisp.c - an application with windows on two displays
 */

/*
 * Include files required for all Toolkit programs
 */
#include <Xm/Xm.h>	/* Intrinsics Definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <Xm/Label.h>		/* Motif Label Widget */

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_con;
    Widget topLevel1, topLevel2, hello, goodbye;
    Display *display1, *display2;
    
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);
    
    XtToolkitInitialize();
    
    app_con = XtCreateApplicationContext();
    
    display1 = XtOpenDisplay(app_con, "obsidian:0", argv[0], "XHello", 
			     NULL, 0, &argc, argv);
    
    display2 = XtOpenDisplay(app_con, "harry:0", argv[0], "XGoodbye", 
			     NULL, 0, &argc, argv);
    
    topLevel1 = XtAppCreateShell("name1", "XHello", 
				 applicationShellWidgetClass, display1, 
				 NULL, 0);
    
    topLevel2 = XtAppCreateShell("name2", "XGoodbye", 
				 applicationShellWidgetClass, display2, 
				 NULL, 0);
  
    hello = XtCreateManagedWidget(
	        "hello",		/* arbitrary widget name */
		xmLabelWidgetClass,	/* widget class from Label.h */
		topLevel1,		/* parent widget*/
		NULL,			/* argument list */
		0			/* arg list size */
		);

    goodbye = XtCreateManagedWidget(
		"goodbye",		/* arbitrary widget name */
		xmLabelWidgetClass,	/* widget class from Label.h */
		topLevel2,		/* parent widget*/
		NULL,			/* argument list */
		0			/* arg list size */
		);
    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel1);
    XtRealizeWidget(topLevel2);
    
    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_con);
}
