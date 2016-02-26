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
 * xtwoapp.c - an application with two app contexts and two displays
 */

/*
 * Include files required for all Toolkit programs
 */
#include <Xm/Xm.h>	/* Intrinsics Definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <Xm/PushB.h>		/* Motif Label Widget */


/* 
 * This kind of works, but is not elegant, since it ruins Xlib's
 * network optimizations.
 */
void OurMainLoop(app1, app2, dis1, dis2)
    XtAppContext app1, app2;
	Display *dis1, *dis2;
{
    XEvent event;

    for (;;) {
        if (XtAppPeekEvent(app1, &event) == TRUE) {
	    XtAppNextEvent(app1, &event);
	    XtDispatchEvent(&event);
	    if (XtAppPeekEvent(app2, &event) == TRUE) {
	        XtAppNextEvent(app2, &event);
		XtDispatchEvent(&event);
	      }
	    else XFlush(dis2);
	  }
	else {
	    XFlush(dis1);
	    if (XtAppPeekEvent(app2, &event) == TRUE) {
	        XtAppNextEvent(app2, &event);
		XtDispatchEvent(&event);
	      }
	    else XFlush(dis2);
	  }
      }
  }

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_con1, app_con2;
    Widget topLevel1, topLevel2, hello, goodbye;
    Display *display1, *display2;
    
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);
    
    XtToolkitInitialize();
    
    app_con1 = XtCreateApplicationContext();
    app_con2 = XtCreateApplicationContext();
    
    display1 = XtOpenDisplay(app_con1, "obsidian:0", argv[0], "XHello", 
			     NULL, 0, &argc, argv);

    display2 = XtOpenDisplay(app_con2, "harry:0", argv[0], "XGoodbye", 
			     NULL, 0, &argc, argv);

    topLevel1 = XtAppCreateShell("name1", "XHello", 
				 applicationShellWidgetClass, display1, 
				 NULL, 0);

    topLevel2 = XtAppCreateShell("name2", "XGoodbye", 
				 applicationShellWidgetClass, display2, 
				 NULL, 0);

    hello = XtCreateManagedWidget(
		"hello",		/* arbitrary widget name */
		xmPushButtonWidgetClass,/* widget class from PushB.h */
		topLevel1,		/* parent widget*/
		NULL,			/* argument list */
		0			/* arg list size */
		);

    goodbye = XtCreateManagedWidget(
		"goodbye",		/* arbitrary widget name */
		xmPushButtonWidgetClass,/* widget class from PushB.h */
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
     *  Notice that since XtAppMainLoop only handles on
     *  app context, we have to write our own juggling loop.
     *  This is not easy (just an attempt made here).
     */
    OurMainLoop(app_con1, app_con2, display1, display2);
}
