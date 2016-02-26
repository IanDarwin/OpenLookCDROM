/*
 * Copyright 1989 O'Reilly and Associates, Inc.

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
 * xtwoapp.c
 */

/*
 * Include files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>	/* Intrinsics Definitions */
#include <X11/StringDefs.h>	/* Standard Name-String definitions */

#include <X11/Shell.h>		/* Needed for applicationShellWidgetClass */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <X11/Xaw/Label.h>		/* Athena Label Widget */


/* 
 * This sort of works, but it is not elegant, since it ruins Xlib's
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

	XtToolkitInitialize();

	app_con1 = XtCreateApplicationContext();
	app_con2 = XtCreateApplicationContext();

	display1 = XtOpenDisplay(app_con1, "spike:0", argv[0], "XHello", NULL, 0, &argc, argv);

 	if (display1 == NULL) {
                printf("can't open display #1\n");
                exit(1);
        }

	display2 = XtOpenDisplay(app_con2, "ncd:0", argv[0], "XGoodbye", NULL, 0, &argc, argv);

 	if (display2 == NULL) {
                printf("can't open display #2\n");
                exit(1);
        }

	topLevel1 = XtAppCreateShell("name1", "XHello", applicationShellWidgetClass, display1, NULL, 0);

	topLevel2 = XtAppCreateShell("name2", "XGoodbye", applicationShellWidgetClass, display2, NULL, 0);

	hello = XtCreateManagedWidget(
		"hello",		/* arbitrary widget name */
		labelWidgetClass,	/* widget class from Label.h */
		topLevel1,	/* parent widget*/
		NULL,		/* argument list */
		0		/* arg list size */
		);

	goodbye = XtCreateManagedWidget(
		"goodbye",		/* arbitrary widget name */
		labelWidgetClass,	/* widget class from Label.h */
		topLevel2,	/* parent widget*/
		NULL,		/* argument list */
		0		/* arg list size */
		);
	/*
	 *  Create windows for widgets and map them.
	 */
	XtRealizeWidget(topLevel1);
	XtRealizeWidget(topLevel2);

	/*
	 *  Loop for events.
	 *  Notice that since XtAppMainLoop only handles one
     *  app context, we have to write our own juggling loop.
     *  This is not easy.
	 */
	OurMainLoop(app_con1, app_con2, display1, display2);
}
