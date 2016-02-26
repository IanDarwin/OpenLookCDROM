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
 * xtwodisp.c
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

main(argc, argv)
int argc;
char **argv;
{
	XtAppContext app_con;
	Widget topLevel1, topLevel2, hello, goodbye;
	Display *display1, *display2;
	Arg args[1];

	XtToolkitInitialize();

	app_con = XtCreateApplicationContext();

	display1 = XtOpenDisplay(app_con, "spike:0", argv[0], "XHello", NULL, 0, (Cardinal *) &argc, argv);

 	if (display1 == NULL) {
                printf("can't open display #1\n");
                exit(1);
        }

	display2 = XtOpenDisplay(app_con, "ncd:0", argv[0], "XGoodbye", NULL, 0, (Cardinal *) &argc, argv);

 	if (display2 == NULL) {
                printf("can't open display #2\n");
                exit(1);
        }

	topLevel1 = XtAppCreateShell("name1", "XHello", applicationShellWidgetClass, display1, (ArgList) args, 0);

	topLevel2 = XtAppCreateShell("name2", "XGoodbye", applicationShellWidgetClass, display2, (ArgList) args, 0);

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
	 */
	XtAppMainLoop(app_con);
}
