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
 *  xbox2.c
 */

/*
 *  So that we can use fprintf:
 */
#include <stdio.h>

/* 
 * Standard Toolkit include files:
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/*
 * Public include files for widgets used in this file.
 */
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>

/*
 * quit button callback function
 */
/*ARGSUSED*/
void Quit(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{ 
	exit(0); 
}

/*
 * "Press me!" button callback function
 */
/*ARGSUSED*/
void PressMe(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{ 
	fprintf(stderr, "Thankyou!\n"); 
}

main(argc, argv)
int argc;
char **argv;
{
	XtAppContext app_context;
	Widget form, quit, pressme, topLevel;

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
		"XBox2",            /* application class name */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

	form = XtVaCreateManagedWidget(
		"form", 	/* widget name */
		formWidgetClass,	/* widget class */
		topLevel,	/* parent widget*/
		NULL);	/* argument list*/

	quit = XtVaCreateManagedWidget(
		"quit",	/* widget name */
		commandWidgetClass,	/* widget class */
		form,	/* parent widget*/
		NULL);	/* argument list*/

	pressme = XtVaCreateManagedWidget(
		"pressme",	/* widget name	 */
		commandWidgetClass,	/* widget class */
		form,	/* parent widget*/
		NULL);	/* argument list*/

	XtAddCallback(quit, XtNcallback, Quit, 0);
	XtAddCallback(pressme, XtNcallback, PressMe, 0);

	XtRealizeWidget(topLevel);

	XtAppMainLoop(app_context);
}
