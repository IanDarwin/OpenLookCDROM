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
 * xclickcount.c - simple program to count button clicks.
 */

#include <stdio.h>

/*
 * Include files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>	/* Intrinsics Definitions */
#include <X11/StringDefs.h>	/* Standard Name-String definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <X11/Xaw/Label.h>		/* Athena Label Widget */

#define MAXLEN 50

char userstring[MAXLEN];

/*ARGSUSED*/
static void IncrementCount(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params; 
Cardinal *num_params;
{
	char string[MAXLEN];
	static int count;

	count++;

	(void) sprintf(string, "%s: %d", userstring, count);

	XtVaSetValues(w, 
			XtNwidth, 100,
			XtNlabel, string,
			NULL);
}

main(argc, argv)
int argc;
char **argv;
{
	XtAppContext app_context;
	Widget topLevel, hello;
	String trans =
	"<BtnDown>:	IncrementCount()";

	XtTranslations compiled_trans;

	static XtActionsRec hello_actions[] = {
		{"IncrementCount", IncrementCount},
	};

	char *p;

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XClickcount",         /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

	hello = XtVaCreateManagedWidget(
		"hello",		/* arbitrary widget name */
		labelWidgetClass,	/* widget class from Label.h */
		topLevel,	/* parent widget*/
		NULL);		/* terminate varargs list */

	XtVaGetValues(hello, 
		XtNlabel, &p,
		NULL);

	strcpy(userstring, p);

	XtAppAddActions(app_context, hello_actions, 1);

	compiled_trans = XtParseTranslationTable(trans);

	XtVaSetValues(hello, 
		XtNtranslations, compiled_trans,
		NULL);
	/*
	 *  Create windows for widgets and map them.
	 */
	XtRealizeWidget(topLevel);

	/*
	 *  Loop for events.
	 */
	XtAppMainLoop(app_context);
}
