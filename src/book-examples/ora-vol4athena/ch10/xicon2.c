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
 * xicon2.c
 */

/*
 * Include files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>	/* Intrinsics Definitions */
#include <X11/IntrinsicP.h>	/* For core structure in XtOffset */
#include <X11/StringDefs.h>	/* Standard Name-String definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <X11/Xaw/Label.h>		/* Athena Label Widget */

#include <X11/Shell.h>

#include <stdio.h>

main(argc, argv)
int argc;
char **argv;
{
	XtAppContext app_context;
	Widget topLevel, hello;
	XrmValue from, to;
	static char icon_file_name[] = "./icon.bit";
	Arg arg;

	static XtConvertArgRec screenConvertArg[] = {
		{
		XtBaseOffset,
		(XtPointer)XtOffset(Widget, core.screen),
		sizeof(Screen *)
		}
	};

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
		"XIcon2",           /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

	XtAppAddConverter(app_context, XtRString, XtRBitmap, XmuCvtStringToBitmap,
		screenConvertArg, XtNumber(screenConvertArg));

	from.size = strlen(icon_file_name);
	from.addr = icon_file_name;

	XtConvert(topLevel, XtRString, &from, XtRBitmap, &to);

	if (to.addr != NULL) {
        	XtSetArg(arg, XtNiconPixmap, *(Pixmap *)to.addr);
        	XtSetValues(topLevel, &arg, 1);
	}
	else
		fprintf(stderr, "xhello: unable to find icon pixmap file.\n");

	hello = XtCreateManagedWidget(
		"hello",		/* arbitrary widget name */
		labelWidgetClass,	/* widget class from Label.h */
		topLevel,	/* parent widget*/
		NULL,		/* argument list */
		0		/* arg list size */
		);

	/*
	 *  Create windows for widgets and map them.
	 */
	XtRealizeWidget(topLevel);

	/*
	 *  Loop for events.
	 */
	XtAppMainLoop(app_context);
}

