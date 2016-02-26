/*
 * xtoggle.c - implement radio buttons with the Toggle widget
 *
 *    The X Consortium, and any party obtaining a copy of these files from
 *    the X Consortium, directly or indirectly, is granted, free of charge, a
 *    full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *    nonexclusive right and license to deal in this software and
 *    documentation files (the "Software"), including without limitation the
 *    rights to use, copy, modify, merge, publish, distribute, sublicense,
 *    and/or sell copies of the Software, and to permit persons who receive
 *    copies from any such party to do so.  This license includes without
 *    limitation a license to do the foregoing actions under any patents of
 *    the party supplying this software to the X Consortium.
 */

/*
 * Include files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>     /* Intrinsics Definitions*/
#include <X11/StringDefs.h>    /* Standard Name-String definitions*/

/*
 * Public include file for widgets we actually use in this file.
 */
#include <X11/Xaw/Toggle.h>     /* Athena Toggle Widget */
#include <X11/Xaw/Box.h>     /* Athena Box Widget */

/* ARGSUSED */
void Toggled(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
	/* call_data unused */
	String string;

	/* Beware: the Toggle widget Notify action calls this callback
         * twice every time a selection is made, once for the previous 
	 * selection and once for the current one.
	 */

	XtVaGetValues(w, XtNlabel, &string, NULL);

	printf("Customer wants meal %s.\n", string);
}

main(argc, argv)
int argc;
char **argv;
{
	XtAppContext app_context;
	Widget topLevel, box, toggle[5];
	static int val[] = { 0, 1, 2, 3, 4 };

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XToggle",         /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

	box = XtVaCreateManagedWidget(
		"box",			/* arbitrary widget name */
		boxWidgetClass,	/* widget class from Label.h */
		topLevel,			/* parent widget */
        	NULL);              /* terminate varargs list */

	toggle[0] = XtVaCreateManagedWidget(
		"toggle0",			/* arbitrary widget name */
		toggleWidgetClass,	/* widget class from Label.h */
		box,			/* parent widget */
		XtNstate, True,
       		NULL);              /* terminate varargs list */

	toggle[1] = XtVaCreateManagedWidget(
		"toggle1", toggleWidgetClass, box,		
		XtNradioGroup, toggle[0], /* previous toggle */
       		NULL);

	toggle[2] = XtVaCreateManagedWidget(
		"toggle2", toggleWidgetClass, box,		
		XtNradioGroup, toggle[1],
       		NULL);

	toggle[3] = XtVaCreateManagedWidget(
		"toggle3", toggleWidgetClass, box,		
		XtNradioGroup, toggle[2],
       		NULL);

	toggle[4] = XtVaCreateManagedWidget(
		"toggle4", toggleWidgetClass, box,		
		XtNradioGroup, toggle[3],
       		NULL);

	XtAddCallback(toggle[0], XtNcallback, Toggled, (XtPointer) 0);
	XtAddCallback(toggle[1], XtNcallback, Toggled, (XtPointer) 1);
	XtAddCallback(toggle[2], XtNcallback, Toggled, (XtPointer) 2);
	XtAddCallback(toggle[3], XtNcallback, Toggled, (XtPointer) 3);
	XtAddCallback(toggle[4], XtNcallback, Toggled, (XtPointer) 4);

	/*
	 *  Create windows for widgets and map them.
	 */
	XtRealizeWidget(topLevel);

	/*
	 *  Loop for events.
	 */
	XtAppMainLoop(app_context);
}

