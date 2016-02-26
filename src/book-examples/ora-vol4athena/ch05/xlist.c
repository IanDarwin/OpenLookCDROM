/*
 * xlist.c - simple program to use the List widget
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
#include <X11/Xaw/List.h>     /* Athena List Widget */

void DoSomething();

main(argc, argv)
int argc;
char **argv;
{
	XtAppContext app_context;
    Widget topLevel, list;
	static  String listitems[] = {
		"Delaware",
		"Virginia",
		"Washington",
	};
	static int nitems = 3;

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XList",         /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

	list = XtVaCreateManagedWidget(
		"list",			/* arbitrary widget name */
		listWidgetClass,	/* widget class from Label.h */
		topLevel,			/* parent widget */
        	NULL);              /* terminate varargs list */

	XawListChange(list,
		listitems, nitems,
		-1,	/* List Widget calculates longest */
		True);	/* List Widget attempts resize */

	XtAddCallback(list, XtNcallback, DoSomething, 0);

	/*
	 *  Create windows for widgets and map them.
	 */
	XtRealizeWidget(topLevel);

	/*
	 *  Loop for events.
	 */
	XtAppMainLoop(app_context);
}

/* ARGSUSED */
void DoSomething(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
        XawListReturnStruct *list_struct = (XawListReturnStruct *) call_data;
        printf("Item number %d is the state of %s.\n", list_struct->list_index,
list_struct->string);
}

