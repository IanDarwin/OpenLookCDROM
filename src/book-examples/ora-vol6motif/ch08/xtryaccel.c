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
 * xtryaccel.c
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
#include <X11/Xaw/Command.h>		/* Athena Label Widget */
#include <X11/Xaw/Box.h>		/* Athena Label Widget */

/*
 * Goodbye button callback function
 */
/* ARGSUSED */
void Quit(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{ 
	exit(0);
}

/*
 * Hello button callback function
 */
/* ARGSUSED */
void PressMe(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{ 
	fprintf(stderr, "It was nice knowing you.\n");
}

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, box, quit, pressme;

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
	"XTryAccel",        /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    box = XtCreateManagedWidget(
	"box",		/* arbitrary widget name */
	boxWidgetClass,	/* widget class from Label.h */
	topLevel,	/* parent widget*/
	NULL,		/* argument list */
	0		/* arg list size */
	);

    quit = XtCreateManagedWidget(
	"quit",			/* arbitrary widget name */
	commandWidgetClass,	/* widget class from Label.h */
	box,			/* parent widget*/
	NULL,			/* argument list */
	0			/* arg list size */
	);

    pressme = XtCreateManagedWidget(
	"pressme",		/* arbitrary widget name */
	commandWidgetClass,	/* widget class from Label.h */
	box,			/* parent widget*/
	NULL,			/* argument list */
	0			/* arg list size */
	);

    XtAddCallback(quit, XtNcallback, Quit, NULL);
    XtAddCallback(pressme, XtNcallback, PressMe, NULL);

    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel);
    
    XtInstallAccelerators(box, quit);
    XtInstallAccelerators(box, pressme);
    
    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_context);
}

