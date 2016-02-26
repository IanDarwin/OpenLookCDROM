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
 * xmotdbltst.c - simple program to test getting motion events and 
 * double clicks in the same widget.
 */

#include <stdio.h>
/*
 * Include files required for all Toolkit programs
 */
#include <Xm/Xm.h>	/* Intrinsics and Motif Definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <Xm/PushB.h>		/* Push Button Widget */

#define NUM_ACTIONS 3

/*ARGSUSED*/
static void ActionA(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
	printf("action A executed\n");
}

/*ARGSUSED*/
static void ActionB(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
	printf("action B executed\n");
}

/*ARGSUSED*/
static void ActionC(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
	printf("action C executed\n");
}

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, trial;
    
    static XtActionsRec trial_actions[] = {
      {"actionA", ActionA},
      {"actionB", ActionB},
      {"actionC", ActionC},
    };
    
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
	"XMotdbltst",       /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    trial = XtCreateManagedWidget(
	"trial",			/* arbitrary widget name */
	xmPushButtonWidgetClass,	/* widget class from Label.h */
	topLevel,			/* parent widget*/
	NULL,				/* argument list */
	0);				/* arg list size */

    XtAppAddActions(app_context, trial_actions, NUM_ACTIONS);

    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel);

    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_context);
}
