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
 * xstdin.c
 */

#include <stdio.h>
/*
 * Include files required for all Toolkit programs
 */
#include <Xm/Xm.h>	/* Intrinsics Definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <Xm/PushB.h>		/* Motif PushButton Widget */

/*
 * Quit button callback function
 */
/* ARGSUSED */
void Quit(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{ 
	fprintf(stderr, "It was nice knowing you.\n");
	exit(0); 
}

/* ARGSUSED */
get_file_input(client_data, fid, id)
XtPointer client_data;	/* unused */
int *fid;
XtInputId *id;
{
	char buf[BUFSIZ];
	int nbytes;
	int i;


	if ((nbytes = read(*fid, buf, BUFSIZ)) == -1)
		perror("get_file_input");

	if (nbytes)
		for (i = 0; i < nbytes; i++)
			putchar(buf[i]);
}

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, goodbye;

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XStdin",           /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    goodbye = XtCreateManagedWidget(
	"goodbye",			/* arbitrary widget name */
	xmPushButtonWidgetClass,	/* widget class from Label.h */
	topLevel,			/* parent widget*/
	NULL,				/* argument list */
	0);				/* arg list size */

    XtAddCallback(goodbye, XmNactivateCallback, Quit, NULL);

    /* register function to handle that input */
    XtAppAddInput(app_context, fileno(stdin), XtInputReadMask, 
		  get_file_input, NULL);

    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel);
    
    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_context);
}
