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
 *  xdialog.c - 
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

#include <X11/Shell.h>

/*
 * Public include files for widgets used in this file.
 */
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Dialog.h>

/*
 * The popup shell ID is global because both dialog and pshell
 * are needed in the dialogDone callback, and both can't be
 * passed in without creating a structure.
 */
Widget pshell, pressme, quit;

/*
 * dialog button
 */
/*ARGSUSED*/
void PopupDialog(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* cast to topLevel */
XtPointer call_data;
{
    Widget topLevel = (Widget) client_data;
    Position x, y;
    Dimension width, height;

    /*
     * get the coordinates of the middle of topLevel widget.
     */
    XtVaGetValues(topLevel, 
            XtNwidth, &width,
            XtNheight, &height,
            NULL);
    
    /*
     * translate coordinates in application top-level window
     * into coordinates from root window origin.
     */
    XtTranslateCoords(topLevel,                /* Widget */
            (Position) width/2,        /* x */
            (Position) height/2,       /* y */
            &x, &y);          /* coords on root window */

    /* move popup shell to this position (it's not visible yet) */
    XtVaSetValues(pshell, 
            XtNx, x,
            XtNy, y,
            NULL);
}

/*
 * dialog done button
 */
/*ARGSUSED*/
void DialogDone(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* cast to dialog */
XtPointer call_data;  /* unused */
{
    Widget dialog = (Widget) client_data;
    String string;

    XtPopdown(pshell);

    XtSetSensitive(pressme, TRUE);

    /*
     * Changes to XawDialogGetValueString in R4
     */
    string = XawDialogGetValueString(dialog);

    printf("User typed: %s\n", string);
}

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

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget box, topLevel, dialog, dialogDone;

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
	"XDialog",          /* application class name */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    box = XtCreateManagedWidget(
        "box",  	/* widget name */
        boxWidgetClass, /* widget class */
        topLevel,   	/* parent widget*/
        NULL,   	/* argument list*/
        0   		/* arglist size */
        );

    quit = XtCreateManagedWidget(
        "quit", 		/* widget name */
        commandWidgetClass, 	/* widget class */
        box,    		/* parent widget*/
        NULL,   		/* argument list*/
        0   			/* arglist size */
        );

    pressme = XtCreateManagedWidget(
        "pressme",  		/* widget name   */
        commandWidgetClass, 	/* widget class */
        box,    		/* parent widget*/
        NULL,   		/* argument list*/
        0   			/* arglist size */
        );

    pshell = XtCreatePopupShell(
        "pshell",		/* widget name */
        transientShellWidgetClass, /* widget class */
        topLevel,		/* parent widget */
        NULL,			/* argument list */
        0			/* arglist size */
        );

    dialog = XtCreateManagedWidget(
        "dialog",               /* widget name   */
        dialogWidgetClass,      /* widget class */
        pshell,                 /* parent widget*/
        NULL,                   /* argument list*/
        0           		/* arglist size */
        );

    dialogDone = XtCreateManagedWidget(
        "dialogDone",           /* widget name   */
        commandWidgetClass,     /* widget class */
        dialog,                 /* parent widget*/
        NULL,                   /* argument list*/
        0      			/* arglist size */
        );

    XtAddCallback(quit, XtNcallback, Quit, NULL);

    XtAddCallback(dialogDone, XtNcallback, DialogDone, dialog);

    XtAddCallback(pressme, XtNcallback, PopupDialog, topLevel);
    XtAddCallback(pressme, XtNcallback, XtCallbackExclusive, pshell);

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}
