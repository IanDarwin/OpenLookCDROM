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
 *  xmainwindow.c - main window with help and quit
 */

/*
 *  So that we can use fprintf:
 */
#include <stdio.h>

/* 
 * Standard Toolkit include files:
 */
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RepType.h>
/*
 * Public include files for widgets used in this file.
 */
#include <Xm/PushB.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/CascadeB.h>
#include <Xm/Frame.h>

/* 
 * callback to pop up help dialog widget 
 */
/*ARGSUSED*/
void ShowHelp(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Widget dialog = (Widget) client_data;
    XtManageChild(dialog);
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
    Widget topLevel, mainWindow, menuBar, frame;
    Widget fileButton, fileMenu, quit, helpButton, helpMenu, help, helpBox;
    Widget temp;

    /*
     * Register the default language procedure
     */
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
            &app_context,       /* Application context */
            "XMainWindow",      /* application class name */
            NULL, 0,            /* command line option list */
            &argc, argv,        /* command line args */
            NULL,               /* for missing app-defaults file */
            NULL);              /* terminate varargs list */

    /* create main window */
    mainWindow = XtVaCreateManagedWidget(
            "mainWindow",               /* widget name */
            xmMainWindowWidgetClass,    /* widget class */
            topLevel,                   /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* register converter for tearoff menus */
    XmRepTypeInstallTearOffModelConverter();

    /* create menu bar along top inside of main window */
    menuBar = XmCreateMenuBar(
            mainWindow, /* parent widget*/
            "menuBar",  /* widget name */
            NULL,       /* no arguments needed */
            0);         /* no arguments needed */
    XtManageChild(menuBar);

    frame = XtVaCreateManagedWidget(
            "frame",            /* widget name */
            xmFrameWidgetClass, /* widget class */
            mainWindow,         /* parent widget*/
            NULL);              /* terminate varargs list */

    /*  Set MainWindow areas */
    XmMainWindowSetAreas (mainWindow, menuBar, NULL, NULL, NULL, frame);

    /*
     *  CREATE FILE MENU AND CHILDREN
     */

    /* create button that will pop up the menu */
    fileButton = XtVaCreateManagedWidget(
            "fileButton",               /* widget name */
            xmCascadeButtonWidgetClass, /* widget class */
            menuBar,                    /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* create menu (really a Shell widget and RowColumn widget combo) */
    fileMenu = XmCreatePulldownMenu(
            menuBar,    /* parent widget*/
            "fileMenu", /* widget name */
            NULL,       /* no argument list needed */
            0);         /* no argument list needed */

    /* create the quit button up in the menu */
    quit = XtVaCreateManagedWidget(
            "quit",                     /* widget name */
            xmPushButtonWidgetClass,    /* widget class */
            fileMenu,                   /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* 
     * Specify which menu fileButton will pop up.
     */
    XtVaSetValues(fileButton,
		  XmNsubMenuId, fileMenu,
		  NULL);

    /* arrange for quit button to call function that exits. */
    XtAddCallback(quit, XmNactivateCallback, Quit, 0);

    /*
     *  CREATE HELP BUTTON AND BOX
     */

    /* create button that will bring up help menu */
    helpButton = XtVaCreateManagedWidget(
            "helpButton",               /* widget name */
            xmCascadeButtonWidgetClass, /* widget class */
            menuBar,                    /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* tell menuBar which is the help button (will be specially positioned) */
    XtVaSetValues(menuBar,
		  XmNmenuHelpWidget, helpButton,
		  NULL);

    /* create menu (really a Shell widget and RowColumn widget combo) */
    helpMenu = XmCreatePulldownMenu(
            menuBar,    /* parent widget*/
            "helpMenu", /* widget name */
            NULL,       /* no argument list needed */
            0);         /* no argument list needed */

    /* create the help button up in the menu */
    help = XtVaCreateManagedWidget(
            "help",                     /* widget name */
            xmPushButtonWidgetClass,    /* widget class */
            helpMenu,                   /* parent widget*/
            NULL);                      /* terminate varargs list */

    /* 
     * Specify which menu helpButton will pop up.
     */
    XtVaSetValues(helpButton,
		  XmNsubMenuId, helpMenu,
		  NULL);

    /* create popup that will contain help */
    helpBox = XmCreateMessageDialog(
	    help,       /* parent widget*/
            "helpBox",  /* widget name   */
            NULL,       /* no arguments needed */
            0);         /* no arguments needed */

    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild (temp);
    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild (temp);

    /* arrange for getHelp button to pop up helpBox */
    XtAddCallback(help, XmNactivateCallback, ShowHelp, helpBox);

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}
