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
 *  Ex3-9.c - pass struct to callback function
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

/*
 * Public include files for widgets used in this file.
 */
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

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

typedef struct {
    String name;
    String street;
    String city;
} app_stuff;

/*
 * "Press me!" button callback function
 */
/*ARGSUSED*/
void PressMe(w, client_data, call_data)
Widget w;
XtPointer client_data; /* to be cast to app_stuff */
XtPointer call_data;
{ 
    app_stuff *address = (app_stuff *) client_data;

    fprintf(stderr, "%s\n%s\n%s\n", address->name, address->street, 
            address->city); 
}

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget rowColumn, quit, pressme, topLevel;

    static app_stuff stuff = {
        "John Doe",
        "1776 Constitution Way",
        "Phily, PA 01029"
    };

    /*
     * Register the default language procedure
     */
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
            &app_context,       /* Application context */
            "XRowColumn",       /* application class name */
            NULL, 0,            /* command line option list */
            &argc, argv,        /* command line args */
            NULL,               /* for missing app-defaults file */
            NULL);              /* terminate varargs list */

    rowColumn = XtVaCreateManagedWidget(
            "rowColumn",            /* widget name */
            xmRowColumnWidgetClass, /* widget class */
            topLevel,               /* parent widget*/
            NULL);                  /* argument list*/

    quit = XtVaCreateManagedWidget(
            "quit",                     /* widget name */
            xmPushButtonWidgetClass,    /* widget class */
            rowColumn,                  /* parent widget*/
            NULL);                      /* argument list*/

    pressme = XtVaCreateManagedWidget(
            "pressme",                  /* widget name   */
            xmPushButtonWidgetClass,    /* widget class */
            rowColumn,                  /* parent widget*/
            NULL);                      /* argument list*/

    XtAddCallback(quit, XmNactivateCallback, Quit, 0);
    XtAddCallback(pressme, XmNactivateCallback, PressMe, &stuff);

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}
