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
 *  xmenu5.c - cascaded spring-loaded menu
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
#include <X11/Xaw/Label.h>

/*
 * The popup shell ID is global because both dialog and pshell
 * are needed in the dialogDone callback, and both can't be
 * passed in without creating a structure.
 */
Widget pshell, menubox, subshell, subbox;


/*ARGSUSED*/
void PlaceMenu(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    XButtonEvent *bevent = (XButtonEvent *) event;

    /* should make sure coordinates allow menu to fit on screen */

    /* move submenu shell to slightly left and above button
     * press position */
    XtVaSetValues(pshell, 
            XtNx, bevent->x_root - 10,
            XtNy, bevent->y_root - 10,
            NULL);
}

/*ARGSUSED*/
void CheckRightAndPopupSubmenu(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    XLeaveWindowEvent *leave_event = (XLeaveWindowEvent *) event;
    Dimension height, width;

    XtVaGetValues(w, 
        XtNheight, &height,
        XtNwidth, &width,
        NULL);

    if ((leave_event->x > width) && (leave_event->y > 0) && (leave_event->y < height)) {
        /* move submenu shell to start just right of pane,
         * using an arbitrary offset to place pointer in 
         * first item. */
        XtVaSetValues(subshell, 
            XtNx, leave_event->x_root,
            XtNy, leave_event->y_root - 12,
            NULL);
        XtPopup(subshell, XtGrabNonexclusive);
    }
}

/*ARGSUSED*/
void PopdownSubmenu(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    XtPopdown(subshell);
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

/*
 * menu pane button callback function
 */
/*ARGSUSED*/
void SubPaneChosen(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* cast to pane_number */
XtPointer call_data;
{ 
    int pane_number = (int) client_data;
    printf("SubPane %d chosen.\n", pane_number);
    XtPopdown(subshell);
    XtPopdown(pshell);
}

/*
 * menu pane button callback function
 */
/*ARGSUSED*/
void MainPaneChosen(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{ 
    int pane_number = (int) client_data;
    printf("Pane %d chosen.\n", pane_number);

    XtPopdown(pshell);
    XtPopdown(subshell);
}

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget box, topLevel, quit, label, menulabel, menupane[10], sublabel, subpane[10];
    int i;
    String buf[50];

    static XtActionsRec trial_actions[] = {
        {"checkRightAndPopupSubmenu", CheckRightAndPopupSubmenu},
        {"popdownSubmenu", PopdownSubmenu},
        {"placeMenu", PlaceMenu},
    };

        XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XMenu5",   /* application class name */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    box = XtCreateManagedWidget(
        "box",  /* widget name */
        boxWidgetClass, /* widget class */
        topLevel,   /* parent widget*/
        NULL,   /* argument list*/
        0   /* arglist size */
        );

    quit = XtCreateManagedWidget(
        "quit", /* widget name */
        commandWidgetClass, /* widget class */
        box,    /* parent widget*/
        NULL,   /* argument list*/
        0   /* arglist size */
        );

    label = XtCreateManagedWidget(
        "label",    /* widget name   */
        labelWidgetClass,   /* widget class */
        box,    /* parent widget*/
        NULL,   /* argument list*/
        0   /* arglist size */
        );

    pshell = XtCreatePopupShell(
        "pshell",
        overrideShellWidgetClass,
        topLevel,
        NULL,
        0
        );

    menubox = XtCreateManagedWidget(
        "menubox",               /* widget name   */
        boxWidgetClass,              /* widget class */
        pshell,                         /* parent widget*/
        NULL,                    /* argument list*/
        0           /* arglist size */
        );
    
    menulabel = XtCreateManagedWidget(
        "menulabel",               /* widget name   */
        labelWidgetClass,              /* widget class */
        menubox,                         /* parent widget*/
        NULL,                    /* argument list*/
        0           /* arglist size */
        );

    XtAppAddActions(app_context, trial_actions, XtNumber(trial_actions));

    for (i = 0; i < 10; i++) {
        sprintf(buf, "menupane%d", i);
            menupane[i] = XtCreateManagedWidget((String)buf,   /* widget name   */
                    commandWidgetClass, menubox, NULL, 0);

        XtAddCallback(menupane[i], XtNcallback, MainPaneChosen, (XtPointer) i);
    }

    subshell = XtCreatePopupShell(
        "subshell",
        overrideShellWidgetClass,
        topLevel,
        NULL,
        0
        );

    subbox = XtCreateManagedWidget(
        "subbox",               /* widget name   */
        boxWidgetClass,              /* widget class */
        subshell,                         /* parent widget*/
        NULL,                    /* argument list*/
        0           /* arglist size */
        );

    sublabel = XtCreateManagedWidget(
        "sublabel",               /* widget name   */
        labelWidgetClass,              /* widget class */
        subbox,                         /* parent widget*/
        NULL,                    /* argument list*/
        0           /* arglist size */
        );
    
    for (i = 0; i < 10; i++) {
        sprintf(buf, "subpane%d", i);
            subpane[i] = XtCreateManagedWidget((String)buf,   /* widget name   */
                    commandWidgetClass, subbox, NULL, 0);

        XtAddCallback(subpane[i], XtNcallback, SubPaneChosen, (XtPointer) i);
    }

    XtAddCallback(quit, XtNcallback, Quit, 0);

    XtAppAddActions(app_context, trial_actions, XtNumber(trial_actions));

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}
