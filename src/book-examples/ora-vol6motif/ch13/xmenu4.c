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
 *  xmenu4.c - a pulldown menu with a cascading submenu 
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
Widget pshell, subshell, subbox, pressme;


/*ARGSUSED*/
void PlaceMenu(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    Position x, y;

    /*
     * translate coordinates in application top-level window
     * into coordinates from root window origin.
     */
    XtTranslateCoords(pressme,                /* Widget */
        (Position) 0,        /* x */
        (Position) 0,       /* y */
        &x, &y);          /* coords on root window */

    /* move popup shell one pixel above and left of this position 
     * (it's not visible yet) */
    XtVaSetValues(pshell, 
            XtNx, x - 1,
            XtNy, y - 1,
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
    Dimension height;

    XtVaGetValues(w, 
            XtNheight, &height,
            NULL);

    if ((leave_event->x > 0) && (leave_event->y > 0) 
                && (leave_event->y < height)) {
        /* move submenu shell to start just right of pane,
         * using an arbitrary offset to place pointer in 
         * first item. */
        XtVaSetValues(subshell, 
                XtNx, leave_event->x_root,
                XtNy, leave_event->y_root - 12,
                NULL);
        XtPopup(subshell, XtGrabExclusive);
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
XtPointer call_data;  /* unused */
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
    Widget box, topLevel, quit, menubox, menulabel, menupane[10];
    Widget sublabel, subpane[10];
    int i;
    String buf[50];

    static XtActionsRec trial_actions[] = {
        {"checkRightAndPopupSubmenu", CheckRightAndPopupSubmenu},
        {"popdownSubmenu", PopdownSubmenu},
    };

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XMenu4",           /* application class name */
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
        pressme,		/* parent widget */
        NULL,			/* argument list */
        0			/* arglist size */
        );

    menubox = XtCreateManagedWidget(
        "menubox",              /* widget name   */
        boxWidgetClass,         /* widget class */
        pshell,                 /* parent widget*/
        NULL,                   /* argument list*/
        0           		/* arglist size */
        );

    menulabel = XtCreateManagedWidget(
        "menulabel",            /* widget name   */
        labelWidgetClass,       /* widget class */
        menubox,                /* parent widget*/
        NULL,                   /* argument list*/
        0           		/* arglist size */
        );

    XtAppAddActions(app_context, trial_actions, XtNumber(trial_actions));

    for (i = 0; i < 10; i++) {
      sprintf(buf, "menupane%d", i);
      menupane[i] = XtCreateManagedWidget(buf,   /* widget name   */
                    commandWidgetClass, menubox, NULL, 0);

      XtAddCallback(menupane[i], XtNcallback, MainPaneChosen, i);
    }

    subshell = XtCreatePopupShell(
        "subshell",		/* widget name */
        transientShellWidgetClass, /* widget class */
	pressme,		/* parent widget */
        NULL,			/* argument list */
        0			/* arglist size */
        );

    subbox = XtCreateManagedWidget(
        "subbox",               /* widget name   */
        boxWidgetClass,         /* widget class */
        subshell,               /* parent widget*/
        NULL,                   /* argument list*/
        0           		/* arglist size */
        );

    sublabel = XtCreateManagedWidget(
        "sublabel",             /* widget name   */
        labelWidgetClass,       /* widget class */
        subbox,                 /* parent widget*/
        NULL,                   /* argument list*/
        0           		/* arglist size */
        );
    
    for (i = 0; i < 10; i++) {
      sprintf(buf, "subpane%d", i);
      subpane[i] = XtCreateManagedWidget(buf,   /* widget name   */
                    commandWidgetClass, subbox, NULL, 0);

      XtAddCallback(subpane[i], XtNcallback, SubPaneChosen, i);
    }

    XtAddCallback(quit, XtNcallback, Quit, NULL);

    XtAppAddActions(app_context, trial_actions, XtNumber(trial_actions));

    XtAddCallback(pshell, XtNpopupCallback, PlaceMenu, NULL);

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}
