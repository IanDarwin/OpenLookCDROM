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
 *  xpopupcasc.c - Motif popup menu with cascading submenu
 */

/*
 *  So that we can use fprintf:
 */
#include <stdio.h>

/* 
 * Standard Toolkit include files:
 */
#include <Xm/Xm.h>

/*
 * Public include files for widgets used in this file.
 */
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/CascadeB.h>

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
void PaneChosen(w, client_data, call_data)
Widget w;
XtPointer client_data;   /* cast to pane_number */
XtPointer call_data;
{ 
    int pane_number = (int) client_data;
    printf("Pane %d chosen.\n", pane_number);
}

static void  
PostMenu (w, client_data, event)
Widget         w;
XtPointer      client_data;
XEvent  *event;
{
    Widget popup = (Widget) client_data;

    XmMenuPosition(popup, event);
    XtManageChild (popup);
}

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, box, quit, label, menupane[10];
    Widget menu, cascade, submenu, subentry[10];
    int i;
    String buf[50];

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XPopup",   	    /* application class name */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    box = XtVaCreateManagedWidget(
        "box",  		/* widget name */
        xmRowColumnWidgetClass, /* widget class */
        topLevel,   		/* parent widget*/
        NULL);   		/* argument list*/

    label = XtVaCreateManagedWidget(
        "label",    		/* widget name */
        xmLabelWidgetClass,   	/* widget class */
        box,    		/* parent widget*/
        NULL);   		/* argument list*/

    quit = XtVaCreateManagedWidget(
        "quit", 		/* widget name */
        xmPushButtonWidgetClass,/* widget class */
        box,    		/* parent widget*/
        NULL);   		/* argument list*/

    XtAddCallback(quit, XmNactivateCallback, Quit, NULL);

    menu = XmCreatePopupMenu(label, "menu", NULL, 0);

    XtAddEventHandler(label, ButtonPressMask, False, PostMenu, menu);

    for (i = 0; i < 10; i++) {
      sprintf(buf, "entry%d", i);
      menupane[i] = XtVaCreateManagedWidget(buf,   /* widget name   */
                    xmPushButtonWidgetClass, menu, NULL, 0);

      XtAddCallback(menupane[i], XmNactivateCallback, PaneChosen, i);
    }

    submenu = XmCreatePulldownMenu(menu, "submenu", NULL, 0);

    cascade = XtVaCreateManagedWidget("cascade", 
				      xmCascadeButtonWidgetClass, menu, 
				      XmNsubMenuId, submenu,
				      NULL);

    for (i = 0; i < 10; i++) {
      sprintf(buf, "subentry%d", i);
      subentry[i] = XtVaCreateManagedWidget(buf,   /* widget name   */
                    xmPushButtonWidgetClass, submenu, NULL);
      XtAddCallback(subentry[i], XmNactivateCallback, PaneChosen, i);
    }

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}
