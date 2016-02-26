/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 *
 *   The X Consortium, and any party obtaining a copy of these files from
 *   the X Consortium, directly or indirectly, is granted, free of charge, a
 *   full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *   nonexclusive right and license to deal in this software and
 *   documentation files (the "Software"), including without limitation the
 *   rights to use, copy, modify, merge, publish, distribute, sublicense,
 *   and/or sell copies of the Software, and to permit persons who receive
 *   copies from any such party to do so.  This license includes without
 *   limitation a license to do the foregoing actions under any patents of
 *   the party supplying this software to the X Consortium.
 */

/* file_menu2.c -- demonstrate how to create a menu bar and pulldown
 * menu using the Motif creation routines.  Has tear off functionality.
 */
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>

main(argc, argv)
int argc;
char *argv[];
{
    Widget toplevel, MainWindow, MenuBar, FilePullDown;
    XmString    label_str;
    XtAppContext app;

    XtSetLanguageProc (NULL, NULL, NULL);

    toplevel = XtVaAppInitialize (&app, "Demos", NULL, 0,
        &argc, argv, NULL, NULL);

    MainWindow = XtVaCreateManagedWidget ("main_w",
        xmMainWindowWidgetClass, toplevel,
        XmNscrollingPolicy,  XmAUTOMATIC,
        NULL);

    MenuBar = XmCreateMenuBar (MainWindow, "MenuBar", NULL, 0); 

    /* create the "File" Menu */
    FilePullDown = XmCreatePulldownMenu (MenuBar, "FilePullDown", NULL, 0);
    XtVaSetValues (FilePullDown, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);

    /* create the "File" button (attach Menu via XmNsubMenuId) */
    label_str = XmStringCreateLocalized ("File");
    XtVaCreateManagedWidget ("File", 
	xmCascadeButtonWidgetClass, MenuBar,
	XmNlabelString,  label_str,
	XmNmnemonic,    'F',
	XmNsubMenuId,    FilePullDown,
	NULL);
    XmStringFree (label_str); 

    /* Now add the menu items */
    XtVaCreateManagedWidget ("Open",
	xmPushButtonGadgetClass, FilePullDown, NULL);

    XtVaCreateManagedWidget ("Save",
	xmPushButtonGadgetClass, FilePullDown, NULL);

    XtVaCreateManagedWidget ("separator",
	xmSeparatorGadgetClass, FilePullDown, NULL);

    XtVaCreateManagedWidget ("Exit",
	xmPushButtonGadgetClass, FilePullDown, NULL);

    XtManageChild (MenuBar);

    XtRealizeWidget (toplevel);
    XtAppMainLoop (app);
}
