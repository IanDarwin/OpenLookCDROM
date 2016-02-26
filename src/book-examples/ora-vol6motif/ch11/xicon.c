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
 * xicon.c - specifying an icon pixmap
 */

/*
 * Include files required for all Toolkit programs
 */
#include <Xm/Xm.h>	/* Intrinsics Definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <Xm/Label.h>		/* Motif Label Widget */

#include <X11/Shell.h>

#include "icon.bit"

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, hello;
    Pixmap icon_pixmap;

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
	"XIcon",            /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    icon_pixmap = XCreateBitmapFromData(XtDisplay(topLevel),
	RootWindowOfScreen(XtScreen(topLevel)),
	icon_bits,
	icon_width, icon_height );

    XtVaSetValues(topLevel, 
	XmNiconPixmap, icon_pixmap,
	NULL);
    
    hello = XtCreateManagedWidget(
	"hello",		/* arbitrary widget name */
	xmLabelWidgetClass,	/* widget class from Label.h */
	topLevel,	/* parent widget*/
	NULL,		/* argument list */
	0		/* arg list size */
	);

    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel);

    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_context);
  }
