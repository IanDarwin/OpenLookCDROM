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
 * xpixmap.c - simple program to test invoking a converter directly
 */

#include <X11/IntrinsicP.h>	/* temporary, until we find out how
				 * to register a type converter without
				 * referencing the core structure */

/*
 * Include files required for all Toolkit programs
 */
#include <X11/Intrinsic.h>	/* Intrinsics Definitions */
#include <X11/StringDefs.h>	/* Standard Name-String definitions */

/*
 * Public include file for widgets we actually use in this file.
 */
#include <Xm/Label.h>		/* Motif Label Widget */

/*
 * Include file for Xmu converters
 */
#include <X11/Xmu/Converters.h>

#define OURPIXMAPNAME "xlogo64"

main(argc, argv)
int argc;
char **argv;
{
    XtAppContext app_context;
    Widget topLevel, hello;
    XrmValue from, to;
    Pixmap pixmap;
    void XmuCvtStringToBitmap();
    Boolean success;

    static XtConvertArgRec screenConvertArg[] = {
      {
	XtBaseOffset, 
	(XtPointer) XtOffset(Widget, core.screen), 
	sizeof(Screen *)
	}
    };
    
    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
        &app_context,       /* Application context */
        "XHello",           /* Application class */
        NULL, 0,            /* command line option list */
        &argc, argv,        /* command line args */
        NULL,               /* for missing app-defaults file */
        NULL);              /* terminate varargs list */

    XtAppAddConverter(app_context, XmRString, XmRBitmap, XmuCvtStringToBitmap,
		      screenConvertArg, XtNumber(screenConvertArg));

    from.addr = OURPIXMAPNAME;
    from.size = strlen(OURPIXMAPNAME);

    success = XtConvertAndStore(topLevel, XmRString, &from, XmRBitmap, &to);

    if (!success)
      exit(1);
	
    pixmap = *(Pixmap *) to.addr;


    hello = XtVaCreateManagedWidget(
	"hello",		/* arbitrary widget name */
	xmLabelWidgetClass,	/* widget class from Label.h */
	topLevel,		/* parent widget*/
	XmNlabelType, XmPIXMAP,
        XmNlabelPixmap, pixmap,
	NULL);

    /*
     *  Create windows for widgets and map them.
     */
    XtRealizeWidget(topLevel);
    
    /*
     *  Loop for events.
     */
    XtAppMainLoop(app_context);
}
