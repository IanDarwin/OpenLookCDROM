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
 *  xbitmap3.c - bitmap in main window with small pixmaps
 *               implemented with translations and actions
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
#include <Xm/PanedW.h>    /* paned window */
#include <Xm/PushB.h>     /* push button */
#include <Xm/MessageB.h>  /* message box */
#include <Xm/CascadeB.h>  /* cascade button */
#include <Xm/DrawingA.h>  /* drawing area */
#include <Xm/RowColumn.h> /* row column */
#include <Xm/Frame.h>     /* frame */
#include <Xm/ScrolledW.h> /* scrolled window */

#include "BitmapEdit.h"

#define DRAWN 1
#define UNDRAWN 0

struct {
    GC draw_gc, undraw_gc;
    Pixmap normal_bitmap, reverse_bitmap;
    Widget showNormalBitmap, showReverseBitmap;
    String filename;    /* filename to read and write */
    Dimension pixmap_width_in_cells, pixmap_height_in_cells;
} bitmap_stuff;

static void CellToggled();

/* ARGSUSED */
static void
RedrawSmallPicture(w, event, params, num_params)
Widget w;
XExposeEvent *event;
String *params;
Cardinal *num_params;
{
    Pixmap pixmap;

    if (w == bitmap_stuff.showNormalBitmap)
        pixmap = bitmap_stuff.normal_bitmap;
    else
        pixmap = bitmap_stuff.reverse_bitmap;

    if (DefaultDepthOfScreen(XtScreen(w)) == 1)
        XCopyArea(XtDisplay(w), pixmap, XtWindow(w),
                DefaultGCOfScreen(XtScreen(w)), event->x, event->y, 
                event->width, event->height, event->x, event->y);
    else
        XCopyPlane(XtDisplay(w), pixmap, XtWindow(w),
                DefaultGCOfScreen(XtScreen(w)), event->x, event->y, 
                event->width, event->height, event->x, event->y, 1);
}

/*
 * The printout routine writes the data into a standard X11 bitmap file.
 */
/* ARGSUSED */
static void 
PrintOut(widget, client_data, call_data)
Widget widget;
XtPointer client_data;   /* unused */
XtPointer call_data;    /* unused */
{
    XWriteBitmapFile(XtDisplay(widget), bitmap_stuff.filename, 
            bitmap_stuff.normal_bitmap,
            bitmap_stuff.pixmap_width_in_cells, 
            bitmap_stuff.pixmap_height_in_cells, 0, 0);
}

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
    Widget topLevel, mainWindow, menuBar;
    Widget fileButton, fileMenu, quit, helpButton, helpMenu, help, helpBox;
    Widget temp;
    Widget bigBitmap, output, smallPixmapBox;
    Widget scrolledWin, frame1, frame2;

    /* never call a Widget variable "exit"! */
    extern exit();

    static XrmOptionDescRec table[] = {
        {"-pw",            "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
        {"-pixmapwidth",   "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
        {"-ph",            "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
        {"-pixmapheight",  "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
        {"-cellsize",      "*cellSizeInPixels",           XrmoptionSepArg, NULL},

    };
    
    static XtActionsRec actions[] = {
        {"RedrawSmallPicture", RedrawSmallPicture},
    };

    /*static char trans[] = "<Expose>: RedrawSmallPicture()";*/

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
            &app_context,       /* Application context */
            "XBitmap3", 	/* Application class */
            table, XtNumber(table),   /* command line option list */
            &argc, argv,        /* command line args */
            NULL,               /* for missing app-defaults file */
            NULL);              /* terminate varargs list */

    if (argv[1] != NULL)
         bitmap_stuff.filename = argv[1];
    else {
         fprintf(stderr, "xbitmap: must specify filename on command line\n");
         exit(1);
    }

    /* create main window */
    mainWindow = XtVaCreateManagedWidget(
            "mainWindow",   		/* widget name */
            xmPanedWindowWidgetClass,   /* widget class */
            topLevel,   		/* parent widget*/
            NULL);              	/* terminate varargs list */

    /* create menu bar along top inside of main window */
    menuBar = XmCreateMenuBar(
            mainWindow, /* parent widget*/
            "menuBar",  /* widget name */
            NULL,       /* no arguments needed */
            0);         /* no arguments needed */
    XtManageChild(menuBar);

    scrolledWin = XtVaCreateManagedWidget("scrolledWin", 
            xmScrolledWindowWidgetClass, mainWindow, 
            NULL);

    bigBitmap = XtVaCreateManagedWidget("bigBitmap", 
            bitmapEditWidgetClass, scrolledWin, 
            NULL);

    XtVaGetValues(bigBitmap,
            XtNpixmapWidthInCells, &bitmap_stuff.pixmap_width_in_cells,
            XtNpixmapHeightInCells, &bitmap_stuff.pixmap_height_in_cells,
            NULL);

    /*
     *  CREATE FILE MENU AND CHILDREN
     */

    /* create button that will pop up the menu */
    fileButton = XtVaCreateManagedWidget(
            "fileButton",   		/* widget name */
            xmCascadeButtonWidgetClass, /* widget class */
            menuBar,    		/* parent widget*/
            NULL);              	/* terminate varargs list */

    /* create menu (really a Shell widget and RowColumn widget combo) */
    fileMenu = XmCreatePulldownMenu(
            menuBar,    /* parent widget*/
            "fileMenu", /* widget name */
            NULL,       /* no argument list needed */
            0);         /* no argument list needed */

    /*
     *  CREATE BUTTON TO OUTPUT BITMAP
     */

    /* create button that will pop up the menu */
    output = XtVaCreateManagedWidget(
            "output",   		/* widget name */
            xmPushButtonWidgetClass,    /* widget class */
            fileMenu,   		/* parent widget*/
            NULL);              	/* terminate varargs list */

    XtAddCallback(output, XmNactivateCallback, PrintOut, 0);

    /* create the quit button up in the menu */
    quit = XtVaCreateManagedWidget(
            "quit", 			/* widget name */
            xmPushButtonWidgetClass,    /* widget class */
            fileMenu,   		/* parent widget*/
            NULL);              	/* terminate varargs list */
    
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
    helpButton = XtVaCreateManagedWidget( "helpButton",
	    xmCascadeButtonWidgetClass, menuBar, NULL);

    /* tell menuBar which is the help button (will be specially positioned) */
    XtVaSetValues(menuBar,
		  XmNmenuHelpWidget, helpButton,
		  NULL);

    /* create menu (really a Shell widget and RowColumn widget combo) */
    helpMenu = XmCreatePulldownMenu( menuBar,
            "helpMenu", NULL, 0);

    /* create the help button up in the menu */
    help = XtVaCreateManagedWidget( "help",
            xmPushButtonWidgetClass, helpMenu, NULL);

    /* 
     * Specify which menu helpButton will pop up.
     */
    XtVaSetValues(helpButton,
		  XmNsubMenuId, helpMenu,
		  NULL);

    /* create popup that will contain help */
    helpBox = XmCreateMessageDialog(
            help,    /* parent widget*/
            "helpBox",  /* widget name   */
            NULL,       /* no arguments needed */
            0);         /* no arguments needed */

    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild (temp);
    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild (temp);

    /* arrange for getHelp button to pop up helpBox */
    XtAddCallback(help, XmNactivateCallback, ShowHelp, helpBox);

    /*
     * Create box containing two small bitmaps.
     */

    /* create pixmaps and GCs needed for expose strategy */
    SetUpThings(topLevel);

    smallPixmapBox = XtVaCreateManagedWidget("smallPixmapBox", 
            xmRowColumnWidgetClass, mainWindow, 
            NULL);

    frame1 = XtVaCreateManagedWidget("frameNormal", 
            xmFrameWidgetClass, smallPixmapBox, 
            NULL);

    bitmap_stuff.showNormalBitmap = XtVaCreateManagedWidget("showNormalBitmap", 
            xmPrimitiveWidgetClass, frame1, 
            /*XmNtranslations, XtParseTranslationTable(trans),*/
            XmNwidth, bitmap_stuff.pixmap_width_in_cells,
            XmNheight, bitmap_stuff.pixmap_height_in_cells,
            NULL);

    frame2 = XtVaCreateManagedWidget("frameReverse", 
        xmFrameWidgetClass, smallPixmapBox, 
        NULL);

    bitmap_stuff.showReverseBitmap = XtVaCreateManagedWidget("showReverseBitmap", 
        xmPrimitiveWidgetClass, frame2, 
        /*XmNtranslations, XtParseTranslationTable(trans),*/
        XmNwidth, bitmap_stuff.pixmap_width_in_cells,
        XmNheight, bitmap_stuff.pixmap_height_in_cells,
        NULL);

    XtAddCallback(bigBitmap, XtNtoggleCallback, CellToggled, 0);

    XtAppAddActions(app_context, actions, XtNumber(actions));


    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
  }

SetUpThings(w)
Widget w;
{
    XGCValues values;


    bitmap_stuff.normal_bitmap = XCreatePixmap(XtDisplay(w), 
            RootWindowOfScreen(XtScreen(w)),
            bitmap_stuff.pixmap_width_in_cells, bitmap_stuff.pixmap_height_in_cells, 1);

    bitmap_stuff.reverse_bitmap = XCreatePixmap(XtDisplay(w), 
            RootWindowOfScreen(XtScreen(w)),
            bitmap_stuff.pixmap_width_in_cells, bitmap_stuff.pixmap_height_in_cells, 1);

    values.foreground = 1;
    values.background = 0;
    /* note that normal_bitmap is used as the drawable because it
     * is one bit deep.  The root window may not be one bit deep */
    bitmap_stuff.draw_gc = XCreateGC(XtDisplay(w),  bitmap_stuff.normal_bitmap,
            GCForeground | GCBackground, &values);

    values.foreground = 0;
    values.background = 1;
    bitmap_stuff.undraw_gc = XCreateGC(XtDisplay(w), 
            bitmap_stuff.normal_bitmap,
            GCForeground | GCBackground, &values);

    /* pixmaps must be cleared - may contain garbage */
    XFillRectangle(XtDisplay(w), 
            bitmap_stuff.reverse_bitmap, bitmap_stuff.draw_gc,
            0, 0, bitmap_stuff.pixmap_width_in_cells + 1, 
            bitmap_stuff.pixmap_height_in_cells + 1);
    XFillRectangle(XtDisplay(w), 
            bitmap_stuff.normal_bitmap, bitmap_stuff.undraw_gc,
            0, 0, bitmap_stuff.pixmap_width_in_cells + 1, 
            bitmap_stuff.pixmap_height_in_cells + 1);
}

/* ARGSUSED */
static void
CellToggled(w, client_data, call_data)
Widget w;
XtPointer client_data;  /* unused */
XtPointer call_data;    /* will be cast to cur_info */
{
    /* cast pointer to needed type: */
    BitmapEditPointInfo *cur_info = (BitmapEditPointInfo *) call_data;
    /* 
     * Note, BitmapEditPointInfo is defined in BitmapEdit.h 
     */
    XExposeEvent pseudo;

    XDrawPoint(XtDisplay(w), bitmap_stuff.normal_bitmap, 
            ((cur_info->mode == DRAWN) ? bitmap_stuff.draw_gc : 
            bitmap_stuff.undraw_gc), cur_info->newx, cur_info->newy);
    XDrawPoint(XtDisplay(w), bitmap_stuff.reverse_bitmap, 
            ((cur_info->mode == DRAWN) ? bitmap_stuff.undraw_gc : 
            bitmap_stuff.draw_gc), cur_info->newx, cur_info->newy); 

    pseudo.x = cur_info->newx;
    pseudo.y = cur_info->newy;
    pseudo.width = pseudo.height = 1;

    /* call RedrawSmallPicture, passing it a simulated event */
    RedrawSmallPicture(bitmap_stuff.showNormalBitmap, &pseudo);
    RedrawSmallPicture(bitmap_stuff.showReverseBitmap, &pseudo);
}
