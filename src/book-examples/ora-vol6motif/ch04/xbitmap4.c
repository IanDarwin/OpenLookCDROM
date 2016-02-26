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
 * xbitmap4.c
 */
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>

#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/MessageB.h> 
#include <Xm/CascadeB.h> 
#include <Xm/RowColumn.h>
/* we use XmPrimitive, but no header file needed (in Xm.h) */

#include <stdio.h>

/* 
 * The following could be placed in an "xbitmap.h" file.
 */
#define XtNdebug "debug"
#define XtCDebug "Debug"
#define XtNpixmapWidthInCells "pixmapWidthInCells"
#define XtCPixmapWidthInCells "PixmapWidthInCells"
#define XtNpixmapHeightInCells "pixmapHeightInCells"
#define XtCPixmapHeightInCells "PixmapHeightInCells"
#define XtNcellSizeInPixels "cellSizeInPixels"
#define XtCCellSizeInPixels "CellSizeInPixels"

#define DRAWN 1
#define UNDRAWN 0

#define DRAW 1
#define UNDRAW 0

#define MAXLINES  1000

#define MINBITMAPWIDTH  2
#define MAXBITMAPWIDTH  1000
#define MINBITMAPHEIGHT  2
#define MAXBITMAPHEIGHT  1000
#define MINCELLSIZE  4
#define MAXCELLSIZE  100

#define SCROLLBARWIDTH 15

/* 
 * Data structure for private data.
 * (This avoids lots of global variables.)
 */
typedef struct {
    Pixmap big_picture;
    GC draw_gc, undraw_gc; /* for drawing into the big_picture, 1-bit deep */
    GC copy_gc; /* for copying from pixmap into window, screen depth */
    Widget bitmap;  /* this is the drawing surface */
    char *cell;  /* this is the array for printing output 
                  * and keeping track of cells drawn */
    int cur_x, cur_y;
    Dimension pixmap_width_in_pixels, pixmap_height_in_pixels;
} PrivateAppData;

/* data structure for application resources */
typedef struct {
    Pixel copy_fg;
    Pixel copy_bg;
    int pixmap_width_in_cells;
    int pixmap_height_in_cells;
    int cell_size_in_pixels;
    Boolean debug;
} AppData;

AppData app_data;
PrivateAppData private_app_data;

/* resource list */
static XtResource resources[] = {
    {
        XmNforeground,
        XmCForeground,
        XmRPixel,
        sizeof(Pixel),
        XtOffsetOf(AppData, copy_fg),
        XmRString,
        XtDefaultForeground
    },
    {
        XmNbackground,
        XmCBackground,
        XmRPixel,
        sizeof(Pixel),
        XtOffsetOf(AppData, copy_bg),
        XmRString,
        XtDefaultBackground
    },
    {
        XtNpixmapWidthInCells,
        XtCPixmapWidthInCells,
        XmRInt,
        sizeof(int),
        XtOffsetOf(AppData, pixmap_width_in_cells),
        XmRImmediate,
        (XtPointer) 32,
    },
    {
        XtNpixmapHeightInCells,
        XtCPixmapHeightInCells,
        XmRInt,
        sizeof(int),
        XtOffsetOf(AppData, pixmap_height_in_cells),
        XmRImmediate,
        (XtPointer) 32,
    },
    {
        XtNcellSizeInPixels,
        XtCCellSizeInPixels,
        XmRInt,
        sizeof(int),
        XtOffsetOf(AppData, cell_size_in_pixels),
        XmRImmediate,
        (XtPointer) 30,
    },
    {
        XtNdebug,
        XtCDebug,
        XmRBoolean,
        sizeof(Boolean),
        XtOffsetOf(AppData, debug),
        XmRImmediate,
        (XtPointer) FALSE,
    },
};

/* Command-line options table */
static XrmOptionDescRec options[] = {
    {"-pw",            "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
    {"-pixmapwidth",   "*pixmapWidthInCells",        XrmoptionSepArg, NULL},
    {"-ph",            "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
    {"-pixmapheight",  "*pixmapHeightInCells",       XrmoptionSepArg, NULL},
    {"-cellsize",      "*cellSizeInPixels",           XrmoptionSepArg, NULL},
    {"-fg",            "*foreground",           XrmoptionSepArg, NULL},
    {"-foreground",    "*foreground",           XrmoptionSepArg, NULL},
    {"-debug",    "*debug",           XrmoptionNoArg, "True"},
};

/* callback function to print cell array to stdout */
/* ARGSUSED */
static void 
PrintOut(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    /*
     * The absense of the small pixmaps in this version makes it
     * more difficult to call XWriteBitmapFile.  Therefore, we will
     * stick with the printed output for this version.
     */
    int x, y;
    putchar('\n');
    for (y = 0; y < app_data.pixmap_height_in_cells; y++) {
        for (x = 0; x < app_data.pixmap_width_in_cells; x++)
            putchar(private_app_data.cell[x + y * 
                    app_data.pixmap_width_in_cells] ? '1' : '0');
        putchar('\n');
    }
    putchar('\n');
}

static void RedrawPicture(), DrawCell(), UndrawCell(), ToggleCell(), DrawPixmaps();

static void Syntax(argc, argv)
int argc;
char * argv[];
{
    int i;
    static int errs = False;

    /* first argument is program name - skip that */
    for (i = 1; i < argc; i++) {
        if (!errs++) /* do first time through */
            fprintf(stderr, "xbitmap4: command line option not understood:\n");
        fprintf(stderr, "option: %s\n", argv[i]);
    }

    fprintf(stderr, "xbitmap understands all standard Xt command line options.\n");

    fprintf(stderr, "Additional options are as follows:\n");
    fprintf(stderr, "Option             Valid Range\n");
    fprintf(stderr, "-pw                MINBITMAPWIDTH to MAXBITMAPWIDTH\n");
    fprintf(stderr, "-pixmapwidth       MINBITMAPWIDTH to MAXBITMAPWIDTH\n");
    fprintf(stderr, "-ph                MINBITMAPHEIGHT to MAXBITMAPHEIGHT\n");
    fprintf(stderr, "-pixmapheight      MINBITMAPHEIGHT to MAXBITMAPHEIGHT\n");
    fprintf(stderr, "-cellsize          MINCELLSIZE to MAXCELLSIZE\n");
    fprintf(stderr, "-fg                color name\n");
    fprintf(stderr, "-foreground        color name\n");
    fprintf(stderr, "-debug             no value necessary\n");
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
char *argv[];
{
    XtAppContext app_context;
    Widget topLevel, mainWindow, menuBar;
    Widget fileButton, fileMenu, quit, output, helpButton, helpMenu, help;
    Widget helpBox, temp;
    extern exit();

    /* translation table for bitmap core widget */
    String trans =
    "<Expose>:  RedrawPicture()         \n\
         <Btn1Down>:    DrawCell()              \n\
         <Btn2Down>:    UndrawCell()            \n\
         <Btn3Down>:    ToggleCell()            \n\
         <Btn1Motion>:  DrawCell()              \n\
         <Btn2Motion>:  UndrawCell()            \n\
         <Btn3Motion>:  ToggleCell()";

    static XtActionsRec window_actions[] = {
        {"RedrawPicture",   RedrawPicture},
        {"DrawCell",    DrawCell},
        {"UndrawCell",  UndrawCell},
        {"ToggleCell",  ToggleCell},
    };

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

    topLevel = XtVaAppInitialize(
            &app_context,       /* Application context */
            "XBitmap4", 
            options, XtNumber(options), 
            &argc, argv,        /* command line args */
            NULL,               /* for missing app-defaults file */
            NULL);              /* terminate varargs list */

    /* XtInitialize leaves program name in args */
    if (argc > 1)
        Syntax(argc, argv);

    XtGetApplicationResources(topLevel, 
            &app_data, 
            resources, 
            XtNumber(resources), 
            NULL, 
            0);

    /*
     * We must check the application resource values here.
     * Otherwise, user could supply out of range values and crash program.
     * Conversion routines do this automatically, so colors are already checked.
     */
    if ((app_data.pixmap_width_in_cells > MAXBITMAPWIDTH) || 
            (app_data.pixmap_width_in_cells < MINBITMAPWIDTH) ||
            (app_data.pixmap_height_in_cells > MAXBITMAPWIDTH) || 
            (app_data.pixmap_height_in_cells < MINBITMAPWIDTH)) {
        fprintf(stderr, "xbitmap: error in resource settings:\
                dimension must be between %d and %d cells\n", 
                MINBITMAPWIDTH, MAXBITMAPWIDTH);
        exit(1);
    }
    if ((app_data.cell_size_in_pixels < MINCELLSIZE) || 
            (app_data.cell_size_in_pixels > MAXCELLSIZE)) {
        fprintf(stderr, "xbitmap: error in resource settings:\
                cell size must be between %d and %d pixels\n", 
                MINCELLSIZE, MAXCELLSIZE);
        exit(1);
    }

    /* begin application code */

    set_up_things(topLevel);

    private_app_data.cell = XtCalloc(app_data.pixmap_width_in_cells * 
            app_data.pixmap_height_in_cells, sizeof(char));

    if (app_data.debug)
        fprintf(stderr, "xbitmap: pixmap dimensions are %d by %d\n", 
                app_data.pixmap_width_in_cells, 
                app_data.pixmap_height_in_cells);

    /* create main window */
    mainWindow = XtVaCreateManagedWidget( "mainWindow",
            xmMainWindowWidgetClass, topLevel, 
            NULL);

    /* create menu bar along top inside of main window */
    menuBar = XmCreateMenuBar( mainWindow, "menuBar",
            (ArgList) NULL, (Cardinal) 0);
    XtManageChild(menuBar);

    /* note: no header file needed to create xmPrimitive */
    private_app_data.bitmap = XtVaCreateManagedWidget("bitmap", 
            xmPrimitiveWidgetClass, mainWindow, 
            XmNtranslations, XtParseTranslationTable(trans),
            XmNwidth, private_app_data.pixmap_width_in_pixels,
            XmNheight, private_app_data.pixmap_height_in_pixels,
            NULL);

    /*  Set MainWindow areas */
    XmMainWindowSetAreas (mainWindow, menuBar, NULL, NULL, NULL,
            private_app_data.bitmap);

    /*
     *  CREATE FILE MENU AND CHILDREN
     */

    /* create button that will pop up the menu */
    fileButton = XtVaCreateManagedWidget("fileButton",
            xmCascadeButtonWidgetClass, menuBar, NULL);

    /* create menu (really a Shell widget and RowColumn widget combo) */
    fileMenu = XmCreatePulldownMenu( menuBar,
            "fileMenu", NULL, 0);

    /*
     *  CREATE BUTTON TO OUTPUT BITMAP
     */

    /* create button that will pop up the menu */
    output = XtVaCreateManagedWidget( "output",
            xmPushButtonWidgetClass, fileMenu, NULL);

    XtAddCallback(output, XmNactivateCallback, PrintOut, NULL);

    /* create the quit button up in the menu */
    quit = XtVaCreateManagedWidget( "quit",
            xmPushButtonWidgetClass, fileMenu, NULL);

    /* 
     * Specify which menu fileButton will pop up.
     */
    XtVaSetValues(fileButton,
            XmNsubMenuId, fileMenu,
            NULL);

    /* arrange for quit button to call function that exits. */
    XtAddCallback(quit, XmNactivateCallback, Quit, NULL);

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
    helpBox = XmCreateMessageDialog( help,
            "helpBox", NULL, 0);

    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild (temp);
    temp = XmMessageBoxGetChild (helpBox, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild (temp);

    /* arrange for getHelp button to pop up helpBox */
    XtAddCallback(help, XmNactivateCallback, ShowHelp, helpBox);

    XtAppAddActions(app_context, window_actions, XtNumber(window_actions));

    XtRealizeWidget(topLevel);

    XtAppMainLoop(app_context);
}

set_up_things(w)
Widget w;
{
    XGCValues values;
    int x, y;
    XSegment segment[MAXLINES];
    int n_horiz_segments, n_vert_segments;

    private_app_data.pixmap_width_in_pixels = 
            app_data.pixmap_width_in_cells * 
            app_data.cell_size_in_pixels;
    private_app_data.pixmap_height_in_pixels = 
            app_data.pixmap_height_in_cells * 
            app_data.cell_size_in_pixels;

    private_app_data.big_picture = XCreatePixmap(XtDisplay(w),
            RootWindowOfScreen(XtScreen(w)),
            private_app_data.pixmap_width_in_pixels, 
            private_app_data.pixmap_height_in_pixels, 1);

    values.foreground = 1;
    values.background = 0;
    values.dashes = 1;
    values.dash_offset = 0;
    values.line_style = LineOnOffDash;

    private_app_data.draw_gc = XCreateGC(XtDisplay(w), 
            private_app_data.big_picture, GCForeground | GCBackground 
            | GCDashOffset | GCDashList | GCLineStyle, &values);

    values.foreground = 0;
    values.background = 1;
    private_app_data.undraw_gc = XCreateGC(XtDisplay(w), 
            private_app_data.big_picture, GCForeground | GCBackground 
            | GCDashOffset | GCDashList | GCLineStyle, &values);

    values.foreground = app_data.copy_fg;
    values.background = app_data.copy_bg;
    private_app_data.copy_gc = XCreateGC(XtDisplay(w), 
            RootWindowOfScreen(XtScreen(w)),
            GCForeground | GCBackground, &values);

    XFillRectangle(XtDisplay(w), private_app_data.big_picture, 
            private_app_data.undraw_gc, 0, 0, 
            private_app_data.pixmap_width_in_pixels, 
            private_app_data.pixmap_height_in_pixels);

    /* draw permanent grid into pixmap */
    n_horiz_segments = app_data.pixmap_height_in_cells + 1;
    n_vert_segments = app_data.pixmap_width_in_cells + 1;

    for (x = 0; x < n_horiz_segments; x += 1) {
        segment[x].x1 = 0;
        segment[x].x2 = private_app_data.pixmap_width_in_pixels;
        segment[x].y1 = app_data.cell_size_in_pixels * x;
        segment[x].y2 = app_data.cell_size_in_pixels * x;
    }

    /* drawn only once into pixmap */
    XDrawSegments(XtDisplay(w), private_app_data.big_picture, 
            private_app_data.draw_gc, segment, n_horiz_segments);

    for (y = 0; y < n_vert_segments; y += 1) {
        segment[y].x1 = y * app_data.cell_size_in_pixels;
        segment[y].x2 = y * app_data.cell_size_in_pixels;
        segment[y].y1 = 0;
        segment[y].y2 = private_app_data.pixmap_height_in_pixels;
    }

    /* drawn only once into pixmap */
    XDrawSegments(XtDisplay(w), private_app_data.big_picture, 
            private_app_data.draw_gc, segment, n_vert_segments);
}

/* ARGSUSED */
static void
RedrawPicture(w, event, params, num_params)
Widget w;
XExposeEvent *event;
String *params;
Cardinal *num_params;
{
    register int x, y;
    unsigned int width, height;

    if (event) {    /* drawing because of expose or button press */
        x = event->x;
        y = event->y; 
        width = event->width;
        height =  event->height;
    } 
    else {  /* drawing because of scrolling */
        x = 0;
        y = 0; 
        width =  10000;  /* always the whole window! */
        height =  10000;
    }

    if (DefaultDepthOfScreen(XtScreen(w)) == 1)
        XCopyArea(XtDisplay(w), private_app_data.big_picture, XtWindow(w),
               private_app_data.copy_gc, x + private_app_data.cur_x, 
               y + private_app_data.cur_y, width, height, x, y);
    else
        XCopyPlane(XtDisplay(w), private_app_data.big_picture, XtWindow(w),
               private_app_data.copy_gc, x + private_app_data.cur_x, 
               y + private_app_data.cur_y, width, height, x, y, 1);
}

/* ARGSUSED */
static void
DrawCell(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
    DrawPixmaps(private_app_data.draw_gc, DRAW, w, event);
}

/* ARGSUSED */
static void
UndrawCell(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
    DrawPixmaps(private_app_data.undraw_gc, UNDRAW, w, event);
}

/* ARGSUSED */
static void
ToggleCell(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
    static int oldx = -1, oldy = -1;
    GC gc;
    int mode;
    int newx = (private_app_data.cur_x + event->x) / app_data.cell_size_in_pixels;
    int newy = (private_app_data.cur_y + event->y) / app_data.cell_size_in_pixels;

    if ((mode = private_app_data.cell[newx + newy * 
            app_data.pixmap_width_in_cells]) == DRAWN) {
        gc = private_app_data.undraw_gc;
        mode = UNDRAW;
    }
    else {
        gc = private_app_data.draw_gc;
        mode = DRAW;
    }

    if (oldx != newx || oldy != newy) {
        oldx = newx;
        oldy = newy;
        DrawPixmaps(gc, mode, w, event);
    }
}

/* Private Function */
static void
DrawPixmaps(gc, mode, w, event)
GC gc;
int mode;
Widget w;
XButtonEvent *event;
{
    int newx = (private_app_data.cur_x + event->x) / 
            app_data.cell_size_in_pixels;
    int newy = (private_app_data.cur_y + event->y) / 
            app_data.cell_size_in_pixels;
    XExposeEvent fake_event;

    /* if already done, return */
    if (private_app_data.cell[newx + newy * 
            app_data.pixmap_width_in_cells] == mode) 
        return;

    XFillRectangle(XtDisplay(w), private_app_data.big_picture, gc,
            app_data.cell_size_in_pixels*newx + 2, 
            app_data.cell_size_in_pixels*newy + 2, 
            (unsigned int) app_data.cell_size_in_pixels - 3, 
            (unsigned int) app_data.cell_size_in_pixels - 3);

    private_app_data.cell[newx + newy * 
            app_data.pixmap_width_in_cells] = mode;

    fake_event.x = app_data.cell_size_in_pixels * newx - 
            private_app_data.cur_x;
    fake_event.y = app_data.cell_size_in_pixels * newy - 
            private_app_data.cur_y;
    fake_event.width = app_data.cell_size_in_pixels;
    fake_event.height = app_data.cell_size_in_pixels;

    RedrawPicture(private_app_data.bitmap, &fake_event);
}
