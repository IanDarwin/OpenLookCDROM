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
 * BitmapEdit.c - bitmap editor widget.
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include <stdio.h>

#include "BitmapEdiP.h"

#define INTERNAL_WIDTH    2
#define INTERNAL_HEIGHT 4

#define DEFAULT_PIXMAP_WIDTH	32	/* in cells */
#define DEFAULT_PIXMAP_HEIGHT	32	/* in cells */

#define DEFAULT_CELL_SIZE	30	/* in pixels */

/* values for instance variable is_drawn */
#define DRAWN 1
#define UNDRAWN 0

/* modes for drawing */
#define DRAW 1
#define UNDRAW 0

#define MAXLINES 1000	/* max of horiz or vertical cells */
#define SCROLLBARWIDTH 15

#define DEFAULTWIDTH 300  /* widget size when showAll is False */

#define offset(field) XtOffsetOf(BitmapEditRec, field)

static XtResource resources[] = {
     {
	XtNforeground, 
	XtCForeground, 
	XtRPixel, 
	sizeof(Pixel),
	offset(bitmapEdit.foreground), 
	XtRString, 
	XtDefaultForeground
     },
     {
	XtNcallback, 
	XtCCallback, 
	XtRCallback, 
	sizeof(XtPointer),
	offset(bitmapEdit.callback), 
	XtRCallback, 
	NULL
     },
     {
	XtNcellSizeInPixels, 
	XtCCellSizeInPixels, 
	XtRInt, sizeof(int),
	offset(bitmapEdit.cell_size_in_pixels), 
	XtRImmediate, 
	(XtPointer)DEFAULT_CELL_SIZE
     },
     {
	XtNpixmapWidthInCells, 
	XtCPixmapWidthInCells, 
	XtRDimension, 
	sizeof(Dimension),
	offset(bitmapEdit.pixmap_width_in_cells), 
	XtRImmediate, 
	(XtPointer)DEFAULT_PIXMAP_WIDTH
     },
     {
	XtNpixmapHeightInCells, 
	XtCPixmapHeightInCells, 
	XtRDimension, 
	sizeof(Dimension),
	offset(bitmapEdit.pixmap_height_in_cells), 
	XtRImmediate, 
	(XtPointer)DEFAULT_PIXMAP_HEIGHT
     },
     {
	XtNcurX, 
	XtCCurX, 
	XtRInt, 
	sizeof(int),
	offset(bitmapEdit.cur_x), 
	XtRImmediate, 
	(XtPointer) 0
     },
     {
	XtNcurY, 
	XtCCurY, 
	XtRInt, 
	sizeof(int),
	offset(bitmapEdit.cur_y), 
	XtRImmediate, 
	(XtPointer) 0
     },
     {
	XtNcellArray, 
	XtCCellArray, 
	XtRString, 
	sizeof(String),
	offset(bitmapEdit.cell), 
	XtRImmediate, 
	(XtPointer) 0
     },
     {
	XtNshowEntireBitmap, 
	XtCShowEntireBitmap, 
	XtRBoolean, 
	sizeof(Boolean),
	offset(bitmapEdit.showAll), 
	XtRImmediate, 
	(XtPointer) TRUE
     },
};

/* Declaration of methods */

static void Initialize();
static void Redisplay();
static void Destroy();
static void Resize();
static Boolean SetValues();
static XtGeometryResult QueryGeometry();

/* these Core methods not needed by BitmapEdit:
 *
 * static void ClassInitialize();
 * static void Realize();
 */

/* the following are private functions unique to BitmapEdit */
static void DrawPixmaps(), DoCell(), ChangeCellSize();

/* the following are actions of BitmapEdit */
static void DrawCell(), UndrawCell(), ToggleCell();

/* The following are public functions of BitmapEdit, declared extern
 * in the public include file: */
char *BitmapEditGetArrayString(); 

static char defaultTranslations[] =
	"<Btn1Down>:    DrawCell()              \n\
	<Btn2Down>:    UndrawCell()            \n\
	<Btn3Down>:    ToggleCell()            \n\
	<Btn1Motion>:  DrawCell()              \n\
	<Btn2Motion>:  UndrawCell()            \n\
	<Btn3Motion>:  ToggleCell()";

static XtActionsRec actions[] = {
        {"DrawCell", DrawCell},
        {"UndrawCell", UndrawCell},
        {"ToggleCell", ToggleCell},
};

/* definition in BitmapEdit.h */
static BitmapEditPointInfo info;

BitmapEditClassRec bitmapEditClassRec = {
    {
    /* core_class fields */
    /* superclass	  	 */ (WidgetClass) &coreClassRec,
    /* class_name	  	 */ "BitmapEdit",
    /* widget_size	  	 */ sizeof(BitmapEditRec),
    /* class_initialize   	 */ NULL,
    /* class_part_initialize	 */ NULL,
    /* class_inited       	 */ FALSE,
    /* initialize	  	 */ Initialize,
    /* initialize_hook		 */ NULL,
    /* realize		  	 */ XtInheritRealize,
    /* actions		  	 */ actions,
    /* num_actions	  	 */ XtNumber(actions),
    /* resources	  	 */ resources,
    /* num_resources	  	 */ XtNumber(resources),
    /* xrm_class	  	 */ NULLQUARK,
    /* compress_motion	  	 */ TRUE,
    /* compress_exposure  	 */ XtExposeCompressMultiple,
    /* compress_enterleave	 */ TRUE,
    /* visible_interest	  	 */ FALSE,
    /* destroy		  	 */ Destroy,
    /* resize		  	 */ Resize,
    /* expose		  	 */ Redisplay,
    /* set_values	  	 */ SetValues,
    /* set_values_hook		 */ NULL,
    /* set_values_almost	 */ XtInheritSetValuesAlmost,
    /* get_values_hook		 */ NULL,
    /* accept_focus	 	 */ NULL,
    /* version			 */ XtVersion,
    /* callback_private   	 */ NULL,
    /* tm_table		   	 */ defaultTranslations,
    /* query_geometry		 */ QueryGeometry,
    /* display_accelerator       */ XtInheritDisplayAccelerator,
    /* extension                 */ NULL
    },
    {
    /* dummy_field               */ 0,
    },
};

WidgetClass bitmapEditWidgetClass = (WidgetClass) & bitmapEditClassRec;

static void
GetDrawGC(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    XGCValues values;
    XtGCMask mask = GCForeground | GCBackground | GCDashOffset | GCDashList | GCLineStyle;

    /* 
     * Setting foreground and background to 1 and 0 looks like a 
     * kludge but isn't.  This GC is used for drawing
     * into a pixmap of depth one.  Real colors are applied with a
     * separate GC when the pixmap is copied into the window.
     */
    values.foreground = 1;
    values.background = 0;
    values.dashes = 1;
    values.dash_offset = 0;
    values.line_style = LineOnOffDash;

    cw->bitmapEdit.draw_gc = XCreateGC(XtDisplay(cw), cw->bitmapEdit.big_picture, mask, &values);
}

static void
GetUndrawGC(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    XGCValues values;
    XtGCMask mask = GCForeground | GCBackground;

    /* this looks like a kludge but isn't.  This GC is used for drawing
     * into a pixmap of depth one.  Real colors are applied as the 
     * pixmap is copied into the window.
     */
    values.foreground = 0;
    values.background = 1;

    cw->bitmapEdit.undraw_gc = XCreateGC(XtDisplay(cw), cw->bitmapEdit.big_picture, mask, &values);
}

static void
GetCopyGC(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    XGCValues values;
    XtGCMask mask = GCForeground | GCBackground;

    values.foreground = cw->bitmapEdit.foreground;
    values.background = cw->core.background_pixel;

    cw->bitmapEdit.copy_gc = XtGetGC(cw, mask, &values);
}

/* ARGSUSED */
static void
Initialize(treq, tnew, args, num_args)
Widget treq, tnew;
ArgList args;
Cardinal *num_args;
{
	BitmapEditWidget new = (BitmapEditWidget) tnew;
    new->bitmapEdit.cur_x = 0;
    new->bitmapEdit.cur_y = 0;

    /* 
     *  Check instance values set by resources that may be invalid. 
     */

    if ((new->bitmapEdit.pixmap_width_in_cells < 1) ||
		(new->bitmapEdit.pixmap_height_in_cells < 1))  {
	XtWarning("BitmapEdit: pixmapWidth and/or pixmapHeight is too small (using 10 x 10)."); 
    	new->bitmapEdit.pixmap_width_in_cells = 10;
    	new->bitmapEdit.pixmap_height_in_cells = 10;
    }

    if (new->bitmapEdit.cell_size_in_pixels < 5) {
	XtWarning("BitmapEdit: cellSize is too small (using 5)."); 
    	new->bitmapEdit.cell_size_in_pixels = 5;
    }

    if ((new->bitmapEdit.cur_x < 0) ||  (new->bitmapEdit.cur_y < 0)) {
	XtWarning("BitmapEdit: cur_x and cur_y must be non-negative (using 0, 0)."); 
    	new->bitmapEdit.cur_x = 0;
    	new->bitmapEdit.cur_y = 0;
    }

    if (new->bitmapEdit.cell == NULL)
        new->bitmapEdit.cell = XtCalloc(new->bitmapEdit.pixmap_width_in_cells * new->bitmapEdit.pixmap_height_in_cells, sizeof(char));

    new->bitmapEdit.pixmap_width_in_pixels = new->bitmapEdit.pixmap_width_in_cells * new->bitmapEdit.cell_size_in_pixels;

    new->bitmapEdit.pixmap_height_in_pixels = new->bitmapEdit.pixmap_height_in_cells * new->bitmapEdit.cell_size_in_pixels;

    if (new->core.width == 0) {
		if (new->bitmapEdit.showAll == False)
			new->core.width = (new->bitmapEdit.pixmap_width_in_pixels > DEFAULTWIDTH) ? DEFAULTWIDTH : (new->bitmapEdit.pixmap_width_in_pixels);
		else
			new->core.width = new->bitmapEdit.pixmap_width_in_pixels;
	}

    if (new->core.height == 0) {
		if (new->bitmapEdit.showAll == False)
			new->core.height = (new->bitmapEdit.pixmap_height_in_pixels > DEFAULTWIDTH) ? DEFAULTWIDTH : (new->bitmapEdit.pixmap_height_in_pixels);
		else
			new->core.height = new->bitmapEdit.pixmap_height_in_pixels;
	}

    CreateBigPixmap(new);

    GetDrawGC(new);
    GetUndrawGC(new);
    GetCopyGC(new);

    DrawIntoBigPixmap(new);
}

/* ARGSUSED */
static void
Redisplay(w, event)
Widget w;
XExposeEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    register int x, y;
    unsigned int width, height;
    if (!XtIsRealized(cw))
	return;

    if (event) {  /* called from btn-event or expose */
		x = event->x;
		y = event->y; 
		width = event->width;
		height =  event->height;
    } 
    else {        /* called because complete redraw */
		x = 0;
		y = 0; 
		width = cw->bitmapEdit.pixmap_width_in_pixels;
		height = cw->bitmapEdit.pixmap_height_in_pixels;
    }

    if (DefaultDepthOfScreen(XtScreen(cw)) == 1)
		XCopyArea(XtDisplay(cw), cw->bitmapEdit.big_picture, XtWindow(cw),
	    	cw->bitmapEdit.copy_gc, x + cw->bitmapEdit.cur_x, y + 
	    	cw->bitmapEdit.cur_y, width, height, x, y);
    else
		XCopyPlane(XtDisplay(cw), cw->bitmapEdit.big_picture, XtWindow(cw),
	    	cw->bitmapEdit.copy_gc, x + cw->bitmapEdit.cur_x, y + 
	    	cw->bitmapEdit.cur_y, width, height, x, y, 1);

}

/* ARGSUSED */
static Boolean
SetValues(current, request, new, args, num_args)
Widget current, request, new;
ArgList args;
Cardinal *num_args;
{
    BitmapEditWidget curcw = (BitmapEditWidget) current;
    BitmapEditWidget newcw = (BitmapEditWidget) new;
    Boolean do_redisplay = False;

    if (curcw->bitmapEdit.foreground != newcw->bitmapEdit.foreground) {
		XtReleaseGC(curcw, curcw->bitmapEdit.copy_gc);
		GetCopyGC(newcw);
		do_redisplay = True;
    }

    if ((curcw->bitmapEdit.cur_x != newcw->bitmapEdit.cur_x) || 
			(curcw->bitmapEdit.cur_y != newcw->bitmapEdit.cur_y))
		do_redisplay = True;

    if (curcw->bitmapEdit.cell_size_in_pixels != newcw->bitmapEdit.cell_size_in_pixels) {
		ChangeCellSize(curcw, newcw->bitmapEdit.cell_size_in_pixels);
		do_redisplay = True;
    }

    if (curcw->bitmapEdit.pixmap_width_in_cells != 
			newcw->bitmapEdit.pixmap_width_in_cells)  {
        newcw->bitmapEdit.pixmap_width_in_cells = curcw->bitmapEdit.pixmap_width_in_cells;
		XtWarning("BitmapEdit: pixmap_width_in_cells cannot be set by XtSetValues.\n");
    }

    if (curcw->bitmapEdit.pixmap_height_in_cells != 
		newcw->bitmapEdit.pixmap_height_in_cells) {
        newcw->bitmapEdit.pixmap_height_in_cells = curcw->bitmapEdit.pixmap_height_in_cells;
	XtWarning("BitmapEdit: pixmap_height_in_cells cannot be set by XtSetValues.\n");
    }

    return do_redisplay;
}


static void
Destroy(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    if (cw->bitmapEdit.big_picture)
	XFreePixmap(XtDisplay(cw), cw->bitmapEdit.big_picture);

    if (cw->bitmapEdit.draw_gc)
	XFreeGC(XtDisplay(cw), cw->bitmapEdit.draw_gc);

    if (cw->bitmapEdit.undraw_gc)
	XFreeGC(XtDisplay(cw), cw->bitmapEdit.undraw_gc);

    if (cw->bitmapEdit.copy_gc)
	XFreeGC(XtDisplay(cw), cw->bitmapEdit.copy_gc);

    /* NOTE!  This should only free when the application didn't 
     * allocate it.  Need to add another.  */
    XtFree(cw->bitmapEdit.cell);
}

static void
DrawCell(w, event)
Widget w;
XEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    DrawPixmaps(cw->bitmapEdit.draw_gc, DRAW, cw, event);
}

static void
UndrawCell(w, event)
Widget w;
XEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    DrawPixmaps(cw->bitmapEdit.undraw_gc, UNDRAW, cw, event);
}

static void
ToggleCell(w, event)
Widget w;
XEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    static int oldx = -1, oldy = -1;
    GC gc;
    int mode;
    int newx, newy;

    /* This is strictly correct, but doesn't
     * seem to be necessary */
    if (event->type == ButtonPress) {
        newx = (cw->bitmapEdit.cur_x + ((XButtonEvent *)event)->x) / 
		cw->bitmapEdit.cell_size_in_pixels;
        newy = (cw->bitmapEdit.cur_y + ((XButtonEvent *)event)->y) / 
		cw->bitmapEdit.cell_size_in_pixels;
    }
    else  {
        newx = (cw->bitmapEdit.cur_x + ((XMotionEvent *)event)->x) / 
		cw->bitmapEdit.cell_size_in_pixels;
        newy = (cw->bitmapEdit.cur_y + ((XMotionEvent *)event)->y) / 
		cw->bitmapEdit.cell_size_in_pixels;
    }


    if ((mode = cw->bitmapEdit.cell[newx + newy * cw->bitmapEdit.pixmap_width_in_cells]) == DRAWN) {
        gc = cw->bitmapEdit.undraw_gc;
	mode = UNDRAW;
    }
    else {
        gc = cw->bitmapEdit.draw_gc;
	mode = DRAW;
    }

    if (oldx != newx || oldy != newy) {
        oldx = newx;
        oldy = newy;
        DrawPixmaps(gc, mode, cw, event);
    } 
}

static void
DrawPixmaps(gc, mode, w, event)
GC gc;
int mode;
Widget w;
XButtonEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    int newx = (cw->bitmapEdit.cur_x + event->x) / cw->bitmapEdit.cell_size_in_pixels;
    int newy = (cw->bitmapEdit.cur_y + event->y) / cw->bitmapEdit.cell_size_in_pixels;
    XExposeEvent fake_event;

	/* if already done, return */
    if (cw->bitmapEdit.cell[newx + newy * cw->bitmapEdit.pixmap_width_in_cells] == mode)
        return;

	/* otherwise, draw or undraw */
    XFillRectangle(XtDisplay(cw), cw->bitmapEdit.big_picture, gc,
    		cw->bitmapEdit.cell_size_in_pixels*newx + 2, 
		cw->bitmapEdit.cell_size_in_pixels*newy + 2, 
		(unsigned int)cw->bitmapEdit.cell_size_in_pixels - 3, 
		(unsigned int)cw->bitmapEdit.cell_size_in_pixels - 3);

    cw->bitmapEdit.cell[newx + newy * cw->bitmapEdit.pixmap_width_in_cells] = mode;
    info.mode = mode;
    info.newx = newx;
    info.newy = newy;

    fake_event.x = cw->bitmapEdit.cell_size_in_pixels * newx - cw->bitmapEdit.cur_x;
    fake_event.y = cw->bitmapEdit.cell_size_in_pixels * newy - cw->bitmapEdit.cur_y;
    fake_event.width = cw->bitmapEdit.cell_size_in_pixels;
    fake_event.height = cw->bitmapEdit.cell_size_in_pixels;

    Redisplay(cw, &fake_event);
    XtCallCallbacks(cw, XtNcallback, &info);
}

CreateBigPixmap(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    /* always a 1 bit deep pixmap, regardless of screen depth */
    cw->bitmapEdit.big_picture = XCreatePixmap(XtDisplay(cw),
            RootWindow(XtDisplay(cw), DefaultScreen(XtDisplay(cw))),
            cw->bitmapEdit.pixmap_width_in_pixels + 2, cw->bitmapEdit.pixmap_height_in_pixels + 2, 1);
}

DrawIntoBigPixmap(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	int n_horiz_segments, n_vert_segments;
	XSegment segment[MAXLINES];
	register int x, y;

	XFillRectangle(XtDisplay(cw), cw->bitmapEdit.big_picture,
	cw->bitmapEdit.undraw_gc, 0, 0, cw->bitmapEdit.pixmap_width_in_pixels 
	+ 2, cw->bitmapEdit.pixmap_height_in_pixels + 2);

	n_horiz_segments = cw->bitmapEdit.pixmap_height_in_cells + 1;
	n_vert_segments = cw->bitmapEdit.pixmap_width_in_cells + 1;

	for (x = 0; x < n_horiz_segments; x++) {
		segment[x].x1 = 0;
		segment[x].x2 = cw->bitmapEdit.pixmap_width_in_pixels;
		segment[x].y1 = cw->bitmapEdit.cell_size_in_pixels * x;
		segment[x].y2 = cw->bitmapEdit.cell_size_in_pixels * x;
	}

	XDrawSegments(XtDisplay(cw), cw->bitmapEdit.big_picture, cw->bitmapEdit.draw_gc, segment, n_horiz_segments);

	for (y = 0; y < n_vert_segments; y++) {
		segment[y].x1 = y * cw->bitmapEdit.cell_size_in_pixels;
		segment[y].x2 = y * cw->bitmapEdit.cell_size_in_pixels;
		segment[y].y1 = 0;
		segment[y].y2 = cw->bitmapEdit.pixmap_height_in_pixels;
	}

	XDrawSegments(XtDisplay(cw), cw->bitmapEdit.big_picture, cw->bitmapEdit.draw_gc, segment, n_vert_segments);

    /* draw current cell array into pixmap */
    for (x = 0; x < cw->bitmapEdit.pixmap_width_in_cells; x++) {
        for (y = 0; y < cw->bitmapEdit.pixmap_height_in_cells; y++) {
            if (cw->bitmapEdit.cell[x + (y * cw->bitmapEdit.pixmap_width_in_cells)] == DRAWN)
                DoCell(cw, x, y, cw->bitmapEdit.draw_gc);
            else
                DoCell(cw, x, y, cw->bitmapEdit.undraw_gc);
        }
    }
}

/* A Public function, not static */
char *
BitmapEditGetArrayString(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    return (cw->bitmapEdit.cell);
}

/* ARGSUSED */
static void
Resize(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    /* resize does nothing unless new size is bigger than entire pixmap */
    if ((cw->core.width > cw->bitmapEdit.pixmap_width_in_pixels) &&
            (cw->core.height > cw->bitmapEdit.pixmap_height_in_pixels)) {
        /* 
         * Calculate the maximum cell size that will allow the
         * entire bitmap to be displayed.
         */
        Dimension w_temp_cell_size_in_pixels, h_temp_cell_size_in_pixels;
        Dimension new_cell_size_in_pixels;
    
        w_temp_cell_size_in_pixels = cw->core.width / cw->bitmapEdit.pixmap_width_in_cells;
        h_temp_cell_size_in_pixels = cw->core.height / cw->bitmapEdit.pixmap_height_in_cells;
    
        if (w_temp_cell_size_in_pixels < h_temp_cell_size_in_pixels)
            new_cell_size_in_pixels = w_temp_cell_size_in_pixels;
        else
            new_cell_size_in_pixels = h_temp_cell_size_in_pixels;
    
        /* if size change mandates a new pixmap, make one */
        if (new_cell_size_in_pixels != cw->bitmapEdit.cell_size_in_pixels)
            ChangeCellSize(cw, new_cell_size_in_pixels);
    }
}

static void
ChangeCellSize(w, new_cell_size)
Widget w;
int new_cell_size;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    int x, y;

    cw->bitmapEdit.cell_size_in_pixels = new_cell_size;

    /* recalculate variables based on cell size */
    cw->bitmapEdit.pixmap_width_in_pixels = cw->bitmapEdit.pixmap_width_in_cells * cw->bitmapEdit.cell_size_in_pixels;

    cw->bitmapEdit.pixmap_height_in_pixels = cw->bitmapEdit.pixmap_height_in_cells * cw->bitmapEdit.cell_size_in_pixels;
    
    /* destroy old and create new pixmap of correct size */
    XFreePixmap(XtDisplay(cw), cw->bitmapEdit.big_picture);
    CreateBigPixmap(cw);
    
    /* draw lines into new pixmap */
    DrawIntoBigPixmap(cw);
    
    /* draw current cell array into pixmap */
    for (x = 0; x < cw->bitmapEdit.pixmap_width_in_cells; x++) {
        for (y = 0; y < cw->bitmapEdit.pixmap_height_in_cells; y++) {
            if (cw->bitmapEdit.cell[x + (y * cw->bitmapEdit.pixmap_width_in_cells)] == DRAWN)
                DoCell(cw, x, y, cw->bitmapEdit.draw_gc);
            else
                DoCell(cw, x, y, cw->bitmapEdit.undraw_gc);
        }
    }
}

static void
DoCell(w, x, y, gc)
Widget w;
int x, y;
GC gc;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
        /* otherwise, draw or undraw */
    XFillRectangle(XtDisplay(cw), cw->bitmapEdit.big_picture, gc,
                cw->bitmapEdit.cell_size_in_pixels * x + 2,
                cw->bitmapEdit.cell_size_in_pixels * y + 2,
                (unsigned int)cw->bitmapEdit.cell_size_in_pixels - 3,
                (unsigned int)cw->bitmapEdit.cell_size_in_pixels - 3);

}

static XtGeometryResult QueryGeometry(w, proposed, answer)
Widget w;
XtWidgetGeometry *proposed, *answer;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
    answer->request_mode = CWWidth | CWHeight;

    /* initial width and height */
	if (cw->bitmapEdit.showAll == True)
		answer->width = cw->bitmapEdit.pixmap_width_in_pixels;
	else
    	answer->width = (cw->bitmapEdit.pixmap_width_in_pixels > 
				DEFAULTWIDTH) ? DEFAULTWIDTH : 
				cw->bitmapEdit.pixmap_width_in_pixels;

	if (cw->bitmapEdit.showAll == True)
		answer->height = cw->bitmapEdit.pixmap_height_in_pixels;
	else
    	answer->height = (cw->bitmapEdit.pixmap_height_in_pixels > 
				DEFAULTWIDTH) ? DEFAULTWIDTH : 
				cw->bitmapEdit.pixmap_height_in_pixels;

    if (  ((proposed->request_mode & (CWWidth | CWHeight))
                == (CWWidth | CWHeight)) &&
          proposed->width == answer->width &&
          proposed->height == answer->height)
        return XtGeometryYes;
    else if (answer->width == cw->core.width &&
             answer->height == cw->core.height)
        return XtGeometryNo;
    else
        return XtGeometryAlmost;
}

