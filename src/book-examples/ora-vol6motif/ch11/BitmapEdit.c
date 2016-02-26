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
 * BitmapEdit.c - bitmap editor widget that implements selections
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include <stdio.h>

#include <X11/Xatom.h>
#include <X11/Xmu/Atoms.h>

#include "BitmapEdiP.h"

#define INTERNAL_WIDTH	2
#define INTERNAL_HEIGHT 4

#define DEFAULT_PIXMAP_WIDTH	32	/* in cells */
#define DEFAULT_PIXMAP_HEIGHT	32	/* in cells */

#define DEFAULT_CELL_SIZE	30	/* in pixels */

/* values for instance variable is_drawn, and for char elements
 * in cell state array. */
#define DRAWN 'y'
#define UNDRAWN 'n'

/* modes for drawing */
#define DRAW 'y'
#define UNDRAW 'n'

#define MAXLINES 1000	/* max of horiz or vertical cells */
#define SCROLLBARWIDTH 15


#define offset(field) XtOffset(BitmapEditWidget, field)

static XtResource resources[] = {
	{XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
		offset(bitmapEdit.foreground), XtRString, XtDefaultForeground },
	{XtNselectionForeground, XtCSelectionForeground, XtRPixel, 
		sizeof(Pixel), offset(bitmapEdit.selectionForeground), 
		XtRString, "red" },
	{XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
		offset(bitmapEdit.callback), XtRCallback, NULL},
	{XtNcellSizeInPixels, XtCCellSizeInPixels, XtRInt, sizeof(int),
		offset(bitmapEdit.cell_size_in_pixels), XtRImmediate, 
		(XtPointer)DEFAULT_CELL_SIZE },
	{XtNpixmapWidthInCells, XtCPixmapWidthInCells, XtRDimension, 
		sizeof(Dimension), offset(bitmapEdit.pixmap_width_in_cells), 
		XtRImmediate, (XtPointer)DEFAULT_PIXMAP_WIDTH },
	{XtNpixmapHeightInCells, XtCPixmapHeightInCells, XtRDimension, 
		sizeof(Dimension), offset(bitmapEdit.pixmap_height_in_cells), 
		XtRImmediate, (XtPointer)DEFAULT_PIXMAP_HEIGHT },
	{XtNcurX, XtCCurX, XtRInt, sizeof(int),
		offset(bitmapEdit.cur_x), XtRImmediate, (XtPointer) 0},
	{XtNcurY, XtCCurY, XtRInt, sizeof(int),
		offset(bitmapEdit.cur_y), XtRImmediate, (XtPointer) 0},
};

/* Declaration of methods */

static void Initialize();
static void Redisplay();
static void Destroy();
static void Resize();
static Boolean SetValues();

/* these Core methods not needed by BitmapEdit:
 *
 * static void ClassInitialize();
 * static void Realize();
 * static void ClassInitialize();
 * static void Realize();
 */

/* the following are private functions unique to BitmapEdit */
static void DrawPixmaps(), HighlightCell(), draw_box(), DrawCell();

/* private functions for selections */
static void lose_ownership_proc(), transfer_done_proc(), 
		requestor_callback();
static Boolean convert_proc();

/* the following are actions of BitmapEdit */
static void DoCell(), UndoCell(), ToggleCell();
static void TopLeft(), BottomRight(), DragHighlight();
static void PasteSelection();

/* The following are public functions of BitmapEdit, declared extern
 * in the public include file: */
char *BitmapEditGetArray(); 

static char defaultTranslations[] =
	"Shift<Btn1Down>:	TopLeft()		  \n\
	 Shift<Btn1Motion>:  DragHighlight()	\n\
	 Shift<Btn1Up>:	  BottomRight()	  \n\
	 Shift<Btn2Down>:	PasteSelection()   \n\
	 ~Shift<Btn1Down>:	DoCell()		  \n\
	 ~Shift<Btn2Down>:	UndoCell()		\n\
	 ~Shift<Btn3Down>:	ToggleCell()	  \n\
	 ~Shift<Btn1Motion>:  DoCell()		  \n\
	 ~Shift<Btn2Motion>:  UndoCell()		\n\
	 ~Shift<Btn3Motion>:  ToggleCell()";

static XtActionsRec actions[] = {
	{"DoCell", DoCell},
	{"UndoCell", UndoCell},
	{"ToggleCell", ToggleCell},
	{"TopLeft", TopLeft},
	{"DragHighlight", DragHighlight},
	{"BottomRight", BottomRight},
	{"PasteSelection", PasteSelection},
};

/* definition in BitmapEdit.h */
static BitmapEditPointInfo info;

BitmapEditClassRec bitmapEditClassRec = {
	{
	/* core_class fields */
	/* superclass	  	 */ (WidgetClass) &widgetClassRec,
	/* class_name	  	 */ "BitmapEdit",
	/* widget_size	  	 */ sizeof(BitmapEditRec),
	/* class_initialize   	 */ NULL,
	/* class_part_initialize	 */ NULL,
	/* class_inited	   	 */ FALSE,
	/* initialize	  	 */ Initialize,
	/* initialize_hook		 */ NULL,
	/* realize		  	 */ XtInheritRealize,
	/* actions		  	 */ actions,
	/* num_actions	  	 */ XtNumber(actions),
	/* resources	  	 */ resources,
	/* num_resources	  	 */ XtNumber(resources),
	/* xrm_class	  	 */ NULLQUARK,
	/* compress_motion	  	 */ TRUE,
	/* compress_exposure  	 */ TRUE,
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
	/* query_geometry		 */ NULL,
	},
	{
	/* dummy_field			   */ 0,
	},
};

WidgetClass bitmapEditWidgetClass = (WidgetClass) & bitmapEditClassRec;

static void
GetDrawGC(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	XGCValues values;
	XtGCMask mask = GCForeground | GCBackground | GCDashOffset 
			| GCDashList | GCLineStyle;

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

	cw->bitmapEdit.draw_gc = XCreateGC(XtDisplay(cw), 
			cw->bitmapEdit.big_picture, mask, &values);
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

	cw->bitmapEdit.undraw_gc = XCreateGC(XtDisplay(cw), 
			cw->bitmapEdit.big_picture, mask, &values);
}

static void
GetDeepUndrawGC(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	XGCValues values;
	XtGCMask mask = GCForeground | GCBackground;

	values.foreground = cw->core.background_pixel;
	values.background = cw->bitmapEdit.foreground;

	cw->bitmapEdit.deep_undraw_gc = XtGetGC(cw, mask, &values);
}

static void
GetDeepDrawGC(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	XGCValues values;
	XtGCMask mask = GCForeground | GCBackground;

	values.foreground = cw->bitmapEdit.foreground;
	values.background = cw->core.background_pixel;

	cw->bitmapEdit.deep_draw_gc = XtGetGC(cw, mask, &values);
}

/* ARGSUSED */
static void
Initialize(treq, tnew, args, num_args)
Widget treq, tnew;
ArgList args;
Cardinal *num_args;
{
	BitmapEditWidget request = (BitmapEditWidget) treq;
	BitmapEditWidget new = (BitmapEditWidget) tnew;
	new->bitmapEdit.cur_x = 0;
	new->bitmapEdit.cur_y = 0;

	/* 
	 *  Check instance values set by resources that may be invalid. 
	 */

	if ((new->bitmapEdit.pixmap_width_in_cells < 1) ||
		(new->bitmapEdit.pixmap_height_in_cells < 1))  {
	XtWarning("BitmapEdit: pixmapWidth and/or pixmapHeight is too small\
			(using 10 x 10)."); 
		new->bitmapEdit.pixmap_width_in_cells = 10;
		new->bitmapEdit.pixmap_height_in_cells = 10;
	}

	if (new->bitmapEdit.cell_size_in_pixels < 5) {
	XtWarning("BitmapEdit: cellSize is too small (using 5)."); 
		new->bitmapEdit.cell_size_in_pixels = 5;
	}

	if ((new->bitmapEdit.cur_x < 0) ||  (new->bitmapEdit.cur_y < 0)) {
	XtWarning("BitmapEdit: cur_x and cur_y must be non-negative\
			(using 0, 0)."); 
		new->bitmapEdit.cur_x = 0;
		new->bitmapEdit.cur_y = 0;
	}

	new->bitmapEdit.cell = XtCalloc(new->bitmapEdit.pixmap_width_in_cells 
			* new->bitmapEdit.pixmap_height_in_cells, sizeof(char));

	new->bitmapEdit.pixmap_width_in_pixels = new->bitmapEdit.pixmap_width_in_cells 
			* new->bitmapEdit.cell_size_in_pixels;

	new->bitmapEdit.pixmap_height_in_pixels = new->bitmapEdit.pixmap_height_in_cells 
			* new->bitmapEdit.cell_size_in_pixels;

	if (new->core.width == 0)
	new->core.width = (new->bitmapEdit.pixmap_width_in_pixels > 300) 
			? 300 : (new->bitmapEdit.pixmap_width_in_pixels);
	if (new->core.height == 0)
	new->core.height = (new->bitmapEdit.pixmap_height_in_pixels > 300) 
			? 300 : (new->bitmapEdit.pixmap_height_in_pixels);

	CreateBigPixmap(new);

	GetDrawGC(new);
	GetUndrawGC(new);
	GetDeepDrawGC(new);
	GetDeepUndrawGC(new);

	DrawIntoBigPixmap(new);

	new->bitmapEdit.target_atom = XmInternAtom(XtDisplay(new), 
			"CELL_ARRAY", False);

	/* This makes Xmu initialize the necessary atoms.
	 * Wierd, but true.
	(void) XmuInternAtom( XtDisplay(new), XmuMakeAtom("NULL") );
	 */

	new->bitmapEdit.data = XtCalloc(
		new->bitmapEdit.pixmap_width_in_cells * 
		new->bitmapEdit.pixmap_height_in_cells, sizeof(char));
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

	if (event) {  /* called from btn-event */
		x = event->x;
		y = event->y; 
		width = event->width;
		height =  event->height;
	} 
	else {		/* called because of expose */
		x = 0;
		y = 0; 
		width = cw->bitmapEdit.pixmap_width_in_pixels;
		height = cw->bitmapEdit.pixmap_height_in_pixels;
	}

	if (DefaultDepthOfScreen(XtScreen(cw)) == 1)
		XCopyArea(XtDisplay(cw), cw->bitmapEdit.big_picture, XtWindow(cw),
				cw->bitmapEdit.deep_draw_gc, x + cw->bitmapEdit.cur_x, y + 
				cw->bitmapEdit.cur_y, width, height, x, y);
	else
		XCopyPlane(XtDisplay(cw), cw->bitmapEdit.big_picture, XtWindow(cw),
				cw->bitmapEdit.deep_draw_gc, x + cw->bitmapEdit.cur_x, y + 
				cw->bitmapEdit.cur_y, width, height, x, y, 1);

	if (cw->bitmapEdit.first_box)
		draw_box(cw, cw->bitmapEdit.select_end_x, cw->bitmapEdit.select_end_y, True);
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
		XtReleaseGC(curcw, curcw->bitmapEdit.deep_draw_gc);
		XtReleaseGC(curcw, curcw->bitmapEdit.deep_undraw_gc);
		GetDeepDrawGC(newcw);
		GetDeepUndrawGC(newcw);
		do_redisplay = True;
	}

	if ((curcw->bitmapEdit.cur_x != newcw->bitmapEdit.cur_x) || 
			(curcw->bitmapEdit.cur_y != newcw->bitmapEdit.cur_y)) {
		do_redisplay = True;
	}

	if ((curcw->bitmapEdit.pixmap_width_in_cells != 
			newcw->bitmapEdit.pixmap_width_in_cells) || 
			(curcw->bitmapEdit.pixmap_height_in_cells != 
			newcw->bitmapEdit.pixmap_height_in_cells) || 
			(curcw->bitmapEdit.cell_size_in_pixels != 
			newcw->bitmapEdit.cell_size_in_pixels))
		XtWarning("BitmapEdit widget: pixmap_width_in_cells,\
				 pixmap_heigt, and cell_size_in_pixels resources\
				 can only be set before widget is created\n");

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

	XtFree(cw->bitmapEdit.cell);
}

static void
DoCell(w, event)
Widget w;
XEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	DrawPixmaps(cw->bitmapEdit.draw_gc, DRAW, cw, event);
}

static void
UndoCell(w, event)
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
	int newx, newy;

	/* This is strictly correct, but doesn't
	 * seem to be necessary */
	if (event->type == ButtonPress) {
		newx = (cw->bitmapEdit.cur_x + ((XButtonEvent *)event)->x) 
				/ cw->bitmapEdit.cell_size_in_pixels;
		newy = (cw->bitmapEdit.cur_y + ((XButtonEvent *)event)->y) 
				/ cw->bitmapEdit.cell_size_in_pixels;
	}
	else  {
		newx = (cw->bitmapEdit.cur_x + ((XMotionEvent *)event)->x) 
				/ cw->bitmapEdit.cell_size_in_pixels;
		newy = (cw->bitmapEdit.cur_y + ((XMotionEvent *)event)->y) 
				/ cw->bitmapEdit.cell_size_in_pixels;

		if (oldx == newx && oldy == newy)
			return;
	}

	if (cw->bitmapEdit.cell[newx + newy * cw->bitmapEdit.pixmap_width_in_cells] == DRAWN)
		DrawPixmaps(cw->bitmapEdit.undraw_gc, UNDRAW, cw, event);
	else
		DrawPixmaps(cw->bitmapEdit.draw_gc, DRAW, cw, event);

	if (event->type == MotionNotify) {
	oldx = newx;
	oldy = newy;
	}
}

static void
DrawPixmaps(gc, mode, w, event)
GC gc;
char mode;
Widget w;
XButtonEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	int newx = (cw->bitmapEdit.cur_x + event->x) 
			/ cw->bitmapEdit.cell_size_in_pixels;
	int newy = (cw->bitmapEdit.cur_y + event->y) 
			/ cw->bitmapEdit.cell_size_in_pixels;
	XExposeEvent fake_event;

	/* if already done, return */
	if (cw->bitmapEdit.cell[newx + newy * 
			cw->bitmapEdit.pixmap_width_in_cells] == mode)
		return;

	DrawCell(cw, newx, newy, gc); 

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

static void
DrawCell(w, x, y, gc)
Widget w;
int x, y;
GC gc;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;

	/* draw or undraw */
	XFillRectangle(XtDisplay(cw), cw->bitmapEdit.big_picture, gc,
			cw->bitmapEdit.cell_size_in_pixels * x + 2, 
			cw->bitmapEdit.cell_size_in_pixels * y + 2, 
			(unsigned int)cw->bitmapEdit.cell_size_in_pixels - 3, 
			(unsigned int)cw->bitmapEdit.cell_size_in_pixels - 3);
}

CreateBigPixmap(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	/* always a 1 bit deep pixmap, regardless of screen depth */
	cw->bitmapEdit.big_picture = XCreatePixmap(XtDisplay(cw),
			RootWindow(XtDisplay(cw), DefaultScreen(XtDisplay(cw))),
			cw->bitmapEdit.pixmap_width_in_pixels, cw->bitmapEdit.pixmap_height_in_pixels, 1);
}

DrawIntoBigPixmap(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	int n_horiz_segments, n_vert_segments;
	XSegment segment[MAXLINES];
	register int x, y;

	XFillRectangle(XtDisplay(cw), cw->bitmapEdit.big_picture,
			cw->bitmapEdit.undraw_gc, 0, 0, 
			cw->bitmapEdit.pixmap_width_in_pixels
			+ 2, cw->bitmapEdit.pixmap_height_in_pixels + 2);

	n_horiz_segments = cw->bitmapEdit.pixmap_height_in_cells + 1;
	n_vert_segments = cw->bitmapEdit.pixmap_width_in_cells + 1;

	for (x = 0; x < n_horiz_segments; x += 1) {
		segment[x].x1 = 0;
		segment[x].x2 = cw->bitmapEdit.pixmap_width_in_pixels;
		segment[x].y1 = cw->bitmapEdit.cell_size_in_pixels * x;
		segment[x].y2 = cw->bitmapEdit.cell_size_in_pixels * x;
	}

	XDrawSegments(XtDisplay(cw), cw->bitmapEdit.big_picture, 
			cw->bitmapEdit.draw_gc, segment, n_horiz_segments);

	for (y = 0; y < n_vert_segments; y += 1) {
		segment[y].x1 = y * cw->bitmapEdit.cell_size_in_pixels;
		segment[y].x2 = y * cw->bitmapEdit.cell_size_in_pixels;
		segment[y].y1 = 0;
		segment[y].y2 = cw->bitmapEdit.pixmap_height_in_pixels;
	}

	XDrawSegments(XtDisplay(cw), cw->bitmapEdit.big_picture, 
			cw->bitmapEdit.draw_gc, segment, n_vert_segments);
}

/* A Public function, not static */
char *
BitmapEditGetArray(w, width_in_cells, height_in_cells)
Widget w;
int *width_in_cells, *height_in_cells;
{
    BitmapEditWidget cw = (BitmapEditWidget) w;

    *width_in_cells = cw->bitmapEdit.pixmap_width_in_cells;
    *height_in_cells = cw->bitmapEdit.pixmap_height_in_cells;
    return (cw->bitmapEdit.cell);
}

/* ARGSUSED */
static void
Resize(w)
Widget w;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	/*
	 * This takes care of the case where the widget is resized larger
	 * but there is no more pixmap to display in the bottom right corner.
	 * in that case, we need to move cur_x and cur_y to make sure that
	 * the pixmap fills as much of the window as possible, and fill 
	 * the remainder of the window with gray, or draw a line around it.
	 */
	if (cw->bitmapEdit.cur_x + cw->core.width > 
			cw->bitmapEdit.pixmap_width_in_pixels)
		cw->bitmapEdit.cur_x = cw->bitmapEdit.pixmap_width_in_pixels 
				- cw->core.width;

	if (cw->bitmapEdit.cur_y + cw->core.height > 
			cw->bitmapEdit.pixmap_height_in_pixels)
		cw->bitmapEdit.cur_y = cw->bitmapEdit.pixmap_height_in_pixels 
				- cw->core.height;

	/* Can't clear window is it hasn't been realized yet,
	 * as would happen if -geometry option is specified. */
	if (XtIsRealized(cw))
		/* now force redraw by clearing window */
		XClearArea(XtDisplay(cw), XtWindow(cw), 0, 0, 
				cw->bitmapEdit.pixmap_width_in_pixels, 
				cw->bitmapEdit.pixmap_height_in_pixels, True);
}

static void
TopLeft(w, event)
Widget w;
XButtonEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	cw->bitmapEdit.first_box = False;

	cw->bitmapEdit.select_start_x = (cw->bitmapEdit.cur_x + event->x) 
			/ cw->bitmapEdit.cell_size_in_pixels;
	cw->bitmapEdit.select_start_y = (cw->bitmapEdit.cur_y + event->y) 
			/ cw->bitmapEdit.cell_size_in_pixels;

	/* clear old selection */
	Redisplay(cw, NULL);
}

/* ARGSUSED */
static void
transfer_done_proc(w, selection, target)
Widget w;
Atom *selection;
Atom *target;
{
	/* 
	 * This widget keeps the selection ready for pasting additional
	 * times.  Having a transfer_done_proc indicates that the
	 * selection owner owns the storage allocated for
	 * converting the selection.  However, this widget allocates
	 * this storage only once in initialize, and keeps it until
	 * quitting.  Therefore, no \f(CWXtFree\fP call necessary here.
	 */
}

static void
BottomRight(w, event)
Widget w;
XButtonEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	int temp;

	cw->bitmapEdit.select_end_x = (cw->bitmapEdit.cur_x + event->x) 
			/ cw->bitmapEdit.cell_size_in_pixels;
	cw->bitmapEdit.select_end_y = (cw->bitmapEdit.cur_y + event->y) 
			/ cw->bitmapEdit.cell_size_in_pixels;

	if ((cw->bitmapEdit.select_end_x == cw->bitmapEdit.select_start_x) && 
			(cw->bitmapEdit.select_end_y == cw->bitmapEdit.select_start_y))  {
		Redisplay(cw, NULL);
		return;	/* no selection */
	}

	/* swap start and end if end is greater than start */
	if (cw->bitmapEdit.select_end_x < cw->bitmapEdit.select_start_x) {
		temp = cw->bitmapEdit.select_end_x;
		cw->bitmapEdit.select_end_x = cw->bitmapEdit.select_start_x;
		cw->bitmapEdit.select_start_x = temp;
	}

	if (cw->bitmapEdit.select_end_y < cw->bitmapEdit.select_start_y) {
		temp = cw->bitmapEdit.select_end_y;
		cw->bitmapEdit.select_end_y = cw->bitmapEdit.select_start_y;
		cw->bitmapEdit.select_start_y = temp;
	}

	if (XtOwnSelection(cw, XA_PRIMARY, event->time, convert_proc, 
			lose_ownership_proc, transfer_done_proc) == False) {
		XtWarning("bitmapEdit: failed attempting to become selection owner; make a new selection.\n");
		/* Clear old selection, because lose_ownership_proc 
		 * isn't registered. */
		Redisplay(cw, NULL); 
	}
}

static Boolean
convert_proc(w, selection, target, type_return, value_return, length_return, format_return)
Widget w;
Atom *selection;
Atom *target;
Atom *type_return;
XtPointer *value_return;
unsigned long *length_return;
int *format_return;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	int x, y;
	int width, height;
	XSelectionRequestEvent* req = XtGetSelectionRequest(w, 
			*selection, (XtRequestId) NULL);

	/* handle all required atoms, and the one that we use */
	if (*target == (XmInternAtom(XtDisplay(cw), "TARGETS", False))) {
		/* TARGETS handling copied from xclipboard.c */
		Atom* targetP;
		Atom* std_targets;
		unsigned long std_length;
		XmuConvertStandardSelection(cw, req->time, selection, 
				target, type_return,
   				(XtPointer*)&std_targets, 
				&std_length, format_return);
		*value_return = XtMalloc(sizeof(Atom)*(std_length + 1));
		targetP = *(Atom**)value_return;
		*length_return = std_length + 1;
		*targetP++ = cw->bitmapEdit.target_atom;
		bcopy((char*)std_targets, (char*)targetP, sizeof(Atom)*std_length);
		XtFree((char*)std_targets);
		*type_return = XA_ATOM;
		*format_return = sizeof(Atom) * 8;
		return(True);
	}
	/* Xt already handles MULTIPLE, no branch necessary */ 
	else if (*target == cw->bitmapEdit.target_atom) {
		char *data;

		width = cw->bitmapEdit.select_end_x - cw->bitmapEdit.select_start_x;
		height = cw->bitmapEdit.select_end_y - cw->bitmapEdit.select_start_y;

		/* 8 chars is enough for two 3-digit numbers and two delimiters */
		*length_return = ((width * height) + 8) * sizeof(char);
			
		data = XtMalloc(*length_return);

		sprintf(data, "%d@%d~", width, height);

		for (x = 0; x < width; x++) {
			for (y = 0; y < height; y++) {
				data[8 + x + (y * width)] = cw->bitmapEdit.cell[(x + cw->bitmapEdit.select_start_x) + ((y + cw->bitmapEdit.select_start_y) * cw->bitmapEdit.pixmap_width_in_cells)];
			}
		}

		*value_return = data;

		*type_return = cw->bitmapEdit.target_atom;

		*format_return = 8;  /* number of bits in char */
		return(True);
	}
	else { 
 		if (XmuConvertStandardSelection(cw, CurrentTime, selection, 
				target, type_return, value_return, 
				length_return, format_return))
				return True;
		else {
			XtWarning("bitmapEdit: requestor is requesting\
					 unsupported selection target type.\n");
			return(False);
		}
	}
}

/* ARGSUSED */
static void
lose_ownership_proc(w, selection)
Widget w;
Atom *selection;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	/* clear old selection */
	cw->bitmapEdit.first_box = False;
	cw->bitmapEdit.select_start_x = 0;
	cw->bitmapEdit.select_start_y = 0;
	cw->bitmapEdit.select_end_x = 0;
	cw->bitmapEdit.select_end_y = 0;
	Redisplay(cw, NULL);
}

/* Note: these global variables are necessary
 * because the x and y position in the buttonpress event
 * last only as long as the PasteSelection Action, yet
 * these values are needed when interpreting the selection
 * in requestor_callback. */

static int xPos, yPos;

static void
PasteSelection(w, event, params, num_params)
Widget w;
XButtonEvent *event;
String *params;
Cardinal *num_params;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	xPos = event->x;
        yPos = event->y;

	/* 
	 * Note: the actual pasting takes place in requestor_callback.
	 * This is a callback because the owner and requestor roles 
	 * may not be in the same widget.
	 */
	XtGetSelectionValue(cw, XA_PRIMARY, cw->bitmapEdit.target_atom, 
			requestor_callback, (XtPointer) event /* client_data */,
			XtLastTimestampProcessed(XtDisplay(w)));
}

/* ARGSUSED */
static void
requestor_callback(w, client_data, selection, type, value, length, format)
Widget w;
XtPointer client_data;
Atom *selection;
Atom *type;
XtPointer value;
unsigned long *length;
int *format;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	char *data = (char *)value;

        if (*type == 0 /*XT_CONVERT_FAIL*/ || *length == 0 || value == NULL) {
		XBell(XtDisplay(cw), 100);
		XtWarning("bitmapEdit: no selection or selection timed out; try again\n");
	}
	else {
		XButtonEvent *event = (XButtonEvent *) client_data;
		int width, height;
		int x, y;
		int dst_offset_x, dst_offset_y;
		char *ptr;

		dst_offset_x = (cw->bitmapEdit.cur_x + xPos) / cw->bitmapEdit.cell_size_in_pixels;
		dst_offset_y = (cw->bitmapEdit.cur_y + yPos) / cw->bitmapEdit.cell_size_in_pixels;

		ptr = (char *) value;
		width = atoi(ptr);
		ptr = index(ptr, '@');
		ptr++;
		height = atoi(ptr);
		ptr = &data[8];


		for (x = 0; x < width; x++) {
			for (y = 0; y < height; y++) {
				/* range checking */
				if (((dst_offset_x + x) > 
					   cw->bitmapEdit.pixmap_width_in_cells) 
					   || ((dst_offset_x + x) < 0))
					break;
				if (((dst_offset_y + y) > 
					   cw->bitmapEdit.pixmap_height_in_cells) 
					   || ((dst_offset_y + y) < 0))
					break;
				cw->bitmapEdit.cell[(dst_offset_x + x) 
					   + ((dst_offset_y + y) * 
					   cw->bitmapEdit.pixmap_width_in_cells)] 
					   = ptr[x + (y * width)];
				if (cw->bitmapEdit.cell[(dst_offset_x + x) 
					   + ((dst_offset_y + y) * 
					   cw->bitmapEdit.pixmap_width_in_cells)] 
					   == DRAWN)
					DrawCell(cw, dst_offset_x + x, 
							dst_offset_y + y, 
							cw->bitmapEdit.draw_gc);
				else
					DrawCell(cw, dst_offset_x + x, 
							dst_offset_y + y, 
							cw->bitmapEdit.undraw_gc);
			}
		}
		/* Regardless of the presence of a 
		 * \f(CWtransfer_done_proc\fP in the owner, 
		 * the requestor must free the data passed by 
		 * Xt after using it. */
		XtFree(value);
		Redisplay(cw, NULL);
	}
}

static void
DragHighlight(w, event)
Widget w;
XMotionEvent *event;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	static int last_drawn_x, last_drawn_y;
	int event_cell_x, event_cell_y;

	event_cell_x = cw->bitmapEdit.cur_x + (event->x / cw->bitmapEdit.cell_size_in_pixels);
	event_cell_y = cw->bitmapEdit.cur_y + (event->y / cw->bitmapEdit.cell_size_in_pixels);

	if ((event_cell_x == last_drawn_x) && (event_cell_y == last_drawn_y))
		return;

	if (cw->bitmapEdit.first_box) {
		draw_box(cw, last_drawn_x, last_drawn_y, False);
		draw_box(cw, event_cell_x, event_cell_y, True);
	}
	else {
		draw_box(cw, event_cell_x, event_cell_y, True);
		cw->bitmapEdit.first_box = True;
	}

	last_drawn_x = event_cell_x;
	last_drawn_y = event_cell_y;
}

static void
draw_box(w, x, y, draw)
Widget w;
Position x, y;
Bool draw;
{
	BitmapEditWidget cw = (BitmapEditWidget) w;
	Position start_pos_x, start_pos_y;
	Dimension width, height;
	GC gc;
	int i, j;

	start_pos_x = cw->bitmapEdit.cur_x + cw->bitmapEdit.select_start_x;
	start_pos_y = cw->bitmapEdit.cur_x + cw->bitmapEdit.select_start_y;
	
	/* swap start and end if end is greater than start */
	if (x < start_pos_x) {
		width = start_pos_x - x;
		start_pos_x = x;
	}
	else {
		width = x - start_pos_x;
	}

	if (y < start_pos_y) {
		height = start_pos_y - y;
		start_pos_y = y;
	}
	else {
		height = y - start_pos_y;
	}

/*	XDrawRectangle(XtDisplay(cw), XtWindow(cw), gc,
			(start_pos_x * cw->bitmapEdit.cell_size_in_pixels) - 1,
			(start_pos_y * cw->bitmapEdit.cell_size_in_pixels) - 1, 
			(unsigned int) width * cw->bitmapEdit.cell_size_in_pixels + 2,
			(unsigned int) height * cw->bitmapEdit.cell_size_in_pixels + 2);
*/
	for (i=start_pos_x;i < start_pos_x + width;i++)
		for (j=start_pos_y;j < start_pos_y + height;j++)
				DrawX(cw, i, j, draw);
}

DrawX(cw, x, y, draw)
BitmapEditWidget cw;
Position x, y;
Bool draw;
{
	GC gc;
   	if (cw->bitmapEdit.cell[x + y * cw->bitmapEdit.pixmap_width_in_cells] == DRAWN)
		if (draw)
			gc = cw->bitmapEdit.deep_undraw_gc;
		else
			gc = cw->bitmapEdit.deep_draw_gc;
	else
		if (draw)
			gc = cw->bitmapEdit.deep_draw_gc;
		else
			gc = cw->bitmapEdit.deep_undraw_gc;

	XDrawLine(XtDisplay(cw), XtWindow(cw), gc,
			x * cw->bitmapEdit.cell_size_in_pixels,
			y * cw->bitmapEdit.cell_size_in_pixels,
			(x + 1) * cw->bitmapEdit.cell_size_in_pixels,
			(y + 1) * cw->bitmapEdit.cell_size_in_pixels);

	XDrawLine(XtDisplay(cw), XtWindow(cw), gc,
			x * cw->bitmapEdit.cell_size_in_pixels,
			(y + 1) * cw->bitmapEdit.cell_size_in_pixels,
			(x + 1) * cw->bitmapEdit.cell_size_in_pixels,
			y * cw->bitmapEdit.cell_size_in_pixels);
}
