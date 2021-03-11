/* $XConsortium: CribBoard.c,v 1.4 91/02/17 16:18:42 converse Exp $ */

/* Copyright	Massachusetts Institute of Technology	1987, 1988
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "CribBoardP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(CribBoardRec, cribBoard.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    { XtNpegColor, XtCForeground, XtRPixel, sizeof (unsigned long),
      offset (pegColor), XtRString, XtDefaultForeground },
    { XtNholeColor, XtCForeground, XtRPixel, sizeof (unsigned long),
      offset (holeColor), XtRString, XtDefaultForeground },
    { XtNpegSize, XtCPegSize, XtRInt, sizeof (int),
      offset (pegSize), XtRImmediate, (XtPointer) 7 },
    { XtNholeSize, XtCHoleSize, XtRInt, sizeof (int), 
      offset (holeSize), XtRImmediate, (XtPointer) 5 },
    { XtNnumPegs, XtCNumPegs, XtRInt, sizeof (int), 
      offset (numPegs), XtRImmediate, (XtPointer) 2 },
    { XtNgroupSpace, XtCGroupSpace, XtRInt, sizeof (int), 
      offset (groupSpace), XtRImmediate, (XtPointer) 4 },
    { XtNrowSpace, XtCRowSpace, XtRInt, sizeof (int), 
      offset (rowSpace), XtRImmediate, (XtPointer) 15 },
    { XtNnumCols, XtCNumCols, XtRInt, sizeof (int),
      offset (numCols), XtRImmediate, (XtPointer) 30 },
    { XtNnumRows, XtCNumRows, XtRInt, sizeof (int),
      offset (numRows), XtRImmediate, (XtPointer) 4 },
#undef offset
};

static void	Initialize(), Destroy (), Redisplay ();
static Boolean	SetValues ();

CribBoardClassRec cribBoardClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"CribBoard",
    /* widget_size		*/	sizeof(CribBoardRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	NULL,
    /* num_actions		*/	0,
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	NULL,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	NULL,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* empty			*/	0
  },
  { /* cribBoard fields */
    /* empty			*/	0
  }
};

WidgetClass cribBoardWidgetClass = (WidgetClass)&cribBoardClassRec;

static void
getSize (w, widthp, heightp)
    CribBoardWidget    w;
    Dimension	    *widthp, *heightp;
{
    int	size;

    *widthp = (w->cribBoard.numCols / 5 - 1) * w->cribBoard.groupSpace +
	      (w->cribBoard.numCols * w->cribBoard.pegSize * 2);
    *heightp = w->cribBoard.numRows * (w->cribBoard.pegSize + w->cribBoard.rowSpace);
}

static void Initialize (greq, gnew)
    Widget  greq, gnew;
{
    CribBoardWidget	req = (CribBoardWidget) greq,
			new = (CribBoardWidget) gnew;
    XGCValues		gcv;
    int			i;

    getSize (new, &new->core.width, &new->core.height);
    gcv.foreground = new->cribBoard.pegColor;
    new->cribBoard.pegGC = XtGetGC (gnew, GCForeground, &gcv);
    gcv.foreground = new->cribBoard.holeColor;
    new->cribBoard.holeGC = XtGetGC (gnew, GCForeground, &gcv);
    new->cribBoard.pegs = Some (int, req->cribBoard.numPegs);
    for (i = 0; i < req->cribBoard.numPegs; i++)
	new->cribBoard.pegs[i] = CribBoardUnset;
}

static void
Destroy (gw)
    Widget  gw;
{
    CribBoardWidget    w = (CribBoardWidget) gw;

    XtReleaseGC (gw, w->cribBoard.pegGC);
    XtReleaseGC (gw, w->cribBoard.holeGC);
    Dispose (w->cribBoard.pegs);
}

#define RowPos(w, row)	((row + 1) * ((w)->cribBoard.holeSize + (w)->cribBoard.rowSpace))
#define ColPos(w, col)	(((col) / 5) * (w)->cribBoard.groupSpace + (col+ 1) * (w)->cribBoard.pegSize * 2)

/*
 * The peg is drawn as a trapezoid topped by a filled arc
 */
 
#define ANGLE	    60	    /* angle from vertical */
#define COS	    0.5
#define SIN	    0.866025403784439

#define PEG_SIZE    ((w)->cribBoard.pegSize)
#define HOLE_SIZE   ((w)->cribBoard.holeSize)
 
#define PEG_RAD	    (PEG_SIZE / 2.0)
#define HOLE_RAD    (HOLE_SIZE / 2.0)

#define PEG_LENGTH  PEG_RAD * 2

#define CIRCLE_X    -COS * PEG_LENGTH
#define CIRCLE_Y    -SIN * PEG_LENGTH

#define POLY_X0	    SIN * HOLE_RAD
#define POLY_Y0	    -COS * HOLE_RAD
 
#define POLY_X1	    -SIN * HOLE_RAD
#define POLY_Y1	    COS * HOLE_RAD
 
#define POLY_X2	    CIRCLE_X - PEG_RAD * SIN
#define POLY_Y2	    CIRCLE_Y + PEG_RAD * COS
 
#define POLY_X3	    CIRCLE_X + PEG_RAD * SIN
#define POLY_Y3	    CIRCLE_Y - PEG_RAD * COS


static void
drawPeg (w, value)
    CribBoardWidget	w;
    int			value;
{
    int	    row, col;
    int	    x;
    int	    y;
    XPoint  p[4];

    if (value == CribBoardUnset)
	return;
    row = value / w->cribBoard.numCols;
    col = value % w->cribBoard.numCols;
    if (row & 1)
	col = (w->cribBoard.numCols - 1) - col;
    x = ColPos (w, col);
    y = RowPos (w, row);
    p[0].x = x + POLY_X0;
    p[0].y = y + POLY_Y0;
    p[1].x = x + POLY_X1;
    p[1].y = y + POLY_Y1;
    p[2].x = x + POLY_X2;
    p[2].y = y + POLY_Y2;
    p[3].x = x + POLY_X3;
    p[3].y = y + POLY_Y3;
    XFillPolygon (XtDisplay (w), XtWindow (w), w->cribBoard.pegGC, 
		  p, 4, Convex, CoordModeOrigin);
    XFillArc (XtDisplay (w), XtWindow (w), w->cribBoard.pegGC,
	      (int) (x + CIRCLE_X - PEG_RAD), (int) (y + CIRCLE_Y - PEG_RAD),
	      PEG_SIZE, PEG_SIZE, 0, 64 * 360);
}
 
static void
drawHole (w, value)
    CribBoardWidget	w;
    int			value;
{
    int	    row, col;
    int	    x;
    int	    y;

    row = value / w->cribBoard.numCols;
    col = value % w->cribBoard.numCols;
    x = ColPos (w, col);
    y = RowPos (w, row);
    XFillArc (XtDisplay (w), XtWindow (w), w->cribBoard.pegGC,
	      (int) (x - HOLE_RAD), (int) (y - HOLE_RAD),
	      HOLE_SIZE, HOLE_SIZE, 0, 64 * 360);
}

static void
Redisplay (gw, event, region)
    Widget  gw;
    XEvent  *event;
    Region  region;
{
    CribBoardWidget w = (CribBoardWidget) gw;
    int		    v;

    for (v = 0; v < w->cribBoard.numRows * w->cribBoard.numCols; v++)
	drawHole (w, v);
    for (v = 0; v < w->cribBoard.numPegs; v++)
	drawPeg (w, w->cribBoard.pegs[v]);
}

static Boolean
SetValues (gcur, greq, gnew)
    Widget  gcur, greq, gnew;
{
    CribBoardWidget    cur = (CribBoardWidget) gcur,
		    req = (CribBoardWidget) greq,
		    new = (CribBoardWidget) gnew;
    XGCValues	    gcv;
    Boolean	    redraw = FALSE;
    Dimension	    width, height;

    if (req->cribBoard.pegColor != cur->cribBoard.pegColor)
    {
	XtReleaseGC (gcur, cur->cribBoard.pegGC);
	gcv.foreground = req->cribBoard.pegColor;
	new->cribBoard.pegGC = XtGetGC (gnew, GCForeground, &gcv);
	redraw = TRUE;
    }
    if (req->cribBoard.holeColor != cur->cribBoard.holeColor)
    {
	XtReleaseGC (gcur, cur->cribBoard.holeGC);
	gcv.foreground = req->cribBoard.holeColor;
	new->cribBoard.holeGC = XtGetGC (gnew, GCForeground, &gcv);
	redraw = TRUE;
    }
    if (req->cribBoard.groupSpace != cur->cribBoard.groupSpace ||
	req->cribBoard.rowSpace != cur->cribBoard.rowSpace)
    {
	getSize (new, &width, &height);
	XtMakeResizeRequest (gnew, width, height, &width, &height);
	redraw = TRUE;
    }
    return redraw;
}

XkwCribBoardSetPeg (gw, i, v)
    Widget  gw;
    int	    i, v;
{
    CribBoardWidget w = (CribBoardWidget) gw;

    if (0 <= i && i < w->cribBoard.numPegs && w->cribBoard.pegs[i] != v)
    {
	w->cribBoard.pegs[i] = v;
	XClearArea (XtDisplay (w), XtWindow (w), 0, 0, 0, 0, True);
    }
}
