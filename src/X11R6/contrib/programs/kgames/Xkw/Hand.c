/* $XConsortium: Hand.c,v 1.18 91/07/26 15:21:52 keith Exp $ */
/*
 * Copyright 1991 Massachusetts Institute of Technology
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

/*
 * Hand - display collection of cards
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Converters.h>
#include <stdio.h>
#include <ctype.h>
#include "HandP.h"

#define offset(field)	XtOffsetOf(HandRec, hand.field)

static XtResource resources[] = {
    {XtNcardWidth, XtCCardWidth, XtRDimension, sizeof (Dimension),
     offset(card_width), XtRImmediate, (XtPointer) 0 },
    {XtNcardHeight, XtCCardHeight, XtRDimension, sizeof (Dimension),
     offset(card_height), XtRImmediate, (XtPointer) 0 },
    {XtNdisplayWidth, XtCDisplayWidth, XtRDimension, sizeof (Dimension),
     offset(display_width), XtRImmediate, (XtPointer) 0 },
    {XtNdisplayHeight, XtCDisplayHeight, XtRDimension, sizeof (Dimension),
     offset(display_height), XtRImmediate, (XtPointer) 0 },
    {XtNdisplayX, XtCDisplayX, XtRDimension, sizeof (Dimension),
     offset(display_x), XtRImmediate, (XtPointer) 0 },
    {XtNdisplayY, XtCDisplayY, XtRDimension, sizeof (Dimension),
     offset(display_y), XtRImmediate, (XtPointer) 0 },
    {XtNinternalBorderWidth, XtCInternalBorderWidth, XtRDimension, sizeof (Dimension),
     offset(internal_border), XtRImmediate, (XtPointer) 0},
    {XtNnumRows, XtCNumRows, XtRDimension, sizeof (Dimension),
     offset(num_rows), XtRImmediate, (XtPointer) 0 },
    {XtNnumCols, XtCNumCols, XtRDimension, sizeof (Dimension),
     offset(num_cols), XtRImmediate, (XtPointer) 0 },
    {XtNrowsHint, XtCRowsHint, XtRBoolean, sizeof (Boolean),
     offset(rows_hint), XtRImmediate, (XtPointer) False},
    {XtNcolsHint, XtCColsHint, XtRBoolean, sizeof (Boolean),
     offset(cols_hint), XtRImmediate, (XtPointer) False},
    {XtNrowOffset, XtCRowOffset, XtRDimension, sizeof (Dimension),
     offset(row_offset), XtRImmediate, (XtPointer) 0 },
    {XtNcolOffset, XtCColOffset, XtRDimension, sizeof (Dimension),
     offset(col_offset), XtRImmediate, (XtPointer) 0 },
    {XtNrowMajor, XtCRowMajor, XtRBoolean, sizeof (Boolean),
     offset(row_major), XtRImmediate, (XtPointer) False },
    {XtNdisplayCallback, XtCDisplayCallback, XtRCallback, sizeof (XtPointer),
     offset(display_callback), XtRCallback, (XtPointer) NULL},
    {XtNinputCallback, XtCInputCallback, XtRCallback, sizeof (XtPointer),
     offset(input_callback), XtRCallback, (XtPointer) NULL},
    {XtNrowInsert, XtCInsert, XtRBoolean, sizeof (Boolean),
     offset(row_insert), XtRImmediate, (XtPointer) False},
    {XtNcolInsert, XtCInsert, XtRBoolean, sizeof (Boolean),
     offset(col_insert), XtRImmediate, (XtPointer) False},
    {XtNimmediateUpdate, XtCImmediateUpdate, XtRBoolean, sizeof (Boolean),
     offset(immediate_update), XtRImmediate, (XtPointer) True},
};

#undef offset

static void Initialize ();		    /* set default sizes */
static void Destroy ();			    /* free card records */
static void Resize ();			    /* set widget size */
static void Realize ();			    /* realize widget */
static void Redisplay ();		    /* draw */
void HandUpdateDisplay ();		    /* display */
static XtGeometryResult QueryGeometry ();   /* query_geometry */
static Boolean SetValues ();		    /* set resource values */

static char defaultTranslations[] =
"<BtnDown>: select()";

static void ActionSelect();
static XtActionsRec actions[] = {
    { "select", ActionSelect },		    /* select card */
};

#define SuperClass  ((SimpleWidgetClass)&simpleClassRec)

HandClassRec handClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) SuperClass,
    /* class_name		*/	"Hand",
    /* widget_size		*/	sizeof(HandRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	FALSE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	NULL,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	defaultTranslations,
    /* query_geometry		*/	QueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* simple fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
  { /* hand fields */
    /* ignore                   */	0
  }
};

WidgetClass handWidgetClass = (WidgetClass) &handClassRec;

static int
BestOffset (want, have, desired, item_size, num_items)
    int	    want, have, desired, item_size, num_items;
{
    int	val, min;
    int	last_showing;

    if (want <= have || num_items <= 1)
	return desired;
    if (have < 0)
	have = 0;
    last_showing = item_size;
    min = item_size / 4;
    do {
	last_showing /= 2;
	val = (have - last_showing) / (num_items - 1);
    } while (val < min && val < last_showing);
    if (val > desired)
	val = desired;
    return val;
}

static int
BestColOffset (w, num_cols)
    HandWidget	w;
    int		num_cols;
{
    int	borders = 2 * w->hand.internal_border;
    
    return BestOffset (handWidth (w, num_cols) - borders,
		       w->core.width - borders,
		       w->hand.col_offset, w->hand.card_width,
		       num_cols);
}

static int
BestRowOffset (w, num_rows)
    HandWidget	w;
    int		num_rows;
{
    int	borders = 2 * w->hand.internal_border;
    
    return BestOffset (handHeight (w, num_rows) - borders,
		       w->core.height - borders,
		       w->hand.row_offset, w->hand.card_height,
		       num_rows);
}

static int
ColsInRow (w, row)
    HandWidget	w;
    int		row;
{
    int	    maxCol = -1;
    CardPtr c;
    for (c = w->hand.bottomCard; c; c = c->prev)
	if (c->row == row && c->col > maxCol)
	    maxCol = c->col;
    return maxCol;
}
    
static int
RowsInCol (w, col)
    HandWidget	w;
    int		col;
{
    int	maxRow = -1;
    CardPtr c;
    for (c = w->hand.bottomCard; c; c = c->prev)
	if (c->col == col && c->row > maxRow)
	    maxRow = c->row;
    return maxRow;
}
    
static int
XPos (w, row, col)
    HandWidget	w;
    int		row, col;
{
    int	offset;
    CardPtr c;
    int	numCol;
    int numDefault;
    int	defaultOffset;
    int	lastCol;
    
    if (w->hand.row_major)
    {
	numCol = ColsInRow (w, row) + 1;
	if (w->hand.cols_hint)
	    defaultOffset = BestColOffset (w, numCol);
	else
	    defaultOffset = w->hand.real_col_offset;
	lastCol = 0;
	offset = 0;
	numDefault = 0;
	for (c = w->hand.bottomCard; c; c = c->prev)
	{
	    if (c->row == row && c->col >= lastCol && c->col < col)
	    {
		numDefault += (c->col - lastCol);
		if (c->offset == XkwHandDefaultOffset || 
		    c->offset > defaultOffset)
		    numDefault++;
		else
		    offset += c->offset;
		lastCol = c->col + 1;
	    }
	}
	if (col > lastCol)
	    numDefault += (col - lastCol);
	if (numDefault)
	    offset += numDefault * defaultOffset;
    }
    else
	offset = col * w->hand.real_col_offset;
    return offset + w->hand.internal_border;
}

static int
ColFromX (w, x, row)
    HandWidget	w;
    int		x;
    int		row;
{
    int	offset;
    int	adjust;
    int	x1, x2;
    int	col;

    if (w->hand.row_major)
    {
	x2 = w->hand.internal_border;
	for (col = 1; ; col++)
	{
	    x1 = x2;
	    x2 = XPos (w, row, col);
	    adjust = 0;
	    if (x2 - x1 > w->hand.card_width)
		adjust = ((x2 - x1) - w->hand.card_width) / 2;
	    if (x < x2 - adjust)
		return col - 1;
	}
    }
    else
    {
	offset = w->hand.real_col_offset;
	adjust = -w->hand.internal_border;
	if (offset > w->hand.card_width)
	    adjust += (offset - w->hand.card_width) / 2;
	return (x + adjust) / offset;
    }
}

static int
YPos (w, row, col)
    HandWidget	w;
    int		row, col;
{
    CardPtr c;
    int	    offset;
    int	    numRow;
    int	    numDefault;
    int	    defaultOffset;
    int	    lastRow;
    
    if (!w->hand.row_major)
    {
	numRow = RowsInCol (w, col) + 1;
	if (w->hand.rows_hint)
	    defaultOffset = BestRowOffset (w, numRow);
	else
	    defaultOffset = w->hand.real_row_offset;
	lastRow = 0;
	offset = 0;
	numDefault = 0;
	for (c = w->hand.bottomCard; c; c = c->prev)
	{
	    if (c->col == col && c->row >= lastRow && c->row < row)
	    {
		numDefault += (c->row - lastRow);
		if (c->offset == XkwHandDefaultOffset || 
		    c->offset > defaultOffset)
		    numDefault++;
		else
		    offset += c->offset;
		lastRow = c->row + 1;
	    }
	}
	if (row > lastRow)
	    numDefault += (row - lastRow);
	if (numDefault)
	    offset += numDefault * defaultOffset;
    }
    else
	offset = row * w->hand.real_row_offset;
    return offset + w->hand.internal_border;
}

static int
RowFromY (w, y, col)
    HandWidget	w;
    int		y;
    int		col;
{
    int	offset;
    int	adjust;
    int	y1, y2;
    int	row;

    if (!w->hand.row_major)
    {
	y2 = w->hand.internal_border;
	for (row = 1; ; row++)
	{
	    y1 = y2;
	    y2 = YPos (w, row, col);
	    adjust = 0;
	    if (y2 - y1 > w->hand.card_height)
		adjust = ((y2 - y1) - w->hand.card_height) / 2;
	    if (y < y2 - adjust)
		return row - 1;
	}
    }
    else
    {
	offset = w->hand.real_row_offset;
	adjust = -w->hand.internal_border;
	if (offset > w->hand.card_height)
	    adjust += (offset - w->hand.card_height) / 2;
	return (y + adjust) / offset;
    }
}

static int
handWidth (w, num_cols)
    HandWidget	w;
    int		num_cols;
{
    return w->hand.card_width + 2 * w->hand.internal_border +
	   (num_cols - 1) * w->hand.col_offset;
}

static int
handHeight (w, num_rows)
    HandWidget	w;
    int		num_rows;
{
    return w->hand.card_height + 2 * w->hand.internal_border +
	   (num_rows - 1) * w->hand.row_offset;
}

static void
getHandSize (w, widthp, heightp)
    HandWidget	w;
    Dimension	*widthp, *heightp;
{
    *widthp = handWidth (w, w->hand.num_cols);
    *heightp = handHeight (w, w->hand.num_rows);
}


static void Initialize (greq, gnew)
    Widget  greq, gnew;
{
    HandWidget	req = (HandWidget) greq,
		new = (HandWidget) gnew;

    getHandSize (req, &new->core.width, &new->core.height);
    new->hand.real_row_offset = new->hand.row_offset;
    new->hand.real_col_offset = new->hand.col_offset;
    new->hand.topCard = NULL;
    new->hand.bottomCard = NULL;
    new->hand.erased = NULL;
    new->hand.exposeTime = 0;
}

#define MotionMask ( \
	PointerMotionMask | Button1MotionMask | \
	Button2MotionMask | Button3MotionMask | Button4MotionMask | \
	Button5MotionMask | ButtonMotionMask )
#define PointerGrabMask ( \
	ButtonPressMask | ButtonReleaseMask | \
	EnterWindowMask | LeaveWindowMask | \
	PointerMotionHintMask | KeymapStateMask | \
	MotionMask )
	    
static void Realize (widget, value_mask, attributes)
    Widget		 widget;
    XtValueMask		 *value_mask;
    XSetWindowAttributes *attributes;
{
    unsigned int    event_mask = 0;
#define MAX_BUT	256
    unsigned char   mapping[MAX_BUT];
    int	    i, max;
    
    (*SuperClass->core_class.realize)(widget, value_mask, attributes);
    if (*value_mask & CWEventMask)
	event_mask = attributes->event_mask;
    event_mask &= PointerGrabMask;
    if (event_mask & ButtonPressMask)
    {
	max = XGetPointerMapping (XtDisplay (widget), mapping, MAX_BUT);
	for (i = 0; i < max; i++)
	{
	    if (mapping[i] != 0)
		XtGrabButton (widget, i, AnyModifier, True, event_mask,
			      GrabModeAsync, GrabModeAsync, None, None);
	}
    }
}

static void Destroy (gw)
    Widget  gw;
{
    HandWidget	w = (HandWidget) gw;
    CardPtr	c, n;

    for (c = w->hand.topCard; c; c = n) {
	n = c->next;
	Dispose (c);
    }
}

static XtGeometryResult QueryGeometry (gw, intended, prefered)
    Widget  gw;
    XtWidgetGeometry	*intended, *prefered;
{
    HandWidget	w = (HandWidget) gw;
    Dimension	width, height;
    XtGeometryResult	result;

    getHandSize (w, &width, &height);
    result = XtGeometryYes;
    prefered->request_mode = 0;
    if (intended->request_mode & CWWidth) {
	if (intended->width != width) {
	    if (width == w->core.width) {
		result = XtGeometryNo;
	    } else if (result != XtGeometryNo) {
	    	prefered->request_mode |= CWWidth;
	    	prefered->width = width;
	    	result = XtGeometryAlmost;
	    }
	}
    }
    if (intended->request_mode & CWHeight) {
	if (intended->height != height) {
	    if (height == w->core.height) {
		result = XtGeometryNo;
	    } else if (result != XtGeometryNo) {
	    	prefered->request_mode |= CWHeight;
	    	prefered->height = height;
	    	result = XtGeometryAlmost;
	    }
	}
    }
    return result;
}

static Bool XYInCard (w, c, x, y)
    HandWidget	w;
    CardPtr	c;
    int		x, y;
{
    return c->x <= x && x < c->x + w->hand.card_width &&
	   c->y <= y && y < c->y + w->hand.card_height;
}

static CardPtr XYToCard (w, x, y)
    HandWidget	w;
    int		x, y;
{
    CardPtr c;

    for (c = w->hand.topCard; c; c = c->next) {
	if (XYInCard (w, c, x, y))
	    return c;
    }
    return NULL;
}

static void ActionSelect (gw, event, params, num_params)
    Widget  gw;
    XEvent  *event;
    String  *params;
    Cardinal	*num_params;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr	    c;
    HandInputRec    input;
    int		    row, col;
    XtPointer	    private;

    c = XYToCard (w, event->xbutton.x, event->xbutton.y);
    if (c)
    {
	row = c->row;
	col = c->col;
	private = c->private;
    }
    else
    {
	if (w->hand.row_major)
	{
	    row = RowFromY (w, event->xbutton.y, -1);
	    col = ColFromX (w, event->xbutton.x, row);
	}
	else
	{
	    col = ColFromX (w, event->xbutton.x, -1);
	    row = RowFromY (w, event->xbutton.y, col);
	}
	if (row < 0)
	    row = 0;
	else if (w->hand.row_major && w->hand.num_rows <= row)
	    row = w->hand.num_rows - 1;
	if (col < 0)
	    col = 0;
	else if (!w->hand.row_major && w->hand.num_cols <= col)
	    col = w->hand.num_cols - 1;
	private = 0;
    }
    input.w = gw;
    input.row = row;
    input.col = col;
    input.private = private;
    input.params = params;
    input.event = *event;
    input.num_params = num_params;
    XtCallCallbackList ((Widget) w, w->hand.input_callback, (XtPointer) &input);
}

static Bool CardInRect (w, rect, c)
    HandWidget	w;
    XRectangle	*rect;
    CardPtr	c;
{
    XRectangle	*card = &c->clip;

    return rect->x < card->x + card->width &&
	   card->x < rect->x + rect->width &&
	   rect->y < card->y + card->height &&
	   card->y < rect->y + rect->height;
}

#define ClipRects(allClipped) { \
    if (cx1 <= x1 && x2 <= cx2) \
    { \
	if (cy1 <= y1 && y1 < cy2) \
	    y1 = cy2; \
	if (cy1 < y2 && y2 <= cy2) \
	    y2 = cy1; \
	if (y2 < y1) \
	    y2 = y1; \
	if (y1 == y2) \
	    allClipped; \
    } \
    if (cy1 <= y1 && y2 <= cy2) \
    { \
	if (cx1 <= x1 && x1 < cx2) \
	    x1 = cx2; \
	if (cx1 < x2 && x2 <= cx2) \
	    x2 = cx1; \
	if (x2 < x1) \
	    x2 = x1; \
	if (x1 == x2) \
	    allClipped; \
    } \
}

static void MarkCard (w, c)
    HandWidget	w;
    CardPtr	c;
{
    XRectangle	*r;
    ErasedPtr	e, n, *p;
    int		cx1, cy1, cx2, cy2;
    int		x1, y1, x2, y2;
    Boolean	allClipped;

    c->redisplay = TRUE;
    if (w->hand.erased)
    {
	cx1 = c->x + w->hand.display_x;
	cy1 = c->y + w->hand.display_y;
	cx2 = cx1 + w->hand.display_width;
	cy2 = cy1 + w->hand.display_height;
	if (cx1 < c->clip.x)
	    cx1 = c->clip.x;
	if (cx2 > c->clip.x + c->clip.width)
	    cx2 = c->clip.x + c->clip.width;
	if (cy1 < c->clip.y)
	    cy1 = c->clip.y;
	if (cy2 > c->clip.y + c->clip.height)
	    cy2 = c->clip.y + c->clip.height;
	p = &w->hand.erased;
	for (e = w->hand.erased; e; e = n)
	{
	    n = e->next;
	    allClipped = False;
	    x1 = e->r.x;
	    x2 = x1 + e->r.width;
	    y1 = e->r.y;
	    y2 = y1 + e->r.height;
	    ClipRects (allClipped = True);
	    if (allClipped)
	    {
		Dispose (e);
		*p = n;
	    }
	    else
	    {
		e->r.x = x1;
		e->r.y = y1;
		e->r.width = x2 - x1;
		e->r.height = y2 - y1;
		p = &e->next;
	    }
	}
    }
    r = &c->clip;
    for (c = c->prev; c; c = c->prev)
	if (!c->redisplay && CardInRect (w, r, c))
	    MarkCard (w, c);
}

static void MarkRectangle (w, r)
    HandWidget	w;
    XRectangle	*r;
{
    CardPtr c;

    for (c = w->hand.bottomCard; c; c = c->prev)
	if (!c->redisplay && CardInRect (w, r, c))
	    MarkCard (w, c);
}

static void
EraseCard (w, c)
    HandWidget	w;
    CardPtr	c;
{
    ErasedPtr	e = New(ErasedRec);

    e->r = c->clip;
    e->next = w->hand.erased;
    e->fill = True;
    e->isCard = True;
    e->cardX = c->x;
    e->cardY = c->y;
    w->hand.erased = e;
}


Boolean
ComputeClip (w, card)
    HandWidget	    w;
    CardPtr	    card;
{
    int	    x1, y1, x2, y2;
    int	    cx1, cy1, cx2, cy2;
    int	    ox1, oy1, ox2, oy2;
    CardPtr c;

    ox1 = x1 = card->x;
    oy1 = y1 = card->y;
    ox2 = x2 = x1 + w->hand.card_width;
    oy2 = y2 = y1 + w->hand.card_height;
    c = card;
    while (c = c->prev) 
    {
	if (!c->shouldBeUp)
	    continue;
	cx1 = c->x + w->hand.display_x;
	cy1 = c->y + w->hand.display_y;
	cx2 = cx1 + w->hand.display_width;
	cy2 = cy1 + w->hand.display_height;
	ClipRects (break);
    }
    card->clip.x = x1;
    card->clip.y = y1;
    card->clip.width = x2 - x1;
    card->clip.height = y2 - y1;
    if (x1 == ox1 && y1 == oy1 && x2 == ox2 && y2 == oy2)
	card->clipped = ClipUnclipped;
    else if (x2 > x1 && y2 > y1)
	card->clipped = ClipPartclipped;
    else
	card->clipped = ClipAllclipped;
}

void
HandUpdateDisplay (gw)
    Widget  gw;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr	    c, nc;
    ErasedPtr	    e, ne;
    HandDisplayRec  display;
    int		    x, y;

    if (!XtIsRealized (gw))
	return;
    
    /* Mark cards for redisplay */
    for (c = w->hand.topCard; c; c = nc)
    {
	nc = c->next;
	if (c->shouldBeUp)
	{
	    x = XPos (w, c->row, c->col);
	    y = YPos (w, c->row, c->col);
	    if (c->isUp)
	    {
		if (c->x != x || c->y != y)
		{
		    EraseCard (w, c);
		    c->isUp = False;
		}
	    }
	    c->x = x;
	    c->y = y;
	    ComputeClip (w, c);
	}
	else if (c->isUp)
	    EraseCard (w, c);
	if (c->delete)
	{
	    if (c->prev)
		c->prev->next = c->next;
	    else
		w->hand.topCard = c->next;
	    if (c->next)
		c->next->prev = c->prev;
	    else
		w->hand.bottomCard = c->prev;
	    Dispose (c);
	}
    }
    /* mark effects of redisplay */
    for (c = w->hand.bottomCard; c; c = c->prev)
    {
	if (c->shouldBeUp && !c->isUp || c->forceRedraw)
	{
	    MarkCard (w, c);
	    c->isUp = True;
	    c->forceRedraw = False;
	}
    }
    
    /* clear out the blank areas */
    for (e = w->hand.erased; e; Dispose(e), e = ne) 
    {
	ne = e->next;
	if (e->isCard)
	{
	    for (c = w->hand.bottomCard; c; c = c->prev)
		if (c->redisplay && c->x == e->cardX && c->y == e->cardY)
		    break;
	    if (c)
		continue;
	}
	MarkRectangle (w, &e->r);
	if (e->fill)
	{
	    XClearArea (XtDisplay(w), XtWindow (w), 
			e->r.x, e->r.y, e->r.width, e->r.height, False);
	}
    }
    w->hand.erased = NULL;

    /* redisplay cards */
    for (c = w->hand.bottomCard; c; c = c->prev)
	if (c->redisplay)
	{
	    if (c->clipped != ClipAllclipped)
	    {
		display.w = (Widget) w;
		display.x = c->x;
		display.y = c->y;
		display.private = c->private;
		display.clipped = False;
		display.clip = c->clip;
		if (c->clipped == ClipPartclipped)
		    display.clipped = True;
		XtCallCallbackList ((Widget) w, w->hand.display_callback, (XtPointer) &display);
	    }
	    c->redisplay = FALSE;
	}
}

/* 
 * widget was resized, adjust row and column offsets.  The expose
 * event will cause the card positions to get recomputed which will
 * then cause them all to be redrawn
 */

static void Resize (gw)
    Widget  gw;
{
    HandWidget	w = (HandWidget) gw;
    
    w->hand.real_col_offset = BestColOffset (w, w->hand.num_cols);
    w->hand.real_row_offset = BestRowOffset (w, w->hand.num_rows);
    w->hand.exposeTime = 0;
}

/*
 * Redisplay function.  Queue the rectangle as an erased area
 * and redisplay when we've gotten all of the events
 */
static void Redisplay (gw, event, region)
    Widget  gw;
    XEvent  *event;
    Region  region;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr	    c;
    XRectangle	    r;
    ErasedPtr	    e;
    int		    cx1, cy1, cx2, cy2;
    int		    x1, y1, x2, y2;

    x1 = event->xexpose.x;
    x2 = x1 + event->xexpose.width;
    y1 = event->xexpose.y;
    y2 = y1 + event->xexpose.height;
    if (event->xexpose.serial <= w->hand.exposeTime)
    {
	cx1 = w->hand.lastExpose.x;
	cy1 = w->hand.lastExpose.y;
	cx2 = cx1 + w->hand.lastExpose.width;
	cy2 = cy1 + w->hand.lastExpose.height;
	ClipRects (return)
    }
    e = New(ErasedRec);
    e->r.x = x1;
    e->r.y = y1;
    e->r.width = x2 - x1;
    e->r.height = y2 - y1;
    e->next = w->hand.erased;
    e->fill = False;
    e->isCard = False;
    w->hand.erased = e;
    if (event->xexpose.count == 0) {
	w->hand.exposeTime = NextRequest (XtDisplay (w));
	w->hand.lastExpose.x = x1;
	w->hand.lastExpose.y = y1;
	w->hand.lastExpose.width = x2 - x1;
	w->hand.lastExpose.height = y2 - y1;
	HandUpdateDisplay (w);
    }
}

/*
 * When values are set which change the desired size, ask for
 * the new size 
 */
 
static Boolean SetValues (gcur, greq, gnew)
    Widget  gcur, greq, gnew;
{
    HandWidget	cur = (HandWidget) gcur,
		req = (HandWidget) greq,
 		new = (HandWidget) gnew;
    Dimension	curwidth, curheight, reqwidth, reqheight;
    Dimension	width, height;

    getHandSize (cur, &curwidth, &curheight);
    getHandSize (req, &reqwidth, &reqheight);
    if (curwidth != reqwidth || curheight != reqheight)
    {
	XtMakeResizeRequest (gnew, reqwidth, reqheight, &width, &height);
	if (width != curwidth || height != curheight)
	    Resize (gnew);
    }
    new->hand.exposeTime = 0;
    return TRUE;
}

/* Insert a card */
XtPointer
HandAddCard (gw, private, row, col, offset)
    Widget	gw;
    XtPointer	private;
    int		row;
    int		col;
    int		offset;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr	    c, sib;
    int		    maxPos;

    /* compute values for row/col if requested */
    if (row == InsertRow) 
    {
	maxPos = -1;
	for (c = w->hand.topCard; c; c = c->next)
	{
	    if (w->hand.row_major)
	    {
		if (c->row > maxPos)
		    maxPos = c->row;
	    }
	    else
	    {
		if (c->col == col && c->row > maxPos)
		    maxPos = c->row;
	    }
	}
	row = maxPos + 1;
    }
    if (col == InsertCol)
    {
	maxPos = -1;
	for (c = w->hand.topCard; c; c = c->next)
	{
	    if (!w->hand.row_major)
	    {
		if (c->col > maxPos)
		    maxPos = c->col;
	    }
	    else
	    {
		if (c->row == row && c->col > maxPos)
		    maxPos = c->col;
	    }
	}
	col = maxPos + 1;
    }
    /* find the card below this one */
    sib = NULL;
    for (c = w->hand.bottomCard; c; c = c->prev)
    {
	if (w->hand.row_major)
	{
	    if ((c->row == row && c->col <= col) || c->row < row)
		sib = c;
	    else
		break;
	}
	else
	{
	    if ((c->col == col && c->row <= row) || c->col < col)
		sib = c;
	    else
		break;
	}
    }
    c = New(CardRec);
    c->redisplay = False;
    c->forceRedraw = False;
    c->isUp = False;
    c->shouldBeUp = True;
    c->delete = False;
    c->private = private;
    c->row = row;
    c->col = col;
    c->offset = offset;
    /* insert the new card on the list */
    c->next = sib;	/* c is above sib */
    
    if (sib)
	c->prev = sib->prev;
    else
	c->prev = w->hand.bottomCard;
    
    if (c->next)
	c->next->prev = c;
    else
	w->hand.bottomCard = c;
    
    if (c->prev)
	c->prev->next = c;
    else
	w->hand.topCard = c;
    
    /* adjust higher cards rows */
    if (w->hand.row_insert) 
    {
	for (sib = c->prev; sib; sib = sib->prev)
	    if (sib->col == c->col && sib->row == row)
		sib->row = ++row;
    }
    /* adjust higher cards columns */
    if (w->hand.col_insert)
    {
	for (sib = c->prev; sib; sib = sib->prev)
	    if (sib->row == c->row && sib->col == col)
		sib->col = ++col;
    }
    w->hand.exposeTime = 0;
    if (XtIsRealized(gw) && w->hand.immediate_update)
	HandUpdateDisplay (gw);
    return (XtPointer) c;
}

/* remove a card */
void
HandRemoveCard (gw, card)
    Widget	gw;
    XtPointer	card;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr	    c, sib;
    int		    row, col;

    for (c = w->hand.topCard; c; c = c->next)
	if (c == (CardPtr) card)
	    break;
    if (!c)
	return;
    c->shouldBeUp = False;
    c->delete = True;
    if (w->hand.row_insert) 
    {
	row = c->row;
	for (sib = c->prev; sib; sib = sib->prev)
	    if (sib->col == c->col && sib->row == row + 1)
		sib->row = row++;
    }
    if (w->hand.col_insert)
    {
	col = c->col;
	for (sib = c->prev; sib; sib = sib->prev)
	    if (sib->row == c->row && sib->col == col + 1)
		sib->col = col++;
    }
    w->hand.exposeTime = 0;
    if (XtIsRealized (gw) && w->hand.immediate_update)
	HandUpdateDisplay (gw);
}

/* change the private value for a card and force a redisplay */
void
HandReplaceCard (gw, card, private, offset)
    Widget	gw;
    XtPointer	card;
    XtPointer	private;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr c;
    for (c = w->hand.topCard; c; c = c->next)
	if (c == (CardPtr) card)
	    break;
    if (!c)
	return;
    c->private = private;
    c->forceRedraw = True;
    c->offset = offset;
    w->hand.exposeTime = 0;
    if (XtIsRealized (gw) && w->hand.immediate_update)
    	HandUpdateDisplay (w);
}

void
HandRectangleForPos (gw, row, col, r)
    Widget	gw;
    int		row, col;
    XRectangle	*r;
{
    HandWidget	    w = (HandWidget) gw;

    r->x = XPos (w, row, col);
    r->y = YPos (w, row, col);
    r->width = w->hand.card_width;
    r->height = w->hand.card_height;
}

void
HandRectangleForCard (gw, card, r)
    Widget	gw;
    XtPointer	card;
    XRectangle	*r;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr	c;

    for (c = w->hand.topCard; c; c = c->next)
	if (c == (CardPtr) card)
	    break;
    if (!c) 
    {
	r->x = 0;
	r->y = 0;
	r->width = 0;
	r->height = 0;
    }
    else
	HandRectangleForPos (gw, c->row, c->col, r);
}

Boolean
HandXYToPos (gw, x, y, rowp, colp)
    Widget  gw;
    int	    x, y;
    int	    *rowp, *colp;
{
    HandWidget	w = (HandWidget) gw;

    CardPtr	c;
    c = XYToCard (w, x, y);
    if (c)
    {
	*rowp = c->row;
	*colp = c->col;
	return True;
    }
    return False;
}

void
HandRemoveAllCards (gw)
    Widget  gw;
{
    HandWidget	    w = (HandWidget) gw;
    CardPtr	    c, n;

    for (c = w->hand.topCard; c; c = n) {
	n = c->next;
	free ((char *) c);
    }
    w->hand.topCard = 0;
    w->hand.bottomCard = 0;
    w->hand.exposeTime = 0;
    if (XtIsRealized (gw))
	XClearWindow (XtDisplay (w), XtWindow (w));
}
