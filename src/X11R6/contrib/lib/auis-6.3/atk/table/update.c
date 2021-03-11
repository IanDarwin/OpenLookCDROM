/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/table/RCS/update.c,v 1.12 1993/01/11 22:56:14 rr2b R6tape $";
#endif

extern char * fcvt();

#include <class.h>

#include <graphic.ih>
#include <fontdesc.ih>
#include <im.ih>
#include <view.ih>
#include <dataobj.ih>
#include <rect.h>
#include <table.ih>

#define AUXMODULE
#include <spread.eh>

extern struct view *spread_FindSubview();

static boolean debug=0;

/* loops over cells */

#define TooFarLeft(T, j, z, cr) ((j) < table_NumberOfColumns(T) && z + spread_SPACING + table_ColumnWidth(T, j) - spread_CELLMARGIN <= rectangle_Left(cr))
#define TooFarUp(T, j, z, cr) ((j) < V->rowInfoCount && z + V->rowInfo[j].computedHeight - spread_CELLMARGIN <= rectangle_Top(cr))
#define XinRange(T, j, z, cr) ((j) < table_NumberOfColumns(T) && (z) < rectangle_Right(cr))
#define YinRange(V, T, j, z, cr) ((j) < table_NumberOfRows(T) && (j) < V->rowInfoCount && (z) < rectangle_Bottom(cr))

/* return first pixel value for given cell index */

long spread_Width(V, i, j)
struct spread * V;
int i;
int j;
{
    struct table *T = MyTable(V);
    long p = 0;

    if (i < 0)
	i = 0;
    if (j < 0)
	j = 0;
    while (i < j)
	p += (table_ColumnWidth(T, i++) + spread_SPACING);
    while (i > j)
	p -= (table_ColumnWidth(T, --i) + spread_SPACING);
    return p;
}

long spread_Height(V, i, j)
struct spread * V;
int i;
int j;
{
    long p = 0;

    if (j < 0)
	j = 0;
    if (i < 0)
	i = 0;
    if (j > V->rowInfoCount)
	j = V->rowInfoCount;
    if (i > V->rowInfoCount)
	i = V->rowInfoCount;
    while (i < j)
	p += (V->rowInfo[i++].computedHeight);
    while (i > j)
	p -= (V->rowInfo[--i].computedHeight);
    return p;
}

/* should I have highlighting? */

int spread_WantHighlight(V)
struct spread *V;
{
    struct table *T = MyTable(V);

    if (spread_WantLimitedHighlighting()) {
	return (V->hasInputFocus);
    }

    if (V->hasInputFocus
	|| V->bufferstatus == BUFFERHASINPUT
	|| V->bufferstatus == BUFFERHASPARAM
	|| (V->anchor.TopRow <= V->anchor.BotRow
	    && V->anchor.LeftCol <= V->anchor.RightCol
	    && table_GetCell(T, V->anchor.TopRow,
			     V->anchor.LeftCol)->celltype
	    == table_ImbeddedObject))
	return 1;
    else
	return 0;
}

/* fix cursors after update */

static void FixCursors(V)
struct spread * V;
{
    struct rectangle vb;

    if (!(V->hasInputFocus))
	spread_RetractCursor(V, V->tableCursor);
    else {
	spread_GetVisualBounds(V, &vb);
	spread_PostCursor(V, &vb, V->tableCursor);
    }
}

/* notify children of full update event */

static void NotifyKids(V, how, updateClipRect)
struct spread * V;
enum view_UpdateType how;
struct rectangle *updateClipRect;
{
    struct table *T = MyTable(V);
    int r, c, x, y;
    int r0, c0, x0, y0;
    struct rectangle bodyClipRect;
    struct rectangle cellBounds;
    struct rectangle cellClipRect;
    struct view *child;
    struct cell *cell;
    int     xth, yth;
    int     rr, cc;

    rectangle_IntersectRect(&bodyClipRect, &bodyClipRect, updateClipRect);
    for (FirstY(V, r0, y0); TooFarUp(T, r0, y0, &bodyClipRect); NextY(V, r0, y0))
	/* skip rows not visible at top */ ;
    for (FirstX(V, c0, x0); TooFarLeft(T, c0, x0, &bodyClipRect); NextX(V, c0, x0))
	/* skip columns not visible to left */ ;

    for (r = r0, y = y0; YinRange(V, T, r, y + spread_SPACING + spread_CELLMARGIN, &bodyClipRect); NextY(V, r, y)) {
	for (c = c0, x = x0; XinRange(T, c, x + spread_SPACING + spread_CELLMARGIN, &bodyClipRect); NextX(V, c, x)) {
	    if (!table_IsJoinedToAnother(T, r, c)) {
		cell = table_GetCell(T, r, c);
		if (cell->celltype == table_ImbeddedObject) {
		    child = spread_FindSubview(V, cell);
		    if (child != 0) {
			for (rr = r + 1, yth = V->rowInfo[r].computedHeight - spread_SPACING - 2 * spread_CELLMARGIN;
			     table_IsJoinedAbove(T, rr, c);
			     NextY(V, rr, yth)) ;
			for (cc = c + 1, xth = table_ColumnWidth(T, c) - 2 * spread_CELLMARGIN;
			     table_IsJoinedToLeft(T, r, cc);
			     NextX(V, cc, xth)) ;
			rectangle_SetRectSize(&cellBounds,
					      x + spread_SPACING + spread_CELLMARGIN, y + spread_SPACING + spread_CELLMARGIN, xth, yth);
			view_InsertView(child, V, &cellBounds);
			rectangle_IntersectRect(&cellClipRect, cellBounds, bodyClipRect);
			view_FullUpdate(child, how,
			  rectangle_Left(&cellClipRect) - rectangle_Left(&cellBounds),
			  rectangle_Top(&cellClipRect) - rectangle_Top(&cellBounds),
			  rectangle_Width(&cellClipRect),
			  rectangle_Height(&cellClipRect));
		    }
		}
	    }
	}
    }
}

/*  redraw when exposed or size changed, etc */

spread_update_FullUpdate(V, how, updateClipRect)
struct spread * V;
enum view_UpdateType how;
struct rectangle *updateClipRect;
{
    struct FontSummary *fs;

    if (V->grayPix == NULL)
	InitializeGraphic(V);
    fontdesc_StringSize (V->writingFont, getDrawable(V), "0", &(V->zeroWidth), NULL);
    fontdesc_StringSize (V->writingFont, getDrawable(V), ".", &(V->dotWidth), NULL);
    fs = fontdesc_FontSummary(V->writingFont, getDrawable(V));
    V->standardHeight = fs->maxHeight;

    V->lastTime = -1;
    switch(how) {

	case view_MoveNoRedraw:
	case view_Remove:
	    NotifyKids(V, how, updateClipRect);
	    FixCursors(V);
	    break;

	default:
	    spread_PartialUpdate(V, view_FullRedraw, updateClipRect);
    }
}


/*  redraw when contents changed */

spread_PartialUpdate(V, how, updateClipRect)
struct spread * V;
enum view_UpdateType how;
struct rectangle *updateClipRect;
{
    struct table *T = MyTable(V);
    int zapped;
    int wanthigh;

    spread_SetClippingRect(V, updateClipRect);
    if (V->lastTime < table_EverythingTimestamp(T)) {
	zapped = 1;
	V->borderDrawn = 2;
	spread_SetTransferMode(V, graphic_COPY);
	spread_EraseRect(V, updateClipRect);
	V->selectionvisible = FALSE;
	V->lastTime = -1;
	ComputeRowSizes(V);
    }
    else
	zapped = 0;

    if (debug) printf("Update, cellChanged=%d, lastTime=%d, requests = %d\n", table_CellsTimestamp(T), V->lastTime, V->updateRequests);

    wanthigh = spread_WantHighlight(V);
    if (wanthigh != V->borderDrawn || V->lastTime < table_EdgesTimestamp(T))
	updateEdges(V, updateClipRect);
    if(V->lastTime < table_CellsTimestamp(T))
	updateCells(V, zapped, how, updateClipRect);
    if (wanthigh != V->borderDrawn)
	updateBorder(V, updateClipRect);
    V->lastTime = table_CurrentTimestamp(T);
    if (wanthigh) {
	if (!(V->selectionvisible))
	    SmashSelection (V);
	V->selectionvisible = TRUE;
    }
    FixCursors(V);
    V->updateRequests = 0;
    if (debug)
	printf("*****\n");
}

spread_InvertRectangle(V, left, top, width, height)
struct spread * V;
int left, top, width, height;
{
    spread_SetTransferMode(V, graphic_INVERT);
    if (width < 0) {
	left += width;
	width = -width;
    }
    if (height < 0) {
	top += height;
	height = -height;
    }
    spread_FillRectSize(V, left, top, width, height, V->blackPix);
}

static SmashSelection (V)
struct spread * V;
{
    int x0, x1, y0, y1;
    int x2, x3, y2, y3;

    if (debug)
	printf("Selection %s\n", (V->selectionvisible) ? "cleared" : "drawn");

    x0 = CtoX (V, V->anchor.LeftCol) + spread_SPACING;
    x1 = CtoX (V, V->anchor.RightCol + 1);
    y0 = RtoY (V, V->anchor.TopRow) + spread_SPACING;
    y1 = RtoY (V, V->anchor.BotRow + 1);
    if (V->anchor.LeftCol < 0)
	x0 = spread_SPACING;
    else if (x0 < spread_BORDER + spread_SPACING)
	x0 = spread_BORDER;
    if (V->anchor.TopRow < 0)
	y0 = spread_SPACING;
    else if (y0 < spread_BORDER + spread_SPACING)
	y0 = spread_BORDER;
    if (x0 > x1) x1 = x0;
    if (y0 > y1) y1 = y0;

    x2 = CtoX (V, V->selection.LeftCol);
    x3 = CtoX (V, V->selection.RightCol + 1) + spread_SPACING;
    y2 = RtoY (V, V->selection.TopRow);
    y3 = RtoY (V, V->selection.BotRow + 1) + spread_SPACING;
    if (V->selection.LeftCol < 0)
	x2 = 0;
    else if (x2 < spread_BORDER)
	x2 = spread_BORDER;
    if (V->selection.TopRow < 0)
	y2 = 0;
    else if (y2 < spread_BORDER)
	y2 = spread_BORDER;
    if (x2 > x3) x3 = x2;
    if (y2 > y3) y3 = y2;

    if (x0 < x1 && y0 < y1) {
	spread_InvertRectangle (V, x2, y2, x3 - x2, y0 - y2);
	spread_InvertRectangle (V, x2, y0, x0 - x2, y1 - y0);
	spread_InvertRectangle (V, x1, y0, x3 - x1, y1 - y0);
	spread_InvertRectangle (V, x2, y1, x3 - x2, y3 - y1);
    } else
	spread_InvertRectangle (V, x2, y2, x3 - x2, y3 - y2);
}

spread_ClearSelectionBox (V)
struct spread * V ;
{
    if(V->selectionvisible) {
	SmashSelection (V);
	V->selectionvisible = FALSE;
    }
}

static Flush(V)
struct spread * V;
{
    spread_FlushGraphics(V);
}

static updateCells(V, zapped, how, updateClipRect)
struct spread * V;
int zapped;
enum view_UpdateType how;
struct rectangle *updateClipRect;
{
    struct table *T = MyTable(V);
    int r, c, x, y;
    int r0, c0, x0, y0;
    struct rectangle bodyClipRect;
    struct rectangle cellBounds;

    spread_ClearSelectionBox(V);
    if (debug)
	printf("Cells updated\n");

    rectangle_SetRectSize(&bodyClipRect, spread_BORDER, spread_BORDER, localWidth(V) - spread_BORDER, localHeight(V) - spread_BORDER);
    rectangle_IntersectRect(&bodyClipRect, &bodyClipRect, updateClipRect);
    for (FirstY(V, r0, y0); TooFarUp(T, r0, y0, &bodyClipRect); NextY(V, r0, y0))
	/* skip rows not visible at top */ ;
    for (FirstX(V, c0, x0); TooFarLeft(T, c0, x0, &bodyClipRect); NextX(V, c0, x0))
	/* skip columns not visible to left */ ;

    for (r = r0, y = y0; YinRange(V, T, r, y + spread_SPACING + spread_CELLMARGIN, &bodyClipRect); NextY(V, r, y)) {
	for (c = c0, x = x0; XinRange(T, c, x + spread_SPACING + spread_CELLMARGIN, &bodyClipRect); NextX(V, c, x)) {
	    if (!table_IsJoinedToAnother(T, r, c)) {
		struct cell * cell = table_GetCell(T, r, c);
		table_ReEval(T, r, c);
		if (V->lastTime < cell->lastcalc) {
		    int     xth, yth;
		    int     rr, cc;
		    for (rr = r + 1, yth = V->rowInfo[r].computedHeight - spread_SPACING - 2 * spread_CELLMARGIN;
			table_IsJoinedAbove(T, rr, c);
			NextY(V, rr, yth)) ;
		    for (cc = c + 1, xth = table_ColumnWidth(T, c) - 2 * spread_CELLMARGIN;
			table_IsJoinedToLeft(T, r, cc);
			NextX(V, cc, xth)) ;
		    rectangle_SetRectSize(&cellBounds,
			x + spread_SPACING + spread_CELLMARGIN, y + spread_SPACING + spread_CELLMARGIN, xth, yth);
		    updateCell(V, cell, zapped, how, &bodyClipRect, &cellBounds);
		}
	    }
	}
    }
    spread_SetClippingRect(V, updateClipRect);
}

static updateString (V, justification, string, cellBounds)
struct spread * V;
char justification;
char *string;
struct rectangle *cellBounds;
{
    if (justification == '\"') { 		/* right */
	spread_MoveTo(V, rectangle_Left(cellBounds) + rectangle_Width(cellBounds) - 1, rectangle_Top(cellBounds));
	spread_DrawString(V, string, graphic_ATTOP | graphic_ATRIGHT);
    }
    else if (justification == '^') {		/* center */
	spread_MoveTo(V, rectangle_Left(cellBounds) + (rectangle_Width(cellBounds) >> 1), rectangle_Top(cellBounds));
	spread_DrawString(V, string, graphic_ATTOP | graphic_BETWEENLEFTANDRIGHT);
    }
    else {					/* left */
	spread_MoveTo(V, rectangle_Left(cellBounds), rectangle_Top(cellBounds));
	spread_DrawString(V, string, graphic_ATTOP | graphic_ATLEFT);
    }
}

static updateValue (V, value, format, precision, cellBounds)
struct spread * V;
extended_double *value;
char format;
int precision;
struct rectangle *cellBounds;
{
    struct table *T = MyTable(V);
    int     x, y;
    double val;
    int decpt, sign;
    int rightshim;
    char    buf[1030], *p;
    int adjustedprecision = precision + 1;

    if (IsBogus(value)) {
	updateString(V, '^', ExtractBogus(value), cellBounds);
	return;
    }
    if (strcmp(fcvt(StandardValue(value), 10, &decpt, &sign), "NAN()") == 0) {
	updateString(V, '^', "ARITH!", cellBounds);
	return;
    }

    switch (format) {
	case PERCENTFORMAT:
	case CURRENCYFORMAT:
	case EXPFORMAT:
	case GENERALFORMAT:
	case FIXEDFORMAT:
	    {
		do {
		    adjustedprecision--;
		    p = buf;
		    if (format == CURRENCYFORMAT)
			*p++ = '$';
		    if (format == PERCENTFORMAT)
			sprintf (p, "%.*f", adjustedprecision, StandardValue(value) * 100);
		    else
			sprintf (p, "%.*f", adjustedprecision, StandardValue(value));
		    while (*p)
			p++;
		    if (format == PERCENTFORMAT) {
			*p++ = '%';
			*p = '\0';
		    }
		    p--;
		    fontdesc_StringSize (V->writingFont, getDrawable(V), buf, &x, &y);
		} while (adjustedprecision > 0 && (x > rectangle_Width(cellBounds)));
		if (format == GENERALFORMAT && precision > 0) {
		    while (p > buf && *p == '0')
			*p-- = ' ';
		}
		if (p > buf && *p == '.')
		    *p-- = ' ';
		if (x > rectangle_Width(cellBounds)) {
		    strcpy (buf, "*");
		    fontdesc_StringSize (V->writingFont, getDrawable(V), buf, &x, &y);
		    adjustedprecision = 0;
		}
		rightshim = ((adjustedprecision > 0 || precision <= 0) ? 0 : V->dotWidth) + (precision - adjustedprecision) * V->zeroWidth;
		if (rightshim > rectangle_Width(cellBounds) - x)
		    rightshim = rectangle_Width(cellBounds) - x;
		spread_MoveTo(V, rectangle_Left(cellBounds) + rectangle_Width(cellBounds) - 1 - rightshim - x, rectangle_Top(cellBounds));
		spread_DrawString(V, buf, graphic_ATTOP | graphic_ATLEFT);
		break;
	    }
	case HORIZONTALBARFORMAT:
	    val = StandardValue(value);
	    if(val > 1.0)
		val = 1.0;
	    if(val < -1.0)
		val = -1.0;
	    if(val < 0.0) {
		int     h = (int) (-val * rectangle_Width(cellBounds) + 0.5);
		spread_FillRectSize(V, rectangle_Left(cellBounds) + rectangle_Width(cellBounds) - h, rectangle_Top(cellBounds), h, rectangle_Height(cellBounds), V->blackPix);
	    }
	    else {
		int     h = (int)(val * rectangle_Width(cellBounds) + 0.5);
		spread_FillRectSize (V, rectangle_Left(cellBounds), rectangle_Top(cellBounds), h, rectangle_Height(cellBounds), V->blackPix);
	    }
	    break;
	case VERTICALBARFORMAT:
	    val = StandardValue(value);
	    if(val > 1.0)
		val = 1.0;
	    if(val < -1.0)
		val = -1.0;
	    if(val < 0.0) {
		int     h = (int) (-val * rectangle_Height(cellBounds) + 0.5);
		spread_FillRectSize (V, rectangle_Left(cellBounds), rectangle_Top(cellBounds), rectangle_Width(cellBounds), h, V->blackPix);
	    }
	    else {
		int     h = (int)(val * rectangle_Height(cellBounds) + 0.5);
		spread_FillRectSize (V, rectangle_Left(cellBounds), rectangle_Top(cellBounds) + rectangle_Height(cellBounds) - h, rectangle_Width(cellBounds), rectangle_Height(cellBounds), V->blackPix);
	    }
	    break;
	case DDMMMYYYYFORMAT: 
	case MMMYYYYFORMAT: 
	case DDMMMFORMAT: 
	    {
		val = StandardValue(value);
		table_FormatDate(T, val, buf, format);
		updateString(V, '^', buf, cellBounds);
		break;
	    }
    }
}

static updateCell(V, cell, zapped, how, bodyClipRect, cellBounds)
struct spread * V;
struct cell * cell;
enum view_UpdateType how;
int     zapped;
struct rectangle *bodyClipRect;
struct rectangle *cellBounds;
{
    struct rectangle cellClipRect;
    struct view *child;

    rectangle_IntersectRect(&cellClipRect, cellBounds, bodyClipRect);
    if (rectangle_Width(&cellClipRect) <= 0 || rectangle_Height(&cellClipRect) <= 0) {
	fprintf(stderr, "Unnecessary attempt to draw cell.\n");
	return;
    }
    spread_SetClippingRect(V, &cellClipRect);

    spread_SetTransferMode(V, graphic_COPY);
    if (!zapped)
	spread_EraseRect(V, &cellClipRect);
    spread_SetFont(V, V->writingFont);

    switch (cell->celltype) {

	case table_EmptyCell:
	    /* do nothing */
	    break;

	case table_TextCell:
	    {
	        char justification, *string;

		string = cell->interior.TextCell.textstring;
		justification = *string;
		if (justification == '\'' || justification == '^' || justification == '\"')
		    string++;
		updateString(V, justification, string, cellBounds);
	    }
	    break;

	case table_ValCell:
	    updateValue (V, &(cell->interior.ValCell.value), cell->format, cell->precision, cellBounds);
	    break;

	case table_ImbeddedObject:
	    if ((child = spread_FindSubview(V, cell))) {
		view_InsertView(child, V, cellBounds);
		view_FullUpdate(child, how, rectangle_Left(&cellClipRect) - rectangle_Left(cellBounds), rectangle_Top(&cellClipRect) - rectangle_Top(cellBounds), rectangle_Width(&cellClipRect), rectangle_Height(&cellClipRect));
	    }
	    break;
    }
}

static updateEdges(V, updateClipRect)
struct spread * V;
struct rectangle *updateClipRect;
{
    struct table *T = MyTable(V);
    int     x, y, r, c, k;
    int t = ((spread_SPACING - spread_LINEWIDTH)/2);
    struct rectangle edgeClipRect;

    spread_ClearSelectionBox(V);
    if (debug)
	printf("Edges updated\n");

    rectangle_SetRectSize(&edgeClipRect, spread_BORDER, spread_BORDER, localWidth(V) - spread_BORDER, localHeight(V) - spread_BORDER);
    rectangle_IntersectRect(&edgeClipRect, &edgeClipRect, updateClipRect);
    spread_SetClippingRect(V, &edgeClipRect);
    spread_SetTransferMode(V, graphic_COPY);

/* 
    Because vertical and horizontal lines intersect, we must first erase all
    old edges and then draw new edges.  To minimize window manager calls, we
    erase full-length lines and draw the longest possible line segments.
    These may not be the full grid length, even while erasing, for we can
    not erase across a joined cell.

 */    /* Erasing phase */

    for (FirstY(V, r, y); YinRange(V, T, (r-1), y, &edgeClipRect); NextY(V, r, y)) {
	for (FirstX(V, c, x); XinRange(T, (c-1), x, &edgeClipRect); NextX(V, c, x)) {
	    int     w;
	    
	    /* Erase horizontal line beginning above this cell. */

	    if (c < table_NumberOfColumns(T)) {
		if ((c <= 0 || table_IsJoinedAbove(T, r, c - 1)) && !table_IsJoinedAbove(T, r, c)) {
		    for (k = c, w = x;
		      XinRange(T, k, w, &edgeClipRect) && !table_IsJoinedAbove(T, r, k);
		      NextX(V, k, w)) ;
		    spread_EraseRectSize(V, x + t, y + t, w - x + spread_LINEWIDTH, spread_LINEWIDTH);
		}
	    }
	    
	    /* Erase vertical line beginning to left of this cell. */

	    if (r < table_NumberOfRows(T)) {
		if ((r <= 0 || table_IsJoinedToLeft(T, r - 1, c)) && !table_IsJoinedToLeft(T, r, c)) { 
		    for (k = r, w = y;
		      YinRange(V, T, k, w, &edgeClipRect) && !table_IsJoinedToLeft(T, k, c);
		      NextY(V, k, w)) ;
		    spread_EraseRectSize(V, x + t, y + t, spread_LINEWIDTH, w - y + spread_LINEWIDTH);
		}
	    }
	}
    }

    /* Drawing phase.  The line drawn may be black or dotted. */

    for (FirstY(V, r, y); YinRange(V, T, (r-1), y, &edgeClipRect); NextY(V, r, y)) {
	for (FirstX(V, c, x); XinRange(T, (c-1), x, &edgeClipRect); NextX(V, c, x)) {
	    int     w, color;
	    
	    /* Draw horizontal line beginning above this cell. */

	    if (c < table_NumberOfColumns(T)) {
		color = table_ColorAbove(T, r, c);
		if ((spread_WantHighlight(V) || color == BLACK)
		  && (c <= 0 || color != table_ColorAbove(T, r, c - 1))
		  && color >= GHOST) {
		    for (k = c, w =  x;
		      XinRange(T, k, w, &edgeClipRect) && color == table_ColorAbove(T, r, k);
		      NextX(V, k, w)) ;
		    spread_FillRectSize(V, x + t, y + t, w - x + spread_LINEWIDTH, spread_LINEWIDTH, color == GHOST ? V->grayPix : V->blackPix);
		}
	    }
	    
	    /* Draw vertical line beginning to left of this cell. */

	    if (r < table_NumberOfRows(T)) {
		color = table_ColorToLeft(T, r, c);
		if ((spread_WantHighlight(V) || color == BLACK)
		  && (r <= 0 || color != table_ColorToLeft(T, r - 1, c)) && color >= GHOST) { 
		    for (k = r, w = y;
		      YinRange(V, T, k, w, &edgeClipRect) && color == table_ColorToLeft(T, k, c);
		      NextY(V, k, w)) ;
		    spread_FillRectSize(V, x + t, y + t, spread_LINEWIDTH, w - y + spread_LINEWIDTH, color == GHOST ? V->grayPix : V->blackPix);
		}
	    }
	}
    }
    spread_SetClippingRect(V, updateClipRect);
}

#define	spread_BORDERMARGIN 2	/* white space from table edge to bordering label */

static updateBorder(V, updateClipRect)
struct spread * V;
struct rectangle *updateClipRect;
{
    struct table *T = MyTable(V);
    int     r, c, x, y;
    char    buff[6];
    struct rectangle borderClipRect;

    spread_ClearSelectionBox(V);
    if (debug)
	printf("Border updated\n");

    V->borderDrawn = spread_WantHighlight(V);
    spread_SetFont(V, V->writingFont);

    spread_SetTransferMode(V, graphic_COPY);
    spread_EraseRectSize(V, 0, spread_BORDER, spread_BORDER, localHeight(V) - spread_BORDER);
    spread_EraseRectSize(V, spread_BORDER, 0, localWidth(V) - spread_BORDER, spread_BORDER);

    if (V->borderDrawn) {

	/* border labels down left side */

	rectangle_SetRectSize (&borderClipRect, 0, spread_BORDER, spread_BORDER, localHeight(V) - spread_BORDER);
	rectangle_IntersectRect(&borderClipRect, &borderClipRect, updateClipRect);
	spread_SetClippingRect(V, &borderClipRect);
	for (FirstY(V, r, y); TooFarUp(T, r, y, &borderClipRect); NextY(V, r, y))
	    /* skip rows */ ;
	for (; YinRange(V, T, r, y + spread_SPACING + spread_CELLMARGIN, &borderClipRect); NextY(V, r, y)) {
	    sprintf (buff, "%d", r + 1);
	    spread_MoveTo(V, spread_BORDER - spread_BORDERMARGIN, y + spread_SPACING + spread_CELLMARGIN);
	    spread_DrawString(V, buff, graphic_ATTOP | graphic_ATRIGHT);
	}

	/* border labels across top */

	rectangle_SetRectSize (&borderClipRect, spread_BORDER, 0, localWidth(V) - spread_BORDER, spread_BORDER);
	rectangle_IntersectRect(&borderClipRect, &borderClipRect, updateClipRect);
	spread_SetClippingRect(V, &borderClipRect);
	for (FirstX(V, c, x); TooFarLeft(T, c, x, &borderClipRect); NextX(V, c, x))
	    /* skip columns */ ;
	for (; XinRange(T, c, x + spread_SPACING + spread_CELLMARGIN, &borderClipRect); NextX(V, c, x)) {
	    sprintf (buff, "%d", c + 1);
	    spread_MoveTo(V, x +(table_ColumnWidth(T, c) / 2), (spread_BORDER - spread_BORDERMARGIN));
	    spread_DrawString(V, buff, graphic_ATBASELINE | graphic_BETWEENLEFTANDRIGHT);
	}

    }
    spread_SetClippingRect(V, updateClipRect);
}
