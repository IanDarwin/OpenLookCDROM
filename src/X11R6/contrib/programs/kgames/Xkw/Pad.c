/*
 * $NCD$
 *
 * Copyright 1992 Network Computing Devices
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of NCD. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  NCD. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * NCD. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NCD.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, Network Computing Devices
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include "PadP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(PadRec, pad.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    { XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
      offset (font), XtRString, XtDefaultFont },
    { XtNforeground, XtCForeground, XtRPixel, sizeof (unsigned long),
      offset (foreground_pixel), XtRString, XtDefaultForeground },
    { XtNnumRows, XtCNumRows, XtRDimension, sizeof (Dimension),
      offset (rows), XtRImmediate, (XtPointer) 1},
    { XtNnumCols, XtCNumCols, XtRDimension, sizeof (Dimension),
      offset (cols), XtRImmediate, (XtPointer) 1},
    {XtNinternalBorderWidth, XtCInternalBorderWidth, XtRDimension, sizeof (Dimension),
     offset(internal_border), XtRImmediate, (XtPointer) 2},
#undef offset
};

static void	Initialize(), Destroy (), Redisplay (), Resize ();
static Boolean	SetValues ();

PadClassRec padClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"Pad",
    /* widget_size		*/	sizeof(PadRec),
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
    /* compress_exposure	*/	XtExposeCompressSeries|XtExposeGraphicsExpose|XtExposeNoExpose,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
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
  { /* pad fields */
    /* empty			*/	0
  }
};

WidgetClass padWidgetClass = (WidgetClass)&padClassRec;

static void
Clear (l, n)
    PadLinePtr	l;
{
    char	*t, *a;
    
    t = l->text;
    a = l->attr;
    while (n--) {
	*t++ = ' ';
	*a++ = XkwPadNormal;
    }
    *t = '\0';
    l->serial = 0;
}

static void
ResizeBuffer (bp, old_rows, old_cols, new_rows, new_cols)
    PadLinePtr	*bp;
    Dimension	old_rows, old_cols, new_rows, new_cols;
{
    PadLinePtr	oldb;
    PadLinePtr	b;
    int		row, col;
    int		max_row, max_col;

    b = Some(PadLineRec, new_rows);
    for (row = 0; row < new_rows; row++)
	AllocText (&b[row], new_cols);
    oldb = *bp;
    if (oldb)
    {
	max_col = new_cols;
	if (max_col > old_cols)
	    max_col = old_cols;
	max_row = new_rows;
	if (max_row > old_rows)
	    max_row = old_rows;
	for (row = 0; row < max_row; row++)
	{
	    CopyText (&oldb[row], &b[row], 0, max_col);
	    b[row].serial = oldb[row].serial;
	    b[row].id = oldb[row].id;
	}
	for (; row < old_rows; row++)
	    DisposeText (&oldb[row]);
	Dispose (oldb);
    }
    *bp = b;
}

static void
ResizeText (w, rows, cols)
    PadWidget	w;
    Dimension	rows, cols;
{
    int	    row;
    
    ResizeBuffer (&w->pad.is, w->pad.rows, w->pad.cols, rows, cols);
    ResizeBuffer (&w->pad.want, w->pad.rows, w->pad.cols, rows, cols);
    w->pad.rows = rows;
    w->pad.cols = cols;
    for (row = 0; row < rows; row++)
	w->pad.is[row].id = w->pad.want[row].id = NextSerial (w);
}

static void
getSize (w, widthp, heightp)
    PadWidget    w;
    Dimension	    *widthp, *heightp;
{
    int	size;
    unsigned long   value, value2;
    Atom	UNDERLINE_POSITION, UNDERLINE_THICKNESS;
    int		dir, font_ascent, font_descent;
    XCharStruct	overall;

    w->pad.fixed_width = (w->pad.font->max_bounds.width == 
			 w->pad.font->min_bounds.width);
    if (XGetFontProperty (w->pad.font, XA_UNDERLINE_POSITION, &value))
	w->pad.underline_pos = (int) value;
    else
	w->pad.underline_pos = (w->pad.font->max_bounds.descent + 1) >> 1;
    
    if (XGetFontProperty (w->pad.font, XA_UNDERLINE_THICKNESS, &value))
	w->pad.underline_thickness = (int) value;
    else if (XGetFontProperty (w->pad.font, XA_WEIGHT, &value) &&
	     XGetFontProperty (w->pad.font, XInternAtom (XtDisplay (w),
							 "CAP_HEIGHT",
							 False), &value2))
    {
	w->pad.underline_thickness = (int) value * value2 / 1000;
    }
    else
    {
	/* this is not quite right -- the thickness of an underscore
	 * may not be what we want, but it should work OK 
	 */
	XTextExtents (w->pad.font, "_", 1, &dir, &font_ascent, &font_descent,
		      &overall);
	size = overall.ascent + overall.descent;
	if (size < (font_ascent + font_descent) / 2)
	    w->pad.underline_thickness = size;
	else
	    w->pad.underline_thickness = (font_ascent + font_descent) / 2;
    }
    if (w->pad.underline_thickness <= 0)
	w->pad.underline_thickness = 1;
    if (w->pad.underline_pos + w->pad.underline_thickness > w->pad.font->descent)
    {
	if (w->pad.underline_pos > 1)
	    w->pad.underline_pos = w->pad.font->descent - w->pad.underline_thickness;
	if (w->pad.underline_pos <= 1)
	{
	    if (w->pad.underline_pos < 0)
		w->pad.underline_pos = 0;
	    w->pad.underline_thickness = w->pad.font->descent - w->pad.underline_pos;
	}
    }
    w->pad.char_width = XTextWidth (w->pad.font, "0", 1);
    w->pad.char_height = w->pad.font->ascent + w->pad.font->descent;
    w->pad.char_vAdjust = w->pad.font->ascent;
    w->pad.char_hAdjust = 0;
    *widthp = w->pad.char_width * w->pad.cols + 2 * w->pad.internal_border;
    *heightp = w->pad.char_height * w->pad.rows + 2 * w->pad.internal_border;
}

static void
setSize (w)
    PadWidget	w;
{
    Dimension	rows, cols;

    rows = (w->core.height - 2 * w->pad.internal_border) / w->pad.char_height;
    cols = (w->core.width - 2 * w->pad.internal_border) / w->pad.char_width;
    ResizeText (w, rows, cols);
}

static void Initialize (greq, gnew)
    Widget  greq, gnew;
{
    PadWidget	req = (PadWidget) greq,
		new = (PadWidget) gnew;
    XGCValues	gcv;

    getSize (new, &new->core.width, &new->core.height);
    gcv.foreground = new->pad.foreground_pixel;
    gcv.background = new->core.background_pixel;
    gcv.font = new->pad.font->fid;
    new->pad.normal_gc = XtGetGC (gnew, GCForeground|GCBackground|GCFont, &gcv);
    gcv.foreground = new->core.background_pixel;
    gcv.background = new->pad.foreground_pixel;
    gcv.font = new->pad.font->fid;
    new->pad.inverse_gc = XtGetGC (gnew, GCForeground|GCBackground|GCFont, &gcv);
    new->pad.is = 0;
    new->pad.want = 0;
    new->pad.serial = 0;
    new->pad.copy = 0;
    ResizeText (new, new->pad.rows, new->pad.cols);
}

static void
Destroy (gw)
    Widget  gw;
{
    PadWidget    w = (PadWidget) gw;

    XtReleaseGC (gw, w->pad.normal_gc);
    XtReleaseGC (gw, w->pad.inverse_gc);
}

static int
XToCol(w, row, x)
    PadWidget	w;
    int		row;
    int		x;
{
    char    *c;
    int	    col;
    
    c = w->pad.is[row].text;
    for (col = 0; col < w->pad.cols - 1; col++)
	if (x < XTextWidth (w->pad.font, c, col))
	    break;
    return col;
}

static void
DrawText (w, row, start_col, end_col)
    PadWidget	w;
{
    GC		gc;
    Display	*dpy = XtDisplay(w);
    Window	win = XtWindow (w);
    int		change_col;
    char	attr;
    char	*is_a, *is_t;
    PadLinePtr	is;
    int		x, y;
    int		width;
    
    is = &w->pad.is[row];
    is_a = is->attr + start_col;
    is_t = is->text + start_col;
    change_col = start_col;
    x = TextX (w, start_col);
    y = TextY (w, row);
    while (start_col < end_col) {
	attr = *is_a;
	do {
	    ++is_a;
	    ++change_col;
	} while (change_col < end_col && *is_a == attr);
	gc = w->pad.normal_gc;
	if (attr & XkwPadInverse)
	    gc = w->pad.inverse_gc;
	XDrawImageString (dpy, win, gc, x, y,
			  is_t, change_col - start_col);
	if (w->pad.fixed_width)
	    width = (change_col - start_col) * w->pad.char_width;
	else
	    width = XTextWidth (w->pad.font, is_t, change_col - start_col);
	if (attr & XkwPadUnderline)
	{
	    XFillRectangle (dpy, win, gc, 
			    x, y + w->pad.underline_pos,
			    width, w->pad.underline_thickness);
	}
	x += width;
	is_t += (change_col - start_col);
	start_col = change_col;
    }
}

static void
RedrawText (w, row, start_col, end_col)
    PadWidget	w;
{
    char    *t, *a;
    
    t = w->pad.is[row].text + start_col;
    a = w->pad.is[row].attr + start_col;
    while (start_col < end_col && *t++ == ' ' && *a++ == XkwPadNormal)
	start_col++;
    t = w->pad.is[row].text + end_col;
    a = w->pad.is[row].attr + end_col;
    while (end_col > start_col && *--t == ' ' && *--a == XkwPadNormal)
	end_col--;
    if (start_col < end_col)
	DrawText (w, row, start_col, end_col);
}

static int
UntilEqual(w, start)
    PadWidget	w;
    int		start;
{
    PadLinePtr	want = &w->pad.want[start],
		is = &w->pad.is[start];

    while (start < w->pad.rows && want->id != is->id) {
	want++;
	is++;
	start++;
    }

    return start;
}

static void
CopyLines (w, top, bottom, count)
    PadWidget	w;
    int		top, bottom, count;
{
    int		src, dst, amt;
    PadCopyPtr	copy, *prev;
    
    if (count < 0)
    {
	src = top - count;
	dst = top;
	count = -count;
    }
    else
    {
	src = top;
	dst = top + count;
    }
    amt = bottom - top - count;
    copy = New(PadCopyRec);
    copy->next = 0;
    copy->src = src;
    copy->dst = dst;
    copy->amt = amt;
    copy->copy_serial = NextRequest (XtDisplay (w));
    prev = &w->pad.copy;
    while (*prev)
	prev = &(*prev)->next;
    *prev = copy;
    XCopyArea (XtDisplay (w), XtWindow (w), XtWindow (w), w->pad.normal_gc,
	       0, YPos(w, src), w->core.width, amt * w->pad.char_height,
	       0, YPos(w, dst));
}

static void
ClearLines (w, start, amt)
    PadWidget	w;
    int		start, amt;
{
    XClearArea (XtDisplay (w), XtWindow (w),
		0, YPos (w, start), w->core.width, amt * w->pad.char_height,
		False);
}

static void
ScrollBuffer (w, b, start_row, end_row, dist)
    PadWidget	    w;
    PadLinePtr	    b;
    int		    start_row, end_row, dist;
{
    int		    first_row, row, next_row;
    PadLineRec	    tmp1, tmp2;
    int		    n;

    if (end_row <= start_row)
	return;
    n = end_row - start_row;
    first_row = start_row;
    while (n) {
	tmp2 = b[first_row];
	row = first_row;
	do 
	{
	    next_row = row + dist;
	    if (next_row < start_row)
		next_row = next_row + end_row - start_row;
	    else if (next_row >= end_row)
		next_row = next_row - end_row + start_row;
	    tmp1 = b[next_row];
	    b[next_row] = tmp2;
	    tmp2 = tmp1;
	    n--;
	    row = next_row;
	} while (row != first_row);
	first_row++;
    }
    n = dist;
    if (n < 0)
    {
	n = -n;
	row = end_row - n;
    }
    else
	row = start_row;
    while (n--)
    {
	Clear (&b[row], w->pad.cols);
	row++;
    }
}

static Boolean
AddLines(w, at, num)
    PadWidget	w;
    int		at, num;
{
    int	i;
    int	bottom = UntilEqual(w, at + num);

    if (num == 0 || num >= ((bottom - 1) - at))
	return False;	/* We did nothing */

    CopyLines (w, at, bottom, num);
    ClearLines (w, at, num);
    ScrollBuffer (w, w->pad.is, at, bottom, num);
    
    return True;	/* We did something. */
}

    
static Boolean
DelLines(w, at, num)
    PadWidget	w;
    int		at, num;
{
    register int	i;
    int	bottom = UntilEqual(w, at + num);

    if (num == 0 || num >= ((bottom - 1) - at))
	return False;

    CopyLines (w, at, bottom, -num);
    ClearLines (w, bottom - num, num);
    ScrollBuffer (w, w->pad.is, at, bottom, -num);
    
    return True;
}

static void
DoInsertDelete (w, start)
    PadWidget	w;
{
    PadLinePtr  is, want;
    int		i, j;

    /* Some changes have been made.  Try for insert or delete lines.
    If either case has happened, Addlines and/or DelLines will do
    necessary scrolling, also CONVERTING w->pad.is to account for the
    physical changes.  The comparison continues from where the
    insertion/deletion takes place; this doesn't happen very often,
    usually it happens with more than one window with the same
    buffer. */

    is = &w->pad.is[start];
    want = &w->pad.want[start];
    for (i = start; i < w->pad.rows; i++, is++, want++)
	if (want->id != is->id)
	    break;

    for (; i < w->pad.rows; i++) {
	for (j = i + 1; j < w->pad.rows; j++) {
	    want = &w->pad.want[j];
	    is = &w->pad.is[j];
	    if (want->id == is->id)
		break;
	    if (want->id == w->pad.is[i].id) {
		if (AddLines(w, i, j - i)) {
		    DoInsertDelete(w, j);
		    return;
		}
		break;
	    }
	    if ((want = &w->pad.want[i])->id == is->id) {
		if (DelLines(w, i, j - i)) {
		    DoInsertDelete(w, i);
		    return;
		}
		break;
	    }
	}
    }
}

/*
 * Redisplay -- repaint damaged areas on the screen
 *  This is complicated by the CopyArea calls which may have
 *  copied exposed regions on the screen
 */

static void
Redisplay (gw, event, region)
    Widget  gw;
    XEvent  *event;
    Region  region;
{
    PadWidget   w = (PadWidget) gw;
    int		start_row, end_row, row;
    int		start_col, end_col;
    PadCopyPtr  copy;
    unsigned long   expose_serial;
    Boolean	*repaint;
    int		amt;
    Boolean	*r;

    if (!XtIsRealized (gw))
	return;
    if (event->type != NoExpose)
    {
	
	/* Mark rows for redisplay */
	repaint = Some (Boolean, w->pad.rows);
	for (row = 0; row < w->pad.rows; row++) repaint[row] = False;
	start_row = RowPos (w, event->xexpose.y);
	if (start_row < 0)
	    start_row = 0;
	end_row = RowPos (w, event->xexpose.y + event->xexpose.height - 1);
	if (end_row >= w->pad.rows)
	    end_row = w->pad.rows - 1;
	for (row = start_row; row <= end_row; row++)
	    repaint[row] = True;
	
	/* Track effects of CopyArea calls on exposure events */
	copy = w->pad.copy;
	expose_serial = event->xexpose.serial;
	for (copy = w->pad.copy; copy; copy = copy->next)
	    if (copy->copy_serial > expose_serial)
		break;
	for (; copy; copy = copy->next)
	{
	    bcopy (repaint + copy->src, repaint + copy->dst, copy->amt * sizeof (Boolean));
	    r = repaint;
	    if (copy->src > copy->dst)
		r += copy->dst + copy->amt;
	    else
		r += copy->src;
	    amt = copy->amt;
	    while (amt--)
		*r++ = False;
	}

	/* repaint the resultant rows */
	if (w->pad.fixed_width)
	{
	    start_col = ColPos (w, event->xexpose.x);
	    if (start_col < 0)
		start_col = 0;
	    end_col = ColPos (w, event->xexpose.x + event->xexpose.width - 1);
	    if (end_col >= w->pad.cols)
		end_col = w->pad.cols - 1;
	}
	for (row = 0; row < w->pad.rows; row++)
	{
	    if (!repaint[row])
		continue;
	    if (!w->pad.fixed_width)
	    {
		start_col = XToCol (w, row, event->xexpose.x);
		end_col = XToCol (w, row, event->xexpose.x + event->xexpose.width - 1);
	    }
	    RedrawText (w, row, start_col, end_col + 1);
	}
	Dispose (repaint);
	if (event->xexpose.count != 0)
	    return;
    }
    if (event->type != Expose)
    {
	copy = w->pad.copy;
	if (!copy)
	    abort ();
	if (event->xexpose.serial != copy->copy_serial)
	    abort ();
	w->pad.copy = copy->next;
	Dispose (copy);
    }
}

static void
Resize (gw)
    Widget  gw;
{
    PadWidget   w = (PadWidget) gw;

    setSize (w);
}

void
XkwPadUpdate (gw)
    Widget  gw;
{
    Display	*dpy = XtDisplay (gw);
    Window	win = XtWindow (gw);
    PadWidget	w = (PadWidget) gw;
    int		row, start_col, end_col;
    int		cols;
    PadLinePtr	is, want;
    char	*is_t, *want_t, *is_a, *want_a;
    Boolean	DoneInsertDelete = False;

    if (!XtIsRealized (gw))
	return;
    is = w->pad.is;
    want = w->pad.want;
    cols = w->pad.cols;
    for (row = 0; row < w->pad.rows; row++, is++, want++)
    {
	if (is->serial == want->serial)
	    continue;
	
	if (is->id != want->id && !DoneInsertDelete)
	{
	    DoInsertDelete (w, row);
	    DoneInsertDelete = True;
	}
	/* 
	 * This is simplistic -- painting a single range of
	 * text which covers all the changed characters
	 */

	/* search for start of mismatching characters */
	is_t = is->text;
	is_a = is->attr;

	want_t = want->text;
	want_a = want->attr;
	for (start_col = 0; start_col < cols; start_col++)
	    if (*is_t++ != *want_t++ || *is_a++ != *want_a++)
		break;
	
	/* search for end of mismatching characters */
	is_t = is->text + cols;
	is_a = is->attr + cols;

	want_t = want->text + cols;
	want_a = want->attr + cols;
	for (end_col = cols; end_col > start_col; end_col--)
	    if (*--is_t != *--want_t || *--is_a != *--want_a)
		break;

	/* paint the range of mismatching characters */
	if (start_col < end_col)
	{
	    CopyText (want, is, start_col, end_col - start_col);
	    if (!w->pad.fixed_width)
	    {
		start_col = 0;
		end_col = w->pad.cols;
		XClearArea (dpy, win, 0, YPos(w, row), 
			    w->core.width, w->pad.char_height, False);
	    }
	    DrawText (w, row, start_col, end_col);
	}
	is->serial = want->serial;
	is->id = want->id;
    }
}

void
XkwPadText (gw, row, col, text, len)
    Widget  gw;
    int	    row, col;
    char    *text;
    int	    len;
{
    PadWidget	    w = (PadWidget) gw;
    PadLinePtr	    want;

    if (row >= w->pad.rows || col >= w->pad.cols)
	return;
    want = &w->pad.want[row];
    if (col + len > w->pad.cols)
	len = w->pad.cols - col;
    bcopy (text, want->text + col, len);
    want->serial = NextSerial(w);
}

XkwPadAttributes (gw, row, col, attr, len)
    Widget  gw;
    int	    row, col;
    char    *attr;
    int	    len;
{
    PadWidget	    w = (PadWidget) gw;
    PadLinePtr	    want;
    
    if (row >= w->pad.rows || col >= w->pad.cols)
	return;
    want = &w->pad.want[row];
    if (col + len > w->pad.cols)
	len = w->pad.cols - col;
    bcopy (attr, want->attr + col, len);
    want->serial = NextSerial(w);
}

XkwPadTextAndAttributes (gw, row, col, text, attr, len)
    Widget  gw;
    int	    row, col;
    char    *text, *attr;
    int	    len;
{
    PadWidget	    w = (PadWidget) gw;
    PadLinePtr	    want;
    
    if (row >= w->pad.rows || col >= w->pad.cols)
	return;
    want = &w->pad.want[row];
    if (col + len > w->pad.cols)
	len = w->pad.cols - col;
    bcopy (text, want->text + col, len);
    bcopy (attr, want->attr + col, len);
    want->serial = NextSerial(w);
}
    
void
XkwPadClearToEnd (gw, row, col)
    Widget  gw;
    int	    row, col;
{
    PadWidget	    w = (PadWidget) gw;
    PadLinePtr	    want;
    char	    *t, *a;

    if (row >= w->pad.rows || col >= w->pad.cols)
	return;
    want = &w->pad.want[row];
    t = want->text + col;
    a = want->attr + col;
    col = w->pad.cols - col;
    while (col--) 
    {
	*t++ = ' ';
	*a++ = XkwPadNormal;
    }
    want->serial = NextSerial (w);
}
    
void
XkwPadClear (gw)
    Widget  gw;
{
    PadWidget	    w = (PadWidget) gw;
    int		    row;

    for (row = 0; row < w->pad.rows; row++)
	XkwPadClearToEnd (gw, row, 0);
}

void
XkwPadScroll (gw, start_row, end_row, dist)
    Widget  gw;
    int	    start_row, end_row, dist;
{
    PadWidget	    w = (PadWidget) gw;

    ScrollBuffer (w, w->pad.want, start_row, end_row, dist);
}

void
XkwPadXYToRowCol (gw, x, y, rowp, colp)
    Widget  gw;
    int	    x, y;
    int	    *rowp, *colp;
{
    PadWidget	    w = (PadWidget) gw;
    int		    row, col;
    
    row = RowPos (w, y);
    if (row < 0)
	row = 0;
    if (row >= w->pad.rows)
	row = w->pad.rows - 1;
    if (w->pad.fixed_width)
	col = ColPos (w, x);
    else
	col = XToCol (w, row, x);
    if (col < 0)
	col = 0;
    if (col >= w->pad.cols)
	col = w->pad.cols - 1;
    *rowp = row;
    *colp = col;
}

static Boolean
SetValues (gcur, greq, gnew)
    Widget  gcur, greq, gnew;
{
    PadWidget	    cur = (PadWidget) gcur,
		    req = (PadWidget) greq,
		    new = (PadWidget) gnew;
    XGCValues	    gcv;
    Boolean	    redraw = FALSE, newgc = FALSE, newsize = FALSE;
    Dimension	    width, height;

    if (req->pad.foreground_pixel != cur->pad.foreground_pixel)
	newgc = TRUE;
    if (req->pad.font != cur->pad.font)
	newgc = newsize = TRUE;
    if (req->pad.rows != cur->pad.rows ||
	req->pad.cols != cur->pad.cols)
    {
	newsize = TRUE;
	ResizeText (new, req->pad.rows, req->pad.cols);
    }
    if (newgc)
    {
	XtReleaseGC (gcur, cur->pad.normal_gc);
	gcv.foreground = req->pad.foreground_pixel;
	gcv.font = req->pad.font->fid;
	new->pad.normal_gc = XtGetGC (gnew, GCForeground|GCFont, &gcv);
	redraw = TRUE;
    }
    if (newsize)
    {
	getSize (new, &width, &height);
	XtMakeResizeRequest (gnew, width, height, &width, &height);
	if (width != cur->core.width || height != cur->core.height)
	    setSize (new, width, height);
	redraw = TRUE;
    }
    return redraw;
}
