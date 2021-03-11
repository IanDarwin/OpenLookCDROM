/* $XConsortium: Thermo.c,v 1.4 91/02/17 16:18:42 converse Exp $ */

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
#include "ThermoP.h"

static XtResource resources[] = {
#define offset(field) XtOffsetOf(ThermoRec, thermo.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
    { XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
      offset (font), XtRString, XtDefaultFont },
    { XtNmercuryColor, XtCForeground, XtRPixel, sizeof (unsigned long),
      offset (mercuryColor), XtRString, XtDefaultForeground },
    { XtNtextColor, XtCForeground, XtRPixel, sizeof (unsigned long),
      offset (textColor), XtRString, XtDefaultForeground },
    { XtNtickColor, XtCForeground, XtRPixel, sizeof (unsigned long),
      offset (tickColor), XtRString, XtDefaultForeground },
    { XtNthickness, XtCThickness, XtRInt, sizeof (int),
      offset (reqThickness), XtRImmediate, (XtPointer) ThermoUnspecified },
    { XtNminimum, XtCMinimum, XtRInt, sizeof (int), 
      offset (minimum), XtRImmediate, (XtPointer) 0},
    { XtNmaximum, XtCMaximum, XtRInt, sizeof (int), 
      offset (maximum), XtRImmediate, (XtPointer) 0},
    { XtNcurrent, XtCCurrent, XtRInt, sizeof (int), 
      offset (current), XtRImmediate, (XtPointer) 0},
    { XtNminorStart, XtCMinorStart, XtRInt, sizeof (int), 
      offset (reqMinorStart), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNmajorStart, XtCMajorStart, XtRInt, sizeof (int), 
      offset (reqMajorStart), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNminorStep, XtCMinorStep, XtRInt, sizeof (int), 
      offset (reqMinorStep), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNmajorStep, XtCMajorStep, XtRInt, sizeof (int), 
      offset (reqMajorStep), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNstartPad, XtCStartPad, XtRDimension, sizeof (Dimension),
      offset (reqStartPad), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNendPad, XtCEndPad, XtRDimension, sizeof (Dimension),
      offset (reqEndPad), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNmajorTickLen, XtCStartPad, XtRDimension, sizeof (Dimension),
      offset (reqMajorTickLen), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNminorTickLen, XtCMinorTickLen, XtRDimension, sizeof (Dimension),
      offset (reqMinorTickLen), XtRImmediate, (XtPointer) ThermoUnspecified},
    { XtNvertical, XtCVertical, XtRBoolean, sizeof (Boolean),
      offset (vertical), XtRImmediate, (XtPointer) FALSE},
#undef offset
};

static void	Initialize(), Destroy (), Redisplay ();
static Boolean	SetValues ();

ThermoClassRec thermoClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"Thermo",
    /* widget_size		*/	sizeof(ThermoRec),
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
  { /* thermo fields */
    /* empty			*/	0
  }
};

WidgetClass thermoWidgetClass = (WidgetClass)&thermoClassRec;

static int
NumberLength (n)
    int	n;
{
    int	l;
    if (n < 0)
	return NumberLength (-n) + 1;
    l = 1;
    while (n >= 10)
    {
	n /= 10;
	l++;
    }
    return l;
}

static void
getSize (w, widthp, heightp)
    ThermoWidget    w;
    Dimension	    *widthp, *heightp;
{
    int	size;

    if (w->thermo.vertical)
    {
	size = w->thermo.textWidth;
	if (w->thermo.majorTickLen > size)
	    size = w->thermo.majorTickLen;
	if (w->thermo.minorTickLen > size)
	    size = w->thermo.minorTickLen;
	*widthp = size + w->thermo.thickness;
    }
    else
    {
	size = w->thermo.font->ascent + w->thermo.font->descent;
	size += w->thermo.majorTickLen;
	if (w->thermo.minorTickLen > size)
	    size = w->thermo.minorTickLen;
	*heightp = size + w->thermo.thickness;
    }
}

static int
NiceValue (num, den)
    int	num, den;
{
    int	v;
    int	l;

    v = num / den;
    if (v == 0)
	return 1;
    if (v < 5)
	return v;
    if (v < 8)
	return 5;
    l = 1;
    while (v > 100)
    {
	v /= 10;
	l++;
    }
    if (v < 15)
	v = 10;
    else if (v < 37)
	v = 25;
    else if (v < 75)
	v = 50;
    else
	v = 100;
    while (l--)
	v *= 10;
    return v;
}

static void
setDefaults (req, new)
    ThermoWidget    req, new;
{
    int	minTextLen, maxTextLen;

    minTextLen = NumberLength (req->thermo.minimum);
    maxTextLen = NumberLength (req->thermo.maximum);
    if (minTextLen > maxTextLen)
	maxTextLen = minTextLen;
    new->thermo.textWidth = XTextWidth (req->thermo.font, "0", 1) * maxTextLen;
    if (req->thermo.reqThickness == ThermoUnspecified)
	new->thermo.thickness = (req->thermo.font->ascent +
				 req->thermo.font->descent) * 2;
    else
	new->thermo.thickness = req->thermo.reqThickness;
    if (req->thermo.reqMinorStart == ThermoUnspecified)
	new->thermo.minorStart = req->thermo.minimum;
    else
	new->thermo.minorStart = req->thermo.reqMinorStart;
    if (req->thermo.majorStart == ThermoUnspecified)
	new->thermo.majorStart = req->thermo.minimum;
    else
	new->thermo.majorStart = req->thermo.reqMajorStart;
    if (req->thermo.reqMajorStep == ThermoUnspecified)
	new->thermo.majorStep = NiceValue (req->thermo.maximum - req->thermo.majorStart, 10);
    else
	new->thermo.majorStep = req->thermo.reqMajorStep;
    if (req->thermo.reqMinorStep == ThermoUnspecified)
	new->thermo.minorStep = NiceValue (new->thermo.majorStep, 4);
    else
	new->thermo.minorStep = req->thermo.reqMinorStep;
    if (req->thermo.reqStartPad == ThermoUnspecified) {
	if (new->thermo.vertical)
	    new->thermo.startPad = 0;
	else
	    new->thermo.startPad = new->thermo.textWidth / 2 + 1;
    }
    if (req->thermo.reqEndPad == ThermoUnspecified)
    {
	if (new->thermo.vertical)
	    new->thermo.endPad = req->thermo.font->ascent + 
				 req->thermo.font->descent + 2;
	else
	    new->thermo.endPad = new->thermo.textWidth / 2 + 1;
    }
    if (req->thermo.reqMajorTickLen == ThermoUnspecified)
	new->thermo.majorTickLen =  req->thermo.font->ascent +
				    req->thermo.font->descent;
    else
	new->thermo.majorTickLen = req->thermo.reqMajorTickLen;
    if (req->thermo.reqMinorTickLen == ThermoUnspecified)
	new->thermo.minorTickLen =  req->thermo.majorTickLen / 2;
    else
	new->thermo.minorTickLen = req->thermo.reqMinorTickLen;
}

static void Initialize (greq, gnew)
    Widget  greq, gnew;
{
    ThermoWidget	req = (ThermoWidget) greq,
			new = (ThermoWidget) gnew;
    XGCValues		gcv;

    setDefaults (req, new);
    getSize (new, &new->core.width, &new->core.height);
    gcv.foreground = new->thermo.mercuryColor;
    new->thermo.mercuryGC = XtGetGC (gnew, GCForeground, &gcv);
    gcv.foreground = new->thermo.textColor;
    gcv.font = new->thermo.font->fid;
    new->thermo.textGC = XtGetGC (gnew, GCForeground|GCFont, &gcv);
    gcv.foreground = new->thermo.tickColor;
    new->thermo.tickGC = XtGetGC (gnew, GCForeground, &gcv);
}

static void
Destroy (gw)
    Widget  gw;
{
    ThermoWidget    w = (ThermoWidget) gw;

    XtReleaseGC (gw, w->thermo.mercuryGC);
    XtReleaseGC (gw, w->thermo.textGC);
    XtReleaseGC (gw, w->thermo.tickGC);
}

#define AreaPad(w)	((w)->thermo.startPad + (w)->thermo.endPad)
#define VArea(w)	((w)->core.height - AreaPad(w))
#define HArea(w)	((w)->core.width  - AreaPad(w))
#define VerticalPos(w,v)    ((w)->core.height - (w)->thermo.startPad - VArea(w) * (v) / \
			    ((w)->thermo.maximum - (w)->thermo.minimum))
#define HorizontalPos(w,v)    ((w)->thermo.startPad + HArea(w) * (v) / \
			    ((w)->thermo.maximum - (w)->thermo.minimum))

drawMercury (w, old, new)
    ThermoWidget    w;
    int		    old, new;
{
    int	    x, y, other, width, height;

    if (w->thermo.vertical)
    {
	width = w->thermo.thickness;
	x = w->core.width - width;
	other = VerticalPos (w, old);
	y = VerticalPos (w, new);
	height = other - y;
	if (height < 0)
	    XClearArea (XtDisplay (w), XtWindow (w), x, other, width, -height, FALSE);
	else
	    XFillRectangle (XtDisplay (w), XtWindow (w), w->thermo.mercuryGC,
			    x, y, width, height);
    }
    else
    {
	height = w->thermo.thickness;
	y = w->core.height - height;
	x = HorizontalPos (w, old);
	other = HorizontalPos (w, new);
	width = other - x;
	if (width < 0)
	    XClearArea (XtDisplay (w), XtWindow (w), other, y, -width, height, FALSE);
	else
	    XFillRectangle (XtDisplay (w), XtWindow (w), w->thermo.mercuryGC,
			    x, y, width, height);
    }
}
 
drawTick (w, v, len)
    ThermoWidget    w;
    int		    v;
    int		    len;
{
    int	    x, y, width, height;

    if (w->thermo.vertical) {
	x = w->core.width - w->thermo.thickness - len;
	width = len;
	height = 1;
	y = VerticalPos (w, v);
    }
    else
    {
	y = w->core.height - w->thermo.thickness - len;
	height = len;
	width = 1;
	x = HorizontalPos (w, v);
    }
    XFillRectangle (XtDisplay (w), XtWindow (w), w->thermo.tickGC,
		    x, y, width, height);
}

drawValue (w, v)
    ThermoWidget    w;
    int		    v;
{
    char    label[30];
    int	    width;
    int	    len;
    int	    x, y;

    sprintf (label, "%d", v);
    len = strlen (label);
    width = XTextWidth (w->thermo.font, label, len);
    if (w->thermo.vertical) {
	x = w->core.width - w->thermo.thickness - width;
	y = VerticalPos (w, v) - 2;
    }
    else
    {
	y = w->core.height - w->thermo.thickness - w->thermo.majorTickLen;
	x = HorizontalPos (w, v) - width / 2;
    }
    XDrawString (XtDisplay (w), XtWindow (w), w->thermo.textGC, x, y,
		 label, len);
}

static void
Redisplay (gw, event, region)
    Widget  gw;
    XEvent  *event;
    Region  region;
{
    ThermoWidget    w = (ThermoWidget) gw;
    int		    v;

    drawMercury (w, w->thermo.minimum, w->thermo.current);
    for (v = w->thermo.minorStart; v <= w->thermo.maximum; v += w->thermo.minorStep)
	drawTick (w, v, w->thermo.minorTickLen);
    for (v = w->thermo.majorStart; v <= w->thermo.maximum; v += w->thermo.majorStep)
    {
	drawTick (w, v, w->thermo.majorTickLen);
	drawValue (w, v);
    }
}

static Boolean
SetValues (gcur, greq, gnew)
    Widget  gcur, greq, gnew;
{
    ThermoWidget    cur = (ThermoWidget) gcur,
		    req = (ThermoWidget) greq,
		    new = (ThermoWidget) gnew;
    XGCValues	    gcv;
    Boolean	    redraw = FALSE;
    Dimension	    width, height;

    if (req->thermo.mercuryColor != cur->thermo.mercuryColor)
    {
	XtReleaseGC (gcur, cur->thermo.mercuryGC);
	gcv.foreground = req->thermo.mercuryColor;
	new->thermo.mercuryGC = XtGetGC (gnew, GCForeground, &gcv);
	redraw = TRUE;
    }
    if (req->thermo.textColor != cur->thermo.textColor ||
	req->thermo.font != cur->thermo.font)
    {
	XtReleaseGC (gcur, cur->thermo.textGC);
	gcv.foreground = req->thermo.textColor;
	gcv.font = req->thermo.font->fid;
	new->thermo.textGC = XtGetGC (gnew, GCForeground|GCFont, &gcv);
	redraw = TRUE;
    }
    if (req->thermo.tickColor != cur->thermo.tickColor)
    {
	XtReleaseGC (gcur, cur->thermo.tickGC);
	gcv.foreground = req->thermo.tickColor;
	new->thermo.tickGC = XtGetGC (gnew, GCForeground, &gcv);
	redraw = TRUE;
    }
    if (req->thermo.minimum != cur->thermo.minimum ||
	req->thermo.maximum != cur->thermo.maximum ||
	req->thermo.reqThickness != cur->thermo.reqThickness ||
	req->thermo.reqMinorStart != cur->thermo.reqMinorStart ||
	req->thermo.reqMajorStart != cur->thermo.reqMajorStart ||
	req->thermo.reqMinorStep != cur->thermo.reqMinorStep ||
	req->thermo.reqMajorStep != cur->thermo.reqMajorStep ||
	req->thermo.reqStartPad != cur->thermo.reqStartPad ||
	req->thermo.reqEndPad != cur->thermo.reqEndPad ||
	req->thermo.reqMinorTickLen != cur->thermo.reqMinorTickLen ||
	req->thermo.reqMajorTickLen != cur->thermo.reqMajorTickLen ||
	req->thermo.vertical != cur->thermo.vertical)
    {
	setDefaults (req, new);
	getSize (new, &width, &height);
	XtMakeResizeRequest (gnew, width, height, &width, &height);
	redraw = TRUE;
    }
    if (!redraw && req->thermo.current != cur->thermo.current)
    {
	drawMercury (new, cur->thermo.current, req->thermo.current);
    }
    return redraw;
}
