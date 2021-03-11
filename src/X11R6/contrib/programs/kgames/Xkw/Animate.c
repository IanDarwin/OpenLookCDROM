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

# include	<X11/Xlib.h>
# include	<math.h>
# include	<X11/Intrinsic.h>
# include	<X11/StringDefs.h>
# include	<X11/Xos.h>
# include	"Cards.h"

#define DEFAULT_ANIMATION_SPEED	20
double	animation_speed = DEFAULT_ANIMATION_SPEED;

static Dimension  width, height;
static Pixel	    xor_value;

AnimateSetSpeed (i)
    int	    i;
{
    if (i < 0)
	i = DEFAULT_ANIMATION_SPEED;
    animation_speed = i;
}

Animate (srcWidget, srcRow, srcCol, dstWidget, dstRow, dstCol)
    Widget  srcWidget;
    int	    srcRow, srcCol;
    Widget  dstWidget;
    int	    dstRow, dstCol;
{
    int	ox, oy, dx, dy;
    Arg	arg[4];
    Pixel   obverse, black;

    if (!animation_speed) return;
    XtSetArg (arg[0], XtNcardWidth, &width);
    XtSetArg (arg[1], XtNcardHeight, &height);
    XtSetArg (arg[2], XtNobverseColor, &obverse);
    XtSetArg (arg[3], XtNblackColor, &black);
    XtGetValues (srcWidget, arg, 4);
    xor_value = obverse ^ black;
    compute_position (srcWidget, srcRow, srcCol, XtParent(srcWidget), &ox, &oy);
    compute_position (dstWidget, dstRow, dstCol, XtParent(srcWidget), &dx, &dy);
    do_animate (XtParent(srcWidget), ox, oy, dx, dy);
}

# define abs(x)	((x) < 0 ? -(x) : (x))

/*
 * bigger numbers make it go faster
 */

# define accerate(v,r)	((v) + (speed/25 * (r)))

static
msleep (ms)
{
    struct { int s, us; } t;
    int f = 0;

    t.s = ms / 1000;
    t.us = (ms % 1000) * 1000;
    select (1, &f, 0, 0, &t);
}
    
static
do_animate (widget, ox, oy, dx, dy)
    Widget  widget;
{
	Display	*dpy = XtDisplay (widget);
	double	x, y;
	double	xc, yc;
	int	xd, yd;
	int	xp, yp;
	int	x1, y1, x2, y2, x3, y3, x4, y4;
	int	ix, iy;
	double	dist;
	double	rx, ry;
	double	speed;

	x = ox;
	y = oy;
	xd = dx - ox;
	yd = dy - oy;
	dist = sqrt ((double) xd * xd + yd * yd);
	rx = (double) xd / dist;
	ry = (double) yd / dist;
	speed = animation_speed;
	xc = speed * rx;
	yc = speed * ry;
	xp = yp = -32767;
	XFlush (dpy);
	while (abs(dx - x) > abs (xc) || abs(dy - y) > abs (yc)) {
		ix = x;
		iy = y;
		if (xp == -32767)
			draw_square (widget, ix, iy, ix + width, iy + height);
		else {
			if (xp < ix) {
				x1 = xp + width;
				x2 = ix + width;
				x3 = ix;
				x4 = ix + width;
			} else if (xp > ix) {
				x1 = ix;
				x2 = xp;
				x3 = ix;
				x4 = ix + width;
			} else {
				x1 = -32767;
				x2 = -32767;
				x3 = ix;
				x4 = ix + width;
			}
			if (yp < iy) {
				y1 = iy;
				y2 = yp + height;
				y3 = yp + height;
				y4 = iy + height;
			} else if (yp > iy) {
				y1 = yp;
				y2 = iy + height;
				y3 = iy;
				y4 = yp;
			} else {
				y1 = iy;
				y2 = iy + height;
				y3 = -32767;
				y4 = -32767;
			}
			if (x1 != -32767 && y1 != -32767)
				draw_square (widget, x1, y1, x2, y2);
			if (x3 != -32767 && y3 != -32767)
				draw_square (widget, x3, y3, x4, y4);
			if (ix < xp) {
				x1 = ix + width;
				x2 = xp + width;
				x3 = xp;
				x4 = xp + width;
			} else if (ix > xp) {
				x1 = xp;
				x2 = ix;
				x3 = xp;
				x4 = xp + width;
			} else {
				x1 = -32767;
				x2 = -32767;
				x3 = xp;
				x4 = xp + width;
			}
			if (iy < yp) {
				y1 = yp;
				y2 = iy + height;
				y3 = iy + height;
				y4 = yp + height;
			} else if (iy > yp) {
				y1 = iy;
				y2 = yp + height;
				y3 = yp;
				y4 = iy;
			} else {
				y1 = yp;
				y2 = yp + height;
				y3 = -32767;
				y4 = -32767;
			}
			if (x1 != -32767 && y1 != -32767)
				draw_square (widget, x1, y1, x2, y2);
			if (x3 != -32767 && y3 != -32767)
				draw_square (widget, x3, y3, x4, y4);
		}
		xp = ix;
		yp = iy;
		if (abs (dx - x) > xc)
			x += xc;
		if (abs (dy - y) > yc)
			y += yc;
		xc = accerate(xc, rx);
		yc = accerate(yc, ry);
		XFlush (dpy);
		msleep (10);
	}
	draw_square (widget, xp, yp, xp+width, yp+height);
	XFlush (dpy);
}

static
draw_square (widget, x1, y1, x2, y2)
    Widget  widget;
{
    static GC	    gc;
    static Widget   oldw;
    static Pixel    oldp;
    XGCValues	    gcv;

    if (gc && (oldw != widget || oldp != xor_value))
    {
	XtReleaseGC (oldw, gc);
	gc = 0;
    }
    if (!gc)
    {
	gcv.function = GXxor;
	gcv.foreground = xor_value;
	gcv.subwindow_mode = IncludeInferiors;
	gc = XtGetGC (widget, GCForeground | GCFunction | GCSubwindowMode, &gcv);
	oldw = widget;
    }
    XFillRectangle (XtDisplay (widget), XtWindow(widget),
		    gc, x1, y1, x2-x1, y2-y1);
}


static
compute_position (w, row, col, animate, xp, yp)
    Widget  w;
    int	    row, col;
    Widget  animate;
    int	    *xp, *yp;
{
    XRectangle	r;
    Position	x, y;
    Arg		args[2];
    
    HandRectangleForPos (w, row, col, &r);
    XtSetArg (args[0], XtNx, &x);
    XtSetArg (args[1], XtNy, &y);
    XtGetValues (w, args, 2);
    *xp = r.x + x;
    *yp = r.y + y;
}
