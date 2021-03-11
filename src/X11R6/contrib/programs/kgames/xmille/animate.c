/*
 * animation
 */

# include	"mille.h"
# include	"card.h"
# include	"uiXt.h"
# include	<math.h>
# include	<X11/Intrinsic.h>
# include	<X11/StringDefs.h>
# include	<X11/Xos.h>

extern int	iscolor;

double	animation_speed = .5;

animate_move (player, orig_type, orig_arg, dest_type, dest_arg)
{
	int	ox, oy, dx, dy;

	if (!animation_speed) return;
	compute_position (player, orig_type, orig_arg, &ox, &oy);
	compute_position (player, dest_type, dest_arg, &dx, &dy);
	do_animate (ox, oy, dx, dy);
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
do_animate (ox, oy, dx, dy)
{
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
			draw_square (ix, iy, ix + WIDTH, iy + HEIGHT);
		else {
			if (xp < ix) {
				x1 = xp + WIDTH;
				x2 = ix + WIDTH;
				x3 = ix;
				x4 = ix + WIDTH;
			} else if (xp > ix) {
				x1 = ix;
				x2 = xp;
				x3 = ix;
				x4 = ix + WIDTH;
			} else {
				x1 = -32767;
				x2 = -32767;
				x3 = ix;
				x4 = ix + WIDTH;
			}
			if (yp < iy) {
				y1 = iy;
				y2 = yp + HEIGHT;
				y3 = yp + HEIGHT;
				y4 = iy + HEIGHT;
			} else if (yp > iy) {
				y1 = yp;
				y2 = iy + HEIGHT;
				y3 = iy;
				y4 = yp;
			} else {
				y1 = iy;
				y2 = iy + HEIGHT;
				y3 = -32767;
				y4 = -32767;
			}
			if (x1 != -32767 && y1 != -32767)
				draw_square (x1, y1, x2, y2);
			if (x3 != -32767 && y3 != -32767)
				draw_square (x3, y3, x4, y4);
			if (ix < xp) {
				x1 = ix + WIDTH;
				x2 = xp + WIDTH;
				x3 = xp;
				x4 = xp + WIDTH;
			} else if (ix > xp) {
				x1 = xp;
				x2 = ix;
				x3 = xp;
				x4 = xp + WIDTH;
			} else {
				x1 = -32767;
				x2 = -32767;
				x3 = xp;
				x4 = xp + WIDTH;
			}
			if (iy < yp) {
				y1 = yp;
				y2 = iy + HEIGHT;
				y3 = iy + HEIGHT;
				y4 = yp + HEIGHT;
			} else if (iy > yp) {
				y1 = iy;
				y2 = yp + HEIGHT;
				y3 = yp;
				y4 = iy;
			} else {
				y1 = yp;
				y2 = yp + HEIGHT;
				y3 = -32767;
				y4 = -32767;
			}
			if (x1 != -32767 && y1 != -32767)
				draw_square (x1, y1, x2, y2);
			if (x3 != -32767 && y3 != -32767)
				draw_square (x3, y3, x4, y4);
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
	draw_square (xp, yp, xp+WIDTH, yp+HEIGHT);
	XFlush (dpy);
}

extern Widget	human_hand, deck_hand,
		computer_play, human_play,
		computer_safeties, human_safeties, layout;

static
draw_square (x1, y1, x2, y2)
{
	XFillRectangle (dpy, XtWindow(layout), xor_gc, x1, y1, x2-x1, y2-y1);
}


static
compute_position (player, type, arg, xp, yp)
int	*xp, *yp;
{
	Widget	w;
	XRectangle	r;
	int	row, col;
	int	yForce = 0;
	Position	x, y;
	Arg	args[2];
	
	switch (type) {
	case ANIMATE_HAND:
		switch (player) {
		case 0:
			w = human_hand;
			row = 0;
			col = arg;
			break;
		case 1:
			w = computer_play;
			yForce = -HEIGHT;
			break;
		}
		row = 0;
		col = arg;
		break;
	case ANIMATE_DECK:
		w = deck_hand;
		row = 0;
		col = 0;
		break;
	case ANIMATE_DISC:
		w = deck_hand;
		row = 0;
		col = 1;
		break;
	case ANIMATE_MILES:
		switch (player) {
		case 0:
			w = human_play;
			break;
		case 1:
			w = computer_play;
			break;
		}
		row = 0;
		col = (2 + C_200 - arg);
		break;
	case ANIMATE_BATTLE:
		switch (player) {
		case 0:
			w = human_play;
			break;
		case 1:
			w = computer_play;
			break;
		}
		row = 0;
		col = 1;
		break;
	case ANIMATE_SPEED:
		switch (player) {
		case 0:
			w = human_play;
			break;
		case 1:
			w = computer_play;
			break;
		}
		row = 0;
		col = 0;
		break;
	case ANIMATE_OBATTLE:
		switch (1-player) {
		case 0:
			w = human_play;
			break;
		case 1:
			w = computer_play;
			break;
		}
		row = 0;
		col = 1;
		break;
	case ANIMATE_OSPEED:
		switch (1-player) {
		case 0:
			w = human_play;
			break;
		case 1:
			w = computer_play;
			break;
		}
		row = 0;
		col = 0;
		break;
		break;
	case ANIMATE_SAFETY:
		switch (player) {
		case 0:
			w = human_safeties;
			break;
		case 1:
			w = computer_safeties;
			break;
		}
		row = arg & 1;
		col = (arg & 2) >> 1;
		break;
	}
	HandRectangleForPos (w, row, col, &r);
	XtSetArg (args[0], XtNx, &x);
	XtSetArg (args[1], XtNy, &y);
	XtGetValues (w, args, 2);
	*xp = r.x + x;
	if (yForce)
		*yp = yForce;
	else
		*yp = r.y + y;
}
