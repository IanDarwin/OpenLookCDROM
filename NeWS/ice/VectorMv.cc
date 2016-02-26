/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Vector.h"

extern void	drawcrosshairs();
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pg_erasecoords();
extern void	pg_showcoords(int, int, int, int, float, float, int, int);

extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;
extern Vector *attr_vec;
extern boolean vec_drawn;
extern int vec_x, vec_y;
extern int vec_ex, vec_ey;

void
vecmv_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, junk;
	float fx, fy;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (vec_drawn) {
			XDrawLine(dpy, pg_cpm, pg_xgc, vec_x, vec_y, vec_ex, vec_ey);
			XDrawLine(dpy, pg_cwin, pg_xgc, vec_x-vx, vec_y-vy, vec_ex-vx, vec_ey-vy);
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		if (ice_op == VEC_MVLOC1)
			attr_vec->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		else
			attr_vec->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
		ice_op= VEC_ATTR;
		return;

	case ButtonPress:
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (ice_op == VEC_MVLOC1) {
			attr_vec->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_vec->getloc(&fx, &fy, &x, &junk);
		}
		else {
			attr_vec->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_vec->getend(&fx, &fy, &x, &junk);
		}
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		if (ice_op == VEC_MVLOC1) {
			attr_vec->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_vec->getloc(&fx, &fy, &x, &junk);
		}
		else {
			attr_vec->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_vec->getend(&fx, &fy, &x, &junk);
		}
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (vec_drawn) {
		XDrawLine(dpy, pg_cpm, pg_xgc, vec_x, vec_y, vec_ex, vec_ey);
		XDrawLine(dpy, pg_cwin, pg_xgc, vec_x-vx, vec_y-vy, vec_ex-vx, vec_ey-vy);
	}
	if (ice_op == VEC_MVLOC1) {
		vec_x= x;
		vec_y= y;
	}
	else {
		vec_ex= x;
		vec_ey= y;
	}
	XDrawLine(dpy, pg_cpm, pg_xgc, vec_x, vec_y, vec_ex, vec_ey);
	XDrawLine(dpy, pg_cwin, pg_xgc, vec_x-vx, vec_y-vy, vec_ex-vx, vec_ey-vy);
	vec_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}
