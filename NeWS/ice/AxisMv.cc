/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Axis.h"

extern void	drawcrosshairs();
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pg_erasecoords();
extern void	pg_showcoords(int, int, int, int, float, float, int, int);

extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;
extern Axis *attr_axis;
extern boolean axis_drawn;
extern int axis_x, axis_y;
extern int axis_ex, axis_ey;

void
axismv_event(XEvent *event)
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
		if (axis_drawn) {
			XDrawLine(dpy, pg_cpm, pg_xgc, axis_x, axis_y, axis_ex, axis_ey);
			XDrawLine(dpy, pg_cwin, pg_xgc, axis_x-vx, axis_y-vy, axis_ex-vx, axis_ey-vy);
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		if (ice_op == AXIS_MVLOC1)
			attr_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		else
			attr_axis->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
		ice_op= AXIS_ATTR;
		return;

	case ButtonPress:
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (ice_op == AXIS_MVLOC1) {
			attr_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_axis->getloc(&fx, &fy, &x, &junk);
		}
		else {
			attr_axis->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_axis->getend(&fx, &fy, &x, &junk);
		}
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		if (ice_op == AXIS_MVLOC1) {
			attr_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_axis->getloc(&fx, &fy, &x, &junk);
		}
		else {
			attr_axis->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_axis->getend(&fx, &fy, &x, &junk);
		}
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (axis_drawn) {
		XDrawLine(dpy, pg_cpm, pg_xgc, axis_x, axis_y, axis_ex, axis_ey);
		XDrawLine(dpy, pg_cwin, pg_xgc, axis_x-vx, axis_y-vy, axis_ex-vx, axis_ey-vy);
	}
	if (ice_op == AXIS_MVLOC1) {
		axis_x= x;
		axis_y= y;
	}
	else {
		axis_ex= x;
		axis_ey= y;
	}
	XDrawLine(dpy, pg_cpm, pg_xgc, axis_x, axis_y, axis_ex, axis_ey);
	XDrawLine(dpy, pg_cwin, pg_xgc, axis_x-vx, axis_y-vy, axis_ex-vx, axis_ey-vy);
	axis_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}
