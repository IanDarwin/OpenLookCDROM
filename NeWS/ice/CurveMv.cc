/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Curve.h"

extern void	crv_drawoutline();
extern void	drawcrosshairs();
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pg_erasecoords();
extern void	pg_showcoords(int, int, int, int, float, float, int, int);

extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;
extern Curve *attr_crv;
extern boolean crv_drawn;
extern int ncrvvert, crvixvert[], crviyvert[];

void
crvmv_event(XEvent *event)
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
		if (crv_drawn)
			crv_drawoutline();
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		switch (ice_op) {
		case CRV_MVLOC1:
			attr_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			break;
		case CRV_MVLOC2:
			attr_crv->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			break;
		case CRV_MVCNT1:
			attr_crv->setpcontrol1(x, pg_pixheight-1-y, (float) pg_dpi);
			break;
		case CRV_MVCNT2:
			attr_crv->setpcontrol2(x, pg_pixheight-1-y, (float) pg_dpi);
			break;
		}
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
		ice_op= CRV_ATTR;
		return;

	case ButtonPress:
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		switch (ice_op) {
		case CRV_MVLOC1:
			attr_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getloc(&fx, &fy, &x, &junk);
			break;
		case CRV_MVLOC2:
			attr_crv->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getend(&fx, &fy, &x, &junk);
			break;
		case CRV_MVCNT1:
			attr_crv->setpcontrol1(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getcontrol1(&fx, &fy, &x, &junk);
			break;
		case CRV_MVCNT2:
			attr_crv->setpcontrol2(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getcontrol2(&fx, &fy, &x, &junk);
			break;
		}
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		switch (ice_op) {
		case CRV_MVLOC1:
			attr_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getloc(&fx, &fy, &x, &junk);
			break;
		case CRV_MVLOC2:
			attr_crv->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getend(&fx, &fy, &x, &junk);
			break;
		case CRV_MVCNT1:
			attr_crv->setpcontrol1(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getcontrol1(&fx, &fy, &x, &junk);
			break;
		case CRV_MVCNT2:
			attr_crv->setpcontrol2(x, pg_pixheight-1-y, (float) pg_dpi);
			attr_crv->getcontrol2(&fx, &fy, &x, &junk);
			break;
		}
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (crv_drawn)
		crv_drawoutline();
	switch (ice_op) {
	case CRV_MVLOC1:
		crvixvert[0]= x;
		crviyvert[0]= y;
		break;
	case CRV_MVCNT1:
		crvixvert[1]= x;
		crviyvert[1]= y;
		break;
	case CRV_MVCNT2:
		crvixvert[2]= x;
		crviyvert[2]= y;
		break;
	case CRV_MVLOC2:
		crvixvert[3]= x;
		crviyvert[3]= y;
		break;
	}
	crv_drawoutline();
	crv_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}
