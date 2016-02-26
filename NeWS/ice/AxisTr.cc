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

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;
extern boolean axis_drawn;
extern int axis_x, axis_y;
extern int axis_ex, axis_ey;

Axis *tr_axis;
int axis_xinitpos, axis_yinitpos;
int axis_xdiff, axis_ydiff;

void
traxis_proc(Menu *m, Menu_item *mi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];


	if ((tr_axis= (Axis *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Axis *) NULL) {
		ice_err("Cannot locate selected axis object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= AXIS_TRANSLATE;

	if (pg_loc == PG_CURSORLOC) {
		tr_axis->getloc(&fx, &fy, &axis_xinitpos, &axis_yinitpos);
		axis_x= axis_xinitpos;
		axis_y= pg_pixheight-1-axis_yinitpos;
		tr_axis->getend(&fx, &fy, &axis_ex, &axis_ey);
		axis_ey= pg_pixheight-1-axis_ey;
		axis_xdiff= axis_ex-axis_x;
		axis_ydiff= axis_ey-axis_y;
		axis_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) tr_axis;
	loc_grobj->getloc(&fx, &fy, &ix, &iy);
	panelitem_set(locattr_panel, locattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	switch (pg_units) {
	case PG_PIXELS:
		(void) sprintf(xbuf, "%1d", ix);
		(void) sprintf(ybuf, "%1d", iy);
		break;
	case PG_POINTS:
		(void) sprintf(xbuf, "%4.2f", (float) (fx*72.));
		(void) sprintf(ybuf, "%4.2f", (float) (fy*72.));
		break;
	case PG_INCHES:
		(void) sprintf(xbuf, "%4.2f", fx);
		(void) sprintf(ybuf, "%4.2f", fy);
		break;
	case PG_USER:
		fx= (float) ((fx-pg_xri)*pg_hsi)+pg_xru;
		fy= (float) ((fy-pg_yri)*pg_vsi)+pg_yru;
		(void) sprintf(xbuf, "%4.2f", fx);
		(void) sprintf(ybuf, "%4.2f", fy);
		break;
	}
	panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, xbuf, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, ybuf, LXPI_NULL);
	XMapRaised(dpy, locattr_frame);
	return;
}

void
axistr_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, junk;
	int ex, ey, oex, oey;
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
		tr_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		tr_axis->getend(&fx, &fy, &oex, &oey);
		ex= oex+(x-axis_xinitpos);
		ey= oey+(pg_pixheight-1-y-axis_yinitpos);
		tr_axis->setpend(ex, ey, (float) pg_dpi);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
		ice_op= MAIN_MENU;
		return;

	case ButtonPress:
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		tr_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		tr_axis->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		tr_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		tr_axis->getloc(&fx, &fy, &x, &junk);
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
	axis_x= x;
	axis_y= y;
	axis_ex= axis_x+axis_xdiff;
	axis_ey= axis_y+axis_ydiff;
	XDrawLine(dpy, pg_cpm, pg_xgc, axis_x, axis_y, axis_ex, axis_ey);
	XDrawLine(dpy, pg_cwin, pg_xgc, axis_x-vx, axis_y-vy, axis_ex-vx, axis_ey-vy);
	axis_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}
