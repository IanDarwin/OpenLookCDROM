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

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;
extern boolean crv_drawn;
extern int ncrvvert, crvixvert[], crviyvert[];

Curve *tr_crv;
int crvoixvert[4], crvoiyvert[4];

void
trcrv_proc(Menu *m, Menu_item *mi)
{
	float fx, fy;
	int ix, iy, i;
	char xbuf[30], ybuf[30];


	if ((tr_crv= (Curve *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Curve *) NULL) {
		ice_err("Cannot locate selected curve object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= CRV_TRANSLATE;

	if (pg_loc == PG_CURSORLOC) {
		ncrvvert= 4;
		tr_crv->getloc(&fx, &fy, &crvixvert[0], &crviyvert[0]);
		crviyvert[0]= pg_pixheight-1-crviyvert[0];
		tr_crv->getcontrol1(&fx, &fy, &crvixvert[1], &crviyvert[1]);
		crviyvert[1]= pg_pixheight-1-crviyvert[1];
		tr_crv->getcontrol2(&fx, &fy, &crvixvert[2], &crviyvert[2]);
		crviyvert[2]= pg_pixheight-1-crviyvert[2];
		tr_crv->getend(&fx, &fy, &crvixvert[3], &crviyvert[3]);
		crviyvert[3]= pg_pixheight-1-crviyvert[3];
		for (i= 0; i < 4; i++) {
			crvoixvert[i]= crvixvert[i];
			crvoiyvert[i]= crviyvert[i];
		}
		crv_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) tr_crv;
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
crvtr_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, junk, i;
	int ex, ey;
	int xdiff, ydiff;
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
		xdiff= x-crvoixvert[0];
		ydiff= y-crvoiyvert[0];
		tr_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		ex= crvoixvert[1]+xdiff;
		ey= pg_pixheight-1-crvoiyvert[1]-ydiff;
		tr_crv->setpcontrol1(ex, ey, (float) pg_dpi);
		ex= crvoixvert[2]+xdiff;
		ey= pg_pixheight-1-crvoiyvert[2]-ydiff;
		tr_crv->setpcontrol2(ex, ey, (float) pg_dpi);
		ex= crvoixvert[3]+xdiff;
		ey= pg_pixheight-1-crvoiyvert[3]-ydiff;
		tr_crv->setpend(ex, ey, (float) pg_dpi);
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
		tr_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		tr_crv->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		tr_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		tr_crv->getloc(&fx, &fy, &x, &junk);
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (crv_drawn)
		crv_drawoutline();
	xdiff= x-crvoixvert[0];
	ydiff= y-crvoiyvert[0];
	for (i= 0; i < 4; i++) {
		crvixvert[i]= crvoixvert[i]+xdiff;
		crviyvert[i]= crvoiyvert[i]+ydiff;
	}
	crv_drawoutline();
	crv_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}
