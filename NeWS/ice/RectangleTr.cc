/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <math.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Rectangle.h"

extern void	drawcrosshairs();
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pg_erasecoords();
extern void	pg_f2pcoords(float, float, int *, int *);
extern void	pg_showcoords(int, int, int, int, float, float, int, int);

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

Rectangle *tr_rect;
static boolean rect_drawn;
static int rect_x[4], rect_y[4];

void
trrect_proc(Menu *m, Menu_item *mi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];
	void recttr_corners();

	if ((tr_rect= (Rectangle *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Rectangle *) NULL) {
		ice_err("Cannot locate selected rectangle object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= RECT_TRANSLATE;

	if (pg_loc == PG_CURSORLOC) {
		rect_drawn= FALSE;
		recttr_corners();
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) tr_rect;
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
recttr_corners()
{
	int i, j;
	float fx[4], fy[4];
	float w, h, deg, sine, cosine;
	double rad;

	((Grobj *) tr_rect)->getloc(&(fx[0]), &(fy[0]), &i, &j);
	tr_rect->getsize(&w, &h);
	deg= ((Grobj *) tr_rect)->getrotation();

	if ((deg >= 0.) && (deg < 90.)) {
		rad= (double) ((deg*PI)/180.);
		sine= (float) sin(rad);
		cosine= (float) cos(rad);
		if (w == 0.) {
			fx[1]= fx[0];
			fy[1]= fy[0];
		}
		else {
			fx[1]= fx[0]+(w*cosine);
			fy[1]= fy[0]+(w*sine);
		}
		if (h == 0.) {
			fx[2]= fx[1];
			fy[2]= fy[1];
		}
		else {
			fx[2]= fx[1]+(h*sine*-1.);
			fy[2]= fy[1]+(h*cosine);
		}
	}
	else if ((deg >= 90.) && (deg < 180.)) {
		rad= (double) (((deg-90.)*PI)/180.);
		sine= (float) sin(rad);
		cosine= (float) cos(rad);
		if (w == 0.) {
			fx[1]= fx[0];
			fy[1]= fy[0];
		}
		else {
			fx[1]= fx[0]+(w*sine*-1.);
			fy[1]= fy[0]+(w*cosine);
		}
		if (h == 0.) {
			fx[2]= fx[1];
			fy[2]= fy[1];
		}
		else {
			fx[2]= fx[1]+(h*cosine*-1.);
			fy[2]= fy[1]+(h*sine*-1.);
		}
	}
	else if ((deg >= 180.) && (deg < 270.)) {
		rad= (double) (((deg-180.)*PI)/180.);
		sine= (float) sin(rad);
		cosine= (float) cos(rad);
		if (w == 0.) {
			fx[1]= fx[0];
			fy[1]= fy[0];
		}
		else {
			fx[1]= fx[0]+(w*cosine*-1.);
			fy[1]= fy[0]+(w*sine*-1.);
		}
		if (h == 0.) {
			fx[2]= fx[1];
			fy[2]= fy[1];
		}
		else {
			fx[2]= fx[1]+(h*sine);
			fy[2]= fy[1]+(h*cosine*-1.);
		}
	}
	else if ((deg >= 270.) && (deg < 360.)) {
		rad= (double) (((deg-270.)*PI)/180.);
		sine= (float) sin(rad);
		cosine= (float) cos(rad);
		if (w == 0.) {
			fx[1]= fx[0];
			fy[1]= fy[0];
		}
		else {
			fx[1]= fx[0]+(w*sine);
			fy[1]= fy[0]+(w*cosine*-1.);
		}
		if (h == 0.) {
			fx[2]= fx[1];
			fy[2]= fy[1];
		}
		else {
			fx[2]= fx[1]+(h*cosine);
			fy[2]= fy[1]+(h*sine);
		}
	}
	fx[3]= fx[0]+(fx[2]-fx[1]);
	fy[3]= fy[0]+(fy[2]-fy[1]);

	for (i= 0; i < 4; i++)
		pg_f2pcoords(fx[i], fy[i], &(rect_x[i]), &(rect_y[i]));

	return;
}

void
recttr_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, junk;
	float fx, fy;
	int xdiff, ydiff, i;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (rect_drawn) {
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_x[1], rect_y[1]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_x[1]-vx, rect_y[1]-vy);
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[1], rect_y[1], rect_x[2], rect_y[2]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[1]-vx, rect_y[1]-vy, rect_x[2]-vx, rect_y[2]-vy);
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[2], rect_y[2], rect_x[3], rect_y[3]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[2]-vx, rect_y[2]-vy, rect_x[3]-vx, rect_y[3]-vy);
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[3], rect_y[3], rect_x[0], rect_y[0]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[3]-vx, rect_y[3]-vy, rect_x[0]-vx, rect_y[0]-vy);
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		tr_rect->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
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
		tr_rect->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		tr_rect->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		tr_rect->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		tr_rect->getloc(&fx, &fy, &x, &junk);
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (rect_drawn) {
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_x[1], rect_y[1]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_x[1]-vx, rect_y[1]-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[1], rect_y[1], rect_x[2], rect_y[2]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[1]-vx, rect_y[1]-vy, rect_x[2]-vx, rect_y[2]-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[2], rect_y[2], rect_x[3], rect_y[3]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[2]-vx, rect_y[2]-vy, rect_x[3]-vx, rect_y[3]-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[3], rect_y[3], rect_x[0], rect_y[0]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[3]-vx, rect_y[3]-vy, rect_x[0]-vx, rect_y[0]-vy);
	}
	xdiff= x-rect_x[0];
	ydiff= y-rect_y[0];
	for (i= 0; i < 4; i++) {
		rect_x[i]+= xdiff;
		rect_y[i]+= ydiff;
	}
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_x[1], rect_y[1]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_x[1]-vx, rect_y[1]-vy);
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[1], rect_y[1], rect_x[2], rect_y[2]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[1]-vx, rect_y[1]-vy, rect_x[2]-vx, rect_y[2]-vy);
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[2], rect_y[2], rect_x[3], rect_y[3]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[2]-vx, rect_y[2]-vy, rect_x[3]-vx, rect_y[3]-vy);
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[3], rect_y[3], rect_x[0], rect_y[0]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[3]-vx, rect_y[3]-vy, rect_x[0]-vx, rect_y[0]-vy);
	rect_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}
