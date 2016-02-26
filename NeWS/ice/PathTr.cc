/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern void	drawcrosshairs();
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pg_erasecoords();
extern void	pg_showcoords(int, int, int, int, float, float, int, int);

extern Path *loc_path;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

Path *tr_pth;
static float ipp;

void
trpth_proc(Menu *m, Menu_item *mi)
{
	int nvertices;
	float *xvertices, *yvertices, fx, fy;
	char xbuf[30], ybuf[30];

	if ((tr_pth= (Path *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Path *) NULL) {
		ice_err("Cannot locate selected path object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= PATH_TRANSLATE;

	if (pg_loc == PG_CURSORLOC) {
		ipp= 1./pg_dpi;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_path= tr_pth;
	loc_path->getvertices(&nvertices, &xvertices, &yvertices);
	panelitem_set(locattr_panel, locattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	switch (pg_units) {
	case PG_PIXELS:
		(void) sprintf(xbuf, "%1d", (int) (xvertices[0]*pg_dpi));
		(void) sprintf(ybuf, "%1d", (int) (yvertices[0]*pg_dpi));
		break;
	case PG_POINTS:
		(void) sprintf(xbuf, "%4.2f", (float) (xvertices[0]*72.));
		(void) sprintf(ybuf, "%4.2f", (float) (yvertices[0]*72.));
		break;
	case PG_INCHES:
		(void) sprintf(xbuf, "%4.2f", xvertices[0]);
		(void) sprintf(ybuf, "%4.2f", yvertices[0]);
		break;
	case PG_USER:
		fx= (float) ((xvertices[0]-pg_xri)*pg_hsi)+pg_xru;
		fy= (float) ((yvertices[0]-pg_yri)*pg_vsi)+pg_yru;
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
pthtr_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y;
	float fx, fy;
	int nvert, i;
	float *xvert, *yvert, xdiff, ydiff;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		x= bevt->x+vx;
		y= bevt->y+vy;
		fx= ipp*((float) x);
		fy= ipp*((float) (pg_pixheight-1-y));
		tr_pth->getvertices(&nvert, &xvert, &yvert);
		xdiff= fx-xvert[0];
		ydiff= fy-yvert[0];
		for (i= 0; i < nvert; i++) {
			xvert[i]+= xdiff;
			yvert[i]+= ydiff;
		}
		if ((tr_pth->getvisibility() == PATH_VISIBLE) ||
		    (tr_pth->getreferences() > 0))
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
		fx= ipp*((float) x);
		fy= ipp*((float) (pg_pixheight-1-y));
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		fx= ipp*((float) x);
		fy= ipp*((float) (pg_pixheight-1-y));
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}
