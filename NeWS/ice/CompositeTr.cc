/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"
#include "Vector.h"
#include "Curve.h"
#include "Axis.h"

extern void	drawcrosshairs();
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pg_erasecoords();
extern void	pg_showcoords(int, int, int, int, float, float, int, int);

extern Composite *loc_cmpobj;
extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

Composite *tr_cmp;
Grobj *tr_atom;
float tr_ipp;

void
trcmp_proc(Menu *m, Menu_item *mi)
{
	Composite *cmp;
	int nchildren;
	Grobj **children;
	void trcmp_setup();

	if ((tr_cmp= (Composite *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Composite *) NULL) {
		ice_err("Cannot locate selected path object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	for (tr_atom= (Grobj *) NULL, cmp= tr_cmp; tr_atom == (Grobj *) NULL; ) {
		cmp->getchildren(&nchildren, &children);
		if (children[0]->gettype() == GROBJ_COMPOSITE)
			cmp= (Composite *) children[0];
		else
			tr_atom= children[0];
	}
	trcmp_setup();
	return;
}

void
trcmp_setup()
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	tr_atom->getloc(&fx, &fy, &ix, &iy);

	ice_op= CMP_TRANSLATE;

	if (pg_loc == PG_CURSORLOC) {
		tr_ipp= 1./pg_dpi;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_cmpobj= tr_cmp;
	loc_grobj= tr_atom;
	panelitem_set(locattr_panel, locattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	switch (pg_units) {
	case PG_PIXELS:
		(void) sprintf(xbuf, "%1d", (int) (fx*pg_dpi));
		(void) sprintf(ybuf, "%1d", (int) (fy*pg_dpi));
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
cmptr_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, ix, iy;
	float fx, fy, initx, inity;
	int natoms, i;
	Grobj **atoms;
	void cmptr_adjatom(Grobj *, float, float);

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
		fx= tr_ipp*((float) x);
		fy= tr_ipp*((float) (pg_pixheight-1-y));
		tr_atom->getloc(&initx, &inity, &ix, &iy);
		tr_cmp->getatoms(&natoms, &atoms);
		for (i= 0; i < natoms; i++)
			cmptr_adjatom(atoms[i], fx-initx, fy-inity);
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
		fx= tr_ipp*((float) x);
		fy= tr_ipp*((float) (pg_pixheight-1-y));
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		fx= tr_ipp*((float) x);
		fy= tr_ipp*((float) (pg_pixheight-1-y));
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

void
cmptr_adjatom(Grobj *gr, float xdiff, float ydiff)
{
	float fx, fy;
	int ix, iy;

	gr->getloc(&fx, &fy, &ix, &iy);
	fx+= xdiff;
	fy+= ydiff;
	gr->setfloc(fx, fy, (float) pg_dpi);

	switch (gr->gettype()) {
	case GROBJ_PSDOC:
	case GROBJ_RASTER:
		break;
	case GROBJ_INTOBJ:
		switch (((Intobj *) gr)->getintobjtype()) {
		case INTOBJ_TEXT:
		case INTOBJ_MARKER:
		case INTOBJ_RECTANGLE:
		case INTOBJ_POLYGON:
			break;
		case INTOBJ_VECTOR:
			((Vector *) gr)->getend(&fx, &fy, &ix, &iy);
			fx+= xdiff;
			fy+= ydiff;
			((Vector *) gr)->setfend(fx, fy, (float) pg_dpi);
			break;
		case INTOBJ_CURVE:
			((Curve *) gr)->getcontrol1(&fx, &fy, &ix, &iy);
			fx+= xdiff;
			fy+= ydiff;
			((Curve *) gr)->setfcontrol1(fx, fy, (float) pg_dpi);
			((Curve *) gr)->getcontrol2(&fx, &fy, &ix, &iy);
			fx+= xdiff;
			fy+= ydiff;
			((Curve *) gr)->setfcontrol2(fx, fy, (float) pg_dpi);
			((Curve *) gr)->getend(&fx, &fy, &ix, &iy);
			fx+= xdiff;
			fy+= ydiff;
			((Curve *) gr)->setfend(fx, fy, (float) pg_dpi);
			break;
		case INTOBJ_AXIS:
			((Axis *) gr)->getend(&fx, &fy, &ix, &iy);
			fx+= xdiff;
			fy+= ydiff;
			((Axis *) gr)->setfend(fx, fy, (float) pg_dpi);
			break;
		}
		break;
	}

	return;
}
