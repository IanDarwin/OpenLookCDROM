/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Vector.h"
#include "Curve.h"
#include "Polygon.h"
#include "Axis.h"
#include "Path.h"

extern void		cmptr_adjatom(Grobj *, float, float);
extern void		ice_err(char *, int);
extern void		pg_draw();

extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern int rect_dimmode;

Grobj *loc_grobj;
Composite *loc_cmpobj;
Path *loc_path;
int loc_nvertices, loc_vertex;
float *loc_xvertices, *loc_yvertices;

static float loc_xorig, loc_yorig;

void
locattrcont_proc(Panel *p, Panel_item *pi)
{
	void locattrins_proc(Panel *, Panel_item *);
	void locattrtr_proc(Panel *, Panel_item *);

	switch (ice_op) {
	case PSD_INSERTLOC:
	case RAS_INSERTLOC:
	case TEXT_INSERTLOC:
	case VEC_INSERTLOC1:
	case VEC_INSERTLOC2:
	case CRV_INSERTLOC1:
	case CRV_INSERTCNT1:
	case CRV_INSERTCNT2:
	case CRV_INSERTLOC2:
	case MRK_INSERTLOC:
	case RECT_INSERTLOC:
	case POLY_INSERTLOC:
	case AXIS_INSERTLOC1:
	case AXIS_INSERTLOC2:
	case PATH_INSERTLOC:
		locattrins_proc(p, pi);
		break;
	case PSD_TRANSLATE:
	case RAS_TRANSLATE:
	case TEXT_TRANSLATE:
	case VEC_TRANSLATE:
	case MRK_TRANSLATE:
	case RECT_TRANSLATE:
	case POLY_TRANSLATE:
	case AXIS_TRANSLATE:
	case CMP_TRANSLATE:
	case PATH_TRANSLATE:
	case VEC_MVLOC1:
	case VEC_MVLOC2:
	case CRV_MVLOC1:
	case CRV_MVLOC2:
	case CRV_MVCNT1:
	case CRV_MVCNT2:
	case AXIS_MVLOC1:
	case AXIS_MVLOC2:
	case PSD_COPY:
	case RAS_COPY:
	case TEXT_COPY:
	case VEC_COPY:
	case CRV_COPY:
	case MRK_COPY:
	case RECT_COPY:
	case POLY_COPY:
	case AXIS_COPY:
	case CMP_COPY:
		locattrtr_proc(p, pi);
		break;
	}
	return;
}

void
locattrins_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	float fx, fy;
	char label[40], errmsg[MAX_ERRMSGLEN+1];

	buf= (char *) panelitem_get(locattr_panel, locattr_x, LXPTEXT_VALUE);
	fx= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid x value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(locattr_panel, locattr_y, LXPTEXT_VALUE);
	fy= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid y value.", NONFATAL);
		return;
	}

	switch (pg_units) {
	case PG_PIXELS:
		fx/= pg_dpi;
		fy/= pg_dpi;
		break;
	case PG_POINTS:
		fx/= 72.;
		fy/= 72.;
		break;
	case PG_INCHES:
		break;
	case PG_USER:
		fx= ((fx-pg_xru)/pg_hsi)+pg_xri;
		fy= ((fy-pg_yru)/pg_vsi)+pg_yri;
		break;
	}

	switch (ice_op) {
	case PSD_INSERTLOC:
	case RAS_INSERTLOC:
	case TEXT_INSERTLOC:
	case MRK_INSERTLOC:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		XUnmapWindow(dpy, locattr_frame);
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		ice_op= MAIN_MENU;
		break;
	case VEC_INSERTLOC1:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Vector Terminus", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "", LXPI_NULL);
		panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "", LXPI_NULL);
		ice_op= VEC_INSERTLOC2;
		break;
	case VEC_INSERTLOC2:
		((Vector *) loc_grobj)->setfend(fx, fy, pg_dpi);
		XUnmapWindow(dpy, locattr_frame);
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		ice_op= MAIN_MENU;
		break;
	case CRV_INSERTLOC1:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Curve Control A", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "", LXPI_NULL);
		panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "", LXPI_NULL);
		ice_op= CRV_INSERTCNT1;
		break;
	case CRV_INSERTCNT1:
		((Curve *) loc_grobj)->setfcontrol1(fx, fy, pg_dpi);
		panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Curve Control B", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "", LXPI_NULL);
		panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "", LXPI_NULL);
		ice_op= CRV_INSERTCNT2;
		break;
	case CRV_INSERTCNT2:
		((Curve *) loc_grobj)->setfcontrol2(fx, fy, pg_dpi);
		panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Curve Terminus", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "", LXPI_NULL);
		panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "", LXPI_NULL);
		ice_op= CRV_INSERTLOC2;
		break;
	case CRV_INSERTLOC2:
		((Curve *) loc_grobj)->setfend(fx, fy, pg_dpi);
		XUnmapWindow(dpy, locattr_frame);
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		ice_op= MAIN_MENU;
		break;
	case RECT_INSERTLOC:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		XUnmapWindow(dpy, locattr_frame);
		switch (rect_dimmode) {
		case RECT_CURSORDIM:
			pgcoords_drawn= crosshairs_drawn= FALSE;
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
			ice_op= RECT_INSERTROT;
			break;
		case RECT_TEXTDIM:
			XDefineCursor(dpy, pg_cwin, hg_cursor);
			XSync(dpy, False);
			pg_draw();
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			ice_op= MAIN_MENU;
			break;
		}
		break;
	case POLY_INSERTLOC:
		if (loc_vertex == 0) {
			loc_grobj->setfloc(fx, fy, pg_dpi);
			loc_xorig= fx;
			loc_yorig= fy;
			loc_xvertices[0]= 0.;
			loc_yvertices[0]= 0.;
		}
		else {
			loc_xvertices[loc_vertex]= fx-loc_xorig;
			loc_yvertices[loc_vertex]= fy-loc_yorig;
		}
		loc_vertex++;
		if (loc_vertex < loc_nvertices) {
			(void) sprintf(label, "Vertex %1d", loc_vertex+1);
			panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, label, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "", LXPI_NULL);
			panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "", LXPI_NULL);
		}
		else {
			((Polygon *) loc_grobj)->setvertices(loc_nvertices, loc_xvertices, loc_yvertices);
			loc_xvertices= loc_yvertices= (float *) NULL;
			XUnmapWindow(dpy, locattr_frame);
			XDefineCursor(dpy, pg_cwin, hg_cursor);
			XSync(dpy, False);
			pg_draw();
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			ice_op= MAIN_MENU;
		}
		break;
	case AXIS_INSERTLOC1:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Axis Terminus", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "", LXPI_NULL);
		panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "", LXPI_NULL);
		ice_op= AXIS_INSERTLOC2;
		break;
	case AXIS_INSERTLOC2:
		((Axis *) loc_grobj)->setfend(fx, fy, pg_dpi);
		XUnmapWindow(dpy, locattr_frame);
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		ice_op= MAIN_MENU;
		break;
	case PATH_INSERTLOC:
		loc_xvertices[loc_vertex]= fx;
		loc_yvertices[loc_vertex]= fy;
		loc_vertex++;
		if (loc_vertex < loc_nvertices) {
			(void) sprintf(label, "Vertex %1d", loc_vertex+1);
			panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, label, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "", LXPI_NULL);
			panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "", LXPI_NULL);
		}
		else {
			loc_path->setvertices(loc_nvertices, loc_xvertices, loc_yvertices);
			loc_xvertices= loc_yvertices= (float *) NULL;
			XUnmapWindow(dpy, locattr_frame);
			if (loc_path->getvisibility() == PATH_VISIBLE) {
				loc_path->x11draw(pg_dpi, pg_pixheight, pg_cpm);
				canvas_flush(pg_canvas);
			}
			ice_op= MAIN_MENU;
		}
		break;
	}

	return;
}

void
locattrtr_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	int ix, iy, nvertices, i, natoms;
	float fx, fy, ox, oy, *xvertices, *yvertices, xdiff, ydiff;
	Grobj **atoms;
	char errmsg[MAX_ERRMSGLEN+1];

	XUnmapWindow(dpy, locattr_frame);

	buf= (char *) panelitem_get(locattr_panel, locattr_x, LXPTEXT_VALUE);
	fx= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid x value.", NONFATAL);
		switch (ice_op) {
		case PSD_TRANSLATE:
		case RAS_TRANSLATE:
		case TEXT_TRANSLATE:
		case VEC_TRANSLATE:
		case CRV_TRANSLATE:
		case MRK_TRANSLATE:
		case RECT_TRANSLATE:
		case POLY_TRANSLATE:
		case AXIS_TRANSLATE:
		case CMP_TRANSLATE:
		case PATH_TRANSLATE:
			ice_op= MAIN_MENU;
			break;
		case VEC_MVLOC1:
		case VEC_MVLOC2:
			ice_op= VEC_ATTR;
			break;
		case CRV_MVLOC1:
		case CRV_MVLOC2:
		case CRV_MVCNT1:
		case CRV_MVCNT2:
			ice_op= CRV_ATTR;
			break;
		case AXIS_MVLOC1:
		case AXIS_MVLOC2:
			ice_op= AXIS_ATTR;
			break;
		case PSD_COPY:
		case RAS_COPY:
		case TEXT_COPY:
		case VEC_COPY:
		case CRV_COPY:
		case MRK_COPY:
		case RECT_COPY:
		case POLY_COPY:
		case AXIS_COPY:
		case CMP_COPY:
			return;
		}
		return;
	}

	buf= (char *) panelitem_get(locattr_panel, locattr_y, LXPTEXT_VALUE);
	fy= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid y value.", NONFATAL);
		switch (ice_op) {
		case PSD_TRANSLATE:
		case RAS_TRANSLATE:
		case TEXT_TRANSLATE:
		case VEC_TRANSLATE:
		case CRV_TRANSLATE:
		case MRK_TRANSLATE:
		case RECT_TRANSLATE:
		case POLY_TRANSLATE:
		case AXIS_TRANSLATE:
		case CMP_TRANSLATE:
		case PATH_TRANSLATE:
			ice_op= MAIN_MENU;
			break;
		case VEC_MVLOC1:
		case VEC_MVLOC2:
			ice_op= VEC_ATTR;
			break;
		case CRV_MVLOC1:
		case CRV_MVLOC2:
		case CRV_MVCNT1:
		case CRV_MVCNT2:
			ice_op= CRV_ATTR;
			break;
		case AXIS_MVLOC1:
		case AXIS_MVLOC2:
			ice_op= AXIS_ATTR;
			break;
		case PSD_COPY:
		case RAS_COPY:
		case TEXT_COPY:
		case VEC_COPY:
		case CRV_COPY:
		case MRK_COPY:
		case RECT_COPY:
		case POLY_COPY:
		case AXIS_COPY:
		case CMP_COPY:
			return;
		}
		return;
	}

	switch (pg_units) {
	case PG_PIXELS:
		fx/= pg_dpi;
		fy/= pg_dpi;
		break;
	case PG_POINTS:
		fx/= 72.;
		fy/= 72.;
		break;
	case PG_INCHES:
		break;
	case PG_USER:
		fx= ((fx-pg_xru)/pg_hsi)+pg_xri;
		fy= ((fy-pg_yru)/pg_vsi)+pg_yri;
		break;
	}

	switch (ice_op) {
	case PSD_TRANSLATE:
	case RAS_TRANSLATE:
	case TEXT_TRANSLATE:
	case MRK_TRANSLATE:
	case RECT_TRANSLATE:
	case POLY_TRANSLATE:
	case PSD_COPY:
	case RAS_COPY:
	case TEXT_COPY:
	case MRK_COPY:
	case RECT_COPY:
	case POLY_COPY:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		ice_op= MAIN_MENU;
		break;
	case VEC_TRANSLATE:
	case VEC_COPY:
		loc_grobj->getloc(&ox, &oy, &ix, &iy);
		xdiff= fx-ox;
		ydiff= fy-oy;
		loc_grobj->setfloc(fx, fy, pg_dpi);
		((Vector *) loc_grobj)->getend(&ox, &oy, &ix, &iy);
		((Vector *) loc_grobj)->setfend(ox+xdiff, oy+ydiff, pg_dpi);
		ice_op= MAIN_MENU;
		break;
	case CRV_TRANSLATE:
	case CRV_COPY:
		loc_grobj->getloc(&ox, &oy, &ix, &iy);
		xdiff= fx-ox;
		ydiff= fy-oy;
		loc_grobj->setfloc(fx, fy, pg_dpi);
		((Curve *) loc_grobj)->getcontrol1(&ox, &oy, &ix, &iy);
		((Curve *) loc_grobj)->setfcontrol1(ox+xdiff, oy+ydiff, pg_dpi);
		((Curve *) loc_grobj)->getcontrol2(&ox, &oy, &ix, &iy);
		((Curve *) loc_grobj)->setfcontrol2(ox+xdiff, oy+ydiff, pg_dpi);
		((Curve *) loc_grobj)->getend(&ox, &oy, &ix, &iy);
		((Curve *) loc_grobj)->setfend(ox+xdiff, oy+ydiff, pg_dpi);
		ice_op= MAIN_MENU;
		break;
	case AXIS_TRANSLATE:
	case AXIS_COPY:
		loc_grobj->getloc(&ox, &oy, &ix, &iy);
		xdiff= fx-ox;
		ydiff= fy-oy;
		loc_grobj->setfloc(fx, fy, pg_dpi);
		((Axis *) loc_grobj)->getend(&ox, &oy, &ix, &iy);
		((Axis *) loc_grobj)->setfend(ox+xdiff, oy+ydiff, pg_dpi);
		ice_op= MAIN_MENU;
		break;
	case CMP_TRANSLATE:
	case CMP_COPY:
		loc_grobj->getloc(&ox, &oy, &ix, &iy);
		loc_cmpobj->getatoms(&natoms, &atoms);
		for (i= 0; i < natoms; i++)
			cmptr_adjatom(atoms[i], fx-ox, fy-oy);
		ice_op= MAIN_MENU;
		break;
	case PATH_TRANSLATE:
		loc_path->getvertices(&nvertices, &xvertices, &yvertices);
		xdiff= fx-xvertices[0];
		ydiff= fy-yvertices[0];
		for (i= 0; i < nvertices; i++) {
			xvertices[i]+= xdiff;
			yvertices[i]+= ydiff;
		}
		ice_op= MAIN_MENU;
		break;
	case VEC_MVLOC1:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		ice_op= VEC_ATTR;
		break;
	case VEC_MVLOC2:
		((Vector *) loc_grobj)->setfend(fx, fy, pg_dpi);
		ice_op= VEC_ATTR;
		break;
	case CRV_MVLOC1:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		ice_op= CRV_ATTR;
		break;
	case CRV_MVLOC2:
		((Curve *) loc_grobj)->setfend(fx, fy, pg_dpi);
		ice_op= CRV_ATTR;
		break;
	case CRV_MVCNT1:
		((Curve *) loc_grobj)->setfcontrol1(fx, fy, pg_dpi);
		ice_op= CRV_ATTR;
		break;
	case CRV_MVCNT2:
		((Curve *) loc_grobj)->setfcontrol2(fx, fy, pg_dpi);
		ice_op= CRV_ATTR;
		break;
	case AXIS_MVLOC1:
		loc_grobj->setfloc(fx, fy, pg_dpi);
		ice_op= AXIS_ATTR;
		break;
	case AXIS_MVLOC2:
		((Axis *) loc_grobj)->setfend(fx, fy, pg_dpi);
		ice_op= AXIS_ATTR;
		break;
	}

	switch (ice_op) {
	case PSD_COPY:
	case RAS_COPY:
	case TEXT_COPY:
	case VEC_COPY:
	case CRV_COPY:
	case MRK_COPY:
	case RECT_COPY:
	case POLY_COPY:
	case AXIS_COPY:
	case CMP_COPY:
		panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);
	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
locattrabort_proc(Panel *p, Panel_item *pi)
{
	switch (ice_op) {
	case PSD_INSERTLOC:
	case RAS_INSERTLOC:
	case TEXT_INSERTLOC:
	case VEC_INSERTLOC1:
	case VEC_INSERTLOC2:
	case CRV_INSERTLOC1:
	case CRV_INSERTCNT1:
	case CRV_INSERTCNT2:
	case CRV_INSERTLOC2:
	case MRK_INSERTLOC:
	case RECT_INSERTLOC:
	case POLY_INSERTLOC:
	case AXIS_INSERTLOC1:
	case AXIS_INSERTLOC2:
	case PATH_INSERTLOC:
		return;
	case PSD_TRANSLATE:
	case RAS_TRANSLATE:
	case TEXT_TRANSLATE:
	case VEC_TRANSLATE:
	case CRV_TRANSLATE:
	case MRK_TRANSLATE:
	case RECT_TRANSLATE:
	case POLY_TRANSLATE:
	case AXIS_TRANSLATE:
	case CMP_TRANSLATE:
	case PATH_TRANSLATE:
		ice_op= MAIN_MENU;
		break;
	case VEC_MVLOC1:
	case VEC_MVLOC2:
		ice_op= VEC_ATTR;
		break;
	case CRV_MVLOC1:
	case CRV_MVLOC2:
	case CRV_MVCNT1:
	case CRV_MVCNT2:
		ice_op= CRV_ATTR;
		break;
	case AXIS_MVLOC1:
	case AXIS_MVLOC2:
		ice_op= AXIS_ATTR;
		break;
	}

	XUnmapWindow(dpy, locattr_frame);

	return;
}
