/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Polygon.h"

extern "C" {
int			strlen(char *);
}

extern void	attrpoly_proc(Menu *, Menu_item *);
extern void	cmpop_proc(Menu *, Menu_item *);
extern int	copy_grobj(Grobj *, Grobj *);
extern void	delpoly_proc(Menu *, Menu_item *);
extern void	dmpobj_proc(Menu *, Menu_item *);
extern void	ice_err(char *, int);
extern void	poly_del(Polygon *);
extern void	trpoly_proc(Menu *, Menu_item *);

extern Grobj *loc_grobj;

extern Polygon *tr_poly;

void
cppoly_proc(Menu *m, Menu_item *mi)
{
	Polygon *poly;
	int val;
	char name[LXADEF_MAXSTORE+1];
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];
	Polygon *poly_copy(Polygon *, char *);

	if ((poly= (Polygon *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Polygon *) NULL) {
		ice_err("Cannot locate selected polygon object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	if (alert_prompt(progname, dpy, &val,
			LXA_TEXT, "Name:", "", name,
			LXA_BUTTON, "Continue", 0,
			LXA_BUTTON, "Abort", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	if (val == 1) {
		ice_op= MAIN_MENU;
		return;
	}

	if (strlen(name) == 0)
		(void) sprintf(name, "UnnamedPolygon-%d", unnamed_polygons++);

	if ((tr_poly= poly_copy(poly, name)) == (Polygon *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= POLY_COPY;

	if (pg_loc == PG_CURSORLOC) {
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) tr_poly;
	loc_grobj->getloc(&fx, &fy, &ix, &iy);
	panelitem_set(locattr_panel, locattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
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

Polygon *
poly_copy(Polygon *poly, char *name)
{
	Polygon *new_poly;
	int nv, cl, mode, clr, bw, i;
	float w;
	float *ovx, *ovy, *nvx, *nvy;
	unsigned char r, g, b;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	if ((new_poly= new Polygon((Dlnk **) &grobjs, name, poly->getsequence())) == (Polygon *) NULL) {
		ice_err("Cannot create polygon object.", NONFATAL);
		return (Polygon *) NULL;
	}
	npolygons++;

	if (copy_grobj(poly, new_poly) != GROBJ_SUCCESS) {
		poly_del(new_poly);
		return (Polygon *) NULL;
	}
	poly->getvertices(&nv, &ovx, &ovy);
	if (nv > 0) {
		if ((nvx= new float[nv]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			poly_del(new_poly);
			return (Polygon *) NULL;
		}
		for (i= 0; i < nv; i++)
			nvx[i]= ovx[i];
		if ((nvy= new float[nv]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete nvx;
			poly_del(new_poly);
			return (Polygon *) NULL;
		}
		for (i= 0; i < nv; i++)
			nvy[i]= ovy[i];
	}
	else {
		nvx= (float *) NULL;
		nvy= (float *) NULL;
	}
	(void) new_poly->setvertices(nv, nvx, nvy);
	cl= poly->getclosure();
	(void) new_poly->setclosure(cl);
	poly->getboundary(&mode, &bw, &w, &clr, &r, &g, &b);
	(void) new_poly->setboundary(mode, bw, w, clr, r, g, b);
	poly->getfill(&mode, &clr, &r, &g, &b);
	(void) new_poly->setfill(mode, clr, r, g, b);

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, delpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		return (Polygon *) NULL;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, attrpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		return (Polygon *) NULL;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, trpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return (Polygon *) NULL;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, cppoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return (Polygon *) NULL;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return (Polygon *) NULL;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return (Polygon *) NULL;
	}
	(void) menuitem_insert(delpoly_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpoly_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpoly_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cppoly_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppoly_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoppoly_menu, cmpopitem);

	return new_poly;
}
