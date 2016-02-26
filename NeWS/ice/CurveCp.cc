/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Curve.h"

extern "C" {
int			strlen(char *);
}

extern void	attrcrv_proc(Menu *, Menu_item *);
extern void	cmpop_proc(Menu *, Menu_item *);
extern int	copy_grobj(Grobj *, Grobj *);
extern void	crv_del(Curve *);
extern void	delcrv_proc(Menu *, Menu_item *);
extern void	dmpobj_proc(Menu *, Menu_item *);
extern void	ice_err(char *, int);
extern void	trcrv_proc(Menu *, Menu_item *);

extern Grobj *loc_grobj;

extern Curve *tr_crv;
extern boolean crv_drawn;
extern int ncrvvert, crvixvert[], crviyvert[];
extern int crvoixvert[], crvoiyvert[];

void
cpcrv_proc(Menu *m, Menu_item *mi)
{
	Curve *crv;
	int val;
	char name[LXADEF_MAXSTORE+1];
	float fx, fy;
	int ix, iy, i;
	char xbuf[30], ybuf[30];
	Curve *crv_copy(Curve *, char *);

	if ((crv= (Curve *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Curve *) NULL) {
		ice_err("Cannot locate selected curve object.", NONFATAL);
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
		(void) sprintf(name, "UnnamedCurve-%d", unnamed_curves++);

	if ((tr_crv= crv_copy(crv, name)) == (Curve *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= CRV_COPY;

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

Curve *
crv_copy(Curve *crv, char *name)
{
	Curve *new_crv;
	int ix, iy, lw, style, dpl, i, clr;
	float fx, fy, w, dpo;
	float *odp, *ndp;
	unsigned char r, g, b;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	if ((new_crv= new Curve((Dlnk **) &grobjs, name, crv->getsequence())) == (Curve *) NULL) {
		ice_err("Cannot create curve object.", NONFATAL);
		return (Curve *) NULL;
	}
	ncurves++;

	if (copy_grobj(crv, new_crv) != GROBJ_SUCCESS) {
		crv_del(new_crv);
		return (Curve *) NULL;
	}
	crv->getend(&fx, &fy, &ix, &iy);
	(void) new_crv->setfend(fx, fy, pg_dpi);
	crv->getcontrol1(&fx, &fy, &ix, &iy);
	(void) new_crv->setfcontrol1(fx, fy, pg_dpi);
	crv->getcontrol2(&fx, &fy, &ix, &iy);
	(void) new_crv->setfcontrol2(fx, fy, pg_dpi);
	crv->getwidth(&lw, &w);
	(void) new_crv->setwidth(lw, w);
	style= crv->getlinestyle();
	(void) new_crv->setlinestyle(style);
	crv->getdashstyle(&style, &odp, &dpl, &dpo);
	if (dpl > 0) {
		if ((ndp= new float[dpl]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			crv_del(new_crv);
			return (Curve *) NULL;
		}
		for (i= 0; i < dpl; i++)
			ndp[i]= odp[i];
	}
	else
		ndp= (float *) NULL;
	(void) new_crv->setdashstyle(style, ndp, dpl, dpo);
	style= crv->getcapstyle();
	(void) new_crv->setcapstyle(style);
	crv->getforeground(&clr, &r, &g, &b);
	(void) new_crv->setforeground(clr, r, g, b);

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, delcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		return (Curve *) NULL;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, attrcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		menuitem_destroy(delitem);
		return (Curve *) NULL;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, trcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return (Curve *) NULL;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, cpcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return (Curve *) NULL;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return (Curve *) NULL;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return (Curve *) NULL;
	}
	(void) menuitem_insert(delcrv_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrcrv_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trcrv_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpcrv_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpcrv_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopcrv_menu, cmpopitem);

	return new_crv;
}
