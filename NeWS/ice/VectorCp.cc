/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Vector.h"

extern "C" {
int			strlen(char *);
}

extern void	attrvec_proc(Menu *, Menu_item *);
extern void	cmpop_proc(Menu *, Menu_item *);
extern int	copy_grobj(Grobj *, Grobj *);
extern void	delvec_proc(Menu *, Menu_item *);
extern void	dmpobj_proc(Menu *, Menu_item *);
extern void	ice_err(char *, int);
extern void	trvec_proc(Menu *, Menu_item *);
extern void	vec_del(Vector *);

extern Grobj *loc_grobj;

extern Vector *tr_vec;
extern boolean vec_drawn;
extern int vec_x, vec_y;
extern int vec_ex, vec_ey;
extern int vec_xinitpos, vec_yinitpos;
extern int vec_xdiff, vec_ydiff;

void
cpvec_proc(Menu *m, Menu_item *mi)
{
	Vector *vec;
	int val;
	char name[LXADEF_MAXSTORE+1];
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];
	Vector *vec_copy(Vector *, char *);

	if ((vec= (Vector *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Vector *) NULL) {
		ice_err("Cannot locate selected vector object.", NONFATAL);
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
		(void) sprintf(name, "UnnamedVector-%d", unnamed_vectors++);

	if ((tr_vec= vec_copy(vec, name)) == (Vector *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= VEC_COPY;

	if (pg_loc == PG_CURSORLOC) {
		tr_vec->getloc(&fx, &fy, &vec_xinitpos, &vec_yinitpos);
		vec_x= vec_xinitpos;
		vec_y= pg_pixheight-1-vec_yinitpos;
		tr_vec->getend(&fx, &fy, &vec_ex, &vec_ey);
		vec_ey= pg_pixheight-1-vec_ey;
		vec_xdiff= vec_ex-vec_x;
		vec_ydiff= vec_ey-vec_y;
		vec_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) tr_vec;
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

Vector *
vec_copy(Vector *vec, char *name)
{
	Vector *new_vec;
	int ix, iy, lw, style, dpl, p, clr;
	float fx, fy, w, dpo, po, pi;
	float *odp, *ndp;
	unsigned char r, g, b;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	if ((new_vec= new Vector((Dlnk **) &grobjs, name, vec->getsequence())) == (Vector *) NULL) {
		ice_err("Cannot create vector object.", NONFATAL);
		return (Vector *) NULL;
	}
	nvectors++;

	if (copy_grobj(vec, new_vec) != GROBJ_SUCCESS) {
		vec_del(new_vec);
		return (Vector *) NULL;
	}
	vec->getend(&fx, &fy, &ix, &iy);
	(void) new_vec->setfend(fx, fy, pg_dpi);
	vec->getwidth(&lw, &w);
	(void) new_vec->setwidth(lw, w);
	style= vec->getlinestyle();
	(void) new_vec->setlinestyle(style);
	vec->getdashstyle(&style, &odp, &dpl, &dpo);
	if (dpl > 0) {
		if ((ndp= new float[dpl]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			vec_del(new_vec);
			return (Vector *) NULL;
		}
		for (p= 0; p < dpl; p++)
			ndp[p]= odp[p];
	}
	else
		ndp= (float *) NULL;
	(void) new_vec->setdashstyle(style, ndp, dpl, dpo);
	style= vec->getcapstyle();
	(void) new_vec->setcapstyle(style);
	vec->getptrstyle(&p, &style, &w, &po, &pi);
	(void) new_vec->setptrstyle(p, style, w, po, pi);
	vec->getforeground(&clr, &r, &g, &b);
	(void) new_vec->setforeground(clr, r, g, b);

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, delvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		return (Vector *) NULL;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, attrvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		return (Vector *) NULL;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, trvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return (Vector *) NULL;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, cpvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return (Vector *) NULL;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return (Vector *) NULL;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return (Vector *) NULL;
	}
	(void) menuitem_insert(delvec_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrvec_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trvec_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpvec_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpvec_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopvec_menu, cmpopitem);

	return new_vec;
}
