/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Axis.h"

extern "C" {
int			strlen(char *);
}

extern void	attraxis_proc(Menu *, Menu_item *);
extern void	axis_del(Axis *);
extern void	cmpop_proc(Menu *, Menu_item *);
extern int	copy_grobj(Grobj *, Grobj *);
extern void	delaxis_proc(Menu *, Menu_item *);
extern void	dmpobj_proc(Menu *, Menu_item *);
extern void	ice_err(char *, int);
extern void	traxis_proc(Menu *, Menu_item *);

extern Grobj *loc_grobj;

extern Axis *tr_axis;
extern boolean axis_drawn;
extern int axis_x, axis_y;
extern int axis_ex, axis_ey;
extern int axis_xinitpos, axis_yinitpos;
extern int axis_xdiff, axis_ydiff;

void
cpaxis_proc(Menu *m, Menu_item *mi)
{
	Axis *axis;
	int val;
	char name[LXADEF_MAXSTORE+1];
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];
	Axis *axis_copy(Axis *, char *);

	if ((axis= (Axis *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Axis *) NULL) {
		ice_err("Cannot locate selected axis object.", NONFATAL);
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
		(void) sprintf(name, "UnnamedAxis-%d", unnamed_axes++);

	if ((tr_axis= axis_copy(axis, name)) == (Axis *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= AXIS_COPY;

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

Axis *
axis_copy(Axis *axis, char *name)
{
	Axis *new_axis;
	int ix, iy, l, sd, aw, tw, f, clr;
	float fx, fy, w, pt, st, tt, fs, fo;
	double orig, term;
	char *fn;
	unsigned char r, g, b;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	if ((new_axis= new Axis((Dlnk **) &grobjs, name, axis->getsequence())) == (Axis *) NULL) {
		ice_err("Cannot create axis object.", NONFATAL);
		return (Axis *) NULL;
	}
	naxes++;

	if (copy_grobj(axis, new_axis) != GROBJ_SUCCESS) {
		axis_del(new_axis);
		return (Axis *) NULL;
	}
	axis->getend(&fx, &fy, &ix, &iy);
	(void) new_axis->setfend(fx, fy, pg_dpi);
	axis->getotl(&orig, &term, &l);
	(void) new_axis->setotl(orig, term, l);
	sd= axis->getsubdiv();
	(void) new_axis->setsubdiv(sd);
	axis->getaxiswidth(&aw, &w);
	(void) new_axis->setaxiswidth(aw, w);
	axis->gettick(&l, &pt, &st, &tt, &tw, &w);
	(void) new_axis->settick(l, pt, st, tt, tw, w);
	axis->getfont(&f, &fn);
	(void) new_axis->setfont(f, fn);
	axis->getfontsize(&f, &fs);
	(void) new_axis->setfontsize(f, fs);
	axis->getlabelattr(&l, &f, &fo);
	(void) new_axis->setlabelattr(l, f, fo);
	axis->getline(&clr, &r, &g, &b);
	(void) new_axis->setline(clr, r, g, b);
	axis->getlabel(&clr, &r, &g, &b);
	(void) new_axis->setlabel(clr, r, g, b);

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, delaxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		return (Axis *) NULL;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, attraxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		menuitem_destroy(delitem);
		return (Axis *) NULL;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, traxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return (Axis *) NULL;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, cpaxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return (Axis *) NULL;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return (Axis *) NULL;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return (Axis *) NULL;
	}
	(void) menuitem_insert(delaxis_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attraxis_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(traxis_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpaxis_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpaxis_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopaxis_menu, cmpopitem);

	return new_axis;
}
