/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Raster.h"

extern void	attrras_proc(Menu *, Menu_item *);
extern void	cmapras_proc(Menu *, Menu_item *);
extern void	cmpop_proc(Menu *, Menu_item *);
extern int	copy_grobj(Grobj *, Grobj *);
extern void	delras_proc(Menu *, Menu_item *);
extern void	dmpobj_proc(Menu *, Menu_item *);
extern FILE *	doc_open(char *, char *);
extern void	ice_err(char *, int);
extern void	ras_del(Raster *);
extern void	trras_proc(Menu *, Menu_item *);

extern Grobj *loc_grobj;

extern Raster *tr_ras;

void
cpras_proc(Menu *m, Menu_item *mi)
{
	Raster *ras;
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];
	Raster *ras_copy(Raster *, char *);

	if ((ras= (Raster *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Raster *) NULL) {
		ice_err("Cannot locate selected raster.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	if ((tr_ras= ras_copy(ras, ras->getname())) == (Raster *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= RAS_COPY;

	if (pg_loc == PG_CURSORLOC) {
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) tr_ras;
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

Raster *
ras_copy(Raster *ras, char *name)
{
	Raster *new_ras;
	FILE *fp;
	int pr, ol, dm, w, h, d, clr;
	unsigned long pix;
	unsigned char r, g, b;
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem, *cmapitem;

	if ((fp= doc_open(name, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", name);
		ice_err(errmsg, NONFATAL);
		return (Raster *) NULL;
	}
	(void) fclose(fp);

	if ((new_ras= new Raster((Dlnk **) &grobjs, name, dpy, visual, fg_pixel, pg_pixdepth, ras->getsequence())) == (Raster *) NULL) {
		ice_err("Cannot create raster.", NONFATAL);
		return (Raster *) NULL;
	}
	nrasters++;
	if (new_ras->getname() == (char *) NULL) {
		ice_err("Raster load error.", NONFATAL);
		ras_del(new_ras);
		return (Raster *) NULL;
	}

	if (copy_grobj(ras, new_ras) != GROBJ_SUCCESS) {
		ras_del(new_ras);
		return (Raster *) NULL;
	}
	pr= ras->getpixrep();
	(void) new_ras->setpixrep(pr);
	ol= ras->getorigloc();
	(void) new_ras->setorigloc(ol);
	dm= ras->getdrawmode();
	(void) new_ras->setdrawmode(dm);
	new_ras->getrassize(&w, &h, &d);
	if (d == 1) {
		ras->getfg(&clr, &pix, &r, &g, &b);
		(void) new_ras->setfg(clr, pix, r, g, b);
		ras->getbg(&clr, &pix, &r, &g, &b);
		(void) new_ras->setbg(clr, pix, r, g, b);
	}

	if (dm == RASTER_FULL) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		if (new_ras->bldimage() != GROBJ_SUCCESS) {
			ice_err("Raster load error.", NONFATAL);
			ras_del(new_ras);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			return (Raster *) NULL;
		}
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, delras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		return (Raster *) NULL;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, attrras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		return (Raster *) NULL;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, trras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return (Raster *) NULL;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, cpras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return (Raster *) NULL;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return (Raster *) NULL;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return (Raster *) NULL;
	}
	if ((pg_pixdepth >= 8) && (d >= 8)) {
		if ((cmapitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, cmapras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			menuitem_destroy(cpitem);
			menuitem_destroy(dmpitem);
			menuitem_destroy(cmpopitem);
			return (Raster *) NULL;
		}
		if (new_ras->getcolormap() == None)
			(void) menuitem_set(cmapitem, LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}
	(void) menuitem_insert(delras_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrras_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trras_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpras_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpras_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopras_menu, cmpopitem);
	if ((pg_pixdepth >= 8) && (d >= 8))
		(void) menuitem_insert(cmap_menu, cmapitem);

	return new_ras;
}
