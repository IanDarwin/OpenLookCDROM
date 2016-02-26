/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Curve.h"
#include "Pathdup.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrcrv_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cpcrv_proc(Menu *, Menu_item *);
extern void		crv_del(Curve *);
extern void		delcrv_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern int		str2flt(char *, float **, int *);
extern void		trcrv_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

boolean crv_drawn;
int ncrvvert, crvixvert[4], crviyvert[4];

static Curve *new_crv;

void
inscrv_proc(Menu *m, Menu_item *mi)
{
	char buf[80];

	panelitem_set(crvattr_panel, crvattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_mvloc1, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_mvloc2, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_mvcnt1, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_mvcnt2, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		crv_drawn= FALSE;
		ice_op= CRV_INSERTATTR;
		XMapRaised(dpy, crvattr_frame);
		return;
	}

	panelitem_set(crvattr_panel, crvattr_linewd, LXPENUM_VALUE, CURVE_GLOBALWIDTH, LXPI_NULL);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	panelitem_set(crvattr_panel, crvattr_lwidth, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_linestyle, LXPENUM_VALUE, CURVE_SOLID, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_dashstyle, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, CURVE_SIMPLEDASH, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_dashlen, LXPTEXT_VALUE, "4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_dashgaplen, LXPTEXT_VALUE, "4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_dashpattern, LXPTEXT_VALUE, "4 4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_dashoffset, LXPTEXT_VALUE, "0", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_capstyle, LXPENUM_VALUE, CURVE_BUTTCAP, LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_fg, LXPENUM_VALUE, CURVE_GLOBALFG, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfg);
	panelitem_set(crvattr_panel, crvattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	panelitem_set(crvattr_panel, crvattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	panelitem_set(crvattr_panel, crvattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	panelitem_set(crvattr_panel, crvattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	panelitem_set(crvattr_panel, crvattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	panelitem_set(crvattr_panel, crvattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	crv_drawn= FALSE;
	ice_op= CRV_INSERTATTR;
	XMapRaised(dpy, crvattr_frame);
	return;
}

void
crvinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *name, *buf;
	float width, dl, dgl, *dp, dpo;
	int lw, ls, ds, cs, dpl, fg, dtk, seq;
	unsigned char rfg, gfg, bfg;
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char nmbuf[30];
	float fx, fy;
	char xbuf[30], ybuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	XUnmapWindow(dpy, crvattr_frame);
	ice_op= MAIN_MENU;

	dp= (float *) NULL;

	name= (char *) panelitem_get(crvattr_panel, crvattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		(void) sprintf(nmbuf, "UnnamedCurve-%d", unnamed_curves++);
		name= nmbuf;
	}

	lw= *((int *) panelitem_get(crvattr_panel, crvattr_linewd, LXPENUM_VALUE));
	switch (lw) {
	case CURVE_GLOBALWIDTH:
		width= gdf_linewidth;
		break;
	case CURVE_OTHERWIDTH:
		buf= (char *) panelitem_get(crvattr_panel, crvattr_lwidth, LXPTEXT_VALUE);
		width= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid width value.", NONFATAL);
			delete dp;
			return;
		}
		if (width < 0.) {
			ice_err("Width value must be non-negative.", NONFATAL);
			delete dp;
			return;
		}
		break;
	}

	ls= *((int *) panelitem_get(crvattr_panel, crvattr_linestyle, LXPENUM_VALUE));
	switch (ls) {
	case CURVE_SOLID:
		if (str2flt("4 4", &dp, &dpl) != GROBJ_SUCCESS) {
			ice_err("Cannot convert dash pattern.", NONFATAL);
			delete dp;
			return;
		}
		dpo= 0.;
		ds= CURVE_SIMPLEDASH;
		break;
	case CURVE_DASHED:
		ds= *((int *) panelitem_get(crvattr_panel, crvattr_dashstyle, LXPENUM_VALUE));
		switch (ds) {
		case CURVE_SIMPLEDASH:
			buf= (char *) panelitem_get(crvattr_panel, crvattr_dashlen, LXPTEXT_VALUE);
			dl= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid dash length value.", NONFATAL);
				delete dp;
				return;
			}
			if (dl < 0.) {
				ice_err("Dash length value must be non-negative.", NONFATAL);
				delete dp;
				return;
			}
			buf= (char *) panelitem_get(crvattr_panel, crvattr_dashgaplen, LXPTEXT_VALUE);
			dgl= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid dash gap length value.", NONFATAL);
				delete dp;
				return;
			}
			if (dgl < 0.) {
				ice_err("Dash gap length value must be non-negative.", NONFATAL);
				delete dp;
				return;
			}
			if ((dp= new float[2]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				return;
			}
			*dp= dl;
			*(dp+1)= dgl;
			dpl= 2;
			dpo= 0.;
			break;
		case CURVE_COMPLEXDASH:
			buf= (char *) panelitem_get(crvattr_panel, crvattr_dashpattern, LXPTEXT_VALUE);
			if (str2flt(buf, &dp, &dpl) != GROBJ_SUCCESS) {
				ice_err("Cannot convert dash pattern.", NONFATAL);
				delete dp;
				return;
			}
			buf= (char *) panelitem_get(crvattr_panel, crvattr_dashoffset, LXPTEXT_VALUE);
			dpo= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid dash pattern offset value.", NONFATAL);
				delete dp;
				return;
			}
			if (dpo < 0.) {
				ice_err("Dash pattern offset value must be non-negative.", NONFATAL);
				delete dp;
				return;
			}
			break;
		}
		break;
	}

	cs= *((int *) panelitem_get(crvattr_panel, crvattr_capstyle, LXPENUM_VALUE));

	fg= *((int *) panelitem_get(crvattr_panel, crvattr_fg, LXPENUM_VALUE));
	switch (fg) {
	case CURVE_GLOBALFG:
		rfg= gdf_rfg;
		gfg= gdf_gfg;
		bfg= gdf_bfg;
		break;
	case CURVE_BLACKFG:
		rfg= gfg= bfg= 0;
		break;
	case CURVE_WHITEFG:
		rfg= gfg= bfg= 255;
		break;
	case CURVE_OTHERFG:
		buf= (char *) panelitem_get(crvattr_panel, crvattr_rfg, LXPTEXT_VALUE);
		rfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red foreground value.", NONFATAL);
			delete dp;
			return;
		}
		if ((rfg < 0) || (rfg > 255)) {
			ice_err("Invalid red foreground value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(crvattr_panel, crvattr_gfg, LXPTEXT_VALUE);
		gfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green foreground value.", NONFATAL);
			delete dp;
			return;
		}
		if ((gfg < 0) || (gfg > 255)) {
			ice_err("Invalid green foreground value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(crvattr_panel, crvattr_bfg, LXPTEXT_VALUE);
		bfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue foreground value.", NONFATAL);
			delete dp;
			return;
		}
		if ((bfg < 0) || (bfg > 255)) {
			ice_err("Invalid blue foreground value.", NONFATAL);
			delete dp;
			return;
		}
		break;
	}

	pathnm= (char *) panelitem_get(crvattr_panel, crvattr_clip, LXPTEXT_VALUE);
	if (strlen(pathnm) == 0)
		pth= (Path *) NULL;
	else {
		for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
			if (!strcmp(pathnm, pth->getname()))
				break;
		}
		if (pth == (Path *) NULL) {
			ice_err("Specified path does not exist.", NONFATAL);
			delete dp;
			return;
		}
	}

	dtk= *((int *) panelitem_get(crvattr_panel, crvattr_dtk, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
		rdtk= gdf_rdtk;
		gdtk= gdf_gdtk;
		bdtk= gdf_bdtk;
		break;
	case GROBJ_WHITEDTK:
		rdtk= gdtk= bdtk= 255;
		break;
	case GROBJ_BLACKDTK:
		rdtk= gdtk= bdtk= 0;
		break;
	case GROBJ_OTHERDTK:
		buf= (char *) panelitem_get(crvattr_panel, crvattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(crvattr_panel, crvattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			delete dp;
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(crvattr_panel, crvattr_bdtk, LXPTEXT_VALUE);
		bdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		if ((bdtk < 0) || (bdtk > 255)) {
			ice_err("Invalid blue dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		break;
	}

	buf= (char *) panelitem_get(crvattr_panel, crvattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		delete dp;
		return;
	}

	if ((new_crv= new Curve((Dlnk **) &grobjs, name, seq)) == (Curve *) NULL) {
		ice_err("Cannot create curve object.", NONFATAL);
		delete dp;
		return;
	}
	ncurves++;
	(void) new_crv->setwidth(lw, width);
	(void) new_crv->setlinestyle(ls);
	(void) new_crv->setdashstyle(ds, dp, dpl, dpo);
	(void) new_crv->setcapstyle(cs);
	(void) new_crv->setforeground(fg, rfg, gfg, bfg);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_crv->setclip(pth);

	(void) new_crv->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_crv->setdtkpix(dtkpix);

	ice_op= CRV_INSERTLOC1;

	if (pg_loc == PG_CURSORLOC) {
		ncrvvert= 0;
		pgcoords_drawn= crosshairs_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, delcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, attrcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_crv,
				LXMI_PROC, trcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(new_crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
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
		ice_op= MAIN_MENU;
		return;
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
		ice_op= MAIN_MENU;
		return;
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
		ice_op= MAIN_MENU;
		return;
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

	loc_grobj= (Grobj *) new_crv;
	panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Curve Origin", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	switch (pg_units) {
	case PG_PIXELS:
	case PG_POINTS:
	case PG_INCHES:
		panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, "0", LXPI_NULL);
		panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, "0", LXPI_NULL);
		break;
	case PG_USER:
		fx= (float) (-pg_xri*pg_hsi)+pg_xru;
		fy= (float) (-pg_yri*pg_vsi)+pg_yru;
		(void) sprintf(xbuf, "%4.2f", fx);
		(void) sprintf(ybuf, "%4.2f", fy);
		panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, xbuf, LXPI_NULL);
		panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, ybuf, LXPI_NULL);
		break;
	}
	XMapRaised(dpy, locattr_frame);
}

void
insicecrv_rd(FILE *fp, int gdf, int newobj)
{
	char *name, *c;
	float width, dl, dgl, *dp, dpo;
	int lw, ls, ds, cs, dpl, dplx;
	int len, fg, dtk, seq;
	int ir, ig, ib;
	float ox, oy, tx, ty;
	float c1x, c1y, c2x, c2y;
	boolean endfound;
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	unsigned char rfg, gfg, bfg;
	int iot;
	char nmbuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Curve *crv;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Crv: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Crv: Begin ")+len-1)= '\0';
	if ((name= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(name, ice_iobuf+strlen("%%ICE-Crv: Begin "));
	if (!strncmp(name, "UnnamedCurve-", strlen("UnnamedCurve-"))) {
		delete name;
		(void) sprintf(nmbuf, "UnnamedCurve-%d", unnamed_curves++);
		if ((name= new char[strlen(nmbuf)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(name, nmbuf);
	}

	ox= oy= tx= ty= 0.;
	c1x= c1y= c2x= c2y= 0.;
	lw= CURVE_GLOBALWIDTH;
	width= gdf_linewidth;
	ls= CURVE_SOLID;
	ds= CURVE_SIMPLEDASH;
	dl= dgl= 4.;
	if (str2flt("4 4", &dp, &dpl) != GROBJ_SUCCESS) {
		ice_err("Cannot convert dash pattern.", NONFATAL);
		delete name;
		return;
	}
	dpo= 0.;
	cs= CURVE_BUTTCAP;
	fg= CURVE_GLOBALFG;
	rfg= gdf_rfg;
	gfg= gdf_gfg;
	bfg= gdf_bfg;
	pathnm= (char *) NULL;
	pth= (Path *) NULL;
	dtk= GROBJ_GLOBALDTK;
	rdtk= gdf_rdtk;
	gdtk= gdf_gdtk;
	bdtk= gdf_bdtk;
	seq= 0;
	iot= GROBJ_NULLIOTAG;

	endfound= FALSE;
	while (fgets(ice_iobuf, INPUT_BUFSZ, fp) != (char *) NULL) {
		insice_lineno++;

		if (strncmp("%%ICE-", ice_iobuf, strlen("%%ICE-")))
			continue;

		if (!strncmp("%%ICE-Crv: Loc ", ice_iobuf, strlen("%%ICE-Crv: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: Loc "), "%f %f %f %f", &ox, &oy, &tx, &ty) != 4) {
				ice_err("Invalid curve location value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Crv: Cnt ", ice_iobuf, strlen("%%ICE-Crv: Cnt "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: Cnt "), "%f %f %f %f", &c1x, &c1y, &c2x, &c2y) != 4) {
				ice_err("Invalid curve control value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Crv: Line ", ice_iobuf, strlen("%%ICE-Crv: Line "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: Line "), "%d %f %d %d", &lw, &width, &ls, &cs) != 4) {
				ice_err("Invalid curve line value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((lw != CURVE_GLOBALWIDTH) &&
			    (lw != CURVE_OTHERWIDTH)) {
				ice_err("Invalid curve line width value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (width < 0.) {
				ice_err("Invalid curve line width value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ls != CURVE_SOLID) &&
			    (ls != CURVE_DASHED)) {
				ice_err("Invalid curve line style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((cs != CURVE_BUTTCAP) &&
			    (cs != CURVE_ROUNDCAP) &&
			    (cs != CURVE_SQUARECAP)) {
				ice_err("Invalid curve cap style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Crv: Dash ", ice_iobuf, strlen("%%ICE-Crv: Dash "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: Dash "), "%d %f %d", &ds, &dpo, &dpl) != 3) {
				ice_err("Invalid curve dash value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ds != CURVE_SIMPLEDASH) &&
			    (ds != CURVE_COMPLEXDASH)) {
				ice_err("Invalid curve dash style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (dpo < 0.) {
				ice_err("Invalid curve dash pattern offset value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (dpl <= 0) {
				ice_err("Invalid dash pattern length value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			delete dp;
			c= ice_iobuf+strlen("%%ICE-Crv: Dash ");
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			*(c+strlen(c)-1)= '\0';
			if (str2flt(c, &dp, &dplx) != GROBJ_SUCCESS) {
				ice_err("Cannot convert dash pattern.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (dplx != dpl) {
				ice_err("Cannot read dash pattern.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Crv: FG ", ice_iobuf, strlen("%%ICE-Crv: FG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: FG "), "%d %d %d %d", &fg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid curve foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((fg != CURVE_GLOBALFG) &&
			    (fg != CURVE_BLACKFG) &&
			    (fg != CURVE_WHITEFG) &&
			    (fg != CURVE_OTHERFG)) {
				ice_err("Invalid curve foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid curve red foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid curve green foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid curve blue foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			rfg= (unsigned char) ir;
			gfg= (unsigned char) ig;
			bfg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Crv: Clip ", ice_iobuf, strlen("%%ICE-Crv: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Crv: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete dp;
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strncmp("%%ICE-Crv: DTK ", ice_iobuf, strlen("%%ICE-Crv: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid curve DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid curve DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid curve red DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid curve green DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid curve blue DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Crv: Seq ", ice_iobuf, strlen("%%ICE-Crv: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid curve sequence value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Crv: IOT ", ice_iobuf, strlen("%%ICE-Crv: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Crv: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid curve tag value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid curve tag value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Crv: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Curve end not found.", NONFATAL);
		delete name;
		delete dp;
		delete pathnm;
		return;
	}

	if (pathnm != (char *) NULL) {
		char *oldname, *newname;
		Pathdup *duppth;

		for (duppth= duppaths; duppth != (Pathdup *) NULL; duppth= (Pathdup *) duppth->succ()) {
			duppth->getnames(&oldname, &newname);
			if (!strcmp(pathnm, oldname)) {
				delete pathnm;
				if ((pathnm= new char[strlen(newname)+1]) == (char *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete name;
					delete dp;
					return;
				}
				(void) strcpy(pathnm, newname);
				break;
			}
		}
		for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
			if (!strcmp(pathnm, pth->getname()))
				break;
		}
		if (pth == (Path *) NULL) {
			ice_err("Specified path does not exist.", NONFATAL);
			delete name;
			delete dp;
			delete pathnm;
			return;
		}
	}
	else
		pth= (Path *) NULL;
	delete pathnm;

	if (gdf == INSICE_CURRGDF) {
		switch (newobj) {
		case INSICE_NEWOBJCURRGDF:
			if (lw == CURVE_GLOBALWIDTH)
				width= gdf_linewidth;
			if (fg == CURVE_GLOBALFG) {
				rfg= gdf_rfg;
				gfg= gdf_gfg;
				bfg= gdf_bfg;
			}
			if (dtk == GROBJ_GLOBALDTK) {
				rdtk= gdf_rdtk;
				gdtk= gdf_gdtk;
				bdtk= gdf_bdtk;
			}
			break;
		case INSICE_NEWOBJNEWGDF:
			if (lw == CURVE_GLOBALWIDTH)
				lw= CURVE_OTHERWIDTH;
			if (fg == CURVE_GLOBALFG)
				fg= CURVE_OTHERFG;
			if (dtk == GROBJ_GLOBALDTK)
				dtk= GROBJ_OTHERDTK;
			break;
		}
	}

	if ((crv= new Curve((Dlnk **) &grobjs, name, seq)) == (Curve *) NULL) {
		ice_err("Cannot create curve object.", NONFATAL);
		delete name;
		delete dp;
		return;
	}
	delete name;
	ncurves++;

	(void) crv->setwidth(lw, width);
	(void) crv->setlinestyle(ls);
	(void) crv->setdashstyle(ds, dp, dpl, dpo);
	(void) crv->setcapstyle(cs);
	(void) crv->setforeground(fg, rfg, gfg, bfg);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	crv->setclip(pth);
	crv->setiotag(iot);

	(void) crv->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	crv->setdtkpix(dtkpix);

	crv->setfloc(ox, oy, (float) pg_dpi);
	crv->setfend(tx, ty, (float) pg_dpi);
	crv->setfcontrol1(c1x, c1y, (float) pg_dpi);
	crv->setfcontrol2(c2x, c2y, (float) pg_dpi);

	if ((delitem= menuitem_create(LXMI_STRING, crv->getname(),
				LXMI_CLIENTDATA, (char *) crv,
				LXMI_PROC, delcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(crv);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, crv->getname(),
				LXMI_CLIENTDATA, (char *) crv,
				LXMI_PROC, attrcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(crv);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, crv->getname(),
				LXMI_CLIENTDATA, (char *) crv,
				LXMI_PROC, trcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, crv->getname(),
				LXMI_CLIENTDATA, (char *) crv,
				LXMI_PROC, cpcrv_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, crv->getname(),
				LXMI_CLIENTDATA, (char *) crv,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, crv->getname(),
				LXMI_CLIENTDATA, (char *) crv,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		crv_del(crv);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
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

	return;
}

void
crvins_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, junk;
	float fx, fy;
	Menu_item *delitem, *attritem;
	Menu_item *tritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;
	void crv_drawoutline();

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (crv_drawn) {
			crv_drawoutline();
			crv_drawn= FALSE;
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		switch (ice_op) {
		case CRV_INSERTLOC1:
			new_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			crvixvert[ncrvvert]= x;
			crviyvert[ncrvvert]= y;
			ncrvvert++;
			ice_op= CRV_INSERTCNT1;
			return;
		case CRV_INSERTCNT1:
			new_crv->setpcontrol1(x, pg_pixheight-1-y, (float) pg_dpi);
			crvixvert[ncrvvert]= x;
			crviyvert[ncrvvert]= y;
			ncrvvert++;
			ice_op= CRV_INSERTCNT2;
			return;
		case CRV_INSERTCNT2:
			new_crv->setpcontrol2(x, pg_pixheight-1-y, (float) pg_dpi);
			crvixvert[ncrvvert]= x;
			crviyvert[ncrvvert]= y;
			ncrvvert++;
			ice_op= CRV_INSERTLOC2;
			return;
		}
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		new_crv->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
		if ((delitem= menuitem_create(LXMI_STRING, new_crv->getname(),
					LXMI_CLIENTDATA, (char *) new_crv,
					LXMI_PROC, delcrv_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			crv_del(new_crv);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_crv->getname(),
					LXMI_CLIENTDATA, (char *) new_crv,
					LXMI_PROC, attrcrv_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			crv_del(new_crv);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_crv->getname(),
					LXMI_CLIENTDATA, (char *) new_crv,
					LXMI_PROC, trcrv_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			crv_del(new_crv);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_crv->getname(),
					LXMI_CLIENTDATA, (char *) new_crv,
					LXMI_PROC, cpcrv_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			crv_del(new_crv);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_crv->getname(),
					LXMI_CLIENTDATA, (char *) new_crv,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			crv_del(new_crv);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			menuitem_destroy(cpitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_crv->getname(),
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
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
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
		switch (ice_op) {
		case CRV_INSERTLOC1:
			new_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getloc(&fx, &fy, &x, &junk);
			break;
		case CRV_INSERTCNT1:
			new_crv->setpcontrol1(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getcontrol1(&fx, &fy, &x, &junk);
			break;
		case CRV_INSERTCNT2:
			new_crv->setpcontrol2(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getcontrol2(&fx, &fy, &x, &junk);
			break;
		case CRV_INSERTLOC2:
			new_crv->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getend(&fx, &fy, &x, &junk);
			break;
		}
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		switch (ice_op) {
		case CRV_INSERTLOC1:
			new_crv->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getloc(&fx, &fy, &x, &junk);
			break;
		case CRV_INSERTCNT1:
			new_crv->setpcontrol1(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getcontrol1(&fx, &fy, &x, &junk);
			break;
		case CRV_INSERTCNT2:
			new_crv->setpcontrol2(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getcontrol2(&fx, &fy, &x, &junk);
			break;
		case CRV_INSERTLOC2:
			new_crv->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			new_crv->getend(&fx, &fy, &x, &junk);
			break;
		}
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (crv_drawn)
		crv_drawoutline();
	if (ncrvvert > 0) {
		crvixvert[ncrvvert]= x;
		crviyvert[ncrvvert]= y;
		crv_drawoutline();
		crv_drawn= TRUE;
	}
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
delcrv_proc(Menu *m, Menu_item *mi)
{
	Curve *crv;
	Menu_item *item;

	if ((crv= (Curve *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Curve *) NULL) {
		ice_err("Cannot locate selected curve object.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	crv_del(crv);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrcrv_menu, LXMI_CLIENTDATA, (char *) crv);
	menuitem_delete(attrcrv_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trcrv_menu, LXMI_CLIENTDATA, (char *) crv);
	menuitem_delete(trcrv_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cpcrv_menu, LXMI_CLIENTDATA, (char *) crv);
	menuitem_delete(cpcrv_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmpcrv_menu, LXMI_CLIENTDATA, (char *) crv);
	menuitem_delete(dmpcrv_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpopcrv_menu, LXMI_CLIENTDATA, (char *) crv);
	menuitem_delete(cmpopcrv_menu, item);
	menuitem_destroy(item);
	if (ncurves == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}
	if (grobjs == (Grobj *) NULL) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		pg_draw();
		XSync(dpy, False);
		XDefineCursor(dpy, pg_cwin, std_cursor);
	}
	return;
}

void
crv_del(Curve *crv)
{
	if (grobjs == (Grobj *) crv) {
		if (crv->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) crv->succ();
	}
	crv->unlink();
	delete crv;
	ncurves--;
	return;
}

void
crv_drawoutline()
{
	int vx, vy;
	int x0, x1, y0, y1, i;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	x0= crvixvert[0];
	y0= crviyvert[0];
	for (i= 1; (i < ncrvvert+1) && (i < 4); i++, x0= x1, y0= y1) {
		x1= crvixvert[i];
		y1= crviyvert[i];
		XDrawLine(dpy, pg_cpm, pg_xgc, x0, y0, x1, y1);
		XDrawLine(dpy, pg_cwin, pg_xgc, x0-vx, y0-vy, x1-vx, y1-vy);
	}
	return;
}
