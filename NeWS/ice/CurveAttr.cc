/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Curve.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		crvinscont_proc(Panel *, Panel_item *);
extern int		flt2str(float *, int, char **);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern int		str2flt(char *, float **, int *);

extern Grobj *loc_grobj;
extern boolean crv_drawn;
extern int ncrvvert, crvixvert[], crviyvert[];

Curve *attr_crv;

void
attrcrv_proc(Menu *m, Menu_item *mi)
{
	char buf[80];
	char *dps;
	unsigned char rfg, gfg, bfg;
	unsigned char rdtk, gdtk, bdtk;
	int lw, ls, ds, dpl, fg, dtk, state;
	float w, dl, dgl, *dp, dpo;
	Path *pth;

	if ((attr_crv= (Curve *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Curve *) NULL) {
		ice_err("Cannot locate selected curve object.", NONFATAL);
		return;
	}
	panelitem_set(crvattr_panel, crvattr_name, LXPTEXT_VALUE, attr_crv->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	attr_crv->getwidth(&lw, &w);
	panelitem_set(crvattr_panel, crvattr_linewd, LXPENUM_VALUE, lw, LXPI_NULL);
	if (lw == CURVE_GLOBALWIDTH)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	(void) gconvert((double) w, 10, 0, buf);
	panelitem_set(crvattr_panel, crvattr_lwidth, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	ls= attr_crv->getlinestyle();
	panelitem_set(crvattr_panel, crvattr_linestyle, LXPENUM_VALUE, ls, LXPI_NULL);

	attr_crv->getdashstyle(&ds, &dp, &dpl, &dpo);
	panelitem_set(crvattr_panel, crvattr_dashstyle, LXPI_STATE, (ls == CURVE_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPENUM_VALUE, ds, LXPI_NULL);
	dps= (char *) NULL;
	switch (ds) {
	case CURVE_SIMPLEDASH:
		if (dp != (float *) NULL) {
			if (flt2str(dp, dpl, &dps) != GROBJ_SUCCESS) {
				ice_err("Cannot retrieve dash pattern.", NONFATAL);
				return;
			}
			if (dpl < 2)
				dl= dgl= 2.;
			else {
				dl= *dp;
				dgl= *(dp+1);
			}
		}
		else {
			dl= dgl= 2.;
			dps= (char *) NULL;
		}
		(void) gconvert((double) dl, 10, 0, buf);
		panelitem_set(crvattr_panel, crvattr_dashlen, LXPI_STATE, (ls == CURVE_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dgl, 10, 0, buf);
		panelitem_set(crvattr_panel, crvattr_dashgaplen, LXPI_STATE, (ls == CURVE_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		panelitem_set(crvattr_panel, crvattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, dps, LXPI_NULL);
		(void) gconvert((double) dpo, 10, 0, buf);
		panelitem_set(crvattr_panel, crvattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		break;
	case CURVE_COMPLEXDASH:
		if (dp != (float *) NULL) {
			if (flt2str(dp, dpl, &dps) != GROBJ_SUCCESS) {
				ice_err("Cannot retrieve dash pattern.", NONFATAL);
				return;
			}
			if (dpl < 2)
				dl= dgl= 2.;
			else {
				dl= *dp;
				dgl= *(dp+1);
			}
		}
		else {
			dl= dgl= 2.;
			dps= (char *) NULL;
		}
		panelitem_set(crvattr_panel, crvattr_dashpattern, LXPI_STATE, (ls == CURVE_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, dps, LXPI_NULL);
		(void) gconvert((double) dpo, 10, 0, buf);
		panelitem_set(crvattr_panel, crvattr_dashoffset, LXPI_STATE, (ls == CURVE_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dl, 10, 0, buf);
		panelitem_set(crvattr_panel, crvattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dgl, 10, 0, buf);
		panelitem_set(crvattr_panel, crvattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		break;
	}
	if (dps != (char *) NULL)
		delete dps;

	panelitem_set(crvattr_panel, crvattr_capstyle, LXPENUM_VALUE, attr_crv->getcapstyle(), LXPI_NULL);

	attr_crv->getforeground(&fg, &rfg, &gfg, &bfg);
	panelitem_set(crvattr_panel, crvattr_fg, LXPENUM_VALUE, fg, LXPI_NULL);
	if (fg == CURVE_OTHERFG)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rfg);
	panelitem_set(crvattr_panel, crvattr_rfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gfg);
	panelitem_set(crvattr_panel, crvattr_gfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bfg);
	panelitem_set(crvattr_panel, crvattr_bfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_crv->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(crvattr_panel, crvattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(crvattr_panel, crvattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_crv->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(crvattr_panel, crvattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
	if (dtk == GROBJ_OTHERDTK)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rdtk);
	panelitem_set(crvattr_panel, crvattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(crvattr_panel, crvattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(crvattr_panel, crvattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_crv->getsequence());
	panelitem_set(crvattr_panel, crvattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(crvattr_panel, crvattr_mvloc1, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_mvloc2, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_mvcnt1, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	panelitem_set(crvattr_panel, crvattr_mvcnt2, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);

	ice_op= CRV_ATTR;
	XMapRaised(dpy, crvattr_frame);
	return;
}

void
crvattrlinewd_proc(Panel *p, Panel_item *pi)
{
	int lw;

	lw= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (lw) {
	case CURVE_GLOBALWIDTH:
		panelitem_set(p, crvattr_lwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case CURVE_OTHERWIDTH:
		panelitem_set(p, crvattr_lwidth, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
crvattrline_proc(Panel *p, Panel_item *pi)
{
	int ls, ds;

	ls= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ls) {
	case CURVE_SOLID:
		panelitem_set(p, crvattr_dashstyle, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case CURVE_DASHED:
		panelitem_set(p, crvattr_dashstyle, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		ds= *((int *) panelitem_get(p, crvattr_dashstyle, LXPENUM_VALUE));
		switch (ds) {
		case CURVE_SIMPLEDASH:
			panelitem_set(p, crvattr_dashlen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			panelitem_set(p, crvattr_dashgaplen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			break;
		case CURVE_COMPLEXDASH:
			panelitem_set(p, crvattr_dashpattern, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			panelitem_set(p, crvattr_dashoffset, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			break;
		}
		break;
	}
	return;
}

void
crvattrdash_proc(Panel *p, Panel_item *pi)
{
	int ds;

	ds= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ds) {
	case CURVE_SIMPLEDASH:
		panelitem_set(p, crvattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashlen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashgaplen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	case CURVE_COMPLEXDASH:
		panelitem_set(p, crvattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashpattern, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_dashoffset, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
crvattrfg_proc(Panel *p, Panel_item *pi)
{
	int fg;

	fg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fg) {
	case CURVE_GLOBALFG:
	case CURVE_BLACKFG:
	case CURVE_WHITEFG:
		panelitem_set(p, crvattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case CURVE_OTHERFG:
		panelitem_set(p, crvattr_rfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_gfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_bfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
crvattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, crvattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, crvattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, crvattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
crvattrmvloc1_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case CRV_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT1:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT2:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= CRV_MVLOC1;

	if (pg_loc == PG_CURSORLOC) {
		ncrvvert= 4;
		attr_crv->getloc(&fx, &fy, &crvixvert[0], &crviyvert[0]);
		crviyvert[0]= pg_pixheight-1-crviyvert[0];
		attr_crv->getcontrol1(&fx, &fy, &crvixvert[1], &crviyvert[1]);
		crviyvert[1]= pg_pixheight-1-crviyvert[1];
		attr_crv->getcontrol2(&fx, &fy, &crvixvert[2], &crviyvert[2]);
		crviyvert[2]= pg_pixheight-1-crviyvert[2];
		attr_crv->getend(&fx, &fy, &crvixvert[3], &crviyvert[3]);
		crviyvert[3]= pg_pixheight-1-crviyvert[3];
		crv_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) attr_crv;
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
crvattrmvcnt1_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case CRV_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT1:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT2:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= CRV_MVCNT1;

	if (pg_loc == PG_CURSORLOC) {
		ncrvvert= 4;
		attr_crv->getloc(&fx, &fy, &crvixvert[0], &crviyvert[0]);
		crviyvert[0]= pg_pixheight-1-crviyvert[0];
		attr_crv->getcontrol1(&fx, &fy, &crvixvert[1], &crviyvert[1]);
		crviyvert[1]= pg_pixheight-1-crviyvert[1];
		attr_crv->getcontrol2(&fx, &fy, &crvixvert[2], &crviyvert[2]);
		crviyvert[2]= pg_pixheight-1-crviyvert[2];
		attr_crv->getend(&fx, &fy, &crvixvert[3], &crviyvert[3]);
		crviyvert[3]= pg_pixheight-1-crviyvert[3];
		crv_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) attr_crv;
	attr_crv->getcontrol1(&fx, &fy, &ix, &iy);
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
crvattrmvcnt2_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case CRV_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT1:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT2:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= CRV_MVCNT2;

	if (pg_loc == PG_CURSORLOC) {
		ncrvvert= 4;
		attr_crv->getloc(&fx, &fy, &crvixvert[0], &crviyvert[0]);
		crviyvert[0]= pg_pixheight-1-crviyvert[0];
		attr_crv->getcontrol1(&fx, &fy, &crvixvert[1], &crviyvert[1]);
		crviyvert[1]= pg_pixheight-1-crviyvert[1];
		attr_crv->getcontrol2(&fx, &fy, &crvixvert[2], &crviyvert[2]);
		crviyvert[2]= pg_pixheight-1-crviyvert[2];
		attr_crv->getend(&fx, &fy, &crvixvert[3], &crviyvert[3]);
		crviyvert[3]= pg_pixheight-1-crviyvert[3];
		crv_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) attr_crv;
	attr_crv->getcontrol2(&fx, &fy, &ix, &iy);
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
crvattrmvloc2_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case CRV_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT1:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVCNT2:
		ice_err("Control location edit already in progress.", NONFATAL);
		return;
	case CRV_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= CRV_MVLOC2;

	if (pg_loc == PG_CURSORLOC) {
		ncrvvert= 4;
		attr_crv->getloc(&fx, &fy, &crvixvert[0], &crviyvert[0]);
		crviyvert[0]= pg_pixheight-1-crviyvert[0];
		attr_crv->getcontrol1(&fx, &fy, &crvixvert[1], &crviyvert[1]);
		crviyvert[1]= pg_pixheight-1-crviyvert[1];
		attr_crv->getcontrol2(&fx, &fy, &crvixvert[2], &crviyvert[2]);
		crviyvert[2]= pg_pixheight-1-crviyvert[2];
		attr_crv->getend(&fx, &fy, &crvixvert[3], &crviyvert[3]);
		crviyvert[3]= pg_pixheight-1-crviyvert[3];
		crv_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}


	loc_grobj= (Grobj *) attr_crv;
	attr_crv->getend(&fx, &fy, &ix, &iy);
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
crvattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	float width, dl, dgl, *dp, dpo;
	int lw, ls, ds, cs, dpl, fg, dtk, seq;
	unsigned char rfg, gfg, bfg;
	char *pathnm;
	Path *pth, *opth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char errmsg[MAX_ERRMSGLEN+1];

	switch (ice_op) {
	case CRV_INSERTATTR:
		crvinscont_proc(p, pi);
		return;
	case CRV_MVLOC1:
		ice_err("Origin location edit still in progress.", NONFATAL);
		return;
	case CRV_MVCNT1:
		ice_err("Control location edit still in progress.", NONFATAL);
		return;
	case CRV_MVCNT2:
		ice_err("Control location edit still in progress.", NONFATAL);
		return;
	case CRV_MVLOC2:
		ice_err("Terminus location edit still in progress.", NONFATAL);
		return;
	}

	XUnmapWindow(dpy, crvattr_frame);
	ice_op= MAIN_MENU;

	dp= (float *) NULL;

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

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_crv->setwidth(lw, width);
	(void) attr_crv->setlinestyle(ls);
	if (ls == CURVE_DASHED)
		(void) attr_crv->setdashstyle(ds, dp, dpl, dpo);
	(void) attr_crv->setcapstyle(cs);
	(void) attr_crv->setforeground(fg, rfg, gfg, bfg);
	opth= attr_crv->getclip();
	if (opth != pth) {
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_crv->setclip(pth);
	}

	(void) attr_crv->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	attr_crv->setdtkpix(dtkpix);

	if (seq != attr_crv->getsequence()) {
		attr_crv->setsequence(seq);
		attr_crv->sortsequence(&grobjs);
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
crvattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, crvattr_frame);
	ice_op= MAIN_MENU;
	return;
}
