/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Vector.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern int		flt2str(float *, int, char **);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern int		str2flt(char *, float **, int *);
extern void		vecinscont_proc(Panel *, Panel_item *);

extern Grobj *loc_grobj;
extern boolean vec_drawn;
extern int vec_x, vec_y;
extern int vec_ex, vec_ey;

Vector *attr_vec;

void
attrvec_proc(Menu *m, Menu_item *mi)
{
	char buf[80];
	char *dps;
	unsigned char rfg, gfg, bfg;
	unsigned char rdtk, gdtk, bdtk;
	int lw, ls, ds, dpl, ptr, ptrs, fg, dtk, state;
	float w, dl, dgl, *dp, dpo, pwd, pol, pil;
	Path *pth;

	if ((attr_vec= (Vector *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Vector *) NULL) {
		ice_err("Cannot locate selected vector object.", NONFATAL);
		return;
	}
	panelitem_set(vecattr_panel, vecattr_name, LXPTEXT_VALUE, attr_vec->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	attr_vec->getwidth(&lw, &w);
	panelitem_set(vecattr_panel, vecattr_linewd, LXPENUM_VALUE, lw, LXPI_NULL);
	if (lw == VECTOR_GLOBALWIDTH)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	(void) gconvert((double) w, 10, 0, buf);
	panelitem_set(vecattr_panel, vecattr_lwidth, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	ls= attr_vec->getlinestyle();
	panelitem_set(vecattr_panel, vecattr_linestyle, LXPENUM_VALUE, ls, LXPI_NULL);

	attr_vec->getdashstyle(&ds, &dp, &dpl, &dpo);
	panelitem_set(vecattr_panel, vecattr_dashstyle, LXPI_STATE, (ls == VECTOR_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPENUM_VALUE, ds, LXPI_NULL);
	dps= (char *) NULL;
	switch (ds) {
	case VECTOR_SIMPLEDASH:
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
		panelitem_set(vecattr_panel, vecattr_dashlen, LXPI_STATE, (ls == VECTOR_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dgl, 10, 0, buf);
		panelitem_set(vecattr_panel, vecattr_dashgaplen, LXPI_STATE, (ls == VECTOR_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		panelitem_set(vecattr_panel, vecattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, dps, LXPI_NULL);
		(void) gconvert((double) dpo, 10, 0, buf);
		panelitem_set(vecattr_panel, vecattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		break;
	case VECTOR_COMPLEXDASH:
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
		panelitem_set(vecattr_panel, vecattr_dashpattern, LXPI_STATE, (ls == VECTOR_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, dps, LXPI_NULL);
		(void) gconvert((double) dpo, 10, 0, buf);
		panelitem_set(vecattr_panel, vecattr_dashoffset, LXPI_STATE, (ls == VECTOR_SOLID) ? LXPI_INACTIVE : LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dl, 10, 0, buf);
		panelitem_set(vecattr_panel, vecattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dgl, 10, 0, buf);
		panelitem_set(vecattr_panel, vecattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		break;
	}
	if (dps != (char *) NULL)
		delete dps;

	panelitem_set(vecattr_panel, vecattr_capstyle, LXPENUM_VALUE, attr_vec->getcapstyle(), LXPI_NULL);

	attr_vec->getptrstyle(&ptr, &ptrs, &pwd, &pol, &pil);
	panelitem_set(vecattr_panel, vecattr_ptr, LXPENUM_VALUE, ptr, LXPI_NULL);
	if (ptr == VECTOR_NOPTR)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	panelitem_set(vecattr_panel, vecattr_ptrstyle, LXPENUM_VALUE, ptrs, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert((double) pwd, 10, 0, buf);
	panelitem_set(vecattr_panel, vecattr_ptrwd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) pol, 10, 0, buf);
	panelitem_set(vecattr_panel, vecattr_ptrolen, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	if ((ptr == VECTOR_NOPTR) || (ptrs == VECTOR_OPENPTR))
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	(void) gconvert((double) pil, 10, 0, buf);
	panelitem_set(vecattr_panel, vecattr_ptrilen, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_vec->getforeground(&fg, &rfg, &gfg, &bfg);
	panelitem_set(vecattr_panel, vecattr_fg, LXPENUM_VALUE, fg, LXPI_NULL);
	if (fg == VECTOR_OTHERFG)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rfg);
	panelitem_set(vecattr_panel, vecattr_rfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gfg);
	panelitem_set(vecattr_panel, vecattr_gfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bfg);
	panelitem_set(vecattr_panel, vecattr_bfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_vec->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(vecattr_panel, vecattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(vecattr_panel, vecattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_vec->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(vecattr_panel, vecattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
	if (dtk == GROBJ_OTHERDTK)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rdtk);
	panelitem_set(vecattr_panel, vecattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(vecattr_panel, vecattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(vecattr_panel, vecattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_vec->getsequence());
	panelitem_set(vecattr_panel, vecattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_mvloc1, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_mvloc2, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);

	ice_op= VEC_ATTR;
	XMapRaised(dpy, vecattr_frame);
	return;
}

void
vecattrlinewd_proc(Panel *p, Panel_item *pi)
{
	int lw;

	lw= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (lw) {
	case VECTOR_GLOBALWIDTH:
		panelitem_set(p, vecattr_lwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case VECTOR_OTHERWIDTH:
		panelitem_set(p, vecattr_lwidth, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
vecattrline_proc(Panel *p, Panel_item *pi)
{
	int ls, ds;

	ls= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ls) {
	case VECTOR_SOLID:
		panelitem_set(p, vecattr_dashstyle, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case VECTOR_DASHED:
		panelitem_set(p, vecattr_dashstyle, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		ds= *((int *) panelitem_get(p, vecattr_dashstyle, LXPENUM_VALUE));
		switch (ds) {
		case VECTOR_SIMPLEDASH:
			panelitem_set(p, vecattr_dashlen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			panelitem_set(p, vecattr_dashgaplen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			break;
		case VECTOR_COMPLEXDASH:
			panelitem_set(p, vecattr_dashpattern, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			panelitem_set(p, vecattr_dashoffset, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
			break;
		}
		break;
	}
	return;
}

void
vecattrdash_proc(Panel *p, Panel_item *pi)
{
	int ds;

	ds= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ds) {
	case VECTOR_SIMPLEDASH:
		panelitem_set(p, vecattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashlen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashgaplen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	case VECTOR_COMPLEXDASH:
		panelitem_set(p, vecattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashpattern, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_dashoffset, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
vecattrptr_proc(Panel *p, Panel_item *pi)
{
	int ptr;
	void vecattrptrstyle_proc(Panel *, Panel_item *);

	ptr= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ptr) {
	case VECTOR_NOPTR:
		panelitem_set(p, vecattr_ptrstyle, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_ptrwd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_ptrolen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_ptrilen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case VECTOR_ORIGPTR:
	case VECTOR_TERMPTR:
	case VECTOR_BOTHPTR:
		panelitem_set(p, vecattr_ptrstyle, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_ptrwd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_ptrolen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		vecattrptrstyle_proc(p, vecattr_ptrstyle);
		break;
	}
	return;
}

void
vecattrptrstyle_proc(Panel *p, Panel_item *pi)
{
	int ptrs;

	ptrs= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ptrs) {
	case VECTOR_OPENPTR:
		panelitem_set(p, vecattr_ptrilen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case VECTOR_CLOSEDPTR:
		panelitem_set(p, vecattr_ptrilen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
vecattrfg_proc(Panel *p, Panel_item *pi)
{
	int fg;

	fg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fg) {
	case VECTOR_GLOBALFG:
	case VECTOR_BLACKFG:
	case VECTOR_WHITEFG:
		panelitem_set(p, vecattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case VECTOR_OTHERFG:
		panelitem_set(p, vecattr_rfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_gfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_bfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
vecattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, vecattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, vecattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, vecattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
vecattrmvloc1_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case VEC_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case VEC_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= VEC_MVLOC1;

	if (pg_loc == PG_CURSORLOC) {
		attr_vec->getend(&fx, &fy, &vec_ex, &vec_ey);
		vec_ey= pg_pixheight-1-vec_ey;
		vec_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) attr_vec;
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
vecattrmvloc2_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case VEC_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case VEC_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= VEC_MVLOC2;

	if (pg_loc == PG_CURSORLOC) {
		attr_vec->getloc(&fx, &fy, &vec_x, &vec_y);
		vec_y= pg_pixheight-1-vec_y;
		vec_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}


	loc_grobj= (Grobj *) attr_vec;
	attr_vec->getend(&fx, &fy, &ix, &iy);
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
vecattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	float width, dl, dgl, *dp, dpo, pwd, pol, pil;
	int lw, ls, ds, cs, ptr, ptrs, dpl, fg, dtk, seq;
	unsigned char rfg, gfg, bfg;
	char *pathnm;
	Path *pth, *opth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char errmsg[MAX_ERRMSGLEN+1];

	switch (ice_op) {
	case VEC_INSERTATTR:
		vecinscont_proc(p, pi);
		return;
	case VEC_MVLOC1:
		ice_err("Origin location edit still in progress.", NONFATAL);
		return;
	case VEC_MVLOC2:
		ice_err("Terminus location edit still in progress.", NONFATAL);
		return;
	}

	XUnmapWindow(dpy, vecattr_frame);
	ice_op= MAIN_MENU;

	dp= (float *) NULL;

	lw= *((int *) panelitem_get(vecattr_panel, vecattr_linewd, LXPENUM_VALUE));
	switch (lw) {
	case VECTOR_GLOBALWIDTH:
		width= gdf_linewidth;
		break;
	case VECTOR_OTHERWIDTH:
		buf= (char *) panelitem_get(vecattr_panel, vecattr_lwidth, LXPTEXT_VALUE);
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

	ls= *((int *) panelitem_get(vecattr_panel, vecattr_linestyle, LXPENUM_VALUE));
	switch (ls) {
	case VECTOR_SOLID:
		break;
	case VECTOR_DASHED:
		ds= *((int *) panelitem_get(vecattr_panel, vecattr_dashstyle, LXPENUM_VALUE));
		switch (ds) {
		case VECTOR_SIMPLEDASH:
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashlen, LXPTEXT_VALUE);
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
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashgaplen, LXPTEXT_VALUE);
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
		case VECTOR_COMPLEXDASH:
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashpattern, LXPTEXT_VALUE);
			if (str2flt(buf, &dp, &dpl) != GROBJ_SUCCESS) {
				ice_err("Cannot convert dash pattern.", NONFATAL);
				delete dp;
				return;
			}
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashoffset, LXPTEXT_VALUE);
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

	cs= *((int *) panelitem_get(vecattr_panel, vecattr_capstyle, LXPENUM_VALUE));

	ptr= *((int *) panelitem_get(vecattr_panel, vecattr_ptr, LXPENUM_VALUE));
	switch (ptr) {
	case VECTOR_NOPTR:
		ptrs= VECTOR_OPENPTR;
		pwd= pol= pil= 10.;
		break;
	case VECTOR_ORIGPTR:
	case VECTOR_TERMPTR:
	case VECTOR_BOTHPTR:
		ptrs= *((int *) panelitem_get(vecattr_panel, vecattr_ptrstyle, LXPENUM_VALUE));
		buf= (char *) panelitem_get(vecattr_panel, vecattr_ptrwd, LXPTEXT_VALUE);
		pwd= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid pointer width value.", NONFATAL);
			delete dp;
			return;
		}
		if (pwd < 0.) {
			ice_err("Invalid pointer width value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(vecattr_panel, vecattr_ptrolen, LXPTEXT_VALUE);
		pol= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid pointer outside length value.", NONFATAL);
			delete dp;
			return;
		}
		if (pol < 0.) {
			ice_err("Invalid pointer outside length value.", NONFATAL);
			delete dp;
			return;
		}
		if (ptrs == VECTOR_CLOSEDPTR) {
			buf= (char *) panelitem_get(vecattr_panel, vecattr_ptrilen, LXPTEXT_VALUE);
			pil= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid pointer inside length value.", NONFATAL);
				delete dp;
				return;
			}
			if (pil < 0.) {
				ice_err("Invalid pointer inside length value.", NONFATAL);
				delete dp;
				return;
			}
		}
		else
			pil= pol;
		break;
	}

	fg= *((int *) panelitem_get(vecattr_panel, vecattr_fg, LXPENUM_VALUE));
	switch (fg) {
	case VECTOR_GLOBALFG:
		rfg= gdf_rfg;
		gfg= gdf_gfg;
		bfg= gdf_bfg;
		break;
	case VECTOR_BLACKFG:
		rfg= gfg= bfg= 0;
		break;
	case VECTOR_WHITEFG:
		rfg= gfg= bfg= 255;
		break;
	case VECTOR_OTHERFG:
		buf= (char *) panelitem_get(vecattr_panel, vecattr_rfg, LXPTEXT_VALUE);
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_gfg, LXPTEXT_VALUE);
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_bfg, LXPTEXT_VALUE);
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

	pathnm= (char *) panelitem_get(vecattr_panel, vecattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(vecattr_panel, vecattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_rdtk, LXPTEXT_VALUE);
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_gdtk, LXPTEXT_VALUE);
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(vecattr_panel, vecattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		delete dp;
		return;
	}

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_vec->setwidth(lw, width);
	(void) attr_vec->setlinestyle(ls);
	if (ls == VECTOR_DASHED)
		(void) attr_vec->setdashstyle(ds, dp, dpl, dpo);
	(void) attr_vec->setcapstyle(cs);
	(void) attr_vec->setptrstyle(ptr, ptrs, pwd, pol, pil);
	(void) attr_vec->setforeground(fg, rfg, gfg, bfg);
	opth= attr_vec->getclip();
	if (opth != pth) {
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_vec->setclip(pth);
	}

	(void) attr_vec->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	attr_vec->setdtkpix(dtkpix);

	if (seq != attr_vec->getsequence()) {
		attr_vec->setsequence(seq);
		attr_vec->sortsequence(&grobjs);
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
vecattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, vecattr_frame);
	ice_op= MAIN_MENU;
	return;
}
