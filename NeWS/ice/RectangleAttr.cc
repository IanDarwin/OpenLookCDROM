/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Rectangle.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern int		flt2str(float *, int, char **);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		rectinscont_proc(Panel *, Panel_item *);
extern int		str2flt(char *, float **, int *);

static Rectangle *attr_rect;

void
attrrect_proc(Menu *m, Menu_item *mi)
{
	char buf[20];
	char *dps;
	float width, height, bndwidth;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	unsigned char rdtk, gdtk, bdtk;
	int bndmode, bndwd, bndclr, fillmode, fillclr, dtk, state;
	int ls, ds, dpl;
	float dl, dgl, *dp, dpo;
	Path *pth;

	if ((attr_rect= (Rectangle *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Rectangle *) NULL) {
		ice_err("Cannot locate selected rectangle object.", NONFATAL);
		return;
	}

	panelitem_set(rectattr_panel, rectattr_name, LXPTEXT_VALUE, attr_rect->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	attr_rect->getboundary(&bndmode, &bndclr, &rbnd, &gbnd, &bbnd);
	panelitem_set(rectattr_panel, rectattr_bndmode, LXPENUM_VALUE, bndmode, LXPI_NULL);

	attr_rect->getwidth(&bndwd, &bndwidth);
	if (bndmode == RECTANGLE_OPAQUEBNDM)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(rectattr_panel, rectattr_bndwd, LXPI_STATE, state, LXPENUM_VALUE, bndwd, LXPI_NULL);
	if ((bndmode == RECTANGLE_OPAQUEBNDM) && (bndwd == RECTANGLE_OTHERBNDWIDTH))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) gconvert((double) bndwidth, 10, 0, buf);
	panelitem_set(rectattr_panel, rectattr_bwidth, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	ls= attr_rect->getlinestyle();
	if (bndmode == RECTANGLE_OPAQUEBNDM)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(rectattr_panel, rectattr_linestyle, LXPENUM_VALUE, ls, LXPI_NULL);

	attr_rect->getdashstyle(&ds, &dp, &dpl, &dpo);
	if ((bndmode == RECTANGLE_OPAQUEBNDM) && (ls == RECTANGLE_DASHED))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(rectattr_panel, rectattr_dashstyle, LXPI_STATE, state, LXPENUM_VALUE, ds, LXPI_NULL);
	dps= (char *) NULL;
	switch (ds) {
	case RECTANGLE_SIMPLEDASH:
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
		panelitem_set(rectattr_panel, rectattr_dashlen, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dgl, 10, 0, buf);
		panelitem_set(rectattr_panel, rectattr_dashgaplen, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		panelitem_set(rectattr_panel, rectattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, dps, LXPI_NULL);
		(void) gconvert((double) dpo, 10, 0, buf);
		panelitem_set(rectattr_panel, rectattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		break;
	case RECTANGLE_COMPLEXDASH:
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
		panelitem_set(rectattr_panel, rectattr_dashpattern, LXPI_STATE, state, LXPTEXT_VALUE, dps, LXPI_NULL);
		(void) gconvert((double) dpo, 10, 0, buf);
		panelitem_set(rectattr_panel, rectattr_dashoffset, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dl, 10, 0, buf);
		panelitem_set(rectattr_panel, rectattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) gconvert((double) dgl, 10, 0, buf);
		panelitem_set(rectattr_panel, rectattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
		break;
	}
	if (dps != (char *) NULL)
		delete dps;

	if (bndmode == RECTANGLE_OPAQUEBNDM)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(rectattr_panel, rectattr_bndcolor, LXPI_STATE, state, LXPENUM_VALUE, bndclr, LXPI_NULL);
	if ((bndmode == RECTANGLE_OPAQUEBNDM) && (bndclr == RECTANGLE_OTHERBND))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rbnd);
	panelitem_set(rectattr_panel, rectattr_rbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gbnd);
	panelitem_set(rectattr_panel, rectattr_gbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bbnd);
	panelitem_set(rectattr_panel, rectattr_bbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_rect->getfill(&fillmode, &fillclr, &rfill, &gfill, &bfill);
	panelitem_set(rectattr_panel, rectattr_fillmode, LXPENUM_VALUE, fillmode, LXPI_NULL);
	if (fillmode == RECTANGLE_OPAQUEFILLM)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(rectattr_panel, rectattr_fillcolor, LXPI_STATE, state, LXPENUM_VALUE, fillclr, LXPI_NULL);
	if ((fillmode == RECTANGLE_OPAQUEFILLM) && (fillclr == RECTANGLE_OTHERFILL))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rfill);
	panelitem_set(rectattr_panel, rectattr_rfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gfill);
	panelitem_set(rectattr_panel, rectattr_gfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bfill);
	panelitem_set(rectattr_panel, rectattr_bfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_dimmode, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	attr_rect->getsize(&width, &height);
	(void) sprintf(buf, "%6.4f", (double) width);
	panelitem_set(rectattr_panel, rectattr_width, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%6.4f", (double) height);
	panelitem_set(rectattr_panel, rectattr_height, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) attr_rect->getrotation(), 7, 0, buf);
	panelitem_set(rectattr_panel, rectattr_rot, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_rect->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(rectattr_panel, rectattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(rectattr_panel, rectattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_rect->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(rectattr_panel, rectattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		state= LXPI_INACTIVE;
		break;
	case GROBJ_OTHERDTK:
		state= LXPI_ACTIVE;
		break;
	}
	(void) sprintf(buf, "%1d", (int) rdtk);
	panelitem_set(rectattr_panel, rectattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(rectattr_panel, rectattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(rectattr_panel, rectattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_rect->getsequence());
	panelitem_set(rectattr_panel, rectattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	ice_op= RECT_ATTR;
	XMapRaised(dpy, rectattr_frame);
	return;
}

void
rectattrbndmode_proc(Panel *p, Panel_item *pi)
{
	int bndmode;
	void rectattrbndwd_proc(Panel *p, Panel_item *pi);
	void rectattrline_proc(Panel *p, Panel_item *pi);
	void rectattrbndclr_proc(Panel *p, Panel_item *pi);

	bndmode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndmode) {
	case RECTANGLE_OPAQUEBNDM:
		panelitem_set(p, rectattr_bndwd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		rectattrbndwd_proc(p, rectattr_bndwd);
		panelitem_set(p, rectattr_linestyle, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		rectattrline_proc(p, rectattr_linestyle);
		panelitem_set(p, rectattr_bndcolor, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		rectattrbndclr_proc(p, rectattr_bndcolor);
		break;
	case RECTANGLE_TRANSPARENTBNDM:
		panelitem_set(p, rectattr_bndwd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_linestyle, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashstyle, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bndcolor, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rectattrbndwd_proc(Panel *p, Panel_item *pi)
{
	int bndwd;

	bndwd= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndwd) {
	case RECTANGLE_GLOBALBNDWIDTH:
		panelitem_set(p, rectattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RECTANGLE_OTHERBNDWIDTH:
		panelitem_set(p, rectattr_bwidth, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rectattrline_proc(Panel *p, Panel_item *pi)
{
	int ls;
	void rectattrdash_proc(Panel *p, Panel_item *pi);

	ls= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ls) {
	case RECTANGLE_SOLID:
		panelitem_set(p, rectattr_dashstyle, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RECTANGLE_DASHED:
		panelitem_set(p, rectattr_dashstyle, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		rectattrdash_proc(p, rectattr_dashstyle);
		break;
	}
	return;
}

void
rectattrdash_proc(Panel *p, Panel_item *pi)
{
	int ds;

	ds= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (ds) {
	case RECTANGLE_SIMPLEDASH:
		panelitem_set(p, rectattr_dashpattern, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashlen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashgaplen, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	case RECTANGLE_COMPLEXDASH:
		panelitem_set(p, rectattr_dashlen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashgaplen, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashpattern, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_dashoffset, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rectattrbndclr_proc(Panel *p, Panel_item *pi)
{
	int bndclr;

	bndclr= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndclr) {
	case RECTANGLE_GLOBALBND:
	case RECTANGLE_BLACKBND:
	case RECTANGLE_WHITEBND:
		panelitem_set(p, rectattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RECTANGLE_OTHERBND:
		panelitem_set(p, rectattr_rbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rectattrfillmode_proc(Panel *p, Panel_item *pi)
{
	int mode;
	void rectattrfillclr_proc(Panel *, Panel_item *);

	mode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (mode) {
	case RECTANGLE_TRANSPARENTFILLM:
		panelitem_set(p, rectattr_fillcolor, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RECTANGLE_OPAQUEFILLM:
		panelitem_set(p, rectattr_fillcolor, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		rectattrfillclr_proc(p, rectattr_fillcolor);
		break;
	}
	return;
}

void
rectattrfillclr_proc(Panel *p, Panel_item *pi)
{
	int fillclr;

	fillclr= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fillclr) {
	case RECTANGLE_GLOBALFILL:
	case RECTANGLE_WHITEFILL:
	case RECTANGLE_BLACKFILL:
		panelitem_set(p, rectattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RECTANGLE_OTHERFILL:
		panelitem_set(p, rectattr_rfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rectattrdimmode_proc(Panel *p, Panel_item *pi)
{
	int dm;

	dm= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dm) {
	case RECT_CURSORDIM:
		panelitem_set(p, rectattr_width, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_height, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_rot, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RECT_TEXTDIM:
		panelitem_set(p, rectattr_width, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_height, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_rot, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rectattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, rectattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, rectattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rectattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rectattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	float bndwidth, dl, dgl, *dp, dpo, width, height, rot;
	int bndmode, bndwd, bnd, ls, ds, dpl, fillmode, fill, dtk, seq;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	char *pathnm;
	Path *pth, *opth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char errmsg[MAX_ERRMSGLEN+1];

	switch (ice_op) {
	case RECT_INSERTATTR:
		rectinscont_proc(p, pi);
		return;
	}

	XUnmapWindow(dpy, rectattr_frame);
	ice_op= MAIN_MENU;

	dp= (float *) NULL;

	bndmode= *((int *) panelitem_get(rectattr_panel, rectattr_bndmode, LXPENUM_VALUE));
	switch (bndmode) {
	case RECTANGLE_OPAQUEBNDM:
		bndwd= *((int *) panelitem_get(rectattr_panel, rectattr_bndwd, LXPENUM_VALUE));
		switch (bndwd) {
		case RECTANGLE_GLOBALBNDWIDTH:
			bndwidth= gdf_bndwidth;
			break;
		case RECTANGLE_OTHERBNDWIDTH:
			buf= (char *) panelitem_get(rectattr_panel, rectattr_bwidth, LXPTEXT_VALUE);
			bndwidth= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid boundary width value.", NONFATAL);
				return;
			}
			if (bndwidth < 0.) {
				ice_err("Boundary width value may not be less than 0.", NONFATAL);
				return;
			}
			break;
		}

		ls= *((int *) panelitem_get(rectattr_panel, rectattr_linestyle, LXPENUM_VALUE));
		switch (ls) {
		case RECTANGLE_SOLID:
			break;
		case RECTANGLE_DASHED:
			ds= *((int *) panelitem_get(rectattr_panel, rectattr_dashstyle, LXPENUM_VALUE));
			switch (ds) {
			case RECTANGLE_SIMPLEDASH:
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashlen, LXPTEXT_VALUE);
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
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashgaplen, LXPTEXT_VALUE);
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
			case RECTANGLE_COMPLEXDASH:
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashpattern, LXPTEXT_VALUE);
				if (str2flt(buf, &dp, &dpl) != GROBJ_SUCCESS) {
					ice_err("Cannot convert dash pattern.", NONFATAL);
					delete dp;
					return;
				}
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashoffset, LXPTEXT_VALUE);
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

		bnd= *((int *) panelitem_get(rectattr_panel, rectattr_bndcolor, LXPENUM_VALUE));
		switch (bnd) {
		case RECTANGLE_GLOBALBND:
			rbnd= gdf_rbnd;
			gbnd= gdf_gbnd;
			bbnd= gdf_bbnd;
			break;
		case RECTANGLE_BLACKBND:
			rbnd= gbnd= bbnd= 0;
			break;
		case RECTANGLE_WHITEBND:
			rbnd= gbnd= bbnd= 255;
			break;
		case RECTANGLE_OTHERBND:
			buf= (char *) panelitem_get(rectattr_panel, rectattr_rbnd, LXPTEXT_VALUE);
			rbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			if ((rbnd < 0) || (rbnd > 255)) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_gbnd, LXPTEXT_VALUE);
			gbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			if ((gbnd < 0) || (gbnd > 255)) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_bbnd, LXPTEXT_VALUE);
			bbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid blue boundary color value.", NONFATAL);
				return;
			}
			if ((bbnd < 0) || (bbnd > 255)) {
				ice_err("Invalid blue boundary color value.", NONFATAL);
				return;
			}
			break;
		}
		break;
	case RECTANGLE_TRANSPARENTBNDM:
		bndwd= RECTANGLE_GLOBALBNDWIDTH;
		bndwidth= gdf_bndwidth;
		ls= RECTANGLE_SOLID;
		bnd= RECTANGLE_GLOBALBND;
		rbnd= gdf_rbnd;
		gbnd= gdf_gbnd;
		bbnd= gdf_bbnd;
		break;
	}

	fillmode= *((int *) panelitem_get(rectattr_panel, rectattr_fillmode, LXPENUM_VALUE));
	switch (fillmode) {
	case RECTANGLE_TRANSPARENTFILLM:
		fill= RECTANGLE_GLOBALFILL;
		rfill= gdf_rfill;
		gfill= gdf_gfill;
		bfill= gdf_bfill;
		break;
	case RECTANGLE_OPAQUEFILLM:
		fill= *((int *) panelitem_get(rectattr_panel, rectattr_fillcolor, LXPENUM_VALUE));
		switch (fill) {
		case RECTANGLE_GLOBALFILL:
			rfill= gdf_rfill;
			gfill= gdf_gfill;
			bfill= gdf_bfill;
			break;
		case RECTANGLE_WHITEFILL:
			rfill= gfill= bfill= 255;
			break;
		case RECTANGLE_BLACKFILL:
			rfill= gfill= bfill= 0;
			break;
		case RECTANGLE_OTHERFILL:
			buf= (char *) panelitem_get(rectattr_panel, rectattr_rfill, LXPTEXT_VALUE);
			rfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red fill color value.", NONFATAL);
				return;
			}
			if ((rfill < 0) || (rfill > 255)) {
				ice_err("Invalid red fill color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_gfill, LXPTEXT_VALUE);
			gfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green fill color value.", NONFATAL);
				return;
			}
			if ((gfill < 0) || (gfill > 255)) {
				ice_err("Invalid green fill color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_bfill, LXPTEXT_VALUE);
			bfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid blue fill color value.", NONFATAL);
				return;
			}
			if ((bfill < 0) || (bfill > 255)) {
				ice_err("Invalid blue fill color value.", NONFATAL);
				return;
			}
			break;
		}
		break;
	}

	buf= (char *) panelitem_get(rectattr_panel, rectattr_width, LXPTEXT_VALUE);
	width= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid width value.", NONFATAL);
		return;
	}
	if (width < 0.) {
		ice_err("Rectangle width may not be less than 0.", NONFATAL);
		return;
	}
	buf= (char *) panelitem_get(rectattr_panel, rectattr_height, LXPTEXT_VALUE);
	height= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid height value.", NONFATAL);
		return;
	}
	if (height < 0.) {
		ice_err("Rectangle height may not be less than 0.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(rectattr_panel, rectattr_rot, LXPTEXT_VALUE);
	rot= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid rotation value.", NONFATAL);
		return;
	}

	pathnm= (char *) panelitem_get(rectattr_panel, rectattr_clip, LXPTEXT_VALUE);
	if (strlen(pathnm) == 0)
		pth= (Path *) NULL;
	else {
		for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
			if (!strcmp(pathnm, pth->getname()))
				break;
		}
		if (pth == (Path *) NULL) {
			ice_err("Specified path does not exist.", NONFATAL);
			return;
		}
	}

	dtk= *((int *) panelitem_get(rectattr_panel, rectattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(rectattr_panel, rectattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(rectattr_panel, rectattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(rectattr_panel, rectattr_bdtk, LXPTEXT_VALUE);
		bdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue dump transparency key value.", NONFATAL);
			return;
		}
		if ((bdtk < 0) || (bdtk > 255)) {
			ice_err("Invalid blue dump transparency key value.", NONFATAL);
			return;
		}
		break;
	}

	buf= (char *) panelitem_get(rectattr_panel, rectattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_rect->setboundary(bndmode, bnd, rbnd, gbnd, bbnd);
	(void) attr_rect->setwidth(bndwd, bndwidth);
	(void) attr_rect->setlinestyle(ls);
	if (ls == RECTANGLE_DASHED)
		(void) attr_rect->setdashstyle(ds, dp, dpl, dpo);
	(void) attr_rect->setfill(fillmode, fill, rfill, gfill, bfill);
	(void) attr_rect->setsize(width, height);
	attr_rect->setrotation(rot);
	opth= attr_rect->getclip();
	if (opth != pth) {
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_rect->setclip(pth);
	}

	(void) attr_rect->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	attr_rect->setdtkpix(dtkpix);

	if (seq != attr_rect->getsequence()) {
		attr_rect->setsequence(seq);
		attr_rect->sortsequence(&grobjs);
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
rectattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, rectattr_frame);
	ice_op= MAIN_MENU;
	return;
}
