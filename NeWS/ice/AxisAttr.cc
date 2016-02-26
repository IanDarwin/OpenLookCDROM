/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Axis.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern void		axisinscont_proc(Panel *, Panel_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_draw();

extern Grobj *loc_grobj;
extern boolean axis_drawn;
extern int axis_x, axis_y;
extern int axis_ex, axis_ey;

Axis *attr_axis;

void
attraxis_proc(Menu *m, Menu_item *mi)
{
	char buf[80];
	char *axisfontnm;
	int fl, font, ff, fn, fontsz, forient;
	float fontsize, fontoff;
	boolean found;
	unsigned char rlin, glin, blin;
	unsigned char rlab, glab, blab;
	unsigned char rdtk, gdtk, bdtk;
	int ll, sd, aw, tl, tw, lab, lin, dtk, state;
	float pt, st, tt, w;
	double orig, term;
	Path *pth;

	if ((attr_axis= (Axis *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Axis *) NULL) {
		ice_err("Cannot locate selected axis object.", NONFATAL);
		return;
	}
	panelitem_set(axisattr_panel, axisattr_name, LXPTEXT_VALUE, attr_axis->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	attr_axis->getotl(&orig, &term, &ll);
	(void) gconvert(orig, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_orig, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert(term, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_term, LXPTEXT_VALUE, buf, LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_linlog, LXPENUM_VALUE, ll, LXPI_NULL);

	sd= attr_axis->getsubdiv();
	(void) sprintf(buf, "%1d", sd);
	panelitem_set(axisattr_panel, axisattr_div, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_axis->getaxiswidth(&aw, &w);
	panelitem_set(axisattr_panel, axisattr_axiswd, LXPENUM_VALUE, aw, LXPI_NULL);
	if (aw == AXIS_GLOBALWIDTH)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	(void) gconvert((double) w, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_axiswidth, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	attr_axis->gettick(&tl, &pt, &st, &tt, &tw, &w);
	panelitem_set(axisattr_panel, axisattr_tickloc, LXPENUM_VALUE, tl, LXPI_NULL);
	if (tl == AXIS_NOLOC)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	(void) gconvert((double) pt, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_ptickht, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert((double) st, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_stickht, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert((double) tt, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_ttickht, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_tickwd, LXPENUM_VALUE, tw, LXPI_STATE, state, LXPI_NULL);
	if ((tl != AXIS_NOLOC) && (tw == AXIS_OTHERWIDTH))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) gconvert((double) w, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_tickwidth, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	attr_axis->getline(&lin, &rlin, &glin, &blin);
	panelitem_set(axisattr_panel, axisattr_lin, LXPENUM_VALUE, lin, LXPI_NULL);
	if (lin == AXIS_OTHERLINE)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rlin);
	panelitem_set(axisattr_panel, axisattr_rlin, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) glin);
	panelitem_set(axisattr_panel, axisattr_glin, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) blin);
	panelitem_set(axisattr_panel, axisattr_blin, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_axis->getlabelattr(&fl, &forient, &fontoff);
	panelitem_set(axisattr_panel, axisattr_fontloc, LXPENUM_VALUE, fl, LXPI_NULL);

	attr_axis->getfont(&font, &axisfontnm);
	if (fl == AXIS_NOLOC)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	panelitem_set(axisattr_panel, axisattr_font, LXPI_STATE, state, LXPENUM_VALUE, font, LXPI_NULL);
	if ((font == AXIS_OTHERFONT) && (fl != AXIS_NOLOC))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	found= FALSE;
	for (ff= 0; ff < npsfontfamilies; ff++) {
		char *c;
		c= psfontfamilies[ff].psff_name;
		for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
			if (!strcmp(axisfontnm, psfontfamilies[ff].psff_fonts[fn])) {
				found= TRUE;
				break;
			}
		}
		if (found == TRUE)
			break;
	}
	panelitem_set(axisattr_panel, axisattr_fontfamily, LXPI_STATE, state, LXPENUM_VALUE, ff, LXPI_NULL);
	if (axis_psff != (Panel_item *) NULL)
		panelitem_set(axisattr_panel, axis_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	axis_psff= psfontfamilies[ff].psff_axispi;
	panelitem_set(axisattr_panel, axis_psff, LXPI_STATE, state, LXPENUM_VALUE, fn, LXPI_NULL);

	attr_axis->getfontsize(&fontsz, &fontsize);
	if (fl == AXIS_NOLOC)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	panelitem_set(axisattr_panel, axisattr_fontsz, LXPI_STATE, state, LXPENUM_VALUE, fontsz, LXPI_NULL);
	if ((fontsz == AXIS_OTHERFONTSZ) && (fl != AXIS_NOLOC))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) gconvert((double) fontsize, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_fontsize, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	if (fl == AXIS_NOLOC)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	panelitem_set(axisattr_panel, axisattr_fontorient, LXPI_STATE, state, LXPENUM_VALUE, forient, LXPI_NULL);
	(void) gconvert((double) fontoff, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_fontoff, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_axis->getlabel(&lab, &rlab, &glab, &blab);
	panelitem_set(axisattr_panel, axisattr_lab, LXPENUM_VALUE, lab, LXPI_NULL);
	if (lab == AXIS_OTHERLABEL)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rlab);
	panelitem_set(axisattr_panel, axisattr_rlab, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) glab);
	panelitem_set(axisattr_panel, axisattr_glab, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) blab);
	panelitem_set(axisattr_panel, axisattr_blab, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_axis->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(axisattr_panel, axisattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(axisattr_panel, axisattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_axis->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(axisattr_panel, axisattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
	if (dtk == GROBJ_OTHERDTK)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rdtk);
	panelitem_set(axisattr_panel, axisattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(axisattr_panel, axisattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(axisattr_panel, axisattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_axis->getsequence());
	panelitem_set(axisattr_panel, axisattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_mvloc1, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_mvloc2, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);

	ice_op= AXIS_ATTR;
	XMapRaised(dpy, axisattr_frame);
	return;
}

void
axisattraxiswd_proc(Panel *p, Panel_item *pi)
{
	int aw;

	aw= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (aw) {
	case AXIS_GLOBALWIDTH:
		panelitem_set(p, axisattr_axiswidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case AXIS_OTHERWIDTH:
		panelitem_set(p, axisattr_axiswidth, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrtickloc_proc(Panel *p, Panel_item *pi)
{
	int tl;
	void axisattrtickwd_proc(Panel *, Panel_item *);

	tl= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (tl) {
	case AXIS_STDLOC:
	case AXIS_ALTLOC:
		panelitem_set(p, axisattr_ptickht, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_stickht, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_ttickht, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_tickwd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		axisattrtickwd_proc(p, axisattr_tickwd);
		break;
	case AXIS_NOLOC:
		panelitem_set(p, axisattr_ptickht, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_stickht, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_ttickht, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_tickwd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_tickwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrtickwd_proc(Panel *p, Panel_item *pi)
{
	int tw;

	tw= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (tw) {
	case AXIS_GLOBALWIDTH:
		panelitem_set(p, axisattr_tickwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case AXIS_OTHERWIDTH:
		panelitem_set(p, axisattr_tickwidth, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrlin_proc(Panel *p, Panel_item *pi)
{
	int lin;

	lin= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (lin) {
	case AXIS_GLOBALLINE:
	case AXIS_BLACKLINE:
	case AXIS_WHITELINE:
		panelitem_set(p, axisattr_rlin, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_glin, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_blin, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case AXIS_OTHERLINE:
		panelitem_set(p, axisattr_rlin, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_glin, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_blin, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrfontloc_proc(Panel *p, Panel_item *pi)
{
	int fl;
	void axisattrfont_proc(Panel *, Panel_item *);
	void axisattrfontsz_proc(Panel *, Panel_item *);
	void axisattrlab_proc(Panel *, Panel_item *);

	fl= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fl) {
	case AXIS_STDLOC:
	case AXIS_ALTLOC:
		panelitem_set(p, axisattr_font, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		axisattrfont_proc(p, axisattr_font);
		panelitem_set(p, axisattr_fontsz, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		axisattrfontsz_proc(p, axisattr_fontsz);
		panelitem_set(p, axisattr_fontorient, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_fontoff, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_lab, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		axisattrlab_proc(p, axisattr_lab);
		break;
	case AXIS_NOLOC:
		panelitem_set(p, axisattr_font, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_fontfamily, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axis_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_fontsz, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_fontsize, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_fontorient, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_fontoff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_rlab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_glab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_blab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrfont_proc(Panel *p, Panel_item *pi)
{
	int f;

	f= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (f) {
	case AXIS_GLOBALFONT:
		panelitem_set(p, axisattr_fontfamily, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axis_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case AXIS_OTHERFONT:
		panelitem_set(p, axisattr_fontfamily, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axis_psff, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrfontfamily_proc(Panel *p, Panel_item *pi)
{
	int f;

	f= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	if (axis_psff != (Panel_item *) NULL)
		panelitem_set(p, axis_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	axis_psff= psfontfamilies[f].psff_axispi;
	panelitem_set(p, axis_psff, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	return;
}

void
axisattrfontsz_proc(Panel *p, Panel_item *pi)
{
	int f;

	f= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (f) {
	case AXIS_GLOBALFONTSZ:
		panelitem_set(p, axisattr_fontsize, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case AXIS_OTHERFONTSZ:
		panelitem_set(p, axisattr_fontsize, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrlab_proc(Panel *p, Panel_item *pi)
{
	int lab;

	lab= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (lab) {
	case AXIS_GLOBALLABEL:
	case AXIS_BLACKLABEL:
	case AXIS_WHITELABEL:
		panelitem_set(p, axisattr_rlab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_glab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_blab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case AXIS_OTHERLABEL:
		panelitem_set(p, axisattr_rlab, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_glab, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_blab, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, axisattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, axisattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, axisattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
axisattrmvloc1_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case AXIS_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case AXIS_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= AXIS_MVLOC1;

	if (pg_loc == PG_CURSORLOC) {
		attr_axis->getend(&fx, &fy, &axis_ex, &axis_ey);
		axis_ey= pg_pixheight-1-axis_ey;
		axis_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) attr_axis;
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
axisattrmvloc2_proc(Panel *p, Panel_item *pi)
{
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];

	switch (ice_op) {
	case AXIS_MVLOC1:
		ice_err("Origin location edit already in progress.", NONFATAL);
		return;
	case AXIS_MVLOC2:
		ice_err("Terminus location edit already in progress.", NONFATAL);
		return;
	}

	ice_op= AXIS_MVLOC2;

	if (pg_loc == PG_CURSORLOC) {
		attr_axis->getloc(&fx, &fy, &axis_x, &axis_y);
		axis_y= pg_pixheight-1-axis_y;
		axis_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) attr_axis;
	attr_axis->getend(&fx, &fy, &ix, &iy);
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
axisattrcont_proc(Panel *p, Panel_item *pi)
{
	char *axisfontnm, *cptr, *buf;
	double orig, term;
	float awidth, twidth;
	int ll, sd, aw, tl, tw, lin;
	float pt, st, tt;
	int fl, font, ff, fn, fs, fo, lab, dtk, seq;
	float fsize, foff;
	unsigned char rlin, glin, blin;
	unsigned char rlab, glab, blab;
	char *pathnm;
	Path *pth, *opth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char errmsg[MAX_ERRMSGLEN+1];

	switch (ice_op) {
	case AXIS_INSERTATTR:
		axisinscont_proc(p, pi);
		return;
	case AXIS_MVLOC1:
		ice_err("Origin location edit still in progress.", NONFATAL);
		return;
	case AXIS_MVLOC2:
		ice_err("Terminus location edit still in progress.", NONFATAL);
		return;
	}

	XUnmapWindow(dpy, axisattr_frame);
	ice_op= MAIN_MENU;

	buf= (char *) panelitem_get(axisattr_panel, axisattr_orig, LXPTEXT_VALUE);
	orig= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid origin value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(axisattr_panel, axisattr_term, LXPTEXT_VALUE);
	term= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid terminus value.", NONFATAL);
		return;
	}
	if (term < orig) {
		ice_err("Terminus value may not be less than origin value.", NONFATAL);
		return;
	}

	ll= *((int *) panelitem_get(axisattr_panel, axisattr_linlog, LXPENUM_VALUE));
	if ((ll == AXIS_LOG) &&
	    ((orig <= 0.) || (term <= 0.))) {
		ice_err("Invalid non-positive logarithmic axis.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(axisattr_panel, axisattr_div, LXPTEXT_VALUE);
	sd= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid subdivision value.", NONFATAL);
		return;
	}
	if (sd < 0) {
		ice_err("Invalid subdivision value.", NONFATAL);
		return;
	}

	aw= *((int *) panelitem_get(axisattr_panel, axisattr_axiswd, LXPENUM_VALUE));
	switch (aw) {
	case AXIS_GLOBALWIDTH:
		awidth= gdf_linewidth;
		break;
	case AXIS_OTHERWIDTH:
		buf= (char *) panelitem_get(axisattr_panel, axisattr_axiswidth, LXPTEXT_VALUE);
		awidth= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid axis width value.", NONFATAL);
			return;
		}
		if (awidth < 0.) {
			ice_err("Axis width value must be non-negative.", NONFATAL);
			return;
		}
		break;
	}

	tl= *((int *) panelitem_get(axisattr_panel, axisattr_tickloc, LXPENUM_VALUE));
	switch (tl) {
	case AXIS_STDLOC:
	case AXIS_ALTLOC:
		tw= *((int *) panelitem_get(axisattr_panel, axisattr_tickwd, LXPENUM_VALUE));
		switch (tw) {
		case AXIS_GLOBALWIDTH:
			twidth= gdf_linewidth;
			break;
		case AXIS_OTHERWIDTH:
			buf= (char *) panelitem_get(axisattr_panel, axisattr_tickwidth, LXPTEXT_VALUE);
			twidth= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid tick width value.", NONFATAL);
				return;
			}
			if (twidth < 0.) {
				ice_err("Tick width value must be non-negative.", NONFATAL);
				return;
			}
			break;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_ptickht, LXPTEXT_VALUE);
		pt= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid primary tick height value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_stickht, LXPTEXT_VALUE);
		st= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid secondary tick height value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_ttickht, LXPTEXT_VALUE);
		tt= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid tertiary tick height value.", NONFATAL);
			return;
		}
		break;
	case AXIS_NOLOC:
		tw= AXIS_GLOBALWIDTH;
		twidth= gdf_linewidth;
		pt= 7.;
		st= 5.;
		tt= 3.;
		break;
	}

	lin= *((int *) panelitem_get(axisattr_panel, axisattr_lin, LXPENUM_VALUE));
	switch (lin) {
	case AXIS_GLOBALLINE:
		rlin= gdf_rfg;
		glin= gdf_gfg;
		blin= gdf_bfg;
		break;
	case AXIS_BLACKLINE:
		rlin= glin= blin= 0;
		break;
	case AXIS_WHITELINE:
		rlin= glin= blin= 255;
		break;
	case AXIS_OTHERLINE:
		buf= (char *) panelitem_get(axisattr_panel, axisattr_rlin, LXPTEXT_VALUE);
		rlin= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red line value.", NONFATAL);
			return;
		}
		if ((rlin < 0) || (rlin > 255)) {
			ice_err("Invalid red line value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_glin, LXPTEXT_VALUE);
		glin= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green line value.", NONFATAL);
			return;
		}
		if ((glin < 0) || (glin > 255)) {
			ice_err("Invalid green line value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_blin, LXPTEXT_VALUE);
		blin= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue line value.", NONFATAL);
			return;
		}
		if ((blin < 0) || (blin > 255)) {
			ice_err("Invalid blue line value.", NONFATAL);
			return;
		}
		break;
	}

	fl= *((int *) panelitem_get(axisattr_panel, axisattr_fontloc, LXPENUM_VALUE));
	switch (fl) {
	case AXIS_STDLOC:
	case AXIS_ALTLOC:
		font= *((int *) panelitem_get(axisattr_panel, axisattr_font, LXPENUM_VALUE));
		switch (font) {
		case AXIS_GLOBALFONT:
			axisfontnm= gdf_fontname;
			break;
		case AXIS_OTHERFONT:
			ff= *((int *) panelitem_get(axisattr_panel, axisattr_fontfamily, LXPENUM_VALUE));
			fn= *((int *) panelitem_get(axisattr_panel, axis_psff, LXPENUM_VALUE));
			axisfontnm= psfontfamilies[ff].psff_fonts[fn];
			break;
		}

		fs= *((int *) panelitem_get(axisattr_panel, axisattr_fontsz, LXPENUM_VALUE));
		switch (fs) {
		case AXIS_GLOBALFONTSZ:
			fsize= gdf_fontsize;
			break;
		case AXIS_OTHERFONTSZ:
			buf= (char *) panelitem_get(axisattr_panel, axisattr_fontsize, LXPTEXT_VALUE);
			fsize= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid font size value.", NONFATAL);
				return;
			}
			if (fsize <= 0.) {
				ice_err("Font size value must be greater than 0.", NONFATAL);
				return;
			}
			break;
		}
		fo= *((int *) panelitem_get(axisattr_panel, axisattr_fontorient, LXPENUM_VALUE));
		buf= (char *) panelitem_get(axisattr_panel, axisattr_fontoff, LXPTEXT_VALUE);
		foff= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid font offset value.", NONFATAL);
			return;
		}
		break;
	case AXIS_NOLOC:
		font= AXIS_GLOBALFONT;
		axisfontnm= gdf_fontname;
		fs= AXIS_GLOBALFONTSZ;
		fsize= gdf_fontsize;
		fo= AXIS_FONTORIENT0;
		foff= gdf_fontsize;
		break;
	}

	lab= *((int *) panelitem_get(axisattr_panel, axisattr_lab, LXPENUM_VALUE));
	switch (lab) {
	case AXIS_GLOBALLABEL:
		rlab= gdf_rfg;
		glab= gdf_gfg;
		blab= gdf_bfg;
		break;
	case AXIS_BLACKLABEL:
		rlab= glab= blab= 0;
		break;
	case AXIS_WHITELABEL:
		rlab= glab= blab= 255;
		break;
	case AXIS_OTHERLABEL:
		buf= (char *) panelitem_get(axisattr_panel, axisattr_rlab, LXPTEXT_VALUE);
		rlab= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red label value.", NONFATAL);
			return;
		}
		if ((rlab < 0) || (rlab > 255)) {
			ice_err("Invalid red label value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_glab, LXPTEXT_VALUE);
		glab= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green label value.", NONFATAL);
			return;
		}
		if ((glab < 0) || (glab > 255)) {
			ice_err("Invalid green label value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_blab, LXPTEXT_VALUE);
		blab= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue label value.", NONFATAL);
			return;
		}
		if ((blab < 0) || (blab > 255)) {
			ice_err("Invalid blue label value.", NONFATAL);
			return;
		}
		break;
	}

	pathnm= (char *) panelitem_get(axisattr_panel, axisattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(axisattr_panel, axisattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(axisattr_panel, axisattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(axisattr_panel, axisattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(axisattr_panel, axisattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_axis->setotl(orig, term, ll);
	(void) attr_axis->setsubdiv(sd);
	(void) attr_axis->setaxiswidth(aw, awidth);
	(void) attr_axis->settick(tl, pt, st, tt, tw, twidth);
	(void) attr_axis->setline(lin, rlin, glin, blin);
	(void) attr_axis->setfont(font, axisfontnm);
	(void) attr_axis->setfontsize(fs, fsize);
	(void) attr_axis->setlabelattr(fl, fo, foff);
	(void) attr_axis->setlabel(lab, rlab, glab, blab);
	opth= attr_axis->getclip();
	if (opth != pth) {
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_axis->setclip(pth);
	}

	(void) attr_axis->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	attr_axis->setdtkpix(dtkpix);

	if (seq != attr_axis->getsequence()) {
		attr_axis->setsequence(seq);
		attr_axis->sortsequence(&grobjs);
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
axisattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, axisattr_frame);
	ice_op= MAIN_MENU;
	return;
}
