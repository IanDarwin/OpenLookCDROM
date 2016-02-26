/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Axis.h"
#include "Pathdup.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attraxis_proc(Menu *, Menu_item *);
extern void		axis_del(Axis *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cpaxis_proc(Menu *, Menu_item *);
extern void		delaxis_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		traxis_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

boolean axis_drawn;
int axis_x, axis_y;
int axis_ex, axis_ey;

static Axis *new_axis;

void
insaxis_proc(Menu *m, Menu_item *mi)
{
	boolean found;
	int ff, fn;
	char buf[80];

	panelitem_set(axisattr_panel, axisattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_mvloc1, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_mvloc2, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		axis_drawn= FALSE;
		ice_op= AXIS_INSERTATTR;
		XMapRaised(dpy, axisattr_frame);
		return;
	}

	panelitem_set(axisattr_panel, axisattr_orig, LXPTEXT_VALUE, "0", LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_term, LXPTEXT_VALUE, "100", LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_linlog, LXPENUM_VALUE, AXIS_LINEAR, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_div, LXPTEXT_VALUE, "10", LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_axiswd, LXPENUM_VALUE, AXIS_GLOBALWIDTH, LXPI_NULL);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_axiswidth, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_tickloc, LXPENUM_VALUE, AXIS_STDLOC, LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_ptickht, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, "7", LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_stickht, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, "5", LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_ttickht, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, "3", LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_tickwd, LXPENUM_VALUE, AXIS_GLOBALWIDTH, LXPI_NULL);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_tickwidth, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_lin, LXPENUM_VALUE, AXIS_GLOBALLINE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfg);
	panelitem_set(axisattr_panel, axisattr_rlin, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	panelitem_set(axisattr_panel, axisattr_glin, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	panelitem_set(axisattr_panel, axisattr_blin, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_fontloc, LXPENUM_VALUE, AXIS_STDLOC, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_font, LXPENUM_VALUE, AXIS_GLOBALFONT, LXPI_NULL);
	found= FALSE;
	for (ff= 0; ff < npsfontfamilies; ff++) {
		for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
			if (!strcmp(gdf_fontname, psfontfamilies[ff].psff_fonts[fn])) {
				found= TRUE;
				break;
			}
		}
		if (found == TRUE)
			break;
	}
	panelitem_set(axisattr_panel, axisattr_fontfamily, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, ff, LXPI_NULL);
	if (axis_psff != (Panel_item *) NULL)
		panelitem_set(axisattr_panel, axis_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	axis_psff= psfontfamilies[ff].psff_axispi;
	panelitem_set(axisattr_panel, axis_psff, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, fn, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_fontsz, LXPENUM_VALUE, AXIS_GLOBALFONTSZ, LXPI_NULL);
	(void) gconvert((double) gdf_fontsize, 10, 0, buf);
	panelitem_set(axisattr_panel, axisattr_fontsize, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	panelitem_set(axisattr_panel, axisattr_fontoff, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_fontorient, LXPENUM_VALUE, AXIS_FONTORIENT0, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_lab, LXPENUM_VALUE, AXIS_GLOBALLABEL, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfg);
	panelitem_set(axisattr_panel, axisattr_rlab, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	panelitem_set(axisattr_panel, axisattr_glab, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	panelitem_set(axisattr_panel, axisattr_blab, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	panelitem_set(axisattr_panel, axisattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	panelitem_set(axisattr_panel, axisattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	panelitem_set(axisattr_panel, axisattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(axisattr_panel, axisattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	axis_drawn= FALSE;
	ice_op= AXIS_INSERTATTR;
	XMapRaised(dpy, axisattr_frame);
	return;
}

void
axisinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *name, *axisfontnm, *buf;
	double orig, term;
	float awidth, twidth, fsize, foff;
	int sd, ll, aw, tl, tw, lin;
	int fl, font, ff, fn, fs, fo, lab, dtk, seq;
	float pt, st, tt;
	unsigned char rlin, glin, blin;
	unsigned char rlab, glab, blab;
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

	XUnmapWindow(dpy, axisattr_frame);
	ice_op= MAIN_MENU;

	name= (char *) panelitem_get(axisattr_panel, axisattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		(void) sprintf(nmbuf, "UnnamedAxis-%d", unnamed_axes++);
		name= nmbuf;
	}

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

	if ((new_axis= new Axis((Dlnk **) &grobjs, name, seq)) == (Axis *) NULL) {
		ice_err("Cannot create axis object.", NONFATAL);
		return;
	}
	naxes++;
	(void) new_axis->setotl(orig, term, ll);
	(void) new_axis->setsubdiv(sd);
	(void) new_axis->setaxiswidth(aw, awidth);
	(void) new_axis->settick(tl, pt, st, tt, tw, twidth);
	(void) new_axis->setline(lin, rlin, glin, blin);
	(void) new_axis->setfont(font, axisfontnm);
	(void) new_axis->setfontsize(fs, fsize);
	(void) new_axis->setlabelattr(fl, fo, foff);
	(void) new_axis->setlabel(lab, rlab, glab, blab);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_axis->setclip(pth);

	(void) new_axis->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_axis->setdtkpix(dtkpix);

	ice_op= AXIS_INSERTLOC1;

	if (pg_loc == PG_CURSORLOC) {
		pgcoords_drawn= crosshairs_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, delaxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, attraxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_axis,
				LXMI_PROC, traxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(new_axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
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
		ice_op= MAIN_MENU;
		return;
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
		ice_op= MAIN_MENU;
		return;
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
		ice_op= MAIN_MENU;
		return;
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

	loc_grobj= (Grobj *) new_axis;
	panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Axis Origin", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
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
	return;
}

void
insiceaxis_rd(FILE *fp, int gdf, int newobj)
{
	char *name, *axisfontnm;
	double orig, term;
	float awidth, twidth;
	int ll, sd, aw, tl, tw;
	int fl, font, fs, fo;
	float pt, st, tt;
	float fsize, foff;
	int len, lin, lab, dtk, seq;
	int ir, ig, ib;
	float ox, oy, tx, ty;
	int iot;
	boolean endfound;
	char *c, *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	unsigned char rlin, glin, blin;
	unsigned char rlab, glab, blab;
	char nmbuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Axis *axis;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Axis: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Axis: Begin ")+len-1)= '\0';
	if ((name= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(name, ice_iobuf+strlen("%%ICE-Axis: Begin "));
	if (!strncmp(name, "UnnamedAxis-", strlen("UnnamedAxis-"))) {
		delete name;
		(void) sprintf(nmbuf, "UnnamedAxis-%d", unnamed_axes++);
		if ((name= new char[strlen(nmbuf)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(name, nmbuf);
	}

	ox= oy= tx= ty= 0.;
	orig= term= 0.;
	ll= AXIS_LINEAR;
	aw= AXIS_GLOBALWIDTH;
	awidth= gdf_linewidth;
	tl= AXIS_STDLOC;
	pt= 7.;
	st= 5.;
	tt= 3.;
	tw= AXIS_GLOBALWIDTH;
	twidth= gdf_linewidth;
	lin= AXIS_GLOBALLINE;
	rlin= gdf_rfg;
	glin= gdf_gfg;
	blin= gdf_bfg;
	fl= AXIS_STDLOC;
	font= AXIS_GLOBALFONT;
	if ((axisfontnm= new char[strlen(gdf_fontname)+1]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		delete name;
		return;
	}
	(void) strcpy(axisfontnm, gdf_fontname);
	fs= AXIS_GLOBALFONTSZ;
	fsize= gdf_fontsize;
	fo= AXIS_FONTORIENT0;
	foff= gdf_fontsize;
	lab= AXIS_GLOBALLABEL;
	rlab= gdf_rfg;
	glab= gdf_gfg;
	blab= gdf_bfg;
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

		if (!strncmp("%%ICE-Axis: Loc ", ice_iobuf, strlen("%%ICE-Axis: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: Loc "), "%f %f %f %f", &ox, &oy, &tx, &ty) != 4) {
				ice_err("Invalid axis location value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Axis: OTL ", ice_iobuf, strlen("%%ICE-Axis: OTL "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: OTL "), "%lf %lf %d", &orig, &term, &ll) != 3) {
				ice_err("Invalid axis OTL value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if (term < orig) {
				ice_err("Terminus value may not be less than origin value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ll != AXIS_LINEAR) &&
			    (ll != AXIS_LOG)) {
				ice_err("Invalid axis OTL mode value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ll == AXIS_LOG) &&
			    ((orig <= 0.) || (term <= 0.))) {
				ice_err("Invalid non-positive logarithmic axis.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Axis: AF ", ice_iobuf, strlen("%%ICE-Axis: AF "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: AF "), "%d %d %f", &sd, &aw, &awidth) != 3) {
				ice_err("Invalid axis flags value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if (sd < 0) {
				ice_err("Invalid axis subdivision value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((aw != AXIS_GLOBALWIDTH) &&
			    (aw != AXIS_OTHERWIDTH)) {
				ice_err("Invalid axis width value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if (awidth < 0.) {
				ice_err("Invalid axis width value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Axis: TF ", ice_iobuf, strlen("%%ICE-Axis: TF "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: TF "), "%d %f %f %f %d %f", &tl, &pt, &st, &tt, &tw, &twidth) != 6) {
				ice_err("Invalid axis tick flags value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((tl != AXIS_STDLOC) &&
			    (tl != AXIS_ALTLOC) &&
			    (tl != AXIS_NOLOC)) {
				ice_err("Invalid axis tick location value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((tw != AXIS_GLOBALWIDTH) &&
			    (tw != AXIS_OTHERWIDTH)) {
				ice_err("Invalid axis tick width value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if (twidth < 0.) {
				ice_err("Invalid axis tick width value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Axis: Line ", ice_iobuf, strlen("%%ICE-Axis: Line "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: Line "), "%d %d %d %d", &lin, &ir, &ig, &ib) != 4) {
				ice_err("Invalid axis line value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((lin != AXIS_GLOBALLINE) &&
			    (lin != AXIS_BLACKLINE) &&
			    (lin != AXIS_WHITELINE) &&
			    (lin != AXIS_OTHERLINE)) {
				ice_err("Invalid axis line value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid axis red line value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid axis green line value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid axis blue line value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			rlin= (unsigned char) ir;
			glin= (unsigned char) ig;
			blin= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Axis: Font ", ice_iobuf, strlen("%%ICE-Axis: Font "))) {
			char *c;

			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: Font "), "%d %d", &fl, &font) != 2) {
				ice_err("Invalid axis font value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((fl != AXIS_STDLOC) &&
			    (fl != AXIS_ALTLOC) &&
			    (fl != AXIS_NOLOC)) {
				ice_err("Invalid axis font location value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((font != AXIS_GLOBALFONT) &&
			    (font != AXIS_OTHERFONT)) {
				ice_err("Invalid axis font value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			c= ice_iobuf+strlen("%%ICE-Axis: Font ");
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			delete axisfontnm;
			len= strlen(c);
			*(c+len-1)= '\0';
			if ((axisfontnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			(void) strcpy(axisfontnm, c);
		}

		else if (!strncmp("%%ICE-Axis: FF ", ice_iobuf, strlen("%%ICE-Axis: FF "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: FF "), "%d %f %d %f", &fs, &fsize, &fo, &foff) != 4) {
				ice_err("Invalid axis font flags value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((fs != AXIS_GLOBALFONTSZ) &&
			    (fs != AXIS_OTHERFONTSZ)) {
				ice_err("Invalid axis font size value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if (fsize <= 0.) {
				ice_err("Axis font size must be greater than 0.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((fo != AXIS_FONTORIENT0) &&
			    (fo != AXIS_FONTORIENT90) &&
			    (fo != AXIS_FONTORIENT180) &&
			    (fo != AXIS_FONTORIENT270)) {
				ice_err("Invalid axis font orientation value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Axis: Label ", ice_iobuf, strlen("%%ICE-Axis: Label "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: Label "), "%d %d %d %d", &lab, &ir, &ig, &ib) != 4) {
				ice_err("Invalid axis label value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((lab != AXIS_GLOBALLABEL) &&
			    (lab != AXIS_BLACKLABEL) &&
			    (lab != AXIS_WHITELABEL) &&
			    (lab != AXIS_OTHERLABEL)) {
				ice_err("Invalid axis label value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid axis red label value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid axis green label value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid axis blue label value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			rlab= (unsigned char) ir;
			glab= (unsigned char) ig;
			blab= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Axis: Clip ", ice_iobuf, strlen("%%ICE-Axis: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Axis: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete axisfontnm;
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strncmp("%%ICE-Axis: DTK ", ice_iobuf, strlen("%%ICE-Axis: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid axis DTK value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid axis DTK value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid axis red DTK value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid axis green DTK value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid axis blue DTK value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Axis: Seq ", ice_iobuf, strlen("%%ICE-Axis: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid axis sequence value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Axis: IOT ", ice_iobuf, strlen("%%ICE-Axis: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Axis: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid axis tag value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid axis tag value.", NONFATAL);
				delete name;
				delete axisfontnm;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Axis: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Axis end not found.", NONFATAL);
		delete name;
		delete axisfontnm;
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
					delete axisfontnm;
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
			delete axisfontnm;
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
			if (aw == AXIS_GLOBALWIDTH)
				awidth= gdf_linewidth;
			if (tw == AXIS_GLOBALWIDTH)
				twidth= gdf_linewidth;
			if (lin == AXIS_GLOBALLINE) {
				rlin= gdf_rfg;
				glin= gdf_gfg;
				blin= gdf_bfg;
			}
			if (font == AXIS_GLOBALFONT) {
				delete axisfontnm;
				if ((axisfontnm= new char[strlen(gdf_fontname)+1]) == (char *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete name;
					return;
				}
				(void) strcpy(axisfontnm, gdf_fontname);
			}
			if (fs == AXIS_GLOBALFONTSZ)
				fsize= gdf_fontsize;
			if (lab == AXIS_GLOBALLABEL) {
				rlab= gdf_rfg;
				glab= gdf_gfg;
				blab= gdf_bfg;
			}
			if (dtk == GROBJ_GLOBALDTK) {
				rdtk= gdf_rdtk;
				gdtk= gdf_gdtk;
				bdtk= gdf_bdtk;
			}
			break;
		case INSICE_NEWOBJNEWGDF:
			if (aw == AXIS_GLOBALWIDTH)
				aw= AXIS_OTHERWIDTH;
			if (tw == AXIS_GLOBALWIDTH)
				tw= AXIS_OTHERWIDTH;
			if (lin == AXIS_GLOBALLINE)
				lin= AXIS_OTHERLINE;
			if (font == AXIS_GLOBALFONT)
				font= AXIS_OTHERFONT;
			if (fs == AXIS_GLOBALFONTSZ)
				fs= AXIS_OTHERFONTSZ;
			if (lab == AXIS_GLOBALLABEL)
				lab= AXIS_OTHERLABEL;
			if (dtk == GROBJ_GLOBALDTK)
				dtk= GROBJ_OTHERDTK;
			break;
		}
	}

	if (npsfonts > 0) {
		int ff, fn;
		boolean found;

		found= FALSE;
		for (ff= 0; ff < npsfontfamilies; ff++) {
			for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
				if (!strcmp(axisfontnm, psfontfamilies[ff].psff_fonts[fn])) {
					found= TRUE;
					break;
				}
			}
			if (found == TRUE)
				break;
		}
		if (found == FALSE) {
			(void) sprintf(errmsg, "Requested axis font '%s' not available.", axisfontnm);
			ice_err(errmsg, NONFATAL);
			delete name;
			delete axisfontnm;
			return;
		}
	}

	if ((axis= new Axis((Dlnk **) &grobjs, name, seq)) == (Axis *) NULL) {
		ice_err("Cannot create axis object.", NONFATAL);
		delete name;
		return;
	}
	delete name;
	naxes++;

	(void) axis->setotl(orig, term, ll);
	(void) axis->setsubdiv(sd);
	(void) axis->setaxiswidth(aw, awidth);
	(void) axis->settick(tl, pt, st, tt, tw, twidth);
	(void) axis->setline(lin, rlin, glin, blin);
	(void) axis->setfont(font, axisfontnm);
	delete axisfontnm;
	(void) axis->setfontsize(fs, fsize);
	(void) axis->setlabelattr(fl, fo, foff);
	(void) axis->setlabel(lab, rlab, glab, blab);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	axis->setclip(pth);
	axis->setiotag(iot);

	(void) axis->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	axis->setdtkpix(dtkpix);

	axis->setfloc(ox, oy, (float) pg_dpi);
	axis->setfend(tx, ty, (float) pg_dpi);

	if ((delitem= menuitem_create(LXMI_STRING, axis->getname(),
				LXMI_CLIENTDATA, (char *) axis,
				LXMI_PROC, delaxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(axis);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, axis->getname(),
				LXMI_CLIENTDATA, (char *) axis,
				LXMI_PROC, attraxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(axis);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, axis->getname(),
				LXMI_CLIENTDATA, (char *) axis,
				LXMI_PROC, traxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, axis->getname(),
				LXMI_CLIENTDATA, (char *) axis,
				LXMI_PROC, cpaxis_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, axis->getname(),
				LXMI_CLIENTDATA, (char *) axis,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, axis->getname(),
				LXMI_CLIENTDATA, (char *) axis,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		axis_del(axis);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
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

	return;
}

void
axisins_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, junk;
	float fx, fy;
	Menu_item *delitem, *attritem;
	Menu_item *tritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (axis_drawn) {
			XDrawLine(dpy, pg_cpm, pg_xgc, axis_x, axis_y, axis_ex, axis_ey);
			XDrawLine(dpy, pg_cwin, pg_xgc, axis_x-vx, axis_y-vy, axis_ex-vx, axis_ey-vy);
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		if (ice_op == AXIS_INSERTLOC1) {
			new_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			axis_x= x;
			axis_y= y;
			ice_op= AXIS_INSERTLOC2;
			return;
		}
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		new_axis->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
		if ((delitem= menuitem_create(LXMI_STRING, new_axis->getname(),
					LXMI_CLIENTDATA, (char *) new_axis,
					LXMI_PROC, delaxis_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			axis_del(new_axis);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_axis->getname(),
					LXMI_CLIENTDATA, (char *) new_axis,
					LXMI_PROC, attraxis_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			axis_del(new_axis);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_axis->getname(),
					LXMI_CLIENTDATA, (char *) new_axis,
					LXMI_PROC, traxis_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			axis_del(new_axis);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_axis->getname(),
					LXMI_CLIENTDATA, (char *) new_axis,
					LXMI_PROC, cpaxis_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			axis_del(new_axis);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_axis->getname(),
					LXMI_CLIENTDATA, (char *) new_axis,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			axis_del(new_axis);
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
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_axis->getname(),
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
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
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
		if (ice_op == AXIS_INSERTLOC1) {
			new_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			new_axis->getloc(&fx, &fy, &x, &junk);
		}
		else {
			new_axis->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			new_axis->getend(&fx, &fy, &x, &junk);
		}
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		if (ice_op == AXIS_INSERTLOC1) {
			new_axis->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			new_axis->getloc(&fx, &fy, &x, &junk);
		}
		else {
			new_axis->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			new_axis->getend(&fx, &fy, &x, &junk);
		}
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (axis_drawn) {
		XDrawLine(dpy, pg_cpm, pg_xgc, axis_x, axis_y, axis_ex, axis_ey);
		XDrawLine(dpy, pg_cwin, pg_xgc, axis_x-vx, axis_y-vy, axis_ex-vx, axis_ey-vy);
	}
	if (ice_op == AXIS_INSERTLOC2) {
		axis_ex= x;
		axis_ey= y;
		XDrawLine(dpy, pg_cpm, pg_xgc, axis_x, axis_y, axis_ex, axis_ey);
		XDrawLine(dpy, pg_cwin, pg_xgc, axis_x-vx, axis_y-vy, axis_ex-vx, axis_ey-vy);
		axis_drawn= TRUE;
	}
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
delaxis_proc(Menu *m, Menu_item *mi)
{
	Axis *axis;
	Menu_item *item;

	if ((axis= (Axis *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Axis *) NULL) {
		ice_err("Cannot locate selected axis object.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	axis_del(axis);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attraxis_menu, LXMI_CLIENTDATA, (char *) axis);
	menuitem_delete(attraxis_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(traxis_menu, LXMI_CLIENTDATA, (char *) axis);
	menuitem_delete(traxis_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cpaxis_menu, LXMI_CLIENTDATA, (char *) axis);
	menuitem_delete(cpaxis_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmpaxis_menu, LXMI_CLIENTDATA, (char *) axis);
	menuitem_delete(dmpaxis_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpopaxis_menu, LXMI_CLIENTDATA, (char *) axis);
	menuitem_delete(cmpopaxis_menu, item);
	menuitem_destroy(item);
	if (naxes == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
axis_del(Axis *axis)
{
	if (grobjs == (Grobj *) axis) {
		if (axis->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) axis->succ();
	}
	axis->unlink();
	delete axis;
	naxes--;
	return;
}
