/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Marker.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		mrkinscont_proc(Panel *, Panel_item *);

static Marker *attr_mrk;

void
attrmrk_proc(Menu *m, Menu_item *mi)
{
	char buf[20];
	float radius, bndwidth;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	unsigned char rdtk, gdtk, bdtk;
	int mt, mtype, sz, bndmode, bndwd, bndclr;
	int fillmode, fillclr, dtk, state;
	float hscale, vscale;
	Path *pth;

	if ((attr_mrk= (Marker *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Marker *) NULL) {
		ice_err("Cannot locate selected marker object.", NONFATAL);
		return;
	}

	panelitem_set(mrkattr_panel, mrkattr_name, LXPTEXT_VALUE, attr_mrk->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	attr_mrk->getmarkertype(&mt, &mtype);
	panelitem_set(mrkattr_panel, mrkattr_type, LXPENUM_VALUE, mt, LXPI_NULL);

	attr_mrk->getsize(&sz, &radius);
	panelitem_set(mrkattr_panel, mrkattr_size, LXPENUM_VALUE, sz, LXPI_NULL);
	switch (sz) {
	case MARKER_GLOBALSIZE:
		state= LXPI_INACTIVE;
		break;
	case MARKER_OTHERSIZE:
		state= LXPI_ACTIVE;
		break;
	}
	(void) gconvert((double) radius, 10, 0, buf);
	panelitem_set(mrkattr_panel, mrkattr_radius, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_mrk->getboundary(&bndmode, &bndwd, &bndwidth, &bndclr, &rbnd, &gbnd, &bbnd);
	panelitem_set(mrkattr_panel, mrkattr_bndmode, LXPENUM_VALUE, bndmode, LXPI_NULL);
	if (bndmode == MARKER_OPAQUEBNDM)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(mrkattr_panel, mrkattr_bndwd, LXPI_STATE, state, LXPENUM_VALUE, bndwd, LXPI_NULL);
	panelitem_set(mrkattr_panel, mrkattr_bndcolor, LXPI_STATE, state, LXPENUM_VALUE, bndclr, LXPI_NULL);
	if ((bndmode == MARKER_OPAQUEBNDM) && (bndwd == MARKER_OTHERBNDWIDTH))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) gconvert((double) bndwidth, 10, 0, buf);
	panelitem_set(mrkattr_panel, mrkattr_bwidth, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	if ((bndmode == MARKER_OPAQUEBNDM) && (bndclr == MARKER_OTHERBND))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rbnd);
	panelitem_set(mrkattr_panel, mrkattr_rbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gbnd);
	panelitem_set(mrkattr_panel, mrkattr_gbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bbnd);
	panelitem_set(mrkattr_panel, mrkattr_bbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_mrk->getfill(&fillmode, &fillclr, &rfill, &gfill, &bfill);
	panelitem_set(mrkattr_panel, mrkattr_fillmode, LXPENUM_VALUE, fillmode, LXPI_NULL);
	if (fillmode == MARKER_OPAQUEFILLM)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(mrkattr_panel, mrkattr_fillcolor, LXPI_STATE, state, LXPENUM_VALUE, fillclr, LXPI_NULL);
	if ((fillmode == MARKER_OPAQUEFILLM) && (fillclr == MARKER_OTHERFILL))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rfill);
	panelitem_set(mrkattr_panel, mrkattr_rfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gfill);
	panelitem_set(mrkattr_panel, mrkattr_gfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bfill);
	panelitem_set(mrkattr_panel, mrkattr_bfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_mrk->getscale(&hscale, &vscale);
	(void) gconvert((double) hscale, 10, 0, buf);
	panelitem_set(mrkattr_panel, mrkattr_hscale, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) vscale, 10, 0, buf);
	panelitem_set(mrkattr_panel, mrkattr_vscale, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) gconvert((double) attr_mrk->getrotation(), 10, 0, buf);
	panelitem_set(mrkattr_panel, mrkattr_rot, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_mrk->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(mrkattr_panel, mrkattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(mrkattr_panel, mrkattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_mrk->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(mrkattr_panel, mrkattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
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
	panelitem_set(mrkattr_panel, mrkattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(mrkattr_panel, mrkattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(mrkattr_panel, mrkattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_mrk->getsequence());
	panelitem_set(mrkattr_panel, mrkattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	ice_op= MRK_ATTR;
	XMapRaised(dpy, mrkattr_frame);
	return;
}

void
mrkattrsz_proc(Panel *p, Panel_item *pi)
{
	int sz;

	sz= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (sz) {
	case MARKER_GLOBALSIZE:
		panelitem_set(p, mrkattr_radius, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case MARKER_OTHERSIZE:
		panelitem_set(p, mrkattr_radius, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
mrkattrbndmode_proc(Panel *p, Panel_item *pi)
{
	int bndmode;
	void mrkattrbndwd_proc(Panel *p, Panel_item *pi);
	void mrkattrbndclr_proc(Panel *p, Panel_item *pi);

	bndmode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndmode) {
	case MARKER_OPAQUEBNDM:
		panelitem_set(p, mrkattr_bndwd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		mrkattrbndwd_proc(p, mrkattr_bndwd);
		panelitem_set(p, mrkattr_bndcolor, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		mrkattrbndclr_proc(p, mrkattr_bndcolor);
		break;
	case MARKER_TRANSPARENTBNDM:
		panelitem_set(p, mrkattr_bndwd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bndcolor, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
mrkattrbndwd_proc(Panel *p, Panel_item *pi)
{
	int bndwd;

	bndwd= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndwd) {
	case MARKER_GLOBALBNDWIDTH:
		panelitem_set(p, mrkattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case MARKER_OTHERBNDWIDTH:
		panelitem_set(p, mrkattr_bwidth, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
mrkattrbndclr_proc(Panel *p, Panel_item *pi)
{
	int bndclr;

	bndclr= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndclr) {
	case MARKER_GLOBALBND:
	case MARKER_BLACKBND:
	case MARKER_WHITEBND:
		panelitem_set(p, mrkattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case MARKER_OTHERBND:
		panelitem_set(p, mrkattr_rbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
mrkattrfillmode_proc(Panel *p, Panel_item *pi)
{
	int mode;
	void mrkattrfillclr_proc(Panel *, Panel_item *);

	mode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (mode) {
	case MARKER_TRANSPARENTFILLM:
		panelitem_set(p, mrkattr_fillcolor, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case MARKER_OPAQUEFILLM:
		panelitem_set(p, mrkattr_fillcolor, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		mrkattrfillclr_proc(p, mrkattr_fillcolor);
		break;
	}
	return;
}

void
mrkattrfillclr_proc(Panel *p, Panel_item *pi)
{
	int fillclr;

	fillclr= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fillclr) {
	case MARKER_GLOBALFILL:
	case MARKER_WHITEFILL:
	case MARKER_BLACKFILL:
		panelitem_set(p, mrkattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case MARKER_OTHERFILL:
		panelitem_set(p, mrkattr_rfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
mrkattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, mrkattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, mrkattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, mrkattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
mrkattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	float radius, bndwidth, hscale, vscale, rot;
	int mt, mtype, sz, bndmode, bndwd, bnd, fillmode, fill, dtk, seq;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	char *pathnm;
	Path *pth, *opth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char errmsg[MAX_ERRMSGLEN+1];

	if (ice_op == MRK_INSERTATTR) {
		mrkinscont_proc(p, pi);
		return;
	}

	XUnmapWindow(dpy, mrkattr_frame);
	ice_op= MAIN_MENU;

	mt= *((int *) panelitem_get(mrkattr_panel, mrkattr_type, LXPENUM_VALUE));
	switch (mt) {
	case MARKER_GLOBALTYPE:
		mtype= gdf_mrktype+1;
		break;
	default:
		mtype= mt;
		break;
	}

	sz= *((int *) panelitem_get(mrkattr_panel, mrkattr_size, LXPENUM_VALUE));
	switch (sz) {
	case MARKER_GLOBALSIZE:
		radius= gdf_mrksize;
		break;
	case MARKER_OTHERSIZE:
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_radius, LXPTEXT_VALUE);
		radius= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid size value.", NONFATAL);
			ice_op= MAIN_MENU;
			return;
		}
		if (radius <= 0.) {
			ice_err("Size value must be greater than 0.", NONFATAL);
			ice_op= MAIN_MENU;
			return;
		}
		break;
	}

	bndmode= *((int *) panelitem_get(mrkattr_panel, mrkattr_bndmode, LXPENUM_VALUE));
	switch (bndmode) {
	case MARKER_OPAQUEBNDM:
		bndwd= *((int *) panelitem_get(mrkattr_panel, mrkattr_bndwd, LXPENUM_VALUE));
		switch (bndwd) {
		case MARKER_GLOBALBNDWIDTH:
			bndwidth= gdf_bndwidth;
			break;
		case MARKER_OTHERBNDWIDTH:
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bwidth, LXPTEXT_VALUE);
			bndwidth= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid boundary width value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			if (bndwidth < 0.) {
				ice_err("Boundary width value may not be less than 0.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			break;
		}

		bnd= *((int *) panelitem_get(mrkattr_panel, mrkattr_bndcolor, LXPENUM_VALUE));
		switch (bnd) {
		case MARKER_GLOBALBND:
			rbnd= gdf_rbnd;
			gbnd= gdf_gbnd;
			bbnd= gdf_bbnd;
			break;
		case MARKER_BLACKBND:
			rbnd= gbnd= bbnd= 0;
			break;
		case MARKER_WHITEBND:
			rbnd= gbnd= bbnd= 255;
			break;
		case MARKER_OTHERBND:
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rbnd, LXPTEXT_VALUE);
			rbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			if ((rbnd < 0) || (rbnd > 255)) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_gbnd, LXPTEXT_VALUE);
			gbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			if ((gbnd < 0) || (gbnd > 255)) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bbnd, LXPTEXT_VALUE);
			bbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid blue boundary color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			if ((bbnd < 0) || (bbnd > 255)) {
				ice_err("Invalid blue boundary color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			break;
		}
		break;
	case MARKER_TRANSPARENTBNDM:
		bndwd= MARKER_GLOBALBNDWIDTH;
		bndwidth= gdf_bndwidth;
		bnd= MARKER_GLOBALBND;
		rbnd= gdf_rbnd;
		gbnd= gdf_gbnd;
		bbnd= gdf_bbnd;
		break;
	}

	fillmode= *((int *) panelitem_get(mrkattr_panel, mrkattr_fillmode, LXPENUM_VALUE));
	switch (fillmode) {
	case MARKER_TRANSPARENTFILLM:
		fill= MARKER_GLOBALFILL;
		rfill= gdf_rfill;
		gfill= gdf_gfill;
		bfill= gdf_bfill;
		break;
	case MARKER_OPAQUEFILLM:
		fill= *((int *) panelitem_get(mrkattr_panel, mrkattr_fillcolor, LXPENUM_VALUE));
		switch (fill) {
		case MARKER_GLOBALFILL:
			rfill= gdf_rfill;
			gfill= gdf_gfill;
			bfill= gdf_bfill;
			break;
		case MARKER_WHITEFILL:
			rfill= gfill= bfill= 255;
			break;
		case MARKER_BLACKFILL:
			rfill= gfill= bfill= 0;
			break;
		case MARKER_OTHERFILL:
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rfill, LXPTEXT_VALUE);
			rfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red fill color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			if ((rfill < 0) || (rfill > 255)) {
				ice_err("Invalid red fill color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_gfill, LXPTEXT_VALUE);
			gfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green fill color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			if ((gfill < 0) || (gfill > 255)) {
				ice_err("Invalid green fill color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bfill, LXPTEXT_VALUE);
			bfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid blue fill color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			if ((bfill < 0) || (bfill > 255)) {
				ice_err("Invalid blue fill color value.", NONFATAL);
				ice_op= MAIN_MENU;
				return;
			}
			break;
		}
		break;
	}

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_hscale, LXPTEXT_VALUE);
	hscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid horizontal scale value.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_vscale, LXPTEXT_VALUE);
	vscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid vertical scale value.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rot, LXPTEXT_VALUE);
	rot= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid rotation value.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	pathnm= (char *) panelitem_get(mrkattr_panel, mrkattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(mrkattr_panel, mrkattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_mrk->setmarkertype(mt, mtype);
	(void) attr_mrk->setsize(sz, radius);
	(void) attr_mrk->setboundary(bndmode, bndwd, bndwidth, bnd, rbnd, gbnd, bbnd);
	(void) attr_mrk->setfill(fillmode, fill, rfill, gfill, bfill);
	attr_mrk->setscale(hscale, vscale);
	attr_mrk->setrotation(rot);
	opth= attr_mrk->getclip();
	if (opth != pth) {
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_mrk->setclip(pth);
	}

	(void) attr_mrk->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	attr_mrk->setdtkpix(dtkpix);

	if (seq != attr_mrk->getsequence()) {
		attr_mrk->setsequence(seq);
		attr_mrk->sortsequence(&grobjs);
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
mrkattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, mrkattr_frame);
	ice_op= MAIN_MENU;
	return;
}
