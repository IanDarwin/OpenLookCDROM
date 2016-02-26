/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Polygon.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		polyinscont_proc(Panel *, Panel_item *);

static Polygon *attr_poly;

void
attrpoly_proc(Menu *m, Menu_item *mi)
{
	char buf[20];
	float bndwidth;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	unsigned char rdtk, gdtk, bdtk;
	int cls, bndmode, bndwd, bndclr, fillmode, fillclr, dtk, state;
	float hscale, vscale;
	Path *pth;

	if ((attr_poly= (Polygon *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Polygon *) NULL) {
		ice_err("Cannot locate selected polygon object.", NONFATAL);
		return;
	}

	panelitem_set(polyattr_panel, polyattr_name, LXPTEXT_VALUE, attr_poly->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	cls= attr_poly->getclosure();
	panelitem_set(polyattr_panel, polyattr_closure, LXPENUM_VALUE, cls, LXPI_NULL);

	attr_poly->getboundary(&bndmode, &bndwd, &bndwidth, &bndclr, &rbnd, &gbnd, &bbnd);
	panelitem_set(polyattr_panel, polyattr_bndmode, LXPENUM_VALUE, bndmode, LXPI_NULL);
	if (bndmode == POLYGON_OPAQUEBNDM)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(polyattr_panel, polyattr_bndwd, LXPI_STATE, state, LXPENUM_VALUE, bndwd, LXPI_NULL);
	panelitem_set(polyattr_panel, polyattr_bndcolor, LXPI_STATE, state, LXPENUM_VALUE, bndclr, LXPI_NULL);
	if ((bndmode == POLYGON_OPAQUEBNDM) && (bndwd == POLYGON_OTHERBNDWIDTH))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) gconvert((double) bndwidth, 10, 0, buf);
	panelitem_set(polyattr_panel, polyattr_bwidth, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	if ((bndmode == POLYGON_OPAQUEBNDM) && (bndclr == POLYGON_OTHERBND))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rbnd);
	panelitem_set(polyattr_panel, polyattr_rbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gbnd);
	panelitem_set(polyattr_panel, polyattr_gbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bbnd);
	panelitem_set(polyattr_panel, polyattr_bbnd, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	if (cls == POLYGON_CLOSED)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	attr_poly->getfill(&fillmode, &fillclr, &rfill, &gfill, &bfill);
	panelitem_set(polyattr_panel, polyattr_fillmode, LXPI_STATE, state, LXPENUM_VALUE, fillmode, LXPI_NULL);
	if ((cls == POLYGON_CLOSED) && (fillmode == POLYGON_OPAQUEFILLM))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(polyattr_panel, polyattr_fillcolor, LXPI_STATE, state, LXPENUM_VALUE, fillclr, LXPI_NULL);
	if ((cls == POLYGON_CLOSED) &&
	    (fillmode == POLYGON_OPAQUEFILLM) &&
	    (fillclr == POLYGON_OTHERFILL))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rfill);
	panelitem_set(polyattr_panel, polyattr_rfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gfill);
	panelitem_set(polyattr_panel, polyattr_gfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bfill);
	panelitem_set(polyattr_panel, polyattr_bfill, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_poly->getscale(&hscale, &vscale);
	(void) gconvert((double) hscale, 10, 0, buf);
	panelitem_set(polyattr_panel, polyattr_hscale, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) vscale, 10, 0, buf);
	panelitem_set(polyattr_panel, polyattr_vscale, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) gconvert((double) attr_poly->getrotation(), 10, 0, buf);
	panelitem_set(polyattr_panel, polyattr_rot, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_poly->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(polyattr_panel, polyattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(polyattr_panel, polyattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_poly->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(polyattr_panel, polyattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
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
	panelitem_set(polyattr_panel, polyattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(polyattr_panel, polyattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(polyattr_panel, polyattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_poly->getsequence());
	panelitem_set(polyattr_panel, polyattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	ice_op= POLY_ATTR;
	XMapRaised(dpy, polyattr_frame);
	return;
}

void
polyattrcls_proc(Panel *p, Panel_item *pi)
{
	int cls;
	void polyattrfillmode_proc(Panel *p, Panel_item *pi);

	cls= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (cls) {
	case POLYGON_CLOSED:
		panelitem_set(p, polyattr_fillmode, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		polyattrfillmode_proc(p, polyattr_fillmode);
		break;
	case POLYGON_OPEN:
		panelitem_set(p, polyattr_fillmode, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_fillcolor, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
polyattrbndmode_proc(Panel *p, Panel_item *pi)
{
	int bndmode;
	void polyattrbndwd_proc(Panel *p, Panel_item *pi);
	void polyattrbndclr_proc(Panel *p, Panel_item *pi);

	bndmode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndmode) {
	case POLYGON_OPAQUEBNDM:
		panelitem_set(p, polyattr_bndwd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		polyattrbndwd_proc(p, polyattr_bndwd);
		panelitem_set(p, polyattr_bndcolor, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		polyattrbndclr_proc(p, polyattr_bndcolor);
		break;
	case POLYGON_TRANSPARENTBNDM:
		panelitem_set(p, polyattr_bndwd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bndcolor, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
polyattrbndwd_proc(Panel *p, Panel_item *pi)
{
	int bndwd;

	bndwd= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndwd) {
	case POLYGON_GLOBALBNDWIDTH:
		panelitem_set(p, polyattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case POLYGON_OTHERBNDWIDTH:
		panelitem_set(p, polyattr_bwidth, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
polyattrbndclr_proc(Panel *p, Panel_item *pi)
{
	int bndclr;

	bndclr= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bndclr) {
	case POLYGON_GLOBALBND:
	case POLYGON_BLACKBND:
	case POLYGON_WHITEBND:
		panelitem_set(p, polyattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case POLYGON_OTHERBND:
		panelitem_set(p, polyattr_rbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
polyattrfillmode_proc(Panel *p, Panel_item *pi)
{
	int mode;
	void polyattrfillclr_proc(Panel *, Panel_item *);

	mode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (mode) {
	case POLYGON_TRANSPARENTFILLM:
		panelitem_set(p, polyattr_fillcolor, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case POLYGON_OPAQUEFILLM:
		panelitem_set(p, polyattr_fillcolor, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		polyattrfillclr_proc(p, polyattr_fillcolor);
		break;
	}
	return;
}

void
polyattrfillclr_proc(Panel *p, Panel_item *pi)
{
	int fillclr;

	fillclr= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fillclr) {
	case POLYGON_GLOBALFILL:
	case POLYGON_WHITEFILL:
	case POLYGON_BLACKFILL:
		panelitem_set(p, polyattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case POLYGON_OTHERFILL:
		panelitem_set(p, polyattr_rfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
polyattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, polyattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, polyattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, polyattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
polyattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	float bndwidth, hscale, vscale, rot;
	int cl, bndmode, bndwd, bnd, fillmode, fill, dtk, seq;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	char *pathnm;
	Path *pth, *opth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char errmsg[MAX_ERRMSGLEN+1];

	if (ice_op == POLY_INSERTATTR) {
		polyinscont_proc(p, pi);
		return;
	}

	XUnmapWindow(dpy, polyattr_frame);
	ice_op= MAIN_MENU;

	cl= *((int *) panelitem_get(polyattr_panel, polyattr_closure, LXPENUM_VALUE));

	bndmode= *((int *) panelitem_get(polyattr_panel, polyattr_bndmode, LXPENUM_VALUE));
	switch (bndmode) {
	case POLYGON_OPAQUEBNDM:
		bndwd= *((int *) panelitem_get(polyattr_panel, polyattr_bndwd, LXPENUM_VALUE));
		switch (bndwd) {
		case POLYGON_GLOBALBNDWIDTH:
			bndwidth= gdf_bndwidth;
			break;
		case POLYGON_OTHERBNDWIDTH:
			buf= (char *) panelitem_get(polyattr_panel, polyattr_bwidth, LXPTEXT_VALUE);
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

		bnd= *((int *) panelitem_get(polyattr_panel, polyattr_bndcolor, LXPENUM_VALUE));
		switch (bnd) {
		case POLYGON_GLOBALBND:
			rbnd= gdf_rbnd;
			gbnd= gdf_gbnd;
			bbnd= gdf_bbnd;
			break;
		case POLYGON_BLACKBND:
			rbnd= gbnd= bbnd= 0;
			break;
		case POLYGON_WHITEBND:
			rbnd= gbnd= bbnd= 255;
			break;
		case POLYGON_OTHERBND:
			buf= (char *) panelitem_get(polyattr_panel, polyattr_rbnd, LXPTEXT_VALUE);
			rbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			if ((rbnd < 0) || (rbnd > 255)) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(polyattr_panel, polyattr_gbnd, LXPTEXT_VALUE);
			gbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			if ((gbnd < 0) || (gbnd > 255)) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(polyattr_panel, polyattr_bbnd, LXPTEXT_VALUE);
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
	case POLYGON_TRANSPARENTBNDM:
		bndwd= POLYGON_GLOBALBNDWIDTH;
		bndwidth= gdf_bndwidth;
		bnd= POLYGON_GLOBALBND;
		rbnd= gdf_rbnd;
		gbnd= gdf_gbnd;
		bbnd= gdf_bbnd;
		break;
	}

	switch (cl) {
	case POLYGON_CLOSED:
		fillmode= *((int *) panelitem_get(polyattr_panel, polyattr_fillmode, LXPENUM_VALUE));
		switch (fillmode) {
		case POLYGON_TRANSPARENTFILLM:
			fill= POLYGON_GLOBALFILL;
			rfill= gdf_rfill;
			gfill= gdf_gfill;
			bfill= gdf_bfill;
			break;
		case POLYGON_OPAQUEFILLM:
			fill= *((int *) panelitem_get(polyattr_panel, polyattr_fillcolor, LXPENUM_VALUE));
			switch (fill) {
			case POLYGON_GLOBALFILL:
				rfill= gdf_rfill;
				gfill= gdf_gfill;
				bfill= gdf_bfill;
				break;
			case POLYGON_WHITEFILL:
				rfill= gfill= bfill= 255;
				break;
			case POLYGON_BLACKFILL:
				rfill= gfill= bfill= 0;
				break;
			case POLYGON_OTHERFILL:
				buf= (char *) panelitem_get(polyattr_panel, polyattr_rfill, LXPTEXT_VALUE);
				rfill= (unsigned char) strtol(buf, &cptr, 10);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid red fill color value.", NONFATAL);
					return;
				}
				if ((rfill < 0) || (rfill > 255)) {
					ice_err("Invalid red fill color value.", NONFATAL);
					return;
				}
				buf= (char *) panelitem_get(polyattr_panel, polyattr_gfill, LXPTEXT_VALUE);
				gfill= (unsigned char) strtol(buf, &cptr, 10);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid green fill color value.", NONFATAL);
					return;
				}
				if ((gfill < 0) || (gfill > 255)) {
					ice_err("Invalid green fill color value.", NONFATAL);
					return;
				}
				buf= (char *) panelitem_get(polyattr_panel, polyattr_bfill, LXPTEXT_VALUE);
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
		break;
	case POLYGON_OPEN:
		fillmode= POLYGON_TRANSPARENTFILLM;
		fill= POLYGON_GLOBALFILL;
		rfill= gdf_rfill;
		gfill= gdf_gfill;
		bfill= gdf_bfill;
		break;
	}

	buf= (char *) panelitem_get(polyattr_panel, polyattr_hscale, LXPTEXT_VALUE);
	hscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid horizontal scale value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(polyattr_panel, polyattr_vscale, LXPTEXT_VALUE);
	vscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid vertical scale value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(polyattr_panel, polyattr_rot, LXPTEXT_VALUE);
	rot= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid rotation value.", NONFATAL);
		return;
	}

	pathnm= (char *) panelitem_get(polyattr_panel, polyattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(polyattr_panel, polyattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(polyattr_panel, polyattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(polyattr_panel, polyattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(polyattr_panel, polyattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(polyattr_panel, polyattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_poly->setclosure(cl);
	(void) attr_poly->setboundary(bndmode, bndwd, bndwidth, bnd, rbnd, gbnd, bbnd);
	(void) attr_poly->setfill(fillmode, fill, rfill, gfill, bfill);
	attr_poly->setscale(hscale, vscale);
	attr_poly->setrotation(rot);
	opth= attr_poly->getclip();
	if (opth != pth) {
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_poly->setclip(pth);
	}

	(void) attr_poly->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	attr_poly->setdtkpix(dtkpix);

	if (seq != attr_poly->getsequence()) {
		attr_poly->setsequence(seq);
		attr_poly->sortsequence(&grobjs);
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
polyattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, polyattr_frame);
	ice_op= MAIN_MENU;
	return;
}
