/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Psdoc.h"
#include "Raster.h"
#include "Text.h"
#include "Vector.h"
#include "Curve.h"
#include "Marker.h"
#include "Rectangle.h"
#include "Polygon.h"
#include "Axis.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_draw();

void
attrgdf_proc(Menu *m, Menu_item *mi)
{
	char buf[80];
	int state;

	if (npsfonts > 0) {
		int ff, fn;
		boolean found;

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
		panelitem_set(gdfattr_panel, gdfattr_fontfamily, LXPENUM_VALUE, ff, LXPI_NULL);
		if (gdf_psff != (Panel_item *) NULL)
			panelitem_set(gdfattr_panel, gdf_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		gdf_psff= psfontfamilies[ff].psff_gdfpi;
		panelitem_set(gdfattr_panel, gdf_psff, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, fn, LXPI_NULL);

		(void) gconvert((double) gdf_fontsize, 10, 0, buf);
		panelitem_set(gdfattr_panel, gdfattr_fontsize, LXPTEXT_VALUE, buf, LXPI_NULL);

		(void) gconvert((double) gdf_fontlead, 10, 0, buf);
		panelitem_set(gdfattr_panel, gdfattr_fontlead, LXPTEXT_VALUE, buf, LXPI_NULL);
	}

	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	panelitem_set(gdfattr_panel, gdfattr_linewidth, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(gdfattr_panel, gdfattr_fg, LXPENUM_VALUE, gdf_fg, LXPI_NULL);
	switch (gdf_fg) {
	case GDF_BLACKFG:
	case GDF_WHITEFG:
		state= LXPI_INACTIVE;
		break;
	case GDF_OTHERFG:
		state= LXPI_ACTIVE;
		break;
	}
	(void) sprintf(buf, "%1d", (int) gdf_rfg);
	panelitem_set(gdfattr_panel, gdfattr_rfg, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gfg);
	panelitem_set(gdfattr_panel, gdfattr_gfg, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bfg);
	panelitem_set(gdfattr_panel, gdfattr_bfg, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	panelitem_set(gdfattr_panel, gdfattr_bg, LXPENUM_VALUE, gdf_bg, LXPI_NULL);
	switch (gdf_bg) {
	case GDF_WHITEBG:
	case GDF_BLACKBG:
		state= LXPI_INACTIVE;
		break;
	case GDF_OTHERBG:
		state= LXPI_ACTIVE;
		break;
	}
	(void) sprintf(buf, "%1d", (int) gdf_rbg);
	panelitem_set(gdfattr_panel, gdfattr_rbg, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gbg);
	panelitem_set(gdfattr_panel, gdfattr_gbg, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bbg);
	panelitem_set(gdfattr_panel, gdfattr_bbg, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	panelitem_set(gdfattr_panel, gdfattr_mrktype, LXPENUM_VALUE, gdf_mrktype, LXPI_NULL);
	(void) gconvert((double) gdf_mrksize, 10, 0, buf);
	panelitem_set(gdfattr_panel, gdfattr_mrksize, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) gconvert((double) gdf_bndwidth, 10, 0, buf);
	panelitem_set(gdfattr_panel, gdfattr_bndwidth, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(gdfattr_panel, gdfattr_bnd, LXPENUM_VALUE, gdf_bnd, LXPI_NULL);
	switch (gdf_bnd) {
	case GDF_BLACKBND:
	case GDF_WHITEBND:
		state= LXPI_INACTIVE;
		break;
	case GDF_OTHERBND:
		state= LXPI_ACTIVE;
		break;
	}
	(void) sprintf(buf, "%1d", (int) gdf_rbnd);
	panelitem_set(gdfattr_panel, gdfattr_rbnd, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gbnd);
	panelitem_set(gdfattr_panel, gdfattr_gbnd, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bbnd);
	panelitem_set(gdfattr_panel, gdfattr_bbnd, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	panelitem_set(gdfattr_panel, gdfattr_fill, LXPENUM_VALUE, gdf_fill, LXPI_NULL);
	switch (gdf_fill) {
	case GDF_WHITEFILL:
	case GDF_BLACKFILL:
		state= LXPI_INACTIVE;
		break;
	case GDF_OTHERFILL:
		state= LXPI_ACTIVE;
		break;
	}
	(void) sprintf(buf, "%1d", (int) gdf_rfill);
	panelitem_set(gdfattr_panel, gdfattr_rfill, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gfill);
	panelitem_set(gdfattr_panel, gdfattr_gfill, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bfill);
	panelitem_set(gdfattr_panel, gdfattr_bfill, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	panelitem_set(gdfattr_panel, gdfattr_dtk, LXPENUM_VALUE, gdf_dtk, LXPI_NULL);
	switch (gdf_dtk) {
	case GDF_WHITEDTK:
	case GDF_BLACKDTK:
		state= LXPI_INACTIVE;
		break;
	case GDF_OTHERDTK:
		state= LXPI_ACTIVE;
		break;
	}
	(void) sprintf(buf, "%1d", (int) gdf_rdtk);
	panelitem_set(gdfattr_panel, gdfattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gdtk);
	panelitem_set(gdfattr_panel, gdfattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bdtk);
	panelitem_set(gdfattr_panel, gdfattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	ice_op= GDF_ATTR;
	XMapRaised(dpy, gdfattr_frame);
	return;
}

void
gdfattrfontfamily_proc(Panel *p, Panel_item *pi)
{
	int f;

	f= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	if (gdf_psff != (Panel_item *) NULL)
		panelitem_set(p, gdf_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	gdf_psff= psfontfamilies[f].psff_gdfpi;
	panelitem_set(p, gdf_psff, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	return;
}

void
gdfattrfg_proc(Panel *p, Panel_item *pi)
{
	int fg;

	fg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fg) {
	case GDF_BLACKFG:
	case GDF_WHITEFG:
		panelitem_set(p, gdfattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GDF_OTHERFG:
		panelitem_set(p, gdfattr_rfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
gdfattrbg_proc(Panel *p, Panel_item *pi)
{
	int bg;

	bg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bg) {
	case GDF_WHITEBG:
	case GDF_BLACKBG:
		panelitem_set(p, gdfattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GDF_OTHERBG:
		panelitem_set(p, gdfattr_rbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
gdfattrbnd_proc(Panel *p, Panel_item *pi)
{
	int bnd;

	bnd= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bnd) {
	case GDF_BLACKBND:
	case GDF_WHITEBND:
		panelitem_set(p, gdfattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GDF_OTHERBND:
		panelitem_set(p, gdfattr_rbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bbnd, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
gdfattrfill_proc(Panel *p, Panel_item *pi)
{
	int fill;

	fill= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fill) {
	case GDF_WHITEFILL:
	case GDF_BLACKFILL:
		panelitem_set(p, gdfattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GDF_OTHERFILL:
		panelitem_set(p, gdfattr_rfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bfill, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
gdfattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GDF_WHITEDTK:
	case GDF_BLACKDTK:
		panelitem_set(p, gdfattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GDF_OTHERDTK:
		panelitem_set(p, gdfattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, gdfattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
gdfattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf, *fnm;
	float fs, fl, lw, ms, bw;
	int ff, fn;
	int fg, bg, mt, bnd, fill, dtk;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	unsigned char rdtk, gdtk, bdtk;
	boolean redraw;
	Grobj *gr;

	XUnmapWindow(dpy, gdfattr_frame);
	ice_op= MAIN_MENU;

	redraw= FALSE;

	if (npsfonts > 0) {
		ff= *((int *) panelitem_get(gdfattr_panel, gdfattr_fontfamily, LXPENUM_VALUE));
		fn= *((int *) panelitem_get(gdfattr_panel, gdf_psff, LXPENUM_VALUE));
		fnm= psfontfamilies[ff].psff_fonts[fn];

		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_fontsize, LXPTEXT_VALUE);
		fs= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid font size value.", NONFATAL);
			return;
		}
		if (fs <= 0.) {
			ice_err("Font size must be greater than 0.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_fontlead, LXPTEXT_VALUE);
		fl= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid font lead value.", NONFATAL);
			return;
		}
	}

	buf= (char *) panelitem_get(gdfattr_panel, gdfattr_linewidth, LXPTEXT_VALUE);
	lw= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid line width value.", NONFATAL);
		return;
	}
	if (lw < 0.) {
		ice_err("Line width may not be less than 0.", NONFATAL);
		return;
	}

	fg= *((int *) panelitem_get(gdfattr_panel, gdfattr_fg, LXPENUM_VALUE));
	switch (fg) {
	case GDF_BLACKFG:
		rfg= bfg= gfg= (unsigned char) 0;
		break;
	case GDF_WHITEFG:
		rfg= bfg= gfg= (unsigned char) 255;
		break;
	case GDF_OTHERFG:
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_rfg, LXPTEXT_VALUE);
		rfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red background value.", NONFATAL);
			return;
		}
		if ((rfg < 0) || (rfg > 255)) {
			ice_err("Invalid red background value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_gfg, LXPTEXT_VALUE);
		gfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green background value.", NONFATAL);
			return;
		}
		if ((gfg < 0) || (gfg > 255)) {
			ice_err("Invalid green background value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_bfg, LXPTEXT_VALUE);
		bfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue background value.", NONFATAL);
			return;
		}
		if ((bfg < 0) || (bfg > 255)) {
			ice_err("Invalid blue background value.", NONFATAL);
			return;
		}
		break;
	}

	bg= *((int *) panelitem_get(gdfattr_panel, gdfattr_bg, LXPENUM_VALUE));
	switch (bg) {
	case GDF_WHITEBG:
		rbg= bbg= gbg= (unsigned char) 255;
		break;
	case GDF_BLACKBG:
		rbg= bbg= gbg= (unsigned char) 0;
		break;
	case GDF_OTHERBG:
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_rbg, LXPTEXT_VALUE);
		rbg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red background value.", NONFATAL);
			return;
		}
		if ((rbg < 0) || (rbg > 255)) {
			ice_err("Invalid red background value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_gbg, LXPTEXT_VALUE);
		gbg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green background value.", NONFATAL);
			return;
		}
		if ((gbg < 0) || (gbg > 255)) {
			ice_err("Invalid green background value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_bbg, LXPTEXT_VALUE);
		bbg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue background value.", NONFATAL);
			return;
		}
		if ((bbg < 0) || (bbg > 255)) {
			ice_err("Invalid blue background value.", NONFATAL);
			return;
		}
		break;
	}

	mt= *((int *) panelitem_get(gdfattr_panel, gdfattr_mrktype, LXPENUM_VALUE));

	buf= (char *) panelitem_get(gdfattr_panel, gdfattr_mrksize, LXPTEXT_VALUE);
	ms= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid marker size value.", NONFATAL);
		return;
	}
	if (ms <= 0.) {
		ice_err("Marker size must be greater than 0.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(gdfattr_panel, gdfattr_bndwidth, LXPTEXT_VALUE);
	bw= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid boundary width value.", NONFATAL);
		return;
	}
	if (bw < 0.) {
		ice_err("Boundary width may not be less than 0.", NONFATAL);
		return;
	}

	bnd= *((int *) panelitem_get(gdfattr_panel, gdfattr_bnd, LXPENUM_VALUE));
	switch (bnd) {
	case GDF_BLACKBND:
		rbnd= bbnd= gbnd= (unsigned char) 0;
		break;
	case GDF_WHITEBND:
		rbnd= bbnd= gbnd= (unsigned char) 255;
		break;
	case GDF_OTHERBND:
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_rbnd, LXPTEXT_VALUE);
		rbnd= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red boundary color value.", NONFATAL);
			return;
		}
		if ((rbnd < 0) || (rbnd > 255)) {
			ice_err("Invalid red boundary color value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_gbnd, LXPTEXT_VALUE);
		gbnd= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green boundary color value.", NONFATAL);
			return;
		}
		if ((gbnd < 0) || (gbnd > 255)) {
			ice_err("Invalid green boundary color value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_bbnd, LXPTEXT_VALUE);
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

	fill= *((int *) panelitem_get(gdfattr_panel, gdfattr_fill, LXPENUM_VALUE));
	switch (fill) {
	case GDF_WHITEFILL:
		rfill= bfill= gfill= (unsigned char) 255;
		break;
	case GDF_BLACKFILL:
		rfill= bfill= gfill= (unsigned char) 0;
		break;
	case GDF_OTHERFILL:
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_rfill, LXPTEXT_VALUE);
		rfill= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red fill color value.", NONFATAL);
			return;
		}
		if ((rfill < 0) || (rfill > 255)) {
			ice_err("Invalid red fill color value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_gfill, LXPTEXT_VALUE);
		gfill= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green fill color value.", NONFATAL);
			return;
		}
		if ((gfill < 0) || (gfill > 255)) {
			ice_err("Invalid green fill color value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_bfill, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(gdfattr_panel, gdfattr_dtk, LXPENUM_VALUE));
	switch (dtk) {
	case GDF_WHITEDTK:
		rdtk= bdtk= gdtk= (unsigned char) 255;
		break;
	case GDF_BLACKDTK:
		rdtk= bdtk= gdtk= (unsigned char) 0;
		break;
	case GDF_OTHERDTK:
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(gdfattr_panel, gdfattr_bdtk, LXPTEXT_VALUE);
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

	/* update all Grobjs as necessary */
	for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
		Raster *ras;
		Text *text;
		Vector *vec;
		Curve *crv;
		Marker *mrk;
		Rectangle *rect;
		Polygon *poly;
		Axis *axis;
		boolean rebuild_ras;
		int w, h, d;
		int ofg, obgm, obg, obndm, obndclr, ofillm, ofillclr, odtk;
		int omrkt, omrktype;
		unsigned char orfg, ogfg, obfg;
		unsigned char orbg, ogbg, obbg;
		unsigned char orbnd, ogbnd, obbnd;
		unsigned char orfill, ogfill, obfill;
		unsigned char ordtk, ogdtk, obdtk;
		unsigned long fgp, bgp, dtkp;
		unsigned long ofgp, obgp;
		int font, fontsz, tl;
		char *fntnm;
		float size, lead, orad, obw, pt, st, tt;
		int olinewd, osz, obndwd;

		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			if ((rdtk == gdf_rdtk) &&
			    (gdtk == gdf_gdtk) &&
			    (bdtk == gdf_bdtk))
				break;
			gr->getdtk(&odtk, &ordtk, &ogdtk, &obdtk);
			if (odtk != GROBJ_GLOBALDTK)
				break;
			if ((rdtk == ordtk) &&
			    (gdtk == ogdtk) &&
			    (bdtk == obdtk))
				break;
			(void) gr->setdtk(odtk, rdtk, gdtk, bdtk);
			if (pg_pixdepth == 1)
				dtkp= cmap_lookup(rdtk, gdtk, bdtk, 2);
			else
				dtkp= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
			gr->setdtkpix(dtkp);
			break;
		case GROBJ_RASTER:
			if ((rfg == gdf_rfg) &&
			    (gfg == gdf_gfg) &&
			    (bfg == gdf_bfg) &&
			    (rbg == gdf_rbg) &&
			    (gbg == gdf_gbg) &&
			    (bbg == gdf_bbg))
				break;
			ras= (Raster *) gr;
			ras->getrassize(&w, &h, &d);
			if (d != 1)
				break;
			ras->getfg(&ofg, &ofgp, &orfg, &ogfg, &obfg);
			ras->getbg(&obg, &obgp, &orbg, &ogbg, &obbg);
			if ((ofg != RASTER_GLOBALFG) && (obg != RASTER_GLOBALBG))
				break;
			rebuild_ras= FALSE;
			if ((ofg == RASTER_GLOBALFG) &&
			    ((orfg != rfg) || (ogfg != gfg) || (obfg != bfg))) {
				if (pg_pixdepth == 8)
					fgp= cmap_lookup(rfg, gfg, bfg, PSEUDOCOLOR_MAPSZ);
				else if (pg_pixdepth == 1)
					fgp= cmap_lookup(rfg, gfg, bfg, 2);
				ras->setfg(ofg, fgp, rfg, gfg, bfg);
				if (ras->getdrawmode() == RASTER_FULL)
					rebuild_ras= TRUE;
			}
			else
				fgp= ofgp;
			if ((obg == RASTER_GLOBALBG) &&
			    ((orbg != rbg) || (ogbg != gbg) || (obbg != bbg))) {
				if (pg_pixdepth == 8)
					bgp= cmap_lookup(rbg, gbg, bbg, PSEUDOCOLOR_MAPSZ);
				else if (pg_pixdepth == 1)
					bgp= cmap_lookup(rbg, gbg, bbg, 2);
				ras->setbg(obg, bgp, rbg, gbg, bbg);
				if (ras->getdrawmode() == RASTER_FULL)
					rebuild_ras= TRUE;
			}
			else
				bgp= obgp;
			if (fgp == bgp) {
				fgp= black_pixel;
				bgp= white_pixel;
				if (ofg == RASTER_GLOBALFG)
					ras->setfg(ofg, fgp, rfg, gfg, bfg);
				else
					ras->setfg(ofg, fgp, orfg, ogfg, obfg);
				if (obg == RASTER_GLOBALBG)
					ras->setbg(obg, bgp, rbg, gbg, bbg);
				else
					ras->setbg(obg, bgp, orbg, ogbg, obbg);
				if (ras->getdrawmode() == RASTER_FULL)
					rebuild_ras= TRUE;
			}
			if (rebuild_ras) {
				ras->bldimage();
				redraw= TRUE;
			}
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {

			case INTOBJ_TEXT:
				text= (Text *) gr;
				text->getfont(&font, &fntnm);
				if ((font == TEXT_GLOBALFONT) &&
				    (fnm != gdf_fontname)) {
					(void) text->setfont(font, fnm);
					redraw= TRUE;
				}
				text->getfontsize(&fontsz, &size, &lead);
				if ((fontsz == TEXT_GLOBALFONTSZ) &&
				    ((fs != gdf_fontsize) || (fl != gdf_fontlead))) {
					(void) text->setfontsize(fontsz, fs, fl);
					redraw= TRUE;
				}
				text->getforeground(&ofg, &orfg, &ogfg, &obfg);
				if ((ofg == TEXT_GLOBALFG) &&
				    ((rfg != gdf_rfg) || (gfg != gdf_gfg) || (bfg != gdf_bfg))) {
					(void) text->setforeground(ofg, rfg, gfg, bfg);
					redraw= TRUE;
				}
				text->getbackground(&obgm, &obg, &orbg, &ogbg, &obbg);
				if ((obg == TEXT_GLOBALBG) &&
				    ((rbg != gdf_rbg) || (gbg != gdf_gbg) || (bbg != gdf_bbg))) {
					(void) text->setbackground(obgm, obg, rbg, gbg, bbg);
					if (obgm == TEXT_OPAQUEBGM)
						redraw= TRUE;
				}
				break;
			case INTOBJ_VECTOR:
				vec= (Vector *) gr;
				vec->getwidth(&olinewd, &size);
				if ((olinewd == VECTOR_GLOBALWIDTH) &&
				    (lw != gdf_linewidth)) {
					(void) vec->setwidth(olinewd, lw);
					redraw= TRUE;
				}
				vec->getforeground(&ofg, &orfg, &ogfg, &obfg);
				if ((ofg == VECTOR_GLOBALFG) &&
				    ((rfg != gdf_rfg) || (gfg != gdf_gfg) || (bfg != gdf_bfg))) {
					(void) vec->setforeground(ofg, rfg, gfg, bfg);
					redraw= TRUE;
				}
				break;
			case INTOBJ_CURVE:
				crv= (Curve *) gr;
				crv->getwidth(&olinewd, &size);
				if ((olinewd == CURVE_GLOBALWIDTH) &&
				    (lw != gdf_linewidth)) {
					(void) crv->setwidth(olinewd, lw);
					redraw= TRUE;
				}
				crv->getforeground(&ofg, &orfg, &ogfg, &obfg);
				if ((ofg == CURVE_GLOBALFG) &&
				    ((rfg != gdf_rfg) || (gfg != gdf_gfg) || (bfg != gdf_bfg))) {
					(void) crv->setforeground(ofg, rfg, gfg, bfg);
					redraw= TRUE;
				}
				break;
			case INTOBJ_MARKER:
				mrk= (Marker *) gr;
				mrk->getmarkertype(&omrkt, &omrktype);
				if ((omrkt == MARKER_GLOBALTYPE) &&
				    (mt != gdf_mrktype)) {
					(void) mrk->setmarkertype(omrkt, mt+1);
					redraw= TRUE;
				}
				mrk->getsize(&osz, &orad);
				if ((osz == MARKER_GLOBALSIZE) &&
				    (ms != gdf_mrksize)) {
					(void) mrk->setsize(osz, ms);
					redraw= TRUE;
				}
				mrk->getboundary(&obndm, &obndwd, &obw, &obndclr, &orbnd, &ogbnd, &obbnd);
				if ((obndwd == MARKER_GLOBALBNDWIDTH) &&
				    (bw != gdf_bndwidth)) {
					obw= bw;
					(void) mrk->setboundary(obndm, obndwd, obw, obndclr, orbnd, ogbnd, obbnd);
					redraw= TRUE;
				}
				if ((obndclr == MARKER_GLOBALBND) &&
				    ((rbnd != gdf_rbnd) || (gbnd != gdf_gbnd) || (bbnd != gdf_bbnd))) {
					(void) mrk->setboundary(obndm, obndwd, obw, obndclr, rbnd, gbnd, bbnd);
					redraw= TRUE;
				}
				mrk->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
				if ((ofillclr == MARKER_GLOBALFILL) &&
				    ((rfill != gdf_rfill) || (gfill != gdf_gfill) || (bfill != gdf_bfill))) {
					(void) mrk->setfill(ofillm, ofillclr, rfill, gfill, bfill);
					redraw= TRUE;
				}
				break;
			case INTOBJ_RECTANGLE:
				rect= (Rectangle *) gr;
				rect->getwidth(&obndwd, &obw);
				if ((obndwd == RECTANGLE_GLOBALBNDWIDTH) &&
				    (bw != gdf_bndwidth)) {
					obw= bw;
					(void) rect->setwidth(obndwd, obw);
					redraw= TRUE;
				}
				rect->getboundary(&obndm, &obndclr, &orbnd, &ogbnd, &obbnd);
				if ((obndclr == RECTANGLE_GLOBALBND) &&
				    ((rbnd != gdf_rbnd) || (gbnd != gdf_gbnd) || (bbnd != gdf_bbnd))) {
					(void) rect->setboundary(obndm, obndclr, rbnd, gbnd, bbnd);
					redraw= TRUE;
				}
				rect->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
				if ((ofillclr == RECTANGLE_GLOBALFILL) &&
				    ((rfill != gdf_rfill) || (gfill != gdf_gfill) || (bfill != gdf_bfill))) {
					(void) rect->setfill(ofillm, ofillclr, rfill, gfill, bfill);
					redraw= TRUE;
				}
				break;
			case INTOBJ_POLYGON:
				poly= (Polygon *) gr;
				poly->getboundary(&obndm, &obndwd, &obw, &obndclr, &orbnd, &ogbnd, &obbnd);
				if ((obndwd == POLYGON_GLOBALBNDWIDTH) &&
				    (bw != gdf_bndwidth)) {
					obw= bw;
					(void) poly->setboundary(obndm, obndwd, obw, obndclr, orbnd, ogbnd, obbnd);
					redraw= TRUE;
				}
				if ((obndclr == POLYGON_GLOBALBND) &&
				    ((rbnd != gdf_rbnd) || (gbnd != gdf_gbnd) || (bbnd != gdf_bbnd))) {
					(void) poly->setboundary(obndm, obndwd, obw, obndclr, rbnd, gbnd, bbnd);
					redraw= TRUE;
				}
				poly->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
				if ((ofillclr == POLYGON_GLOBALFILL) &&
				    ((rfill != gdf_rfill) || (gfill != gdf_gfill) || (bfill != gdf_bfill))) {
					(void) poly->setfill(ofillm, ofillclr, rfill, gfill, bfill);
					redraw= TRUE;
				}
				break;
			case INTOBJ_AXIS:
				axis= (Axis *) gr;
				axis->getaxiswidth(&olinewd, &size);
				if ((olinewd == AXIS_GLOBALWIDTH) &&
				    (lw != gdf_linewidth)) {
					(void) axis->setaxiswidth(olinewd, lw);
					redraw= TRUE;
				}
				axis->gettick(&tl, &pt, &st, &tt, &olinewd, &size);
				if ((olinewd == AXIS_GLOBALWIDTH) &&
				    (lw != gdf_linewidth)) {
					(void) axis->settick(tl, pt, st, tt, olinewd, lw);
					redraw= TRUE;
				}
				axis->getline(&ofg, &orfg, &ogfg, &obfg);
				if ((ofg == AXIS_GLOBALLINE) &&
				    ((rfg != gdf_rfg) || (gfg != gdf_gfg) || (bfg != gdf_bfg))) {
					(void) axis->setline(ofg, rfg, gfg, bfg);
					redraw= TRUE;
				}
				axis->getfont(&font, &fntnm);
				if ((font == AXIS_GLOBALFONT) &&
				    (fnm != gdf_fontname)) {
					(void) axis->setfont(font, fnm);
					redraw= TRUE;
				}
				axis->getfontsize(&fontsz, &size);
				if ((fontsz == AXIS_GLOBALFONTSZ) &&
				    (fs != gdf_fontsize)) {
					(void) axis->setfontsize(fontsz, fs);
					redraw= TRUE;
				}
				axis->getlabel(&ofg, &orfg, &ogfg, &obfg);
				if ((ofg == AXIS_GLOBALLABEL) &&
				    ((rfg != gdf_rfg) || (gfg != gdf_gfg) || (bfg != gdf_bfg))) {
					(void) axis->setlabel(ofg, rfg, gfg, bfg);
					redraw= TRUE;
				}
				break;
			}
			if ((rdtk != gdf_rdtk) ||
			    (gdtk != gdf_gdtk) ||
			    (bdtk != gdf_bdtk)) {
				gr->getdtk(&odtk, &ordtk, &ogdtk, &obdtk);
				if ((odtk == GROBJ_GLOBALDTK) &&
				    ((rdtk != ordtk) || (gdtk != ogdtk) || (bdtk != obdtk))) {
					(void) gr->setdtk(odtk, rdtk, gdtk, bdtk);
					if (pg_pixdepth == 1)
						dtkp= cmap_lookup(rdtk, gdtk, bdtk, 2);
					else
						dtkp= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
					gr->setdtkpix(dtkp);
				}
			}
			break;
		}
	}

	if (pg_update == PG_MANUAL)
		redraw= FALSE;

	if (npsfonts > 0) {
		gdf_fontname= fnm;
		gdf_fontsize= fs;
		gdf_fontlead= fl;
	}
	gdf_linewidth= lw;
	gdf_fg= fg;
	gdf_rfg= rfg;
	gdf_gfg= gfg;
	gdf_bfg= bfg;
	gdf_bg= bg;
	gdf_rbg= rbg;
	gdf_gbg= gbg;
	gdf_bbg= bbg;
	gdf_mrktype= mt;
	gdf_mrksize= ms;
	gdf_bndwidth= bw;
	gdf_bnd= bnd;
	gdf_rbnd= rbnd;
	gdf_gbnd= gbnd;
	gdf_bbnd= bbnd;
	gdf_fill= fill;
	gdf_rfill= rfill;
	gdf_gfill= gfill;
	gdf_bfill= bfill;
	gdf_dtk= dtk;
	gdf_rdtk= rdtk;
	gdf_gdtk= gdtk;
	gdf_bdtk= bdtk;


	if (!redraw)
		return;

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);
	pg_draw();
	XSync(dpy, False);
	XDefineCursor(dpy, pg_cwin, std_cursor);

	return;
}

void
gdfattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, gdfattr_frame);
	ice_op= MAIN_MENU;
	return;
}
