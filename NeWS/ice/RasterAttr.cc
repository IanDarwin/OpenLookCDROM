/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Raster.h"

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		rasinscont_proc(Panel *, Panel_item *);

static Raster *attr_ras;

void
attrras_proc(Menu *m, Menu_item *mi)
{
	char buf[20];
	int w, h, d;
	int state, fg, bg;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	unsigned long fgp, bgp;

	if ((attr_ras= (Raster *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Raster *) NULL) {
		ice_err("Cannot locate selected raster.", NONFATAL);
		return;
	}

	panelitem_set(rasattr_panel, rasattr_fname, LXPTEXT_VALUE, attr_ras->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_ras->getpixrep());
	panelitem_set(rasattr_panel, rasattr_pixrep, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(rasattr_panel, rasattr_orig, LXPENUM_VALUE, attr_ras->getorigloc(), LXPI_NULL);

	panelitem_set(rasattr_panel, rasattr_draw, LXPENUM_VALUE, attr_ras->getdrawmode(), LXPI_NULL);

	attr_ras->getrassize(&w, &h, &d);
	if (d == 1) {
		attr_ras->getfg(&fg, &fgp, &rfg, &gfg, &bfg);
		panelitem_set(rasattr_panel, rasattr_fg, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, fg, LXPI_NULL);
		switch (fg) {
		case RASTER_GLOBALFG:
		case RASTER_BLACKFG:
		case RASTER_WHITEFG:
			state= LXPI_INACTIVE;
			break;
		case RASTER_OTHERFG:
			state= LXPI_ACTIVE;
			break;
		}
		(void) sprintf(buf, "%1d", (int) rfg);
		panelitem_set(rasattr_panel, rasattr_rfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) gfg);
		panelitem_set(rasattr_panel, rasattr_gfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) bfg);
		panelitem_set(rasattr_panel, rasattr_bfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		attr_ras->getbg(&bg, &bgp, &rbg, &gbg, &bbg);
		panelitem_set(rasattr_panel, rasattr_bg, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, bg, LXPI_NULL);
		switch (bg) {
		case RASTER_GLOBALBG:
		case RASTER_BLACKBG:
		case RASTER_WHITEBG:
			state= LXPI_INACTIVE;
			break;
		case RASTER_OTHERBG:
			state= LXPI_ACTIVE;
			break;
		}
		(void) sprintf(buf, "%1d", (int) rbg);
		panelitem_set(rasattr_panel, rasattr_rbg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) gbg);
		panelitem_set(rasattr_panel, rasattr_gbg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) bbg);
		panelitem_set(rasattr_panel, rasattr_bbg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	}
	else {
		panelitem_set(rasattr_panel, rasattr_fg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(rasattr_panel, rasattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(rasattr_panel, rasattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(rasattr_panel, rasattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(rasattr_panel, rasattr_bg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(rasattr_panel, rasattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(rasattr_panel, rasattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(rasattr_panel, rasattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	}

	(void) sprintf(buf, "%1d", attr_ras->getsequence());
	panelitem_set(rasattr_panel, rasattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	ice_op= RAS_ATTR;
	XMapRaised(dpy, rasattr_frame);
	return;
}

void
rasattrfg_proc(Panel *p, Panel_item *pi)
{
	int fg;

	fg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fg) {
	case RASTER_GLOBALFG:
	case RASTER_BLACKFG:
	case RASTER_WHITEFG:
		panelitem_set(p, rasattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RASTER_OTHERFG:
		panelitem_set(p, rasattr_rfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_gfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_bfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rasattrbg_proc(Panel *p, Panel_item *pi)
{
	int bg;

	bg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bg) {
	case RASTER_GLOBALFG:
	case RASTER_BLACKFG:
	case RASTER_WHITEFG:
		panelitem_set(p, rasattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case RASTER_OTHERFG:
		panelitem_set(p, rasattr_rbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_gbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, rasattr_bbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
rasattrcont_proc(Panel *p, Panel_item *pi)
{
	char *buf;
	int rep, seq, orig, draw;
	int w, h, d;
	int fg, bg;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	int ofg, obg;
	unsigned char orfg, ogfg, obfg;
	unsigned char orbg, ogbg, obbg;
	unsigned long fgp, bgp;
	unsigned long ofgp, obgp;
	char *cptr, errmsg[MAX_ERRMSGLEN+1];
	Menu_item *item;
	boolean rebuild, redraw;

	if (ice_op == RAS_INSERTATTR) {
		rasinscont_proc(p, pi);
		return;
	}

	XUnmapWindow(dpy, rasattr_frame);
	ice_op= MAIN_MENU;

	rebuild= redraw= FALSE;

	buf= (char *) panelitem_get(rasattr_panel, rasattr_pixrep, LXPTEXT_VALUE);
	rep= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid replication value.", NONFATAL);
		return;
	}
	if (rep <= 0) {
		ice_err("Replication value must be greater than 0.", NONFATAL);
		return;
	}

	orig= *((int *) panelitem_get(rasattr_panel, rasattr_orig, LXPENUM_VALUE));

	draw= *((int *) panelitem_get(rasattr_panel, rasattr_draw, LXPENUM_VALUE));

	attr_ras->getrassize(&w, &h, &d);
	if (d == 1) {
		fg= *((int *) panelitem_get(rasattr_panel, rasattr_fg, LXPENUM_VALUE));
		switch (fg) {
		case RASTER_GLOBALFG:
			rfg= gdf_rfg;
			gfg= gdf_gfg;
			bfg= gdf_bfg;
			break;
		case RASTER_BLACKFG:
			rfg= bfg= gfg= (unsigned char) 0;
			break;
		case RASTER_WHITEFG:
			rfg= bfg= gfg= (unsigned char) 255;
			break;
		case RASTER_OTHERFG:
			buf= (char *) panelitem_get(rasattr_panel, rasattr_rfg, LXPTEXT_VALUE);
			rfg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red foreground value.", NONFATAL);
				return;
			}
			if ((rfg < 0) || (rfg > 255)) {
				ice_err("Invalid red foreground value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rasattr_panel, rasattr_gfg, LXPTEXT_VALUE);
			gfg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green foreground value.", NONFATAL);
				return;
			}
			if ((gfg < 0) || (gfg > 255)) {
				ice_err("Invalid green foreground value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rasattr_panel, rasattr_bfg, LXPTEXT_VALUE);
			bfg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid blue foreground value.", NONFATAL);
				return;
			}
			if ((bfg < 0) || (bfg > 255)) {
				ice_err("Invalid blue foreground value.", NONFATAL);
				return;
			}
			break;
		}

		bg= *((int *) panelitem_get(rasattr_panel, rasattr_bg, LXPENUM_VALUE));
		switch (bg) {
		case RASTER_GLOBALFG:
			rbg= gdf_rbg;
			gbg= gdf_gbg;
			bbg= gdf_bbg;
			break;
		case RASTER_BLACKFG:
			rbg= bbg= gbg= (unsigned char) 0;
			break;
		case RASTER_WHITEFG:
			rbg= bbg= gbg= (unsigned char) 255;
			break;
		case RASTER_OTHERFG:
			buf= (char *) panelitem_get(rasattr_panel, rasattr_rbg, LXPTEXT_VALUE);
			rbg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red background value.", NONFATAL);
				return;
			}
			if ((rbg < 0) || (rbg > 255)) {
				ice_err("Invalid red background value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rasattr_panel, rasattr_gbg, LXPTEXT_VALUE);
			gbg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green background value.", NONFATAL);
				return;
			}
			if ((gbg < 0) || (gbg > 255)) {
				ice_err("Invalid green background value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rasattr_panel, rasattr_bbg, LXPTEXT_VALUE);
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
	}

	buf= (char *) panelitem_get(rasattr_panel, rasattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	if (rep != attr_ras->getpixrep()) {
		rebuild= TRUE;
		(void) attr_ras->setpixrep(rep);
	}
	if (orig != attr_ras->getorigloc()) {
		rebuild= TRUE;
		(void) attr_ras->setorigloc(orig);
	}
	if (draw != attr_ras->getdrawmode()) {
		item= menuitem_find(cmap_menu, LXMI_CLIENTDATA, (char *) attr_ras);
		if (draw == RASTER_FULL) {
			rebuild= TRUE;
			if (item != (Menu_item *) NULL)
				(void) menuitem_set(item, LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		}
		else {
			rebuild= FALSE;
			redraw= TRUE;
			if (item != (Menu_item *) NULL)
				(void) menuitem_set(item, LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		}
		(void) attr_ras->setdrawmode(draw);
	}
	if (seq != attr_ras->getsequence()) {
		redraw= TRUE;
		attr_ras->setsequence(seq);
		attr_ras->sortsequence(&grobjs);
	}
	if (d == 1) {
		attr_ras->getfg(&ofg, &ofgp, &orfg, &ogfg, &obfg);
		if ((orfg != rfg) || (ogfg != gfg) || (obfg != bfg)) {
			if (pg_pixdepth == 8)
				fgp= cmap_lookup(rfg, gfg, bfg, PSEUDOCOLOR_MAPSZ);
			else if (pg_pixdepth == 1)
				fgp= cmap_lookup(rfg, gfg, bfg, 2);
			attr_ras->setfg(fg, fgp, rfg, gfg, bfg);
			if (draw == RASTER_FULL)
				rebuild= TRUE;
		}
		else
			fgp= ofgp;
		attr_ras->getbg(&obg, &obgp, &orbg, &ogbg, &obbg);
		if ((orbg != rbg) || (ogbg != gbg) || (obbg != bbg)) {
			if (pg_pixdepth == 8)
				bgp= cmap_lookup(rbg, gbg, bbg, PSEUDOCOLOR_MAPSZ);
			else if (pg_pixdepth == 1)
				bgp= cmap_lookup(rbg, gbg, bbg, 2);
			attr_ras->setbg(bg, bgp, rbg, gbg, bbg);
			if (draw == RASTER_FULL)
				rebuild= TRUE;
		}
		else
			bgp= obgp;
		if (fgp == bgp) {
			fgp= black_pixel;
			bgp= white_pixel;
			attr_ras->setfg(fg, fgp, rfg, gfg, bfg);
			attr_ras->setbg(bg, bgp, rbg, gbg, bbg);
		}
	}

	if (rebuild || redraw) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	if (rebuild) {
		attr_ras->bldimage();
		redraw= TRUE;
	}
	if (redraw) {
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
	}
	return;
}

void
rasattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, rasattr_frame);
	ice_op= MAIN_MENU;
	return;
}
