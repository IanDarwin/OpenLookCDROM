/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <rasterfile.h>
#include <pixrect/pixrect_hs.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Raster.h"

extern "C" {
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrras_proc(Menu *, Menu_item *);
extern void		cmap_copy(Colormap, Colormap);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmapras_proc(Menu *, Menu_item *);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cpras_proc(Menu *, Menu_item *);
extern void		delras_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern FILE *		doc_open(char *, char *);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		ras_del(Raster *);
extern void		ras_drawoutline(Raster *);
extern void		trras_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean rasoutline_drawn;
extern ras_x, ras_y;

static Raster *new_ras;

void
insras_proc(Menu *m, Menu_item *mi)
{
	char buf[80];

	panelitem_set(rasattr_panel, rasattr_fname, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		ice_op= RAS_INSERTATTR;
		XMapRaised(dpy, rasattr_frame);
		return;
	}

	panelitem_set(rasattr_panel, rasattr_pixrep, LXPTEXT_VALUE, "1", LXPI_NULL);

	panelitem_set(rasattr_panel, rasattr_orig, LXPENUM_VALUE, RASTER_ULORIG, LXPI_NULL);

	panelitem_set(rasattr_panel, rasattr_draw, LXPENUM_VALUE, RASTER_FULL, LXPI_NULL);

	panelitem_set(rasattr_panel, rasattr_fg, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, RASTER_GLOBALFG, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_rfg);
	panelitem_set(rasattr_panel, rasattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gfg);
	panelitem_set(rasattr_panel, rasattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bfg);
	panelitem_set(rasattr_panel, rasattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(rasattr_panel, rasattr_bg, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, RASTER_GLOBALBG, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_rbg);
	panelitem_set(rasattr_panel, rasattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gbg);
	panelitem_set(rasattr_panel, rasattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bbg);
	panelitem_set(rasattr_panel, rasattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(rasattr_panel, rasattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= RAS_INSERTATTR;
	XMapRaised(dpy, rasattr_frame);
	return;
}

void
rasinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *filenm, *buf;
	FILE *fp;
	int rep, seq, orig, draw;
	int w, h, d;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	int fg, bg;
	unsigned long fgp, bgp;
	float fx, fy;
	char xbuf[30], ybuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *attritem;
	Menu_item *tritem, *dmpitem, *cmapitem;
	Menu_item *cpitem, *cmpopitem;

	XUnmapWindow(dpy, rasattr_frame);
	ice_op= MAIN_MENU;

	filenm= (char *) panelitem_get(rasattr_panel, rasattr_fname, LXPTEXT_VALUE);
	if (strlen(filenm) == 0) {
		ice_err("No filename specified.", NONFATAL);
		return;
	}
	if ((fp= doc_open(filenm, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		return;
	}
	(void) fclose(fp);

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
	case RASTER_GLOBALBG:
		rbg= gdf_rbg;
		gbg= gdf_gbg;
		bbg= gdf_bbg;
		break;
	case RASTER_WHITEBG:
		rbg= bbg= gbg= (unsigned char) 255;
		break;
	case RASTER_BLACKBG:
		rbg= bbg= gbg= (unsigned char) 0;
		break;
	case RASTER_OTHERBG:
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

	buf= (char *) panelitem_get(rasattr_panel, rasattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	if ((new_ras= new Raster((Dlnk **) &grobjs, filenm, dpy, visual, fg_pixel, pg_pixdepth, seq)) == (Raster *) NULL) {
		ice_err("Cannot create raster.", NONFATAL);
		return;
	}
	nrasters++;
	if (new_ras->getname() == (char *) NULL) {
		ice_err("Raster load error.", NONFATAL);
		ras_del(new_ras);
		return;
	}
	(void) new_ras->setpixrep(rep);
	(void) new_ras->setorigloc(orig);
	(void) new_ras->setdrawmode(draw);
	new_ras->getrassize(&w, &h, &d);
	if (d == 1) {
		if (pg_pixdepth == 8) {
			fgp= cmap_lookup(rfg, gfg, bfg, PSEUDOCOLOR_MAPSZ);
			bgp= cmap_lookup(rbg, gbg, bbg, PSEUDOCOLOR_MAPSZ);
		}
		else if (pg_pixdepth == 1) {
			fgp= cmap_lookup(rfg, gfg, bfg, 2);
			bgp= cmap_lookup(rbg, gbg, bbg, 2);
		}
		if (fgp == bgp) {
			fgp= black_pixel;
			bgp= white_pixel;
		}
		new_ras->setfg(fg, fgp, rfg, gfg, bfg);
		new_ras->setbg(bg, bgp, rbg, gbg, bbg);
	}

	ice_op= RAS_INSERTLOC;

	if (pg_loc == PG_CURSORLOC) {
		pgcoords_drawn= rasoutline_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	if (draw == RASTER_FULL) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		if (new_ras->bldimage() != GROBJ_SUCCESS) {
			ice_err("Raster load error.", NONFATAL);
			ras_del(new_ras);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			ice_op= MAIN_MENU;
			return;
		}
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
	}

	if ((delitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, delras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, attrras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, trras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, cpras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(new_ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((pg_pixdepth >= 8) && (d >= 8)) {
		if ((cmapitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_ras,
				LXMI_PROC, cmapras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			menuitem_destroy(cpitem);
			menuitem_destroy(dmpitem);
			menuitem_destroy(cmpopitem);
			ice_op= MAIN_MENU;
			return;
		}
		if (new_ras->getcolormap() == None)
			(void) menuitem_set(cmapitem, LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}
	(void) menuitem_insert(delras_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrras_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trras_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpras_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpras_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopras_menu, cmpopitem);
	if ((pg_pixdepth >= 8) && (d >= 8))
		(void) menuitem_insert(cmap_menu, cmapitem);

	loc_grobj= (Grobj *) new_ras;
	panelitem_set(locattr_panel, locattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
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
insiceras_rd(FILE *fp, int gdf, int newobj)
{
	char *filenm;
	FILE *rasfp;
	int rep, seq, orig, draw;
	int w, h, d;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	int fg, bg;
	int len, ir, ig, ib;
	float fx, fy;
	unsigned long fgp, bgp;
	int iot;
	boolean endfound;
	char errmsg[MAX_ERRMSGLEN+1];
	Raster *ras;
	Menu_item *delitem, *attritem;
	Menu_item *tritem, *dmpitem, *cmapitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Ras: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Ras: Begin ")+len-1)= '\0';
	if ((filenm= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(filenm, ice_iobuf+strlen("%%ICE-Ras: Begin "));
	if ((rasfp= doc_open(filenm, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		return;
	}
	(void) fclose(rasfp);

	fx= fy= 0.;
	rep= 1;
	orig= RASTER_ULORIG;
	draw= RASTER_FULL;
	fg= RASTER_GLOBALFG;
	rfg= gdf_rfg;
	gfg= gdf_gfg;
	bfg= gdf_bfg;
	bg= RASTER_GLOBALBG;
	rbg= gdf_rbg;
	gbg= gdf_gbg;
	bbg= gdf_bbg;
	seq= 0;
	iot= GROBJ_NULLIOTAG;

	endfound= FALSE;
	while (fgets(ice_iobuf, INPUT_BUFSZ, fp) != (char *) NULL) {
		insice_lineno++;

		if (strncmp("%%ICE-", ice_iobuf, strlen("%%ICE-")))
			continue;

		if (!strncmp("%%ICE-Ras: Loc ", ice_iobuf, strlen("%%ICE-Ras: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Ras: Loc "), "%f %f", &fx, &fy) != 2) {
				ice_err("Invalid raster location value.", NONFATAL);
				delete filenm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Ras: Flags ", ice_iobuf, strlen("%%ICE-Ras: Flags "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Ras: Flags "), "%d %d %d", &rep, &orig, &draw) != 3) {
				ice_err("Invalid raster flags value.", NONFATAL);
				delete filenm;
				return;
			}
			if (rep < 1) {
				ice_err("Invalid raster pixel replication value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((orig != RASTER_ULORIG) &&
			    (orig != RASTER_LLORIG) &&
			    (orig != RASTER_LRORIG) &&
			    (orig != RASTER_URORIG)) {
				ice_err("Invalid raster orientation value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((draw != RASTER_FULL) &&
			    (draw != RASTER_OUTLINE)) {
				ice_err("Invalid raster draw mode value.", NONFATAL);
				delete filenm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Ras: FG ", ice_iobuf, strlen("%%ICE-Ras: FG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Ras: FG "), "%d %d %d %d", &fg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid raster foreground value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((fg != RASTER_GLOBALFG) &&
			    (fg != RASTER_BLACKFG) &&
			    (fg != RASTER_WHITEFG) &&
			    (fg != RASTER_OTHERFG)) {
				ice_err("Invalid raster foreground value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid raster red foreground value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid raster green foreground value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid raster blue foreground value.", NONFATAL);
				delete filenm;
				return;
			}
			rfg= (unsigned char) ir;
			gfg= (unsigned char) ig;
			bfg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Ras: BG ", ice_iobuf, strlen("%%ICE-Ras: BG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Ras: BG "), "%d %d %d %d", &bg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid raster background value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((bg != RASTER_GLOBALBG) &&
			    (bg != RASTER_WHITEBG) &&
			    (bg != RASTER_BLACKBG) &&
			    (bg != RASTER_OTHERBG)) {
				ice_err("Invalid raster background value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid raster red background value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid raster green background value.", NONFATAL);
				delete filenm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid raster blue background value.", NONFATAL);
				delete filenm;
				return;
			}
			rbg= (unsigned char) ir;
			gbg= (unsigned char) ig;
			bbg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Ras: Seq ", ice_iobuf, strlen("%%ICE-Ras: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Ras: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid raster sequence value.", NONFATAL);
				delete filenm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Ras: IOT ", ice_iobuf, strlen("%%ICE-Ras: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Ras: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid raster tag value.", NONFATAL);
				delete filenm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid raster tag value.", NONFATAL);
				delete filenm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Ras: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Raster end not found.", NONFATAL);
		delete filenm;
		return;
	}

	if (gdf == INSICE_CURRGDF) {
		switch (newobj) {
		case INSICE_NEWOBJCURRGDF:
			if (fg == RASTER_GLOBALFG) {
				rfg= gdf_rfg;
				gfg= gdf_gfg;
				bfg= gdf_bfg;
			}
			if (bg == RASTER_GLOBALBG) {
				rbg= gdf_rbg;
				gbg= gdf_gbg;
				bbg= gdf_bbg;
			}
			break;
		case INSICE_NEWOBJNEWGDF:
			if (fg == RASTER_GLOBALFG)
				fg= RASTER_OTHERFG;
			if (bg == RASTER_GLOBALBG)
				bg= RASTER_OTHERBG;
			break;
		}
	}

	if ((ras= new Raster((Dlnk **) &grobjs, filenm, dpy, visual, fg_pixel, pg_pixdepth, seq)) == (Raster *) NULL) {
		ice_err("Cannot create raster.", NONFATAL);
		delete filenm;
		return;
	}
	delete filenm;
	nrasters++;
	if (ras->getname() == (char *) NULL) {
		ice_err("Raster load error.", NONFATAL);
		ras_del(ras);
		return;
	}

	ras->setfloc(fx, fy, (float) pg_dpi);
	(void) ras->setpixrep(rep);
	(void) ras->setorigloc(orig);
	(void) ras->setdrawmode(draw);
	ras->getrassize(&w, &h, &d);
	if (d == 1) {
		if (pg_pixdepth == 8) {
			fgp= cmap_lookup(rfg, gfg, bfg, PSEUDOCOLOR_MAPSZ);
			bgp= cmap_lookup(rbg, gbg, bbg, PSEUDOCOLOR_MAPSZ);
		}
		else if (pg_pixdepth == 1) {
			fgp= cmap_lookup(rfg, gfg, bfg, 2);
			bgp= cmap_lookup(rbg, gbg, bbg, 2);
		}
		if (fgp == bgp) {
			fgp= black_pixel;
			bgp= white_pixel;
		}
		ras->setfg(fg, fgp, rfg, gfg, bfg);
		ras->setbg(bg, bgp, rbg, gbg, bbg);
	}
	ras->setiotag(iot);

	if (draw == RASTER_FULL) {
		if (ras->bldimage() != GROBJ_SUCCESS) {
			ice_err("Raster load error.", NONFATAL);
			ras_del(ras);
			return;
		}
	}
	if ((delitem= menuitem_create(LXMI_STRING, ras->getname(),
				LXMI_CLIENTDATA, (char *) ras,
				LXMI_PROC, delras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(ras);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, ras->getname(),
				LXMI_CLIENTDATA, (char *) ras,
				LXMI_PROC, attrras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(ras);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, ras->getname(),
				LXMI_CLIENTDATA, (char *) ras,
				LXMI_PROC, trras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, ras->getname(),
				LXMI_CLIENTDATA, (char *) ras,
				LXMI_PROC, cpras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, ras->getname(),
				LXMI_CLIENTDATA, (char *) ras,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, ras->getname(),
				LXMI_CLIENTDATA, (char *) ras,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		ras_del(ras);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	if ((pg_pixdepth >= 8) && (d >= 8)) {
		if ((cmapitem= menuitem_create(LXMI_STRING, ras->getname(),
				LXMI_CLIENTDATA, (char *) ras,
				LXMI_PROC, cmapras_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(ras);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			menuitem_destroy(cpitem);
			menuitem_destroy(dmpitem);
			menuitem_destroy(cmpopitem);
			return;
		}
		if (ras->getcolormap() == None)
			(void) menuitem_set(cmapitem, LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}
	(void) menuitem_insert(delras_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrras_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trras_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpras_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpras_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopras_menu, cmpopitem);
	if ((pg_pixdepth >= 8) && (d >= 8))
		(void) menuitem_insert(cmap_menu, cmapitem);

	return;
}

void
rasins_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, w, h, d, junk;
	int rw, rh, rd;
	float fx, fy;
	Menu_item *delitem, *attritem;
	Menu_item *tritem, *dmpitem, *cmapitem;
	Menu_item *cpitem, *cmpopitem;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!rasoutline_drawn)
			return;
		bevt= (XButtonEvent *) event;
		if (pgcoords_drawn)
			pg_erasecoords();
		ras_drawoutline(new_ras);
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		x= bevt->x+vx;
		y= bevt->y+vy;
		new_ras->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		if (new_ras->getdrawmode() == RASTER_FULL) {
			if (new_ras->bldimage() != GROBJ_SUCCESS) {
				ice_err("Raster load error.", NONFATAL);
				ras_del(new_ras);
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
		}
		if ((delitem= menuitem_create(LXMI_STRING, new_ras->getname(),
					LXMI_CLIENTDATA, (char *) new_ras,
					LXMI_PROC, delras_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_ras->getname(),
					LXMI_CLIENTDATA, (char *) new_ras,
					LXMI_PROC, attrras_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_ras->getname(),
					LXMI_CLIENTDATA, (char *) new_ras,
					LXMI_PROC, trras_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_ras->getname(),
					LXMI_CLIENTDATA, (char *) new_ras,
					LXMI_PROC, cpras_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_ras->getname(),
					LXMI_CLIENTDATA, (char *) new_ras,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
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
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_ras->getname(),
					LXMI_CLIENTDATA, (char *) new_ras,
					LXMI_PROC, cmpop_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			ras_del(new_ras);
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
		new_ras->getrassize(&rw, &rh, &rd);
		if ((pg_pixdepth >= 8) && (rd >= 8)) {
			if ((cmapitem= menuitem_create(LXMI_STRING, new_ras->getname(),
					LXMI_CLIENTDATA, (char *) new_ras,
					LXMI_PROC, cmapras_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
				ice_err("Internal toolkit error.", NONFATAL);
				ras_del(new_ras);
				menuitem_destroy(delitem);
				menuitem_destroy(attritem);
				menuitem_destroy(tritem);
				menuitem_destroy(cpitem);
				menuitem_destroy(dmpitem);
				menuitem_destroy(cmpopitem);
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
			if (new_ras->getcolormap() == None)
				(void) menuitem_set(cmapitem, LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		}
		(void) menuitem_insert(delras_menu, delitem);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrras_menu, attritem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trras_menu, tritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cpras_menu, cpitem);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmpras_menu, dmpitem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cmpopras_menu, cmpopitem);
		if ((pg_pixdepth >= 8) && (rd >= 8))
			(void) menuitem_insert(cmap_menu, cmapitem);
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
		new_ras->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_ras->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		new_ras->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_ras->getloc(&fx, &fy, &x, &junk);
		break;
	default:
		return;
	}

	if (rasoutline_drawn)
		ras_drawoutline(new_ras);
	new_ras->getsize(&w, &h, &d);
	pg_showcoords(x-vx+1, y-vy+1, w-2, h-2, fx, fy, x, pg_pixheight-1-y);
	ras_x= x;
	ras_y= y;
	ras_drawoutline(new_ras);

	return;
}

void
delras_proc(Menu *m, Menu_item *mi)
{
	Raster *ras;
	Menu_item *item;

	if ((ras= (Raster *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Raster *) NULL) {
		ice_err("Cannot locate selected raster.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	if (curr_cmap == ras->getcolormap()) {
		cmap_copy(def_cmap, cmap);
		curr_cmap= def_cmap;
	}
	ras_del(ras);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrras_menu, LXMI_CLIENTDATA, (char *) ras);
	menuitem_delete(attrras_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trras_menu, LXMI_CLIENTDATA, (char *) ras);
	menuitem_delete(trras_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cpras_menu, LXMI_CLIENTDATA, (char *) ras);
	menuitem_delete(cpras_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmpras_menu, LXMI_CLIENTDATA, (char *) ras);
	menuitem_delete(dmpras_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpopras_menu, LXMI_CLIENTDATA, (char *) ras);
	menuitem_delete(cmpopras_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmap_menu, LXMI_CLIENTDATA, (char *) ras);
	if (item != (Menu_item *) NULL) {
		menuitem_delete(cmap_menu, item);
		menuitem_destroy(item);
	}
	if (nrasters == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
ras_del(Raster *ras)
{
	if (grobjs == (Grobj *) ras) {
		if (ras->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) ras->succ();
	}
	ras->unlink();
	delete ras;
	nrasters--;
	return;
}
