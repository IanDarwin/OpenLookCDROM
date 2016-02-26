/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Pathdup.h"
#include "Text.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strncmp(char *, char *, int);
int			strlen(char *);
}

extern void		attrtext_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cptext_proc(Menu *, Menu_item *);
extern void		deltext_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern FILE *		doc_open(char *, char *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		text_del(Text *);
extern void		trtext_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

static Text *new_text;

void
instext_proc(Menu *m, Menu_item *mi)
{
	boolean found;
	int ff, fn;
	char buf[80];

	panelitem_set(textattr_panel, textattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		ice_op= TEXT_INSERTATTR;
		XMapRaised(dpy, textattr_frame);
		return;
	}

	panelitem_set(textattr_panel, textattr_src, LXPENUM_VALUE, TEXT_USERINPUT, LXPI_NULL);
	panelitem_set(textattr_panel, textattr_text, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, "", LXPI_NULL);
	panelitem_set(textattr_panel, textattr_filenm, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(textattr_panel, textattr_font, LXPENUM_VALUE, TEXT_GLOBALFONT, LXPI_NULL);
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
	panelitem_set(textattr_panel, textattr_fontfamily, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, ff, LXPI_NULL);
	if (text_psff != (Panel_item *) NULL)
		panelitem_set(textattr_panel, text_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	text_psff= psfontfamilies[ff].psff_textpi;
	panelitem_set(textattr_panel, text_psff, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, fn, LXPI_NULL);

	panelitem_set(textattr_panel, textattr_fontsz, LXPENUM_VALUE, TEXT_GLOBALFONTSZ, LXPI_NULL);
	(void) gconvert((double) gdf_fontsize, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_size, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) gdf_fontlead, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_lead, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(textattr_panel, textattr_just, LXPENUM_VALUE, TEXT_FLUSHLEFT, LXPI_NULL);

	panelitem_set(textattr_panel, textattr_path, LXPTEXT_VALUE, "", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(textattr_panel, textattr_pathoffset, LXPTEXT_VALUE, "0", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(textattr_panel, textattr_letterspace, LXPTEXT_VALUE, "0", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(textattr_panel, textattr_fg, LXPENUM_VALUE, TEXT_GLOBALFG, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfg);
	panelitem_set(textattr_panel, textattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	panelitem_set(textattr_panel, textattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	panelitem_set(textattr_panel, textattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(textattr_panel, textattr_bgmode, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, TEXT_TRANSPARENTBGM, LXPI_NULL);
	panelitem_set(textattr_panel, textattr_bg, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, TEXT_GLOBALBG, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rbg);
	panelitem_set(textattr_panel, textattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gbg);
	panelitem_set(textattr_panel, textattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bbg);
	panelitem_set(textattr_panel, textattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(textattr_panel, textattr_hscale, LXPTEXT_VALUE, "1", LXPI_NULL);
	panelitem_set(textattr_panel, textattr_vscale, LXPTEXT_VALUE, "1", LXPI_NULL);
	panelitem_set(textattr_panel, textattr_rot, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, "0", LXPI_NULL);

	panelitem_set(textattr_panel, textattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(textattr_panel, textattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	panelitem_set(textattr_panel, textattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	panelitem_set(textattr_panel, textattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	panelitem_set(textattr_panel, textattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(textattr_panel, textattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= TEXT_INSERTATTR;
	XMapRaised(dpy, textattr_frame);
	return;
}

void
textinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *name, *text, *filenm, *textfontnm, *buf;
	float size, lead, poff, lspace, hscale, vscale, rot;
	int src, font, ff, fn, fontsz, just, fg, bgmode, bg, dtk, seq;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char nmbuf[30];
	FILE *fp;
	char *tpathnm, *pathnm;
	Path *tpth, *pth;
	float fx, fy;
	char xbuf[30], ybuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	XUnmapWindow(dpy, textattr_frame);
	ice_op= MAIN_MENU;

	name= (char *) panelitem_get(textattr_panel, textattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		(void) sprintf(nmbuf, "UnnamedText-%d", unnamed_texts++);
		name= nmbuf;
	}

	src= *((int *) panelitem_get(textattr_panel, textattr_src, LXPENUM_VALUE));
	switch (src) {
	case TEXT_USERINPUT:
		text= (char *) panelitem_get(textattr_panel, textattr_text, LXPTEXT_VALUE);
		if (strlen(text) == 0) {
			ice_err("No text specified.", NONFATAL);
			return;
		}
		break;
	case TEXT_FILEINPUT:
		filenm= (char *) panelitem_get(textattr_panel, textattr_filenm, LXPTEXT_VALUE);
		if (strlen(filenm) == 0) {
			ice_err("No file name specified.", NONFATAL);
			return;
		}
		if ((fp= doc_open(filenm, "r")) == (FILE *) NULL) {
			(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
			ice_err(errmsg, NONFATAL);
			return;
		}
		(void) fclose(fp);
		break;
	}

	font= *((int *) panelitem_get(textattr_panel, textattr_font, LXPENUM_VALUE));
	switch (font) {
	case TEXT_GLOBALFONT:
		textfontnm= gdf_fontname;
		break;
	case TEXT_OTHERFONT:
		ff= *((int *) panelitem_get(textattr_panel, textattr_fontfamily, LXPENUM_VALUE));
		fn= *((int *) panelitem_get(textattr_panel, text_psff, LXPENUM_VALUE));
		textfontnm= psfontfamilies[ff].psff_fonts[fn];
		break;
	}

	fontsz= *((int *) panelitem_get(textattr_panel, textattr_fontsz, LXPENUM_VALUE));
	switch (fontsz) {
	case TEXT_GLOBALFONTSZ:
		size= gdf_fontsize;
		lead= gdf_fontlead;
		break;
	case TEXT_OTHERFONTSZ:
		buf= (char *) panelitem_get(textattr_panel, textattr_size, LXPTEXT_VALUE);
		size= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid size value.", NONFATAL);
			return;
		}
		if (size <= 0.) {
			ice_err("Size value must be greater than 0.", NONFATAL);
			return;
		}

		buf= (char *) panelitem_get(textattr_panel, textattr_lead, LXPTEXT_VALUE);
		lead= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid lead value.", NONFATAL);
			return;
		}
		break;
	}
	
	just= *((int *) panelitem_get(textattr_panel, textattr_just, LXPENUM_VALUE));
	switch (just) {
	case TEXT_FLUSHLEFT:
	case TEXT_FLUSHRIGHT:
	case TEXT_CENTER:
	case TEXT_JUSTIFY:
		tpth= (Path *) NULL;
		poff= 0.;
		lspace= 0.;
	
		bgmode= *((int *) panelitem_get(textattr_panel, textattr_bgmode, LXPENUM_VALUE));
		switch (bgmode) {
		case TEXT_TRANSPARENTBGM:
			bg= TEXT_GLOBALBG;
			rbg= gdf_rbg;
			gbg= gdf_gbg;
			bbg= gdf_bbg;
			break;
		case TEXT_OPAQUEBGM:
			bg= *((int *) panelitem_get(textattr_panel, textattr_bg, LXPENUM_VALUE));
			switch (bg) {
			case TEXT_GLOBALBG:
				rbg= gdf_rbg;
				gbg= gdf_gbg;
				bbg= gdf_bbg;
				break;
			case TEXT_WHITEBG:
				rbg= gbg= bbg= 255;
				break;
			case TEXT_BLACKBG:
				rbg= gbg= bbg= 0;
				break;
			case TEXT_OTHERBG:
				buf= (char *) panelitem_get(textattr_panel, textattr_rbg, LXPTEXT_VALUE);
				rbg= (unsigned char) strtol(buf, &cptr, 10);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid red background value.", NONFATAL);
					return;
				}
				if ((rbg < 0) || (rbg > 255)) {
					ice_err("Invalid red background value.", NONFATAL);
					return;
				}
				buf= (char *) panelitem_get(textattr_panel, textattr_gbg, LXPTEXT_VALUE);
				gbg= (unsigned char) strtol(buf, &cptr, 10);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid green background value.", NONFATAL);
					return;
				}
				if ((gbg < 0) || (gbg > 255)) {
					ice_err("Invalid green background value.", NONFATAL);
					return;
				}
				buf= (char *) panelitem_get(textattr_panel, textattr_bbg, LXPTEXT_VALUE);
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
			break;
		}

		buf= (char *) panelitem_get(textattr_panel, textattr_rot, LXPTEXT_VALUE);
		rot= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid rotation value.", NONFATAL);
			return;
		}
		break;
	case TEXT_PATH:
		tpathnm= (char *) panelitem_get(textattr_panel, textattr_path, LXPTEXT_VALUE);
		if (strlen(tpathnm) == 0) {
			ice_err("Unspecified path name.", NONFATAL);
			return;
		}
		for (tpth= paths; tpth != (Path *) NULL; tpth= (Path *) tpth->succ()) {
			if (!strcmp(tpathnm, tpth->getname()))
				break;
		}
		if (tpth == (Path *) NULL) {
			ice_err("Specified path does not exist.", NONFATAL);
			return;
		}

		buf= (char *) panelitem_get(textattr_panel, textattr_pathoffset, LXPTEXT_VALUE);
		poff= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid path offset value.", NONFATAL);
			return;
		}
		if (poff < 0.) {
			ice_err("Path offset value must be non-negative.", NONFATAL);
			return;
		}

		buf= (char *) panelitem_get(textattr_panel, textattr_letterspace, LXPTEXT_VALUE);
		lspace= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid letterspace value.", NONFATAL);
			return;
		}

		bgmode= TEXT_TRANSPARENTBGM;
		bg= TEXT_GLOBALBG;
		rbg= gdf_rbg;
		gbg= gdf_gbg;
		bbg= gdf_bbg;
		rot= 0.;
		break;
	}

	fg= *((int *) panelitem_get(textattr_panel, textattr_fg, LXPENUM_VALUE));
	switch (fg) {
	case TEXT_GLOBALFG:
		rfg= gdf_rfg;
		gfg= gdf_gfg;
		bfg= gdf_bfg;
		break;
	case TEXT_BLACKFG:
		rfg= gfg= bfg= 0;
		break;
	case TEXT_WHITEFG:
		rfg= gfg= bfg= 255;
		break;
	case TEXT_OTHERFG:
		buf= (char *) panelitem_get(textattr_panel, textattr_rfg, LXPTEXT_VALUE);
		rfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red foreground value.", NONFATAL);
			return;
		}
		if ((rfg < 0) || (rfg > 255)) {
			ice_err("Invalid red foreground value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(textattr_panel, textattr_gfg, LXPTEXT_VALUE);
		gfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green foreground value.", NONFATAL);
			return;
		}
		if ((gfg < 0) || (gfg > 255)) {
			ice_err("Invalid green foreground value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(textattr_panel, textattr_bfg, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(textattr_panel, textattr_hscale, LXPTEXT_VALUE);
	hscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid horizontal scale value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(textattr_panel, textattr_vscale, LXPTEXT_VALUE);
	vscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid vertical scale value.", NONFATAL);
		return;
	}

	pathnm= (char *) panelitem_get(textattr_panel, textattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(textattr_panel, textattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(textattr_panel, textattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(textattr_panel, textattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(textattr_panel, textattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(textattr_panel, textattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	if ((new_text= new Text((Dlnk **) &grobjs, name, seq)) == (Text *) NULL) {
		ice_err("Cannot create text object.", NONFATAL);
		return;
	}
	ntexts++;
	(void) new_text->setsource(src);
	switch (src) {
	case TEXT_USERINPUT:
		(void) new_text->settext(text);
		break;
	case TEXT_FILEINPUT:
		(void) new_text->setfilename(filenm);
		break;
	}
	(void) new_text->setfont(font, textfontnm);
	(void) new_text->setfontsize(fontsz, size, lead);
	(void) new_text->setjustify(just);
	(void) new_text->setpath(tpth, poff, lspace);
	(void) new_text->setforeground(fg, rfg, gfg, bfg);
	(void) new_text->setbackground(bgmode, bg, rbg, gbg, bbg);
	new_text->setscale(hscale, vscale);
	new_text->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_text->setclip(pth);

	(void) new_text->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_text->setdtkpix(dtkpix);

	if (just != TEXT_PATH) {
		ice_op= TEXT_INSERTLOC;
		if (pg_loc == PG_CURSORLOC) {
			pgcoords_drawn= crosshairs_drawn= FALSE;
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
			return;
		}
	}

	if ((delitem= menuitem_create(LXMI_STRING, new_text->getname(),
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, deltext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, new_text->getname(),
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, attrtext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, new_text->getname(),
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, trtext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, new_text->getname(),
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, cptext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, new_text->getname(),
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, new_text->getname(),
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		return;
	}
	(void) menuitem_insert(deltext_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrtext_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trtext_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cptext_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmptext_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoptext_menu, cmpopitem);

	if (just == TEXT_PATH) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		new_text->setploc(0, 0, (float) pg_dpi);
		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		ice_op= MAIN_MENU;
		return;
	}

	loc_grobj= (Grobj *) new_text;
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
insicetxt_rd(FILE *fp, int gdf, int newobj)
{
	char *name, *text, *filenm, *textfontnm, *c;
	float size, lead, poff, lspace;
	int len, src, font, fontsz, just, fg, bgmode, bg, dtk, seq;
	float hscale, vscale, rot;
	int ir, ig, ib;
	float fx, fy;
	boolean endfound;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	int iot;
	char nmbuf[30];
	FILE *txtfp;
	char *tpathnm, *pathnm;
	Path *tpth, *pth;
	Pathdup *duppth;
	char errmsg[MAX_ERRMSGLEN+1];
	Text *txt;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Txt: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Txt: Begin ")+len-1)= '\0';
	if ((name= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(name, ice_iobuf+strlen("%%ICE-Txt: Begin "));
	if (!strncmp(name, "UnnamedText-", strlen("UnnamedText-"))) {
		delete name;
		(void) sprintf(nmbuf, "UnnamedText-%d", unnamed_texts++);
		if ((name= new char[strlen(nmbuf)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(name, nmbuf);
	}

	fx= fy= 0.;
	hscale= vscale= 1.;
	rot= 0.;
	text= (char *) NULL;
	filenm= (char *) NULL;
	font= TEXT_GLOBALFONT;
	if ((textfontnm= new char[strlen(gdf_fontname)+1]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		delete name;
		return;
	}
	(void) strcpy(textfontnm, gdf_fontname);
	fontsz= TEXT_GLOBALFONTSZ;
	size= gdf_fontsize;
	lead= gdf_fontlead;
	just= TEXT_FLUSHLEFT;
	tpathnm= (char *) NULL;
	tpth= (Path *) NULL;
	poff= 0.;
	lspace= 0.;
	fg= TEXT_GLOBALFG;
	rfg= gdf_rfg;
	gfg= gdf_gfg;
	bfg= gdf_bfg;
	bgmode= TEXT_TRANSPARENTBGM;
	bg= TEXT_GLOBALBG;
	rbg= gdf_rbg;
	gbg= gdf_gbg;
	bbg= gdf_bbg;
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

		if (!strncmp("%%ICE-Txt: Loc ", ice_iobuf, strlen("%%ICE-Txt: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: Loc "), "%f %f", &fx, &fy) != 2) {
				ice_err("Invalid text location value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Txt: Trans ", ice_iobuf, strlen("%%ICE-Txt: Trans "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: Trans "), "%f %f %f", &rot, &hscale, &vscale) != 3) {
				ice_err("Invalid text transform value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Txt: Text ", ice_iobuf, strlen("%%ICE-Txt: Text "))) {
			delete filenm;
			filenm= (char *) NULL;
			delete text;
			len= strlen(ice_iobuf+strlen("%%ICE-Txt: Text "));
			*(ice_iobuf+strlen("%%ICE-Txt: Text ")+len-1)= '\0';
			if ((text= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			(void) strcpy(text, ice_iobuf+strlen("%%ICE-Txt: Text "));
		}

		else if (!strncmp("%%ICE-Txt: Filenm ", ice_iobuf, strlen("%%ICE-Txt: Filenm "))) {
			delete text;
			text= (char *) NULL;
			delete filenm;
			len= strlen(ice_iobuf+strlen("%%ICE-Txt: Filenm "));
			*(ice_iobuf+strlen("%%ICE-Txt: Filenm ")+len-1)= '\0';
			if ((filenm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete text;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			(void) strcpy(filenm, ice_iobuf+strlen("%%ICE-Txt: Filenm "));
			if ((txtfp= doc_open(filenm, "r")) == (FILE *) NULL) {
				(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
				ice_err(errmsg, NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			(void) fclose(txtfp);
		}

		else if (!strncmp("%%ICE-Txt: Font ", ice_iobuf, strlen("%%ICE-Txt: Font "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: Font "), "%d", &font) != 1) {
				ice_err("Invalid text font value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((font != TEXT_GLOBALFONT) &&
			    (font != TEXT_OTHERFONT)) {
				ice_err("Invalid text font value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			c= ice_iobuf+strlen("%%ICE-Txt: Font ");
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			delete textfontnm;
			len= strlen(c);
			*(c+len-1)= '\0';
			if ((textfontnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			(void) strcpy(textfontnm, c);
		}

		else if (!strncmp("%%ICE-Txt: Size ", ice_iobuf, strlen("%%ICE-Txt: Size "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: Size "), "%d %f %f", &fontsz, &size, &lead) != 3) {
				ice_err("Invalid text size value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((fontsz != TEXT_GLOBALFONTSZ) &&
			    (fontsz != TEXT_OTHERFONTSZ)) {
				ice_err("Invalid text size value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if (size <= 0.) {
				ice_err("Invalid text size value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Txt: Just ", ice_iobuf, strlen("%%ICE-Txt: Just "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: Just "), "%d", &just) != 1) {
				ice_err("Invalid text justification value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((just != TEXT_FLUSHLEFT) &&
			    (just != TEXT_FLUSHRIGHT) &&
			    (just != TEXT_CENTER) &&
			    (just != TEXT_JUSTIFY) &&
			    (just != TEXT_PATH)) {
				ice_err("Invalid text justification value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Txt: Pathnm ", ice_iobuf, strlen("%%ICE-Txt: Pathnm "))) {
			c= ice_iobuf+strlen("%%ICE-Txt: Pathnm ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			if ((tpathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete pathnm;
				return;
			}
			(void) strcpy(tpathnm, c);
		}

		else if (!strncmp("%%ICE-Txt: Path ", ice_iobuf, strlen("%%ICE-Txt: Path "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: Path "), "%f %f", &poff, &lspace) != 2) {
				ice_err("Invalid text path value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if (poff < 0.) {
				ice_err("Invalid text path offset value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Txt: FG ", ice_iobuf, strlen("%%ICE-Txt: FG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: FG "), "%d %d %d %d", &fg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid text foreground value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((fg != TEXT_GLOBALFG) &&
			    (fg != TEXT_BLACKFG) &&
			    (fg != TEXT_WHITEFG) &&
			    (fg != TEXT_OTHERFG)) {
				ice_err("Invalid text foreground value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid text red foreground value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid text green foreground value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid text blue foreground value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			rfg= (unsigned char) ir;
			gfg= (unsigned char) ig;
			bfg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Txt: BG ", ice_iobuf, strlen("%%ICE-Txt: BG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: BG "), "%d %d %d %d %d", &bgmode, &bg, &ir, &ig, &ib) != 5) {
				ice_err("Invalid text background value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((bgmode != TEXT_TRANSPARENTBGM) &&
			    (bgmode != TEXT_OPAQUEBGM)) {
				ice_err("Invalid text background mode value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((bg != TEXT_GLOBALBG) &&
			    (bg != TEXT_WHITEBG) &&
			    (bg != TEXT_BLACKBG) &&
			    (bg != TEXT_OTHERBG)) {
				ice_err("Invalid text background value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid text red background value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid text green background value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid text blue background value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			rbg= (unsigned char) ir;
			gbg= (unsigned char) ig;
			bbg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Txt: Clip ", ice_iobuf, strlen("%%ICE-Txt: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Txt: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strncmp("%%ICE-Txt: DTK ", ice_iobuf, strlen("%%ICE-Txt: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid text DTK value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid text DTK value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid text red DTK value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid text green DTK value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid text blue DTK value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Txt: Seq ", ice_iobuf, strlen("%%ICE-Txt: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid text sequence value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Txt: IOT ", ice_iobuf, strlen("%%ICE-Txt: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Txt: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid text tag value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid text tag value.", NONFATAL);
				delete name;
				delete text;
				delete filenm;
				delete textfontnm;
				delete tpathnm;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Txt: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Text end not found.", NONFATAL);
		delete name;
		delete text;
		delete filenm;
		delete textfontnm;
		delete tpathnm;
		delete pathnm;
		return;
	}
	if ((text == (char *) NULL) && (filenm == (char *) NULL)) {
		ice_err("Text source unspecified.", NONFATAL);
		delete name;
		delete text;
		delete filenm;
		delete textfontnm;
		delete tpathnm;
		delete pathnm;
		return;
	}
	else if (text != (char *) NULL)
		src= TEXT_USERINPUT;
	else if (filenm != (char *) NULL)
		src= TEXT_FILEINPUT;

	if (just == TEXT_PATH) {
		char *oldname, *newname;

		if (tpathnm == (char *) NULL) {
			ice_err("Text path unspecified.", NONFATAL);
			delete name;
			delete text;
			delete filenm;
			delete textfontnm;
			delete pathnm;
			return;
		}
		for (duppth= duppaths; duppth != (Pathdup *) NULL; duppth= (Pathdup *) duppth->succ()) {
			duppth->getnames(&oldname, &newname);
			if (!strcmp(tpathnm, oldname)) {
				delete tpathnm;
				if ((tpathnm= new char[strlen(newname)+1]) == (char *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete name;
					delete text;
					delete filenm;
					delete textfontnm;
					delete pathnm;
					return;
				}
				(void) strcpy(tpathnm, newname);
				break;
			}
		}
		for (tpth= paths; tpth != (Path *) NULL; tpth= (Path *) tpth->succ()) {
			if (!strcmp(tpathnm, tpth->getname()))
				break;
		}
		if (tpth == (Path *) NULL) {
			ice_err("Specified path does not exist.", NONFATAL);
			delete name;
			delete text;
			delete filenm;
			delete textfontnm;
			delete tpathnm;
			delete pathnm;
			return;
		}
	}
	else
		tpth= (Path *) NULL;
	delete tpathnm;

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
					delete text;
					delete filenm;
					delete textfontnm;
					delete tpathnm;
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
			delete text;
			delete filenm;
			delete textfontnm;
			delete tpathnm;
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
			if (font == TEXT_GLOBALFONT) {
				delete textfontnm;
				if ((textfontnm= new char[strlen(gdf_fontname)+1]) == (char *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete name;
					delete text;
					delete filenm;
					return;
				}
				(void) strcpy(textfontnm, gdf_fontname);
			}
			if (fontsz == TEXT_GLOBALFONTSZ) {
				size= gdf_fontsize;
				lead= gdf_fontlead;
			}
			if (fg == TEXT_GLOBALFG) {
				rfg= gdf_rfg;
				gfg= gdf_gfg;
				bfg= gdf_bfg;
			}
			if (bg == TEXT_GLOBALBG) {
				rbg= gdf_rbg;
				gbg= gdf_gbg;
				bbg= gdf_bbg;
			}
			if (dtk == GROBJ_GLOBALDTK) {
				rdtk= gdf_rdtk;
				gdtk= gdf_gdtk;
				bdtk= gdf_bdtk;
			}
			break;
		case INSICE_NEWOBJNEWGDF:
			if (font == TEXT_GLOBALFONT)
				font= TEXT_OTHERFONT;
			if (fontsz == TEXT_GLOBALFONTSZ)
				fontsz= TEXT_OTHERFONTSZ;
			if (fg == TEXT_GLOBALFG)
				fg= TEXT_OTHERFG;
			if (bg == TEXT_GLOBALBG)
				bg= TEXT_OTHERBG;
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
				if (!strcmp(textfontnm, psfontfamilies[ff].psff_fonts[fn])) {
					found= TRUE;
					break;
				}
			}
			if (found == TRUE)
				break;
		}
		if (found == FALSE) {
			(void) sprintf(errmsg, "Requested text font '%s' not available.", textfontnm);
			ice_err(errmsg, NONFATAL);
			delete name;
			delete text;
			delete filenm;
			delete textfontnm;
			return;
		}
	}

	if ((txt= new Text((Dlnk **) &grobjs, name, seq)) == (Text *) NULL) {
		ice_err("Cannot create text object.", NONFATAL);
		delete name;
		delete text;
		delete filenm;
		delete textfontnm;
		return;
	}
	delete name;
	ntexts++;

	(void) txt->setsource(src);
	switch (src) {
	case TEXT_USERINPUT:
		(void) txt->settext(text);
		break;
	case TEXT_FILEINPUT:
		(void) txt->setfilename(filenm);
		break;
	}
	delete text;
	delete filenm;
	(void) txt->setfont(font, textfontnm);
	delete textfontnm;
	(void) txt->setfontsize(fontsz, size, lead);
	(void) txt->setjustify(just);
	(void) txt->setpath(tpth, poff, lspace);
	(void) txt->setforeground(fg, rfg, gfg, bfg);
	(void) txt->setbackground(bgmode, bg, rbg, gbg, bbg);
	txt->setfloc(fx, fy, (float) pg_dpi);
	txt->setscale(hscale, vscale);
	txt->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	txt->setclip(pth);
	txt->setiotag(iot);

	(void) txt->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	txt->setdtkpix(dtkpix);

	if ((delitem= menuitem_create(LXMI_STRING, txt->getname(),
				LXMI_CLIENTDATA, (char *) txt,
				LXMI_PROC, deltext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(txt);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, txt->getname(),
				LXMI_CLIENTDATA, (char *) txt,
				LXMI_PROC, attrtext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(txt);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, txt->getname(),
				LXMI_CLIENTDATA, (char *) txt,
				LXMI_PROC, trtext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(txt);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, txt->getname(),
				LXMI_CLIENTDATA, (char *) txt,
				LXMI_PROC, cptext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(txt);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, txt->getname(),
				LXMI_CLIENTDATA, (char *) txt,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(txt);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, txt->getname(),
				LXMI_CLIENTDATA, (char *) txt,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(txt);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	(void) menuitem_insert(deltext_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrtext_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trtext_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cptext_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmptext_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoptext_menu, cmpopitem);

	return;
}

void
textins_event(XEvent *event)
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
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		x= bevt->x+vx;
		y= bevt->y+vy;
		new_text->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		if ((delitem= menuitem_create(LXMI_STRING, new_text->getname(),
					LXMI_CLIENTDATA, (char *) new_text,
					LXMI_PROC, deltext_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			text_del(new_text);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_text->getname(),
					LXMI_CLIENTDATA, (char *) new_text,
					LXMI_PROC, attrtext_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			text_del(new_text);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_text->getname(),
					LXMI_CLIENTDATA, (char *) new_text,
					LXMI_PROC, trtext_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			text_del(new_text);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_text->getname(),
					LXMI_CLIENTDATA, (char *) new_text,
					LXMI_PROC, cptext_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			text_del(new_text);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_text->getname(),
					LXMI_CLIENTDATA, (char *) new_text,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			text_del(new_text);
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
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_text->getname(),
					LXMI_CLIENTDATA, (char *) new_text,
					LXMI_PROC, cmpop_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			text_del(new_text);
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
		(void) menuitem_insert(deltext_menu, delitem);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrtext_menu, attritem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trtext_menu, tritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cptext_menu, cpitem);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmptext_menu, dmpitem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cmpoptext_menu, cmpopitem);
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
		new_text->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_text->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		new_text->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_text->getloc(&fx, &fy, &x, &junk);
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
deltext_proc(Menu *m, Menu_item *mi)
{
	Text *text;
	Menu_item *item;

	if ((text= (Text *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Text *) NULL) {
		ice_err("Cannot locate selected text object.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	text_del(text);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrtext_menu, LXMI_CLIENTDATA, (char *) text);
	menuitem_delete(attrtext_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trtext_menu, LXMI_CLIENTDATA, (char *) text);
	menuitem_delete(trtext_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cptext_menu, LXMI_CLIENTDATA, (char *) text);
	menuitem_delete(cptext_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmptext_menu, LXMI_CLIENTDATA, (char *) text);
	menuitem_delete(dmptext_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpoptext_menu, LXMI_CLIENTDATA, (char *) text);
	menuitem_delete(cmpoptext_menu, item);
	menuitem_destroy(item);
	if (ntexts == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
text_del(Text *text)
{
	if (grobjs == (Grobj *) text) {
		if (text->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) text->succ();
	}
	text->unlink();
	delete text;
	ntexts--;
	return;
}
