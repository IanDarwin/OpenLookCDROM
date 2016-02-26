/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Text.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern FILE *		doc_open(char *, char *);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		textinscont_proc(Panel *, Panel_item *);

static Text *attr_text;

void
attrtext_proc(Menu *m, Menu_item *mi)
{
	char buf[20];
	char *textfontnm;
	int src, font, ff, fn, fontsz, just;
	float size, lead, poff, lspace;
	boolean found;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	unsigned char rdtk, gdtk, bdtk;
	int fg, bgmode, bg, dtk, state;
	float hscale, vscale;
	Path *pth;

	if ((attr_text= (Text *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Text *) NULL) {
		ice_err("Cannot locate selected text object.", NONFATAL);
		return;
	}

	panelitem_set(textattr_panel, textattr_name, LXPTEXT_VALUE, attr_text->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	src= attr_text->getsource();
	panelitem_set(textattr_panel, textattr_src, LXPENUM_VALUE, src, LXPI_NULL);
	switch (src) {
	case TEXT_USERINPUT:
		panelitem_set(textattr_panel, textattr_filenm, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, attr_text->getfilename(), LXPI_NULL);
		panelitem_set(textattr_panel, textattr_text, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, attr_text->gettext(), LXPI_NULL);
		break;
	case TEXT_FILEINPUT:
		panelitem_set(textattr_panel, textattr_text, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, attr_text->gettext(), LXPI_NULL);
		panelitem_set(textattr_panel, textattr_filenm, LXPI_STATE, LXPI_ACTIVE, LXPTEXT_VALUE, attr_text->getfilename(), LXPI_NULL);
		break;
	}

	attr_text->getfont(&font, &textfontnm);
	panelitem_set(textattr_panel, textattr_font, LXPENUM_VALUE, font, LXPI_NULL);
	switch (font) {
	case TEXT_GLOBALFONT:
		state= LXPI_INACTIVE;
		break;
	case TEXT_OTHERFONT:
		state= LXPI_ACTIVE;
		break;
	}
	found= FALSE;
	for (ff= 0; ff < npsfontfamilies; ff++) {
		char *c;
		c= psfontfamilies[ff].psff_name;
		for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
			if (!strcmp(textfontnm, psfontfamilies[ff].psff_fonts[fn])) {
				found= TRUE;
				break;
			}
		}
		if (found == TRUE)
			break;
	}
	panelitem_set(textattr_panel, textattr_fontfamily, LXPI_STATE, state, LXPENUM_VALUE, ff, LXPI_NULL);
	if (text_psff != (Panel_item *) NULL)
		panelitem_set(textattr_panel, text_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	text_psff= psfontfamilies[ff].psff_textpi;
	panelitem_set(textattr_panel, text_psff, LXPI_STATE, state, LXPENUM_VALUE, fn, LXPI_NULL);

	attr_text->getfontsize(&fontsz, &size, &lead);
	panelitem_set(textattr_panel, textattr_fontsz, LXPENUM_VALUE, fontsz, LXPI_NULL);
	switch (fontsz) {
	case TEXT_GLOBALFONTSZ:
		state= LXPI_INACTIVE;
		break;
	case TEXT_OTHERFONTSZ:
		state= LXPI_ACTIVE;
		break;
	}
	(void) gconvert((double) size, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_size, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) gconvert((double) lead, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_lead, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	just= attr_text->getjustify();
	panelitem_set(textattr_panel, textattr_just, LXPENUM_VALUE, just, LXPI_NULL);

	attr_text->getpath(&pth, &poff, &lspace);
	if (just == TEXT_PATH)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	if (pth == (Path *) NULL)
		panelitem_set(textattr_panel, textattr_path, LXPI_STATE, state, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(textattr_panel, textattr_path, LXPI_STATE, state, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);
	(void) gconvert((double) poff, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_pathoffset, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) lspace, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_letterspace, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_text->getforeground(&fg, &rfg, &gfg, &bfg);
	panelitem_set(textattr_panel, textattr_fg, LXPENUM_VALUE, fg, LXPI_NULL);
	switch (fg) {
	case TEXT_GLOBALFG:
	case TEXT_BLACKFG:
	case TEXT_WHITEFG:
		state= LXPI_INACTIVE;
		break;
	case TEXT_OTHERFG:
		state= LXPI_ACTIVE;
		break;
	}
	(void) sprintf(buf, "%1d", (int) rfg);
	panelitem_set(textattr_panel, textattr_rfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gfg);
	panelitem_set(textattr_panel, textattr_gfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bfg);
	panelitem_set(textattr_panel, textattr_bfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_text->getbackground(&bgmode, &bg, &rbg, &gbg, &bbg);
	if (just != TEXT_PATH)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(textattr_panel, textattr_bgmode, LXPI_STATE, state, LXPENUM_VALUE, bgmode, LXPI_NULL);
	if ((just != TEXT_PATH) && (bgmode == TEXT_OPAQUEBGM))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(textattr_panel, textattr_bg, LXPI_STATE, state, LXPENUM_VALUE, bg, LXPI_NULL);
	if ((just != TEXT_PATH) && (bgmode == TEXT_OPAQUEBGM) && (bg == TEXT_OTHERBG))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rbg);
	panelitem_set(textattr_panel, textattr_rbg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gbg);
	panelitem_set(textattr_panel, textattr_gbg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bbg);
	panelitem_set(textattr_panel, textattr_bbg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	attr_text->getscale(&hscale, &vscale);
	(void) gconvert((double) hscale, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_hscale, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) vscale, 10, 0, buf);
	panelitem_set(textattr_panel, textattr_vscale, LXPTEXT_VALUE, buf, LXPI_NULL);

	if (just != TEXT_PATH)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) gconvert((double) attr_text->getrotation(), 10, 0, buf);
	panelitem_set(textattr_panel, textattr_rot, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_text->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(textattr_panel, textattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(textattr_panel, textattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_text->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(textattr_panel, textattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
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
	panelitem_set(textattr_panel, textattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(textattr_panel, textattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(textattr_panel, textattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_text->getsequence());
	panelitem_set(textattr_panel, textattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	ice_op= TEXT_ATTR;
	XMapRaised(dpy, textattr_frame);
	return;
}

void
textattrsrc_proc(Panel *p, Panel_item *pi)
{
	int s;

	s= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (s) {
	case TEXT_USERINPUT:
		panelitem_set(p, textattr_filenm, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_text, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	case TEXT_FILEINPUT:
		panelitem_set(p, textattr_text, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_filenm, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
textattrfont_proc(Panel *p, Panel_item *pi)
{
	int f;

	f= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (f) {
	case TEXT_GLOBALFONT:
		panelitem_set(p, textattr_fontfamily, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, text_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case TEXT_OTHERFONT:
		panelitem_set(p, textattr_fontfamily, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, text_psff, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
textattrfontfamily_proc(Panel *p, Panel_item *pi)
{
	int f;

	f= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	if (text_psff != (Panel_item *) NULL)
		panelitem_set(p, text_psff, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	text_psff= psfontfamilies[f].psff_textpi;
	panelitem_set(p, text_psff, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	return;
}

void
textattrfontsz_proc(Panel *p, Panel_item *pi)
{
	int f;

	f= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (f) {
	case TEXT_GLOBALFONTSZ:
		panelitem_set(p, textattr_size, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_lead, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case TEXT_OTHERFONTSZ:
		panelitem_set(p, textattr_size, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_lead, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
textattrjust_proc(Panel *p, Panel_item *pi)
{
	int j;
	void textattrbgmode_proc(Panel *, Panel_item *);

	j= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (j) {
	case TEXT_FLUSHLEFT:
	case TEXT_FLUSHRIGHT:
	case TEXT_CENTER:
	case TEXT_JUSTIFY:
		panelitem_set(p, textattr_path, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_pathoffset, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_letterspace, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bgmode, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		textattrbgmode_proc(p, textattr_bgmode);
		panelitem_set(p, textattr_rot, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	case TEXT_PATH:
		panelitem_set(p, textattr_path, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_pathoffset, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_letterspace, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bgmode, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_rot, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
textattrfg_proc(Panel *p, Panel_item *pi)
{
	int fg;

	fg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fg) {
	case TEXT_GLOBALFG:
	case TEXT_BLACKFG:
	case TEXT_WHITEFG:
		panelitem_set(p, textattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case TEXT_OTHERFG:
		panelitem_set(p, textattr_rfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
textattrbgmode_proc(Panel *p, Panel_item *pi)
{
	int mode;
	void textattrbg_proc(Panel *, Panel_item *);

	mode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (mode) {
	case TEXT_TRANSPARENTBGM:
		panelitem_set(p, textattr_bg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case TEXT_OPAQUEBGM:
		panelitem_set(p, textattr_bg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		textattrbg_proc(p, textattr_bg);
		break;
	}
	return;
}

void
textattrbg_proc(Panel *p, Panel_item *pi)
{
	int bg;

	bg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bg) {
	case TEXT_GLOBALBG:
	case TEXT_WHITEBG:
	case TEXT_BLACKBG:
		panelitem_set(p, textattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case TEXT_OTHERBG:
		panelitem_set(p, textattr_rbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
textattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, textattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, textattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, textattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
textattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *text, *filenm, *textfontnm, *buf;
	float size, lead, poff, lspace, hscale, vscale, rot;
	int src, font, ff, fn, fontsz, just, fg, bgmode, bg, dtk, seq;
	unsigned char rfg, gfg, bfg;
	unsigned char rbg, gbg, bbg;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	FILE *fp;
	Path *tpth, *pth, *opth;
	char *tpathnm, *pathnm;
	char errmsg[MAX_ERRMSGLEN+1];

	if (ice_op == TEXT_INSERTATTR) {
		textinscont_proc(p, pi);
		return;
	}

	XUnmapWindow(dpy, textattr_frame);
	ice_op= MAIN_MENU;

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

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_text->setsource(src);
	switch (src) {
	case TEXT_USERINPUT:
		(void) attr_text->settext(text);
		break;
	case TEXT_FILEINPUT:
		(void) attr_text->setfilename(filenm);
		break;
	}
	(void) attr_text->setfont(font, textfontnm);
	(void) attr_text->setfontsize(fontsz, size, lead);
	(void) attr_text->setjustify(just);
	(void) attr_text->setpath(tpth, poff, lspace);
	(void) attr_text->setforeground(fg, rfg, gfg, bfg);
	(void) attr_text->setbackground(bgmode, bg, rbg, gbg, bbg);
	attr_text->setscale(hscale, vscale);
	attr_text->setrotation(rot);
	opth= attr_text->getclip();
	if (opth != pth) {
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_text->setclip(pth);
	}

	(void) attr_text->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	attr_text->setdtkpix(dtkpix);

	if (seq != attr_text->getsequence()) {
		attr_text->setsequence(seq);
		attr_text->sortsequence(&grobjs);
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
textattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, textattr_frame);
	ice_op= MAIN_MENU;
	return;
}
