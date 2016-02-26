/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Psdoc.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		psdinscont_proc(Panel *, Panel_item *);

static Psdoc *attr_psd;

void
attrpsd_proc(Menu *m, Menu_item *mi)
{
	char buf[20];
	float hscale, vscale;
	int dtk, state;
	unsigned char rdtk, gdtk, bdtk;
	Path *pth;

	if ((attr_psd= (Psdoc *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Psdoc *) NULL) {
		ice_err("Cannot locate selected raster.", NONFATAL);
		return;
	}

	panelitem_set(psdattr_panel, psdattr_fname, LXPTEXT_VALUE, attr_psd->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	attr_psd->getscale(&hscale, &vscale);
	(void) gconvert((double) hscale, 10, 0, buf);
	panelitem_set(psdattr_panel, psdattr_hscale, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) vscale, 10, 0, buf);
	panelitem_set(psdattr_panel, psdattr_vscale, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) gconvert((double) attr_psd->getrotation(), 10, 0, buf);
	panelitem_set(psdattr_panel, psdattr_rot, LXPTEXT_VALUE, buf, LXPI_NULL);

	pth= attr_psd->getclip();
	if (pth == (Path *) NULL)
		panelitem_set(psdattr_panel, psdattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(psdattr_panel, psdattr_clip, LXPTEXT_VALUE, pth->getname(), LXPI_NULL);

	attr_psd->getdtk(&dtk, &rdtk, &gdtk, &bdtk);
	panelitem_set(psdattr_panel, psdattr_dtk, LXPENUM_VALUE, dtk, LXPI_NULL);
	if (dtk == GROBJ_OTHERDTK)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rdtk);
	panelitem_set(psdattr_panel, psdattr_rdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdtk);
	panelitem_set(psdattr_panel, psdattr_gdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bdtk);
	panelitem_set(psdattr_panel, psdattr_bdtk, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	(void) sprintf(buf, "%1d", attr_psd->getsequence());
	panelitem_set(psdattr_panel, psdattr_seq, LXPTEXT_VALUE, buf, LXPI_NULL);

	ice_op= PSD_ATTR;
	XMapRaised(dpy, psdattr_frame);
	return;
}

void
psdattrdtk_proc(Panel *p, Panel_item *pi)
{
	int dtk;

	dtk= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (dtk) {
	case GROBJ_GLOBALDTK:
	case GROBJ_WHITEDTK:
	case GROBJ_BLACKDTK:
		panelitem_set(p, psdattr_rdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, psdattr_gdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, psdattr_bdtk, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case GROBJ_OTHERDTK:
		panelitem_set(p, psdattr_rdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, psdattr_gdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, psdattr_bdtk, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
psdattrcont_proc(Panel *p, Panel_item *pi)
{
	float hscale, vscale, rot;
	float ohscale, ovscale;
	int seq, dtk, odtk;
	char *cptr, *buf;
	unsigned char rdtk, gdtk, bdtk;
	unsigned char ordtk, ogdtk, obdtk;
	unsigned long dtkpix;
	char *pathnm;
	Path *pth, *opth;
	boolean redraw;

	if (ice_op == PSD_INSERTATTR) {
		psdinscont_proc(p, pi);
		return;
	}

	XUnmapWindow(dpy, psdattr_frame);
	ice_op= MAIN_MENU;
	redraw= FALSE;

	buf= (char *) panelitem_get(psdattr_panel, psdattr_hscale, LXPTEXT_VALUE);
	hscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid horizontal scale value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(psdattr_panel, psdattr_vscale, LXPTEXT_VALUE);
	vscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid vertical scale value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(psdattr_panel, psdattr_rot, LXPTEXT_VALUE);
	rot= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid rotation value.", NONFATAL);
		return;
	}

	pathnm= (char *) panelitem_get(psdattr_panel, psdattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(psdattr_panel, psdattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(psdattr_panel, psdattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(psdattr_panel, psdattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(psdattr_panel, psdattr_bdtk, LXPTEXT_VALUE);
		bdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue value.", NONFATAL);
			return;
		}
		if ((bdtk < 0) || (bdtk > 255)) {
			ice_err("Invalid blue value.", NONFATAL);
			return;
		}
		break;
	}

	buf= (char *) panelitem_get(psdattr_panel, psdattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	attr_psd->getscale(&ohscale, &ovscale);
	if ((ohscale != hscale) || (ovscale != vscale)) {
		redraw= TRUE;
		attr_psd->setscale(hscale, vscale);
	}
	if (rot != attr_psd->getrotation()) {
		redraw= TRUE;
		attr_psd->setrotation(rot);
	}
	opth= attr_psd->getclip();
	if (opth != pth) {
		redraw= TRUE;
		if (opth != (Path *) NULL)
			(void) opth->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		attr_psd->setclip(pth);
	}
	attr_psd->getdtk(&odtk, &ordtk, &ogdtk, &obdtk);
	if ((ordtk != rdtk) || (ogdtk != gdtk) || (obdtk != bdtk)) {
		redraw= TRUE;
		(void) attr_psd->setdtk(dtk, rdtk, gdtk, bdtk);
		if (pg_pixdepth == 1)
			dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
		else
			dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
		attr_psd->setdtkpix(dtkpix);
	}
	if (seq != attr_psd->getsequence()) {
		redraw= TRUE;
		attr_psd->setsequence(seq);
		attr_psd->sortsequence(&grobjs);
	}

	if (redraw) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		pg_draw();
		XSync(dpy, False);
		XDefineCursor(dpy, pg_cwin, std_cursor);
	}

	return;
}

void
psdattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, psdattr_frame);
	ice_op= MAIN_MENU;
	return;
}
