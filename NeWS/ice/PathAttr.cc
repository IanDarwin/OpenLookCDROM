/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char*);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pthinscont_proc(Panel *, Panel_item *);

static Path *attr_pth;

void
attrpth_proc(Menu *m, Menu_item *mi)
{
	char buf[20];
	int vis, state, fg;
	unsigned char rfg, gfg, bfg;
	unsigned long fgp;

	if ((attr_pth= (Path *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Path *) NULL) {
		ice_err("Cannot locate selected path object.", NONFATAL);
		return;
	}

	panelitem_set(pthattr_panel, pthattr_name, LXPTEXT_VALUE, attr_pth->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	panelitem_set(pthattr_panel, pthattr_src, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(pthattr_panel, pthattr_filenm, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(pthattr_panel, pthattr_closure, LXPENUM_VALUE, attr_pth->getclosure(), LXPI_NULL);

	vis= attr_pth->getvisibility();
	panelitem_set(pthattr_panel, pthattr_vis, LXPENUM_VALUE, vis, LXPI_NULL);
	if (vis == PATH_INVISIBLE)
		state= LXPI_INACTIVE;
	else
		state= LXPI_ACTIVE;
	attr_pth->getfg(&fg, &fgp, &rfg, &gfg, &bfg);
	panelitem_set(pthattr_panel, pthattr_fg, LXPI_STATE, state, LXPENUM_VALUE, fg, LXPI_NULL);
	if ((vis == PATH_VISIBLE) && (fg == PATH_OTHERFG))
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) sprintf(buf, "%1d", (int) rfg);
	panelitem_set(pthattr_panel, pthattr_rfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gfg);
	panelitem_set(pthattr_panel, pthattr_gfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) bfg);
	panelitem_set(pthattr_panel, pthattr_bfg, LXPI_STATE, state, LXPTEXT_VALUE, buf, LXPI_NULL);

	ice_op= PATH_ATTR;
	XMapRaised(dpy, pthattr_frame);
	return;
}

void
pthattrsrc_proc(Panel *p, Panel_item *pi)
{
	int src;

	src= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (src) {
	case PTH_USERINPUT:
		panelitem_set(p, pthattr_filenm, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case PTH_FILEINPUT:
		panelitem_set(p, pthattr_filenm, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
pthattrvis_proc(Panel *p, Panel_item *pi)
{
	int vis;
	void pthattrfg_proc(Panel *, Panel_item *);

	vis= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (vis) {
	case PATH_INVISIBLE:
		panelitem_set(p, pthattr_fg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pthattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pthattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pthattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case PATH_VISIBLE:
		panelitem_set(p, pthattr_fg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		pthattrfg_proc(p, pthattr_fg);
		break;
	}
	return;
}

void
pthattrfg_proc(Panel *p, Panel_item *pi)
{
	int fg;

	fg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (fg) {
	case PATH_BLACKFG:
	case PATH_WHITEFG:
		panelitem_set(p, pthattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pthattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pthattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case PATH_OTHERFG:
		panelitem_set(p, pthattr_rfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pthattr_gfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pthattr_bfg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
pthattrcont_proc(Panel *p, Panel_item *pi)
{
	char *buf, *cptr;
	int cl, vis, fg, ocl, ovis, ofg;
	unsigned char rfg, gfg, bfg;
	unsigned long fgp, ofgp;
	unsigned char orfg, ogfg, obfg;
	boolean page_redraw, path_redraw;
	char errmsg[MAX_ERRMSGLEN+1];

	if (ice_op == PATH_INSERTATTR) {
		pthinscont_proc(p, pi);
		return;
	}

	XUnmapWindow(dpy, pthattr_frame);
	ice_op= MAIN_MENU;

	cl= *((int *) panelitem_get(pthattr_panel, pthattr_closure, LXPENUM_VALUE));

	vis= *((int *) panelitem_get(pthattr_panel, pthattr_vis, LXPENUM_VALUE));
	switch (vis) {
	case PATH_INVISIBLE:
		fg= PATH_BLACKFG;
		rfg= bfg= gfg= (unsigned char) 0;
		break;
	case PATH_VISIBLE:
		fg= *((int *) panelitem_get(pthattr_panel, pthattr_fg, LXPENUM_VALUE));
		switch (fg) {
		case PATH_BLACKFG:
			rfg= bfg= gfg= (unsigned char) 0;
			break;
		case PATH_WHITEFG:
			rfg= bfg= gfg= (unsigned char) 255;
			break;
		case PATH_OTHERFG:
			buf= (char *) panelitem_get(pthattr_panel, pthattr_rfg, LXPTEXT_VALUE);
			rfg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red value.", NONFATAL);
				return;
			}
			if ((rfg < 0) || (rfg > 255)) {
				ice_err("Invalid red value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(pthattr_panel, pthattr_gfg, LXPTEXT_VALUE);
			gfg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green value.", NONFATAL);
				return;
			}
			if ((gfg < 0) || (gfg > 255)) {
				ice_err("Invalid green value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(pthattr_panel, pthattr_bfg, LXPTEXT_VALUE);
			bfg= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid blue value.", NONFATAL);
				return;
			}
			if ((bfg < 0) || (bfg > 255)) {
				ice_err("Invalid blue value.", NONFATAL);
				return;
			}
			break;
		}
		break;
	}

	ocl= attr_pth->getclosure();
	ovis= attr_pth->getvisibility();
	attr_pth->getfg(&ofg, &ofgp, &orfg, &ogfg, &obfg);

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	(void) attr_pth->setclosure(cl);
	(void) attr_pth->setvisibility(vis);
	if (pg_pixdepth == 8)
		fgp= cmap_lookup(rfg, gfg, bfg, PSEUDOCOLOR_MAPSZ);
	else if (pg_pixdepth == 1)
		fgp= cmap_lookup(rfg, gfg, bfg, 2);
	attr_pth->setfg(fg, fgp, rfg, gfg, bfg);

	page_redraw= path_redraw= FALSE;
	if (vis != ovis) {
		if (vis == PATH_VISIBLE)
			path_redraw= TRUE;
		else
			page_redraw= TRUE;
	}
	if (vis == PATH_VISIBLE) {
		if ((orfg != rfg) || (ogfg != gfg) || (obfg != bfg))
			path_redraw= TRUE;
		if (cl != ocl) {
			if (cl == PATH_CLOSED)
				path_redraw= TRUE;
			else
				page_redraw= TRUE;
		}
	}
	if (page_redraw)
		pg_draw();
	else if (path_redraw) {
		attr_pth->x11draw(pg_dpi, pg_pixheight, pg_cpm);
		canvas_flush(pg_canvas);
	}
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
pthattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, pthattr_frame);
	ice_op= MAIN_MENU;
	return;
}
