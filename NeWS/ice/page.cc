/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <NeWS/psio.h>

#if defined(sun3)
#include "icecps-sun3.h"
#elif defined(sun4)
#include "icecps-sun4.h"
#endif

#include "ice_defines.h"
#include "ice_externs.h"
#include "Psdoc.h"
#include "Raster.h"
#include "Intobj.h"
#include "Vector.h"
#include "Curve.h"
#include "Axis.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			pprintf( ... );
int			ps_checkfor(PSFILE *, int, int);
int			psio_flushbuf(char, PSFILE *);
int			psio_write(char *, int, int, PSFILE *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		ice_err(char *, int);
extern void		pg_erasecoords();

boolean pgcoords_drawn;
int pgcoords_x, pgcoords_y;
int pgcoords_w, pgcoords_h;

void
pg_draw()
{
	int flags;
	Grobj *gr;
	Path *pth;
	char errmsg[MAX_ERRMSGLEN+1];
	void pg_draworiginhlt(boolean);

	if ((pg_update == PG_MANUAL) && (pg_forceupdate == FALSE))
		return;

	XFillRectangle(dpy, pg_cpm, pg_bggc, 0, 0, pg_pixwidth, pg_pixheight);
	canvas_flush(pg_canvas);
	XSync(dpy, False);

	news_setcanvas((int) pg_cpm, pg_dpi);

	if (pg_clip != (Path *) 0) {
		flags= PATH_DRAWCLOSE | PATH_DRAWCLIP;
		pg_clip->draw(flags, (FILE *) NULL);
	}

	/* make sure that object list is properly sorted by
	   sequence, object type and dump transparency key */
	if (grobjs != (Grobj *) NULL)
		grobjs->sortsequence(&grobjs);

	for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
		XSync(dpy, False);

		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			if (((Psdoc *) gr)->draw((FILE *) NULL) != GROBJ_SUCCESS) {
				(void) sprintf(errmsg, "Error rendering '%s'.", ((Psdoc *) gr)->getname());
				ice_err(errmsg, NONFATAL);
			}
			break;
		case GROBJ_RASTER:
			((Raster *) gr)->copy(pg_cpm);
			break;
		case GROBJ_INTOBJ:
			if (((Intobj *) gr)->draw((FILE *) NULL) != GROBJ_SUCCESS) {
				(void) sprintf(errmsg, "Error rendering '%s'.", ((Intobj *) gr)->getname());
				ice_err(errmsg, NONFATAL);
			}
			break;
		}
	}

	XSync(dpy, False);

	for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
		if (pth->getvisibility() == PATH_VISIBLE)
			pth->x11draw(pg_dpi, pg_pixheight, pg_cpm);
	}

	if (pg_originhlt == PG_ORIGINHLT)
		pg_draworiginhlt((boolean) FALSE);

	canvas_flush(pg_canvas);
	return;
}

void
pg_draworiginhlt(boolean winflush)
{
	Grobj *gr;
	float fx, fy;
	int vx, vy, ix, iy;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
		gr->getloc(&fx, &fy, &ix, &iy);
		iy= pg_pixheight-1-iy;
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-4, iy-4, ix+4, iy-4);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix+4, iy-3, ix+4, iy+3);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-4, iy+4, ix+4, iy+4);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-4, iy-3, ix-4, iy+3);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-5, iy-5, ix+5, iy-5);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix+5, iy-4, ix+5, iy+4);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-5, iy+5, ix+5, iy+5);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-5, iy-4, ix-5, iy+4);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-6, iy-6, ix+6, iy-6);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix+6, iy-5, ix+6, iy+5);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-6, iy+6, ix+6, iy+6);
		XDrawLine(dpy, pg_cpm, pg_xgc, ix-6, iy-5, ix-6, iy+5);
		if (winflush) {
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-4-vx, iy-4-vy, ix+4-vx, iy-4-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix+4-vx, iy-3-vy, ix+4-vx, iy+3-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-4-vx, iy+4-vy, ix+4-vx, iy+4-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-4-vx, iy-3-vy, ix-4-vx, iy+3-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-5-vx, iy-5-vy, ix+5-vx, iy-5-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix+5-vx, iy-4-vy, ix+5-vx, iy+4-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-5-vx, iy+5-vy, ix+5-vx, iy+5-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-5-vx, iy-4-vy, ix-5-vx, iy+4-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-6-vx, iy-6-vy, ix+6-vx, iy-6-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix+6-vx, iy-5-vy, ix+6-vx, iy+5-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-6-vx, iy+6-vy, ix+6-vx, iy+6-vy);
			XDrawLine(dpy, pg_cwin, pg_xgc, ix-6-vx, iy-5-vy, ix-6-vx, iy+5-vy);
		}
	}

	if (winflush)
		XSync(dpy, False);

	return;
}

void
pg_showcoords(int px, int py, int w, int h, float fx, float fy, int ix, int iy)
{
	char buf[80];
	float ux, uy;

	if (pg_locdisplay == PG_NODISPLAYLOC)
		return;

	if (pgcoords_drawn)
		pg_erasecoords();

	switch (pg_units) {
	case PG_PIXELS:
		(void) sprintf(buf, " (%1d, %1d) ", ix, iy);
		break;
	case PG_POINTS:
		(void) sprintf(buf, " (%4.2f, %4.2f) ", fx*72., fy*72.);
		break;
	case PG_INCHES:
		(void) sprintf(buf, " (%4.2f, %4.2f) ", fx, fy);
		break;
	case PG_USER:
		ux= (float) ((fx-pg_xri)*pg_hsi)+pg_xru;
		uy= (float) ((fy-pg_yri)*pg_vsi)+pg_yru;
		(void) sprintf(buf, " (%4.2f, %4.2f) ", ux, uy);
		break;
	}

	pgcoords_x= px;
	pgcoords_y= py;
	pgcoords_w= XTextWidth(font, buf, strlen(buf));
	if ((w >= 0) && (pgcoords_w > w))
		return;
	pgcoords_h= font->max_bounds.ascent+12;
	if ((h >= 0) && (pgcoords_h > h))
		return;

	XClearArea(dpy, pg_cwin, pgcoords_x, pgcoords_y, pgcoords_w, pgcoords_h, False);
	XDrawString(dpy, pg_cwin, pg_gc, pgcoords_x, pgcoords_y+font->max_bounds.ascent+4, buf, strlen(buf));
	XDrawLine(dpy, pg_cwin, pg_gc, pgcoords_x+pgcoords_w-1, pgcoords_y, pgcoords_x+pgcoords_w-1, pgcoords_y+pgcoords_h-1);
	XDrawLine(dpy, pg_cwin, pg_gc, pgcoords_x, pgcoords_y+pgcoords_h-1, pgcoords_x+pgcoords_w-1, pgcoords_y+pgcoords_h-1);
	pgcoords_drawn= TRUE;
	return;
}

void
pg_erasecoords()
{
	int vx, vy;

	if (pg_locdisplay == PG_NODISPLAYLOC)
		return;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	/* just in case the coordinates have been drawn into an area
	   of the window not corresponding to any canvas backing store */
	XClearArea(dpy, pg_cwin, pgcoords_x, pgcoords_y, pgcoords_w, pgcoords_h, False);

	XCopyArea(dpy, pg_cpm, pg_cwin, pg_gc, pgcoords_x+vx, pgcoords_y+vy, pgcoords_w, pgcoords_h, pgcoords_x, pgcoords_y);
	pgcoords_drawn= FALSE;
	return;
}

void
pg_p2fcoords(int x, int y, float *fx, float *fy)
{
	float ipp;

	ipp= 1./pg_dpi;
	*fx= ipp*((float) x);
	*fy= ipp*((float) (pg_pixheight-1-y));
	return;
}

void
pg_f2pcoords(float x, float y, int *px, int *py)
{
	*px= (int) (pg_dpi*x);
	*py= pg_pixheight-1-((int) (pg_dpi*y));
	return;
}

void
attrpg_proc(Menu *m, Menu_item *mi)
{
	char buf[80];
	int state;

	(void) gconvert(pg_width, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_w, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert(pg_height, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_h, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) gconvert((double) pg_dpi, 10, 0, buf);

	panelitem_set(pgattr_panel, pgattr_dpi, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(pgattr_panel, pgattr_update, LXPENUM_VALUE, pg_update, LXPI_NULL);

	panelitem_set(pgattr_panel, pgattr_loc, LXPENUM_VALUE, pg_loc, LXPI_NULL);

	if (pg_loc == PG_CURSORLOC)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	panelitem_set(pgattr_panel, pgattr_locdisplay, LXPENUM_VALUE, pg_locdisplay, LXPI_STATE, state, LXPI_NULL);

	panelitem_set(pgattr_panel, pgattr_units, LXPENUM_VALUE, pg_units, LXPI_NULL);
	if (pg_units == PG_USER)
		state= LXPI_ACTIVE;
	else
		state= LXPI_INACTIVE;
	(void) gconvert(pg_hsi, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_hsi, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert(pg_vsi, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_vsi, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert(pg_xri, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_xri, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert(pg_yri, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_yri, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert(pg_xru, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_xru, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);
	(void) gconvert(pg_yru, 10, 0, buf);
	panelitem_set(pgattr_panel, pgattr_yru, LXPTEXT_VALUE, buf, LXPI_STATE, state, LXPI_NULL);

	if (pg_clip == (Path *) NULL)
		panelitem_set(pgattr_panel, pgattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);
	else
		panelitem_set(pgattr_panel, pgattr_clip, LXPTEXT_VALUE, pg_clip->getname(), LXPI_NULL);

	panelitem_set(pgattr_panel, pgattr_originhlt, LXPENUM_VALUE, pg_originhlt, LXPI_NULL);

	panelitem_set(pgattr_panel, pgattr_bg, LXPENUM_VALUE, pg_bg, LXPI_NULL);
	switch (pg_bg) {
	case PG_WHITEBG:
	case PG_BLACKBG:
		(void) sprintf(buf, "%1d", (int) pg_rbg);
		panelitem_set(pgattr_panel, pgattr_rbg, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) pg_gbg);
		panelitem_set(pgattr_panel, pgattr_gbg, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) pg_bbg);
		panelitem_set(pgattr_panel, pgattr_bbg, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case PG_OTHERBG:
		(void) sprintf(buf, "%1d", (int) pg_rbg);
		panelitem_set(pgattr_panel, pgattr_rbg, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) pg_gbg);
		panelitem_set(pgattr_panel, pgattr_gbg, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		(void) sprintf(buf, "%1d", (int) pg_bbg);
		panelitem_set(pgattr_panel, pgattr_bbg, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}

	ice_op= PAGE_ATTR;
	XMapRaised(dpy, pgattr_frame);
	return;
}

void
pgattrloc_proc(Panel *p, Panel_item *pi)
{
	int loc;

	loc= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (loc) {
	case PG_CURSORLOC:
		panelitem_set(p, pgattr_locdisplay, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	case PG_TEXTLOC:
		panelitem_set(p, pgattr_locdisplay, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
pgattrunits_proc(Panel *p, Panel_item *pi)
{
	int units;

	units= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (units) {
	case PG_PIXELS:
	case PG_POINTS:
	case PG_INCHES:
		panelitem_set(p, pgattr_hsi, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_vsi, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_xri, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_yri, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_xru, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_yru, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case PG_USER:
		panelitem_set(p, pgattr_hsi, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_vsi, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_xri, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_yri, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_xru, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_yru, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
pgattrbg_proc(Panel *p, Panel_item *pi)
{
	int bg;

	bg= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch (bg) {
	case PG_WHITEBG:
	case PG_BLACKBG:
		panelitem_set(p, pgattr_rbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_gbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_bbg, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		break;
	case PG_OTHERBG:
		panelitem_set(p, pgattr_rbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_gbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		panelitem_set(p, pgattr_bbg, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
pgattrcont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *buf;
	double w, h, hsi, vsi, xri, yri, xru, yru;
	int dpi, pw, ph, update, loc, locdisplay, units, ohlt, bg;
	unsigned char rbg, gbg, bbg;
	XGCValues gcv;
	boolean rescale, resize, redraw;
	Grobj *g;
	Path *pth;

	XUnmapWindow(dpy, pgattr_frame);
	ice_op= MAIN_MENU;

	rescale= resize= redraw= FALSE;

	buf= (char *) panelitem_get(pgattr_panel, pgattr_w, LXPTEXT_VALUE);
	w= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid width value.", NONFATAL);
		return;
	}
	if (w <= 0.) {
		ice_err("Width must be greater than 0.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(pgattr_panel, pgattr_h, LXPTEXT_VALUE);
	h= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid height value.", NONFATAL);
		return;
	}
	if (h <= 0.) {
		ice_err("Height must be greater than 0.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(pgattr_panel, pgattr_dpi, LXPTEXT_VALUE);
	dpi= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid DPI value.", NONFATAL);
		return;
	}
	if (dpi <= 0) {
		ice_err("Resolution must be greater than 0 DPI.", NONFATAL);
		return;
	}

	update= *((int *) panelitem_get(pgattr_panel, pgattr_update, LXPENUM_VALUE));

	loc= *((int *) panelitem_get(pgattr_panel, pgattr_loc, LXPENUM_VALUE));
	switch (loc) {
	case PG_CURSORLOC:
		locdisplay= *((int *) panelitem_get(pgattr_panel, pgattr_locdisplay, LXPENUM_VALUE));
		break;
	case PG_TEXTLOC:
		locdisplay= PG_DISPLAYLOC;
		break;
	}

	units= *((int *) panelitem_get(pgattr_panel, pgattr_units, LXPENUM_VALUE));
	switch (units) {
	case PG_PIXELS:
	case PG_POINTS:
	case PG_INCHES:
		break;
	case PG_USER:
		buf= (char *) panelitem_get(pgattr_panel, pgattr_hsi, LXPTEXT_VALUE);
		hsi= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid horizontal scale value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(pgattr_panel, pgattr_vsi, LXPTEXT_VALUE);
		vsi= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid vertical scale value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(pgattr_panel, pgattr_xri, LXPTEXT_VALUE);
		xri= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid X reference value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(pgattr_panel, pgattr_yri, LXPTEXT_VALUE);
		yri= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid Y reference value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(pgattr_panel, pgattr_xru, LXPTEXT_VALUE);
		xru= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid X reference value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(pgattr_panel, pgattr_yru, LXPTEXT_VALUE);
		yru= strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid Y reference value.", NONFATAL);
			return;
		}
		break;
	}

	buf= (char *) panelitem_get(pgattr_panel, pgattr_clip, LXPTEXT_VALUE);
	if (strlen(buf) == 0)
		pth= (Path *) NULL;
	else {
		for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
			if (!strcmp(buf, pth->getname()))
				break;
		}
		if (pth == (Path *) NULL) {
			ice_err("Specified path does not exist.", NONFATAL);
			return;
		}
	}

	ohlt= *((int *) panelitem_get(pgattr_panel, pgattr_originhlt, LXPENUM_VALUE));

	bg= *((int *) panelitem_get(pgattr_panel, pgattr_bg, LXPENUM_VALUE));
	switch (bg) {
	case PG_WHITEBG:
		rbg= bbg= gbg= (unsigned char) 255;
		break;
	case PG_BLACKBG:
		rbg= bbg= gbg= (unsigned char) 0;
		break;
	case PG_OTHERBG:
		buf= (char *) panelitem_get(pgattr_panel, pgattr_rbg, LXPTEXT_VALUE);
		rbg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red background value.", NONFATAL);
			return;
		}
		if ((rbg < 0) || (rbg > 255)) {
			ice_err("Invalid red background value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(pgattr_panel, pgattr_gbg, LXPTEXT_VALUE);
		gbg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green background value.", NONFATAL);
			return;
		}
		if ((gbg < 0) || (gbg > 255)) {
			ice_err("Invalid green background value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(pgattr_panel, pgattr_bbg, LXPTEXT_VALUE);
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

	pg_update= update;
	pg_loc= loc;
	pg_locdisplay= locdisplay;

	pg_units= units;
	switch (units) {
	case PG_PIXELS:
	case PG_POINTS:
	case PG_INCHES:
		break;
	case PG_USER:
		pg_hsi= hsi;
		pg_vsi= vsi;
		pg_xri= xri;
		pg_yri= yri;
		pg_xru= xru;
		pg_yru= yru;
		break;
	}

	if (pg_clip != pth) {
		if (pg_clip != (Path *) NULL)
			(void) pg_clip->setreferences(PATH_REFDECR);
		if (pth != (Path *) NULL)
			(void) pth->setreferences(PATH_REFINCR);
		pg_clip= pth;
		redraw= TRUE;
	}

	pg_bg= bg;
	if ((rbg != pg_rbg) || (gbg != pg_gbg) || (bbg != pg_bbg)) {
		pg_rbg= rbg;
		pg_gbg= gbg;
		pg_bbg= bbg;
		if (pg_pixdepth == 1)
			pg_bgpixel= cmap_lookup(pg_rbg, pg_gbg, pg_bbg, 2);
		else
			pg_bgpixel= cmap_lookup(pg_rbg, pg_gbg, pg_bbg, PSEUDOCOLOR_MAPSZ);
		if (pg_bggc != None)
			XFreeGC(dpy, pg_bggc);
		gcv.function= GXcopy;
		gcv.foreground= pg_bgpixel;
		gcv.plane_mask= AllPlanes;
		pg_bggc= XCreateGC(dpy, pg_cwin, (GCFunction | GCForeground | GCPlaneMask), &gcv);
		redraw= TRUE;
	}

	if (dpi != pg_dpi)
		rescale= TRUE;
	if ((w != pg_width) || (h != pg_height) || (rescale))
		resize= TRUE;

	if (resize)
		redraw= TRUE;
	if (pg_update == PG_MANUAL)
		redraw= FALSE;

	if (redraw) {
		XFillRectangle(dpy, pg_cpm, pg_bggc, 0, 0, pg_pixwidth, pg_pixheight);
		canvas_flush(pg_canvas);
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}

	if (resize) {
		pw= (int) (w*((double) dpi));
		ph= (int) (h*((double) dpi));
		if (canvas_set(pg_canvas,
				LXC_WIDTH, pw,
				LXC_HEIGHT, ph,
				LXC_NULL) != LX_SUCCESS) {
			ice_err("Canvas resize error.", NONFATAL);
			XFillRectangle(dpy, pg_cpm, pg_bggc, 0, 0, pg_pixwidth, pg_pixheight);
			canvas_flush(pg_canvas);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			return;
		}

		pg_width= w;
		pg_height= h;
		pg_dpi= dpi;
		pg_pixwidth= pw;
		pg_pixheight= ph;
		pg_cwin= *((Window *) canvas_get(pg_canvas, LXC_WINDOW));
		pg_cpm= *((Pixmap *) canvas_get(pg_canvas, LXC_PIXMAP));
	}

	if (rescale) {
		float fx, fy;
		int ix, iy;

		for (g= grobjs; g != (Grobj *) NULL; g= (Grobj *) g->succ()) {
			g->getloc(&fx, &fy, &ix, &iy);
			g->setfloc(fx, fy, (float) pg_dpi);
			switch (g->gettype()) {
			case GROBJ_INTOBJ:
				switch (((Intobj *) g)->getintobjtype()) {
				case INTOBJ_VECTOR:
					((Vector *) g)->getend(&fx, &fy, &ix, &iy);
					((Vector *) g)->setfend(fx, fy, (float) pg_dpi);
					break;
				case INTOBJ_CURVE:
					((Curve *) g)->getend(&fx, &fy, &ix, &iy);
					((Curve *) g)->setfend(fx, fy, (float) pg_dpi);
					((Curve *) g)->getcontrol1(&fx, &fy, &ix, &iy);
					((Curve *) g)->setfcontrol1(fx, fy, (float) pg_dpi);
					((Curve *) g)->getcontrol2(&fx, &fy, &ix, &iy);
					((Curve *) g)->setfcontrol2(fx, fy, (float) pg_dpi);
					break;
				case INTOBJ_AXIS:
					((Axis *) g)->getend(&fx, &fy, &ix, &iy);
					((Axis *) g)->setfend(fx, fy, (float) pg_dpi);
					break;
				}
				break;
			}
		}
	}

	if (!redraw && (ohlt != pg_originhlt))
		pg_draworiginhlt((boolean) TRUE);
	pg_originhlt= ohlt;

	if (redraw) {
		pg_draw();
		XSync(dpy, False);
		XDefineCursor(dpy, pg_cwin, std_cursor);
	}

	return;
}

void
pgattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, pgattr_frame);
	ice_op= MAIN_MENU;
	return;
}

void
pgdsp_proc(Menu *m, Menu_item *mi)
{
	pg_forceupdate= TRUE;
	pg_draw();
	pg_forceupdate= FALSE;
	return;
}
