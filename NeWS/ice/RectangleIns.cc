/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <math.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Rectangle.h"
#include "Pathdup.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrrect_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cprect_proc(Menu *, Menu_item *);
extern void		delrect_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_p2fcoords(int, int, float *, float *);
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		trrect_proc(Menu *, Menu_item *);
extern void		rect_del(Rectangle *);
extern int		str2flt(char *, float **, int *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

int rect_dimmode;

static Rectangle *new_rect;
static boolean rect_drawn;
static int rect_x[4], rect_y[4];
static float rect_fx[4], rect_fy[4];
static int rect_rx, rect_ry;
static float rect_rfx, rect_rfy;
static double rect_rot;

void
insrect_proc(Menu *m, Menu_item *mi)
{
	char buf[80];

	panelitem_set(rectattr_panel, rectattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_dimmode, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		ice_op= RECT_INSERTATTR;
		XMapRaised(dpy, rectattr_frame);
		return;
	}

	panelitem_set(rectattr_panel, rectattr_bndmode, LXPENUM_VALUE, RECTANGLE_OPAQUEBNDM, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_bndwd, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, RECTANGLE_GLOBALBNDWIDTH, LXPI_NULL);
	(void) gconvert((double) gdf_bndwidth, 10, 0, buf);
	panelitem_set(rectattr_panel, rectattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_linestyle, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, RECTANGLE_SOLID, LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_dashstyle, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, RECTANGLE_SIMPLEDASH, LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_dashlen, LXPTEXT_VALUE, "4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_dashgaplen, LXPTEXT_VALUE, "4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_dashpattern, LXPTEXT_VALUE, "4 4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_dashoffset, LXPTEXT_VALUE, "0", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_bndcolor, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, RECTANGLE_GLOBALBND, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rbnd);
	panelitem_set(rectattr_panel, rectattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gbnd);
	panelitem_set(rectattr_panel, rectattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bbnd);
	panelitem_set(rectattr_panel, rectattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_fillmode, LXPENUM_VALUE, RECTANGLE_TRANSPARENTFILLM, LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_fillcolor, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, RECTANGLE_GLOBALFILL, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfill);
	panelitem_set(rectattr_panel, rectattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfill);
	panelitem_set(rectattr_panel, rectattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfill);
	panelitem_set(rectattr_panel, rectattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_dimmode, LXPENUM_VALUE, RECT_CURSORDIM, LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_width, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "", LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_height, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "", LXPI_NULL);
	panelitem_set(rectattr_panel, rectattr_rot, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "0", LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	panelitem_set(rectattr_panel, rectattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	panelitem_set(rectattr_panel, rectattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	panelitem_set(rectattr_panel, rectattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(rectattr_panel, rectattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= RECT_INSERTATTR;
	XMapRaised(dpy, rectattr_frame);
	return;
}

void
rectinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *name, *buf;
	float width, height, bndwidth, dl, dgl, *dp, dpo, rot;
	int bndmode, bndwd, ls, ds, dpl, bnd, fillmode, fill, dtk, seq;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char nmbuf[30];
	float fx, fy;
	char xbuf[30], ybuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	XUnmapWindow(dpy, rectattr_frame);
	ice_op= MAIN_MENU;

	dp= (float *) NULL;

	name= (char *) panelitem_get(rectattr_panel, rectattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		(void) sprintf(nmbuf, "UnnamedRectangle-%d", unnamed_rectangles++);
		name= nmbuf;
	}

	bndmode= *((int *) panelitem_get(rectattr_panel, rectattr_bndmode, LXPENUM_VALUE));
	switch (bndmode) {
	case RECTANGLE_OPAQUEBNDM:
		bndwd= *((int *) panelitem_get(rectattr_panel, rectattr_bndwd, LXPENUM_VALUE));
		switch (bndwd) {
		case RECTANGLE_GLOBALBNDWIDTH:
			bndwidth= gdf_bndwidth;
			break;
		case RECTANGLE_OTHERBNDWIDTH:
			buf= (char *) panelitem_get(rectattr_panel, rectattr_bwidth, LXPTEXT_VALUE);
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

		ls= *((int *) panelitem_get(rectattr_panel, rectattr_linestyle, LXPENUM_VALUE));
		switch (ls) {
		case RECTANGLE_SOLID:
			if (str2flt("4 4", &dp, &dpl) != GROBJ_SUCCESS) {
				ice_err("Cannot convert dash pattern.", NONFATAL);
				delete dp;
				return;
			}
			dpo= 0.;
			ds= RECTANGLE_SIMPLEDASH;
			break;
		case RECTANGLE_DASHED:
			ds= *((int *) panelitem_get(rectattr_panel, rectattr_dashstyle, LXPENUM_VALUE));
			switch (ds) {
			case RECTANGLE_SIMPLEDASH:
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashlen, LXPTEXT_VALUE);
				dl= (float) strtod(buf, &cptr);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid dash length value.", NONFATAL);
					delete dp;
					return;
				}
				if (dl < 0.) {
					ice_err("Dash length value must be non-negative.", NONFATAL);
					delete dp;
					return;
				}
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashgaplen, LXPTEXT_VALUE);
				dgl= (float) strtod(buf, &cptr);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid dash gap length value.", NONFATAL);
					delete dp;
					return;
				}
				if (dgl < 0.) {
					ice_err("Dash gap length value must be non-negative.", NONFATAL);
					delete dp;
					return;
				}
				if ((dp= new float[2]) == (float *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					return;
				}
				*dp= dl;
				*(dp+1)= dgl;
				dpl= 2;
				dpo= 0.;
				break;
			case RECTANGLE_COMPLEXDASH:
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashpattern, LXPTEXT_VALUE);
				if (str2flt(buf, &dp, &dpl) != GROBJ_SUCCESS) {
					ice_err("Cannot convert dash pattern.", NONFATAL);
					delete dp;
					return;
				}
				buf= (char *) panelitem_get(rectattr_panel, rectattr_dashoffset, LXPTEXT_VALUE);
				dpo= (float) strtod(buf, &cptr);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid dash pattern offset value.", NONFATAL);
					delete dp;
					return;
				}
				if (dpo < 0.) {
					ice_err("Dash pattern offset value must be non-negative.", NONFATAL);
					delete dp;
					return;
				}
				break;
			}
			break;
		}

		bnd= *((int *) panelitem_get(rectattr_panel, rectattr_bndcolor, LXPENUM_VALUE));
		switch (bnd) {
		case RECTANGLE_GLOBALBND:
			rbnd= gdf_rbnd;
			gbnd= gdf_gbnd;
			bbnd= gdf_bbnd;
			break;
		case RECTANGLE_BLACKBND:
			rbnd= gbnd= bbnd= 0;
			break;
		case RECTANGLE_WHITEBND:
			rbnd= gbnd= bbnd= 255;
			break;
		case RECTANGLE_OTHERBND:
			buf= (char *) panelitem_get(rectattr_panel, rectattr_rbnd, LXPTEXT_VALUE);
			rbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			if ((rbnd < 0) || (rbnd > 255)) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_gbnd, LXPTEXT_VALUE);
			gbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			if ((gbnd < 0) || (gbnd > 255)) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_bbnd, LXPTEXT_VALUE);
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
	case RECTANGLE_TRANSPARENTBNDM:
		bndwd= RECTANGLE_GLOBALBNDWIDTH;
		bndwidth= gdf_bndwidth;
		ls= RECTANGLE_SOLID;
		if (str2flt("4 4", &dp, &dpl) != GROBJ_SUCCESS) {
			ice_err("Cannot convert dash pattern.", NONFATAL);
			delete dp;
			return;
		}
		dpo= 0.;
		ds= RECTANGLE_SIMPLEDASH;
		bnd= RECTANGLE_GLOBALBND;
		rbnd= gdf_rbnd;
		gbnd= gdf_gbnd;
		bbnd= gdf_bbnd;
		break;
	}

	fillmode= *((int *) panelitem_get(rectattr_panel, rectattr_fillmode, LXPENUM_VALUE));
	switch (fillmode) {
	case RECTANGLE_TRANSPARENTFILLM:
		fill= RECTANGLE_GLOBALFILL;
		rfill= gdf_rfill;
		gfill= gdf_gfill;
		bfill= gdf_bfill;
		break;
	case RECTANGLE_OPAQUEFILLM:
		fill= *((int *) panelitem_get(rectattr_panel, rectattr_fillcolor, LXPENUM_VALUE));
		switch (fill) {
		case RECTANGLE_GLOBALFILL:
			rfill= gdf_rfill;
			gfill= gdf_gfill;
			bfill= gdf_bfill;
			break;
		case RECTANGLE_WHITEFILL:
			rfill= gfill= bfill= 255;
			break;
		case RECTANGLE_BLACKFILL:
			rfill= gfill= bfill= 0;
			break;
		case RECTANGLE_OTHERFILL:
			buf= (char *) panelitem_get(rectattr_panel, rectattr_rfill, LXPTEXT_VALUE);
			rfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red fill color value.", NONFATAL);
				return;
			}
			if ((rfill < 0) || (rfill > 255)) {
				ice_err("Invalid red fill color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_gfill, LXPTEXT_VALUE);
			gfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green fill color value.", NONFATAL);
				return;
			}
			if ((gfill < 0) || (gfill > 255)) {
				ice_err("Invalid green fill color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(rectattr_panel, rectattr_bfill, LXPTEXT_VALUE);
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

	rect_dimmode= *((int *) panelitem_get(rectattr_panel, rectattr_dimmode, LXPENUM_VALUE));
	if (rect_dimmode == RECT_TEXTDIM) {
		buf= (char *) panelitem_get(rectattr_panel, rectattr_width, LXPTEXT_VALUE);
		width= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid width value.", NONFATAL);
			return;
		}
		if (width < 0.) {
			ice_err("Rectangle width may not be less than 0.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(rectattr_panel, rectattr_height, LXPTEXT_VALUE);
		height= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid height value.", NONFATAL);
			return;
		}
		if (height < 0.) {
			ice_err("Rectangle height may not be less than 0.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(rectattr_panel, rectattr_rot, LXPTEXT_VALUE);
		rot= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid rotation value.", NONFATAL);
			return;
		}
	}

	pathnm= (char *) panelitem_get(rectattr_panel, rectattr_clip, LXPTEXT_VALUE);
	if (strlen(pathnm) == 0)
		pth= (Path *) NULL;
	else {
		for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
			if (!strcmp(pathnm, pth->getname()))
				break;
		}
		if (pth == (Path *) NULL) {
			ice_err("Specified path does not exist.", NONFATAL);
			delete dp;
			return;
		}
	}

	dtk= *((int *) panelitem_get(rectattr_panel, rectattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(rectattr_panel, rectattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(rectattr_panel, rectattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			delete dp;
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(rectattr_panel, rectattr_bdtk, LXPTEXT_VALUE);
		bdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		if ((bdtk < 0) || (bdtk > 255)) {
			ice_err("Invalid blue dump transparency key value.", NONFATAL);
			delete dp;
			return;
		}
		break;
	}

	buf= (char *) panelitem_get(rectattr_panel, rectattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		delete dp;
		return;
	}

	if ((new_rect= new Rectangle((Dlnk **) &grobjs, name, seq)) == (Rectangle *) NULL) {
		ice_err("Cannot create rectangle object.", NONFATAL);
		delete dp;
		return;
	}
	nrectangles++;
	(void) new_rect->setboundary(bndmode, bnd, rbnd, gbnd, bbnd);
	(void) new_rect->setwidth(bndwd, bndwidth);
	(void) new_rect->setlinestyle(ls);
	(void) new_rect->setdashstyle(ds, dp, dpl, dpo);
	(void) new_rect->setfill(fillmode, fill, rfill, gfill, bfill);
	if (rect_dimmode == RECT_TEXTDIM) {
		(void) new_rect->setsize(width, height);
		new_rect->setrotation(rot);
	}
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_rect->setclip(pth);

	(void) new_rect->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_rect->setdtkpix(dtkpix);

	ice_op= RECT_INSERTLOC;

	if (pg_loc == PG_CURSORLOC) {
		pgcoords_drawn= crosshairs_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_rect,
				LXMI_PROC, delrect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(new_rect);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_rect,
				LXMI_PROC, attrrect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(new_rect);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_rect,
				LXMI_PROC, trrect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(new_rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_rect,
				LXMI_PROC, cprect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(new_rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_rect,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(new_rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_rect,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(new_rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		return;
	}
	(void) menuitem_insert(delrect_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrrect_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trrect_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cprect_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmprect_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoprect_menu, cmpopitem);

	loc_grobj= (Grobj *) new_rect;
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
}

void
insicerect_rd(FILE *fp, int gdf, int newobj)
{
	char *name, *c;
	float fx, fy, width, height;
	float bndwidth, dl, dgl, *dp, dpo, rot;
	int bndmode, bndwd, ls, ds, dpl, dplx;
	int len, bnd, fillmode, fill, dtk, seq;
	int ir, ig, ib;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	boolean endfound;
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	int iot;
	char nmbuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Rectangle *rect;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Rect: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Rect: Begin ")+len-1)= '\0';
	if ((name= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(name, ice_iobuf+strlen("%%ICE-Rect: Begin "));
	if (!strncmp(name, "UnnamedRectangle-", strlen("UnnamedRectangle-"))) {
		delete name;
		(void) sprintf(nmbuf, "UnnamedRectangle-%d", unnamed_rectangles++);
		if ((name= new char[strlen(nmbuf)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(name, nmbuf);
	}

	fx= fy= 0.;
	width= height= 0.;
	rot= 0.;
	bndmode= RECTANGLE_OPAQUEBNDM;
	bndwd= RECTANGLE_GLOBALBNDWIDTH;
	bndwidth= gdf_bndwidth;
	ls= RECTANGLE_SOLID;
	ds= RECTANGLE_SIMPLEDASH;
	dl= dgl= 4.;
	if (str2flt("4 4", &dp, &dpl) != GROBJ_SUCCESS) {
		ice_err("Cannot convert dash pattern.", NONFATAL);
		delete name;
		return;
	}
	dpo= 0.;
	bnd= RECTANGLE_GLOBALBND;
	rbnd= gdf_rbnd;
	gbnd= gdf_gbnd;
	bbnd= gdf_bbnd;
	fillmode= RECTANGLE_TRANSPARENTFILLM;
	fill= RECTANGLE_GLOBALFILL;
	rfill= gdf_rfill;
	gfill= gdf_gfill;
	bfill= gdf_bfill;
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

		if (!strncmp("%%ICE-Rect: Loc ", ice_iobuf, strlen("%%ICE-Rect: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: Loc "), "%f %f %f %f", &fx, &fy, &width, &height) != 4) {
				ice_err("Invalid rectangle location value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((width < 0.) || (height < 0.)) {
				ice_err("Invalid rectangle dimensions.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Rect: Rot ", ice_iobuf, strlen("%%ICE-Rect: Rot "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: Rot "), "%f", &rot) != 1) {
				ice_err("Invalid rectangle rotation value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Rect: Bnd ", ice_iobuf, strlen("%%ICE-Rect: Bnd "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: Bnd "), "%d %d %d %d %d", &bndmode, &bnd, &ir, &ig, &ib) != 5) {
				ice_err("Invalid rectangle boundary value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((bndmode != RECTANGLE_OPAQUEBNDM) &&
			    (bndmode != RECTANGLE_TRANSPARENTBNDM)) {
				ice_err("Invalid rectangle boundary mode value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((bnd != RECTANGLE_GLOBALBND) &&
			    (bnd != RECTANGLE_BLACKBND) &&
			    (bnd != RECTANGLE_WHITEBND) &&
			    (bnd != RECTANGLE_OTHERBND)) {
				ice_err("Invalid rectangle boundary value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid rectangle red boundary value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid rectangle green boundary value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid rectangle blue boundary value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			rbnd= (unsigned char) ir;
			gbnd= (unsigned char) ig;
			bbnd= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Rect: Line ", ice_iobuf, strlen("%%ICE-Rect: Line "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: Line "), "%d %f %d", &bndwd, &bndwidth, &ls) != 3) {
				ice_err("Invalid rectangle line value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((bndwd != RECTANGLE_GLOBALBNDWIDTH) &&
			    (bndwd != RECTANGLE_OTHERBNDWIDTH)) {
				ice_err("Invalid rectangle line width value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (bndwidth < 0.) {
				ice_err("Invalid rectangle line width value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ls != RECTANGLE_SOLID) &&
			    (ls != RECTANGLE_DASHED)) {
				ice_err("Invalid rectangle line style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Rect: Dash ", ice_iobuf, strlen("%%ICE-Rect: Dash "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: Dash "), "%d %f %d", &ds, &dpo, &dpl) != 3) {
				ice_err("Invalid rectangle dash value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ds != RECTANGLE_SIMPLEDASH) &&
			    (ds != RECTANGLE_COMPLEXDASH)) {
				ice_err("Invalid rectangle dash style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (dpo < 0.) {
				ice_err("Invalid rectangle dash pattern offset value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (dpl <= 0) {
				ice_err("Invalid dash pattern length value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			delete dp;
			c= ice_iobuf+strlen("%%ICE-Rect: Dash ");
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			for ( ; isgraph(*c); c++);
			for ( ; isspace(*c); c++);
			*(c+strlen(c)-1)= '\0';
			if (str2flt(c, &dp, &dplx) != GROBJ_SUCCESS) {
				ice_err("Cannot convert dash pattern.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (dplx != dpl) {
				ice_err("Cannot read dash pattern.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Rect: Fill ", ice_iobuf, strlen("%%ICE-Rect: Fill "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: Fill "), "%d %d %d %d %d", &fillmode, &fill, &ir, &ig, &ib) != 5) {
				ice_err("Invalid rectangle fill value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((fillmode != RECTANGLE_TRANSPARENTFILLM) &&
			    (fillmode != RECTANGLE_OPAQUEFILLM)) {
				ice_err("Invalid rectangle fill mode value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((fill != RECTANGLE_GLOBALFILL) &&
			    (fill != RECTANGLE_WHITEFILL) &&
			    (fill != RECTANGLE_BLACKFILL) &&
			    (fill != RECTANGLE_OTHERFILL)) {
				ice_err("Invalid rectangle fill value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid rectangle red fill value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid rectangle green fill value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid rectangle blue fill value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			rfill= (unsigned char) ir;
			gfill= (unsigned char) ig;
			bfill= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Rect: Clip ", ice_iobuf, strlen("%%ICE-Rect: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Rect: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete dp;
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strncmp("%%ICE-Rect: DTK ", ice_iobuf, strlen("%%ICE-Rect: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid rectangle DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid rectangle DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid rectangle red DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid rectangle green DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid rectangle blue DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Rect: Seq ", ice_iobuf, strlen("%%ICE-Rect: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid rectangle sequence value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Rect: IOT ", ice_iobuf, strlen("%%ICE-Rect: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Rect: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid rectangle tag value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid rectangle tag value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Rect: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Rectangle end not found.", NONFATAL);
		delete name;
		delete dp;
		delete pathnm;
		return;
	}

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
					delete dp;
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
			delete dp;
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
			if (bndwd == RECTANGLE_GLOBALBNDWIDTH)
				bndwidth= gdf_linewidth;
			if (bnd == RECTANGLE_GLOBALBND) {
				rbnd= gdf_rbnd;
				gbnd= gdf_gbnd;
				bbnd= gdf_bbnd;
			}
			if (fill == RECTANGLE_GLOBALFILL) {
				rfill= gdf_rfill;
				gfill= gdf_gfill;
				bfill= gdf_bfill;
			}
			if (dtk == GROBJ_GLOBALDTK) {
				rdtk= gdf_rdtk;
				gdtk= gdf_gdtk;
				bdtk= gdf_bdtk;
			}
			break;
		case INSICE_NEWOBJNEWGDF:
			if (bndwd == RECTANGLE_GLOBALBNDWIDTH)
				bndwd= RECTANGLE_OTHERBNDWIDTH;
			if (bnd == RECTANGLE_GLOBALBND)
				bnd= RECTANGLE_OTHERBND;
			if (fill == RECTANGLE_GLOBALFILL)
				fill= RECTANGLE_OTHERFILL;
			if (dtk == GROBJ_GLOBALDTK)
				dtk= GROBJ_OTHERDTK;
			break;
		}
	}

	if ((rect= new Rectangle((Dlnk **) &grobjs, name, seq)) == (Rectangle *) NULL) {
		ice_err("Cannot create rectangle object.", NONFATAL);
		delete name;
		delete dp;
		return;
	}
	delete name;
	nrectangles++;

	(void) rect->setsize(width, height);
	(void) rect->setboundary(bndmode, bnd, rbnd, gbnd, bbnd);
	(void) rect->setwidth(bndwd, bndwidth);
	(void) rect->setlinestyle(ls);
	(void) rect->setdashstyle(ds, dp, dpl, dpo);
	(void) rect->setfill(fillmode, fill, rfill, gfill, bfill);
	rect->setfloc(fx, fy, (float) pg_dpi);
	rect->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	rect->setclip(pth);
	rect->setiotag(iot);

	(void) rect->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	rect->setdtkpix(dtkpix);

	rect->setfloc(fx, fy, (float) pg_dpi);

	if ((delitem= menuitem_create(LXMI_STRING, rect->getname(),
				LXMI_CLIENTDATA, (char *) rect,
				LXMI_PROC, delrect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(rect);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, rect->getname(),
				LXMI_CLIENTDATA, (char *) rect,
				LXMI_PROC, attrrect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(rect);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, rect->getname(),
				LXMI_CLIENTDATA, (char *) rect,
				LXMI_PROC, trrect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, rect->getname(),
				LXMI_CLIENTDATA, (char *) rect,
				LXMI_PROC, cprect_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, rect->getname(),
				LXMI_CLIENTDATA, (char *) rect,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, rect->getname(),
				LXMI_CLIENTDATA, (char *) rect,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		rect_del(rect);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	(void) menuitem_insert(delrect_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrrect_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trrect_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cprect_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmprect_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoprect_menu, cmpopitem);

	return;
}

void
rectinsloc_event(XEvent *event)
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
		rect_x[0]= bevt->x+vx;
		rect_y[0]= bevt->y+vy;
		rect_fx[0]= (float) rect_x[0];
		rect_fy[0]= (float) (pg_pixheight-1-rect_y[0]);
		new_rect->setploc(rect_x[0], pg_pixheight-1-rect_y[0], (float) pg_dpi);
		if (rect_dimmode == RECT_TEXTDIM) {
			XDefineCursor(dpy, pg_cwin, hg_cursor);
			XSync(dpy, False);
		}
		if ((delitem= menuitem_create(LXMI_STRING, new_rect->getname(),
					LXMI_CLIENTDATA, (char *) new_rect,
					LXMI_PROC, delrect_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			rect_del(new_rect);
			if (rect_dimmode == RECT_TEXTDIM) {
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
			}
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_rect->getname(),
					LXMI_CLIENTDATA, (char *) new_rect,
					LXMI_PROC, attrrect_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			rect_del(new_rect);
			menuitem_destroy(delitem);
			if (rect_dimmode == RECT_TEXTDIM) {
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
			}
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_rect->getname(),
					LXMI_CLIENTDATA, (char *) new_rect,
					LXMI_PROC, trrect_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			rect_del(new_rect);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			if (rect_dimmode == RECT_TEXTDIM) {
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
			}
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_rect->getname(),
					LXMI_CLIENTDATA, (char *) new_rect,
					LXMI_PROC, cprect_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			rect_del(new_rect);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			if (rect_dimmode == RECT_TEXTDIM) {
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
			}
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_rect->getname(),
					LXMI_CLIENTDATA, (char *) new_rect,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			rect_del(new_rect);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			menuitem_destroy(cpitem);
			if (rect_dimmode == RECT_TEXTDIM) {
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
			}
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_rect->getname(),
					LXMI_CLIENTDATA, (char *) new_rect,
					LXMI_PROC, cmpop_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			rect_del(new_rect);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			menuitem_destroy(cpitem);
			menuitem_destroy(dmpitem);
			if (rect_dimmode == RECT_TEXTDIM) {
				XDefineCursor(dpy, pg_cwin, std_cursor);
				XSync(dpy, False);
			}
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		(void) menuitem_insert(delrect_menu, delitem);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrrect_menu, attritem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trrect_menu, tritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cprect_menu, cpitem);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmprect_menu, dmpitem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cmpoprect_menu, cmpopitem);
		if (rect_dimmode == RECT_CURSORDIM) {
			rect_drawn= FALSE;
			ice_op= RECT_INSERTROT;
		}
		else {
			pg_draw();
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
		}
		return;

	case ButtonPress:
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		new_rect->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_rect->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		new_rect->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_rect->getloc(&fx, &fy, &x, &junk);
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
rectinsrot_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y;
	float fx, fy, rad;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if ((x == rect_x[0]) && (y == rect_y[0]))
			return;
		if (rect_drawn) {
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_rx, rect_ry);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_rx-vx, rect_ry-vy);
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		rect_rfx= (float) x;
		rect_rfy= (float) (pg_pixheight-1-y);
		fx= (rect_rfx-rect_fx[0])/pg_dpi;
		fy= (rect_rfy-rect_fy[0])/pg_dpi;
		rad= (float) atan2((double) fy, (double) fx);
		rect_rot= (rad*180.)/PI;
		if (rect_rot < 0.)
			rect_rot+= 360.;
		rect_drawn= FALSE;
		ice_op= RECT_INSERTDIM;
		return;

	case ButtonPress:
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		pg_p2fcoords(x, y, &fx, &fy);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		pg_p2fcoords(x, y, &fx, &fy);
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (rect_drawn) {
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_rx, rect_ry);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_rx-vx, rect_ry-vy);
	}
	rect_rx= x;
	rect_ry= y;
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_rx, rect_ry);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_rx-vx, rect_ry-vy);
	rect_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
rectinsdim_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y;
	float fx, fy, w, h, rot;
	void rectins_corners(int, int);
	void rectins_getdim(float *, float *, float *);

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (rect_drawn) {
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_x[1], rect_y[1]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_x[1]-vx, rect_y[1]-vy);
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[1], rect_y[1], rect_x[2], rect_y[2]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[1]-vx, rect_y[1]-vy, rect_x[2]-vx, rect_y[2]-vy);
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[2], rect_y[2], rect_x[3], rect_y[3]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[2]-vx, rect_y[2]-vy, rect_x[3]-vx, rect_y[3]-vy);
			XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[3], rect_y[3], rect_x[0], rect_y[0]);
			XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[3]-vx, rect_y[3]-vy, rect_x[0]-vx, rect_y[0]-vy);
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		rectins_corners(x, y);
		rectins_getdim(&w, &h, &rot);
		(void) new_rect->setsize(w, h);
		new_rect->setrotation(rot);
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
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
		pg_p2fcoords(x, y, &fx, &fy);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		pg_p2fcoords(x, y, &fx, &fy);
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (rect_drawn) {
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_x[1], rect_y[1]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_x[1]-vx, rect_y[1]-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[1], rect_y[1], rect_x[2], rect_y[2]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[1]-vx, rect_y[1]-vy, rect_x[2]-vx, rect_y[2]-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[2], rect_y[2], rect_x[3], rect_y[3]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[2]-vx, rect_y[2]-vy, rect_x[3]-vx, rect_y[3]-vy);
		XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[3], rect_y[3], rect_x[0], rect_y[0]);
		XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[3]-vx, rect_y[3]-vy, rect_x[0]-vx, rect_y[0]-vy);
	}
	rectins_corners(x, y);
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[0], rect_y[0], rect_x[1], rect_y[1]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[0]-vx, rect_y[0]-vy, rect_x[1]-vx, rect_y[1]-vy);
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[1], rect_y[1], rect_x[2], rect_y[2]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[1]-vx, rect_y[1]-vy, rect_x[2]-vx, rect_y[2]-vy);
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[2], rect_y[2], rect_x[3], rect_y[3]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[2]-vx, rect_y[2]-vy, rect_x[3]-vx, rect_y[3]-vy);
	XDrawLine(dpy, pg_cpm, pg_xgc, rect_x[3], rect_y[3], rect_x[0], rect_y[0]);
	XDrawLine(dpy, pg_cwin, pg_xgc, rect_x[3]-vx, rect_y[3]-vy, rect_x[0]-vx, rect_y[0]-vy);
	rect_drawn= TRUE;
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
rectins_corners(int x, int y)
/*
   Determine the coordinates (rect_fx[i], rect_fy[i]) of the
   four corners of an arbitrarily rotated rectangle, given
   (i) the origin (rect_fx[0], rect_fy[0]), (ii) a point
   (rect_rfx, rect_rfy) located somewhere along the arbitrarily
   rotated positive x-axis, where the origin of the rectangle
   lies at point (0, 0) of the rotated coordinate system, and
   (iii) the pixel coordinates (x, y) of the corner opposite
   the origin. Note that the floating point coordinates
   (rect_fx[i], rect_fy[i]) use an upwardly increasing y,
   while the integer pixel coordinates (rect_x[i], rect_y[i])
   to which they are ultimately transformed use a downwardly
   increasing y. Thanks to Marc Spiegelman for the efficient
   voodoo dot product algorithm.
*/
{
	float v1x, v1y, v2x, v2y, lenratio;

	rect_x[2]= x;
	rect_y[2]= y;

	/* ensure that opposite corner differs from origin */
	if ((rect_x[0] == rect_x[2]) && (rect_y[0] == rect_y[2])) {
		rect_x[1]= rect_x[3]= x;
		rect_y[1]= rect_y[3]= y;
	}

	rect_fx[2]= (float) x;
	rect_fy[2]= (float) (pg_pixheight-1-y);

	v1x= rect_rfx-rect_fx[0];
	v1y= rect_rfy-rect_fy[0];
	v2x= rect_fx[2]-rect_fx[0];
	v2y= rect_fy[2]-rect_fy[0];
	lenratio= ((v1x*v2x)+(v1y*v2y))/((v1x*v1x)+(v1y*v1y));

	rect_fx[1]= rect_fx[0]+(lenratio*v1x);
	rect_fy[1]= rect_fy[0]+(lenratio*v1y);
	rect_fx[3]= rect_fx[0]+rect_fx[2]-rect_fx[1];
	rect_fy[3]= rect_fy[0]+rect_fy[2]-rect_fy[1];

	rect_x[1]= (int) rect_fx[1];
	rect_y[1]= pg_pixheight-1-((int) rect_fy[1]);
	rect_x[3]= (int) rect_fx[3];
	rect_y[3]= pg_pixheight-1-((int) rect_fy[3]);

	return;
}

void
rectins_getdim(float *w, float *h, float *rot)
{
	float dx, dy, crnrot, rad, sx, sy;
	boolean swap;

	/* ensure that opposite corner differs from origin */
	if ((rect_x[0] == rect_x[2]) && (rect_y[0] == rect_y[2])) {
		*w= *h= *rot= 0.;
		return;
	}

	/* if the opposite corner is in the second or fourth quadrant of
	   the rotated system, we need to swap the two origin-adjacent
	   corners, otherwise the rectangle will come out flipped */
	dx= (rect_fx[2]-rect_fx[0])/pg_dpi;
	dy= (rect_fy[2]-rect_fy[0])/pg_dpi;
	rad= (float) atan2((double) dy, (double) dx);
	crnrot= (rad*180.)/PI;
	if (crnrot < 0.)
		crnrot+= 360.;
	if (crnrot < rect_rot)
		crnrot+= 360.;
	if ((rect_rot < crnrot) && (crnrot < rect_rot+90.))
		swap= FALSE;
	else if ((rect_rot+90. < crnrot) && (crnrot < rect_rot+180.))
		swap= TRUE;
	else if ((rect_rot+180. < crnrot) && (crnrot < rect_rot+270.))
		swap= FALSE;
	else
		swap= TRUE;
	if (swap) {
		sx= rect_fx[1];
		sy= rect_fy[1];
		rect_fx[1]= rect_fx[3];
		rect_fy[1]= rect_fy[3];
		rect_fx[3]= sx;
		rect_fy[3]= sy;
	}

	dx= (rect_fx[1]-rect_fx[0])/pg_dpi;
	dy= (rect_fy[1]-rect_fy[0])/pg_dpi;
	*w= (float) sqrt((double) ((dx*dx)+(dy*dy)));

	rad= (float) atan2((double) dy, (double) dx);
	*rot= (rad*180.)/PI;
	if (*rot < 0.)
		*rot+= 360.;

	dx= (rect_fx[3]-rect_fx[0])/pg_dpi;
	dy= (rect_fy[3]-rect_fy[0])/pg_dpi;
	*h= (float) sqrt((double) ((dx*dx)+(dy*dy)));

	return;
}

void
delrect_proc(Menu *m, Menu_item *mi)
{
	Rectangle *rect;
	Menu_item *item;

	if ((rect= (Rectangle *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Rectangle *) NULL) {
		ice_err("Cannot locate selected rectangle object.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	rect_del(rect);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrrect_menu, LXMI_CLIENTDATA, (char *) rect);
	menuitem_delete(attrrect_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trrect_menu, LXMI_CLIENTDATA, (char *) rect);
	menuitem_delete(trrect_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cprect_menu, LXMI_CLIENTDATA, (char *) rect);
	menuitem_delete(cprect_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmprect_menu, LXMI_CLIENTDATA, (char *) rect);
	menuitem_delete(dmprect_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpoprect_menu, LXMI_CLIENTDATA, (char *) rect);
	menuitem_delete(cmpoprect_menu, item);
	menuitem_destroy(item);
	if (nrectangles == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
rect_del(Rectangle *rect)
{
	if (grobjs == (Grobj *) rect) {
		if (rect->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) rect->succ();
	}
	rect->unlink();
	delete rect;
	nrectangles--;
	return;
}
