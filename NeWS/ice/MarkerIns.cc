/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Marker.h"
#include "Pathdup.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrmrk_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cpmrk_proc(Menu *, Menu_item *);
extern void		delmrk_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		mrk_del(Marker *);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		trmrk_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

static Marker *new_mrk;

void
insmrk_proc(Menu *m, Menu_item *mi)
{
	char buf[80];

	panelitem_set(mrkattr_panel, mrkattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		ice_op= MRK_INSERTATTR;
		XMapRaised(dpy, mrkattr_frame);
		return;
	}

	panelitem_set(mrkattr_panel, mrkattr_type, LXPENUM_VALUE, MARKER_GLOBALTYPE, LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_size, LXPENUM_VALUE, MARKER_GLOBALSIZE, LXPI_NULL);
	(void) gconvert((double) gdf_mrksize, 10, 0, buf);
	panelitem_set(mrkattr_panel, mrkattr_radius, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_bndmode, LXPENUM_VALUE, MARKER_OPAQUEBNDM, LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_bndwd, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, MARKER_GLOBALBNDWIDTH, LXPI_NULL);
	(void) gconvert((double) gdf_bndwidth, 10, 0, buf);
	panelitem_set(mrkattr_panel, mrkattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_bndcolor, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, MARKER_GLOBALBND, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rbnd);
	panelitem_set(mrkattr_panel, mrkattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gbnd);
	panelitem_set(mrkattr_panel, mrkattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bbnd);
	panelitem_set(mrkattr_panel, mrkattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_fillmode, LXPENUM_VALUE, MARKER_TRANSPARENTFILLM, LXPI_NULL);
	panelitem_set(mrkattr_panel, mrkattr_fillcolor, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, MARKER_GLOBALFILL, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfill);
	panelitem_set(mrkattr_panel, mrkattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfill);
	panelitem_set(mrkattr_panel, mrkattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfill);
	panelitem_set(mrkattr_panel, mrkattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_hscale, LXPTEXT_VALUE, "1", LXPI_NULL);
	panelitem_set(mrkattr_panel, mrkattr_vscale, LXPTEXT_VALUE, "1", LXPI_NULL);
	panelitem_set(mrkattr_panel, mrkattr_rot, LXPTEXT_VALUE, "0", LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	panelitem_set(mrkattr_panel, mrkattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	panelitem_set(mrkattr_panel, mrkattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	panelitem_set(mrkattr_panel, mrkattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(mrkattr_panel, mrkattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= MRK_INSERTATTR;
	XMapRaised(dpy, mrkattr_frame);
	return;
}

void
mrkinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *name, *buf;
	float radius, bndwidth, hscale, vscale, rot;
	int mt, mtype, sz, bndmode, bndwd, bnd, fillmode, fill, dtk, seq;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	float fx, fy;
	char xbuf[30], ybuf[30];
	char nmbuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	XUnmapWindow(dpy, mrkattr_frame);
	ice_op= MAIN_MENU;

	name= (char *) panelitem_get(mrkattr_panel, mrkattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		(void) sprintf(nmbuf, "UnnamedMarker-%d", unnamed_markers++);
		name= nmbuf;
	}

	mt= *((int *) panelitem_get(mrkattr_panel, mrkattr_type, LXPENUM_VALUE));
	switch (mt) {
	case MARKER_GLOBALTYPE:
		mtype= gdf_mrktype+1;
		break;
	default:
		mtype= mt;
		break;
	}

	sz= *((int *) panelitem_get(mrkattr_panel, mrkattr_size, LXPENUM_VALUE));
	switch (sz) {
	case MARKER_GLOBALSIZE:
		radius= gdf_mrksize;
		break;
	case MARKER_OTHERSIZE:
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_radius, LXPTEXT_VALUE);
		radius= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid size value.", NONFATAL);
			return;
		}
		if (radius <= 0.) {
			ice_err("Size value must be greater than 0.", NONFATAL);
			return;
		}
		break;
	}

	bndmode= *((int *) panelitem_get(mrkattr_panel, mrkattr_bndmode, LXPENUM_VALUE));
	switch (bndmode) {
	case MARKER_OPAQUEBNDM:
		bndwd= *((int *) panelitem_get(mrkattr_panel, mrkattr_bndwd, LXPENUM_VALUE));
		switch (bndwd) {
		case MARKER_GLOBALBNDWIDTH:
			bndwidth= gdf_bndwidth;
			break;
		case MARKER_OTHERBNDWIDTH:
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bwidth, LXPTEXT_VALUE);
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

		bnd= *((int *) panelitem_get(mrkattr_panel, mrkattr_bndcolor, LXPENUM_VALUE));
		switch (bnd) {
		case MARKER_GLOBALBND:
			rbnd= gdf_rbnd;
			gbnd= gdf_gbnd;
			bbnd= gdf_bbnd;
			break;
		case MARKER_BLACKBND:
			rbnd= gbnd= bbnd= 0;
			break;
		case MARKER_WHITEBND:
			rbnd= gbnd= bbnd= 255;
			break;
		case MARKER_OTHERBND:
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rbnd, LXPTEXT_VALUE);
			rbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			if ((rbnd < 0) || (rbnd > 255)) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_gbnd, LXPTEXT_VALUE);
			gbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			if ((gbnd < 0) || (gbnd > 255)) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bbnd, LXPTEXT_VALUE);
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
	case MARKER_TRANSPARENTBNDM:
		bndwd= MARKER_GLOBALBNDWIDTH;
		bndwidth= gdf_bndwidth;
		bnd= MARKER_GLOBALBND;
		rbnd= gdf_rbnd;
		gbnd= gdf_gbnd;
		bbnd= gdf_bbnd;
		break;
	}

	fillmode= *((int *) panelitem_get(mrkattr_panel, mrkattr_fillmode, LXPENUM_VALUE));
	switch (fillmode) {
	case MARKER_TRANSPARENTFILLM:
		fill= MARKER_GLOBALFILL;
		rfill= gdf_rfill;
		gfill= gdf_gfill;
		bfill= gdf_bfill;
		break;
	case MARKER_OPAQUEFILLM:
		fill= *((int *) panelitem_get(mrkattr_panel, mrkattr_fillcolor, LXPENUM_VALUE));
		switch (fill) {
		case MARKER_GLOBALFILL:
			rfill= gdf_rfill;
			gfill= gdf_gfill;
			bfill= gdf_bfill;
			break;
		case MARKER_WHITEFILL:
			rfill= gfill= bfill= 255;
			break;
		case MARKER_BLACKFILL:
			rfill= gfill= bfill= 0;
			break;
		case MARKER_OTHERFILL:
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rfill, LXPTEXT_VALUE);
			rfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red fill color value.", NONFATAL);
				return;
			}
			if ((rfill < 0) || (rfill > 255)) {
				ice_err("Invalid red fill color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_gfill, LXPTEXT_VALUE);
			gfill= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green fill color value.", NONFATAL);
				return;
			}
			if ((gfill < 0) || (gfill > 255)) {
				ice_err("Invalid green fill color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bfill, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_hscale, LXPTEXT_VALUE);
	hscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid horizontal scale value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_vscale, LXPTEXT_VALUE);
	vscale= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid vertical scale value.", NONFATAL);
		return;
	}

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rot, LXPTEXT_VALUE);
	rot= (float) strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid rotation value.", NONFATAL);
		return;
	}

	pathnm= (char *) panelitem_get(mrkattr_panel, mrkattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(mrkattr_panel, mrkattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(mrkattr_panel, mrkattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(mrkattr_panel, mrkattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	if ((new_mrk= new Marker((Dlnk **) &grobjs, name, seq)) == (Marker *) NULL) {
		ice_err("Cannot create marker object.", NONFATAL);
		return;
	}
	nmarkers++;
	(void) new_mrk->setmarkertype(mt, mtype);
	(void) new_mrk->setsize(sz, radius);
	(void) new_mrk->setboundary(bndmode, bndwd, bndwidth, bnd, rbnd, gbnd, bbnd);
	(void) new_mrk->setfill(fillmode, fill, rfill, gfill, bfill);
	new_mrk->setscale(hscale, vscale);
	new_mrk->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_mrk->setclip(pth);

	(void) new_mrk->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_mrk->setdtkpix(dtkpix);

	ice_op= MRK_INSERTLOC;

	if (pg_loc == PG_CURSORLOC) {
		pgcoords_drawn= crosshairs_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_mrk,
				LXMI_PROC, delmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(new_mrk);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_mrk,
				LXMI_PROC, attrmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(new_mrk);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_mrk,
				LXMI_PROC, trmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(new_mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_mrk,
				LXMI_PROC, cpmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(new_mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_mrk,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(new_mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_mrk,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(new_mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		return;
	}
	(void) menuitem_insert(delmrk_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrmrk_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trmrk_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpmrk_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpmrk_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopmrk_menu, cmpopitem);

	loc_grobj= (Grobj *) new_mrk;
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
insicemrk_rd(FILE *fp, int gdf, int newobj)
{
	char *name;
	float radius, bndwidth, hscale, vscale, rot;
	float fx, fy;
	int mt, mtype, sz, bndmode, bndwd, bnd, fillmode, fill, dtk, seq;
	int len, ir, ig, ib;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	char *c, *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	int iot;
	boolean endfound;
	char nmbuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Marker *mrk;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Mrk: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Mrk: Begin ")+len-1)= '\0';
	if ((name= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(name, ice_iobuf+strlen("%%ICE-Mrk: Begin "));
	if (!strncmp(name, "UnnamedMarker-", strlen("UnnamedMarker-"))) {
		delete name;
		(void) sprintf(nmbuf, "UnnamedMarker-%d", unnamed_markers++);
		if ((name= new char[strlen(nmbuf)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(name, nmbuf);
	}

	fx= fy= 0.;
	hscale= vscale= 1.;
	rot= 0.;
	mt= MARKER_GLOBALTYPE;
	mtype= gdf_mrktype+1;
	sz= MARKER_GLOBALSIZE;
	radius= gdf_mrksize;
	bndmode= MARKER_OPAQUEBNDM;
	bndwd= MARKER_GLOBALBNDWIDTH;
	bndwidth= gdf_bndwidth;
	bnd= MARKER_GLOBALBND;
	rbnd= gdf_rbnd;
	gbnd= gdf_gbnd;
	bbnd= gdf_bbnd;
	fillmode= MARKER_TRANSPARENTFILLM;
	fill= MARKER_GLOBALFILL;
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

		if (!strncmp("%%ICE-Mrk: Loc ", ice_iobuf, strlen("%%ICE-Mrk: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: Loc "), "%f %f", &fx, &fy) != 2) {
				ice_err("Invalid marker location value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Mrk: Trans ", ice_iobuf, strlen("%%ICE-Mrk: Trans "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: Trans "), "%f %f %f", &rot, &hscale, &vscale) != 3) {
				ice_err("Invalid marker transform value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Mrk: Type ", ice_iobuf, strlen("%%ICE-Mrk: Type "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: Type "), "%d %d", &mt, &mtype) != 2) {
				ice_err("Invalid marker type value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((mt != MARKER_GLOBALTYPE) &&
			    (mt != MARKER_SQUARE) &&
			    (mt != MARKER_TRIANGLE) &&
			    (mt != MARKER_CIRCLE) &&
			    (mt != MARKER_CROSS)) {
				ice_err("Invalid marker type value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((mtype != MARKER_SQUARE) &&
			    (mtype != MARKER_TRIANGLE) &&
			    (mtype != MARKER_CIRCLE) &&
			    (mtype != MARKER_CROSS)) {
				ice_err("Invalid marker type value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Mrk: Size ", ice_iobuf, strlen("%%ICE-Mrk: Size "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: Size "), "%d %f", &sz, &radius) != 2) {
				ice_err("Invalid marker size value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((sz != MARKER_GLOBALSIZE) &&
			    (sz != MARKER_OTHERSIZE)) {
				ice_err("Invalid marker size value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if (radius <= 0.) {
				ice_err("Invalid marker size value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Mrk: Bnd ", ice_iobuf, strlen("%%ICE-Mrk: Bnd "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: Bnd "), "%d %d %f %d %d %d %d", &bndmode, &bndwd, &bndwidth, &bnd, &ir, &ig, &ib) != 7) {
				ice_err("Invalid marker boundary value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((bndmode != MARKER_OPAQUEBNDM) &&
			    (bndmode != MARKER_TRANSPARENTBNDM)) {
				ice_err("Invalid marker boundary mode value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((bndwd != MARKER_GLOBALBNDWIDTH) &&
			    (bndwd != MARKER_OTHERBNDWIDTH)) {
				ice_err("Invalid marker boundary width value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if (bndwidth < 0.) {
				ice_err("Invalid marker boundary width value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((bnd != MARKER_GLOBALBND) &&
			    (bnd != MARKER_BLACKBND) &&
			    (bnd != MARKER_WHITEBND) &&
			    (bnd != MARKER_OTHERBND)) {
				ice_err("Invalid marker boundary value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid marker red boundary value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid marker green boundary value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid marker blue boundary value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			rbnd= (unsigned char) ir;
			gbnd= (unsigned char) ig;
			bbnd= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Mrk: Fill ", ice_iobuf, strlen("%%ICE-Mrk: Fill "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: Fill "), "%d %d %d %d %d", &fillmode, &fill, &ir, &ig, &ib) != 5) {
				ice_err("Invalid marker fill value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((fillmode != MARKER_TRANSPARENTFILLM) &&
			    (fillmode != MARKER_OPAQUEFILLM)) {
				ice_err("Invalid marker fill mode value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((fill != MARKER_GLOBALFILL) &&
			    (fill != MARKER_WHITEFILL) &&
			    (fill != MARKER_BLACKFILL) &&
			    (fill != MARKER_OTHERFILL)) {
				ice_err("Invalid marker fill value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid marker red fill value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid marker green fill value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid marker blue fill value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			rfill= (unsigned char) ir;
			gfill= (unsigned char) ig;
			bfill= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Mrk: Clip ", ice_iobuf, strlen("%%ICE-Mrk: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Mrk: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strncmp("%%ICE-Mrk: DTK ", ice_iobuf, strlen("%%ICE-Mrk: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid marker DTK value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid marker DTK value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid marker red DTK value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid marker green DTK value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid marker blue DTK value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Mrk: Seq ", ice_iobuf, strlen("%%ICE-Mrk: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid marker sequence value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Mrk: IOT ", ice_iobuf, strlen("%%ICE-Mrk: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Mrk: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid marker tag value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid marker tag value.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Mrk: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Marker end not found.", NONFATAL);
		delete name;
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
			if (mt == MARKER_GLOBALTYPE)
				mtype= gdf_mrktype+1;
			if (sz == MARKER_GLOBALSIZE)
				radius= gdf_mrksize;
			if (bndwd == MARKER_GLOBALBNDWIDTH)
				bndwidth= gdf_bndwidth;
			if (bnd == MARKER_GLOBALBND) {
				rbnd= gdf_rbnd;
				gbnd= gdf_gbnd;
				bbnd= gdf_bbnd;
			}
			if (fill == MARKER_GLOBALFILL) {
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
			if (mt == MARKER_GLOBALTYPE)
				mt= mtype;
			if (sz == MARKER_GLOBALSIZE)
				sz= MARKER_OTHERSIZE;
			if (bndwd == MARKER_GLOBALBNDWIDTH)
				bndwd= MARKER_OTHERBNDWIDTH;
			if (bnd == MARKER_GLOBALBND)
				bnd= MARKER_OTHERBND;
			if (fill == MARKER_GLOBALFILL)
				fill= MARKER_OTHERFILL;
			if (dtk == GROBJ_GLOBALDTK)
				dtk= GROBJ_OTHERDTK;
			break;
		}
	}

	if ((mrk= new Marker((Dlnk **) &grobjs, name, seq)) == (Marker *) NULL) {
		ice_err("Cannot create marker object.", NONFATAL);
		delete name;
		return;
	}
	delete name;
	nmarkers++;

	(void) mrk->setmarkertype(mt, mtype);
	(void) mrk->setsize(sz, radius);
	(void) mrk->setboundary(bndmode, bndwd, bndwidth, bnd, rbnd, gbnd, bbnd);
	(void) mrk->setfill(fillmode, fill, rfill, gfill, bfill);
	mrk->setfloc(fx, fy, (float) pg_dpi);
	mrk->setscale(hscale, vscale);
	mrk->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	mrk->setclip(pth);
	mrk->setiotag(iot);

	(void) mrk->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	mrk->setdtkpix(dtkpix);

	if ((delitem= menuitem_create(LXMI_STRING, mrk->getname(),
				LXMI_CLIENTDATA, (char *) mrk,
				LXMI_PROC, delmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(mrk);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, mrk->getname(),
				LXMI_CLIENTDATA, (char *) mrk,
				LXMI_PROC, attrmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(mrk);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, mrk->getname(),
				LXMI_CLIENTDATA, (char *) mrk,
				LXMI_PROC, trmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, mrk->getname(),
				LXMI_CLIENTDATA, (char *) mrk,
				LXMI_PROC, cpmrk_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, mrk->getname(),
				LXMI_CLIENTDATA, (char *) mrk,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, mrk->getname(),
				LXMI_CLIENTDATA, (char *) mrk,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		mrk_del(mrk);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	(void) menuitem_insert(delmrk_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrmrk_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trmrk_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpmrk_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpmrk_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopmrk_menu, cmpopitem);

	return;
}

void
mrkins_event(XEvent *event)
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
		new_mrk->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		if ((delitem= menuitem_create(LXMI_STRING, new_mrk->getname(),
					LXMI_CLIENTDATA, (char *) new_mrk,
					LXMI_PROC, delmrk_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			mrk_del(new_mrk);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_mrk->getname(),
					LXMI_CLIENTDATA, (char *) new_mrk,
					LXMI_PROC, attrmrk_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			mrk_del(new_mrk);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_mrk->getname(),
					LXMI_CLIENTDATA, (char *) new_mrk,
					LXMI_PROC, trmrk_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			mrk_del(new_mrk);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_mrk->getname(),
					LXMI_CLIENTDATA, (char *) new_mrk,
					LXMI_PROC, cpmrk_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			mrk_del(new_mrk);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_mrk->getname(),
					LXMI_CLIENTDATA, (char *) new_mrk,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			mrk_del(new_mrk);
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
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_mrk->getname(),
					LXMI_CLIENTDATA, (char *) new_mrk,
					LXMI_PROC, cmpop_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			mrk_del(new_mrk);
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
		(void) menuitem_insert(delmrk_menu, delitem);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrmrk_menu, attritem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trmrk_menu, tritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cpmrk_menu, cpitem);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmpmrk_menu, dmpitem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cmpopmrk_menu, cmpopitem);
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
		new_mrk->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_mrk->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		new_mrk->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_mrk->getloc(&fx, &fy, &x, &junk);
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
delmrk_proc(Menu *m, Menu_item *mi)
{
	Marker *mrk;
	Menu_item *item;

	if ((mrk= (Marker *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Marker *) NULL) {
		ice_err("Cannot locate selected marker object.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	mrk_del(mrk);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrmrk_menu, LXMI_CLIENTDATA, (char *) mrk);
	menuitem_delete(attrmrk_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trmrk_menu, LXMI_CLIENTDATA, (char *) mrk);
	menuitem_delete(trmrk_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cpmrk_menu, LXMI_CLIENTDATA, (char *) mrk);
	menuitem_delete(cpmrk_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmpmrk_menu, LXMI_CLIENTDATA, (char *) mrk);
	menuitem_delete(dmpmrk_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpopmrk_menu, LXMI_CLIENTDATA, (char *) mrk);
	menuitem_delete(cmpopmrk_menu, item);
	menuitem_destroy(item);
	if (nmarkers == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
mrk_del(Marker *mrk)
{
	if (grobjs == mrk) {
		if (mrk->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) mrk->succ();
	}
	mrk->unlink();
	delete mrk;
	nmarkers--;
	return;
}
