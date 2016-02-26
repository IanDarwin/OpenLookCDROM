/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Polygon.h"
#include "Pathdup.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrpoly_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cppoly_proc(Menu *, Menu_item *);
extern void		delpoly_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		poly_del(Polygon *);
extern void		trpoly_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern int loc_nvertices, loc_vertex;
extern float *loc_xvertices, *loc_yvertices;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

static Polygon *new_poly;
static int nvertices;
static float *xvert, *yvert;
static float ipp, xvorig, yvorig;
static int *ixvert, *iyvert;
static boolean poly_drawn;

void
inspoly_proc(Menu *m, Menu_item *mi)
{
	char buf[80];

	panelitem_set(polyattr_panel, polyattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_hscale, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(polyattr_panel, polyattr_vscale, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(polyattr_panel, polyattr_rot, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		ice_op= POLY_INSERTATTR;
		XMapRaised(dpy, polyattr_frame);
		return;
	}

	panelitem_set(polyattr_panel, polyattr_closure, LXPENUM_VALUE, POLYGON_CLOSED, LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_bndmode, LXPENUM_VALUE, POLYGON_OPAQUEBNDM, LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_bndwd, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, POLYGON_GLOBALBNDWIDTH, LXPI_NULL);
	(void) gconvert((double) gdf_bndwidth, 10, 0, buf);
	panelitem_set(polyattr_panel, polyattr_bwidth, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_bndcolor, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, POLYGON_GLOBALBND, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rbnd);
	panelitem_set(polyattr_panel, polyattr_rbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gbnd);
	panelitem_set(polyattr_panel, polyattr_gbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bbnd);
	panelitem_set(polyattr_panel, polyattr_bbnd, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_fillmode, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, POLYGON_TRANSPARENTFILLM, LXPI_NULL);
	panelitem_set(polyattr_panel, polyattr_fillcolor, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, POLYGON_GLOBALFILL, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfill);
	panelitem_set(polyattr_panel, polyattr_rfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfill);
	panelitem_set(polyattr_panel, polyattr_gfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfill);
	panelitem_set(polyattr_panel, polyattr_bfill, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	panelitem_set(polyattr_panel, polyattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	panelitem_set(polyattr_panel, polyattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	panelitem_set(polyattr_panel, polyattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(polyattr_panel, polyattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= POLY_INSERTATTR;
	XMapRaised(dpy, polyattr_frame);
	return;
}

void
polyinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *name, *buf;
	float bndwidth;
	int cl, bndmode, bndwd, bnd, fillmode, fill, dtk, seq;
	unsigned char rbnd, gbnd, bbnd;
	unsigned char rfill, gfill, bfill;
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	char nmbuf[30];
	char vbuf[LXADEF_MAXSTORE+1];
	int val;
	float fx, fy;
	char xbuf[30], ybuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	XUnmapWindow(dpy, polyattr_frame);
	ice_op= MAIN_MENU;

	name= (char *) panelitem_get(polyattr_panel, polyattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		(void) sprintf(nmbuf, "UnnamedPolygon-%d", unnamed_polygons++);
		name= nmbuf;
	}

	cl= *((int *) panelitem_get(polyattr_panel, polyattr_closure, LXPENUM_VALUE));

	bndmode= *((int *) panelitem_get(polyattr_panel, polyattr_bndmode, LXPENUM_VALUE));
	switch (bndmode) {
	case POLYGON_OPAQUEBNDM:
		bndwd= *((int *) panelitem_get(polyattr_panel, polyattr_bndwd, LXPENUM_VALUE));
		switch (bndwd) {
		case POLYGON_GLOBALBNDWIDTH:
			bndwidth= gdf_bndwidth;
			break;
		case POLYGON_OTHERBNDWIDTH:
			buf= (char *) panelitem_get(polyattr_panel, polyattr_bwidth, LXPTEXT_VALUE);
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

		bnd= *((int *) panelitem_get(polyattr_panel, polyattr_bndcolor, LXPENUM_VALUE));
		switch (bnd) {
		case POLYGON_GLOBALBND:
			rbnd= gdf_rbnd;
			gbnd= gdf_gbnd;
			bbnd= gdf_bbnd;
			break;
		case POLYGON_BLACKBND:
			rbnd= gbnd= bbnd= 0;
			break;
		case POLYGON_WHITEBND:
			rbnd= gbnd= bbnd= 255;
			break;
		case POLYGON_OTHERBND:
			buf= (char *) panelitem_get(polyattr_panel, polyattr_rbnd, LXPTEXT_VALUE);
			rbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			if ((rbnd < 0) || (rbnd > 255)) {
				ice_err("Invalid red boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(polyattr_panel, polyattr_gbnd, LXPTEXT_VALUE);
			gbnd= (unsigned char) strtol(buf, &cptr, 10);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			if ((gbnd < 0) || (gbnd > 255)) {
				ice_err("Invalid green boundary color value.", NONFATAL);
				return;
			}
			buf= (char *) panelitem_get(polyattr_panel, polyattr_bbnd, LXPTEXT_VALUE);
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
	case POLYGON_TRANSPARENTBNDM:
		bndwd= POLYGON_GLOBALBNDWIDTH;
		bndwidth= gdf_bndwidth;
		bnd= POLYGON_GLOBALBND;
		rbnd= gdf_rbnd;
		gbnd= gdf_gbnd;
		bbnd= gdf_bbnd;
		break;
	}

	switch (cl) {
	case POLYGON_CLOSED:
		fillmode= *((int *) panelitem_get(polyattr_panel, polyattr_fillmode, LXPENUM_VALUE));
		switch (fillmode) {
		case POLYGON_TRANSPARENTFILLM:
			fill= POLYGON_GLOBALFILL;
			rfill= gdf_rfill;
			gfill= gdf_gfill;
			bfill= gdf_bfill;
			break;
		case POLYGON_OPAQUEFILLM:
			fill= *((int *) panelitem_get(polyattr_panel, polyattr_fillcolor, LXPENUM_VALUE));
			switch (fill) {
			case POLYGON_GLOBALFILL:
				rfill= gdf_rfill;
				gfill= gdf_gfill;
				bfill= gdf_bfill;
				break;
			case POLYGON_WHITEFILL:
				rfill= gfill= bfill= 255;
				break;
			case POLYGON_BLACKFILL:
				rfill= gfill= bfill= 0;
				break;
			case POLYGON_OTHERFILL:
				buf= (char *) panelitem_get(polyattr_panel, polyattr_rfill, LXPTEXT_VALUE);
				rfill= (unsigned char) strtol(buf, &cptr, 10);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid red fill color value.", NONFATAL);
					return;
				}
				if ((rfill < 0) || (rfill > 255)) {
					ice_err("Invalid red fill color value.", NONFATAL);
					return;
				}
				buf= (char *) panelitem_get(polyattr_panel, polyattr_gfill, LXPTEXT_VALUE);
				gfill= (unsigned char) strtol(buf, &cptr, 10);
				if ((cptr == buf) || (*cptr != '\0')) {
					ice_err("Invalid green fill color value.", NONFATAL);
					return;
				}
				if ((gfill < 0) || (gfill > 255)) {
					ice_err("Invalid green fill color value.", NONFATAL);
					return;
				}
				buf= (char *) panelitem_get(polyattr_panel, polyattr_bfill, LXPTEXT_VALUE);
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
		break;
	case POLYGON_OPEN:
		fillmode= POLYGON_TRANSPARENTFILLM;
		fill= POLYGON_GLOBALFILL;
		rfill= gdf_rfill;
		gfill= gdf_gfill;
		bfill= gdf_bfill;
		break;
	}

	pathnm= (char *) panelitem_get(polyattr_panel, polyattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(polyattr_panel, polyattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(polyattr_panel, polyattr_rdtk, LXPTEXT_VALUE);
		rdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		if ((rdtk < 0) || (rdtk > 255)) {
			ice_err("Invalid red dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(polyattr_panel, polyattr_gdtk, LXPTEXT_VALUE);
		gdtk= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid dump transparency key green value.", NONFATAL);
			return;
		}
		if ((gdtk < 0) || (gdtk > 255)) {
			ice_err("Invalid green dump transparency key value.", NONFATAL);
			return;
		}
		buf= (char *) panelitem_get(polyattr_panel, polyattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(polyattr_panel, polyattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		return;
	}

	if ((new_poly= new Polygon((Dlnk **) &grobjs, name, seq)) == (Polygon *) NULL) {
		ice_err("Cannot create polygon object.", NONFATAL);
		return;
	}
	npolygons++;
	(void) new_poly->setclosure(cl);
	(void) new_poly->setboundary(bndmode, bndwd, bndwidth, bnd, rbnd, gbnd, bbnd);
	(void) new_poly->setfill(fillmode, fill, rfill, gfill, bfill);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_poly->setclip(pth);

	(void) new_poly->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_poly->setdtkpix(dtkpix);

	ice_op= POLY_INSERTLOC;

	if (pg_loc == PG_CURSORLOC) {
		nvertices= 0;
		xvert= yvert= (float *) NULL;
		ixvert= iyvert= (int *) NULL;
		ipp= 1./pg_dpi;
		poly_drawn= pgcoords_drawn= crosshairs_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}
	else {
		if (alert_prompt(progname, dpy, &val,
				LXA_TEXT, "Number of Vertices:", "", vbuf,
				LXA_BUTTON, "Continue", 0,
				LXA_BUTTON, "Abort", 1,
				LXA_NULL) == LX_ERROR)
			ice_err("Alert failure.", FATAL);

		if (val == 1) {
			poly_del(new_poly);
			ice_op= MAIN_MENU;
			return;
		}

		if (strlen(vbuf) == 0) {
			ice_err("Number of vertices unspecified.", NONFATAL);
			poly_del(new_poly);
			ice_op= MAIN_MENU;
			return;
		}
		loc_nvertices= (unsigned char) strtol(vbuf, &cptr, 10);
		if ((cptr == vbuf) || (*cptr != '\0')) {
			ice_err("Invalid number of vertices.", NONFATAL);
			poly_del(new_poly);
			ice_op= MAIN_MENU;
			return;
		}
		if (loc_nvertices < 3) {
			ice_err("There must be at least three vertices.", NONFATAL);
			poly_del(new_poly);
			ice_op= MAIN_MENU;
			return;
		}
		if ((loc_xvertices= new float[loc_nvertices]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			poly_del(new_poly);
			ice_op= MAIN_MENU;
			return;
		}
		if ((loc_yvertices= new float[loc_nvertices]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			poly_del(new_poly);
			ice_op= MAIN_MENU;
			delete loc_xvertices;
			return;
		}
		loc_vertex= 0;
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, delpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, attrpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, trpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, cppoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_poly,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(new_poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	(void) menuitem_insert(delpoly_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpoly_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpoly_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cppoly_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppoly_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoppoly_menu, cmpopitem);

	loc_grobj= (Grobj *) new_poly;
	panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Vertex 1", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
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
insicepoly_rd(FILE *fp, int gdf, int newobj)
{
	char *name;
	float bndwidth, hscale, vscale, rot;
	float fx, fy;
	int nvertices, rvertices;
	float *xvertices, *yvertices;
	int cl, bndmode, bndwd, bnd, fillmode, fill, dtk, seq;
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
	Polygon *poly;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Poly: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Poly: Begin ")+len-1)= '\0';
	if ((name= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(name, ice_iobuf+strlen("%%ICE-Poly: Begin "));
	if (!strncmp(name, "UnnamedPolygon-", strlen("UnnamedPolygon-"))) {
		delete name;
		(void) sprintf(nmbuf, "UnnamedPolygon-%d", unnamed_polygons++);
		if ((name= new char[strlen(nmbuf)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(name, nmbuf);
	}

	fx= fy= 0.;
	hscale= vscale= 1.;
	rot= 0.;
	nvertices= 0;
	xvertices= yvertices= (float *) NULL;
	cl= POLYGON_CLOSED;
	bndmode= POLYGON_OPAQUEBNDM;
	bndwd= POLYGON_GLOBALBNDWIDTH;
	bndwidth= gdf_bndwidth;
	bnd= POLYGON_GLOBALBND;
	rbnd= gdf_rbnd;
	gbnd= gdf_gbnd;
	bbnd= gdf_bbnd;
	fillmode= POLYGON_TRANSPARENTFILLM;
	fill= POLYGON_GLOBALFILL;
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

		if (!strncmp("%%ICE-Poly: Loc ", ice_iobuf, strlen("%%ICE-Poly: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Loc "), "%f %f", &fx, &fy) != 2) {
				ice_err("Invalid polygon location value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Poly: Trans ", ice_iobuf, strlen("%%ICE-Poly: Trans "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Trans "), "%f %f %f", &rot, &hscale, &vscale) != 3) {
				ice_err("Invalid polygon transform value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Poly: Cls ", ice_iobuf, strlen("%%ICE-Poly: Cls "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Cls "), "%d", &cl) != 1) {
				ice_err("Invalid polygon closure value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((cl != POLYGON_CLOSED) &&
			    (cl != POLYGON_OPEN)) {
				ice_err("Invalid polygon closure value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Poly: Vert ", ice_iobuf, strlen("%%ICE-Poly: Vert "))) {
			if (nvertices != 0) {
				ice_err("Too many polygon vertices specified.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Vert "), "%d", &nvertices) != 1) {
				ice_err("Invalid polygon vertices value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if (nvertices <= 0) {
				ice_err("Invalid polygon vertices value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((xvertices= new float[nvertices]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete pathnm;
				return;
			}
			if ((yvertices= new float[nvertices]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete xvertices;
				delete pathnm;
				return;
			}
			rvertices= 0;
		}

		else if (!strncmp("%%ICE-Poly: Vert+ ", ice_iobuf, strlen("%%ICE-Poly: Vert+ "))) {
			if (nvertices == 0) {
				ice_err("Unknown number of polygon vertices.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if (rvertices == nvertices) {
				ice_err("Too many polygon vertices specified.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Vert+ "), "%f %f", &(xvertices[rvertices]), &(yvertices[rvertices])) != 2) {
				ice_err("Invalid polygon vertices value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			rvertices++;
		}

		else if (!strncmp("%%ICE-Poly: Bnd ", ice_iobuf, strlen("%%ICE-Poly: Bnd "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Bnd "), "%d %d %f %d %d %d %d", &bndmode, &bndwd, &bndwidth, &bnd, &ir, &ig, &ib) != 7) {
				ice_err("Invalid polygon boundary value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((bndmode != POLYGON_OPAQUEBNDM) &&
			    (bndmode != POLYGON_TRANSPARENTBNDM)) {
				ice_err("Invalid polygon boundary mode value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((bndwd != POLYGON_GLOBALBNDWIDTH) &&
			    (bndwd != POLYGON_OTHERBNDWIDTH)) {
				ice_err("Invalid polygon boundary width value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if (bndwidth < 0.) {
				ice_err("Invalid polygon boundary width value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((bnd != POLYGON_GLOBALBND) &&
			    (bnd != POLYGON_BLACKBND) &&
			    (bnd != POLYGON_WHITEBND) &&
			    (bnd != POLYGON_OTHERBND)) {
				ice_err("Invalid polygon boundary value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid polygon red boundary value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid polygon green boundary value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid polygon blue boundary value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			rbnd= (unsigned char) ir;
			gbnd= (unsigned char) ig;
			bbnd= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Poly: Fill ", ice_iobuf, strlen("%%ICE-Poly: Fill "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Fill "), "%d %d %d %d %d", &fillmode, &fill, &ir, &ig, &ib) != 5) {
				ice_err("Invalid polygon fill value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((fillmode != POLYGON_TRANSPARENTFILLM) &&
			    (fillmode != POLYGON_OPAQUEFILLM)) {
				ice_err("Invalid polygon fill mode value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((fill != POLYGON_GLOBALFILL) &&
			    (fill != POLYGON_WHITEFILL) &&
			    (fill != POLYGON_BLACKFILL) &&
			    (fill != POLYGON_OTHERFILL)) {
				ice_err("Invalid polygon fill value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid polygon red fill value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid polygon green fill value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid polygon blue fill value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			rfill= (unsigned char) ir;
			gfill= (unsigned char) ig;
			bfill= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Poly: Clip ", ice_iobuf, strlen("%%ICE-Poly: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Poly: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strncmp("%%ICE-Poly: DTK ", ice_iobuf, strlen("%%ICE-Poly: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid polygon DTK value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid polygon DTK value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid polygon red DTK value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid polygon green DTK value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid polygon blue DTK value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Poly: Seq ", ice_iobuf, strlen("%%ICE-Poly: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid polygon sequence value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Poly: IOT ", ice_iobuf, strlen("%%ICE-Poly: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Poly: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid polygon tag value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid polygon tag value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Poly: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Polygon end not found.", NONFATAL);
		delete name;
		delete xvertices;
		delete yvertices;
		delete pathnm;
		return;
	}
	if (rvertices < nvertices) {
		ice_err("Polygon vertices specification incomplete.", NONFATAL);
		delete name;
		delete xvertices;
		delete yvertices;
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
					delete xvertices;
					delete yvertices;
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
			delete xvertices;
			delete yvertices;
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
			if (bndwd == POLYGON_GLOBALBNDWIDTH)
				bndwidth= gdf_bndwidth;
			if (bnd == POLYGON_GLOBALBND) {
				rbnd= gdf_rbnd;
				gbnd= gdf_gbnd;
				bbnd= gdf_bbnd;
			}
			if (fill == POLYGON_GLOBALFILL) {
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
			if (bndwd == POLYGON_GLOBALBNDWIDTH)
				bndwd= POLYGON_OTHERBNDWIDTH;
			if (bnd == POLYGON_GLOBALBND)
				bnd= POLYGON_OTHERBND;
			if (fill == POLYGON_GLOBALFILL)
				fill= POLYGON_OTHERFILL;
			if (dtk == GROBJ_GLOBALDTK)
				dtk= GROBJ_OTHERDTK;
			break;
		}
	}

	if ((poly= new Polygon((Dlnk **) &grobjs, name, seq)) == (Polygon *) NULL) {
		ice_err("Cannot create polygon object.", NONFATAL);
		delete name;
		delete xvertices;
		delete yvertices;
		return;
	}
	delete name;
	npolygons++;

	(void) poly->setclosure(cl);
	(void) poly->setboundary(bndmode, bndwd, bndwidth, bnd, rbnd, gbnd, bbnd);
	(void) poly->setfill(fillmode, fill, rfill, gfill, bfill);
	poly->setfloc(fx, fy, (float) pg_dpi);
	poly->setscale(hscale, vscale);
	poly->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	poly->setclip(pth);
	poly->setiotag(iot);

	(void) poly->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	poly->setdtkpix(dtkpix);

	poly->setvertices(nvertices, xvertices, yvertices);
	if ((delitem= menuitem_create(LXMI_STRING, poly->getname(),
				LXMI_CLIENTDATA, (char *) poly,
				LXMI_PROC, delpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(poly);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, poly->getname(),
				LXMI_CLIENTDATA, (char *) poly,
				LXMI_PROC, attrpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(poly);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, poly->getname(),
				LXMI_CLIENTDATA, (char *) poly,
				LXMI_PROC, trpoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, poly->getname(),
				LXMI_CLIENTDATA, (char *) poly,
				LXMI_PROC, cppoly_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, poly->getname(),
				LXMI_CLIENTDATA, (char *) poly,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, poly->getname(),
				LXMI_CLIENTDATA, (char *) poly,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		poly_del(poly);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	(void) menuitem_insert(delpoly_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpoly_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpoly_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cppoly_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppoly_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoppoly_menu, cmpopitem);

	return;
}

void
polyins_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, i;
	float fx, fy, *oxvert, *oyvert;
	int *oixvert, *oiyvert;
	Menu_item *delitem, *attritem;
	Menu_item *tritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;
	void poly_drawoutline();

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	if (xvert == (float *) NULL) {
		if ((xvert= new float[3]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			poly_del(new_poly);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((yvert= new float[3]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete xvert;
			poly_del(new_poly);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((ixvert= new int[3]) == (int *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete xvert;
			delete yvert;
			poly_del(new_poly);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((iyvert= new int[3]) == (int *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete xvert;
			delete yvert;
			delete ixvert;
			poly_del(new_poly);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
	}

	switch (event->type) {

	case ButtonRelease:
		if (!crosshairs_drawn)
			return;
		bevt= (XButtonEvent *) event;
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (poly_drawn) {
			poly_drawoutline();
			poly_drawn= FALSE;
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		if (nvertices == 0) {
			new_poly->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			nvertices++;
			xvert[0]= 0.;
			yvert[0]= 0.;
			xvorig= ipp*((float) x);
			yvorig= ipp*((float) (pg_pixheight-1-y));
			ixvert[0]= x;
			iyvert[0]= y;
			return;
		}
		else if (nvertices == 1) {
			nvertices++;
			xvert[1]= (ipp*((float) x))-xvorig;
			yvert[1]= (ipp*((float) (pg_pixheight-1-y)))-yvorig;
			ixvert[1]= x;
			iyvert[1]= y;
			return;
		}
		else if (bevt->button != Button3) {
			oxvert= xvert;
			oyvert= yvert;
			oixvert= ixvert;
			oiyvert= iyvert;
			nvertices++;
			if ((xvert= new float[nvertices+1]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oxvert;
				delete oyvert;
				delete oixvert;
				delete oiyvert;
				poly_del(new_poly);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
			if ((yvert= new float[nvertices+1]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oxvert;
				delete oyvert;
				delete oixvert;
				delete oiyvert;
				delete xvert;
				poly_del(new_poly);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
			if ((ixvert= new int[nvertices+1]) == (int *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oxvert;
				delete oyvert;
				delete oixvert;
				delete oiyvert;
				delete xvert;
				delete yvert;
				poly_del(new_poly);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
			if ((iyvert= new int[nvertices+1]) == (int *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oxvert;
				delete oyvert;
				delete oixvert;
				delete oiyvert;
				delete xvert;
				delete yvert;
				delete ixvert;
				poly_del(new_poly);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
			for (i= 0; i < nvertices-1; i++) {
				xvert[i]= oxvert[i];
				yvert[i]= oyvert[i];
				ixvert[i]= oixvert[i];
				iyvert[i]= oiyvert[i];
			}
			xvert[nvertices-1]= (ipp*((float) x))-xvorig;
			yvert[nvertices-1]= (ipp*((float) (pg_pixheight-1-y)))-yvorig;
			ixvert[nvertices-1]= x;
			iyvert[nvertices-1]= y;
			delete oxvert;
			delete oyvert;
			delete oixvert;
			delete oiyvert;
			return;
		}
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		nvertices++;
		xvert[nvertices-1]= (ipp*((float) x))-xvorig;
		yvert[nvertices-1]= (ipp*((float) (pg_pixheight-1-y)))-yvorig;
		new_poly->setvertices(nvertices, xvert, yvert);
		delete ixvert;
		delete iyvert;
		if ((delitem= menuitem_create(LXMI_STRING, new_poly->getname(),
					LXMI_CLIENTDATA, (char *) new_poly,
					LXMI_PROC, delpoly_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			poly_del(new_poly);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_poly->getname(),
					LXMI_CLIENTDATA, (char *) new_poly,
					LXMI_PROC, attrpoly_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			poly_del(new_poly);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_poly->getname(),
					LXMI_CLIENTDATA, (char *) new_poly,
					LXMI_PROC, trpoly_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			poly_del(new_poly);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_poly->getname(),
					LXMI_CLIENTDATA, (char *) new_poly,
					LXMI_PROC, cppoly_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			poly_del(new_poly);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_poly->getname(),
					LXMI_CLIENTDATA, (char *) new_poly,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			poly_del(new_poly);
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
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_poly->getname(),
					LXMI_CLIENTDATA, (char *) new_poly,
					LXMI_PROC, cmpop_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			poly_del(new_poly);
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
		(void) menuitem_insert(delpoly_menu, delitem);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrpoly_menu, attritem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trpoly_menu, tritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cppoly_menu, cpitem);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmppoly_menu, dmpitem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cmpoppoly_menu, cmpopitem);
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
		fx= ipp*((float) x);
		fy= ipp*((float) (pg_pixheight-1-y));
		if (nvertices == 0)
			new_poly->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		else {
			xvert[nvertices]= fx-xvorig;
			yvert[nvertices]= fy-yvorig;
		}
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		fx= ipp*((float) x);
		fy= ipp*((float) (pg_pixheight-1-y));
		if (nvertices == 0)
			new_poly->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		else {
			xvert[nvertices]= fx-xvorig;
			yvert[nvertices]= fy-yvorig;
		}
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (poly_drawn)
		poly_drawoutline();
	if (nvertices > 0) {
		ixvert[nvertices]= x;
		iyvert[nvertices]= y;
		poly_drawoutline();
		poly_drawn= TRUE;
	}
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
delpoly_proc(Menu *m, Menu_item *mi)
{
	Polygon *poly;
	Menu_item *item;

	if ((poly= (Polygon *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Polygon *) NULL) {
		ice_err("Cannot locate selected polygon object.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	poly_del(poly);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrpoly_menu, LXMI_CLIENTDATA, (char *) poly);
	menuitem_delete(attrpoly_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trpoly_menu, LXMI_CLIENTDATA, (char *) poly);
	menuitem_delete(trpoly_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cppoly_menu, LXMI_CLIENTDATA, (char *) poly);
	menuitem_delete(cppoly_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmppoly_menu, LXMI_CLIENTDATA, (char *) poly);
	menuitem_delete(dmppoly_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpoppoly_menu, LXMI_CLIENTDATA, (char *) poly);
	menuitem_delete(cmpoppoly_menu, item);
	menuitem_destroy(item);
	if (npolygons == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
poly_del(Polygon *poly)
{
	if (grobjs == poly) {
		if (poly->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) poly->succ();
	}
	poly->unlink();
	delete poly;
	npolygons--;
	return;
}

void
poly_drawoutline()
{
	int vx, vy;
	int x0, x1, y0, y1, i;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	x0= ixvert[0];
	y0= iyvert[0];
	for (i= 1; i < nvertices+1; i++, x0= x1, y0= y1) {
		x1= ixvert[i];
		y1= iyvert[i];
		XDrawLine(dpy, pg_cpm, pg_xgc, x0, y0, x1, y1);
		XDrawLine(dpy, pg_cwin, pg_xgc, x0-vx, y0-vy, x1-vx, y1-vy);
	}
	if ((nvertices > 1) && (new_poly->getclosure() == POLYGON_CLOSED)) {
		XDrawLine(dpy, pg_cpm, pg_xgc, ixvert[0], iyvert[0], x1, y1);
		XDrawLine(dpy, pg_cwin, pg_xgc, ixvert[0]-vx, iyvert[0]-vy, x1-vx, y1-vy);
	}
	return;
}
