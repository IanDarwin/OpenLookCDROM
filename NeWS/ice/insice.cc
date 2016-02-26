/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Pathdup.h"
#include "Psdoc.h"
#include "Raster.h"
#include "Intobj.h"
#include "Text.h"
#include "Vector.h"
#include "Curve.h"
#include "Marker.h"
#include "Rectangle.h"
#include "Polygon.h"
#include "Axis.h"

extern "C" {
void			bcopy(char *, char *, int);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strncmp(char *, char *, int);
int			strlen(char *);
}

extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern FILE *		doc_open(char *, char *);
extern void		ice_err(char *, int);
extern void		insiceaxis_rd(FILE *, int, int);
extern void		insicecmp_rd(FILE *);
extern void		insicecrv_rd(FILE *, int, int);
extern void		insicemrk_rd(FILE *, int, int);
extern void		insicepoly_rd(FILE *, int, int);
extern void		insicepsd_rd(FILE *, int, int);
extern void		insicepth_rd(FILE *);
extern void		insiceras_rd(FILE *, int, int);
extern void		insicerect_rd(FILE *, int, int);
extern void		insicetxt_rd(FILE *, int, int);
extern void		insicevec_rd(FILE *, int, int);
extern void		pg_draw();

char ice_iobuf[INPUT_BUFSZ];
int insice_lineno;

char *newgdf_fontname= (char *) NULL;
float newgdf_fontsize;
float newgdf_fontlead;
float newgdf_linewidth;
int newgdf_fg;
unsigned char newgdf_rfg, newgdf_gfg, newgdf_bfg;
int newgdf_bg;
unsigned char newgdf_rbg, newgdf_gbg, newgdf_bbg;
int newgdf_mrktype;
float newgdf_mrksize;
float newgdf_bndwidth;
int newgdf_bnd;
unsigned char newgdf_rbnd, newgdf_gbnd, newgdf_bbnd;
int newgdf_fill;
unsigned char newgdf_rfill, newgdf_gfill, newgdf_bfill;
int newgdf_dtk;
unsigned char newgdf_rdtk, newgdf_gdtk, newgdf_bdtk;

Pathdup *duppaths;

void
ice_read(char *filenm)
{
	int pg, gdf, currobj, newobj;
	FILE *fp;
	char errmsg[MAX_ERRMSGLEN+1];
	void insice_rdfile(FILE *, int, int, int, int);

	if ((fp= doc_open(filenm, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		return;
	}

	pg= INSICE_NEWPG;
	gdf= INSICE_NEWGDF;
	newobj= INSICE_NEWOBJNEWGDF;
	currobj= INSICE_CURROBJNEWGDF;

	insice_rdfile(fp, pg, gdf, newobj, currobj);

	(void) fclose(fp);
	return;
}

void
insice_proc(Menu *m, Menu_item *mi)
{

	panelitem_set(insice_panel, insice_filenm, LXPTEXT_VALUE, "", LXPI_NULL);
	panelitem_set(insice_panel, insice_pg, LXPENUM_VALUE, INSICE_CURRPG, LXPI_NULL);
	panelitem_set(insice_panel, insice_gdf, LXPENUM_VALUE, INSICE_CURRGDF, LXPI_NULL);
	panelitem_set(insice_panel, insice_currobj, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, INSICE_CURROBJCURRGDF, LXPI_NULL);
	panelitem_set(insice_panel, insice_newobj, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, INSICE_NEWOBJNEWGDF, LXPI_NULL);

	ice_op= ICE_INSERT;
	XMapRaised(dpy, insice_frame);
	return;
}

void
insicegdf_proc(Panel *p, Panel_item *pi)
{
	int gdf;

	gdf= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	switch(gdf) {
	case INSICE_CURRGDF:
		panelitem_set(p, insice_currobj, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, insice_newobj, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	case INSICE_NEWGDF:
		panelitem_set(p, insice_newobj, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
		panelitem_set(p, insice_currobj, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
		break;
	}
	return;
}

void
insicecont_proc(Panel *p, Panel_item *pi)
{
	char *fn;
	int pg, gdf, currobj, newobj;
	FILE *fp;
	char errmsg[MAX_ERRMSGLEN+1];
	void insice_rdfile(FILE *, int, int, int, int);

	XUnmapWindow(dpy, insice_frame);
	ice_op= MAIN_MENU;

	fn= (char *) panelitem_get(insice_panel, insice_filenm, LXPTEXT_VALUE);
	if (fn == (char *) NULL) {
		ice_err("Null filename.", NONFATAL);
		return;
	}
	if (strlen(fn) == 0) {
		ice_err("Empty filename.", NONFATAL);
		return;
	}
	if ((fp= doc_open(fn, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", fn);
		ice_err(errmsg, NONFATAL);
		return;
	}

	pg= *((int *) panelitem_get(insice_panel, insice_pg, LXPENUM_VALUE));

	gdf= *((int *) panelitem_get(insice_panel, insice_gdf, LXPENUM_VALUE));
	switch (gdf) {
	case INSICE_CURRGDF:
		newobj= *((int *) panelitem_get(insice_panel, insice_newobj, LXPENUM_VALUE));
		break;
	case INSICE_NEWGDF:
		currobj= *((int *) panelitem_get(insice_panel, insice_currobj, LXPENUM_VALUE));
		break;
	}

	insice_rdfile(fp, pg, gdf, newobj, currobj);

	(void) fclose(fp);
	return;
}

void
insiceabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, insice_frame);
	ice_op= MAIN_MENU;
	return;
}

void
insice_rdfile(FILE *fp, int pg, int gdf, int newobj, int currobj)
{
	Grobj *gr;
	Pathdup *duppth;
	void insicepg_rd(FILE *, int);
	void insicegdf_rd(FILE *, int, int);

	XFillRectangle(dpy, pg_cpm, pg_bggc, 0, 0, pg_pixwidth, pg_pixheight);
	canvas_flush(pg_canvas);
	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	/* clear all existing I/O tags so that
	   they won't be confused with new objects */
	for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ())
		gr->setiotag(GROBJ_NULLIOTAG);
	for (gr= (Grobj *) cmpobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ())
		gr->setiotag(GROBJ_NULLIOTAG);

	insice_lineno= 0;
	duppaths= (Pathdup *) NULL;
	while (fgets(ice_iobuf, INPUT_BUFSZ, fp) != (char *) NULL) {
		insice_lineno++;

		if (strncmp("%%ICE-", ice_iobuf, strlen("%%ICE-")))
			continue;

		if (!strcmp("%%ICE-Pg: Begin\n", ice_iobuf))
			insicepg_rd(fp, pg);
		else if (!strcmp("%%ICE-Gdf: Begin\n", ice_iobuf))
			insicegdf_rd(fp, gdf, currobj);
		else if (!strncmp("%%ICE-Psd: Begin ", ice_iobuf, strlen("%%ICE-Psd: Begin ")))
			insicepsd_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Ras: Begin ", ice_iobuf, strlen("%%ICE-Ras: Begin ")))
			insiceras_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Txt: Begin ", ice_iobuf, strlen("%%ICE-Txt: Begin ")))
			insicetxt_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Vec: Begin ", ice_iobuf, strlen("%%ICE-Vec: Begin ")))
			insicevec_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Crv: Begin ", ice_iobuf, strlen("%%ICE-Crv: Begin ")))
			insicecrv_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Mrk: Begin ", ice_iobuf, strlen("%%ICE-Mrk: Begin ")))
			insicemrk_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Rect: Begin ", ice_iobuf, strlen("%%ICE-Rect: Begin ")))
			insicerect_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Poly: Begin ", ice_iobuf, strlen("%%ICE-Poly: Begin ")))
			insicepoly_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Axis: Begin ", ice_iobuf, strlen("%%ICE-Axis: Begin ")))
			insiceaxis_rd(fp, gdf, newobj);
		else if (!strncmp("%%ICE-Cmp: Begin ", ice_iobuf, strlen("%%ICE-Cmp: Begin ")))
			insicecmp_rd(fp);
		else if (!strncmp("%%ICE-Path: Begin ", ice_iobuf, strlen("%%ICE-Path: Begin ")))
			insicepth_rd(fp);
	}

	pg_draw();

	while ((duppth= duppaths) != (Pathdup *) NULL) {
		if (duppth->succ() == (Dlnk *) NULL)
			duppaths= (Pathdup *) NULL;
		else
			duppaths= (Pathdup *) duppth->succ();
		duppth->unlink();
		delete duppth;
	}

	XSync(dpy, False);
	XDefineCursor(dpy, pg_cwin, std_cursor);

	return;
}

void
insicepg_rd(FILE *fp, int pg)
{
	double w, h, hsi, vsi, xri, yri, xru, yru;
	int dpi, pw, ph, update, loc, locdisplay, units, ohlt, bg;
	unsigned char rbg, gbg, bbg;
	int ir, ig, ib, len;
	XGCValues gcv;
	boolean rescale, resize;
	Grobj *g;
	char *c, *pathnm;
	Path *pth;
	boolean endfound;

	w= pg_width;
	h= pg_height;
	dpi= pg_dpi;
	update= pg_update;
	loc= pg_loc;
	locdisplay= pg_locdisplay;
	units= pg_units;
	hsi= pg_hsi;
	vsi= pg_vsi;
	xri= pg_xri;
	yri= pg_yri;
	xru= pg_xru;
	yru= pg_yru;
	ohlt= pg_originhlt;
	bg= pg_bg;
	rbg= pg_rbg;
	gbg= pg_gbg;
	bbg= pg_bbg;
	pathnm= (char *) NULL;
	pth= (Path *) NULL;

	endfound= FALSE;
	while (fgets(ice_iobuf, INPUT_BUFSZ, fp) != (char *) NULL) {
		insice_lineno++;

		if (strncmp("%%ICE-", ice_iobuf, strlen("%%ICE-")))
			continue;
		else if (strcmp("%%ICE-Pg: End\n", ice_iobuf) && (pg == INSICE_CURRPG))
			continue;

		else if (!strncmp("%%ICE-Pg: Dim ", ice_iobuf, strlen("%%ICE-Pg: Dim "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: Dim "), "%lf %lf %d", &w, &h, &dpi) != 3) {
				ice_err("Invalid page dimension value.", NONFATAL);
				return;
			}
			if (w <= 0.) {
				ice_err("Width must be greater than 0.", NONFATAL);
				return;
			}
			if (h <= 0.) {
				ice_err("Height must be greater than 0.", NONFATAL);
				return;
			}
			if (dpi <= 0) {
				ice_err("Resolution must be greater than 0 DPI.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: Flags ", ice_iobuf, strlen("%%ICE-Pg: Flags "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: Flags "), "%d %d", &update, &units) != 2) {
				ice_err("Invalid page flags value.", NONFATAL);
				return;
			}
			if ((update != PG_AUTOMATIC) && (update != PG_MANUAL)) {
				ice_err("Invalid page update value.", NONFATAL);
				return;
			}
			if ((units != PG_PIXELS) && (units != PG_POINTS) &&
			    (units != PG_INCHES) && (units != PG_USER)) {
				ice_err("Invalid page units value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: Loc ", ice_iobuf, strlen("%%ICE-Pg: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: Loc "), "%d", &loc) != 1) {
				ice_err("Invalid page location value.", NONFATAL);
				return;
			}
			if ((loc != PG_CURSORLOC) && (loc != PG_TEXTLOC)) {
				ice_err("Invalid page location value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: Locdpy ", ice_iobuf, strlen("%%ICE-Pg: Locdpy "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: Locdpy "), "%d", &locdisplay) != 1) {
				ice_err("Invalid page location display value.", NONFATAL);
				return;
			}
			if ((locdisplay != PG_CURSORLOC) && (locdisplay != PG_TEXTLOC)) {
				ice_err("Invalid page location display value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: SI ", ice_iobuf, strlen("%%ICE-Pg: SI "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: SI "), "%lf %lf", &hsi, &vsi) != 2) {
				ice_err("Invalid page user scale value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: RI ", ice_iobuf, strlen("%%ICE-Pg: RI "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: RI "), "%lf %lf", &xri, &yri) != 2) {
				ice_err("Invalid page inch reference value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: RU ", ice_iobuf, strlen("%%ICE-Pg: RU "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: RU "), "%lf %lf", &xru, &yru) != 2) {
				ice_err("Invalid page user reference value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: Ohlt ", ice_iobuf, strlen("%%ICE-Pg: Ohlt "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: Ohlt "), "%d", &ohlt) != 1) {
				ice_err("Invalid object origin highlight value.", NONFATAL);
				return;
			}
			if ((ohlt != PG_NOORIGINHLT) && (ohlt != PG_ORIGINHLT)) {
				ice_err("Invalid object origin highlight value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Pg: BG ", ice_iobuf, strlen("%%ICE-Pg: BG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Pg: BG "), "%d %d %d %d", &bg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid page background value.", NONFATAL);
				return;
			}
			if ((bg != PG_WHITEBG) && (bg != PG_BLACKBG) && (bg != PG_OTHERBG)) {
				ice_err("Invalid page background value.", NONFATAL);
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid page red background value.", NONFATAL);
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid page green background value.", NONFATAL);
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid page blue background value.", NONFATAL);
				return;
			}
			rbg= (unsigned char) ir;
			gbg= (unsigned char) ig;
			bbg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Pg: Clip ", ice_iobuf, strlen("%%ICE-Pg: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Pg: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strcmp("%%ICE-Pg: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Page description end not found.", NONFATAL);
		return;
	}
	if (pg == INSICE_CURRPG)
		return;

	if (pathnm != (char *) NULL) {
		char *oldname, *newname;
		Pathdup *duppth;

		for (duppth= duppaths; duppth != (Pathdup *) NULL; duppth= (Pathdup *) duppth->succ()) {
			duppth->getnames(&oldname, &newname);
			if (!strcmp(pathnm, oldname)) {
				delete pathnm;
				if ((pathnm= new char[strlen(newname)+1]) == (char *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
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
			delete pathnm;
			return;
		}
	}
	else
		pth= (Path *) NULL;
	delete pathnm;

	rescale= resize= FALSE;

	pg_update= update;
	pg_loc= loc;
	pg_locdisplay= locdisplay;
	pg_units= units;
	pg_hsi= hsi;
	pg_vsi= vsi;
	pg_xri= xri;
	pg_yri= yri;
	pg_xru= xru;
	pg_yru= yru;
	pg_originhlt= ohlt;
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
	}

	if (pg_clip != (Path *) NULL)
		(void) pg_clip->setreferences(PATH_REFDECR);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	pg_clip= pth;

	if (dpi != pg_dpi)
		rescale= TRUE;
	if ((w != pg_width) || (h != pg_height) || (rescale))
		resize= TRUE;

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
		for (g= grobjs; g != (Grobj *) NULL; g= (Grobj *) g->succ()) {
			float fx, fy;
			int ix, iy;

			switch (g->gettype()) {
			case GROBJ_PSDOC:
				break;
			case GROBJ_RASTER:
				g->getloc(&fx, &fy, &ix, &iy);
				g->setfloc(fx, fy, (float) pg_dpi);
				break;
			case GROBJ_INTOBJ:
				break;
			default:
				break;
			}
		}
	}

	return;
}

void
insicegdf_rd(FILE *fp, int gdf, int currobj)
{
	int len, ir, ig, ib;
	Grobj *gr;
	boolean endfound;

	delete newgdf_fontname;
	if ((newgdf_fontname= new char[strlen(gdf_fontname)+1]) == (char *) NULL)
		return;
	(void) strcpy(newgdf_fontname, gdf_fontname);
	newgdf_fontsize= gdf_fontsize;
	newgdf_fontlead= gdf_fontlead;
	newgdf_linewidth= gdf_linewidth;
	newgdf_fg= gdf_fg;
	newgdf_rfg= gdf_rfg;
	newgdf_gfg= gdf_gfg;
	newgdf_bfg= gdf_bfg;
	newgdf_bg= gdf_bg;
	newgdf_rbg= gdf_rbg;
	newgdf_gbg= gdf_gbg;
	newgdf_bbg= gdf_bbg;
	newgdf_mrktype= gdf_mrktype;
	newgdf_mrksize= gdf_mrksize;
	newgdf_bndwidth= gdf_bndwidth;
	newgdf_bnd= gdf_bnd;
	newgdf_rbnd= gdf_rbnd;
	newgdf_gbnd= gdf_gbnd;
	newgdf_bbnd= gdf_bbnd;
	newgdf_fill= gdf_fill;
	newgdf_rfill= gdf_rfill;
	newgdf_gfill= gdf_gfill;
	newgdf_bfill= gdf_bfill;
	newgdf_dtk= gdf_dtk;
	newgdf_rdtk= gdf_rdtk;
	newgdf_gdtk= gdf_gdtk;
	newgdf_bdtk= gdf_bdtk;

	endfound= FALSE;
	while (fgets(ice_iobuf, INPUT_BUFSZ, fp) != (char *) NULL) {
		insice_lineno++;

		if (strncmp("%%ICE-", ice_iobuf, strlen("%%ICE-")))
			continue;

		if (!strncmp("%%ICE-Gdf: Font ", ice_iobuf, strlen("%%ICE-Gdf: Font "))) {
			delete newgdf_fontname;
			len= strlen(ice_iobuf+strlen("%%ICE-Gdf: Font "));
			*(ice_iobuf+strlen("%%ICE-Gdf: Font ")+len-1)= '\0';
			if ((newgdf_fontname= new char[len]) == (char *) NULL)
				return;
			(void) strcpy(newgdf_fontname, ice_iobuf+strlen("%%ICE-Gdf: Font "));
		}

		else if (!strncmp("%%ICE-Gdf: Fontsz ", ice_iobuf, strlen("%%ICE-Gdf: Fontsz "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: Fontsz "), "%f %f", &newgdf_fontsize, &newgdf_fontlead) != 2) {
				ice_err("Invalid default font size value.", NONFATAL);
				return;
			}
			if (newgdf_fontsize <= 0.) {
				ice_err("Invalid default font size value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Gdf: Line ", ice_iobuf, strlen("%%ICE-Gdf: Line "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: Line "), "%f", &newgdf_linewidth) != 1) {
				ice_err("Invalid default line width value.", NONFATAL);
				return;
			}
			if (newgdf_linewidth < 0.) {
				ice_err("Invalid default line width value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Gdf: FG ", ice_iobuf, strlen("%%ICE-Gdf: FG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: FG "), "%d %d %d %d", &newgdf_fg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid default foreground value.", NONFATAL);
				return;
			}
			if ((newgdf_fg != GDF_BLACKFG) &&
			    (newgdf_fg != GDF_WHITEFG) &&
			    (newgdf_fg != GDF_OTHERFG)) {
				ice_err("Invalid default foreground value.", NONFATAL);
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid default red foreground value.", NONFATAL);
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid default green foreground value.", NONFATAL);
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid default blue foreground value.", NONFATAL);
				return;
			}
			newgdf_rfg= (unsigned char) ir;
			newgdf_gfg= (unsigned char) ig;
			newgdf_bfg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Gdf: BG ", ice_iobuf, strlen("%%ICE-Gdf: BG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: BG "), "%d %d %d %d", &newgdf_bg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid default background value.", NONFATAL);
				return;
			}
			if ((newgdf_bg != GDF_WHITEBG) &&
			    (newgdf_bg != GDF_BLACKBG) &&
			    (newgdf_bg != GDF_OTHERBG)) {
				ice_err("Invalid default background value.", NONFATAL);
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid default red background value.", NONFATAL);
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid default green background value.", NONFATAL);
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid default blue background value.", NONFATAL);
				return;
			}
			newgdf_rbg= (unsigned char) ir;
			newgdf_gbg= (unsigned char) ig;
			newgdf_bbg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Gdf: Mark ", ice_iobuf, strlen("%%ICE-Gdf: Mark "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: Mark "), "%d %f", &newgdf_mrktype, &newgdf_mrksize) != 2) {
				ice_err("Invalid default marker value.", NONFATAL);
				return;
			}
			if ((newgdf_mrktype != GDF_SQUARE) &&
			    (newgdf_mrktype != GDF_TRIANGLE) &&
			    (newgdf_mrktype != GDF_CIRCLE) &&
			    (newgdf_mrktype != GDF_CROSS)) {
				ice_err("Invalid default marker type value.", NONFATAL);
				return;
			}
			if (newgdf_mrksize <= 0.) {
				ice_err("Invalid default marker size value.", NONFATAL);
				return;
			}
		}

		else if (!strncmp("%%ICE-Gdf: Bnd ", ice_iobuf, strlen("%%ICE-Gdf: Bnd "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: Bnd "), "%f %d %d %d %d", &newgdf_bndwidth, &newgdf_bnd, &ir, &ig, &ib) != 5) {
				ice_err("Invalid default boundary value.", NONFATAL);
				return;
			}
			if (newgdf_bndwidth < 0.) {
				ice_err("Invalid default boundary width value.", NONFATAL);
				return;
			}
			if ((newgdf_bnd != GDF_BLACKBND) &&
			    (newgdf_bnd != GDF_WHITEBND) &&
			    (newgdf_bnd != GDF_OTHERBND)) {
				ice_err("Invalid default boundary value.", NONFATAL);
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid default red boundary value.", NONFATAL);
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid default green boundary value.", NONFATAL);
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid default blue boundary value.", NONFATAL);
				return;
			}
			newgdf_rbnd= (unsigned char) ir;
			newgdf_gbnd= (unsigned char) ig;
			newgdf_bbnd= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Gdf: Fill ", ice_iobuf, strlen("%%ICE-Gdf: Fill "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: Fill "), "%d %d %d %d", &newgdf_fill, &ir, &ig, &ib) != 4) {
				ice_err("Invalid default fill value.", NONFATAL);
				return;
			}
			if ((newgdf_fill != GDF_WHITEFILL) &&
			    (newgdf_fill != GDF_BLACKFILL) &&
			    (newgdf_fill != GDF_OTHERFILL)) {
				ice_err("Invalid default fill value.", NONFATAL);
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid default red fill value.", NONFATAL);
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid default green fill value.", NONFATAL);
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid default blue fill value.", NONFATAL);
				return;
			}
			newgdf_rfill= (unsigned char) ir;
			newgdf_gfill= (unsigned char) ig;
			newgdf_bfill= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Gdf: DTK ", ice_iobuf, strlen("%%ICE-Gdf: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Gdf: DTK "), "%d %d %d %d", &newgdf_dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid default DTK value.", NONFATAL);
				return;
			}
			if ((newgdf_dtk != GDF_WHITEDTK) &&
			    (newgdf_dtk != GDF_BLACKDTK) &&
			    (newgdf_dtk != GDF_OTHERDTK)) {
				ice_err("Invalid default DTK value.", NONFATAL);
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid default red DTK value.", NONFATAL);
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid default green DTK value.", NONFATAL);
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid default blue DTK value.", NONFATAL);
				return;
			}
			newgdf_rdtk= (unsigned char) ir;
			newgdf_gdtk= (unsigned char) ig;
			newgdf_bdtk= (unsigned char) ib;
		}

		else if (!strcmp("%%ICE-Gdf: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Default description end not found.", NONFATAL);
		return;
	}

	if (gdf == INSICE_CURRGDF)
		return;

	if (npsfonts > 0) {
		int ff, fn;
		boolean found;

		found= FALSE;
		for (ff= 0; ff < npsfontfamilies; ff++) {
			for (fn= 0; fn < psfontfamilies[ff].psff_count; fn++) {
				if (!strcmp(newgdf_fontname, psfontfamilies[ff].psff_fonts[fn])) {
					gdf_fontname= psfontfamilies[ff].psff_fonts[fn];
					found= TRUE;
					break;
				}
			}
			if (found == TRUE)
				break;
		}
		if (found == FALSE)
			ice_err("New default font not available.", NONFATAL);
	}
	gdf_fontsize= newgdf_fontsize;
	gdf_fontlead= newgdf_fontlead;
	gdf_linewidth= newgdf_linewidth;
	gdf_fg= newgdf_fg;
	gdf_rfg= newgdf_rfg;
	gdf_gfg= newgdf_gfg;
	gdf_bfg= newgdf_bfg;
	gdf_bg= newgdf_bg;
	gdf_rbg= newgdf_rbg;
	gdf_gbg= newgdf_gbg;
	gdf_bbg= newgdf_bbg;
	gdf_mrktype= newgdf_mrktype;
	gdf_mrksize= newgdf_mrksize;
	gdf_bndwidth= newgdf_bndwidth;
	gdf_bnd= newgdf_bnd;
	gdf_rbnd= newgdf_rbnd;
	gdf_gbnd= newgdf_gbnd;
	gdf_bbnd= newgdf_bbnd;
	gdf_fill= newgdf_fill;
	gdf_rfill= newgdf_rfill;
	gdf_gfill= newgdf_gfill;
	gdf_bfill= newgdf_bfill;
	gdf_dtk= newgdf_dtk;
	gdf_rdtk= newgdf_rdtk;
	gdf_gdtk= newgdf_gdtk;
	gdf_bdtk= newgdf_bdtk;

	switch (currobj) {
	case INSICE_CURROBJCURRGDF:

		/* preserve all attributes of existing Grobjs except
		   GLOBAL tags, which should now be set to OTHER */
		for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
			Raster *ras;
			Text *text;
			Vector *vec;
			Curve *crv;
			Marker *mrk;
			Rectangle *rect;
			Polygon *poly;
			Axis *axis;
			int w, h, d;
			int ofg, obgm, obg, obndm, obndclr, ofillm, ofillclr, odtk;
			int omrkt, omrktype;
			unsigned char orfg, ogfg, obfg;
			unsigned char orbg, ogbg, obbg;
			unsigned char orbnd, ogbnd, obbnd;
			unsigned char orfill, ogfill, obfill;
			unsigned char ordtk, ogdtk, obdtk;
			unsigned long ofgp, obgp;
			int font, fontsz, tl;
			char *fntnm;
			float size, lead, orad, obw, pt, st, tt;
			int olinewd, osz, obndwd;

			switch (gr->gettype()) {
			case GROBJ_PSDOC:
				gr->getdtk(&odtk, &ordtk, &ogdtk, &obdtk);
				if (odtk != GROBJ_GLOBALDTK)
					break;
				(void) gr->setdtk(GROBJ_OTHERDTK, ordtk, ogdtk, obdtk);
				break;
			case GROBJ_RASTER:
				ras= (Raster *) gr;
				ras->getrassize(&w, &h, &d);
				if (d != 1)
					break;
				ras->getfg(&ofg, &ofgp, &orfg, &ogfg, &obfg);
				ras->getbg(&obg, &obgp, &orbg, &ogbg, &obbg);
				if (ofg == RASTER_GLOBALFG)
					ras->setfg(RASTER_OTHERFG, ofgp, orfg, ogfg, obfg);
				if (obg == RASTER_GLOBALBG)
					ras->setbg(RASTER_OTHERBG, obgp, orbg, ogbg, obbg);
				break;
			case GROBJ_INTOBJ:
				switch (((Intobj *) gr)->getintobjtype()) {

				case INTOBJ_TEXT:
					text= (Text *) gr;
					text->getfont(&font, &fntnm);
					if (font == TEXT_GLOBALFONT)
						(void) text->setfont(TEXT_OTHERFONT, fntnm);
					text->getfontsize(&fontsz, &size, &lead);
					if (fontsz == TEXT_GLOBALFONTSZ)
						(void) text->setfontsize(TEXT_OTHERFONTSZ, size, lead);
					text->getforeground(&ofg, &orfg, &ogfg, &obfg);
					if (ofg == TEXT_GLOBALFG)
						(void) text->setforeground(TEXT_OTHERFG, orfg, ogfg, obfg);
					text->getbackground(&obgm, &obg, &orbg, &ogbg, &obbg);
					if (obg == TEXT_GLOBALBG)
						(void) text->setbackground(obgm, TEXT_OTHERBG, orbg, ogbg, obbg);
					break;
				case INTOBJ_VECTOR:
					vec= (Vector *) gr;
					vec->getwidth(&olinewd, &size);
					if (olinewd == VECTOR_GLOBALWIDTH)
						(void) vec->setwidth(VECTOR_OTHERWIDTH, size);
					vec->getforeground(&ofg, &orfg, &ogfg, &obfg);
					if (ofg == VECTOR_GLOBALFG)
						(void) vec->setforeground(VECTOR_OTHERFG, orfg, ogfg, obfg);
					break;
				case INTOBJ_CURVE:
					crv= (Curve *) gr;
					crv->getwidth(&olinewd, &size);
					if (olinewd == CURVE_GLOBALWIDTH)
						(void) crv->setwidth(CURVE_OTHERWIDTH, size);
					crv->getforeground(&ofg, &orfg, &ogfg, &obfg);
					if (ofg == CURVE_GLOBALFG)
						(void) crv->setforeground(CURVE_OTHERFG, orfg, ogfg, obfg);
					break;
				case INTOBJ_MARKER:
					mrk= (Marker *) gr;
					mrk->getmarkertype(&omrkt, &omrktype);
					if (omrkt == MARKER_GLOBALTYPE)
						(void) mrk->setmarkertype(omrktype, omrktype);
					mrk->getsize(&osz, &orad);
					if (osz == MARKER_GLOBALSIZE)
						(void) mrk->setsize(MARKER_OTHERSIZE, orad);
					mrk->getboundary(&obndm, &obndwd, &obw, &obndclr, &orbnd, &ogbnd, &obbnd);
					if ((obndwd == MARKER_GLOBALBNDWIDTH) ||
					    (obndclr == MARKER_GLOBALBND)) {
						if (obndwd == MARKER_GLOBALBNDWIDTH)
							obndwd= MARKER_OTHERBNDWIDTH;
						if (obndclr == MARKER_GLOBALBND)
							obndclr= MARKER_OTHERBND;
						(void) mrk->setboundary(obndm, obndwd, obw, obndclr, orbnd, ogbnd, obbnd);
					}
					mrk->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
					if (ofillclr == MARKER_GLOBALFILL)
						(void) mrk->setfill(ofillm, MARKER_OTHERFILL, orfill, ogfill, obfill);
					break;
				case INTOBJ_RECTANGLE:
					rect= (Rectangle *) gr;
					rect->getwidth(&obndwd, &obw);
					if (obndwd == RECTANGLE_GLOBALBNDWIDTH)
						(void) rect->setwidth(RECTANGLE_OTHERBNDWIDTH, obw);
					rect->getboundary(&obndm, &obndclr, &orbnd, &ogbnd, &obbnd);
					if (obndclr == RECTANGLE_GLOBALBND)
						(void) rect->setboundary(obndm, RECTANGLE_OTHERBND, orbnd, ogbnd, obbnd);
					rect->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
					if (ofillclr == RECTANGLE_GLOBALFILL)
						(void) rect->setfill(ofillm, RECTANGLE_OTHERFILL, orfill, ogfill, obfill);
					break;
				case INTOBJ_POLYGON:
					poly= (Polygon *) gr;
					poly->getboundary(&obndm, &obndwd, &obw, &obndclr, &orbnd, &ogbnd, &obbnd);
					if ((obndwd == POLYGON_GLOBALBNDWIDTH) ||
					    (obndclr == POLYGON_GLOBALBND)) {
						if (obndwd == POLYGON_GLOBALBNDWIDTH)
							obndwd= POLYGON_OTHERBNDWIDTH;
						if (obndclr == POLYGON_GLOBALBND)
							obndclr= POLYGON_OTHERBND;
						(void) poly->setboundary(obndm, obndwd, obw, obndclr, orbnd, ogbnd, obbnd);
					}
					poly->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
					if (ofillclr == POLYGON_GLOBALFILL)
						(void) poly->setfill(ofillm, POLYGON_OTHERFILL, orfill, ogfill, obfill);
					break;
				case INTOBJ_AXIS:
					axis= (Axis *) gr;
					axis->getaxiswidth(&olinewd, &size);
					if (olinewd == AXIS_GLOBALWIDTH)
						(void) axis->setaxiswidth(AXIS_OTHERWIDTH, size);
					axis->gettick(&tl, &pt, &st, &tt, &olinewd, &size);
					if (olinewd == AXIS_GLOBALWIDTH)
						(void) axis->settick(tl, pt, st, tt, AXIS_OTHERWIDTH, size);
					axis->getline(&ofg, &orfg, &ogfg, &obfg);
					if (ofg == AXIS_GLOBALLINE)
						(void) axis->setline(AXIS_OTHERLINE, orfg, ogfg, obfg);
					axis->getfont(&font, &fntnm);
					if (font == AXIS_GLOBALFONT)
						(void) axis->setfont(AXIS_OTHERFONT, fntnm);
					axis->getfontsize(&fontsz, &size);
					if (fontsz == AXIS_GLOBALFONTSZ)
						(void) axis->setfontsize(AXIS_OTHERFONTSZ, size);
					axis->getlabel(&ofg, &orfg, &ogfg, &obfg);
					if (ofg == AXIS_GLOBALLABEL)
						(void) axis->setlabel(AXIS_OTHERLABEL, orfg, ogfg, obfg);
					break;
				}
				gr->getdtk(&odtk, &ordtk, &ogdtk, &obdtk);
				if (odtk == GROBJ_GLOBALDTK)
					(void) gr->setdtk(GROBJ_OTHERDTK, ordtk, ogdtk, obdtk);
				break;
			}
		}
		return;

	case INSICE_CURROBJNEWGDF:

		/* update all Grobjs as necessary */
		for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
			Raster *ras;
			Text *text;
			Vector *vec;
			Curve *crv;
			Marker *mrk;
			Rectangle *rect;
			Polygon *poly;
			Axis *axis;
			boolean rebuild_ras;
			int w, h, d;
			int ofg, obgm, obg, obndm, obndclr, ofillm, ofillclr, odtk;
			int omrkt, omrktype;
			unsigned char orfg, ogfg, obfg;
			unsigned char orbg, ogbg, obbg;
			unsigned char orbnd, ogbnd, obbnd;
			unsigned char orfill, ogfill, obfill;
			unsigned char ordtk, ogdtk, obdtk;
			unsigned long fgp, bgp, dtkp;
			unsigned long ofgp, obgp;
			int font, fontsz, tl;
			char *fntnm;
			float size, lead, orad, obw, pt, st, tt;
			int olinewd, osz, obndwd;

			switch (gr->gettype()) {
			case GROBJ_PSDOC:
				gr->getdtk(&odtk, &ordtk, &ogdtk, &obdtk);
				if (odtk != GROBJ_GLOBALDTK)
					break;
				if ((gdf_rdtk == ordtk) &&
				    (gdf_gdtk == ogdtk) &&
				    (gdf_bdtk == obdtk))
					break;
				(void) gr->setdtk(odtk, gdf_rdtk, gdf_gdtk, gdf_bdtk);
				if (pg_pixdepth == 1)
					dtkp= cmap_lookup(gdf_rdtk, gdf_gdtk, gdf_bdtk, 2);
				else
					dtkp= cmap_lookup(gdf_rdtk, gdf_gdtk, gdf_bdtk, PSEUDOCOLOR_MAPSZ);
				gr->setdtkpix(dtkp);
				break;
			case GROBJ_RASTER:
				ras= (Raster *) gr;
				ras->getrassize(&w, &h, &d);
				if (d != 1)
					break;
				ras->getfg(&ofg, &ofgp, &orfg, &ogfg, &obfg);
				ras->getbg(&obg, &obgp, &orbg, &ogbg, &obbg);
				if ((ofg != RASTER_GLOBALFG) && (obg != RASTER_GLOBALBG))
					break;
				rebuild_ras= FALSE;
				if ((ofg == RASTER_GLOBALFG) &&
				    ((orfg != gdf_rfg) ||
				     (ogfg != gdf_gfg) ||
				     (obfg != gdf_bfg))) {
					if (pg_pixdepth == 8)
						fgp= cmap_lookup(gdf_rfg, gdf_gfg, gdf_bfg, PSEUDOCOLOR_MAPSZ);
					else if (pg_pixdepth == 1)
						fgp= cmap_lookup(gdf_rfg, gdf_gfg, gdf_bfg, 2);
					ras->setfg(ofg, fgp, gdf_rfg, gdf_gfg, gdf_bfg);
					if (ras->getdrawmode() == RASTER_FULL)
						rebuild_ras= TRUE;
				}
				else
					fgp= ofgp;
				if ((obg == RASTER_GLOBALBG) &&
				    ((orbg != gdf_rbg) ||
				     (ogbg != gdf_gbg) ||
				     (obbg != gdf_bbg))) {
					if (pg_pixdepth == 8)
						bgp= cmap_lookup(gdf_rbg, gdf_gbg, gdf_bbg, PSEUDOCOLOR_MAPSZ);
					else if (pg_pixdepth == 1)
						bgp= cmap_lookup(gdf_rbg, gdf_gbg, gdf_bbg, 2);
					ras->setbg(obg, bgp, gdf_rbg, gdf_gbg, gdf_bbg);
					if (ras->getdrawmode() == RASTER_FULL)
						rebuild_ras= TRUE;
				}
				else
					bgp= obgp;
				if (fgp == bgp) {
					fgp= black_pixel;
					bgp= white_pixel;
					if (ofg == RASTER_GLOBALFG)
						ras->setfg(ofg, fgp, gdf_rfg, gdf_gfg, gdf_bfg);
					else
						ras->setfg(ofg, fgp, orfg, ogfg, obfg);
					if (obg == RASTER_GLOBALBG)
						ras->setbg(obg, bgp, gdf_rbg, gdf_gbg, gdf_bbg);
					else
						ras->setbg(obg, bgp, orbg, ogbg, obbg);
					if (ras->getdrawmode() == RASTER_FULL)
						rebuild_ras= TRUE;
				}
				if (rebuild_ras)
					ras->bldimage();
				break;
			case GROBJ_INTOBJ:
				switch (((Intobj *) gr)->getintobjtype()) {

				case INTOBJ_TEXT:
					text= (Text *) gr;
					text->getfont(&font, &fntnm);
					if ((font == TEXT_GLOBALFONT) &&
					    strcmp(fntnm, gdf_fontname))
						(void) text->setfont(font, gdf_fontname);
					text->getfontsize(&fontsz, &size, &lead);
					if ((fontsz == TEXT_GLOBALFONTSZ) &&
					    ((size != gdf_fontsize) ||
					     (lead != gdf_fontlead)))
						(void) text->setfontsize(fontsz, gdf_fontsize, gdf_fontlead);
					text->getforeground(&ofg, &orfg, &ogfg, &obfg);
					if ((ofg == TEXT_GLOBALFG) &&
					    ((orfg != gdf_rfg) ||
					     (ogfg != gdf_gfg) ||
					     (obfg != gdf_bfg)))
						(void) text->setforeground(ofg, gdf_rfg, gdf_gfg, gdf_bfg);
					text->getbackground(&obgm, &obg, &orbg, &ogbg, &obbg);
					if ((obg == TEXT_GLOBALBG) &&
					    ((orbg != gdf_rbg) ||
					     (ogbg != gdf_gbg) ||
					     (obbg != gdf_bbg)))
						(void) text->setbackground(obgm, obg, gdf_rbg, gdf_gbg, gdf_bbg);
					break;
				case INTOBJ_VECTOR:
					vec= (Vector *) gr;
					vec->getwidth(&olinewd, &size);
					if ((olinewd == VECTOR_GLOBALWIDTH) &&
					    (size != gdf_linewidth))
						(void) vec->setwidth(olinewd, gdf_linewidth);
					vec->getforeground(&ofg, &orfg, &ogfg, &obfg);
					if ((ofg == VECTOR_GLOBALFG) &&
					    ((orfg != gdf_rfg) ||
					     (ogfg != gdf_gfg) ||
					     (obfg != gdf_bfg)))
						(void) vec->setforeground(ofg, gdf_rfg, gdf_gfg, gdf_bfg);
					break;
				case INTOBJ_CURVE:
					crv= (Curve *) gr;
					crv->getwidth(&olinewd, &size);
					if ((olinewd == CURVE_GLOBALWIDTH) &&
					    (size != gdf_linewidth))
						(void) crv->setwidth(olinewd, gdf_linewidth);
					crv->getforeground(&ofg, &orfg, &ogfg, &obfg);
					if ((ofg == CURVE_GLOBALFG) &&
					    ((orfg != gdf_rfg) ||
					     (ogfg != gdf_gfg) ||
					     (obfg != gdf_bfg)))
						(void) crv->setforeground(ofg, gdf_rfg, gdf_gfg, gdf_bfg);
					break;
				case INTOBJ_MARKER:
					mrk= (Marker *) gr;
					mrk->getmarkertype(&omrkt, &omrktype);
					if ((omrkt == MARKER_GLOBALTYPE) &&
					    (omrktype != gdf_mrktype+1))
						(void) mrk->setmarkertype(omrkt, gdf_mrktype+1);
					mrk->getsize(&osz, &orad);
					if ((osz == MARKER_GLOBALSIZE) &&
					    (orad != gdf_mrksize))
						(void) mrk->setsize(osz, gdf_mrksize);
					mrk->getboundary(&obndm, &obndwd, &obw, &obndclr, &orbnd, &ogbnd, &obbnd);
					if ((obndwd == MARKER_GLOBALBNDWIDTH) &&
					    (obw != gdf_bndwidth)) {
						obw= gdf_bndwidth;
						(void) mrk->setboundary(obndm, obndwd, obw, obndclr, orbnd, ogbnd, obbnd);
					}
					if ((obndclr == MARKER_GLOBALBND) &&
					    ((orbnd != gdf_rbnd) ||
					     (ogbnd != gdf_gbnd) ||
					     (obbnd != gdf_bbnd)))
						(void) mrk->setboundary(obndm, obndwd, obw, obndclr, gdf_rbnd, gdf_gbnd, gdf_bbnd);
					mrk->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
					if ((ofillclr == MARKER_GLOBALFILL) &&
					    ((orfill != gdf_rfill) ||
					     (ogfill != gdf_gfill) ||
					     (obfill != gdf_bfill)))
						(void) mrk->setfill(ofillm, ofillclr, gdf_rfill, gdf_gfill, gdf_bfill);
					break;
				case INTOBJ_RECTANGLE:
					rect= (Rectangle *) gr;
					rect->getwidth(&obndwd, &obw);
					if ((obndwd == RECTANGLE_GLOBALBNDWIDTH) &&
					    (obw != gdf_bndwidth)) {
						obw= gdf_bndwidth;
						(void) rect->setwidth(obndwd, obw);
					}
					rect->getboundary(&obndm, &obndclr, &orbnd, &ogbnd, &obbnd);
					if ((obndclr == RECTANGLE_GLOBALBND) &&
					    ((orbnd != gdf_rbnd) ||
					     (ogbnd != gdf_gbnd) ||
					     (obbnd != gdf_bbnd)))
						(void) rect->setboundary(obndm, obndclr, gdf_rbnd, gdf_gbnd, gdf_bbnd);
					rect->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
					if ((ofillclr == RECTANGLE_GLOBALFILL) &&
					    ((orfill != gdf_rfill) ||
					     (ogfill != gdf_gfill) ||
					     (obfill != gdf_bfill)))
						(void) rect->setfill(ofillm, ofillclr, gdf_rfill, gdf_gfill, gdf_bfill);
					break;
				case INTOBJ_POLYGON:
					poly= (Polygon *) gr;
					poly->getboundary(&obndm, &obndwd, &obw, &obndclr, &orbnd, &ogbnd, &obbnd);
					if ((obndwd == POLYGON_GLOBALBNDWIDTH) &&
					    (obw != gdf_bndwidth)) {
						obw= gdf_bndwidth;
						(void) poly->setboundary(obndm, obndwd, obw, obndclr, orbnd, ogbnd, obbnd);
					}
					if ((obndclr == POLYGON_GLOBALBND) &&
					    ((orbnd != gdf_rbnd) ||
					     (ogbnd != gdf_gbnd) ||
					     (obbnd != gdf_bbnd)))
						(void) poly->setboundary(obndm, obndwd, obw, obndclr, gdf_rbnd, gdf_gbnd, gdf_bbnd);
					poly->getfill(&ofillm, &ofillclr, &orfill, &ogfill, &obfill);
					if ((ofillclr == POLYGON_GLOBALFILL) &&
					    ((orfill != gdf_rfill) ||
					     (ogfill != gdf_gfill) ||
					     (obfill != gdf_bfill)))
						(void) poly->setfill(ofillm, ofillclr, gdf_rfill, gdf_gfill, gdf_bfill);
					break;
				case INTOBJ_AXIS:
					axis= (Axis *) gr;
					axis->getaxiswidth(&olinewd, &size);
					if ((olinewd == AXIS_GLOBALWIDTH) &&
					    (size != gdf_linewidth))
						(void) axis->setaxiswidth(olinewd, gdf_linewidth);
					axis->gettick(&tl, &pt, &st, &tt, &olinewd, &size);
					if ((olinewd == AXIS_GLOBALWIDTH) &&
					    (size != gdf_linewidth))
						(void) axis->settick(tl, pt, st, tt, olinewd, gdf_linewidth);
					axis->getline(&ofg, &orfg, &ogfg, &obfg);
					if ((ofg == AXIS_GLOBALLINE) &&
					    ((orfg != gdf_rfg) ||
					     (ogfg != gdf_gfg) ||
					     (obfg != gdf_bfg)))
						(void) axis->setline(ofg, gdf_rfg, gdf_gfg, gdf_bfg);
					axis->getfont(&font, &fntnm);
					if ((font == AXIS_GLOBALFONT) &&
					    strcmp(fntnm, gdf_fontname))
						(void) axis->setfont(font, gdf_fontname);
					axis->getfontsize(&fontsz, &size);
					if ((fontsz == AXIS_GLOBALFONTSZ) &&
					    (size != gdf_fontsize))
						(void) axis->setfontsize(fontsz, gdf_fontsize);
					axis->getlabel(&ofg, &orfg, &ogfg, &obfg);
					if ((ofg == AXIS_GLOBALLABEL) &&
					    ((orfg != gdf_rfg) ||
					     (ogfg != gdf_gfg) ||
					     (obfg != gdf_bfg)))
						(void) axis->setlabel(ofg, gdf_rfg, gdf_gfg, gdf_bfg);
					break;
				}
				gr->getdtk(&odtk, &ordtk, &ogdtk, &obdtk);
				if ((odtk == GROBJ_GLOBALDTK) &&
				    ((gdf_rdtk != ordtk) ||
				     (gdf_gdtk != ogdtk) ||
				     (gdf_bdtk != obdtk))) {
					(void) gr->setdtk(odtk, gdf_rdtk, gdf_gdtk, gdf_bdtk);
					if (pg_pixdepth == 1)
						dtkp= cmap_lookup(gdf_rdtk, gdf_gdtk, gdf_bdtk, 2);
					else
						dtkp= cmap_lookup(gdf_rdtk, gdf_gdtk, gdf_bdtk, PSEUDOCOLOR_MAPSZ);
					gr->setdtkpix(dtkp);
				}
				break;
			}
		}
		break;
	}

	return;
}
