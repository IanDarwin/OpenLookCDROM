/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Pathdup.h"

extern "C" {
char *			strcat(char *, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrpth_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		delpth_proc(Menu *, Menu_item *);
extern void		dmppth_proc(Menu *, Menu_item *);
extern FILE *		doc_open(char *, char *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		pth_del(Path *);
extern void		trpth_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Path *loc_path;
extern int loc_nvertices, loc_vertex;
extern float *loc_xvertices, *loc_yvertices;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

static Path *new_pth;
static int nvertices, allocvertices;
static float *xvert, *yvert;
static float ipp;
static int *ixvert, *iyvert;
static boolean pth_segdrawn;

void
inspth_proc(Menu *m, Menu_item *mi)
{
	panelitem_set(pthattr_panel, pthattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		ice_op= PATH_INSERTATTR;
		XMapRaised(dpy, pthattr_frame);
		return;
	}

	panelitem_set(pthattr_panel, pthattr_src, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, PTH_USERINPUT, LXPI_NULL);
	panelitem_set(pthattr_panel, pthattr_filenm, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(pthattr_panel, pthattr_closure, LXPENUM_VALUE, PATH_OPEN, LXPI_NULL);

	panelitem_set(pthattr_panel, pthattr_vis, LXPENUM_VALUE, PATH_INVISIBLE, LXPI_NULL);
	panelitem_set(pthattr_panel, pthattr_fg, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, PATH_BLACKFG, LXPI_NULL);
	panelitem_set(pthattr_panel, pthattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "0", LXPI_NULL);
	panelitem_set(pthattr_panel, pthattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "0", LXPI_NULL);
	panelitem_set(pthattr_panel, pthattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= PATH_INSERTATTR;
	XMapRaised(dpy, pthattr_frame);
	return;
}

void
pthinscont_proc(Panel *p, Panel_item *pi)
{
	char *name, *filenm, *buf, *cptr;
	int src, cl, vis, fg;
	unsigned char rfg, gfg, bfg;
	unsigned long fgp;
	Path *pth;
	char vbuf[LXADEF_MAXSTORE+1];
	int val;
	float fx, fy;
	char xbuf[30], ybuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	FILE *fp;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	void pth_readfile(Path *, char *);

	XUnmapWindow(dpy, pthattr_frame);
	ice_op= MAIN_MENU;

	name= (char *) panelitem_get(pthattr_panel, pthattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		ice_err("Unspecified path name.", NONFATAL);
		return;
	}
	for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
		if (!strcmp(name, pth->getname())) {
			ice_err("Duplicate path name.", NONFATAL);
			return;
		}
	}

	src= *((int *) panelitem_get(pthattr_panel, pthattr_src, LXPENUM_VALUE));
	if (src == PTH_FILEINPUT) {
		filenm= (char *) panelitem_get(pthattr_panel, pthattr_filenm, LXPTEXT_VALUE);
		if (strlen(filenm) == 0) {
			ice_err("Unspecified source filename.", NONFATAL);
			return;
		}
		if ((fp= doc_open(filenm, "r")) == (FILE *) NULL) {
			(void) sprintf(errmsg, "Cannot open filename '%s'.", filenm);
			ice_err(errmsg, NONFATAL);
			return;
		}
		(void) fclose(fp);
	}
	else
		filenm= (char *) NULL;

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


	if ((new_pth= new Path((Dlnk **) &paths, name, dpy, pg_pixdepth)) == (Path *) NULL) {
		ice_err("Cannot create path object.", NONFATAL);
		return;
	}
	if (new_pth->getname() == (char *) NULL) {
		ice_err("Path load error.", NONFATAL);
		pth_del(new_pth);
		return;
	}
	(void) new_pth->setclosure(cl);
	(void) new_pth->setvisibility(vis);
	if (pg_pixdepth == 8)
		fgp= cmap_lookup(rfg, gfg, bfg, PSEUDOCOLOR_MAPSZ);
	else if (pg_pixdepth == 1)
		fgp= cmap_lookup(rfg, gfg, bfg, 2);
	new_pth->setfg(fg, fgp, rfg, gfg, bfg);
	npaths++;

	if (filenm != (char *) NULL) {
		pth_readfile(new_pth, filenm);
		return;
	}

	ice_op= PATH_INSERTLOC;

	if (pg_loc == PG_CURSORLOC) {
		nvertices= 0;
		xvert= yvert= (float *) NULL;
		ixvert= iyvert= (int *) NULL;
		ipp= 1./pg_dpi;
		pth_segdrawn= pgcoords_drawn= crosshairs_drawn= FALSE;
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
			pth_del(new_pth);
			ice_op= MAIN_MENU;
			return;
		}

		if (strlen(vbuf) == 0) {
			ice_err("Number of vertices unspecified.", NONFATAL);
			pth_del(new_pth);
			ice_op= MAIN_MENU;
			return;
		}
		loc_nvertices= (unsigned char) strtol(vbuf, &cptr, 10);
		if ((cptr == vbuf) || (*cptr != '\0')) {
			ice_err("Invalid number of vertices.", NONFATAL);
			pth_del(new_pth);
			ice_op= MAIN_MENU;
			return;
		}
		if (loc_nvertices < 2) {
			ice_err("There must be at least two vertices.", NONFATAL);
			pth_del(new_pth);
			ice_op= MAIN_MENU;
			return;
		}
		if ((loc_xvertices= new float[loc_nvertices]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			pth_del(new_pth);
			ice_op= MAIN_MENU;
			return;
		}
		if ((loc_yvertices= new float[loc_nvertices]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			pth_del(new_pth);
			ice_op= MAIN_MENU;
			delete loc_xvertices;
			return;
		}
		loc_vertex= 0;
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_pth,
				LXMI_PROC, delpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(new_pth);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_pth,
				LXMI_PROC, attrpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(new_pth);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_pth,
				LXMI_PROC, trpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(new_pth);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		if (pg_loc == PG_TEXTLOC) {
			delete loc_xvertices;
			delete loc_yvertices;
		}
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_pth,
				LXMI_PROC, dmppth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(new_pth);
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
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(delpth_menu, delitem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpth_menu, attritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpth_menu, tritem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppth_menu, dmpitem);

	loc_path= new_pth;
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
pth_readfile(Path *pth, char *filenm)
{
	FILE *fp;
	char buf[80];
	int nvert, n;
	float *xvert, *yvert;
	char errmsg[MAX_ERRMSGLEN+1];
	Menu_item *delitem, *tritem, *attritem, *dmpitem;

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	if ((fp= doc_open(filenm, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open filename '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		pth_del(pth);
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}

	for (nvert= 0; fgets(buf, 80, fp) != (char *) NULL; nvert++);
	if (nvert < 2) {
		ice_err("A path must have at least two vertices.", NONFATAL);
		pth_del(pth);
		(void) fclose(fp);
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	if ((xvert= new float[nvert]) == (float *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		pth_del(pth);
		(void) fclose(fp);
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	if ((yvert= new float[nvert]) == (float *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		pth_del(pth);
		(void) fclose(fp);
		delete xvert;
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}

	if (fseek(fp, 0L, 0) != 0) {
		ice_err("File seek error.", NONFATAL);
		pth_del(pth);
		(void) fclose(fp);
		delete xvert;
		delete yvert;
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	for (n= 0; (fgets(buf, 80, fp) != (char *) NULL) && (n < nvert); n++) {
		if (sscanf(buf, "%f %f", &(xvert[n]), &(yvert[n])) != 2) {
			ice_err("File format error.", NONFATAL);
			pth_del(pth);
			(void) fclose(fp);
			delete xvert;
			delete yvert;
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			return;
		}
		switch (pg_units) {
		case PG_PIXELS:
			xvert[n]/= pg_dpi;
			yvert[n]/= pg_dpi;
			break;
		case PG_POINTS:
			xvert[n]/= 72.;
			yvert[n]/= 72.;
			break;
		case PG_INCHES:
			break;
		case PG_USER:
			xvert[n]= ((xvert[n]-pg_xru)/pg_hsi)+pg_xri;
			yvert[n]= ((yvert[n]-pg_yru)/pg_vsi)+pg_yri;
			break;
		}
	}
	if (n < nvert) {
		ice_err("File read error.", NONFATAL);
		pth_del(pth);
		(void) fclose(fp);
		delete xvert;
		delete yvert;
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	(void) fclose(fp);

	pth->setvertices(nvert, xvert, yvert);

	if ((delitem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, delpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, attrpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		menuitem_destroy(delitem);
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, trpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, dmppth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		return;
	}
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(delpth_menu, delitem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpth_menu, attritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpth_menu, tritem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppth_menu, dmpitem);

	if (pth->getvisibility() == PATH_VISIBLE) {
		pth->x11draw(pg_dpi, pg_pixheight, pg_cpm);
		canvas_flush(pg_canvas);
	}
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
insicepth_rd(FILE *fp)
{
	char *name, *oldname, *newname;
	int nvertices, rvertices;
	float *xvertices, *yvertices;
	int cl, vis, fg, len;
	int ir, ig, ib;
	unsigned char rfg, gfg, bfg;
	unsigned long fgp;
	boolean endfound, dupfound;
	char errmsg[MAX_ERRMSGLEN+1];
	Path *pth;
	Pathdup *duppth;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;

	name= oldname= newname= (char *) NULL;
	len= strlen(ice_iobuf+strlen("%%ICE-Path: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Path: Begin ")+len-1)= '\0';
	if ((newname= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(newname, ice_iobuf+strlen("%%ICE-Path: Begin "));
	for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
		if (!strcmp(newname, pth->getname()))
			break;
	}
	if (pth != (Path *) NULL) {
		dupfound= TRUE;
		oldname= newname;
		for (;;) {
			name= newname;
			if ((newname= new char[strlen(name)+2]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				return;
			}
			(void) strcpy(newname, name);
			(void) strcat(newname, "+");
			if (name != oldname)
				delete name;
			for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
				if (!strcmp(newname, pth->getname()))
					break;
			}
			if (pth == (Path *) NULL)
				break;
		}
	}
	else
		dupfound= FALSE;

	nvertices= 0;
	xvertices= yvertices= (float *) NULL;
	cl= PATH_CLOSED;
	vis= PATH_INVISIBLE;
	fg= PATH_BLACKFG;
	rfg= gfg= bfg= 0;

	endfound= FALSE;
	while (fgets(ice_iobuf, INPUT_BUFSZ, fp) != (char *) NULL) {
		insice_lineno++;

		if (strncmp("%%ICE-", ice_iobuf, strlen("%%ICE-")))
			continue;

		if (!strncmp("%%ICE-Path: Flags ", ice_iobuf, strlen("%%ICE-Path: Flags "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Path: Flags "), "%d %d", &cl, &vis) != 2) {
				ice_err("Invalid path flags value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if ((cl != PATH_CLOSED) &&
			    (cl != PATH_OPEN)) {
				ice_err("Invalid path closure value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if ((vis != PATH_INVISIBLE) &&
			    (vis != PATH_VISIBLE)) {
				ice_err("Invalid path visibility value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
		}

		else if (!strncmp("%%ICE-Path: FG ", ice_iobuf, strlen("%%ICE-Path: FG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Path: FG "), "%d %d %d %d", &fg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid path foreground value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if ((fg != PATH_BLACKFG) &&
			    (fg != PATH_WHITEFG) &&
			    (fg != PATH_OTHERFG)) {
				ice_err("Invalid path foreground value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid path red value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid path green value.", NONFATAL);
				delete name;
				delete xvertices;
				delete yvertices;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid path blue value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			rfg= (unsigned char) ir;
			gfg= (unsigned char) ig;
			bfg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Path: Vert ", ice_iobuf, strlen("%%ICE-Path: Vert "))) {
			if (nvertices != 0) {
				ice_err("Too many path vertices specified.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if (sscanf(ice_iobuf+strlen("%%ICE-Path: Vert "), "%d", &nvertices) != 1) {
				ice_err("Invalid path vertices value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if (nvertices <= 0) {
				ice_err("Invalid path vertices value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if ((xvertices= new float[nvertices]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete newname;
				return;
			}
			if ((yvertices= new float[nvertices]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete newname;
				delete xvertices;
				return;
			}
			rvertices= 0;
		}

		else if (!strncmp("%%ICE-Path: Vert+ ", ice_iobuf, strlen("%%ICE-Path: Vert+ "))) {
			if (nvertices == 0) {
				ice_err("Unknown number of path vertices.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if (rvertices == nvertices) {
				ice_err("Too many path vertices specified.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			if (sscanf(ice_iobuf+strlen("%%ICE-Path: Vert+ "), "%f %f", &(xvertices[rvertices]), &(yvertices[rvertices])) != 2) {
				ice_err("Invalid path vertices value.", NONFATAL);
				delete newname;
				delete xvertices;
				delete yvertices;
				return;
			}
			rvertices++;
		}

		else if (!strcmp("%%ICE-Path: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Path end not found.", NONFATAL);
		delete newname;
		delete xvertices;
		delete yvertices;
		return;
	}
	if (rvertices < nvertices) {
		ice_err("Path vertices specification incomplete.", NONFATAL);
		delete newname;
		delete xvertices;
		delete yvertices;
		return;
	}

	if ((pth= new Path((Dlnk **) &paths, newname, dpy, pg_pixdepth)) == (Path *) NULL) {
		ice_err("Cannot create path object.", NONFATAL);
		delete newname;
		delete xvertices;
		delete yvertices;
		return;
	}
	if (pth->getname() == (char *) NULL) {
		ice_err("Path load error.", NONFATAL);
		pth_del(pth);
		return;
	}
	npaths++;

	(void) pth->setclosure(cl);
	(void) pth->setvisibility(vis);
	if (pg_pixdepth == 8)
		fgp= cmap_lookup(rfg, gfg, bfg, PSEUDOCOLOR_MAPSZ);
	else if (pg_pixdepth == 1)
		fgp= cmap_lookup(rfg, gfg, bfg, 2);
	pth->setfg(fg, fgp, rfg, gfg, bfg);
	pth->setvertices(nvertices, xvertices, yvertices);

	if (dupfound) {
		if ((duppth= new Pathdup((Dlnk **) &duppaths, oldname, newname)) == (Pathdup *) NULL) {
			ice_err("Cannot create pathdup object.", NONFATAL);
			delete newname;
			pth_del(pth);
			return;
		}
	}
	else
		delete newname;

	if ((delitem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, delpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, attrpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, trpth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, pth->getname(),
				LXMI_CLIENTDATA, (char *) pth,
				LXMI_PROC, dmppth_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		pth_del(pth);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(delpth_menu, delitem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpth_menu, attritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpth_menu, tritem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppth_menu, dmpitem);

	return;
}

void
pthins_event(XEvent *event)
{
	XButtonEvent *bevt;
	XMotionEvent *mevt;
	int vx, vy, x, y, i;
	float fx, fy, *oxvert, *oyvert;
	int *oixvert, *oiyvert;
	Menu_item *delitem, *attritem;
	Menu_item *tritem, *dmpitem;
	void pth_drawseg(), pth_drawoutline();

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	if (xvert == (float *) NULL) {
		allocvertices= 30;
		if ((xvert= new float[allocvertices]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			pth_del(new_pth);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((yvert= new float[allocvertices]) == (float *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete xvert;
			pth_del(new_pth);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((ixvert= new int[allocvertices]) == (int *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete xvert;
			delete yvert;
			pth_del(new_pth);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((iyvert= new int[allocvertices]) == (int *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete xvert;
			delete yvert;
			delete ixvert;
			pth_del(new_pth);
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
		if (pth_segdrawn) {
			pth_segdrawn= FALSE;
			if ((nvertices > 1) &&
			    (new_pth->getclosure() == PATH_CLOSED) &&
			    (bevt->button != Button3)) {
				XDrawLine(dpy, pg_cpm, pg_xgc, ixvert[0], iyvert[0], ixvert[nvertices], iyvert[nvertices]);
				XDrawLine(dpy, pg_cwin, pg_xgc, ixvert[0]-vx, iyvert[0]-vy, ixvert[nvertices]-vx, iyvert[nvertices]-vy);
			}
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		if (nvertices == 0) {
			xvert[0]= ipp*((float) x);
			yvert[0]= ipp*((float) (pg_pixheight-1-y));
			ixvert[0]= x;
			iyvert[0]= y;
			nvertices++;
			return;
		}
		else if (bevt->button != Button3) {
			nvertices++;
			if (nvertices == allocvertices) {
				allocvertices+= 30;
				oxvert= xvert;
				oyvert= yvert;
				oixvert= ixvert;
				oiyvert= iyvert;
				if ((xvert= new float[allocvertices]) == (float *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete oxvert;
					delete oyvert;
					delete oixvert;
					delete oiyvert;
					pth_del(new_pth);
					XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
					ice_op= MAIN_MENU;
					return;
				}
				if ((yvert= new float[allocvertices]) == (float *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete oxvert;
					delete oyvert;
					delete oixvert;
					delete oiyvert;
					delete xvert;
					pth_del(new_pth);
					XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
					ice_op= MAIN_MENU;
					return;
				}
				if ((ixvert= new int[allocvertices]) == (int *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete oxvert;
					delete oyvert;
					delete oixvert;
					delete oiyvert;
					delete xvert;
					delete yvert;
					pth_del(new_pth);
					XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
					ice_op= MAIN_MENU;
					return;
				}
				if ((iyvert= new int[allocvertices]) == (int *) NULL) {
					ice_err("Memory allocation error.", NONFATAL);
					delete oxvert;
					delete oyvert;
					delete oixvert;
					delete oiyvert;
					delete xvert;
					delete yvert;
					delete ixvert;
					pth_del(new_pth);
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
				delete oxvert;
				delete oyvert;
				delete oixvert;
				delete oiyvert;
			}
			xvert[nvertices-1]= ipp*((float) x);
			yvert[nvertices-1]= ipp*((float) (pg_pixheight-1-y));
			ixvert[nvertices-1]= x;
			iyvert[nvertices-1]= y;
			return;
		}
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		nvertices++;
		xvert[nvertices-1]= ipp*((float) x);
		yvert[nvertices-1]= ipp*((float) (pg_pixheight-1-y));
		pth_drawoutline();
		if (nvertices < allocvertices) {
			oxvert= xvert;
			oyvert= yvert;
			if ((xvert= new float[nvertices]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oxvert;
				delete oyvert;
				delete ixvert;
				delete iyvert;
				pth_del(new_pth);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
			if ((yvert= new float[nvertices]) == (float *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oxvert;
				delete oyvert;
				delete ixvert;
				delete iyvert;
				delete xvert;
				pth_del(new_pth);
				XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
				ice_op= MAIN_MENU;
				return;
			}
			for (i= 0; i < nvertices; i++) {
				xvert[i]= oxvert[i];
				yvert[i]= oyvert[i];
			}
			delete oxvert;
			delete oyvert;
		}
		new_pth->setvertices(nvertices, xvert, yvert);
		delete ixvert;
		delete iyvert;
		if ((delitem= menuitem_create(LXMI_STRING, new_pth->getname(),
					LXMI_CLIENTDATA, (char *) new_pth,
					LXMI_PROC, delpth_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			pth_del(new_pth);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_pth->getname(),
					LXMI_CLIENTDATA, (char *) new_pth,
					LXMI_PROC, attrpth_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			pth_del(new_pth);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_pth->getname(),
					LXMI_CLIENTDATA, (char *) new_pth,
					LXMI_PROC, trpth_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			pth_del(new_pth);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_pth->getname(),
					LXMI_CLIENTDATA, (char *) new_pth,
					LXMI_PROC, dmppth_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			pth_del(new_pth);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(delpth_menu, delitem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrpth_menu, attritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trpth_menu, tritem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmppth_menu, dmpitem);
		if (new_pth->getvisibility() == PATH_VISIBLE) {
			new_pth->x11draw(pg_dpi, pg_pixheight, pg_cpm);
			canvas_flush(pg_canvas);
		}
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
		xvert[nvertices]= fx;
		yvert[nvertices]= fy;
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		fx= ipp*((float) x);
		fy= ipp*((float) (pg_pixheight-1-y));
		xvert[nvertices]= fx;
		yvert[nvertices]= fy;
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (pth_segdrawn)
		pth_drawseg();
	if (nvertices > 0) {
		ixvert[nvertices]= x;
		iyvert[nvertices]= y;
		pth_drawseg();
		pth_segdrawn= TRUE;
	}
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
delpth_proc(Menu *m, Menu_item *mi)
{
	Path *pth;
	Menu_item *item;
	int vis;

	if ((pth= (Path *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Path *) NULL) {
		ice_err("Cannot locate selected path object.", NONFATAL);
		return;
	}

	if (pth->getreferences() > 0) {
		ice_err("Cannot delete a referenced path object.", NONFATAL);
		return;
	}

	if (ice_op != DELETE_ALL) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		vis= pth->getvisibility();
	}
	pth_del(pth);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrpth_menu, LXMI_CLIENTDATA, (char *) pth);
	menuitem_delete(attrpth_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trpth_menu, LXMI_CLIENTDATA, (char *) pth);
	menuitem_delete(trpth_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmppth_menu, LXMI_CLIENTDATA, (char *) pth);
	menuitem_delete(dmppth_menu, item);
	menuitem_destroy(item);
	if (npaths == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Path"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}
	if (ice_op != DELETE_ALL) {
		if (vis == PATH_VISIBLE)
			pg_draw();
		XSync(dpy, False);
		XDefineCursor(dpy, pg_cwin, std_cursor);
	}
	return;
}

void
pth_del(Path *pth)
{
	if (paths == pth) {
		if (pth->succ() == (Dlnk *) NULL)
			paths= (Path *) NULL;
		else
			paths= (Path *) pth->succ();
	}
	pth->unlink();
	delete pth;
	npaths--;
	return;
}

void
pth_drawseg()
{
	int vx, vy;
	int x0, x1, y0, y1;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	x0= ixvert[nvertices-1];
	y0= iyvert[nvertices-1];
	x1= ixvert[nvertices];
	y1= iyvert[nvertices];
	XDrawLine(dpy, pg_cpm, pg_xgc, x0, y0, x1, y1);
	XDrawLine(dpy, pg_cwin, pg_xgc, x0-vx, y0-vy, x1-vx, y1-vy);
	if ((nvertices > 1) && (new_pth->getclosure() == PATH_CLOSED)) {
		XDrawLine(dpy, pg_cpm, pg_xgc, ixvert[0], iyvert[0], x1, y1);
		XDrawLine(dpy, pg_cwin, pg_xgc, ixvert[0]-vx, iyvert[0]-vy, x1-vx, y1-vy);
	}
	return;
}

void
pth_drawoutline()
{
	int vx, vy;
	int x0, x1, y0, y1, i;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	x0= ixvert[0];
	y0= iyvert[0];
	for (i= 1; i < nvertices; i++, x0= x1, y0= y1) {
		x1= ixvert[i];
		y1= iyvert[i];
		XDrawLine(dpy, pg_cpm, pg_xgc, x0, y0, x1, y1);
		XDrawLine(dpy, pg_cwin, pg_xgc, x0-vx, y0-vy, x1-vx, y1-vy);
	}
	if ((nvertices > 2) && (new_pth->getclosure() == PATH_CLOSED)) {
		XDrawLine(dpy, pg_cpm, pg_xgc, ixvert[0], iyvert[0], x1, y1);
		XDrawLine(dpy, pg_cwin, pg_xgc, ixvert[0]-vx, iyvert[0]-vy, x1-vx, y1-vy);
	}
	return;
}
