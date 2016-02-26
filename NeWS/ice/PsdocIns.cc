/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Psdoc.h"
#include "Pathdup.h"

extern "C" {
char *			gconvert(double, int, int, char *);
char *			getenv(char *);
char *			strcat(char *, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrpsd_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cppsd_proc(Menu *, Menu_item *);
extern void		delpsd_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern FILE *		doc_open(char *, char *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern void		psd_del(Psdoc *);
extern void		trpsd_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

static Psdoc *new_psd;
static boolean psd_noloc;

char psdbuf[PSDOC_MAXLINELEN+1];
static Psdoc *stdin_psd= (Psdoc *) NULL;

void
stdinpsd_proc()
{
	int len, nlines;
	char *dir;
	FILE *fp;
	unsigned long dtkpix;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;
	char errmsg[MAX_ERRMSGLEN+1];

	if ((dir= getenv("ICETMPDIR")) != (char *) NULL) {
		len= strlen(dir)+strlen("/StdInput")+1;
		if ((initpsd_filenm= new char[len]) == (char *) NULL) {
			ice_err("Cannot load initial PS document.", NONFATAL);
			return;
		}
		(void) strcpy(initpsd_filenm, dir);
		(void) strcat(initpsd_filenm, "/StdInput");
	}
	else {
		len= strlen("StdInput")+1;
		if ((initpsd_filenm= new char[len]) == (char *) NULL) {
			ice_err("Cannot load initial PS document.", NONFATAL);
			return;
		}
		(void) strcpy(initpsd_filenm, "StdInput");
	}
	if ((fp= fopen(initpsd_filenm, "w")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", initpsd_filenm);
		ice_err(errmsg, NONFATAL);
		delete initpsd_filenm;
		initpsd_filenm= (char *) NULL;
		return;
	}
	for (nlines= 0; fgets(psdbuf, PSDOC_MAXLINELEN+1, stdin) != (char *) NULL; nlines++)
		fputs(psdbuf, fp);
	(void) fclose(fp);
	if (nlines == 0) {
		ice_err("No initial PS document present on stdin.", NONFATAL);
		delete initpsd_filenm;
		initpsd_filenm= (char *) NULL;
		return;
	}

	if ((stdin_psd= new Psdoc((Dlnk **) &grobjs, initpsd_filenm, 0)) == (Psdoc *) NULL) {
		ice_err("Cannot create PS document.", NONFATAL);
		delete initpsd_filenm;
		initpsd_filenm= (char *) NULL;
		return;
	}
	npsdocs++;

	stdin_psd->setscale((float) 1., (float) 1.);
	stdin_psd->setrotation((float) 0.);
	(void) stdin_psd->setdtk(GROBJ_GLOBALDTK, gdf_rdtk, gdf_gdtk, gdf_bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(gdf_rdtk, gdf_gdtk, gdf_bdtk, 2);
	else
		dtkpix= cmap_lookup(gdf_rdtk, gdf_gdtk, gdf_bdtk, PSEUDOCOLOR_MAPSZ);
	stdin_psd->setdtkpix(dtkpix);

	if ((delitem= menuitem_create(LXMI_STRING, initpsd_filenm,
				LXMI_CLIENTDATA, (char *) stdin_psd,
				LXMI_PROC, delpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(stdin_psd);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, initpsd_filenm,
				LXMI_CLIENTDATA, (char *) stdin_psd,
				LXMI_PROC, attrpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(stdin_psd);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, initpsd_filenm,
				LXMI_CLIENTDATA, (char *) stdin_psd,
				LXMI_PROC, trpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(stdin_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, initpsd_filenm,
				LXMI_CLIENTDATA, (char *) stdin_psd,
				LXMI_PROC, cppsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(stdin_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, initpsd_filenm,
				LXMI_CLIENTDATA, (char *) stdin_psd,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(stdin_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, initpsd_filenm,
				LXMI_CLIENTDATA, (char *) stdin_psd,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(stdin_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	(void) menuitem_insert(delpsd_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpsd_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpsd_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cppsd_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppsd_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoppsd_menu, cmpopitem);

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);
	pg_draw();
	XSync(dpy, False);
	XDefineCursor(dpy, pg_cwin, std_cursor);

	return;
}

void
inspsd_proc(Menu *m, Menu_item *mi)
{
	char buf[80];

	panelitem_set(psdattr_panel, psdattr_fname, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		ice_op= PSD_INSERTATTR;
		XMapRaised(dpy, psdattr_frame);
		return;
	}

	panelitem_set(psdattr_panel, psdattr_hscale, LXPTEXT_VALUE, "1", LXPI_NULL);
	panelitem_set(psdattr_panel, psdattr_vscale, LXPTEXT_VALUE, "1", LXPI_NULL);
	panelitem_set(psdattr_panel, psdattr_rot, LXPTEXT_VALUE, "0", LXPI_NULL);

	panelitem_set(psdattr_panel, psdattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(psdattr_panel, psdattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_rdtk);
	panelitem_set(psdattr_panel, psdattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_gdtk);
	panelitem_set(psdattr_panel, psdattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", (int) gdf_bdtk);
	panelitem_set(psdattr_panel, psdattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(psdattr_panel, psdattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= PSD_INSERTATTR;
	XMapRaised(dpy, psdattr_frame);
	return;
}

void
psdinscont_proc(Panel *p, Panel_item *pi)
{
	float hscale, vscale, rot, fx, fy;
	int seq, dtk;
	char *cptr, *filenm, *buf;
	char errmsg[MAX_ERRMSGLEN+1];
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	FILE *fp;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;
	char xbuf[30], ybuf[30];

	XUnmapWindow(dpy, psdattr_frame);
	ice_op= MAIN_MENU;

	filenm= (char *) panelitem_get(psdattr_panel, psdattr_fname, LXPTEXT_VALUE);
	if (strlen(filenm) == 0) {
		ice_err("No filename specified.", NONFATAL);
		return;
	}
	if ((fp= doc_open(filenm, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		return;
	}
	(void) fclose(fp);

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

	if ((new_psd= new Psdoc((Dlnk **) &grobjs, filenm, seq)) == (Psdoc *) NULL) {
		ice_err("Cannot create PS document.", NONFATAL);
		return;
	}
	npsdocs++;

	new_psd->setscale(hscale, vscale);
	new_psd->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_psd->setclip(pth);
	(void) new_psd->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_psd->setdtkpix(dtkpix);

	ice_op= PSD_INSERTLOC;

	if (pg_loc == PG_CURSORLOC) {
		psd_noloc= FALSE;
		pgcoords_drawn= crosshairs_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_psd,
				LXMI_PROC, delpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(new_psd);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_psd,
				LXMI_PROC, attrpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(new_psd);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_psd,
				LXMI_PROC, trpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(new_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_psd,
				LXMI_PROC, cppsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(new_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_psd,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(new_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) new_psd,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(new_psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		return;
	}
	(void) menuitem_insert(delpsd_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpsd_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpsd_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cppsd_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppsd_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoppsd_menu, cmpopitem);

	loc_grobj= (Grobj *) new_psd;
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
insicepsd_rd(FILE *fp, int gdf, int newobj)
{
	float hscale, vscale, rot;
	int len, seq, dtk;
	int ir, ig, ib;
	float fx, fy;
	char *filenm;
	boolean endfound;
	char errmsg[MAX_ERRMSGLEN+1];
	FILE *psfp;
	Psdoc *psd;
	char *c, *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	int iot;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Psd: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Psd: Begin ")+len-1)= '\0';
	if ((filenm= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(filenm, ice_iobuf+strlen("%%ICE-Psd: Begin "));
	if ((psfp= doc_open(filenm, "r")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		return;
	}
	(void) fclose(psfp);

	fx= fy= 0.;
	hscale= vscale= 1.;
	rot= 0.;
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

		if (!strncmp("%%ICE-Psd: Loc ", ice_iobuf, strlen("%%ICE-Psd: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Psd: Loc "), "%f %f", &fx, &fy) != 2) {
				ice_err("Invalid PS document location value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Psd: Trans ", ice_iobuf, strlen("%%ICE-Psd: Trans "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Psd: Trans "), "%f %f %f", &rot, &hscale, &vscale) != 3) {
				ice_err("Invalid PS document transform value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Psd: Clip ", ice_iobuf, strlen("%%ICE-Psd: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Psd: Clip ");
			for ( ; isspace(*c); c++);
			len= strlen(c);
			*(c+len-1)= '\0';
			delete pathnm;
			if ((pathnm= new char[len]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete filenm;
				return;
			}
			(void) strcpy(pathnm, c);
		}

		else if (!strncmp("%%ICE-Psd: DTK ", ice_iobuf, strlen("%%ICE-Psd: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Psd: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid PS document DTK value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid PS document DTK value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid PS document red DTK value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid PS document green DTK value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid PS document blue DTK value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Psd: Seq ", ice_iobuf, strlen("%%ICE-Psd: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Psd: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid PS document sequence value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Psd: IOT ", ice_iobuf, strlen("%%ICE-Psd: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Psd: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid PS document tag value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid PS document tag value.", NONFATAL);
				delete filenm;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Psd: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("PS document end not found.", NONFATAL);
		delete filenm;
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
					delete filenm;
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
			delete filenm;
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
			if (dtk == GROBJ_GLOBALDTK) {
				rdtk= gdf_rdtk;
				gdtk= gdf_gdtk;
				bdtk= gdf_bdtk;
			}
			break;
		case INSICE_NEWOBJNEWGDF:
			if (dtk == GROBJ_GLOBALDTK)
				dtk= GROBJ_OTHERDTK;
			break;
		}
	}

	if ((psd= new Psdoc((Dlnk **) &grobjs, filenm, seq)) == (Psdoc *) NULL) {
		ice_err("Cannot create PS document.", NONFATAL);
		delete filenm;
		return;
	}
	npsdocs++;

	psd->setfloc(fx, fy, (float) pg_dpi);
	psd->setscale(hscale, vscale);
	psd->setrotation(rot);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	psd->setclip(pth);
	psd->setiotag(iot);

	(void) psd->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	psd->setdtkpix(dtkpix);

	if ((delitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) psd,
				LXMI_PROC, delpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(psd);
		delete filenm;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) psd,
				LXMI_PROC, attrpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(psd);
		menuitem_destroy(delitem);
		delete filenm;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) psd,
				LXMI_PROC, trpsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		delete filenm;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) psd,
				LXMI_PROC, cppsd_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		delete filenm;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) psd,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		delete filenm;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, filenm,
				LXMI_CLIENTDATA, (char *) psd,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		psd_del(psd);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		delete filenm;
		return;
	}
	delete filenm;
	(void) menuitem_insert(delpsd_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrpsd_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trpsd_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cppsd_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmppsd_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoppsd_menu, cmpopitem);

	return;
}

void
psdins_event(XEvent *event)
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
		if (psd_noloc == FALSE) {
			x= bevt->x+vx;
			y= bevt->y+vy;
		}
		else {
			x= 0;
			y= pg_pixheight-1;
		}
		new_psd->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		if ((delitem= menuitem_create(LXMI_STRING, new_psd->getname(),
					LXMI_CLIENTDATA, (char *) new_psd,
					LXMI_PROC, delpsd_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			psd_del(new_psd);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_psd->getname(),
					LXMI_CLIENTDATA, (char *) new_psd,
					LXMI_PROC, attrpsd_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			psd_del(new_psd);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_psd->getname(),
					LXMI_CLIENTDATA, (char *) new_psd,
					LXMI_PROC, trpsd_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			psd_del(new_psd);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_psd->getname(),
					LXMI_CLIENTDATA, (char *) new_psd,
					LXMI_PROC, cppsd_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			psd_del(new_psd);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_psd->getname(),
					LXMI_CLIENTDATA, (char *) new_psd,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			psd_del(new_psd);
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
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_psd->getname(),
					LXMI_CLIENTDATA, (char *) new_psd,
					LXMI_PROC, cmpop_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			psd_del(new_psd);
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
		(void) menuitem_insert(delpsd_menu, delitem);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrpsd_menu, attritem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trpsd_menu, tritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cppsd_menu, cpitem);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmppsd_menu, dmpitem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cmpoppsd_menu, cmpopitem);

		pg_draw();
		XDefineCursor(dpy, pg_cwin, std_cursor);
		XSync(dpy, False);
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
		ice_op= MAIN_MENU;
		return;

	case ButtonPress:
		bevt= (XButtonEvent *) event;
		if ((bevt->state & ShiftMask) || (bevt->state & ControlMask))
			psd_noloc= TRUE;
		x= bevt->x+vx;
		y= bevt->y+vy;
		new_psd->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_psd->getloc(&fx, &fy, &x, &junk);
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		new_psd->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
		new_psd->getloc(&fx, &fy, &x, &junk);
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
delpsd_proc(Menu *m, Menu_item *mi)
{
	Psdoc *psd;
	Menu_item *item;

	if ((psd= (Psdoc *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Psdoc *) NULL) {
		ice_err("Cannot locate selected document.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	psd_del(psd);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrpsd_menu, LXMI_CLIENTDATA, (char *) psd);
	menuitem_delete(attrpsd_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trpsd_menu, LXMI_CLIENTDATA, (char *) psd);
	menuitem_delete(trpsd_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cppsd_menu, LXMI_CLIENTDATA, (char *) psd);
	menuitem_delete(cppsd_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmppsd_menu, LXMI_CLIENTDATA, (char *) psd);
	menuitem_delete(dmppsd_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpoppsd_menu, LXMI_CLIENTDATA, (char *) psd);
	menuitem_delete(cmpoppsd_menu, item);
	menuitem_destroy(item);
	if (npsdocs == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
psd_del(Psdoc *psd)
{
	if (grobjs == (Grobj *) psd) {
		if (psd->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) psd->succ();
	}
	psd->unlink();

	if (psd == stdin_psd) {
		delete initpsd_filenm;
		initpsd_filenm= (char *) NULL;
	}
	delete psd;
	npsdocs--;

	return;
}
