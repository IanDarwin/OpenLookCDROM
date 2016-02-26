/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Vector.h"
#include "Pathdup.h"

extern "C" {
char *			gconvert(double, int, int, char *);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		attrvec_proc(Menu *, Menu_item *);
extern unsigned long	cmap_lookup(unsigned char, unsigned char, unsigned char, int);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cpvec_proc(Menu *, Menu_item *);
extern void		delvec_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		drawcrosshairs();
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		pg_erasecoords();
extern void		pg_showcoords(int, int, int, int, float, float, int, int);
extern int		str2flt(char *, float **, int *);
extern void		trvec_proc(Menu *, Menu_item *);
extern void		vec_del(Vector *);

extern char ice_iobuf[];
extern int insice_lineno;

extern Grobj *loc_grobj;
extern boolean pgcoords_drawn;
extern boolean crosshairs_drawn;
extern crosshairs_x, crosshairs_y;

extern Pathdup *duppaths;

boolean vec_drawn;
int vec_x, vec_y;
int vec_ex, vec_ey;

static Vector *new_vec;

void
insvec_proc(Menu *m, Menu_item *mi)
{
	char buf[80];

	panelitem_set(vecattr_panel, vecattr_name, LXPTEXT_VALUE, "", LXPTEXT_RDONLY, FALSE, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_mvloc1, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_mvloc2, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	if (ins_newobj == INS_LASTEDIT) {
		vec_drawn= FALSE;
		ice_op= VEC_INSERTATTR;
		XMapRaised(dpy, vecattr_frame);
		return;
	}

	panelitem_set(vecattr_panel, vecattr_linewd, LXPENUM_VALUE, VECTOR_GLOBALWIDTH, LXPI_NULL);
	(void) gconvert((double) gdf_linewidth, 10, 0, buf);
	panelitem_set(vecattr_panel, vecattr_lwidth, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_linestyle, LXPENUM_VALUE, VECTOR_SOLID, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_dashstyle, LXPI_STATE, LXPI_INACTIVE, LXPENUM_VALUE, VECTOR_SIMPLEDASH, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_dashlen, LXPTEXT_VALUE, "4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_dashgaplen, LXPTEXT_VALUE, "4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_dashpattern, LXPTEXT_VALUE, "4 4", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_dashoffset, LXPTEXT_VALUE, "0", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_capstyle, LXPENUM_VALUE, VECTOR_BUTTCAP, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_ptr, LXPENUM_VALUE, VECTOR_NOPTR, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_ptrstyle, LXPENUM_VALUE, VECTOR_OPENPTR, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_ptrwd, LXPTEXT_VALUE, "10", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_ptrolen, LXPTEXT_VALUE, "10", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(vecattr_panel, vecattr_ptrilen, LXPTEXT_VALUE, "10", LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_fg, LXPENUM_VALUE, VECTOR_GLOBALFG, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rfg);
	panelitem_set(vecattr_panel, vecattr_rfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gfg);
	panelitem_set(vecattr_panel, vecattr_gfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bfg);
	panelitem_set(vecattr_panel, vecattr_bfg, LXPI_STATE, LXPI_INACTIVE, LXPTEXT_VALUE, buf, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_clip, LXPTEXT_VALUE, "", LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_dtk, LXPENUM_VALUE, GROBJ_GLOBALDTK, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_rdtk);
	panelitem_set(vecattr_panel, vecattr_rdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_gdtk);
	panelitem_set(vecattr_panel, vecattr_gdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	(void) sprintf(buf, "%1d", gdf_bdtk);
	panelitem_set(vecattr_panel, vecattr_bdtk, LXPTEXT_VALUE, buf, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);

	panelitem_set(vecattr_panel, vecattr_seq, LXPTEXT_VALUE, "0", LXPI_NULL);

	vec_drawn= FALSE;
	ice_op= VEC_INSERTATTR;
	XMapRaised(dpy, vecattr_frame);
	return;
}

void
vecinscont_proc(Panel *p, Panel_item *pi)
{
	char *cptr, *name, *buf;
	float width, dl, dgl, *dp, dpo, pwd, pol, pil;
	int lw, ls, ds, cs, ptr, ptrs, dpl, fg, dtk, seq;
	unsigned char rfg, gfg, bfg;
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

	XUnmapWindow(dpy, vecattr_frame);
	ice_op= MAIN_MENU;

	dp= (float *) NULL;

	name= (char *) panelitem_get(vecattr_panel, vecattr_name, LXPTEXT_VALUE);
	if (strlen(name) == 0) {
		(void) sprintf(nmbuf, "UnnamedVector-%d", unnamed_vectors++);
		name= nmbuf;
	}

	lw= *((int *) panelitem_get(vecattr_panel, vecattr_linewd, LXPENUM_VALUE));
	switch (lw) {
	case VECTOR_GLOBALWIDTH:
		width= gdf_linewidth;
		break;
	case VECTOR_OTHERWIDTH:
		buf= (char *) panelitem_get(vecattr_panel, vecattr_lwidth, LXPTEXT_VALUE);
		width= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid width value.", NONFATAL);
			delete dp;
			return;
		}
		if (width < 0.) {
			ice_err("Width value must be non-negative.", NONFATAL);
			delete dp;
			return;
		}
		break;
	}

	ls= *((int *) panelitem_get(vecattr_panel, vecattr_linestyle, LXPENUM_VALUE));
	switch (ls) {
	case VECTOR_SOLID:
		if (str2flt("4 4", &dp, &dpl) != GROBJ_SUCCESS) {
			ice_err("Cannot convert dash pattern.", NONFATAL);
			delete dp;
			return;
		}
		dpo= 0.;
		ds= VECTOR_SIMPLEDASH;
		break;
	case VECTOR_DASHED:
		ds= *((int *) panelitem_get(vecattr_panel, vecattr_dashstyle, LXPENUM_VALUE));
		switch (ds) {
		case VECTOR_SIMPLEDASH:
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashlen, LXPTEXT_VALUE);
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
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashgaplen, LXPTEXT_VALUE);
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
		case VECTOR_COMPLEXDASH:
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashpattern, LXPTEXT_VALUE);
			if (str2flt(buf, &dp, &dpl) != GROBJ_SUCCESS) {
				ice_err("Cannot convert dash pattern.", NONFATAL);
				delete dp;
				return;
			}
			buf= (char *) panelitem_get(vecattr_panel, vecattr_dashoffset, LXPTEXT_VALUE);
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

	cs= *((int *) panelitem_get(vecattr_panel, vecattr_capstyle, LXPENUM_VALUE));

	ptr= *((int *) panelitem_get(vecattr_panel, vecattr_ptr, LXPENUM_VALUE));
	switch (ptr) {
	case VECTOR_NOPTR:
		ptrs= VECTOR_OPENPTR;
		pwd= pol= pil= 10.;
		break;
	case VECTOR_ORIGPTR:
	case VECTOR_TERMPTR:
	case VECTOR_BOTHPTR:
		ptrs= *((int *) panelitem_get(vecattr_panel, vecattr_ptrstyle, LXPENUM_VALUE));
		buf= (char *) panelitem_get(vecattr_panel, vecattr_ptrwd, LXPTEXT_VALUE);
		pwd= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid pointer width value.", NONFATAL);
			delete dp;
			return;
		}
		if (pwd < 0.) {
			ice_err("Invalid pointer width value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(vecattr_panel, vecattr_ptrolen, LXPTEXT_VALUE);
		pol= (float) strtod(buf, &cptr);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid pointer outside length value.", NONFATAL);
			delete dp;
			return;
		}
		if (pol < 0.) {
			ice_err("Invalid pointer outside length value.", NONFATAL);
			delete dp;
			return;
		}
		if (ptrs == VECTOR_CLOSEDPTR) {
			buf= (char *) panelitem_get(vecattr_panel, vecattr_ptrilen, LXPTEXT_VALUE);
			pil= (float) strtod(buf, &cptr);
			if ((cptr == buf) || (*cptr != '\0')) {
				ice_err("Invalid pointer inside length value.", NONFATAL);
				delete dp;
				return;
			}
			if (pil < 0.) {
				ice_err("Invalid pointer inside length value.", NONFATAL);
				delete dp;
				return;
			}
		}
		else
			pil= pol;
		break;
	}

	fg= *((int *) panelitem_get(vecattr_panel, vecattr_fg, LXPENUM_VALUE));
	switch (fg) {
	case VECTOR_GLOBALFG:
		rfg= gdf_rfg;
		gfg= gdf_gfg;
		bfg= gdf_bfg;
		break;
	case VECTOR_BLACKFG:
		rfg= gfg= bfg= 0;
		break;
	case VECTOR_WHITEFG:
		rfg= gfg= bfg= 255;
		break;
	case VECTOR_OTHERFG:
		buf= (char *) panelitem_get(vecattr_panel, vecattr_rfg, LXPTEXT_VALUE);
		rfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid red foreground value.", NONFATAL);
			delete dp;
			return;
		}
		if ((rfg < 0) || (rfg > 255)) {
			ice_err("Invalid red foreground value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(vecattr_panel, vecattr_gfg, LXPTEXT_VALUE);
		gfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid green foreground value.", NONFATAL);
			delete dp;
			return;
		}
		if ((gfg < 0) || (gfg > 255)) {
			ice_err("Invalid green foreground value.", NONFATAL);
			delete dp;
			return;
		}
		buf= (char *) panelitem_get(vecattr_panel, vecattr_bfg, LXPTEXT_VALUE);
		bfg= (unsigned char) strtol(buf, &cptr, 10);
		if ((cptr == buf) || (*cptr != '\0')) {
			ice_err("Invalid blue foreground value.", NONFATAL);
			delete dp;
			return;
		}
		if ((bfg < 0) || (bfg > 255)) {
			ice_err("Invalid blue foreground value.", NONFATAL);
			delete dp;
			return;
		}
		break;
	}

	pathnm= (char *) panelitem_get(vecattr_panel, vecattr_clip, LXPTEXT_VALUE);
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

	dtk= *((int *) panelitem_get(vecattr_panel, vecattr_dtk, LXPENUM_VALUE));
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_rdtk, LXPTEXT_VALUE);
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_gdtk, LXPTEXT_VALUE);
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
		buf= (char *) panelitem_get(vecattr_panel, vecattr_bdtk, LXPTEXT_VALUE);
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

	buf= (char *) panelitem_get(vecattr_panel, vecattr_seq, LXPTEXT_VALUE);
	seq= (int) strtol(buf, &cptr, 10);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid sequence value.", NONFATAL);
		delete dp;
		return;
	}

	if ((new_vec= new Vector((Dlnk **) &grobjs, name, seq)) == (Vector *) NULL) {
		ice_err("Cannot create vector object.", NONFATAL);
		delete dp;
		return;
	}
	nvectors++;
	(void) new_vec->setwidth(lw, width);
	(void) new_vec->setlinestyle(ls);
	(void) new_vec->setdashstyle(ds, dp, dpl, dpo);
	(void) new_vec->setcapstyle(cs);
	(void) new_vec->setptrstyle(ptr, ptrs, pwd, pol, pil);
	(void) new_vec->setforeground(fg, rfg, gfg, bfg);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	new_vec->setclip(pth);

	(void) new_vec->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	new_vec->setdtkpix(dtkpix);

	ice_op= VEC_INSERTLOC1;

	if (pg_loc == PG_CURSORLOC) {
		pgcoords_drawn= crosshairs_drawn= FALSE;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, delvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, attrvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, trvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, cpvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_vec,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(new_vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		return;
	}
	(void) menuitem_insert(delvec_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrvec_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trvec_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpvec_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpvec_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopvec_menu, cmpopitem);

	loc_grobj= (Grobj *) new_vec;
	panelitem_set(locattr_panel, locattr_lab, LXPI_STRING, "Vector Origin", LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
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
insicevec_rd(FILE *fp, int gdf, int newobj)
{
	char *name, *c;
	float width, dl, dgl, *dp, dpo, pwd, pol, pil;
	int lw, ls, ds, cs, ptr, ptrs, dpl, dplx;
	int len, fg, dtk, seq;
	int ir, ig, ib;
	float ox, oy, tx, ty;
	boolean endfound;
	char *pathnm;
	Path *pth;
	unsigned char rdtk, gdtk, bdtk;
	unsigned long dtkpix;
	unsigned char rfg, gfg, bfg;
	int iot;
	char nmbuf[30];
	char errmsg[MAX_ERRMSGLEN+1];
	Vector *vec;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	len= strlen(ice_iobuf+strlen("%%ICE-Vec: Begin "));
	if (len == 0)
		return;
	*(ice_iobuf+strlen("%%ICE-Vec: Begin ")+len-1)= '\0';
	if ((name= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(name, ice_iobuf+strlen("%%ICE-Vec: Begin "));
	if (!strncmp(name, "UnnamedVector-", strlen("UnnamedVector-"))) {
		delete name;
		(void) sprintf(nmbuf, "UnnamedVector-%d", unnamed_vectors++);
		if ((name= new char[strlen(nmbuf)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(name, nmbuf);
	}

	ox= oy= tx= ty= 0.;
	lw= VECTOR_GLOBALWIDTH;
	width= gdf_linewidth;
	ls= VECTOR_SOLID;
	ds= VECTOR_SIMPLEDASH;
	dl= dgl= 4.;
	if (str2flt("4 4", &dp, &dpl) != GROBJ_SUCCESS) {
		ice_err("Cannot convert dash pattern.", NONFATAL);
		delete name;
		return;
	}
	dpo= 0.;
	cs= VECTOR_BUTTCAP;
	ptr= VECTOR_NOPTR;
	ptrs= VECTOR_OPENPTR;
	pwd= pol= pil= 10.;
	fg= VECTOR_GLOBALFG;
	rfg= gdf_rfg;
	gfg= gdf_gfg;
	bfg= gdf_bfg;
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

		if (!strncmp("%%ICE-Vec: Loc ", ice_iobuf, strlen("%%ICE-Vec: Loc "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: Loc "), "%f %f %f %f", &ox, &oy, &tx, &ty) != 4) {
				ice_err("Invalid vector location value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Vec: Line ", ice_iobuf, strlen("%%ICE-Vec: Line "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: Line "), "%d %f %d %d", &lw, &width, &ls, &cs) != 4) {
				ice_err("Invalid vector line value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((lw != VECTOR_GLOBALWIDTH) &&
			    (lw != VECTOR_OTHERWIDTH)) {
				ice_err("Invalid vector line width value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (width < 0.) {
				ice_err("Invalid vector line width value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ls != VECTOR_SOLID) &&
			    (ls != VECTOR_DASHED)) {
				ice_err("Invalid vector line style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((cs != VECTOR_BUTTCAP) &&
			    (cs != VECTOR_ROUNDCAP) &&
			    (cs != VECTOR_SQUARECAP)) {
				ice_err("Invalid vector cap style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Vec: Dash ", ice_iobuf, strlen("%%ICE-Vec: Dash "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: Dash "), "%d %f %d", &ds, &dpo, &dpl) != 3) {
				ice_err("Invalid vector dash value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ds != VECTOR_SIMPLEDASH) &&
			    (ds != VECTOR_COMPLEXDASH)) {
				ice_err("Invalid vector dash style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (dpo < 0.) {
				ice_err("Invalid vector dash pattern offset value.", NONFATAL);
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
			c= ice_iobuf+strlen("%%ICE-Vec: Dash ");
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

		else if (!strncmp("%%ICE-Vec: Ptr ", ice_iobuf, strlen("%%ICE-Vec: Ptr "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: Ptr "), "%d %d %f %f %f", &ptr, &ptrs, &pwd, &pol, &pil) != 5) {
				ice_err("Invalid vector pointer value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ptr != VECTOR_NOPTR) &&
			    (ptr != VECTOR_ORIGPTR) &&
			    (ptr != VECTOR_TERMPTR) &&
			    (ptr != VECTOR_BOTHPTR)) {
				ice_err("Invalid vector pointer value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ptrs != VECTOR_OPENPTR) &&
			    (ptrs != VECTOR_CLOSEDPTR)) {
				ice_err("Invalid vector pointer style value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (pwd < 0.) {
				ice_err("Invalid vector pointer width value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (pol < 0.) {
				ice_err("Invalid vector pointer outside length value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (pil < 0.) {
				ice_err("Invalid vector pointer inside length value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Vec: FG ", ice_iobuf, strlen("%%ICE-Vec: FG "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: FG "), "%d %d %d %d", &fg, &ir, &ig, &ib) != 4) {
				ice_err("Invalid vector foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((fg != VECTOR_GLOBALFG) &&
			    (fg != VECTOR_BLACKFG) &&
			    (fg != VECTOR_WHITEFG) &&
			    (fg != VECTOR_OTHERFG)) {
				ice_err("Invalid vector foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid vector red foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid vector green foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid vector blue foreground value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			rfg= (unsigned char) ir;
			gfg= (unsigned char) ig;
			bfg= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Vec: Clip ", ice_iobuf, strlen("%%ICE-Vec: Clip "))) {
			c= ice_iobuf+strlen("%%ICE-Vec: Clip ");
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

		else if (!strncmp("%%ICE-Vec: DTK ", ice_iobuf, strlen("%%ICE-Vec: DTK "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: DTK "), "%d %d %d %d", &dtk, &ir, &ig, &ib) != 4) {
				ice_err("Invalid vector DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((dtk != GROBJ_GLOBALDTK) &&
			    (dtk != GROBJ_WHITEDTK) &&
			    (dtk != GROBJ_BLACKDTK) &&
			    (dtk != GROBJ_OTHERDTK)) {
				ice_err("Invalid vector DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ir < 0) || (ir > 255)) {
				ice_err("Invalid vector red DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ig < 0) || (ig > 255)) {
				ice_err("Invalid vector green DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if ((ib < 0) || (ib > 255)) {
				ice_err("Invalid vector blue DTK value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			rdtk= (unsigned char) ir;
			gdtk= (unsigned char) ig;
			bdtk= (unsigned char) ib;
		}

		else if (!strncmp("%%ICE-Vec: Seq ", ice_iobuf, strlen("%%ICE-Vec: Seq "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: Seq "), "%d", &seq) != 1) {
				ice_err("Invalid vector sequence value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strncmp("%%ICE-Vec: IOT ", ice_iobuf, strlen("%%ICE-Vec: IOT "))) {
			if (sscanf(ice_iobuf+strlen("%%ICE-Vec: IOT "), "%d", &iot) != 1) {
				ice_err("Invalid vector tag value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
			if (iot <= GROBJ_NULLIOTAG) {
				ice_err("Invalid vector tag value.", NONFATAL);
				delete name;
				delete dp;
				delete pathnm;
				return;
			}
		}

		else if (!strcmp("%%ICE-Vec: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Vector end not found.", NONFATAL);
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
			if (lw == VECTOR_GLOBALWIDTH)
				width= gdf_linewidth;
			if (fg == VECTOR_GLOBALFG) {
				rfg= gdf_rfg;
				gfg= gdf_gfg;
				bfg= gdf_bfg;
			}
			if (dtk == GROBJ_GLOBALDTK) {
				rdtk= gdf_rdtk;
				gdtk= gdf_gdtk;
				bdtk= gdf_bdtk;
			}
			break;
		case INSICE_NEWOBJNEWGDF:
			if (lw == VECTOR_GLOBALWIDTH)
				lw= VECTOR_OTHERWIDTH;
			if (fg == VECTOR_GLOBALFG)
				fg= VECTOR_OTHERFG;
			if (dtk == GROBJ_GLOBALDTK)
				dtk= GROBJ_OTHERDTK;
			break;
		}
	}

	if ((vec= new Vector((Dlnk **) &grobjs, name, seq)) == (Vector *) NULL) {
		ice_err("Cannot create vector object.", NONFATAL);
		delete name;
		delete dp;
		return;
	}
	delete name;
	nvectors++;

	(void) vec->setwidth(lw, width);
	(void) vec->setlinestyle(ls);
	(void) vec->setdashstyle(ds, dp, dpl, dpo);
	(void) vec->setcapstyle(cs);
	(void) vec->setptrstyle(ptr, ptrs, pwd, pol, pil);
	(void) vec->setforeground(fg, rfg, gfg, bfg);
	if (pth != (Path *) NULL)
		(void) pth->setreferences(PATH_REFINCR);
	vec->setclip(pth);
	vec->setiotag(iot);

	(void) vec->setdtk(dtk, rdtk, gdtk, bdtk);
	if (pg_pixdepth == 1)
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, 2);
	else
		dtkpix= cmap_lookup(rdtk, gdtk, bdtk, PSEUDOCOLOR_MAPSZ);
	vec->setdtkpix(dtkpix);

	vec->setfloc(ox, oy, (float) pg_dpi);
	vec->setfend(tx, ty, (float) pg_dpi);

	if ((delitem= menuitem_create(LXMI_STRING, vec->getname(),
				LXMI_CLIENTDATA, (char *) vec,
				LXMI_PROC, delvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(vec);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, vec->getname(),
				LXMI_CLIENTDATA, (char *) vec,
				LXMI_PROC, attrvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(vec);
		menuitem_destroy(delitem);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, vec->getname(),
				LXMI_CLIENTDATA, (char *) vec,
				LXMI_PROC, trvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, vec->getname(),
				LXMI_CLIENTDATA, (char *) vec,
				LXMI_PROC, cpvec_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, vec->getname(),
				LXMI_CLIENTDATA, (char *) vec,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, vec->getname(),
				LXMI_CLIENTDATA, (char *) vec,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		vec_del(vec);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	(void) menuitem_insert(delvec_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrvec_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trvec_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpvec_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpvec_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopvec_menu, cmpopitem);

	return;
}

void
vecins_event(XEvent *event)
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
		x= bevt->x+vx;
		y= bevt->y+vy;
		if (vec_drawn) {
			XDrawLine(dpy, pg_cpm, pg_xgc, vec_x, vec_y, vec_ex, vec_ey);
			XDrawLine(dpy, pg_cwin, pg_xgc, vec_x-vx, vec_y-vy, vec_ex-vx, vec_ey-vy);
		}
		if (pgcoords_drawn)
			pg_erasecoords();
		drawcrosshairs();
		if (ice_op == VEC_INSERTLOC1) {
			new_vec->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			vec_x= x;
			vec_y= y;
			ice_op= VEC_INSERTLOC2;
			return;
		}
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
		new_vec->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
		if ((delitem= menuitem_create(LXMI_STRING, new_vec->getname(),
					LXMI_CLIENTDATA, (char *) new_vec,
					LXMI_PROC, delvec_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			vec_del(new_vec);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((attritem= menuitem_create(LXMI_STRING, new_vec->getname(),
					LXMI_CLIENTDATA, (char *) new_vec,
					LXMI_PROC, attrvec_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			vec_del(new_vec);
			menuitem_destroy(delitem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((tritem= menuitem_create(LXMI_STRING, new_vec->getname(),
					LXMI_CLIENTDATA, (char *) new_vec,
					LXMI_PROC, trvec_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			vec_del(new_vec);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((cpitem= menuitem_create(LXMI_STRING, new_vec->getname(),
					LXMI_CLIENTDATA, (char *) new_vec,
					LXMI_PROC, cpvec_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			vec_del(new_vec);
			menuitem_destroy(delitem);
			menuitem_destroy(attritem);
			menuitem_destroy(tritem);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask);
			ice_op= MAIN_MENU;
			return;
		}
		if ((dmpitem= menuitem_create(LXMI_STRING, new_vec->getname(),
					LXMI_CLIENTDATA, (char *) new_vec,
					LXMI_PROC, dmpobj_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			vec_del(new_vec);
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
		if ((cmpopitem= menuitem_create(LXMI_STRING, new_vec->getname(),
					LXMI_CLIENTDATA, (char *) new_vec,
					LXMI_PROC, cmpop_proc,
					LXMI_NULL)) == (Menu_item *) NULL) {
			ice_err("Internal toolkit error.", NONFATAL);
			vec_del(new_vec);
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
		(void) menuitem_insert(delvec_menu, delitem);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(attrvec_menu, attritem);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(trvec_menu, tritem);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cpvec_menu, cpitem);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(dmpvec_menu, dmpitem);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		(void) menuitem_insert(cmpopvec_menu, cmpopitem);
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
		if (ice_op == VEC_INSERTLOC1) {
			new_vec->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			new_vec->getloc(&fx, &fy, &x, &junk);
		}
		else {
			new_vec->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			new_vec->getend(&fx, &fy, &x, &junk);
		}
		break;

	case MotionNotify:
		mevt= (XMotionEvent *) event;
		x= mevt->x+vx;
		y= mevt->y+vy;
		if (ice_op == VEC_INSERTLOC1) {
			new_vec->setploc(x, pg_pixheight-1-y, (float) pg_dpi);
			new_vec->getloc(&fx, &fy, &x, &junk);
		}
		else {
			new_vec->setpend(x, pg_pixheight-1-y, (float) pg_dpi);
			new_vec->getend(&fx, &fy, &x, &junk);
		}
		break;
	default:
		return;
	}

	if (crosshairs_drawn)
		drawcrosshairs();
	if (vec_drawn) {
		XDrawLine(dpy, pg_cpm, pg_xgc, vec_x, vec_y, vec_ex, vec_ey);
		XDrawLine(dpy, pg_cwin, pg_xgc, vec_x-vx, vec_y-vy, vec_ex-vx, vec_ey-vy);
	}
	if (ice_op == VEC_INSERTLOC2) {
		vec_ex= x;
		vec_ey= y;
		XDrawLine(dpy, pg_cpm, pg_xgc, vec_x, vec_y, vec_ex, vec_ey);
		XDrawLine(dpy, pg_cwin, pg_xgc, vec_x-vx, vec_y-vy, vec_ex-vx, vec_ey-vy);
		vec_drawn= TRUE;
	}
	pg_showcoords(x-vx+1, y-vy+1, -1, -1, fx, fy, x, pg_pixheight-1-y);
	crosshairs_x= x;
	crosshairs_y= y;
	drawcrosshairs();

	return;
}

void
delvec_proc(Menu *m, Menu_item *mi)
{
	Vector *vec;
	Menu_item *item;

	if ((vec= (Vector *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Vector *) NULL) {
		ice_err("Cannot locate selected vector object.", NONFATAL);
		return;
	}
	if ((ice_op != CMP_DELETE) && (ice_op != DELETE_ALL)) {
		XDefineCursor(dpy, pg_cwin, hg_cursor);
		XSync(dpy, False);
	}
	vec_del(vec);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(attrvec_menu, LXMI_CLIENTDATA, (char *) vec);
	menuitem_delete(attrvec_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trvec_menu, LXMI_CLIENTDATA, (char *) vec);
	menuitem_delete(trvec_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cpvec_menu, LXMI_CLIENTDATA, (char *) vec);
	menuitem_delete(cpvec_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmpvec_menu, LXMI_CLIENTDATA, (char *) vec);
	menuitem_delete(dmpvec_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpopvec_menu, LXMI_CLIENTDATA, (char *) vec);
	menuitem_delete(cmpopvec_menu, item);
	menuitem_destroy(item);
	if (nvectors == 0) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
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
vec_del(Vector *vec)
{
	if (grobjs == (Grobj *) vec) {
		if (vec->succ() == (Dlnk *) NULL)
			grobjs= (Grobj *) NULL;
		else
			grobjs= (Grobj *) vec->succ();
	}
	vec->unlink();
	delete vec;
	nvectors--;
	return;
}
