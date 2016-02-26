/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Composite.h"
#include "Intobj.h"
#include "Psdoc.h"
#include "Raster.h"
#include "Text.h"
#include "Vector.h"
#include "Curve.h"
#include "Marker.h"
#include "Rectangle.h"
#include "Polygon.h"
#include "Axis.h"

extern "C" {
char *			strcat(char *, char *);
char *			strcpy(char *, char *);
int			strcmp(char *, char *);
int			strlen(char *);
char *			strncpy(char *, char *, int);
}

extern void		addcmp_proc(Menu *, Menu_item *);
extern void		attrcmp_proc(Menu *, Menu_item *);
extern Axis *		axis_copy(Axis *, char *);
extern void		cmp_del(Composite *);
extern void		cmp_delall(Composite *);
extern void		cmp_setchldrootparent(Composite *, Grobj *);
extern void		cmpop_proc(Menu *, Menu_item *);
extern Curve *		crv_copy(Curve *, char *);
extern void		delaxis_proc(Menu *, Menu_item *);
extern void		delcmp_proc(Menu *, Menu_item *);
extern void		delcrv_proc(Menu *, Menu_item *);
extern void		delmrk_proc(Menu *, Menu_item *);
extern void		delpoly_proc(Menu *, Menu_item *);
extern void		delpsd_proc(Menu *, Menu_item *);
extern void		delras_proc(Menu *, Menu_item *);
extern void		delrect_proc(Menu *, Menu_item *);
extern void		deltext_proc(Menu *, Menu_item *);
extern void		delvec_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		ice_err(char *, int);
extern Marker *		mrk_copy(Marker *, char *);
extern Polygon *	poly_copy(Polygon *, char *);
extern Psdoc *		psd_copy(Psdoc *, char *);
extern Raster *		ras_copy(Raster *, char *);
extern Rectangle *	rect_copy(Rectangle *, char *);
extern void		rmcmp_proc(Menu *, Menu_item *);
extern Text *		text_copy(Text *, char *);
extern void		trcmp_proc(Menu *, Menu_item *);
extern void		ubdcmp_proc(Menu *, Menu_item *);
extern Vector *		vec_copy(Vector *, char *);

extern Composite *loc_cmpobj;
extern Grobj *loc_grobj;
extern Composite *tr_cmp;
extern Grobj *tr_atom;
extern float tr_ipp;

void
cpcmp_proc(Menu *m, Menu_item *mi)
{
	Composite *cmp;
	int val;
	char name[LXADEF_MAXSTORE+1], *nm;
	int nchildren;
	Grobj **children;
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];
	char *cmp_uniquenm(char *, int);
	Composite *cmp_copy(Composite *, char *);

	if ((cmp= (Composite *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	if (alert_prompt(progname, dpy, &val,
			LXA_TEXT, "Name:", "", name,
			LXA_BUTTON, "Continue", 0,
			LXA_BUTTON, "Abort", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	if (val == 1) {
		ice_op= MAIN_MENU;
		return;
	}

	if (strlen(name) == 0)
		(void) strncpy(name, cmp->getname(), LXADEF_MAXSTORE);
	if ((nm= cmp_uniquenm(name, LXADEF_MAXSTORE)) == (char *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	if ((tr_cmp= cmp_copy(cmp, nm)) == (Composite *) NULL) {
		if (nm != name)
			delete nm;
		ice_op= MAIN_MENU;
		return;
	}
	if (nm != name)
		delete nm;
	for (tr_atom= (Grobj *) NULL, cmp= tr_cmp; tr_atom == (Grobj *) NULL; ) {
		cmp->getchildren(&nchildren, &children);
		if (children[0]->gettype() == GROBJ_COMPOSITE)
			cmp= (Composite *) children[0];
		else
			tr_atom= children[0];
	}

	ice_op= CMP_COPY;

	tr_atom->getloc(&fx, &fy, &ix, &iy);

	if (pg_loc == PG_CURSORLOC) {
		tr_ipp= 1./pg_dpi;
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_cmpobj= tr_cmp;
	loc_grobj= tr_atom;
	panelitem_set(locattr_panel, locattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	switch (pg_units) {
	case PG_PIXELS:
		(void) sprintf(xbuf, "%1d", (int) (fx*pg_dpi));
		(void) sprintf(ybuf, "%1d", (int) (fy*pg_dpi));
		break;
	case PG_POINTS:
		(void) sprintf(xbuf, "%4.2f", (float) (fx*72.));
		(void) sprintf(ybuf, "%4.2f", (float) (fy*72.));
		break;
	case PG_INCHES:
		(void) sprintf(xbuf, "%4.2f", fx);
		(void) sprintf(ybuf, "%4.2f", fy);
		break;
	case PG_USER:
		fx= (float) ((fx-pg_xri)*pg_hsi)+pg_xru;
		fy= (float) ((fy-pg_yri)*pg_vsi)+pg_yru;
		(void) sprintf(xbuf, "%4.2f", fx);
		(void) sprintf(ybuf, "%4.2f", fy);
		break;
	}
	panelitem_set(locattr_panel, locattr_x, LXPTEXT_VALUE, xbuf, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_y, LXPTEXT_VALUE, ybuf, LXPI_NULL);
	XMapRaised(dpy, locattr_frame);
	return;
}

char *
cmp_uniquenm(char *name, int maxlen)
/*
   Ensure that name is unique among all composites.
   If name is not unique and its length is less than maxlen,
   rewrite name in place, otherwise allocate dynamic memory
   and store unique name there.
*/
{
	char *nm, *onm;
	int len;
	Composite *c;

	nm= name;
	len= strlen(name);
	c= cmpobjs;
	while (c != (Composite *) NULL) {
		for (c= cmpobjs; c != (Composite *) NULL; c= (Composite *) c->succ()) {
			if (!strcmp(nm, c->getname())) {
				if (len < maxlen)
					(void) strcat(nm, "+");
				else {
					onm= nm;
					if ((nm= new char[len+2]) == (char *) NULL) {
						ice_err("Memory allocation error.", NONFATAL);
						if (onm != name)
							delete onm;
						return (char *) NULL;
					}
					(void) strcpy(nm, onm);
					(void) strcat(nm, "+");
					if (onm != name)
						delete onm;
				}
				len++;
				break;
			}
		}
	}
	return nm;
}

Composite *
cmp_copy(Composite *cpcmp, char *name)
{
	Composite *new_cmp;
	Grobj **children, **cpchildren, *gr;
	int nc, i;
	char *onm, *nm;
	Menu_item *delitem, *attritem, *tritem, *cpitem, *dmpitem;
	Menu_item *cmpopitem, *ubditem, *additem, *rmitem;
	void cpcmp_abort(Grobj **, int);

	cpcmp->getchildren(&nc, &cpchildren);
	if ((children= new Grobj *[nc]) == (Grobj **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return (Composite *) NULL;
	}

	for (i= 0; i < nc; i++) {

		gr= cpchildren[i];
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			if ((children[i]= (Grobj *) psd_copy((Psdoc *) gr, ((Psdoc *) gr)->getname())) == (Grobj *) NULL) {
				cpcmp_abort(children, i);
				delete children;
				return (Composite *) NULL;
			}
			break;
		case GROBJ_RASTER:
			if ((children[i]= (Grobj *) ras_copy((Raster *) gr, ((Raster *) gr)->getname())) == (Grobj *) NULL) {
				cpcmp_abort(children, i);
				delete children;
				return (Composite *) NULL;
			}
			break;
		case GROBJ_INTOBJ:
			onm= ((Intobj *) gr)->getname();
			if ((nm= new char[strlen(onm)+2]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				cpcmp_abort(children, i);
				delete children;
				return (Composite *) NULL;
			}
			(void) strcpy(nm, onm);
			(void) strcat(nm, "+");
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				if ((children[i]= (Grobj *) text_copy((Text *) gr, nm)) == (Grobj *) NULL) {
					cpcmp_abort(children, i);
					delete children;
					delete nm;
					return (Composite *) NULL;
				}
				break;
			case INTOBJ_VECTOR:
				if ((children[i]= (Grobj *) vec_copy((Vector *) gr, nm)) == (Grobj *) NULL) {
					cpcmp_abort(children, i);
					delete children;
					delete nm;
					return (Composite *) NULL;
				}
				break;
			case INTOBJ_CURVE:
				if ((children[i]= (Grobj *) crv_copy((Curve *) gr, nm)) == (Grobj *) NULL) {
					cpcmp_abort(children, i);
					delete children;
					delete nm;
					return (Composite *) NULL;
				}
				break;
			case INTOBJ_MARKER:
				if ((children[i]= (Grobj *) mrk_copy((Marker *) gr, nm)) == (Grobj *) NULL) {
					cpcmp_abort(children, i);
					delete children;
					delete nm;
					return (Composite *) NULL;
				}
				break;
			case INTOBJ_RECTANGLE:
				if ((children[i]= (Grobj *) rect_copy((Rectangle *) gr, nm)) == (Grobj *) NULL) {
					cpcmp_abort(children, i);
					delete children;
					delete nm;
					return (Composite *) NULL;
				}
				break;
			case INTOBJ_POLYGON:
				if ((children[i]= (Grobj *) poly_copy((Polygon *) gr, nm)) == (Grobj *) NULL) {
					cpcmp_abort(children, i);
					delete children;
					delete nm;
					return (Composite *) NULL;
				}
				break;
			case INTOBJ_AXIS:
				if ((children[i]= (Grobj *) axis_copy((Axis *) gr, nm)) == (Grobj *) NULL) {
					cpcmp_abort(children, i);
					delete children;
					delete nm;
					return (Composite *) NULL;
				}
				break;
			}
			delete nm;
			break;
		case GROBJ_COMPOSITE:
			onm= ((Composite *) gr)->getname();
			if ((nm= cmp_uniquenm(onm, 0)) == (char *) NULL) {
				cpcmp_abort(children, i);
				delete children;
				return (Composite *) NULL;
			}
			if ((children[i]= (Grobj *) cmp_copy((Composite *) gr, nm)) == (Composite *) NULL) {
				cpcmp_abort(children, i);
				delete children;
				delete nm;
				return (Composite *) NULL;
			}
			delete nm;
			break;
		}
	}

	if ((new_cmp= new Composite((Dlnk **) &cmpobjs, name)) == (Composite *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		cpcmp_abort(children, nc);
		delete children;
		return (Composite *) NULL;
	}

	if (new_cmp->setchildren(nc, children) != GROBJ_SUCCESS) {
		ice_err("Cannot build composite object child lists.", NONFATAL);
		cpcmp_abort(children, nc);
		delete children;
		cmp_del(new_cmp);
		return (Composite *) NULL;
	}

	if ((delitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, delcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		return (Composite *) NULL;
	}
	if ((tritem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, trcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		return (Composite *) NULL;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, cpcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		return (Composite *) NULL;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return (Composite *) NULL;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return (Composite *) NULL;
	}
	if ((attritem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, attrcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		return (Composite *) NULL;
	}
	if ((ubditem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, ubdcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		return (Composite *) NULL;
	}
	if ((additem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, addcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		menuitem_destroy(ubditem);
		return (Composite *) NULL;
	}
	if ((rmitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, rmcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cpcmp_abort(children, nc);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		menuitem_destroy(ubditem);
		menuitem_destroy(additem);
		return (Composite *) NULL;
	}
	(void) menuitem_insert(delcmp_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trcmp_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cpcmp_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmpcmp_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpopcmp_menu, cmpopitem);
	(void) menuitem_insert(cmpattr_menu, attritem);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Attributes"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpubd_menu, ubditem);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Unbind"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpadd_menu, additem);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Add"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmprm_menu, rmitem);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Remove"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);

	for (i= 0; i < nc; i++) {
		gr= children[i];
		gr->setparent(new_cmp);
		gr->setrootparent(new_cmp);
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			(void) menuitem_set(menuitem_find(delpsd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trpsd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case GROBJ_RASTER:
			(void) menuitem_set(menuitem_find(delras_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trras_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				(void) menuitem_set(menuitem_find(deltext_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trtext_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				break;
			case INTOBJ_VECTOR:
				(void) menuitem_set(menuitem_find(delvec_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trvec_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				break;
			case INTOBJ_CURVE:
				(void) menuitem_set(menuitem_find(delcrv_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trcrv_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				break;
			case INTOBJ_MARKER:
				(void) menuitem_set(menuitem_find(delmrk_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trmrk_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				break;
			case INTOBJ_RECTANGLE:
				(void) menuitem_set(menuitem_find(delrect_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trrect_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				break;
			case INTOBJ_POLYGON:
				(void) menuitem_set(menuitem_find(delpoly_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trpoly_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				break;
			case INTOBJ_AXIS:
				(void) menuitem_set(menuitem_find(delaxis_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(traxis_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
				break;
			}
			break;
		case GROBJ_COMPOSITE:
			cmp_setchldrootparent((Composite *) gr, new_cmp);
			(void) menuitem_set(menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpattr_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpubd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpadd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmprm_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		}
	}

	return new_cmp;
}

void
cpcmp_abort(Grobj **children, int nc)
/*
   Error recovery from an aborted composite copy operation.
   Frees all newly allocated children of the composite.
*/
{
	Grobj *gr;
	int i, op;
	Menu_item *mi;

	/* inhibit redraws */
	op= ice_op;
	ice_op= CMP_DELETE;

	for (i= 0; i < nc; i++) {

		gr= children[i];
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			mi= menuitem_find(delpsd_menu, LXMI_CLIENTDATA, (char *) gr);
			delpsd_proc(delpsd_menu, mi);
			break;
		case GROBJ_RASTER:
			mi= menuitem_find(delras_menu, LXMI_CLIENTDATA, (char *) gr);
			delras_proc(delras_menu, mi);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				mi= menuitem_find(deltext_menu, LXMI_CLIENTDATA, (char *) gr);
				deltext_proc(deltext_menu, mi);
				break;
			case INTOBJ_VECTOR:
				mi= menuitem_find(delvec_menu, LXMI_CLIENTDATA, (char *) gr);
				delvec_proc(delvec_menu, mi);
				break;
			case INTOBJ_CURVE:
				mi= menuitem_find(delcrv_menu, LXMI_CLIENTDATA, (char *) gr);
				delcrv_proc(delcrv_menu, mi);
				break;
			case INTOBJ_MARKER:
				mi= menuitem_find(delmrk_menu, LXMI_CLIENTDATA, (char *) gr);
				delmrk_proc(delmrk_menu, mi);
				break;
			case INTOBJ_RECTANGLE:
				mi= menuitem_find(delrect_menu, LXMI_CLIENTDATA, (char *) gr);
				delrect_proc(delrect_menu, mi);
				break;
			case INTOBJ_POLYGON:
				mi= menuitem_find(delpoly_menu, LXMI_CLIENTDATA, (char *) gr);
				delpoly_proc(delpoly_menu, mi);
				break;
			case INTOBJ_AXIS:
				mi= menuitem_find(delaxis_menu, LXMI_CLIENTDATA, (char *) gr);
				delaxis_proc(delaxis_menu, mi);
				break;
			}
			break;
		case GROBJ_COMPOSITE:
			cmp_delall((Composite *) gr);
			break;
		}
	}

	ice_op= op;

	return;
}
