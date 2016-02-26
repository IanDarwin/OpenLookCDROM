/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"

extern void		ice_err(char *, int);

extern int nopchildren, nopalloc, nopoldchildren;
extern Grobj **opchildren, **opoldchildren;
extern boolean cmpop_done;
extern Grobj *cmpop_selobj;
extern Composite *new_cmp, *add_cmp, *rm_cmp;

int psd_active, ras_active, text_active, vec_active, crv_active;
int mrk_active, rect_active, poly_active, axis_active, cmp_active;

void
cmpop_menusetup()
{
	Grobj *g;
	Menu *m;
	Menu_item *mi;
	int state, *counter;

	psd_active= ras_active= text_active= vec_active= crv_active= 0;
	mrk_active= rect_active= poly_active= axis_active= cmp_active= 0;

	for (g= grobjs; g != (Grobj *) NULL; g= (Grobj *) g->succ()) {
		switch (ice_op) {
		case CMP_BIND:
		case CMP_ADD:
			if (g->getparent() == (Grobj *) NULL)
				state= LXMI_ACTIVE;
			else
				state= LXMI_INACTIVE;
			break;
		case CMP_REMOVE:
			if (g->getparent() == (Grobj *) rm_cmp)
				state= LXMI_ACTIVE;
			else
				state= LXMI_INACTIVE;
			break;
		}
		switch (g->gettype()) {
		case GROBJ_PSDOC:
			m= cmpoppsd_menu;
			counter= &psd_active;
			break;
		case GROBJ_RASTER:
			m= cmpopras_menu;
			counter= &ras_active;
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) g)->getintobjtype()) {
			case INTOBJ_TEXT:
				m= cmpoptext_menu;
				counter= &text_active;
				break;
			case INTOBJ_VECTOR:
				m= cmpopvec_menu;
				counter= &vec_active;
				break;
			case INTOBJ_CURVE:
				m= cmpopcrv_menu;
				counter= &crv_active;
				break;
			case INTOBJ_MARKER:
				m= cmpopmrk_menu;
				counter= &mrk_active;
				break;
			case INTOBJ_RECTANGLE:
				m= cmpoprect_menu;
				counter= &rect_active;
				break;
			case INTOBJ_POLYGON:
				m= cmpoppoly_menu;
				counter= &poly_active;
				break;
			case INTOBJ_AXIS:
				m= cmpopaxis_menu;
				counter= &axis_active;
				break;
			}
			break;
		}
		mi= menuitem_find(m, LXMI_CLIENTDATA, (char *) g);
		(void) menuitem_set(mi, LXMI_STATE, state, LXMI_NULL);
		if (state == LXMI_ACTIVE)
			(*counter)++;
	}

	for (g= cmpobjs; g != (Grobj *) NULL; g= (Grobj *) g->succ()) {
		switch (ice_op) {
		case CMP_BIND:
			if ((g->getparent() == (Grobj *) NULL) && (g != (Grobj *) new_cmp))
				state= LXMI_ACTIVE;
			else
				state= LXMI_INACTIVE;
			if (g != (Grobj *) new_cmp) {
				mi= menuitem_find(cmpopcmp_menu, LXMI_CLIENTDATA, (char *) g);
				(void) menuitem_set(mi, LXMI_STATE, state, LXMI_NULL);
				if (state == LXMI_ACTIVE)
					cmp_active++;
			}
			break;
		case CMP_ADD:
			if ((g->getparent() == (Grobj *) NULL) && (g != (Grobj *) add_cmp))
				state= LXMI_ACTIVE;
			else
				state= LXMI_INACTIVE;
			mi= menuitem_find(cmpopcmp_menu, LXMI_CLIENTDATA, (char *) g);
			(void) menuitem_set(mi, LXMI_STATE, state, LXMI_NULL);
			if (state == LXMI_ACTIVE)
				cmp_active++;
			break;
		case CMP_REMOVE:
			if (g->getparent() == (Grobj *) rm_cmp)
				state= LXMI_ACTIVE;
			else
				state= LXMI_INACTIVE;
			mi= menuitem_find(cmpopcmp_menu, LXMI_CLIENTDATA, (char *) g);
			(void) menuitem_set(mi, LXMI_STATE, state, LXMI_NULL);
			if (state == LXMI_ACTIVE)
				cmp_active++;
			break;
		}
	}

	if (psd_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (ras_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (text_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (vec_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (crv_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (mrk_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (rect_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (poly_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (axis_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (cmp_active > 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	else
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	return;
}

void
cmpop_menuadj(Grobj *g)
{
	Menu *m;
	Menu_item *mi;
	int state, *counter;

	switch (g->gettype()) {
	case GROBJ_PSDOC:
		m= cmpoppsd_menu;
		counter= &psd_active;
		break;
	case GROBJ_RASTER:
		m= cmpopras_menu;
		counter= &ras_active;
		break;
	case GROBJ_INTOBJ:
		switch (((Intobj *) g)->getintobjtype()) {
		case INTOBJ_TEXT:
			m= cmpoptext_menu;
			counter= &text_active;
			break;
		case INTOBJ_VECTOR:
			m= cmpopvec_menu;
			counter= &vec_active;
			break;
		case INTOBJ_CURVE:
			m= cmpopcrv_menu;
			counter= &crv_active;
			break;
		case INTOBJ_MARKER:
			m= cmpopmrk_menu;
			counter= &mrk_active;
			break;
		case INTOBJ_RECTANGLE:
			m= cmpoprect_menu;
			counter= &rect_active;
			break;
		case INTOBJ_POLYGON:
			m= cmpoppoly_menu;
			counter= &poly_active;
			break;
		case INTOBJ_AXIS:
			m= cmpopaxis_menu;
			counter= &axis_active;
			break;
		}
		break;
	case GROBJ_COMPOSITE:
		m= cmpopcmp_menu;
		counter= &cmp_active;
		break;
	}

	mi= menuitem_find(m, LXMI_CLIENTDATA, (char *) g);
	state= (int) *((int *) menuitem_get(mi, LXMI_STATE));
	if (state != LXMI_ACTIVE) 
		return;

	(void) menuitem_set(mi, LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	(*counter)--;

	if (psd_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (ras_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (text_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (vec_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (crv_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (mrk_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (rect_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (poly_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (axis_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (cmp_active == 0)
		(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	return;
}

void
cmpop_proc(Menu *m, Menu_item *mi)
{
	if ((cmpop_selobj= (Grobj *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Grobj *) NULL) {
		ice_err("Cannot locate selected object.", NONFATAL);
		return;
	}

	switch (cmpop_selobj->gettype()) {
	case GROBJ_PSDOC:
		if (--psd_active == 0)
			(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "PS Document"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		break;
	case GROBJ_RASTER:
		if (--ras_active == 0)
			(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Raster"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		break;
	case GROBJ_INTOBJ:
		switch (((Intobj *) cmpop_selobj)->getintobjtype()) {
		case INTOBJ_TEXT:
			if (--text_active == 0)
				(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case INTOBJ_VECTOR:
			if (--vec_active == 0)
				(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Vector"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case INTOBJ_CURVE:
			if (--crv_active == 0)
				(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Curve"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case INTOBJ_MARKER:
			if (--mrk_active == 0)
				(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Marker"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case INTOBJ_RECTANGLE:
			if (--rect_active == 0)
				(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Rectangle"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case INTOBJ_POLYGON:
			if (--poly_active == 0)
				(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Polygon"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		case INTOBJ_AXIS:
			if (--axis_active == 0)
				(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Axis"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		}
		break;
	case GROBJ_COMPOSITE:
		if (--cmp_active == 0)
			(void) menuitem_set(menuitem_find(cmpop_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		break;
	}

	(void) menuitem_set(mi, LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	return;
}

void
cmpopdone_proc(Menu *m, Menu_item *mi)
{
	cmpop_done= TRUE;
	return;
}
