/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"

extern void		cmp_del(Composite *);
extern void		cmp_setchldrootparent(Composite *, Grobj *);
extern void		ice_err(char *, int);
extern Composite *	sel_cmpobj(int, int);

void
cmpubdsel_proc(Menu *m, Menu_item *mi)
{
	if (cmpobjs == (Composite *) NULL) {
		ice_err("There are no existing composite objects.", NONFATAL);
		return;
	}

	ice_op= CMP_UNBIND;
	return;
}

void
cmpubd_event(XEvent *event)
{
	XButtonEvent *bevt;
	Composite *cmp;
	Menu_item *mi;
	void ubdcmp_proc(Menu *, Menu_item *);

	if (event->type != ButtonRelease)
		return;

	bevt= (XButtonEvent *) event;

	if ((cmp= sel_cmpobj(bevt->x, bevt->y)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	mi= menuitem_find(cmpubd_menu, LXMI_CLIENTDATA, (char *) cmp);
	ubdcmp_proc(cmpubd_menu, mi);

	ice_op= MAIN_MENU;
	return;
}

void
ubdcmp_proc(Menu *m, Menu_item *mi)
{
	Composite *cmp;
	Menu_item *item;
	int nchildren, i;
	Grobj **children, *gr;

	if ((cmp= (Composite *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		return;
	}

	cmp->getchildren(&nchildren, &children);
	for (i= 0; i < nchildren; i++) {
		gr= children[i];
		gr->setparent((Composite *) NULL);
		gr->setrootparent((Composite *) NULL);
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			(void) menuitem_set(menuitem_find(delpsd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trpsd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			break;
		case GROBJ_RASTER:
			(void) menuitem_set(menuitem_find(delras_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trras_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				(void) menuitem_set(menuitem_find(deltext_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trtext_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				break;
			case INTOBJ_VECTOR:
				(void) menuitem_set(menuitem_find(delvec_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trvec_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				break;
			case INTOBJ_CURVE:
				(void) menuitem_set(menuitem_find(delcrv_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trcrv_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				break;
			case INTOBJ_MARKER:
				(void) menuitem_set(menuitem_find(delmrk_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trmrk_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				break;
			case INTOBJ_RECTANGLE:
				(void) menuitem_set(menuitem_find(delrect_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trrect_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				break;
			case INTOBJ_POLYGON:
				(void) menuitem_set(menuitem_find(delpoly_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(trpoly_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				break;
			case INTOBJ_AXIS:
				(void) menuitem_set(menuitem_find(delaxis_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				(void) menuitem_set(menuitem_find(traxis_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
				break;
			}
			break;
		case GROBJ_COMPOSITE:
			cmp_setchldrootparent((Composite *) gr, gr);
			(void) menuitem_set(menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpattr_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpubd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpadd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmprm_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
		}
	}

	cmp_del(cmp);
	menuitem_delete(m, mi);
	menuitem_destroy(mi);
	item= menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(delcmp_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(trcmp_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(dmpcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(dmpcmp_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpopcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cmpopcmp_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpattr_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cmpattr_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpadd_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cmpadd_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmprm_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cmprm_menu, item);
	menuitem_destroy(item);
	if (cmpobjs == (Composite *) NULL) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Attributes"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Unbind"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Add"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Remove"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}

	return;
}
