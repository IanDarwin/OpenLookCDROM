/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"

extern void		cmp_setchldrootparent(Composite *, Grobj *);
extern void		cmpop_menuadj(Grobj *);
extern void		cmpop_menusetup();
extern void		ice_err(char *, int);
extern Composite *	sel_cmpobj(int, int);
extern Grobj *		sel_grobj(int, int, int);
extern void		ubdcmp_proc(Menu *, Menu_item *);

extern int nopchildren, nopalloc, nopoldchildren;
extern Grobj **opchildren, **opoldchildren;
extern boolean cmpop_done;
extern Grobj *cmpop_selobj;

Composite *rm_cmp;

void
cmprmsel_proc(Menu *m, Menu_item *mi)
{
	rm_cmp= (Composite *) NULL;
	ice_op= CMP_REMOVE;
	return;
}

void
rmcmp_proc(Menu *m, Menu_item *mi)
{
	int i;

	if ((rm_cmp= (Composite *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		return;
	}

	rm_cmp->getchildren(&nopoldchildren, &opoldchildren);

	nopalloc= 5;
	if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	for (i= 0; i < nopalloc; i++)
		opchildren[i]= (Grobj *) NULL;
	nopchildren= 0;

	ice_op= CMP_REMOVE;
	cmpop_menusetup();

	return;
}

void
cmprm_event(XEvent *event)
{
	XButtonEvent *bevt;
	Grobj **oldrmchildren, **children, *gr;
	int i, j, k, nchildren;
	Menu_item *mi;

	if (event->type != ButtonPress)
		return;

	bevt= (XButtonEvent *) event;

	if (rm_cmp == (Composite *) NULL) {
		if ((rm_cmp= sel_cmpobj(bevt->x, bevt->y)) == (Composite *) NULL) {
			ice_err("Cannot locate selected composite object.", NONFATAL);
			ice_op= MAIN_MENU;
			return;
		}

		rm_cmp->getchildren(&nopoldchildren, &opoldchildren);

		nopalloc= 5;
		if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			ice_op= MAIN_MENU;
			return;
		}
		for (i= 0; i < nopalloc; i++)
			opchildren[i]= (Grobj *) NULL;
		nopchildren= 0;

		cmpop_menusetup();
		return;
	}

	cmpop_done= FALSE;
	cmpop_selobj= (Grobj *) NULL;
	if (bevt->button == Button3) {
		menu_show(cmpop_menu, bevt);
		if ((cmpop_done == FALSE) && (cmpop_selobj == (Grobj *) NULL))
			return;
	}

	if (cmpop_done != TRUE) {
		if (nopchildren == nopalloc) {
			oldrmchildren= opchildren;
			nopalloc+= 5;
			if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oldrmchildren;
				ice_op= MAIN_MENU;
				return;
			}
			for (i= 0; i < nopchildren; i++)
				opchildren[i]= oldrmchildren[i];
			for (; i < nopalloc; i++)
				opchildren[i]= (Grobj *) NULL;
			delete oldrmchildren;
		}

		if (cmpop_selobj == (Grobj *) NULL) {

			/* locate atomic object */
			if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ATOMIC)) == (Grobj *) NULL) {
				ice_err("Cannot locate selected object.", NONFATAL);
				delete opchildren;
				ice_op= MAIN_MENU;
				return;
			}

			/* make sure it's a descendant of rm_cmp */
			if (gr->getrootparent() != (Grobj *) rm_cmp) {
				ice_err("Selected object not contained within composite object.", NONFATAL);
				delete opchildren;
				ice_op= MAIN_MENU;
				return;
			}

			/* search upward for its highest-level ancestor
			   one generation below rm_cmp (i.e., a child of rm_cmp) */
			while (gr->getparent() != (Grobj *) rm_cmp)
				gr= gr->getparent();

			cmpop_menuadj(gr);
		}
		else
			gr= cmpop_selobj;

		for (i= 0; i < nopchildren; i++) {
			if (gr == opchildren[i]) {
				ice_err("Cannot remove the same object more than once.", NONFATAL);
				delete opchildren;
				ice_op= MAIN_MENU;
				return;
			}
		}
		opchildren[nopchildren++]= gr;
	}

	if (bevt->button == Button3) {
		if (cmpop_done != TRUE)
			return;
		else if (nopchildren == 0) {
			ice_err("No objects selected.", NONFATAL);
			delete opchildren;
			ice_op= MAIN_MENU;
			return;
		}
	}
	else if (!(bevt->state & ShiftMask) && !(bevt->state & ControlMask))
		return;

	nchildren= nopoldchildren-nopchildren;
	if (nchildren == 0) {
		mi= menuitem_find(cmpubd_menu, LXMI_CLIENTDATA, (char *) rm_cmp);
		ubdcmp_proc(cmpubd_menu, mi);
		ice_op= MAIN_MENU;
		return;
	}
	if ((children= new Grobj *[nchildren]) == (Grobj **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		delete opchildren;
		ice_op= MAIN_MENU;
		return;
	}

	/* copy all old children which aren't being
	   removed into new children array */
	for (i= j= 0; i < nchildren; i++, j++) {
		for (; TRUE; j++) {
			for (k= 0; k < nopchildren; k++) {
				if (opoldchildren[j] == opchildren[k])
					break;
			}
			if (k == nopchildren)
				break;
		}
		children[i]= opoldchildren[j];
	}

	if (rm_cmp->setchildren(nchildren, children) != GROBJ_SUCCESS) {
		ice_err("Cannot build composite object child lists.", NONFATAL);
		delete opchildren;
		delete children;
		ice_op= MAIN_MENU;
		return;
	}

	for (i= 0; i < nopchildren; i++) {
		gr= opchildren[i];
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

	delete opchildren;
	ice_op= MAIN_MENU;
	return;
}
