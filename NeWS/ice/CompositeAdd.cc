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

extern int nopchildren, nopalloc, nopoldchildren;
extern Grobj **opchildren, **opoldchildren;
extern boolean cmpop_done;
extern Grobj *cmpop_selobj;

Composite *add_cmp;

void
cmpaddsel_proc(Menu *m, Menu_item *mi)
{
	add_cmp= (Composite *) NULL;
	ice_op= CMP_ADD;

	return;
}

void
addcmp_proc(Menu *m, Menu_item *mi)
{
	int i;
	Grobj *gr;

	if ((add_cmp= (Composite *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		return;
	}

	add_cmp->getchildren(&nopoldchildren, &opoldchildren);

	nopalloc= 5;
	if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	for (i= 0; i < nopalloc; i++)
		opchildren[i]= (Grobj *) NULL;
	nopchildren= 0;

	ice_op= CMP_ADD;

	i= GROBJ_NULLIOTAG+1;
	for (gr= (Grobj *) cmpobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ())
		gr->setiotag(i++);

	cmpop_menusetup();

	return;
}

void
cmpadd_event(XEvent *event)
{
	XButtonEvent *bevt;
	Grobj **oldaddchildren, **children, *gr;
	int i, nchildren, maxtag, tag;

	if (event->type != ButtonPress)
		return;

	bevt= (XButtonEvent *) event;

	if (add_cmp == (Composite *) NULL) {
		if ((add_cmp= sel_cmpobj(bevt->x, bevt->y)) == (Composite *) NULL) {
			ice_err("Cannot locate selected composite object.", NONFATAL);
			ice_op= MAIN_MENU;
			return;
		}

		add_cmp->getchildren(&nopoldchildren, &opoldchildren);

		nopalloc= 5;
		if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			ice_op= MAIN_MENU;
			return;
		}
		for (i= 0; i < nopalloc; i++)
			opchildren[i]= (Grobj *) NULL;
		nopchildren= 0;

		i= GROBJ_NULLIOTAG+1;
		for (gr= (Grobj *) cmpobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ())
			gr->setiotag(i++);

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
			oldaddchildren= opchildren;
			nopalloc+= 5;
			if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete oldaddchildren;
				ice_op= MAIN_MENU;
				return;
			}
			for (i= 0; i < nopchildren; i++)
				opchildren[i]= oldaddchildren[i];
			for (; i < nopalloc; i++)
				opchildren[i]= (Grobj *) NULL;
			delete oldaddchildren;
		}

		if (cmpop_selobj == (Grobj *) NULL) {
			if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ROOTPARENT)) == (Grobj *) NULL) {
				ice_err("Cannot locate selected object.", NONFATAL);
				delete opchildren;
				ice_op= MAIN_MENU;
				return;
			}
			cmpop_menuadj(gr);
		}
		else
			gr= cmpop_selobj;
		if (gr == (Grobj *) add_cmp) {
			ice_err("Duplicate objects within a composite object are not allowed.", NONFATAL);
			delete opchildren;
			ice_op= MAIN_MENU;
			return;
		}
		for (i= 0; i < nopchildren; i++) {
			if (gr == opchildren[i]) {
				ice_err("Duplicate objects within a composite object are not allowed.", NONFATAL);
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

	/* combine new and old child lists */
	nchildren= nopchildren+nopoldchildren;
	if ((children= new Grobj *[nchildren]) == (Grobj **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		delete opchildren;
		ice_op= MAIN_MENU;
		return;
	}
	for (i= 0; i < nopoldchildren; i++)
		children[i]= opoldchildren[i];
	for (i= 0; i < nopchildren; i++)
		children[i+nopoldchildren]= opchildren[i];
	if (add_cmp->setchildren(nchildren, children) != GROBJ_SUCCESS) {
		ice_err("Cannot build composite object child lists.", NONFATAL);
		delete opchildren;
		ice_op= MAIN_MENU;
		return;
	}

	maxtag= GROBJ_NULLIOTAG;
	for (i= 0; i < nopchildren; i++) {
		gr= opchildren[i];
		gr->setparent(add_cmp);
		gr->setrootparent(add_cmp);
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
			if ((tag= gr->getiotag()) > maxtag)
				maxtag= tag;
			cmp_setchldrootparent((Composite *) gr, add_cmp);
			(void) menuitem_set(menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpattr_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpubd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpadd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmprm_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		}
	}

	/* if we have added any composite object which follows add_cmp
	   in the composite list, we must move add_cmp to a point later in
	   the list to ensure that objects are dumped in the right order */
	if (maxtag > add_cmp->getiotag()) {
		i= GROBJ_NULLIOTAG+1;
		for (gr= (Grobj *) cmpobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ(), i++) {
			if (i == maxtag)
				break;
		}
		if (cmpobjs == add_cmp)
			cmpobjs= (Composite *) add_cmp->succ();
		add_cmp->unlink();
		add_cmp->link(gr, DLNK_APPEND);
	}

	delete opchildren;
	ice_op= MAIN_MENU;
	return;
}
