/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"

extern "C" {
int			strcat(char *, char *);
int			strcmp(char *, char *);
int			strcpy(char *, char *);
int			strlen(char *);
char *			strncat(char *, char *, int);
}

extern void		addcmp_proc(Menu *, Menu_item *);
extern void		attrcmp_proc(Menu *, Menu_item *);
extern void		cmpop_menuadj(Grobj *);
extern void		cmpop_menusetup();
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cpcmp_proc(Menu *, Menu_item *);
extern void		delaxis_proc(Menu *, Menu_item *);
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
extern void		pg_draw();
extern void		rmcmp_proc(Menu *, Menu_item *);
extern Grobj *		sel_grobj(int, int, int);
extern void		trcmp_proc(Menu *, Menu_item *);
extern void		ubdcmp_proc(Menu *, Menu_item *);

int nopchildren, nopalloc, nopoldchildren;
Grobj **opchildren, **opoldchildren;
boolean cmpop_done;
Grobj *cmpop_selobj;

Composite *new_cmp;

void
cmpbd_proc(Menu *m, Menu_item *mi)
{
	int val, i;
	char objnm[LXADEF_MAXSTORE+1];
	char errmsg[MAX_ERRMSGLEN+1];
	Composite *c;
	void cmp_del(Composite *);

	if (grobjs == (Grobj *) NULL) {
		ice_err("There are no existing objects.", NONFATAL);
		return;
	}

	if (alert_prompt(progname, dpy, &val,
			LXA_TEXT, "Name:", "", objnm,
			LXA_BUTTON, "Continue", 0,
			LXA_BUTTON, "Abort", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	if (val == 1)
		return;

	if (strlen(objnm) == 0) {
		ice_err("No object name specified.", NONFATAL);
		return;
	}

	for (c= cmpobjs; c != (Composite *) NULL; c= (Composite *) c->succ()) {
		if (!strcmp(objnm, c->getname())) {
			ice_err("Illegal duplicate object name specified.", NONFATAL);
			return;
		}
	}

	if ((new_cmp= new Composite((Dlnk **) &cmpobjs, objnm)) == (Composite *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}

	nopalloc= 5;
	if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		cmp_del(new_cmp);
		return;
	}
	for (i= 0; i < nopalloc; i++)
		opchildren[i]= (Grobj *) NULL;
	nopchildren= 0;

	ice_op= CMP_BIND;
	cmpop_menusetup();

	return;
}

void
cmpbd_event(XEvent *event)
{
	XButtonEvent *bevt;
	Grobj **oldchildren, *gr;
	int i;
	Menu_item *delitem, *attritem, *tritem, *cpitem, *dmpitem;
	Menu_item *cmpopitem, *ubditem, *additem, *rmitem;
	void cmp_del(Composite *);
	void delcmp_proc(Menu *, Menu_item *);
	void cmp_setchldrootparent(Composite *, Grobj *);

	if (event->type != ButtonPress)
		return;

	bevt= (XButtonEvent *) event;

	cmpop_done= FALSE;
	cmpop_selobj= (Grobj *) NULL;
	if (bevt->button == Button3) {
		menu_show(cmpop_menu, bevt);
		if ((cmpop_done == FALSE) && (cmpop_selobj == (Grobj *) NULL))
			return;
	}

	if (cmpop_done != TRUE) {
		if (nopchildren == nopalloc) {
			oldchildren= opchildren;
			nopalloc+= 5;
			if ((opchildren= new Grobj *[nopalloc]) == (Grobj **) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				cmp_del(new_cmp);
				delete oldchildren;
				ice_op= MAIN_MENU;
				return;
			}
			for (i= 0; i < nopchildren; i++)
				opchildren[i]= oldchildren[i];
			for (; i < nopalloc; i++)
				opchildren[i]= (Grobj *) NULL;
			delete oldchildren;
		}

		if (cmpop_selobj == (Grobj *) NULL) {
			if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ROOTPARENT)) == (Grobj *) NULL) {
				ice_err("Cannot locate selected object.", NONFATAL);
				cmp_del(new_cmp);
				delete opchildren;
				ice_op= MAIN_MENU;
				return;
			}
			cmpop_menuadj(gr);
		}
		else
			gr= cmpop_selobj;
		for (i= 0; i < nopchildren; i++) {
			if (gr == opchildren[i]) {
				ice_err("Duplicate objects within a composite object are not allowed.", NONFATAL);
				cmp_del(new_cmp);
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
			cmp_del(new_cmp);
			delete opchildren;
			ice_op= MAIN_MENU;
			return;
		}
	}
	else if (!(bevt->state & ShiftMask) && !(bevt->state & ControlMask))
		return;

	if (new_cmp->setchildren(nopchildren, opchildren) != GROBJ_SUCCESS) {
		ice_err("Cannot build composite object child lists.", NONFATAL);
		cmp_del(new_cmp);
		delete opchildren;
		ice_op= MAIN_MENU;
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, delcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		ice_op= MAIN_MENU;
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, trcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, cpcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, attrcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((ubditem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, ubdcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((additem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, addcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		menuitem_destroy(ubditem);
		ice_op= MAIN_MENU;
		return;
	}
	if ((rmitem= menuitem_create(LXMI_STRING, new_cmp->getname(),
				LXMI_CLIENTDATA, (char *) new_cmp,
				LXMI_PROC, rmcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(new_cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		menuitem_destroy(ubditem);
		menuitem_destroy(additem);
		ice_op= MAIN_MENU;
		return;
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

	for (i= 0; i < nopchildren; i++) {
		gr= opchildren[i];
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

	ice_op= MAIN_MENU;
	return;
}

void
cmp_setchldrootparent(Composite *parent, Grobj *root)
/*
   Set the rootparent of all
   children of the specified parent.
*/
{
	int nc, i;
	Grobj **c, *g;

	parent->getchildren(&nc, &c);
	for (i= 0; i < nc; i++) {
		g= c[i];
		g->setrootparent(root);
		if (g->gettype() == GROBJ_COMPOSITE)
			cmp_setchldrootparent((Composite *) g, root);
	}
	return;
}

void
delcmp_proc(Menu *m, Menu_item *mi)
{
	Composite *cmp;
	int val;
	char msg[80];
	void cmp_delall(Composite *);

	if ((cmp= (Composite *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		return;
	}

	(void) strcpy(msg, "composite object '");
	(void) strncat(msg, cmp->getname(), 80-strlen("composite object '")-3);
	(void) strcat(msg, "'.");
	if (alert_prompt(progname, dpy, &val,
			LXA_LABEL, "Please confirm or cancel deletion of",
			LXA_LABEL, msg,
			LXA_BUTTON, "Confirm", 0,
			LXA_BUTTON, "Cancel", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	if (val == 1)
		return;

	ice_op= CMP_DELETE;
	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	cmp_delall(cmp);

	if (cmpobjs == (Composite *) NULL) {
		(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Attributes"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Unbind"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Add"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
		(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Remove"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	}

	pg_draw();
	XSync(dpy, False);
	XDefineCursor(dpy, pg_cwin, std_cursor);
	ice_op= MAIN_MENU;
	return;
}

void
cmp_delall(Composite *cmp)
{
	Menu_item *item;
	int nchildren, i;
	Grobj **children, *gr;
	void cmp_del(Composite *);

	cmp->getchildren(&nchildren, &children);
	for (i= 0; i < nchildren; i++) {
		gr= children[i];
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			item= menuitem_find(delpsd_menu, LXMI_CLIENTDATA, (char *) gr);
			delpsd_proc(delpsd_menu, item);
			break;
		case GROBJ_RASTER:
			item= menuitem_find(delras_menu, LXMI_CLIENTDATA, (char *) gr);
			delras_proc(delras_menu, item);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				item= menuitem_find(deltext_menu, LXMI_CLIENTDATA, (char *) gr);
				deltext_proc(deltext_menu, item);
				break;
			case INTOBJ_VECTOR:
				item= menuitem_find(delvec_menu, LXMI_CLIENTDATA, (char *) gr);
				delvec_proc(delvec_menu, item);
				break;
			case INTOBJ_CURVE:
				item= menuitem_find(delcrv_menu, LXMI_CLIENTDATA, (char *) gr);
				delcrv_proc(delcrv_menu, item);
				break;
			case INTOBJ_MARKER:
				item= menuitem_find(delmrk_menu, LXMI_CLIENTDATA, (char *) gr);
				delmrk_proc(delmrk_menu, item);
				break;
			case INTOBJ_RECTANGLE:
				item= menuitem_find(delrect_menu, LXMI_CLIENTDATA, (char *) gr);
				delrect_proc(delrect_menu, item);
				break;
			case INTOBJ_POLYGON:
				item= menuitem_find(delpoly_menu, LXMI_CLIENTDATA, (char *) gr);
				delpoly_proc(delpoly_menu, item);
				break;
			case INTOBJ_AXIS:
				item= menuitem_find(delaxis_menu, LXMI_CLIENTDATA, (char *) gr);
				delaxis_proc(delaxis_menu, item);
				break;
			}
			break;
		case GROBJ_COMPOSITE:
			cmp_delall((Composite *) gr);
			break;
		}
	}

	cmp_del(cmp);
	item= menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(delcmp_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(trcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(trcmp_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cpcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cpcmp_menu, item);
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
	item= menuitem_find(cmpubd_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cmpubd_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmpadd_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cmpadd_menu, item);
	menuitem_destroy(item);
	item= menuitem_find(cmprm_menu, LXMI_CLIENTDATA, (char *) cmp);
	menuitem_delete(cmprm_menu, item);
	menuitem_destroy(item);

	return;
}

void
cmp_del(Composite *cmp)
{
	if (cmpobjs == cmp) {
		if (cmp->succ() == (Dlnk *) NULL)
			cmpobjs= (Composite *) NULL;
		else
			cmpobjs= (Composite *) cmp->succ();
	}
	cmp->unlink();
	delete cmp;
	return;
}
