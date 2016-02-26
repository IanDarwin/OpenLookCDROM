/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"

extern "C" {
int			strcmp(char *, char *);
int			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
}

extern void		addcmp_proc(Menu *, Menu_item *);
extern void		attrcmp_proc(Menu *, Menu_item *);
extern void		cmp_del(Composite *);
extern void		cmp_setchldrootparent(Composite *, Grobj *);
extern void		cmpop_proc(Menu *, Menu_item *);
extern void		cpcmp_proc(Menu *, Menu_item *);
extern void		delcmp_proc(Menu *, Menu_item *);
extern void		dmpobj_proc(Menu *, Menu_item *);
extern void		ice_err(char *, int);
extern void		pg_draw();
extern void		rmcmp_proc(Menu *, Menu_item *);
extern void		trcmp_proc(Menu *, Menu_item *);
extern void		ubdcmp_proc(Menu *, Menu_item *);

extern char ice_iobuf[];
extern int insice_lineno;

void
insicecmp_rd(FILE *fp)
{
	Composite *cmp;
	int iotag, nchildren, nc, ctag, len;
	Grobj **children, *gr;
	char *c, *objnm;
	boolean endfound;
	Menu_item *delitem, *tritem, *cpitem, *dmpitem, *cmpopitem;
	Menu_item *attritem, *ubditem, *additem, *rmitem;

	if (grobjs == (Grobj *) NULL) {
		ice_err("There are no existing objects.", NONFATAL);
		return;
	}

	if (sscanf(ice_iobuf+strlen("%%ICE-Cmp: Begin "), "%d %d", &iotag, &nchildren) != 2) {
		ice_err("Invalid composite value.", NONFATAL);
		return;
	}
	if (iotag <= GROBJ_NULLIOTAG) {
		ice_err("Invalid composite tag value.", NONFATAL);
		return;
	}
	if (nchildren <= 0) {
		ice_err("Invalid composite child count value.", NONFATAL);
		return;
	}
	c= ice_iobuf+strlen("%%ICE-Cmp: Begin ");
	for ( ; isspace(*c); c++);
	for ( ; isgraph(*c); c++);
	for ( ; isspace(*c); c++);
	for ( ; isgraph(*c); c++);
	for ( ; isspace(*c); c++);
	len= strlen(c);
	*(c+len-1)= '\0';
	if ((objnm= new char[len]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(objnm, c);

	if ((cmp= new Composite((Dlnk **) &cmpobjs, objnm)) == (Composite *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		delete objnm;
		return;
	}
	delete objnm;

	if ((children= new Grobj *[nchildren]) == (Grobj **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		cmp_del(cmp);
		return;
	}

	endfound= FALSE;
	nc= 0;
	while (fgets(ice_iobuf, INPUT_BUFSZ, fp) != (char *) NULL) {
		insice_lineno++;

		if (strncmp("%%ICE-", ice_iobuf, strlen("%%ICE-")))
			continue;

		if (!strncmp("%%ICE-Cmp: AC ", ice_iobuf, strlen("%%ICE-Cmp: AC "))) {
			if (nc == nchildren) {
				ice_err("Too many composite children.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			if (sscanf(ice_iobuf+strlen("%%ICE-Cmp: AC "), "%d", &ctag) != 1) {
				ice_err("Invalid composite child tag value.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			if (ctag <= GROBJ_NULLIOTAG) {
				ice_err("Invalid composite child tag value.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
				if (gr->getiotag() == ctag)
					break;
			}
			if (gr == (Grobj *) NULL) {
				ice_err("Cannot locate composite child object.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			children[nc++]= gr;
		}

		else if (!strncmp("%%ICE-Cmp: CC ", ice_iobuf, strlen("%%ICE-Cmp: CC "))) {
			if (nc == nchildren) {
				ice_err("Too many composite children.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			if (sscanf(ice_iobuf+strlen("%%ICE-Cmp: CC "), "%d", &ctag) != 1) {
				ice_err("Invalid composite child tag value.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			if (ctag <= GROBJ_NULLIOTAG) {
				ice_err("Invalid composite child tag value.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			for (gr= (Composite *) cmpobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
				if (gr->getiotag() == ctag)
					break;
			}
			if (gr == (Grobj *) NULL) {
				ice_err("Cannot locate composite child object.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			if (gr == (Grobj *) cmp) {
				ice_err("Composite object may not contain itself.", NONFATAL);
				cmp_del(cmp);
				delete children;
				return;
			}
			children[nc++]= gr;
		}

		else if (!strcmp("%%ICE-Cmp: End\n", ice_iobuf)) {
			endfound= TRUE;
			break;
		}
	}
	if (endfound == FALSE) {
		ice_err("Composite end not found.", NONFATAL);
		cmp_del(cmp);
		delete children;
		return;
	}

	cmp->setiotag(iotag);
	if (cmp->setchildren(nchildren, children) != GROBJ_SUCCESS) {
		ice_err("Cannot build composite object child lists.", NONFATAL);
		cmp_del(cmp);
		delete children;
		return;
	}

	if ((delitem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, delcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		return;
	}
	if ((tritem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, trcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		return;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, cpcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		return;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return;
	}
	if ((attritem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, attrcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		return;
	}
	if ((ubditem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, ubdcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		return;
	}
	if ((additem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, addcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		menuitem_destroy(ubditem);
		return;
	}
	if ((rmitem= menuitem_create(LXMI_STRING, cmp->getname(),
				LXMI_CLIENTDATA, (char *) cmp,
				LXMI_PROC, rmcmp_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		cmp_del(cmp);
		menuitem_destroy(delitem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		menuitem_destroy(cmpopitem);
		menuitem_destroy(attritem);
		menuitem_destroy(ubditem);
		menuitem_destroy(additem);
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

	for (nc= 0; nc < nchildren; nc++) {
		gr= children[nc];
		gr->setparent(cmp);
		gr->setrootparent(cmp);
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
			cmp_setchldrootparent((Composite *) gr, cmp);
			(void) menuitem_set(menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(trcmp_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpattr_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpubd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmpadd_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			(void) menuitem_set(menuitem_find(cmprm_menu, LXMI_CLIENTDATA, (char *) gr), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
			break;
		}
	}

	return;
}
