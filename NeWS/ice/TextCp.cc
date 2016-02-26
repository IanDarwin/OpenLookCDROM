/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Text.h"

extern "C" {
int			strlen(char *);
}

extern void	attrtext_proc(Menu *, Menu_item *);
extern void	cmpop_proc(Menu *, Menu_item *);
extern int	copy_grobj(Grobj *, Grobj *);
extern void	deltext_proc(Menu *, Menu_item *);
extern void	dmpobj_proc(Menu *, Menu_item *);
extern void	ice_err(char *, int);
extern void	text_del(Text *);
extern void	trtext_proc(Menu *, Menu_item *);

extern Grobj *loc_grobj;

extern Text *tr_text;

void
cptext_proc(Menu *m, Menu_item *mi)
{
	Text *text;
	int val;
	char name[LXADEF_MAXSTORE+1];
	float fx, fy;
	int ix, iy;
	char xbuf[30], ybuf[30];
	Text *text_copy(Text *, char *);

	if ((text= (Text *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Text *) NULL) {
		ice_err("Cannot locate selected text object.", NONFATAL);
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
		(void) sprintf(name, "UnnamedText-%d", unnamed_texts++);

	if ((tr_text= text_copy(text, name)) == (Text *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	ice_op= TEXT_COPY;

	if (pg_loc == PG_CURSORLOC) {
		XSelectInput(dpy, pg_cwin, ExposureMask | ButtonPressMask | ButtonReleaseMask | ButtonMotionMask | PointerMotionMask);
		return;
	}

	loc_grobj= (Grobj *) tr_text;
	loc_grobj->getloc(&fx, &fy, &ix, &iy);
	panelitem_set(locattr_panel, locattr_lab, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	panelitem_set(locattr_panel, locattr_abort, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	switch (pg_units) {
	case PG_PIXELS:
		(void) sprintf(xbuf, "%1d", ix);
		(void) sprintf(ybuf, "%1d", iy);
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

Text *
text_copy(Text *text, char *name)
{
	Text *new_text;
	int src, f, j, clr, md;
	char *str;
	float s, l, po;
	Path *p;
	unsigned char r, g, b;
	Menu_item *delitem, *tritem, *attritem, *dmpitem;
	Menu_item *cpitem, *cmpopitem;

	if ((new_text= new Text((Dlnk **) &grobjs, name, text->getsequence())) == (Text *) NULL) {
		ice_err("Cannot create text object.", NONFATAL);
		return (Text *) NULL;
	}
	ntexts++;

	if (copy_grobj(text, new_text) != GROBJ_SUCCESS) {
		text_del(new_text);
		return (Text *) NULL;
	}
	src= text->getsource();
	(void) new_text->setsource(src);
	switch (src) {
	case TEXT_USERINPUT:
		str= text->gettext();
		if (new_text->settext(str) != GROBJ_SUCCESS) {
			ice_err("Cannot copy text string.", NONFATAL);
			text_del(new_text);
			return (Text *) NULL;
		}
		break;
	case TEXT_FILEINPUT:
		str= text->getfilename();
		if (new_text->setfilename(str) != GROBJ_SUCCESS) {
			ice_err("Cannot copy text filename.", NONFATAL);
			text_del(new_text);
			return (Text *) NULL;
		}
		break;
	}
	text->getfont(&f, &str);
	if (new_text->setfont(f, str) != GROBJ_SUCCESS) {
		ice_err("Cannot copy text fontname.", NONFATAL);
		text_del(new_text);
		return (Text *) NULL;
	}
	text->getfontsize(&f, &s, &l);
	(void) new_text->setfontsize(f, s, l);
	j= text->getjustify();
	(void) new_text->setjustify(j);
	text->getpath(&p, &po, &l);
	(void) new_text->setpath(p, po, l);
	text->getforeground(&clr, &r, &g, &b);
	(void) new_text->setforeground(clr, r, g, b);
	text->getbackground(&md, &clr, &r, &g, &b);
	(void) new_text->setbackground(md, clr, r, g, b);

	if ((delitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, deltext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		return (Text *) NULL;
	}
	if ((attritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, attrtext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		return (Text *) NULL;
	}
	if ((tritem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, trtext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		return (Text *) NULL;
	}
	if ((cpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, cptext_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		return (Text *) NULL;
	}
	if ((dmpitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, dmpobj_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		return (Text *) NULL;
	}
	if ((cmpopitem= menuitem_create(LXMI_STRING, name,
				LXMI_CLIENTDATA, (char *) new_text,
				LXMI_PROC, cmpop_proc,
				LXMI_NULL)) == (Menu_item *) NULL) {
		ice_err("Internal toolkit error.", NONFATAL);
		text_del(new_text);
		menuitem_destroy(delitem);
		menuitem_destroy(attritem);
		menuitem_destroy(tritem);
		menuitem_destroy(cpitem);
		menuitem_destroy(dmpitem);
		return (Text *) NULL;
	}
	(void) menuitem_insert(deltext_menu, delitem);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "All"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(attrtext_menu, attritem);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(attr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(trtext_menu, tritem);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cptext_menu, cpitem);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(dmptext_menu, dmpitem);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Select"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Text"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(pg_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_ACTIVE, LXMI_NULL);
	(void) menuitem_insert(cmpoptext_menu, cmpopitem);

	return new_text;
}
