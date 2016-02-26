/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <math.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Psdoc.h"
#include "Raster.h"
#include "Intobj.h"
#include "Text.h"

extern void		ice_err(char *, int);
extern void		attraxis_proc(Menu *, Menu_item *);
extern void		attrcrv_proc(Menu *, Menu_item *);
extern void		attrmrk_proc(Menu *, Menu_item *);
extern void		attrpoly_proc(Menu *, Menu_item *);
extern void		attrpsd_proc(Menu *, Menu_item *);
extern void		attrras_proc(Menu *, Menu_item *);
extern void		attrrect_proc(Menu *, Menu_item *);
extern void		attrtext_proc(Menu *, Menu_item *);
extern void		attrvec_proc(Menu *, Menu_item *);
extern void		cpaxis_proc(Menu *, Menu_item *);
extern void		cpcmp_proc(Menu *, Menu_item *);
extern void		cpcrv_proc(Menu *, Menu_item *);
extern void		cpmrk_proc(Menu *, Menu_item *);
extern void		cppoly_proc(Menu *, Menu_item *);
extern void		cppsd_proc(Menu *, Menu_item *);
extern void		cpras_proc(Menu *, Menu_item *);
extern void		cprect_proc(Menu *, Menu_item *);
extern void		cptext_proc(Menu *, Menu_item *);
extern void		cpvec_proc(Menu *, Menu_item *);
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
extern void		traxis_proc(Menu *, Menu_item *);
extern void		trcmp_setup();
extern void		trcrv_proc(Menu *, Menu_item *);
extern void		trmrk_proc(Menu *, Menu_item *);
extern void		trpoly_proc(Menu *, Menu_item *);
extern void		trpsd_proc(Menu *, Menu_item *);
extern void		trras_proc(Menu *, Menu_item *);
extern void		trrect_proc(Menu *, Menu_item *);
extern void		trtext_proc(Menu *, Menu_item *);
extern void		trvec_proc(Menu *, Menu_item *);

extern Composite *tr_cmp;
extern Grobj *tr_atom;

void
attrsel_proc(Menu *m, Menu_item *mi)
{
	ice_op= SEL_ATTR;
	return;
}

void
delsel_proc(Menu *m, Menu_item *mi)
{
	ice_op= SEL_DELETE;
	return;
}

void
trsel_proc(Menu *m, Menu_item *mi)
{
	ice_op= SEL_TRANSLATE;
	return;
}

void
cpsel_proc(Menu *m, Menu_item *mi)
{
	ice_op= SEL_COPY;
	return;
}

void
dmpsel_proc(Menu *m, Menu_item *mi)
{
	ice_op= SEL_DUMP;
	return;
}

void
sel_event(XEvent *event)
{
	XButtonEvent *bevt;
	Grobj *gr;
	Composite *cmp;
	Menu_item *mi;
	Grobj *sel_grobj(int, int, int);

	if (grobjs == (Grobj *) NULL) {
		ice_op= MAIN_MENU;
		return;
	}

	if (event->type != ButtonRelease)
		return;

	bevt= (XButtonEvent *) event;

	switch (ice_op) {
	case SEL_ATTR:
		if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ATOMIC)) == (Grobj *) NULL) {
			ice_op= MAIN_MENU;
			return;
		}
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			mi= menuitem_find(attrpsd_menu, LXMI_CLIENTDATA, (char *) gr);
			attrpsd_proc(attrpsd_menu, mi);
			break;
		case GROBJ_RASTER:
			mi= menuitem_find(attrras_menu, LXMI_CLIENTDATA, (char *) gr);
			attrras_proc(attrras_menu, mi);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				mi= menuitem_find(attrtext_menu, LXMI_CLIENTDATA, (char *) gr);
				attrtext_proc(attrtext_menu, mi);
				break;
			case INTOBJ_VECTOR:
				mi= menuitem_find(attrvec_menu, LXMI_CLIENTDATA, (char *) gr);
				attrvec_proc(attrvec_menu, mi);
				break;
			case INTOBJ_CURVE:
				mi= menuitem_find(attrcrv_menu, LXMI_CLIENTDATA, (char *) gr);
				attrcrv_proc(attrcrv_menu, mi);
				break;
			case INTOBJ_MARKER:
				mi= menuitem_find(attrmrk_menu, LXMI_CLIENTDATA, (char *) gr);
				attrmrk_proc(attrmrk_menu, mi);
				break;
			case INTOBJ_RECTANGLE:
				mi= menuitem_find(attrrect_menu, LXMI_CLIENTDATA, (char *) gr);
				attrrect_proc(attrrect_menu, mi);
				break;
			case INTOBJ_POLYGON:
				mi= menuitem_find(attrpoly_menu, LXMI_CLIENTDATA, (char *) gr);
				attrpoly_proc(attrpoly_menu, mi);
				break;
			case INTOBJ_AXIS:
				mi= menuitem_find(attraxis_menu, LXMI_CLIENTDATA, (char *) gr);
				attraxis_proc(attraxis_menu, mi);
				break;
			}
			break;
		case GROBJ_COMPOSITE:
			break;
		}
		break;

	case SEL_DELETE:
		if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ROOTPARENT)) == (Grobj *) NULL) {
			ice_op= MAIN_MENU;
			return;
		}
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
			mi= menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) gr);
			delcmp_proc(delcmp_menu, mi);
			break;
		}
		ice_op= MAIN_MENU;
		break;

	case SEL_TRANSLATE:
		if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ATOMIC)) == (Grobj *) NULL) {
			ice_op= MAIN_MENU;
			return;
		}
		if ((cmp= (Composite *) gr->getrootparent()) != (Composite *) NULL) {
			tr_cmp= cmp;
			tr_atom= gr;
			trcmp_setup();
			return;
		}
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			mi= menuitem_find(trpsd_menu, LXMI_CLIENTDATA, (char *) gr);
			trpsd_proc(trpsd_menu, mi);
			break;
		case GROBJ_RASTER:
			mi= menuitem_find(trras_menu, LXMI_CLIENTDATA, (char *) gr);
			trras_proc(trras_menu, mi);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				mi= menuitem_find(trtext_menu, LXMI_CLIENTDATA, (char *) gr);
				trtext_proc(trtext_menu, mi);
				break;
			case INTOBJ_VECTOR:
				mi= menuitem_find(trvec_menu, LXMI_CLIENTDATA, (char *) gr);
				trvec_proc(trvec_menu, mi);
				break;
			case INTOBJ_CURVE:
				mi= menuitem_find(trcrv_menu, LXMI_CLIENTDATA, (char *) gr);
				trcrv_proc(trcrv_menu, mi);
				break;
			case INTOBJ_MARKER:
				mi= menuitem_find(trmrk_menu, LXMI_CLIENTDATA, (char *) gr);
				trmrk_proc(trmrk_menu, mi);
				break;
			case INTOBJ_RECTANGLE:
				mi= menuitem_find(trrect_menu, LXMI_CLIENTDATA, (char *) gr);
				trrect_proc(trrect_menu, mi);
				break;
			case INTOBJ_POLYGON:
				mi= menuitem_find(trpoly_menu, LXMI_CLIENTDATA, (char *) gr);
				trpoly_proc(trpoly_menu, mi);
				break;
			case INTOBJ_AXIS:
				mi= menuitem_find(traxis_menu, LXMI_CLIENTDATA, (char *) gr);
				traxis_proc(traxis_menu, mi);
				break;
			}
			break;
		}
		break;

	case SEL_COPY:
		if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ROOTPARENT)) == (Grobj *) NULL) {
			ice_op= MAIN_MENU;
			return;
		}
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			mi= menuitem_find(cppsd_menu, LXMI_CLIENTDATA, (char *) gr);
			cppsd_proc(cppsd_menu, mi);
			break;
		case GROBJ_RASTER:
			mi= menuitem_find(cpras_menu, LXMI_CLIENTDATA, (char *) gr);
			cpras_proc(cpras_menu, mi);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				mi= menuitem_find(cptext_menu, LXMI_CLIENTDATA, (char *) gr);
				cptext_proc(cptext_menu, mi);
				break;
			case INTOBJ_VECTOR:
				mi= menuitem_find(cpvec_menu, LXMI_CLIENTDATA, (char *) gr);
				cpvec_proc(cpvec_menu, mi);
				break;
			case INTOBJ_CURVE:
				mi= menuitem_find(cpcrv_menu, LXMI_CLIENTDATA, (char *) gr);
				cpcrv_proc(cpcrv_menu, mi);
				break;
			case INTOBJ_MARKER:
				mi= menuitem_find(cpmrk_menu, LXMI_CLIENTDATA, (char *) gr);
				cpmrk_proc(cpmrk_menu, mi);
				break;
			case INTOBJ_RECTANGLE:
				mi= menuitem_find(cprect_menu, LXMI_CLIENTDATA, (char *) gr);
				cprect_proc(cprect_menu, mi);
				break;
			case INTOBJ_POLYGON:
				mi= menuitem_find(cppoly_menu, LXMI_CLIENTDATA, (char *) gr);
				cppoly_proc(cppoly_menu, mi);
				break;
			case INTOBJ_AXIS:
				mi= menuitem_find(cpaxis_menu, LXMI_CLIENTDATA, (char *) gr);
				cpaxis_proc(cpaxis_menu, mi);
				break;
			}
			break;
		case GROBJ_COMPOSITE:
			mi= menuitem_find(cpcmp_menu, LXMI_CLIENTDATA, (char *) gr);
			cpcmp_proc(cpcmp_menu, mi);
			break;
		}
		break;

	case SEL_DUMP:
		if ((gr= sel_grobj(bevt->x, bevt->y, SEL_ROOTPARENT)) == (Grobj *) NULL) {
			ice_op= MAIN_MENU;
			return;
		}
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			mi= menuitem_find(dmppsd_menu, LXMI_CLIENTDATA, (char *) gr);
			dmpobj_proc(dmppsd_menu, mi);
			break;
		case GROBJ_RASTER:
			mi= menuitem_find(dmpras_menu, LXMI_CLIENTDATA, (char *) gr);
			dmpobj_proc(dmpras_menu, mi);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) gr)->getintobjtype()) {
			case INTOBJ_TEXT:
				mi= menuitem_find(dmptext_menu, LXMI_CLIENTDATA, (char *) gr);
				dmpobj_proc(dmptext_menu, mi);
				break;
			case INTOBJ_VECTOR:
				mi= menuitem_find(dmpvec_menu, LXMI_CLIENTDATA, (char *) gr);
				dmpobj_proc(dmpvec_menu, mi);
				break;
			case INTOBJ_CURVE:
				mi= menuitem_find(dmpcrv_menu, LXMI_CLIENTDATA, (char *) gr);
				dmpobj_proc(dmpcrv_menu, mi);
				break;
			case INTOBJ_MARKER:
				mi= menuitem_find(dmpmrk_menu, LXMI_CLIENTDATA, (char *) gr);
				dmpobj_proc(dmpmrk_menu, mi);
				break;
			case INTOBJ_RECTANGLE:
				mi= menuitem_find(dmprect_menu, LXMI_CLIENTDATA, (char *) gr);
				dmpobj_proc(dmprect_menu, mi);
				break;
			case INTOBJ_POLYGON:
				mi= menuitem_find(dmppoly_menu, LXMI_CLIENTDATA, (char *) gr);
				dmpobj_proc(dmppoly_menu, mi);
				break;
			case INTOBJ_AXIS:
				mi= menuitem_find(dmpaxis_menu, LXMI_CLIENTDATA, (char *) gr);
				dmpobj_proc(dmpaxis_menu, mi);
				break;
			}
			break;
		case GROBJ_COMPOSITE:
			mi= menuitem_find(dmpcmp_menu, LXMI_CLIENTDATA, (char *) gr);
			dmpobj_proc(dmpcmp_menu, mi);
			break;
		}
		break;

	}
	return;
}

Grobj *
sel_grobj(int evtx, int evty, int flag)
{
	Grobj *g, *mg;
	double dist, mdist;
	double xdiff, ydiff;
	int vx, vy, sx, sy, x, y, nvert;
	float fx, fy, poff, lspace, *xvert, *yvert;
	Path *pth;

	if (grobjs == (Grobj *) NULL)
		return (Grobj *) NULL;

	switch (flag) {
	case SEL_ATOMIC:
	case SEL_ROOTPARENT:
		break;
	default:
		return (Grobj *) NULL;
	}

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	sx= evtx+vx;
	sy= evty+vy;
	sy= pg_pixheight-1-sy;

	mg= grobjs;
	mg->getloc(&fx, &fy, &x, &y);
	if (mg->gettype() == GROBJ_INTOBJ) {
		if (((Intobj *) mg)->getintobjtype() == INTOBJ_TEXT) {
			if (((Text *) mg)->getjustify() == TEXT_PATH) {
				((Text *) mg)->getpath(&pth, &poff, &lspace);
				if (pth != (Path *) NULL) {
					pth->getvertices(&nvert, &xvert, &yvert);
					x= (int) ((float) (xvert[0]*((float) pg_dpi)));
					y= (int) ((float) (yvert[0]*((float) pg_dpi)));
				}
			}
		}
	}
	xdiff= (double) (sx-x);
	ydiff= (double) (sy-y);
	mdist= sqrt((double) ((xdiff*xdiff)+(ydiff*ydiff)));

	for (g= (Grobj *) mg->succ(); g != (Grobj *) NULL; g= (Grobj *) g->succ()) {
		g->getloc(&fx, &fy, &x, &y);
		if (g->gettype() == GROBJ_INTOBJ) {
			if (((Intobj *) g)->getintobjtype() == INTOBJ_TEXT) {
				if (((Text *) g)->getjustify() == TEXT_PATH) {
					((Text *) g)->getpath(&pth, &poff, &lspace);
					if (pth != (Path *) NULL) {
						pth->getvertices(&nvert, &xvert, &yvert);
						x= (int) ((float) (xvert[0]*((float) pg_dpi)));
						y= (int) ((float) (yvert[0]*((float) pg_dpi)));
					}
				}
			}
		}
		xdiff= (double) (sx-x);
		ydiff= (double) (sy-y);
		dist= sqrt((double) ((xdiff*xdiff)+(ydiff*ydiff)));
		if (mdist > dist) {
			mdist= dist;
			mg= g;
		}
	}

	if ((flag == SEL_ROOTPARENT) && ((g= mg->getrootparent()) != (Grobj *) NULL))
		return g;
	else
		return mg;
}

Composite *
sel_cmpobj(int evtx, int evty)
{
	Composite *c, *mc;
	Grobj *g, *mg;
	double dist, mdist;
	double xdiff, ydiff;
	int vx, vy, sx, sy, x, y, nvert;
	float fx, fy, poff, lspace, *xvert, *yvert;
	Path *pth;

	if (cmpobjs == (Composite *) NULL)
		return (Composite *) NULL;

	vx= *((int *) canvas_get(pg_canvas, LXC_XOFFSET));
	vy= *((int *) canvas_get(pg_canvas, LXC_YOFFSET));

	sx= evtx+vx;
	sy= evty+vy;
	sy= pg_pixheight-1-sy;

	/* locate the first object with a parent */
	for (mg= grobjs; mg != (Grobj *) NULL; mg= (Grobj *) mg->succ()) {
		if ((mc= (Composite *) mg->getrootparent()) != (Composite *) NULL)
			break;
	}
	mg->getloc(&fx, &fy, &x, &y);
	if (mg->gettype() == GROBJ_INTOBJ) {
		if (((Intobj *) mg)->getintobjtype() == INTOBJ_TEXT) {
			if (((Text *) mg)->getjustify() == TEXT_PATH) {
				((Text *) mg)->getpath(&pth, &poff, &lspace);
				if (pth != (Path *) NULL) {
					pth->getvertices(&nvert, &xvert, &yvert);
					x= (int) ((float) (xvert[0]*((float) pg_dpi)));
					y= (int) ((float) (yvert[0]*((float) pg_dpi)));
				}
			}
		}
	}
	xdiff= (double) (sx-x);
	ydiff= (double) (sy-y);
	mdist= sqrt((double) ((xdiff*xdiff)+(ydiff*ydiff)));

	for (g= (Grobj *) mg->succ(); g != (Grobj *) NULL; g= (Grobj *) g->succ()) {

		/* ignore objects with no parent or whose parent is mc */
		if ((c= (Composite *) g->getrootparent()) == (Composite *) NULL)
			continue;
		if (c == mc)
			continue;

		g->getloc(&fx, &fy, &x, &y);
		if (g->gettype() == GROBJ_INTOBJ) {
			if (((Intobj *) g)->getintobjtype() == INTOBJ_TEXT) {
				if (((Text *) g)->getjustify() == TEXT_PATH) {
					((Text *) g)->getpath(&pth, &poff, &lspace);
					if (pth != (Path *) NULL) {
						pth->getvertices(&nvert, &xvert, &yvert);
						x= (int) ((float) (xvert[0]*((float) pg_dpi)));
						y= (int) ((float) (yvert[0]*((float) pg_dpi)));
					}
				}
			}
		}
		xdiff= (double) (sx-x);
		ydiff= (double) (sy-y);
		dist= sqrt((double) ((xdiff*xdiff)+(ydiff*ydiff)));
		if (mdist > dist) {
			mdist= dist;
			mg= g;
			mc= c;
		}
	}

	return mc;
}
