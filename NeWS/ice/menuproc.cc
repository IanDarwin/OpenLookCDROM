/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"
#include "Psdoc.h"
#include "Raster.h"
#include "Text.h"
#include "Vector.h"
#include "Marker.h"
#include "Rectangle.h"
#include "Polygon.h"
#include "Axis.h"

extern "C" {
int		unlink(char *);
}

extern void	cmap_copy(Colormap, Colormap);
extern void	cmp_del(Composite *);
extern void	delaxis_proc(Menu *, Menu_item *);
extern void	delcrv_proc(Menu *, Menu_item *);
extern void	delmrk_proc(Menu *, Menu_item *);
extern void	delpoly_proc(Menu *, Menu_item *);
extern void	delpsd_proc(Menu *, Menu_item *);
extern void	delpth_proc(Menu *, Menu_item *);
extern void	delras_proc(Menu *, Menu_item *);
extern void	delrect_proc(Menu *, Menu_item *);
extern void	deltext_proc(Menu *, Menu_item *);
extern void	delvec_proc(Menu *, Menu_item *);
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pth_del(Path *);

void
delall_proc(Menu *m, Menu_item *mi)
{
	int val;
	Grobj *gr;
	Composite *cmp;
	Path *pth;

	if (alert_prompt(progname, dpy, &val,
			LXA_LABEL, "Please confirm or cancel deletion of all objects.",
			LXA_BUTTON, "Confirm", 0,
			LXA_BUTTON, "Cancel", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	if (val == 1)
		return;

	ice_op= DELETE_ALL;

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	while ((gr= grobjs) != (Grobj *) NULL) {
		Menu_item *item;

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
		}
	}

	unnamed_texts= unnamed_vectors= unnamed_markers= 1;
	unnamed_rectangles= unnamed_polygons= unnamed_axes= 1;

	while ((cmp= cmpobjs) != (Composite *) NULL) {
		Menu_item *item;

		cmp_del(cmp);
		item= menuitem_find(delcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
		menuitem_delete(delcmp_menu, item);
		menuitem_destroy(item);
		item= menuitem_find(trcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
		menuitem_delete(trcmp_menu, item);
		menuitem_destroy(item);
		item= menuitem_find(dmpcmp_menu, LXMI_CLIENTDATA, (char *) cmp);
		menuitem_delete(dmpcmp_menu, item);
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
	}

	(void) menuitem_set(menuitem_find(del_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(tr_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(dmp_menu, LXMI_STRING, "Composite"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Attributes"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Unbind"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Add"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);
	(void) menuitem_set(menuitem_find(cmp_menu, LXMI_STRING, "Remove"), LXMI_STATE, LXMI_INACTIVE, LXMI_NULL);

	if (pg_clip != (Path *) NULL) {
		(void) pg_clip->setreferences(PATH_REFDECR);
		pg_clip= (Path *) NULL;
	}

	while ((pth= paths) != (Path *) NULL) {
		Menu_item *item;

		item= menuitem_find(delpth_menu, LXMI_CLIENTDATA, (char *) pth);
		delpth_proc(delpth_menu, item);
	}

	pg_draw();
	XSync(dpy, False);
	XDefineCursor(dpy, pg_cwin, std_cursor);
	ice_op= MAIN_MENU;
	return;
}

void
exit_proc(Menu *m, Menu_item *mi)
{
	int val;

	if (alert_prompt(progname, dpy, &val,
			LXA_LABEL, "Please confirm or cancel program exit.",
			LXA_BUTTON, "Confirm", 0,
			LXA_BUTTON, "Cancel", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	switch (val) {
	case 0:
		if (initpsd_filenm != (char *) NULL) {
			if (unlink(initpsd_filenm) < 0)
				fprintf(stderr, "%s: cannot unlink '%s'.", progname, initpsd_filenm);
		}
		exit(0);
		break;
	case 1:
	default:
		break;
	}
	return;
}
