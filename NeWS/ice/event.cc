/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern void	axisins_event(XEvent *);
extern void	axismv_event(XEvent *);
extern void	axistr_event(XEvent *);
extern void	cmpadd_event(XEvent *);
extern void	cmpattr_event(XEvent *);
extern void	cmpbd_event(XEvent *);
extern void	cmprm_event(XEvent *);
extern void	cmptr_event(XEvent *);
extern void	cmpubd_event(XEvent *);
extern void	crvins_event(XEvent *);
extern void	crvmv_event(XEvent *);
extern void	crvtr_event(XEvent *);
extern void	ice_err(char *, int);
extern void	mainmenu_event(XEvent *);
extern void	mrkins_event(XEvent *);
extern void	mrktr_event(XEvent *);
extern void	polyins_event(XEvent *);
extern void	polytr_event(XEvent *);
extern void	psdins_event(XEvent *);
extern void	psdtr_event(XEvent *);
extern void	pthins_event(XEvent *);
extern void	pthtr_event(XEvent *);
extern void	rasins_event(XEvent *);
extern void	rastr_event(XEvent *);
extern void	rectinsdim_event(XEvent *);
extern void	rectinsloc_event(XEvent *);
extern void	rectinsrot_event(XEvent *);
extern void	recttr_event(XEvent *);
extern void	sel_event(XEvent *);
extern void	textins_event(XEvent *);
extern void	texttr_event(XEvent *);
extern void	vecins_event(XEvent *);
extern void	vecmv_event(XEvent *);
extern void	vectr_event(XEvent *);

void
ice_event(XEvent *event)
{
	switch (ice_op) {
	case NULL_OP:
	case PAGE_ATTR:
	case PSD_INSERTATTR:
	case PSD_ATTR:
	case RAS_INSERTATTR:
	case RAS_ATTR:
	case TEXT_INSERTATTR:
	case TEXT_ATTR:
	case VEC_INSERTATTR:
	case VEC_ATTR:
	case CRV_INSERTATTR:
	case CRV_ATTR:
	case MRK_INSERTATTR:
	case MRK_ATTR:
	case RECT_INSERTATTR:
	case RECT_ATTR:
	case POLY_INSERTATTR:
	case POLY_ATTR:
	case AXIS_INSERTATTR:
	case AXIS_ATTR:
		break;
	case MAIN_MENU:
		mainmenu_event(event);
		break;
	case SEL_ATTR:
	case SEL_DELETE:
	case SEL_TRANSLATE:
	case SEL_COPY:
	case SEL_DUMP:
		sel_event(event);
		break;
	case PSD_INSERTLOC:
		if (pg_loc != PG_CURSORLOC)
			return;
		psdins_event(event);
		break;
	case PSD_TRANSLATE:
	case PSD_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		psdtr_event(event);
		break;
	case RAS_INSERTLOC:
		if (pg_loc != PG_CURSORLOC)
			return;
		rasins_event(event);
		break;
	case RAS_TRANSLATE:
	case RAS_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		rastr_event(event);
		break;
	case TEXT_INSERTLOC:
		if (pg_loc != PG_CURSORLOC)
			return;
		textins_event(event);
		break;
	case TEXT_TRANSLATE:
	case TEXT_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		texttr_event(event);
		break;
	case VEC_INSERTLOC1:
	case VEC_INSERTLOC2:
		if (pg_loc != PG_CURSORLOC)
			return;
		vecins_event(event);
		break;
	case VEC_TRANSLATE:
	case VEC_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		vectr_event(event);
		break;
	case VEC_MVLOC1:
	case VEC_MVLOC2:
		if (pg_loc != PG_CURSORLOC)
			return;
		vecmv_event(event);
		break;
	case CRV_INSERTLOC1:
	case CRV_INSERTCNT1:
	case CRV_INSERTCNT2:
	case CRV_INSERTLOC2:
		if (pg_loc != PG_CURSORLOC)
			return;
		crvins_event(event);
		break;
	case CRV_TRANSLATE:
	case CRV_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		crvtr_event(event);
		break;
	case CRV_MVLOC1:
	case CRV_MVCNT1:
	case CRV_MVCNT2:
	case CRV_MVLOC2:
		if (pg_loc != PG_CURSORLOC)
			return;
		crvmv_event(event);
		break;
	case MRK_INSERTLOC:
		if (pg_loc != PG_CURSORLOC)
			return;
		mrkins_event(event);
		break;
	case MRK_TRANSLATE:
	case MRK_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		mrktr_event(event);
		break;
	case RECT_INSERTLOC:
		if (pg_loc != PG_CURSORLOC)
			return;
		rectinsloc_event(event);
		break;
	case RECT_INSERTROT:
		rectinsrot_event(event);
		break;
	case RECT_INSERTDIM:
		rectinsdim_event(event);
		break;
	case RECT_TRANSLATE:
	case RECT_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		recttr_event(event);
		break;
	case POLY_INSERTLOC:
		if (pg_loc != PG_CURSORLOC)
			return;
		polyins_event(event);
		break;
	case POLY_TRANSLATE:
	case POLY_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		polytr_event(event);
		break;
	case AXIS_INSERTLOC1:
	case AXIS_INSERTLOC2:
		if (pg_loc != PG_CURSORLOC)
			return;
		axisins_event(event);
		break;
	case AXIS_TRANSLATE:
	case AXIS_COPY:
		if (pg_loc != PG_CURSORLOC)
			return;
		axistr_event(event);
		break;
	case AXIS_MVLOC1:
	case AXIS_MVLOC2:
		if (pg_loc != PG_CURSORLOC)
			return;
		axismv_event(event);
		break;
	case CMP_BIND:
		cmpbd_event(event);
		break;
	case CMP_ATTR:
		cmpattr_event(event);
		break;
	case CMP_UNBIND:
		cmpubd_event(event);
		break;
	case CMP_ADD:
		cmpadd_event(event);
		break;
	case CMP_REMOVE:
		cmprm_event(event);
		break;
	case CMP_TRANSLATE:
	case CMP_COPY:
		cmptr_event(event);
		break;
	case PATH_INSERTLOC:
		if (pg_loc != PG_CURSORLOC)
			return;
		pthins_event(event);
		break;
	case PATH_TRANSLATE:
		if (pg_loc != PG_CURSORLOC)
			return;
		pthtr_event(event);
		break;
	default:
		break;
	}
	return;
}

void
wmpr_event(XClientMessageEvent *event)
{
	int val;

	/* WM_SAVE_YOURSELF */
	if (event->data.l[0] == wmsy_atom)
		exit(0);

	/* WM_DELETE_WINDOW */
	else if (event->data.l[0] == wmdw_atom) {
		if (alert_prompt(progname, dpy, &val,
				LXA_LABEL, "Please confirm or cancel program exit.",
				LXA_BUTTON, "Confirm", 0,
				LXA_BUTTON, "Cancel", 1,
				LXA_NULL) == LX_ERROR)
			ice_err("Alert failure.", FATAL);

		switch (val) {
		case 0:
			exit(0);
			break;
		case 1:
		default:
			break;
		}
	}

	return;
}

void
mainmenu_event(XEvent *event)
{
	XButtonPressedEvent *evt;

	if (event->type != ButtonPress)
		return;
	else
		evt= (XButtonPressedEvent *) event;

	switch (evt->button) {
	case Button1:
	case Button2:
		break;
	case Button3:
		menu_show(pg_menu, evt);
		break;
	}
}
