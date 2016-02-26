/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <NeWS/psio.h>

#if defined(sun3)
#include "icecps-sun3.h"
#elif defined(sun4)
#include "icecps-sun4.h"
#endif

#include "ice_defines.h"
#include "ice_externs.h"

extern void		ice_err(char *, int);

void
attrins_proc(Menu *m, Menu_item *mi)
{
	panelitem_set(insattr_panel, insattr_newobj, LXPENUM_VALUE, ins_newobj, LXPI_NULL);

	ice_op= INS_ATTR;
	XMapRaised(dpy, insattr_frame);
	return;
}

void
insattrcont_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, insattr_frame);
	ice_op= MAIN_MENU;

	ins_newobj= *((int *) panelitem_get(insattr_panel, insattr_newobj, LXPENUM_VALUE));

	return;
}

void
insattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, insattr_frame);
	ice_op= MAIN_MENU;
	return;
}
