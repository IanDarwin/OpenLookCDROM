/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include "lbstate_cui.h"

/*
 * Initialize an instance of object `locstatepu'.
 */
lcstate_locstatepu_objects *
lcstate_locstatepu_objects_initialize(ip, owner)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (lcstate_locstatepu_objects *) calloc(1, sizeof (lcstate_locstatepu_objects))))
		return (lcstate_locstatepu_objects *) NULL;
	if (!ip->locstatepu)
		ip->locstatepu = lcstate_locstatepu_locstatepu_create(ip, owner);
	if (!ip->lccntl)
		ip->lccntl = lcstate_locstatepu_lccntl_create(ip, ip->locstatepu);
	if (!ip->lcsel)
		ip->lcsel = lcstate_locstatepu_lcsel_create(ip, ip->lccntl);
	if (!ip->lcupdate)
		ip->lcupdate = lcstate_locstatepu_lcupdate_create(ip, ip->lccntl);
	if (!ip->lcstatus)
		ip->lcstatus = lcstate_locstatepu_lcstatus_create(ip, ip->lccntl);
	return ip;
}

/*
 * Create object `locstatepu' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_locstatepu_create(ip, owner)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
{
	void		lbstate_done_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 450,
		XV_HEIGHT, 120,
		XV_LABEL, "locbif State Panel",
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, lbstate_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `lccntl' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lccntl_create(ip, owner)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `lcsel' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lcsel_create(ip, owner)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lcstate_locstatepu_lcsel_notify_callback();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TOGGLE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 16,
		PANEL_CHOICE_NROWS, 2,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Data:",
		PANEL_NOTIFY_PROC, lcstate_locstatepu_lcsel_notify_callback,
		PANEL_CHOICE_STRINGS,
			"State",
			"Parameters",
			"Function Values",
			"Eigenvalues",
			NULL,
		PANEL_VALUE, 0,
		NULL);
	return obj;
}

/*
 * Create object `lcupdate' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lcupdate_create(ip, owner)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lcupdate_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 320,
		XV_Y, 32,
		PANEL_LABEL_STRING, "Update Sel Pt",
		PANEL_NOTIFY_PROC, lcupdate_notify,
		NULL);
	return obj;
}

/*
 * Create object `lcstate' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lcstate_create(ip, owner, x, y, label)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
	int		x, y;
	char		*label;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, label,
		PANEL_VALUE_X, x,
		PANEL_VALUE_Y, y,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `lcparm' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lcparm_create(ip, owner, x, y, label)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
	int		x, y;
	char		*label;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		PANEL_VALUE_X, x,
		PANEL_VALUE_Y, y,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, label,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `lcfunc' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lcfunc_create(ip, owner, x, y, label)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
	int		x, y;
	char		*label;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		PANEL_VALUE_X, x,
		PANEL_VALUE_Y, y,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, label,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `lccharact' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lccharact_create(ip, owner, x, y)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
	int		x, y;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		PANEL_VALUE_X, x,
		PANEL_VALUE_Y, y,
		PANEL_VALUE_DISPLAY_LENGTH, 24,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, TRUE,
		NULL); 
	return obj;
}



/*
 * Create object `lcstatus' in the specified instance.
 */
Xv_opaque
lcstate_locstatepu_lcstatus_create(ip, owner)
	lcstate_locstatepu_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 10,
		XV_Y, 90,
		PANEL_VALUE_UNDERLINED, FALSE,
		PANEL_VALUE_DISPLAY_LENGTH, 56,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LABEL_STRING, "Status:",
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}
