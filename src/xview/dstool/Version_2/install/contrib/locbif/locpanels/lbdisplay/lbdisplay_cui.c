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
#include "lbdisplay_cui.h"

/*
 * Initialize an instance of object `lbdisplaypu'.
 */
lbdisplay_lbdisplaypu_objects *
lbdisplay_lbdisplaypu_objects_initialize(ip, owner)
	lbdisplay_lbdisplaypu_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (lbdisplay_lbdisplaypu_objects *) calloc(1, sizeof (lbdisplay_lbdisplaypu_objects))))
		return (lbdisplay_lbdisplaypu_objects *) NULL;
	if (!ip->lbdisplaypu)
		ip->lbdisplaypu = lbdisplay_lbdisplaypu_lbdisplaypu_create(ip, owner);
	if (!ip->lbdisplaycntl)
		ip->lbdisplaycntl = lbdisplay_lbdisplaypu_lbdisplaycntl_create(ip, ip->lbdisplaypu);
	if (!ip->lbdsoldot)
		ip->lbdsoldot = lbdisplay_rw_create(ip, ip->lbdisplaycntl, "soldot:  ", 16, 16, 8);
	if (!ip->lbdisound)
		ip->lbdisound = lbdisplay_rw_create(ip, ip->lbdisplaycntl, "isound: ", 16, 40, 8);
	if (!ip->lbdiflash)
		ip->lbdiflash = lbdisplay_rw_create(ip, ip->lbdisplaycntl, "iflash:   ", 16, 64, 8);
	if (!ip->lbdmessag)
		ip->lbdmessag = lbdisplay_rw_create(ip, ip->lbdisplaycntl, "messag:", 16, 88, 8);
	if (!ip->lbdmaxnpt)
		ip->lbdmaxnpt = lbdisplay_rw_create(ip, ip->lbdisplaycntl, "maxnpt:", 16, 112, 8);
	if (!ip->lbdinit)
		ip->lbdinit = lbdisplay_rw_create(ip, ip->lbdisplaycntl, "init:       ", 16, 136, 8);

	return ip;
}

/*
 * Create object `lbdisplaypu' in the specified instance.
 */
Xv_opaque
lbdisplay_lbdisplaypu_lbdisplaypu_create(ip, owner)
	lbdisplay_lbdisplaypu_objects	*ip;
	Xv_opaque	owner;
{
	void		lbdisplay_done_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 217,
		XV_HEIGHT, 165,
		XV_LABEL, "locbif Display Panel",
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, lbdisplay_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `lbdisplaycntl' in the specified instance.
 */
Xv_opaque
lbdisplay_lbdisplaypu_lbdisplaycntl_create(ip, owner)
	lbdisplay_lbdisplaypu_objects	*ip;
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
 * Create rw panel items
 */

Xv_opaque
lbdisplay_rw_create(ip, owner, strng , x , y, length)
	lbdisplay_lbdisplaypu_objects	*ip;
	Xv_opaque	owner;
	int		x, y, length;
	char            *strng;
{
	extern Panel_setting	lbd_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, x,
		XV_Y, y,
		PANEL_VALUE_DISPLAY_LENGTH, length,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, strng,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, lbd_notify,
		NULL);
	return obj;
}

