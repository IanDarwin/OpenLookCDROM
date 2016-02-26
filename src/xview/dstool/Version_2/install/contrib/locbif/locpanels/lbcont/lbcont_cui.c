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
#include "lbcont_cui.h"

/*
 * Initialize an instance of object `lbcontpu'.
 */
lbcont_lbcontpu_objects *
lbcont_lbcontpu_objects_initialize(ip, owner)
	lbcont_lbcontpu_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (lbcont_lbcontpu_objects *) calloc(1, sizeof (lbcont_lbcontpu_objects))))
		return (lbcont_lbcontpu_objects *) NULL;
	if (!ip->lbcontpu)
		ip->lbcontpu = lbcont_lbcontpu_lbcontpu_create(ip, owner);
	if (!ip->lbcontcntl)
		ip->lbcontcntl = lbcont_lbcontpu_lbcontcntl_create(ip, ip->lbcontpu);
	if (!ip->lbch0crv)
		ip->lbch0crv = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 24, "h0crv:   ");
	if (!ip->lbcalgcrv)
		ip->lbcalgcrv = lbcont_lbcontpu_lbcalgcrv_create(ip, ip->lbcontcntl);
	if (!ip->lbchmxcrv)
		ip->lbchmxcrv = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 48, "hmxcrv:");
	if (!ip->lbcangcrv)
		ip->lbcangcrv = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 72, "angcrv: ");
	if (!ip->lbcdhcrv)
		ip->lbcdhcrv = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 96, "dhcrv:   ");
	if (!ip->lbcdhjac)
		ip->lbcdhjac = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 120, "dhjac:   ");
	if (!ip->lbcmaxit)
		ip->lbcmaxit = lbcont_lbcontpu_lbcmaxit_create(ip, ip->lbcontcntl);
	if (!ip->lbcepscrv)
		ip->lbcepscrv = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 144, "epscrv: ");
	if (!ip->lbcmodit)
		ip->lbcmodit = lbcont_lbcontpu_lbcmodit_create(ip, ip->lbcontcntl);
	if (!ip->lbcepscrs)
		ip->lbcepscrs = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 168, "epscrs: ");
	if (!ip->lbcepszer)
		ip->lbcepszer = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 192, "epszer: ");
	if (!ip->lbciprsng)
		ip->lbciprsng = lbcont_lbcontpu_lbciprsng_create(ip, ip->lbcontcntl);
	if (!ip->lbcepsext)
		ip->lbcepsext = lbcont_lbcontpu_rw_create(ip, ip->lbcontcntl, 16, 216, "epsext: ");
	return ip;
}

/*
 * Create object `lbcontpu' in the specified instance.
 */
Xv_opaque
lbcont_lbcontpu_lbcontpu_create(ip, owner)
	lbcont_lbcontpu_objects	*ip;
	Xv_opaque	owner;
{
	void		lbcont_done_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 419,
		XV_HEIGHT, 247,
		XV_LABEL, "locbif Continuation Panel",
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, lbcont_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `lbcontcntl' in the specified instance.
 */
Xv_opaque
lbcont_lbcontpu_lbcontcntl_create(ip, owner)
	lbcont_lbcontpu_objects	*ip;
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
 * Create the rw objects in the specified instance
 */
Xv_opaque
lbcont_lbcontpu_rw_create(ip, owner, x, y, strng)
	lbcont_lbcontpu_objects	*ip;
	Xv_opaque	owner;
	char	        *strng;
	int		x, y;
{
	extern Panel_setting	lbcont_rw_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, x,
		XV_Y, y,
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, strng,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, lbcont_rw_notify,
		NULL);
	return obj;
}

/*
 * Create object `lbcalgcrv' in the specified instance.
 */
Xv_opaque
lbcont_lbcontpu_lbcalgcrv_create(ip, owner)
	lbcont_lbcontpu_objects	*ip;
	Xv_opaque	owner;
{
	extern void		lbcalgcrv_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 216,
		XV_Y, 32,
		PANEL_CHOICE_NCOLS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "algcrv:",
		PANEL_NOTIFY_PROC, lbcalgcrv_notify,
		PANEL_CHOICE_STRINGS,
			"Doubling/Halving",
			"Explicit Curvature",
			"Implicit Curvature",
			NULL,
		NULL);
	return obj;
}


/*
 * Create object `lbcmaxit' in the specified instance.
 */
Xv_opaque
lbcont_lbcontpu_lbcmaxit_create(ip, owner)
	lbcont_lbcontpu_objects	*ip;
	Xv_opaque	owner;
{
	extern Panel_setting	lbcmaxit_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 240,
		XV_Y, 128,
		PANEL_VALUE_DISPLAY_LENGTH, 5,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "maxit: ",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 500,
		PANEL_MIN_VALUE, 0,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, lbcmaxit_notify,
		NULL);
	return obj;
}


/*
 * Create object `lbcmodit' in the specified instance.
 */
Xv_opaque
lbcont_lbcontpu_lbcmodit_create(ip, owner)
	lbcont_lbcontpu_objects	*ip;
	Xv_opaque	owner;
{
	extern Panel_setting	lbcmodit_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 240,
		XV_Y, 160,
		PANEL_VALUE_DISPLAY_LENGTH, 5,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "modit: ",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 500,
		PANEL_MIN_VALUE, 0,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, lbcmodit_notify,
		NULL);
	return obj;
}



/*
 * Create object `lbciprsng' in the specified instance.
 */
Xv_opaque
lbcont_lbcontpu_lbciprsng_create(ip, owner)
	lbcont_lbcontpu_objects	*ip;
	Xv_opaque	owner;
{
	extern Panel_setting	lbcipsrng_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 240,
		XV_Y, 192,
		PANEL_VALUE_DISPLAY_LENGTH, 5,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "ipsrng:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 100,
		PANEL_MIN_VALUE, 0,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, lbcipsrng_notify,
		NULL);
	return obj;
}


