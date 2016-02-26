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
#include "lbsolver_cui.h"

/*
 * Initialize an instance of object `lbsolverpu'.
 */
lbsolver_lbsolverpu_objects *
lbsolver_lbsolverpu_objects_initialize(ip, owner)
	lbsolver_lbsolverpu_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (lbsolver_lbsolverpu_objects *) calloc(1, sizeof (lbsolver_lbsolverpu_objects))))
		return (lbsolver_lbsolverpu_objects *) NULL;
	if (!ip->lbsolverpu)
		ip->lbsolverpu = lbsolver_lbsolverpu_lbsolverpu_create(ip, owner);
	if (!ip->lbsolvercntl)
		ip->lbsolvercntl = lbsolver_lbsolverpu_lbsolvercntl_create(ip, ip->lbsolverpu);
	if (!ip->lbsitmap)
		ip->lbsitmap = lbsolver_rw_create(ip, ip->lbsolvercntl, "itmap:", 16, 16, 8);
	if (!ip->lbsiorbit)
		ip->lbsiorbit = lbsolver_rw_create(ip, ip->lbsolvercntl, "iorbit:", 16, 40, 8);
	if (!ip->lbsirhs)
		ip->lbsirhs = lbsolver_rw_create(ip, ip->lbsolvercntl, "irhs:  ", 16, 64, 8);
	if (!ip->lbsisec)
		ip->lbsisec = lbsolver_rw_create(ip, ip->lbsolvercntl, "isec:  ", 16, 88, 8);
	if (!ip->lbsdhint)
		ip->lbsdhint = lbsolver_rw_create(ip, ip->lbsolvercntl, "dhint:   ", 16, 112, 15);
	if (!ip->lbsepsint)
		ip->lbsepsint = lbsolver_rw_create(ip, ip->lbsolvercntl, "epsint: ", 16, 136, 15);
	if (!ip->lbsepsrel)
		ip->lbsepsrel = lbsolver_rw_create(ip, ip->lbsolvercntl, "epsrel: ", 16, 160, 15);
	if (!ip->lbsh0int)
		ip->lbsh0int = lbsolver_rw_create(ip, ip->lbsolvercntl, "h0int:   ", 16, 184, 15);
	if (!ip->lbshmxint)
		ip->lbshmxint = lbsolver_rw_create(ip, ip->lbsolvercntl, "hmxint:", 16, 208, 15);
	if (!ip->lbssolver)
		ip->lbssolver = lbsolver_rw_create(ip, ip->lbsolvercntl, "solver: ", 16, 232, 15);
	if (!ip->lbstint)
		ip->lbstint = lbsolver_rw_create(ip, ip->lbsolvercntl, "tint:      ", 16, 256, 15);
	return ip;
}

/*
 * Create object `lbsolverpu' in the specified instance.
 */
Xv_opaque
lbsolver_lbsolverpu_lbsolverpu_create(ip, owner)
	lbsolver_lbsolverpu_objects	*ip;
	Xv_opaque	owner;
{
	void		lbsolver_done_proc();
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 217,
		XV_HEIGHT, 288,
		XV_LABEL, "locbif Orbit Panel",
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, lbsolver_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `lbsolvercntl' in the specified instance.
 */
Xv_opaque
lbsolver_lbsolverpu_lbsolvercntl_create(ip, owner)
	lbsolver_lbsolverpu_objects	*ip;
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
lbsolver_rw_create(ip, owner, strng , x , y, length)
	lbsolver_lbsolverpu_objects	*ip;
	Xv_opaque	owner;
	int		x, y, length;
	char            *strng;
{
	extern Panel_setting	lbs_notify();
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
		PANEL_NOTIFY_PROC, lbs_notify,
		NULL);
	return obj;
}

