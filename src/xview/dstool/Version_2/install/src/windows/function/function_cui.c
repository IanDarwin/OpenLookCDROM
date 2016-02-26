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
/*
 * function_cui.c - User interface object initialization functions.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <gcm.h>
#include "function_cui.h"

/*
 * Initialize an instance of object `win'.
 */
function_win_objects *
function_win_objects_initialize(ip, owner)
	function_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (function_win_objects *) calloc(1, sizeof (function_win_objects))))
		return (function_win_objects *) NULL;
	if (!ip->win)
		ip->win = function_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = function_win_pan_create(ip, ip->win);
	if (!ip->message)
		ip->message = function_win_message_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
function_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;

	void		function_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 344,
		XV_HEIGHT, 67,
		XV_LABEL, "Function Settings",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		FRAME_DONE_PROC, function_done_proc,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
function_win_pan_create(ip, owner)
	caddr_t		ip;
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
 * Create object `message' in the specified instance.

 */
Xv_opaque
function_win_message_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 88,
		XV_Y, 8,
		XV_WIDTH, 169,
		XV_HEIGHT, 13,
		PANEL_LABEL_STRING, "Initial                       Final",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `funi' in the specified instance.
 */
Xv_opaque
function_win_funi_create(ip, owner, field_number)
	caddr_t		ip;
	Xv_opaque	owner;
	int		field_number;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 32,
		XV_WIDTH, 174,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "function1:",
		PANEL_VALUE_X, 94,
		PANEL_VALUE_Y, 32 + ((int) 20*field_number),
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `funf' in the specified instance.

 */
Xv_opaque
function_win_funf_create(ip, owner, field_number)
	caddr_t		ip;
	Xv_opaque	owner;
	int		field_number;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 212,
		XV_Y, 32,
		XV_WIDTH, 108,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, ":",
		PANEL_VALUE_X, 224,
		PANEL_VALUE_Y, 32 + ((int) 20*field_number),
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

