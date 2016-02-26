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
 * selected_handlers.c - User interface object initialization functions.
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

#include "selected_cui.h"
#include <pm.h>

/*
 * Initialize an instance of object `win'.
 */
selected_win_objects *
selected_win_objects_initialize(ip, owner)
	selected_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (selected_win_objects *) calloc(1, sizeof (selected_win_objects))))
		return (selected_win_objects *) NULL;
	if (!ip->win)
		ip->win = selected_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = selected_win_pan_create(ip, ip->win);
	if (!ip->copy)
		ip->copy = selected_win_copy_create(ip, ip->pan);
	if (!ip->message)
		ip->message = selected_win_message_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.
 */
Xv_opaque
selected_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;

	void		sel_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 262,
		XV_HEIGHT, 125,
		XV_LABEL, "Selected Point",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, sel_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.
 */
Xv_opaque
selected_win_pan_create(ip, owner)
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
 * Create object `copy' in the specified instance.
 */
Xv_opaque
selected_win_copy_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
  extern void copy_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 60,
		XV_Y, 8,
		XV_WIDTH, 139,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Copy Final to Initial",
			PANEL_NOTIFY_PROC, copy_handler,
		NULL);
	return obj;
}


/*
 * Create object `message' in the specified instance.
 */
Xv_opaque
selected_win_message_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 82,
		XV_Y, 48,
		XV_WIDTH, 117,
		XV_HEIGHT, 13,
		PANEL_LABEL_STRING, "Initial                 Final",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `vari' in the specified instance.
 */
Xv_opaque
selected_win_vari_create(ip, owner, field_number)
	caddr_t		ip;
	Xv_opaque	owner;
	int		field_number;
{
	extern Panel_setting	selected_text_notify();
	Xv_opaque	obj;

	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 52,
		XV_Y, 73,
		XV_WIDTH, 99,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "x:",
		PANEL_VALUE_X, 71,
		PANEL_VALUE_Y, 73 + ( (int) 20.0*field_number),
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, selected_text_notify,
		PANEL_CLIENT_DATA, field_number,
		NULL);
	return obj;
}

/*
 * Create object `varf' in the specified instance.
 */
Xv_opaque
selected_win_varf_create(ip, owner, field_number)
	caddr_t		ip;
	Xv_opaque	owner;
	int		field_number;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 152,
		XV_Y, 73,
		XV_WIDTH, 100,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "  :",
		PANEL_VALUE_X, 192,
		PANEL_VALUE_Y, 73 + ( (int) 20.0*field_number),
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

