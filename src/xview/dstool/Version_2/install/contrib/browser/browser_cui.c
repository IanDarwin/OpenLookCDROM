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
 * browser_ui.c - User interface object initialization functions.
 */

#include <stdio.h>
#include <stdlib.h>
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
#include "browser_cui.h"

/*
 * Initialize an instance of object `win'.
 */
browser_win_objects *
browser_win_objects_initialize(ip, owner)
	browser_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (browser_win_objects *) calloc(1, sizeof (browser_win_objects))))
		return (browser_win_objects *) NULL;
	if (!ip->win)
		ip->win = browser_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = browser_win_pan_create(ip, ip->win);
/*	if (!ip->memory)
		ip->memory = browser_win_memory_create(ip, ip->pan); */
	if (!ip->delete)
		ip->delete = browser_win_delete_create(ip, ip->pan);
	if (!ip->highlight)
		ip->highlight = browser_win_highlight_create(ip, ip->pan);
	if (!ip->flow)
		ip->flow = browser_win_flow_create(ip, ip->pan);
	if (!ip->flowmes)
		ip->flowmes = browser_win_flowmes_create(ip, ip->pan);
	if (!ip->copy)
		ip->copy = browser_win_copy_create(ip, ip->pan);
	if (!ip->traj)
		ip->traj = browser_win_traj_create(ip, ip->pan);
	if (!ip->trajmes)
		ip->trajmes = browser_win_trajmes_create(ip, ip->pan);
	if (!ip->point)
		ip->point = browser_win_point_create(ip, ip->pan);
	if (!ip->pointmes)
		ip->pointmes = browser_win_pointmes_create(ip, ip->pan);
	if (!ip->output)
		ip->output = browser_win_output_create(ip, ip->pan);
	if (!ip->message4)
		ip->message4 = browser_win_message4_create(ip, ip->pan);
	if (!ip->message5)
		ip->message5 = browser_win_message5_create(ip, ip->pan);
	if (!ip->message6)
		ip->message6 = browser_win_message6_create(ip, ip->pan);
/*	if (!ip->vars)
		ip->vars = browser_win_vars_create(ip, ip->pan);
	if (!ip->header)
		ip->header = browser_win_header_create(ip, ip->pan);
	if (!ip->body)
		ip->body = browser_win_body_create(ip, ip->pan); */
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
browser_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	void		browser_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 466,
		XV_HEIGHT, 326,
		XV_LABEL, "Data Browser",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, browser_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
browser_win_pan_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Notify_value	browser_event_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	xv_set(obj, WIN_CONSUME_EVENTS,
		LOC_WINENTER,
	        LOC_WINEXIT,
		NULL, NULL);
	notify_interpose_event_func(obj,
		(Notify_func) browser_event_notify, NOTIFY_SAFE);
	return obj;
}

/*
 * Create object `memory' in the specified instance.

 */
Xv_opaque
browser_win_memory_create(ip, owner, i)
     int i;
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		browser_settings_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 12,
		XV_Y, 8,
		XV_WIDTH, 143,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 78,
		PANEL_VALUE_Y, 8,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, i,
		PANEL_LABEL_STRING, "Memory:",
		PANEL_NOTIFY_PROC, browser_settings_notify,
		NULL);
	return obj;
}

/*
 * Create object `delete' in the specified instance.

 */
Xv_opaque
browser_win_delete_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		browser_delete_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 208,
		XV_Y, 12,
		XV_WIDTH, 93,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "DELETE flow",
		PANEL_NOTIFY_PROC, browser_delete_notify,
		NULL);
	return obj;
}

/*
 * Create object `highlight' in the specified instance.
-1z
 */
Xv_opaque
browser_win_highlight_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		browser_highlight_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 308,
		XV_Y, 64,
		XV_WIDTH, 145,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 383,
		PANEL_VALUE_Y, 64,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Highlight:",
		PANEL_NOTIFY_PROC, browser_highlight_notify,
		PANEL_CHOICE_STRINGS,
			"Off",
			"On",
			0,
		NULL);
	return obj;
}

/*
 * Create object `flow' in the specified instance.

 */
Xv_opaque
browser_win_flow_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	browser_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 52,
		XV_Y, 40,
		XV_WIDTH, 141,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Flow:",
		PANEL_VALUE_X, 94,
		PANEL_VALUE_Y, 40,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 100000000,
		PANEL_MIN_VALUE, 1,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, browser_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `flowmes' in the specified instance.

 */
Xv_opaque
browser_win_flowmes_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 212,
		XV_Y, 40,
		XV_WIDTH, 0,
		XV_HEIGHT, 0,
		NULL);
	return obj;
}

/*
 * Create object `copy' in the specified instance.

 */
Xv_opaque
browser_win_copy_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		browser_copy_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 308,
		XV_Y, 88,
		XV_WIDTH, 120,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Copy to Selected",
		PANEL_NOTIFY_PROC, browser_copy_notify,
		NULL);
	return obj;
}

/*
 * Create object `traj' in the specified instance.

 */
Xv_opaque
browser_win_traj_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	browser_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 12,
		XV_Y, 60,
		XV_WIDTH, 180,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Trajectory:",
		PANEL_VALUE_X, 93,
		PANEL_VALUE_Y, 60,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 100000000,
		PANEL_MIN_VALUE, 1,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, browser_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `trajmes' in the specified instance.

 */
Xv_opaque
browser_win_trajmes_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 212,
		XV_Y, 60,
		XV_WIDTH, 0,
		XV_HEIGHT, 0,
		NULL);
	return obj;
}

/*
 * Create object `point' in the specified instance.

 */
Xv_opaque
browser_win_point_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	browser_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 48,
		XV_Y, 80,
		XV_WIDTH, 145,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Point:",
		PANEL_VALUE_X, 94,
		PANEL_VALUE_Y, 80,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 100000000,
		PANEL_MIN_VALUE, 1,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, browser_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `pointmes' in the specified instance.

 */
Xv_opaque
browser_win_pointmes_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 212,
		XV_Y, 80,
		XV_WIDTH, 0,
		XV_HEIGHT, 0,
		NULL);
	return obj;
}

/*
 * Create object `output' in the specified instance.

 */
Xv_opaque
browser_win_output_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		browser_output_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 308,
		XV_Y, 40,
		XV_WIDTH, 109,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Write to stdout",
		PANEL_NOTIFY_PROC, browser_output_notify,
		NULL);
	return obj;
}

/*
 * Create object `message4' in the specified instance.

 */
Xv_opaque
browser_win_message4_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 88,
		XV_Y, 116,
		XV_WIDTH, 35,
		XV_HEIGHT, 13,
		PANEL_LABEL_STRING, "Point",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `message5' in the specified instance.

 */
Xv_opaque
browser_win_message5_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 208,
		XV_Y, 116,
		XV_WIDTH, 48,
		XV_HEIGHT, 13,
		PANEL_LABEL_STRING, "Header",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `message6' in the specified instance.

 */
Xv_opaque
browser_win_message6_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 336,
		XV_Y, 116,
		XV_WIDTH, 33,
		XV_HEIGHT, 13,
		PANEL_LABEL_STRING, "Body",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `vars' in the specified instance.

 */
Xv_opaque
browser_win_vars_create(ip, owner, i)
     int i;
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 12,
		XV_Y, 136,
		XV_WIDTH, 151,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "varname:",
		PANEL_VALUE_X, 83,
		PANEL_VALUE_Y, 136+20*i,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `header' in the specified instance.

 */
Xv_opaque
browser_win_header_create(ip, owner, i)
     int i;
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 188,
		XV_Y, 136,
		XV_WIDTH, 80,
		XV_HEIGHT, 15,
		PANEL_VALUE_X, 188,
		PANEL_VALUE_Y, 136+20*i,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `body' in the specified instance.

 */
Xv_opaque
browser_win_body_create(ip, owner, i)
     int i;
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 312,
		XV_Y, 136,
		XV_WIDTH, 80,
		XV_HEIGHT, 15,
		PANEL_VALUE_X, 312,
		PANEL_VALUE_Y, 136+20*i,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, TRUE,
		NULL);
	return obj;
}

