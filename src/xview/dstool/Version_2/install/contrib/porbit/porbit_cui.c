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
 * porbit_ui.c - User interface object initialization functions.
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
#include "porbit_ui.h"

/*
 * Initialize an instance of object `win'.
 */
porbit_win_objects *
porbit_win_objects_initialize(ip, owner)
	porbit_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (porbit_win_objects *) calloc(1, sizeof (porbit_win_objects))))
		return (porbit_win_objects *) NULL;
	if (!ip->win)
		ip->win = porbit_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = porbit_win_pan_create(ip, ip->win);
/*	if (!ip->var)
		ip->var = porbit_win_var_create(ip, ip->pan); */
	if (!ip->value)
		ip->value = porbit_win_value_create(ip, ip->pan);
	if (!ip->period)
		ip->period = porbit_win_period_create(ip, ip->pan);
	if (!ip->algorithm)
		ip->algorithm = porbit_win_algorithm_create(ip, ip->pan);
/*	if (!ip->param)
		ip->param = porbit_win_param_create(ip, ip->pan); */
	if (!ip->stepsize)
		ip->stepsize = porbit_win_stepsize_create(ip, ip->pan);
	if (!ip->numsteps)
		ip->numsteps = porbit_win_numsteps_create(ip, ip->pan);
	if (!ip->go)
		ip->go = porbit_win_go_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
porbit_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	void		porbit_done_proc(); 
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 244,
		XV_HEIGHT, 230,
	        XV_LABEL, "Periodic Orbits",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, porbit_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
porbit_win_pan_create(ip, owner)
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
 * Create object `var' in the specified instance.

 */
Xv_opaque
porbit_win_var_create(ip, owner,i)
     int i;
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		porbit_settings_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 8,
		XV_WIDTH, 199,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 130,
		PANEL_VALUE_Y, 8,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, i,
		PANEL_LABEL_STRING, "Section variable:",
		PANEL_NOTIFY_PROC, porbit_settings_notify,
		PANEL_CHOICE_STRING, 0, "Choice",
		NULL);
	return obj;
}

/*
 * Create object `value' in the specified instance.

 */
Xv_opaque
porbit_win_value_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	porbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 40,
		XV_WIDTH, 114,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Value:",
		PANEL_VALUE_X, 58,
		PANEL_VALUE_Y, 40,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, porbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `period' in the specified instance.

 */
Xv_opaque
porbit_win_period_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	porbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 68,
		XV_WIDTH, 151,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Period:",
		PANEL_VALUE_X, 62,
		PANEL_VALUE_Y, 68,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 1000,
		PANEL_MIN_VALUE, 1,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, porbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `algorithm' in the specified instance.

 */
Xv_opaque
porbit_win_algorithm_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		porbit_settings_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 92,
		XV_WIDTH, 175,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 87,
		PANEL_VALUE_Y, 92,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Algorithm:",
		PANEL_NOTIFY_PROC, porbit_settings_notify,
		PANEL_CHOICE_STRINGS,
			"Newton",
			"Attracting",
			0,
		NULL);
	return obj;
}

/*
 * Create object `param' in the specified instance.

 */
Xv_opaque
porbit_win_param_create(ip, owner,i)
     int i;
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		porbit_settings_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 120,
		XV_WIDTH, 158,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 89,
		PANEL_VALUE_Y, 120,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, i,
		PANEL_LABEL_STRING, "Parameter:",
		PANEL_NOTIFY_PROC, porbit_settings_notify,
		PANEL_CHOICE_STRING, 0, "Choice",
		NULL);
	return obj;
}

/*
 * Create object `stepsize' in the specified instance.

 */
Xv_opaque
porbit_win_stepsize_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	porbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 152,
		XV_WIDTH, 137,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Step size:",
		PANEL_VALUE_X, 81,
		PANEL_VALUE_Y, 152,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, porbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `numsteps' in the specified instance.

 */
Xv_opaque
porbit_win_numsteps_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	porbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 176,
		XV_WIDTH, 190,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Number of steps:",
		PANEL_VALUE_X, 134,
		PANEL_VALUE_Y, 176,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, porbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `go' in the specified instance.

 */
Xv_opaque
porbit_win_go_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		porbit_go_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 120,
		XV_Y, 204,
		XV_WIDTH, 34,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Go",
		PANEL_NOTIFY_PROC, porbit_go_notify,
		NULL);
	return obj;
}

