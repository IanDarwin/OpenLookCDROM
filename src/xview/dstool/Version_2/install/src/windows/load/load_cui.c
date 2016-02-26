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
 * load_cui.c - User interface object initialization functions.
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
#include "load_ui.h"

/*
 * Initialize an instance of object `win'.
 */
load_win_objects *
load_win_objects_initialize(ip, owner)
	load_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (load_win_objects *) calloc(1, sizeof (load_win_objects))))
		return (load_win_objects *) NULL;
	if (!ip->win)
		ip->win = load_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = load_win_pan_create(ip, ip->win);
	if (!ip->directory)
		ip->directory = load_win_directory_create(ip, ip->pan);
	if (!ip->filename)
		ip->filename = load_win_filename_create(ip, ip->pan);
	if (!ip->option)
		ip->option = load_win_option_create(ip, ip->pan);
	if (!ip->load)
		ip->load = load_win_load_create(ip, ip->pan);
	if (!ip->type)
		ip->type = load_win_type_create(ip, ip->pan);
	if (!ip->color)
		ip->color = load_win_color_create(ip, ip->pan);
	if (!ip->symbol)
		ip->symbol = load_win_symbol_create(ip, ip->pan);
	if (!ip->varb)
		ip->varb = load_win_varb_create(ip, ip->pan);
	if (!ip->param)
		ip->param = load_win_param_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
load_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	void		load_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 375,
		XV_HEIGHT, 493,
		XV_LABEL, "Load",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_DONE_PROC, load_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
load_win_pan_create(ip, owner)
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
 * Create object `directory' in the specified instance.

 */
Xv_opaque
load_win_directory_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 24,
		XV_WIDTH, 395,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Directory:",
		PANEL_VALUE_X, 95,
		PANEL_VALUE_Y, 24,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, load_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `filename' in the specified instance.

 */
Xv_opaque
load_win_filename_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 60,
		XV_WIDTH, 395,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Filename:",
		PANEL_VALUE_X, 95,
		PANEL_VALUE_Y, 60,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, load_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `option' in the specified instance.

 */
Xv_opaque
load_win_option_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 92,
		XV_WIDTH, 253,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 79,
		PANEL_VALUE_Y, 92,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Option:",
		PANEL_NOTIFY_PROC, load_setting_notify,
		PANEL_CHOICE_STRINGS,
			"DsTool 2.0",
			"DsTool 1.x",
			"Unformatted",
			0,
		NULL);
	return obj;
}

/*
 * Create object `load' in the specified instance.

 */
Xv_opaque
load_win_load_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		load_button_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 168,
		XV_Y, 136,
		XV_WIDTH, 46,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Load",
		PANEL_NOTIFY_PROC, load_button_handler,
		NULL);
	return obj;
}

/*
 * Create object `type' in the specified instance.

 */
Xv_opaque
load_win_type_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 196,
		XV_WIDTH, 197,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 101,
		PANEL_VALUE_Y, 196,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Data Type:",
		PANEL_NOTIFY_PROC, load_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Trajectory",
			"Fixed Pt",
			"Continuation",
			"Parameter",
			"Selected Pt",
			0,
		NULL);
	return obj;
}

/*
 * Create object `color' in the specified instance.

 */
Xv_opaque
load_win_color_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 232,
		XV_WIDTH, 64,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 68,
		PANEL_VALUE_Y, 232,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Color:",
		PANEL_NOTIFY_PROC, load_setting_notify,
		PANEL_CHOICE_STRINGS,
			"On",
			"Off",
			0,
		NULL);
	return obj;
}

/*
 * Create object `symbol' in the specified instance.

 */
Xv_opaque
load_win_symbol_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 264,
		XV_WIDTH, 79,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 83,
		PANEL_VALUE_Y, 264,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Symbol:",
		PANEL_NOTIFY_PROC, load_setting_notify,
		PANEL_CHOICE_STRINGS,
			"On",
			"Off",
			0,
		NULL);
	return obj;
}

/*
 * Create object `varb' in the specified instance.

 */
Xv_opaque
load_win_varb_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_list_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_LIST,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 312,
		PANEL_LIST_WIDTH, 200,
		XV_HEIGHT, 74,
		PANEL_LABEL_STRING, "Variables:   ",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LIST_DISPLAY_ROWS, 3,
		PANEL_READ_ONLY, FALSE,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
                PANEL_NOTIFY_PROC, load_list_notify,
		NULL);
	return obj;
}

/*
 * Create object `param' in the specified instance.

 */
Xv_opaque
load_win_param_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	load_list_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_LIST,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 396,
		PANEL_LIST_WIDTH, 200,
		XV_HEIGHT, 74,
		PANEL_LABEL_STRING, "Parameters:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_LIST_DISPLAY_ROWS, 3,
		PANEL_READ_ONLY, FALSE,
		PANEL_CHOOSE_ONE, FALSE,
		PANEL_CHOOSE_NONE, TRUE,
                PANEL_NOTIFY_PROC, load_list_notify,
		NULL);
	return obj;
}

