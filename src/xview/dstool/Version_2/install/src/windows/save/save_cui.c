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
 * save_cui.c - User interface object initialization functions.
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
#include "save_ui.h"

/*
 * Initialize an instance of object `win'.
 */
save_win_objects *
save_win_objects_initialize(ip, owner)
	save_win_objects	*ip;
	Xv_opaque	owner;
{

	if (!ip && !(ip = (save_win_objects *) calloc(1, sizeof (save_win_objects))))
		return (save_win_objects *) NULL;
	if (!ip->win)
		ip->win = save_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = save_win_pan_create(ip, ip->win);
	if (!ip->directory)
		ip->directory = save_win_directory_create(ip, ip->pan);
	if (!ip->filename)
		ip->filename = save_win_filename_create(ip, ip->pan);
	if (!ip->option)
		ip->option = save_win_option_create(ip, ip->pan);
	if (!ip->save)
		ip->save = save_win_save_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
save_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	void		save_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 374,
		XV_HEIGHT, 251,
		XV_LABEL, "Save",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_DONE_PROC, save_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
save_win_pan_create(ip, owner)
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
save_win_directory_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	save_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 24,
		XV_WIDTH, 315,
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
		PANEL_NOTIFY_PROC, save_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `filename' in the specified instance.

 */
Xv_opaque
save_win_filename_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	save_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 60,
		XV_WIDTH, 315,
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
		PANEL_NOTIFY_PROC, save_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `option' in the specified instance.

 */
Xv_opaque
save_win_option_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	save_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TOGGLE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 20,
		XV_Y, 92,
		XV_WIDTH, 326,
		XV_HEIGHT, 110,
		PANEL_VALUE_X, 79,
		PANEL_VALUE_Y, 92,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NCOLS, 2,
		PANEL_LABEL_STRING, "Option:",
		PANEL_NOTIFY_PROC, save_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Current Settings",
			"Configuration",
			"Trajectories",
			"Fixed Points",
			"Continuation Data",
			"Parameter Data",
			"Selected Points",
			"Function Values",
			0,
		PANEL_VALUE, 1,
		NULL);
	return obj;
}

/*
 * Create object `save' in the specified instance.

 */
Xv_opaque
save_win_save_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		save_button_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 164,
		XV_Y, 216,
		XV_WIDTH, 46,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Save",
		PANEL_NOTIFY_PROC, save_button_handler,
		NULL);
	return obj;
}

