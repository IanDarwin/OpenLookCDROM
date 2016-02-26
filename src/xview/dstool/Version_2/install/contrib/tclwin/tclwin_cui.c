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
 * tclwin_cui.c - User interface object initialization functions.
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
#include "tclwin_ui.h"

/*
 * Initialize an instance of object `win'.
 */
tclwin_win_objects *
tclwin_win_objects_initialize(ip, owner)
	tclwin_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (tclwin_win_objects *) calloc(1, sizeof (tclwin_win_objects))))
		return (tclwin_win_objects *) NULL;
	if (!ip->win)
		ip->win = tclwin_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = tclwin_win_pan_create(ip, ip->win);
	if (!ip->directory)
		ip->directory = tclwin_win_directory_create(ip, ip->pan);
	if (!ip->filename)
		ip->filename = tclwin_win_filename_create(ip, ip->pan);
	if (!ip->option)
		ip->option = tclwin_win_option_create(ip, ip->pan);
	if (!ip->load)
		ip->load = tclwin_win_load_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
tclwin_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	void		tcl_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 375,
		XV_HEIGHT, 150,
		XV_LABEL, "Tcl file execution",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_DONE_PROC, tcl_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
tclwin_win_pan_create(ip, owner)
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
tclwin_win_directory_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	tclwin_text_notify();
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
		PANEL_NOTIFY_PROC, tclwin_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `filename' in the specified instance.

 */
Xv_opaque
tclwin_win_filename_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	tclwin_text_notify();
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
		PANEL_NOTIFY_PROC, tclwin_text_notify,
		NULL);
	return obj;
}


Xv_opaque
tclwin_win_option_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		tclwin_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 88,
		XV_Y, 100,
		XV_WIDTH, 88,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 88,
		PANEL_VALUE_Y, 100,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Verbose:",
		PANEL_NOTIFY_PROC, tclwin_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Off",
			"On",
			0,
		NULL);
	return obj;
}

/*
 * Create object `load' in the specified instance.

 */
Xv_opaque
tclwin_win_load_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		tclwin_button_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 176,
		XV_Y, 100,
		XV_WIDTH, 96,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Load Tcl file",
		PANEL_NOTIFY_PROC, tclwin_button_handler,
		NULL);
	return obj;
}

