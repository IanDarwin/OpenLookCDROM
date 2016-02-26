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
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <gcm.h>
#include <prop.h>
#include "prop_cui.h"

/*
 * Initialize an instance of object `win'.
 */
prop_win_objects *
prop_win_objects_initialize(ip, owner)
	prop_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (prop_win_objects *) calloc(1, sizeof (prop_win_objects))))
		return (prop_win_objects *) NULL;
	if (!ip->win)
		ip->win = prop_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = prop_win_pan_create(ip, ip->win);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
prop_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	void		prop_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 396,
		XV_HEIGHT, 130,
		XV_LABEL, "Propagation",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, prop_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
prop_win_pan_create(ip, owner)
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
 * Create object `ifields' in the specified instance.
 */
Xv_opaque
prop_win_ifields_create(ip, owner, field_number, offset)
	caddr_t		ip;
	Xv_opaque	owner;
	int		field_number, offset;
{
	extern Panel_setting	prop_text_notify();
	Xv_opaque	obj;

	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 50,
		XV_Y, 70,
		XV_WIDTH, 100,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "  ",
		PANEL_VALUE_X, 100,
		PANEL_VALUE_Y, offset + 20 * field_number,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, prop_text_notify,
		PANEL_CLIENT_DATA, field_number,
		NULL);
	return obj;
}

/*
 * Create object `dfields' in the specified instance.
 */
Xv_opaque
prop_win_dfields_create(ip, owner, field_number, offset)
	caddr_t		ip;
	Xv_opaque	owner;
	int		field_number, offset;
{
	Xv_opaque	obj;
	extern Panel_setting	prop_text_notify();
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 250,
		XV_Y, 73,
		XV_WIDTH, 100,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "  ",
		PANEL_VALUE_X, 300,
		PANEL_VALUE_Y, offset + 20 * field_number,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 10,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY,FALSE, 
		PANEL_NOTIFY_PROC, prop_text_notify,
		PANEL_CLIENT_DATA, field_number,
		NULL);
	return obj;
}


/*
 * Create object `prop_select' in the specified instance.

 */
Xv_opaque
prop_select_create(ip, owner,n)
	caddr_t		ip;
	Xv_opaque	owner;
	int		n;
{
	Xv_opaque	obj;
	extern int	prop_select_notify();
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 100,
		XV_Y, 25,
		XV_WIDTH, 190,
		XV_HEIGHT, 25,
		PANEL_VALUE_X, 170,
		PANEL_VALUE_Y, 25,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, n,
		PANEL_LABEL_STRING, "              ",
		PANEL_NOTIFY_PROC, prop_select_notify,
		PANEL_CHOICE_STRINGS,
			"   ",
			"   ",
			0,
		NULL);
	return obj;
}


/*
 * Create object `prop_usr_select' in the specified instance.

 */
Xv_opaque
prop_usr_select_create(ip, owner, field_number)
	caddr_t		ip;
	Xv_opaque	owner;
	int		field_number;
{
	Xv_opaque	obj;
	extern int	prop_usr_select_notify();
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 100,
		XV_Y, 55,
		XV_WIDTH, 190,
		XV_HEIGHT, 25,
		PANEL_VALUE_X, 170,
		PANEL_VALUE_Y, 55 + 30 * field_number,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, " ",
		PANEL_NOTIFY_PROC, prop_usr_select_notify,
		PANEL_CLIENT_DATA, field_number,
		PANEL_CHOICE_STRINGS,
			"   ",
			"   ",
			0,
		NULL);
	return obj;
}

