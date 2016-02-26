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
 * trial_cui.c - User interface object initialization functions.
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
#include "trial_cui.h"

/*
 * Initialize an instance of object `win'.
 */
trial_win_objects *
trial_win_objects_initialize(ip, owner)
	trial_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (trial_win_objects *) calloc(1, sizeof (trial_win_objects))))
		return (trial_win_objects *) NULL;
	if (!ip->win)
		ip->win = trial_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = trial_win_pan_create(ip, ip->win);
	if (!ip->button1)
		ip->button1 = trial_win_button1_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
trial_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 268,
		XV_HEIGHT, 115,
		XV_LABEL, "Trial Panel",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
trial_win_pan_create(ip, owner)
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
 * Create object `setting1' in the specified instance.

 */
Xv_opaque
trial_win_setting1_create(ip, owner, n)
	caddr_t		ip;
	Xv_opaque	owner;
        int             n;
{
	extern int		trial_settings_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 16,
		XV_WIDTH, 147,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 86,
		PANEL_VALUE_Y, 16,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, n,
		PANEL_LABEL_STRING, "Variable:",
		PANEL_NOTIFY_PROC, trial_settings_notify,
		PANEL_CHOICE_STRING, 0, "Choice",
		NULL);
	return obj;
}

/*
 * Create object `button1' in the specified instance.

 */
Xv_opaque
trial_win_button1_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		trial_button1_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 184,
		XV_Y, 16,
		XV_WIDTH, 49,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Copy",
		PANEL_NOTIFY_PROC, trial_button1_notify,
		NULL);
	return obj;
}

/*
 * Create object `varb_value' in the specified instance.

 */
Xv_opaque
trial_win_varb_value_create(ip, owner, i)
	caddr_t		ip;
	Xv_opaque	owner;
        int             i;
{
	extern Panel_setting	trial_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 48,
		XV_Y, 64,
		XV_WIDTH, 151,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Varb Name:",
		PANEL_VALUE_X, 135,
		PANEL_VALUE_Y, 64 + 20*i,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, trial_text_notify,
		NULL);
	return obj;
}

