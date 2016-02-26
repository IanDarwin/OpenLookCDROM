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
 * orbit_cui.c - User interface object initialization functions.
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
#include "orbit_cui.h"

/*
 * Initialize an instance of object `win'.
 */
orbit_win_objects *
orbit_win_objects_initialize(ip, owner)
	orbit_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (orbit_win_objects *) calloc(1, sizeof (orbit_win_objects))))
		return (orbit_win_objects *) NULL;
	if (!ip->win)
		ip->win = orbit_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = orbit_win_pan_create(ip, ip->win);
	if (!ip->forwards)
		ip->forwards = orbit_win_forwards_create(ip, ip->pan);
	if (!ip->backwards)
		ip->backwards = orbit_win_backwards_create(ip, ip->pan);
	if (!ip->contin)
		ip->contin = orbit_win_contin_create(ip, ip->pan);
	if (!ip->clearlast)
		ip->clearlast = orbit_win_clearlast_create(ip, ip->pan);
	if (!ip->clearall)
		ip->clearall = orbit_win_clearall_create(ip, ip->pan);
	if (!ip->interrupt)
		ip->interrupt = orbit_win_interrupt_create(ip, ip->pan);
	if (!ip->start)
		ip->start = orbit_win_start_create(ip, ip->pan);
	if (!ip->stop)
		ip->stop = orbit_win_stop_create(ip, ip->pan);
	if (!ip->skip)
		ip->skip = orbit_win_skip_create(ip, ip->pan);
	if (!ip->stepsize)
		ip->stepsize = orbit_win_stepsize_create(ip, ip->pan);
	if (!ip->propagation)
		ip->propagation = orbit_win_propagation_create(ip, ip->pan); 
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
orbit_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	void		orbit_done_proc();
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 298,
		XV_HEIGHT, 349,
		XV_LABEL, "Orbits",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_DONE_PROC, orbit_done_proc,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
orbit_win_pan_create(ip, owner)
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
 * Create object `forwards' in the specified instance.

 */
Xv_opaque
orbit_win_forwards_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		for_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 12,
		XV_Y, 12,
		XV_WIDTH, 74,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Forwards",
		PANEL_NOTIFY_PROC, for_notify,
		NULL);
	return obj;
}

/*
 * Create object `backwards' in the specified instance.

 */
Xv_opaque
orbit_win_backwards_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		back_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 92,
		XV_Y, 12,
		XV_WIDTH, 82,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Backwards",
		PANEL_NOTIFY_PROC, back_notify,
		NULL);
	return obj;
}

/*
 * Create object `contin' in the specified instance.

 */
Xv_opaque
orbit_win_contin_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		cont_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 180,
		XV_Y, 12,
		XV_WIDTH, 74,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Continue",
		PANEL_NOTIFY_PROC, cont_notify,
		NULL);
	return obj;
}

/*
 * Create object `clearlast' in the specified instance.

 */
Xv_opaque
orbit_win_clearlast_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		clearlast_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 12,
		XV_Y, 44,
		XV_WIDTH, 78,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Clear Last",
		PANEL_NOTIFY_PROC, clearlast_handler,
		NULL);
	return obj;
}

/*
 * Create object `clearall' in the specified instance.

 */
Xv_opaque
orbit_win_clearall_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		clearall_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 110,
		XV_Y, 44,
		XV_WIDTH, 70,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Clear All",
		PANEL_NOTIFY_PROC, clearall_handler,
		NULL);
	return obj;
}


/*
 * Create object `interrupt' in the specified instance.
 */
Xv_opaque
orbit_win_interrupt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		interrupt_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 220,
		XV_Y, 44,
		XV_WIDTH, 70,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Interrupt",
		PANEL_NOTIFY_PROC, interrupt_handler,
		NULL);
	return obj;
}


/*
 * Create object `start' in the specified instance.

 */
Xv_opaque
orbit_win_start_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	orbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 80,
		XV_WIDTH, 108,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Start:",
		PANEL_VALUE_X, 52,
		PANEL_VALUE_Y, 80,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, orbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `stop' in the specified instance.

 */
Xv_opaque
orbit_win_stop_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	orbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 136,
		XV_Y, 80,
		XV_WIDTH, 106,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Stop:",
		PANEL_VALUE_X, 178,
		PANEL_VALUE_Y, 80,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, orbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `skip' in the specified instance.

 */
Xv_opaque
orbit_win_skip_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	orbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 104,
		XV_WIDTH, 106,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Plot:", /* also in orbit_data_refresh */
		PANEL_VALUE_X, 50,           /* also in orbit_data_refresh */
		PANEL_VALUE_Y, 104,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, orbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `stepsize' in the specified instance.

 */
Xv_opaque
orbit_win_stepsize_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	orbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 136,
		XV_Y, 104,
		XV_WIDTH, 138,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Step Size:",
		PANEL_VALUE_X, 210,	/* also in orbit_data_refresh */
		PANEL_VALUE_Y, 104,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, orbit_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `stop_cond' in the specified instance.

 */
Xv_opaque
orbit_win_stop_cond_create(ip, owner, n)
	caddr_t		ip;
	Xv_opaque	owner;
	int		n;
{
	extern int		orbit_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 8,
		XV_Y, 128,
		XV_WIDTH, 179,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 151,
		PANEL_VALUE_Y, 128,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, n,
		PANEL_LABEL_STRING, "Stopping Condition:",
		PANEL_NOTIFY_PROC, orbit_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Fixed Steps",
			"Event Stopping",
			0,
		NULL);
	return obj;
}


/*
 * Create object `propagation' in the specified instance.

 */
Xv_opaque
orbit_win_propagation_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		prop_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 12,
		XV_Y, 160,
		XV_WIDTH, 94,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Propagation...",
		PANEL_NOTIFY_PROC, prop_handler,
		NULL);
	return obj;
}

/*
 * Create object `stop_event' in the specified instance.

 */
Xv_opaque
orbit_win_stop_event_create(ip, owner, i)
	caddr_t		ip;
	Xv_opaque	owner;
int i;
{
	extern Panel_setting	orbit_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 44,
		XV_Y, 192,
		XV_WIDTH, 104,
		XV_HEIGHT, 15,
		PANEL_VALUE_X, 84,
		PANEL_VALUE_Y, 192 + 20*i,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 12,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, orbit_text_notify,
		NULL);
	return obj;
}

