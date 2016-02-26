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
 * cmd_ui.c - User interface object initialization functions.
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

#include "cmd_ui.h"
#include <constants.h>
#include <defaults.h>
#include <version.h>
#include "sys_panels.h"
#include "user_panels.h"

/*
 * Create object `filemenu' in the specified instance.

 */
Xv_opaque
cmd_filemenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
			XV_KEY_DATA, INSTANCE, ip,
			NULL);
	add_panels(obj, NUM_FILE_PANELS, FILE_PANELS);
	return obj;
}

/*
 * Create object `viewmenu' in the specified instance.

 */
Xv_opaque
cmd_viewmenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
			XV_KEY_DATA, INSTANCE, ip,
			NULL);
	add_panels(obj, NUM_VIEW_PANELS, VIEW_PANELS);
	return obj;
}

/*
 * Create object `panelmenu' in the specified instance.

 */
Xv_opaque
cmd_panelmenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
			XV_KEY_DATA, INSTANCE, ip,
			NULL);
	add_panels(obj, NUM_PANELS, PANELS);
	add_panels(obj, NUM_USER_PANELS, USER_PANELS);
	return obj;
}

/*
 * Create object `settingsmenu' in the specified instance.

 */
Xv_opaque
cmd_settingsmenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
			XV_KEY_DATA, INSTANCE, ip,
			NULL);
	add_panels(obj, NUM_SET_PANELS, SET_PANELS);
	return obj;
}

/*
 * Create object `dscatmenu' in the specified instance.

 */
Xv_opaque
cmd_dscatmenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
			XV_KEY_DATA, INSTANCE, ip,
			NULL);
	return obj;
}

/*
 * Create object `modelmenu' in the specified instance.

 */
Xv_opaque
cmd_modelmenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	extern Menu_item	models_handler();
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Item",
			MENU_GEN_PROC, models_handler,
			NULL,
			NULL);
	return obj;
}


/*
 * Initialize an instance of object `win'.
 */
cmd_win_objects *
cmd_win_objects_initialize(ip, owner)
	cmd_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (cmd_win_objects *) calloc(1, sizeof (cmd_win_objects))))
		return (cmd_win_objects *) NULL;
	if (!ip->win)
		ip->win = cmd_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = cmd_win_pan_create(ip, ip->win);
	if (!ip->modelbutton)
		ip->modelbutton = cmd_win_modelbutton_create(ip, ip->pan);
	if (!ip->modelname)
		ip->modelname = cmd_win_modelname_create(ip, ip->pan);
	if (!ip->file)
		ip->file = cmd_win_file_create(ip, ip->pan);
	if (!ip->view)
		ip->view = cmd_win_view_create(ip, ip->pan);
	if (!ip->panels)
		ip->panels = cmd_win_panels_create(ip, ip->pan);
	if (!ip->settings)
		ip->settings = cmd_win_settings_create(ip, ip->pan);
	if (!ip->stored_points)
		ip->stored_points = cmd_win_stored_points_create(ip, ip->pan);
	if (!ip->messages)
		ip->messages = cmd_win_messages_create(ip, ip->win);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
cmd_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	Xv_opaque		win_image;
	static unsigned short	win_bits[] = {
#include "dstool.icon"
	};
	
	win_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_BITS, win_bits,
		SERVER_IMAGE_DEPTH, 1,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		NULL);
	obj = xv_create(owner, FRAME,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 425,
		XV_HEIGHT, 50,
		XV_LABEL, DSTOOL_TITLE,
		FRAME_CLOSED, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, TRUE,
		FRAME_ICON, xv_create(XV_NULL, ICON,
			ICON_IMAGE, win_image,
			NULL),
		NULL);
	return obj;
}

/*
 * Create object `pan' in the specified instance.

 */
Xv_opaque
cmd_win_pan_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, 52,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `modelbutton' in the specified instance.

 */
Xv_opaque
cmd_win_modelbutton_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_ABBREV_MENU_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 12,
		XV_Y, 8,
		XV_WIDTH, 73,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Model:",
		PANEL_ITEM_MENU, cmd_dscatmenu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `modelname' in the specified instance.

 */
Xv_opaque
cmd_win_modelname_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 92,
		XV_Y, 8,
		XV_WIDTH, 1,
		XV_HEIGHT, 13,
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `file' in the specified instance.

 */
Xv_opaque
cmd_win_file_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 28,
		XV_WIDTH, 54,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "File",
		PANEL_ITEM_MENU, cmd_filemenu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `view' in the specified instance.

 */
Xv_opaque
cmd_win_view_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 84,
		XV_Y, 28,
		XV_WIDTH, 62,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "View",
		PANEL_ITEM_MENU, cmd_viewmenu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `panels' in the specified instance.

 */
Xv_opaque
cmd_win_panels_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 160,
		XV_Y, 28,
		XV_WIDTH, 72,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Panels",
		PANEL_ITEM_MENU, cmd_panelmenu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `settings' in the specified instance.

 */
Xv_opaque
cmd_win_settings_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 244,
		XV_Y, 28,
		XV_WIDTH, 81,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Settings",
		PANEL_ITEM_MENU, cmd_settingsmenu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `stored_points' in the specified instance.

 */
Xv_opaque
cmd_win_stored_points_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 336,
		XV_Y, 32,
		XV_WIDTH, 0,
		XV_HEIGHT, 0,
		PANEL_LABEL_BOLD, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `messages' in the specified instance.

 */
Xv_opaque
cmd_win_messages_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, TEXTSW,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 53,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		OPENWIN_SHOW_BORDERS, TRUE,
		NULL);
	return obj;
}

