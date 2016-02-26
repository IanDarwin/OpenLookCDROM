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
 * print_cui.c - User interface object initialization functions.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <gcm.h>
#include "print_ui.h"

/*
 * Initialize an instance of object `win'.
 */
print_win_objects *
print_win_objects_initialize(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (print_win_objects *) calloc(1, sizeof (print_win_objects))))
		return (print_win_objects *) NULL;
	if (!ip->win)
		ip->win = print_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = print_win_pan_create(ip, ip->win);
	if (!ip->who)
		ip->who = print_win_who_create(ip, ip->pan);
	if (!ip->owner)
		ip->owner = print_win_owner_create(ip, ip->pan);
	if (!ip->title)
		ip->title = print_win_title_create(ip, ip->pan);
	if (!ip->hor_label)
		ip->hor_label = print_win_hor_label_create(ip, ip->pan);
	if (!ip->ver_label)
		ip->ver_label = print_win_ver_label_create(ip, ip->pan);
	if (!ip->destination)
		ip->destination = print_win_destination_create(ip, ip->pan);
	if (!ip->color)
		ip->color = print_win_color_create(ip, ip->pan);
	if (!ip->dir)
		ip->dir = print_win_dir_create(ip, ip->pan);
	if (!ip->name)
		ip->name = print_win_name_create(ip, ip->pan);
	if (!ip->settings)
		ip->settings = print_win_settings_create(ip, ip->pan);
	if (!ip->print_button)
		ip->print_button = print_win_print_button_create(ip, ip->pan);
	if (!ip->reset_button)
		ip->reset_button = print_win_reset_button_create(ip, ip->pan);
	if (!ip->sys_info)
		ip->sys_info = print_win_sys_info_create(ip, ip->pan);
	if (!ip->font)
		ip->font = print_win_font_create(ip, ip->pan);
	if (!ip->typeface)
		ip->typeface = print_win_typeface_create(ip, ip->pan);
	if (!ip->titlesize)
		ip->titlesize = print_win_titlesize_create(ip, ip->pan);
	if (!ip->labelsize)
		ip->labelsize = print_win_labelsize_create(ip, ip->pan);
	if (!ip->landscape)
		ip->landscape = print_win_landscape_create(ip, ip->pan);
	if (!ip->suggestion)
	  	ip->suggestion = print_win_suggestion_create(ip, ip->pan);
	if (!ip->bound_box)
		ip->bound_box = print_win_bound_box_create(ip, ip->pan);
	if (!ip->num_ticks_x)
		ip->num_ticks_x = print_win_num_ticks_x_create(ip, ip->pan);
	if (!ip->num_ticks_y)
		ip->num_ticks_y = print_win_num_ticks_y_create(ip, ip->pan);
	if (!ip->hor_len)
		ip->hor_len = print_win_hor_len_create(ip, ip->pan);
	if (!ip->ver_len)
		ip->ver_len = print_win_ver_len_create(ip, ip->pan);
	if (!ip->hor_offset)
		ip->hor_offset = print_win_hor_offset_create(ip, ip->pan);
	if (!ip->ver_offset)
		ip->ver_offset = print_win_ver_offset_create(ip, ip->pan);
	if (!ip->connect)
		ip->connect = print_win_connect_create(ip, ip->pan);
        if (!ip->labelrange)
                ip->labelrange = print_win_labelrange_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.
 */
Xv_opaque
print_win_win_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 454,
		XV_HEIGHT, 567,
		XV_LABEL, "Print",
		FRAME_SHOW_FOOTER, TRUE,
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
print_win_pan_create(ip, owner)
	print_win_objects	*ip;
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
	gcm_initialize_colors(obj, NULL, NULL);
	return obj;
}

/*
 * Create object `who' in the specified instance.
 */
Xv_opaque
print_win_who_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 8,
		PANEL_LABEL_STRING, "Owner =",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `owner' in the specified instance.
 */
Xv_opaque
print_win_owner_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	Xv_opaque		owner_image;
	static unsigned short	owner_bits[] = {
#include "owner.icon"
	};
	
	owner_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_DEPTH, 1,
		SERVER_IMAGE_BITS, owner_bits,
		XV_WIDTH, 16,
		XV_HEIGHT, 16,
		NULL);
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 80,
		XV_Y, 8,
		PANEL_ITEM_COLOR, gcm_color_index("Light Blue"),
		PANEL_LABEL_IMAGE, owner_image,
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `title' in the specified instance.
 */
Xv_opaque
print_win_title_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 32,
		PANEL_VALUE_DISPLAY_LENGTH, 35,
		PANEL_VALUE_STORED_LENGTH, 120,
		PANEL_LABEL_STRING, "Title:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `hor_label' in the specified instance.
 */
Xv_opaque
print_win_hor_label_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 56,
		PANEL_VALUE_DISPLAY_LENGTH, 25,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Hor Label:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `ver_label' in the specified instance.
 */
Xv_opaque
print_win_ver_label_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 80,
		PANEL_VALUE_DISPLAY_LENGTH, 25,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Ver Label:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `destination' in the specified instance.
 */
Xv_opaque
print_win_destination_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	dest_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 104,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Destination:",
		PANEL_NOTIFY_PROC, dest_handler,
		PANEL_CHOICE_STRINGS,
			"Printer",
			"File",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `color' in the specified instance.
 */
Xv_opaque
print_win_color_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 248,
		XV_Y, 104,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Color PostScript:",
		PANEL_NOTIFY_PROC, print_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Yes",
			"No",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `dir' in the specified instance.
 */
Xv_opaque
print_win_dir_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 136,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Directory:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `name' in the specified instance.
 */
Xv_opaque
print_win_name_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 164,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Filename:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `settings' in the specified instance.
 */
Xv_opaque
print_win_settings_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 192,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Settings:",
		PANEL_NOTIFY_PROC, print_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Hide",
			"Show",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `print_button' in the specified instance.
 */
Xv_opaque
print_win_print_button_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void		print_butt_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 158,
		XV_Y, 232,
		PANEL_LABEL_STRING, "Print",
		PANEL_NOTIFY_PROC, print_butt_handler,
		NULL);
	return obj;
}

/*
 * Create object `reset_button' in the specified instance.
 */
Xv_opaque
print_win_reset_button_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void		print_reset_handler();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 248,
		XV_Y, 232,
		PANEL_NOTIFY_PROC, print_reset_handler,
		PANEL_LABEL_STRING, "Reset",
		NULL);
	return obj;
}

/*
 * Create object `sys_info' in the specified instance.
 */
Xv_opaque
print_win_sys_info_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 280,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Show System Information:",
		PANEL_NOTIFY_PROC, print_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Yes",
			"No",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `font' in the specified instance.
 */
Xv_opaque
print_win_font_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 320,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Font:",
		PANEL_NOTIFY_PROC, print_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Times",
			"Helvetica",
			"Courier",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `typeface' in the specified instance.
 */
Xv_opaque
print_win_typeface_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TOGGLE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 264,
		XV_Y, 320,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_STRINGS,
			"Oblique",
			"Bold",
			NULL,
		PANEL_VALUE, 0,
		PANEL_NOTIFY_PROC, print_setting_notify,
		NULL);
	return obj;
}

/*
 * Create object `titlesize' in the specified instance.
 */
Xv_opaque
print_win_titlesize_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 352,
		PANEL_VALUE_DISPLAY_LENGTH, 2,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Title Size (Pts):",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 30,
		PANEL_MIN_VALUE, 4,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `labelsize' in the specified instance.
 */
Xv_opaque
print_win_labelsize_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 248,
		XV_Y, 352,
		PANEL_VALUE_DISPLAY_LENGTH, 2,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Label Size (Pts):",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 30,
		PANEL_MIN_VALUE, 4,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `landscape' in the specified instance.
 */
Xv_opaque
print_win_landscape_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 376,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Landscape Mode:",
		PANEL_NOTIFY_PROC, print_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Yes",
			"No",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `suggestion' in the specified instance.
 */
Xv_opaque
print_win_suggestion_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 240,
		XV_Y, 382,
		PANEL_LABEL_STRING, "To Center: Offset = ( xxx, xxx )",
		PANEL_LABEL_BOLD, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `bound_box' in the specified instance.
 */
Xv_opaque
print_win_bound_box_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 408,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Show Bounding Box:",
		PANEL_NOTIFY_PROC, print_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Yes",
			"No",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `num_ticks_x' in the specified instance.
 */
Xv_opaque
print_win_num_ticks_x_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 40,
		XV_Y, 440,
		PANEL_VALUE_DISPLAY_LENGTH, 2,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "# Hor Tick Marks:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 20,
		PANEL_MIN_VALUE, 0,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `num_ticks_y' in the specified instance.
 */
Xv_opaque
print_win_num_ticks_y_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 248,
		XV_Y, 440,
		PANEL_VALUE_DISPLAY_LENGTH, 2,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "# Ver Tick Marks:",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 20,
		PANEL_MIN_VALUE, 0,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `hor_len' in the specified instance.
 */
Xv_opaque
print_win_hor_len_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 40,
		XV_Y, 464,
		PANEL_VALUE_DISPLAY_LENGTH, 3,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Hor Length (Pts):",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 1000,
		PANEL_MIN_VALUE, 10,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `ver_len' in the specified instance.
 */
Xv_opaque
print_win_ver_len_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 248,
		XV_Y, 464,
		PANEL_VALUE_DISPLAY_LENGTH, 3,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Ver Height (Pts):",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 1000,
		PANEL_MIN_VALUE, 10,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `hor_offset' in the specified instance.
 */
Xv_opaque
print_win_hor_offset_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 40,
		XV_Y, 488,
		PANEL_VALUE_DISPLAY_LENGTH, 3,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Hor Offset (Pts):",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 1000,
		PANEL_MIN_VALUE, 0,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `ver_offset' in the specified instance.
 */
Xv_opaque
print_win_ver_offset_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_text_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_NUMERIC_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 248,
		XV_Y, 488,
		PANEL_VALUE_DISPLAY_LENGTH, 3,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_LABEL_STRING, "Ver Offset (Pts):",
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_MAX_VALUE, 1000,
		PANEL_MIN_VALUE, 0,
		PANEL_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, print_text_notify,
		NULL);
	return obj;
}

/*
 * Create object `connect' in the specified instance.
 */
Xv_opaque
print_win_connect_create(ip, owner)
	print_win_objects	*ip;
	Xv_opaque	owner;
{
	extern void	print_setting_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 16,
		XV_Y, 520,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOOSE_NONE, FALSE,
		PANEL_LABEL_STRING, "Connect the Dots:",
		PANEL_NOTIFY_PROC, print_setting_notify,
		PANEL_CHOICE_STRINGS,
			"Yes",
			"No",
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `labelrange' in the specified instance.
 */
Xv_opaque
print_win_labelrange_create(ip, owner)
        print_win_objects       *ip;
        Xv_opaque       owner;
{
	extern void	print_setting_notify();
        Xv_opaque       obj;

        obj = xv_create(owner, PANEL_CHOICE,
                XV_KEY_DATA, INSTANCE, ip,
                XV_X, 248,
                XV_Y, 520,
                PANEL_CHOICE_NROWS, 1,
                PANEL_LAYOUT, PANEL_HORIZONTAL,
                PANEL_CHOOSE_NONE, FALSE,
                PANEL_LABEL_STRING, "Mark Axes Range:",
		PANEL_NOTIFY_PROC, print_setting_notify,
                PANEL_CHOICE_STRINGS,
                        "Yes",
                        "No",
                        NULL,
                NULL);
        return obj;
}

