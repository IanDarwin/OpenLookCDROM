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
#include "twoD_opt_cui.h"
#include "twoD_opt.h"
#include <constants.h>
#include <symbols.h>

/*
 * Initialize an instance of object `win'.
 */
twoD_opt_win_objects *
twoD_opt_win_objects_initialize(ip, owner)
	twoD_opt_win_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (twoD_opt_win_objects *) calloc(1, sizeof (twoD_opt_win_objects))))
		return (twoD_opt_win_objects *) NULL;
	if (!ip->win)
		ip->win = twoD_opt_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = twoD_opt_win_pan_create(ip, ip->win);
	if (!ip->sym)
		ip->sym = twoD_opt_win_sym_create(ip, ip->pan);
	if (!ip->addpt)
		ip->addpt = twoD_opt_win_addpt_create(ip, ip->pan);
	if (!ip->size)
		ip->size = twoD_opt_win_size_create(ip, ip->pan);
	if (!ip->bg)
		ip->bg = twoD_opt_win_bg_create(ip, ip->pan);
	if (!ip->cmap_dir)
		ip->cmap_dir = twoD_opt_win_cmap_dir_create(ip, ip->pan);
	if (!ip->cmap_file)
		ip->cmap_file = twoD_opt_win_cmap_file_create(ip, ip->pan);
	if (!ip->showcol)
		ip->showcol = twoD_opt_win_showcol_create(ip, ip->pan);
	if (!ip->cmap_use)
		ip->cmap_use = twoD_opt_win_cmap_use_create(ip, ip->pan);
	if (!ip->dcoord_min)
		ip->dcoord_min = twoD_opt_win_dcoord_min_create(ip, ip->pan);
	if (!ip->dcoord_max)
		ip->dcoord_max = twoD_opt_win_dcoord_max_create(ip, ip->pan);
	return ip;
}

/*
 * Create object `win' in the specified instance.

 */
Xv_opaque
twoD_opt_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 289,
		XV_HEIGHT, 310,
		XV_LABEL, "2-D View Options",
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
twoD_opt_win_pan_create(ip, owner)
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
   Create object `sym' in the specified instance.

 */

Xv_opaque
twoD_opt_win_sym_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		sym_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 100,
		XV_Y, 8,
		XV_WIDTH, 99,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 163,
		PANEL_VALUE_Y, 8,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Symbol:",
		PANEL_NOTIFY_PROC, sym_notify,
		PANEL_CHOICE_STRINGS,
			Panel_Sym_Names[0],
			Panel_Sym_Names[1],
			Panel_Sym_Names[2],
			Panel_Sym_Names[3],
			0,
		NULL);
	return obj;
}

/*
 * Create object `addpt' in the specified instance.

 */
Xv_opaque
twoD_opt_win_addpt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		addpt_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 12,
		XV_WIDTH, 78,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Add Point",
		PANEL_NOTIFY_PROC, addpt_notify,
		NULL);
	return obj;
}

/*
 * Create object `size' in the specified instance.

 */
Xv_opaque
twoD_opt_win_size_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		size_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 100,
		XV_Y, 32,
		XV_WIDTH, 76,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 140,
		PANEL_VALUE_Y, 32,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Size:",
		PANEL_NOTIFY_PROC, size_notify,
		PANEL_CHOICE_STRINGS,
			Panel_Sym_Sizes[0],
			Panel_Sym_Sizes[1],
			Panel_Sym_Sizes[2],
			Panel_Sym_Sizes[3],
			0,
		NULL);
	return obj;
}

/*
 * Create object `bg' in the specified instance.

 */
Xv_opaque
twoD_opt_win_bg_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		bg_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 64,
		XV_WIDTH, 128,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 96,
		PANEL_VALUE_Y, 64,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Background:",
		PANEL_NOTIFY_PROC, bg_notify,
		PANEL_CHOICE_STRINGS,
			"White",
			"Black",
			"Light Blue",
			0,
		NULL);
	return obj;
}

/*
 * Create object `cmap_dir' in the specified instance.

 */
Xv_opaque
twoD_opt_win_cmap_dir_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	cmap_dir_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 106,
		XV_WIDTH, 278,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Colortable Directory:",
		PANEL_VALUE_X, 154,
		PANEL_VALUE_Y, 106,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 16,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, cmap_dir_notify,
		NULL);
	return obj;
}

/*
 * Create object `cmap_file' in the specified instance.

 */
Xv_opaque
twoD_opt_win_cmap_file_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	cmap_file_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 126,
		XV_WIDTH, 280,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Colortable File:",
		PANEL_VALUE_X, 116,
		PANEL_VALUE_Y, 126,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 21,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, cmap_file_notify,
		NULL);
	return obj;
}

/*
 * Create object `showcol' in the specified instance.

 */
Xv_opaque
twoD_opt_win_showcol_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		showcol_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 146,
		XV_WIDTH, 134,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 122,
		PANEL_VALUE_Y, 146,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Show Colormap:",
		PANEL_NOTIFY_PROC, showcol_notify,
		PANEL_CHOICE_STRINGS,
			"Yes",
			"No",
			0,
		NULL);
	return obj;
}

/*
 * Create object `cmap_use' in the specified instance.

 */
Xv_opaque
twoD_opt_win_cmap_use_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern int		cmap_use_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 174,
		XV_WIDTH, 152,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 120,
		PANEL_VALUE_Y, 174,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "Colormap Type:",
		PANEL_NOTIFY_PROC, cmap_use_notify,
		PANEL_CHOICE_STRINGS,
			"Alternating",
			"Pick Colors",
			"Depth",
			0,
		NULL);
	return obj;
}

/*
 * Create object `depthcoord' in the specified instance.

 */
Xv_opaque
twoD_opt_win_depthcoord_create(ip, owner, n)
	caddr_t		ip;
	Xv_opaque	owner;
	int		n;
{
	extern int		depthcoord_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 198,
		XV_WIDTH, 132,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 100,
		PANEL_VALUE_Y, 198,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, n,
		PANEL_LABEL_STRING, "Depth Coord:",
		PANEL_NOTIFY_PROC, depthcoord_notify,
		PANEL_CHOICE_STRINGS,
			" ",
			0,
		NULL);
	return obj;
}

/*
 * Create object `dcoord_min' in the specified instance.

 */
Xv_opaque
twoD_opt_win_dcoord_min_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	dcoord_min_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 4,
		XV_Y, 228,
		XV_WIDTH, 207,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Coding Range   Min:",
		PANEL_VALUE_X, 147,
		PANEL_VALUE_Y, 228,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, dcoord_min_notify,
		NULL);
	return obj;
}

/*
 * Create object `dcoord_max' in the specified instance.

 */
Xv_opaque
twoD_opt_win_dcoord_max_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	dcoord_max_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 109,
		XV_Y, 254,
		XV_WIDTH, 103,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Max:",
		PANEL_VALUE_X, 148,
		PANEL_VALUE_Y, 254,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 8,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, dcoord_max_notify,
		NULL);
	return obj;
}

