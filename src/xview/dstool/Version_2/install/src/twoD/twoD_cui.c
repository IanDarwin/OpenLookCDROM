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
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/canvas.h>
#include <xview/icon_load.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/notice.h>
#include <gdd.h>
#include <gcm.h>
#include <xview/cms.h>

#include <ui_init.h>
#include "twoD.h"
#include "twoD_opt.h"
#include "twoD_cui.h"
#include <constants.h>
#include <plot.h>
#include <pm.h>


/*
 * Initialize an instance of object `win'.
 */
twoD_win_objects *
twoD_win_objects_initialize(ip, owner, cms)
	twoD_win_objects	*ip;
	Xv_opaque	owner;
	Cms		cms;
{
	if (!ip && !(ip = (twoD_win_objects *) calloc(1, sizeof (twoD_win_objects))))
		return (twoD_win_objects *) NULL;
	if (!ip->win)
		ip->win = twoD_win_win_create(ip, owner);
	if (!ip->pan)
		ip->pan = twoD_win_pan_create(ip, ip->win);
	if (!ip->hormin)
		ip->hormin = twoD_win_hormin_create(ip, ip->pan);
	if (!ip->hormax)
		ip->hormax = twoD_win_hormax_create(ip, ip->pan);
	if (!ip->options)
		ip->options = twoD_win_options_create(ip, ip->pan);
	if (!ip->vermin)
		ip->vermin = twoD_win_vermin_create(ip, ip->pan);
	if (!ip->vermax)
		ip->vermax = twoD_win_vermax_create(ip, ip->pan);
	if (!ip->canvas)
		ip->canvas = twoD_win_canvas_create(ip, ip->win, cms);
	if (!ip->controls1)
		ip->controls1 = twoD_win_controls1_create(ip, ip->win);
	if (!ip->cbar_lt)
		ip->cbar_lt = twoD_win_cbar_lt_create(ip, ip->controls1);
	if (!ip->twoD_cbar)
		ip->twoD_cbar = twoD_win_twoD_cbar_create(ip, ip->win, cms);
	if (!ip->controls2)
		ip->controls2 = twoD_win_controls2_create(ip, ip->win);
	if (!ip->cbar_rt)
		ip->cbar_rt = twoD_win_cbar_rt_create(ip, ip->controls2);
	if (!ip->cursorpos)
		ip->cursorpos = twoD_win_cursorpos_create(ip, ip->pan);
	return ip;
}

/* 
  create menu for panels 
  
  */
Xv_opaque
panel_menu_create(ip, owner)
caddr_t		*ip;
Xv_opaque	owner;
{
	Xv_opaque obj;

	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
		XV_KEY_DATA, INSTANCE, ip,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "File",
			MENU_PULLRIGHT, cmd_filemenu_create(ip, NULL),
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "View",
			MENU_PULLRIGHT, cmd_viewmenu_create(ip, NULL),
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Panels",
			MENU_PULLRIGHT, cmd_panelmenu_create(ip, NULL),
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Settings",
			MENU_PULLRIGHT, cmd_settingsmenu_create(ip, NULL),
			NULL,
		NULL);
	return(obj);
}


/*
 * Create object `optionsmenu' in the specified instance.
 */
Xv_opaque
twoD_optionsmenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	extern Menu_item	win_refresh_handler();
	extern Menu_item	twoD_opt_handler();
 	extern Menu_item	print_handler();
	extern Menu_item        panel_menu_quit();
	Xv_opaque	obj;
	
	obj = xv_create(XV_NULL, MENU_COMMAND_MENU,
		XV_KEY_DATA, INSTANCE, ip,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Refresh",
			MENU_GEN_PROC, win_refresh_handler,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Display...",
			MENU_GEN_PROC, twoD_opt_handler,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Print...",
			MENU_GEN_PROC, print_handler,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Size",
			MENU_PULLRIGHT, twoD_sizemenu_create(ip, NULL),
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING,"Close",
			MENU_GEN_PROC, panel_menu_quit,
			NULL,
		NULL);
	return obj;
}

/*
 * Create object `sizemenu' in the specified instance.

 */
Xv_opaque
twoD_sizemenu_create(ip, owner)
	caddr_t		*ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	extern Menu_item	use_def_size_handler(), set_def_size_handler();
	
	obj = xv_create(XV_NULL, MENU_CHOICE_MENU,
		XV_KEY_DATA, INSTANCE, ip,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Use Default",
			MENU_GEN_PROC, use_def_size_handler,
			NULL,
		MENU_ITEM,
			XV_KEY_DATA, INSTANCE, ip,
			MENU_STRING, "Set Default",
			MENU_GEN_PROC, set_def_size_handler,
			NULL,
		NULL);
	return obj;
}



/*
 * Create object `win' in the specified instance.
 */
Xv_opaque
twoD_win_win_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	Xv_opaque		win_image;
	static unsigned short	win_bits[] = {
#include "twoD.icon"
	};
	void	twoD_done_proc();
	
	win_image = xv_create(XV_NULL, SERVER_IMAGE,
		SERVER_IMAGE_BITS, win_bits,
		SERVER_IMAGE_DEPTH, 1,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		NULL);
	obj = xv_create(owner, FRAME,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 524, 
		XV_HEIGHT, 471,
		XV_LABEL, "dstool - modelname",
		FRAME_DONE_PROC, twoD_done_proc,
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
twoD_win_pan_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	extern void panel_background_handler();
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, 64,
		WIN_BORDER, FALSE,
		NULL);

	xv_set(obj, WIN_MENU, panel_menu_create((caddr_t *) ip, NULL), NULL);
	xv_set(obj, PANEL_BACKGROUND_PROC, panel_background_handler, NULL);

	return obj;
}

/*
 * Create object `hor' in the specified instance.
 */
Xv_opaque
twoD_win_hor_create(ip, owner,n)
	caddr_t		ip;
	Xv_opaque	owner;
	int		n;
{
	extern int              hor_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		/* XV_X, 26,
		XV_Y, 4, */
		XV_WIDTH, 72,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 40,
		PANEL_VALUE_Y, 1,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_NOTIFY_PROC, hor_notify,
		PANEL_CHOICE_NROWS, n,
		PANEL_LABEL_STRING, "Hor:",
		PANEL_CHOICE_STRINGS," ", 0,
		NULL);
	return obj;
}


/*
 * Create object `options' in the specified instance.
 */
Xv_opaque
twoD_win_options_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		/* XV_X, 428,
		XV_Y, 35, */
		XV_X, 2,
		XV_Y, 43,
		XV_WIDTH, 80,
		XV_HEIGHT, 19,
		PANEL_LABEL_STRING, "Options",
		PANEL_ITEM_MENU, twoD_optionsmenu_create((caddr_t *) ip, NULL),
		NULL);
	return obj;
}

/*
 * Create object `ver' in the specified instance.
 */
Xv_opaque
twoD_win_ver_create(ip, owner,n)
	caddr_t		ip;
	Xv_opaque	owner;
	int		n;
{
	extern int              ver_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		/* XV_X, 27,
		XV_Y, 30, */
		XV_WIDTH, 71,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 40,
		PANEL_VALUE_Y, 20,
		PANEL_VALUE,1,				/* selects second item as default */
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, n,
		PANEL_NOTIFY_PROC, ver_notify,
		PANEL_LABEL_STRING, "Ver:",
		PANEL_CHOICE_STRINGS," ", 0,
		NULL);
	return obj;
}



/*
 * Create object `hormin' in the specified instance.
 */
Xv_opaque
twoD_win_hormin_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	min_max_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		/* XV_X, 166,
		XV_Y,15, */
		XV_WIDTH, 102,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Min:",
		PANEL_VALUE_X, 184,
		PANEL_VALUE_Y,4,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, min_max_notify,
		NULL);
	return obj;
}

/*
 * Create object `hormax' in the specified instance.
 */
Xv_opaque
twoD_win_hormax_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	min_max_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		/* XV_X, 291,
		XV_Y,15, */
		XV_WIDTH, 103,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Max:",
		PANEL_VALUE_X, 330,
		PANEL_VALUE_Y,4,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, min_max_notify,
		NULL);
	return obj;
}


/*
 * Create object `vermin' in the specified instance.
 */
Xv_opaque
twoD_win_vermin_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	min_max_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		/* XV_X, 166,
		XV_Y, 43, */
		XV_WIDTH, 102,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Min:",
		PANEL_VALUE_X, 184,
		PANEL_VALUE_Y, 30,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, min_max_notify,
		NULL);
	return obj;
}

/*
 * Create object `vermax' in the specified instance.
 */
Xv_opaque
twoD_win_vermax_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern Panel_setting	min_max_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 291,
		XV_Y, 43,
		XV_WIDTH, 103,
		XV_HEIGHT, 15,
		PANEL_LABEL_STRING, "Max:",
		PANEL_VALUE_X, 330,
		PANEL_VALUE_Y, 30,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 13,
		PANEL_VALUE_STORED_LENGTH, 80,
		PANEL_MAX_VALUE, 0,
		PANEL_MIN_VALUE, 0,
		PANEL_READ_ONLY, FALSE,
		PANEL_NOTIFY_PROC, min_max_notify,
		NULL);
	return obj;
}




/*
 * Create object `canvas' in the specified instance.
 */
Xv_opaque
twoD_win_canvas_create(ip, owner, cms)
	caddr_t		ip;
	Xv_opaque	owner;
	Cms		cms;
{
	extern Notify_value	canvas_handler();
	extern void	twoD_canvas_repaint(), zoom_box();
	Xv_opaque	obj;


	obj = xv_create(owner, CANVAS,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 88,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		CANVAS_AUTO_EXPAND, TRUE,
		CANVAS_AUTO_SHRINK, TRUE,
		CANVAS_AUTO_CLEAR,TRUE,
		CANVAS_X_PAINT_WINDOW,TRUE,
		CANVAS_FIXED_IMAGE,FALSE,
/*		WIN_DYNAMIC_VISUAL,TRUE,*/
		WIN_DYNAMIC_VISUAL,FALSE,
		WIN_CMS, cms,
		CMS_TYPE, XV_DYNAMIC_CMS,
		CANVAS_REPAINT_PROC, twoD_canvas_repaint,
		NULL);
		/*xv_set(canvas_paint_window(obj), WIN_MENU, twoD_gomenu_create((caddr_t *) ip, NULL), NULL);*/
		xv_set(canvas_paint_window(obj), WIN_CONSUME_EVENTS,
		WIN_LEFT_KEYS,
		WIN_MOUSE_BUTTONS,
		LOC_WINENTER,
		LOC_WINEXIT,
		LOC_DRAG,
		LOC_MOVE,
		NULL, NULL);
	notify_interpose_event_func(canvas_paint_window(obj),
		(Notify_func) canvas_handler, NOTIFY_SAFE);
	xv_set(canvas_paint_window(obj), XV_KEY_DATA, INSTANCE, ip, NULL);
	xv_set(canvas_paint_window(obj), WIN_EVENT_PROC, zoom_box, WIN_CONSUME_EVENTS, WIN_MOUSE_BUTTONS,   /* mrm */
		       LOC_DRAG, NULL, NULL);
	return obj;
}

/*
 * Create object `controls1' in the specified instance.
 */
Xv_opaque
twoD_win_controls1_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 64,
		XV_WIDTH, 36,
		XV_HEIGHT, 22,
		WIN_BORDER, FALSE,
		NULL);
	gcm_initialize_colors(obj, NULL, NULL);
	return obj;
}

/*
 * Create object `cbar_lt' in the specified instance.
 */
Xv_opaque
twoD_win_cbar_lt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		cbar_lt_handler();
	extern void		cbar_lt_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 1,
		XV_Y, 2,
		XV_WIDTH, 34,
		XV_HEIGHT, 19,
		PANEL_ITEM_COLOR, gcm_color_index("Red"),
		PANEL_LABEL_STRING, "<-",
		PANEL_NOTIFY_PROC, cbar_lt_notify,
		PANEL_EVENT_PROC, cbar_lt_handler,
		NULL);
	return obj;
}

/*
 * Create object `twoD_cbar' in the specified instance.
 */
Xv_opaque
twoD_win_twoD_cbar_create(ip, owner, cms)
	caddr_t		ip;
	Xv_opaque	owner;
	Cms		cms;
{
	extern Notify_value	twoD_cbar_handler();
	extern void	twoD_cbar_repaint();
	Xv_opaque	obj;

	obj = xv_create(owner, CANVAS,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 74,
		XV_Y, 64,
		XV_WIDTH, WIN_EXTEND_TO_EDGE, 
		XV_HEIGHT, 22,
		CANVAS_AUTO_EXPAND, TRUE,
		CANVAS_AUTO_SHRINK, TRUE,
		CANVAS_AUTO_CLEAR,TRUE,
		WIN_COLLAPSE_EXPOSURES, FALSE,
		CANVAS_FIXED_IMAGE, FALSE,
		CANVAS_X_PAINT_WINDOW,TRUE,
/*		WIN_DYNAMIC_VISUAL,TRUE,*/
		WIN_DYNAMIC_VISUAL,FALSE,
		WIN_CMS, cms,
		CMS_TYPE, XV_DYNAMIC_CMS,
		CANVAS_REPAINT_PROC, twoD_cbar_repaint,
		NULL);
		xv_set(canvas_paint_window(obj), WIN_CONSUME_EVENTS,
		WIN_MOUSE_BUTTONS,
		LOC_WINENTER,
		NULL, NULL);
		notify_interpose_event_func(canvas_paint_window(obj),
		(Notify_func) twoD_cbar_handler, NOTIFY_SAFE);
		xv_set(canvas_paint_window(obj), XV_KEY_DATA, INSTANCE, ip, NULL);
	return obj;
}

/*
 * Create object `controls2' in the specified instance.
 */
Xv_opaque
twoD_win_controls2_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 37,
		XV_Y, 64,
		XV_WIDTH, 36,
		XV_HEIGHT, 22,
		WIN_BORDER, FALSE,
		NULL);
	gcm_initialize_colors(obj, NULL, NULL);
	return obj;
}

/*
 * Create object `cbar_rt' in the specified instance.
 */
Xv_opaque
twoD_win_cbar_rt_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		cbar_r_notify();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 1,
		XV_Y, 2,
		XV_WIDTH, 34,
		XV_HEIGHT, 19,
		PANEL_ITEM_COLOR, gcm_color_index("Red"),
		PANEL_LABEL_STRING, "->",
		PANEL_NOTIFY_PROC, cbar_r_notify,
		NULL);
	return obj;
}


