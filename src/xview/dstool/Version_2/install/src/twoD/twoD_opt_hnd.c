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
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <xview/canvas.h>
#include <xview/cms.h>
#include <gdd.h>

#include "twoD_opt_cui.h"
#include <ui_init.h>
#include <constants.h>
#include "twoD.h"
#include "twoD_opt.h"

/*
 * Notify callback function for `sym'.
 */
int
sym_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
}

/*
 * Notify callback function for `addpt'.
 */
void
addpt_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	
  add_sel_pt( (int) get_twoD_number(ip) ); 
  cmd_data_refresh();
}

/*
 * Notify callback function for `size'.
 */
int
size_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
}


/*
 * Notify callback function for `bg'.
 */
int
bg_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );

  bg_color_refresh( get_twoD_number(ip), value ); 
}

/*
 * Notify callback function for `cmap_dir'.
 */
Panel_setting
cmap_dir_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
	
  return panel_text_notify(item, event);
}


/*
 * Notify callback function for `cmap_file'.
 */
Panel_setting
cmap_file_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
  colortable_refresh(get_twoD_number(ip));

  return panel_text_notify(item, event);
}

/*
 * Notify callback function for `showcol'.
 */
int
showcol_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
}


/*
 * Notify callback function for `cmap_use'.
 */
int
cmap_use_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
}


/*
 * Notify callback function for `depthcoord'.
 */
int
depthcoord_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

  set_depthcoord( get_twoD_number(ip), value );
  opt_data_refresh( get_twoD_number(ip) );
}


/*
 * Notify callback function for `dcoord_min'.
 */
Panel_setting
dcoord_min_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
	
  return panel_text_notify(item, event);
}


/*
 * Notify callback function for `dcoord_max'.
 */
Panel_setting
dcoord_max_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  int	get_twoD_number();

  twoD_opt_win_objects	*ip = (twoD_opt_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  opt_data_refresh( get_twoD_number(ip) );
	
  return panel_text_notify(item, event);
}
