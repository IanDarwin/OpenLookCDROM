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
 * load_handlers.c - Notify and event callback functions.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>
#include <saveload.h>
#include <constants.h>
#include <pm.h>
#include "load_ui.h"



/*
 * Notify callback function for `load'.
 * Last modified: 10 August 1992  fjw
 */
void
load_button_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  load_read_window();
  load_data_refresh();
  load_driver(0);		/* 0 <==> display errors on load window */
  def_data_refresh();				  /* refresh all windows */
  sel_data_refresh();
  cmd_data_refresh();
  periodic_data_refresh();
  orbit_data_refresh();
  mult_data_refresh();
  refresh_all_win();

  return;
}


/*
 * Notify callback function for text field type panel item.
 */
Panel_setting
load_text_notify(item, event)
	Panel_item	item;
	Event		*event;
{
        load_read_window();
	complete_filename("Load"); 
	load_data_refresh();
	if (event_action(event) == '\t' )
	  return( PANEL_NONE );	/* don't advance cursor if TAB */
	else
	  return panel_text_notify(item, event);
}

/*
 * Notify callback function for setting type panel item.
 */
void
load_setting_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
        load_read_window();
	load_data_refresh();
}

/*
 * Notify callback function for list type panel items
 */
int
load_list_notify(item, string, client_data, op, event)
        Panel_item      item;
        char            *string;
        Xv_opaque       client_data;
        Panel_list_op   op;
        Event           *event;
{
        load_read_window();
	load_data_refresh();
}

/* callback for FRAME_DONE */
void
    load_done_proc(frame)
Frame
    frame;
{
    load_close();
}	
