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
 * save_handlers.c - Notify and event callback functions.
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/notice.h>
#include <gdd.h>
#include <saveload.h>
#include <constants.h>
#include "save_ui.h"

/*
 * Notify callback function for `save'.
 * Last modified:  29 January 1991  fjw
 */
void
save_button_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  save_win_objects	*ip = (save_win_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
  if( (int) xv_get(ip->option, PANEL_VALUE) == 0)
    {
      error_notice(ip->pan,"Please choose a valid save option");
      return;
    }

  save_read_window();
  save_data_refresh();
  orbit_read_window();
  periodic_read_window();
  def_read_window();
  selected_read_window();
  switch( save_go(FALSE) )
    {
    case 1:
      if (!error_notice_option(ip->pan, "Specified files exists.", "Cancel Save", "Overwrite File") )
	save_go(TRUE);
      break;
    case -1:
      error_notice(ip->pan, "Saving not successful.  Cannot open file");
      break;
    }  
}

/*
 * Notify callback function for text field type panel item.
 */
Panel_setting
save_text_notify(item, event)
	Panel_item	item;
	Event		*event;
{
        save_read_window();
	complete_filename("Save");
	save_data_refresh();
	if (event_action(event) == '\t' )
	  return( PANEL_NONE );	/* don't advance cursor if TAB */
	else
	  return panel_text_notify(item, event);
}

/*
 * Notify callback function for setting type panel item.
 */
void
save_setting_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
        save_read_window();
	save_data_refresh();
}

/* callback for FRAME_DONE */
void
    save_done_proc(frame)
Frame
    frame;
{
    save_close();
}	
