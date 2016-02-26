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
#include <gdd.h>

#include <constants.h>
#include <pm.h>



/*
 * Notify callback function for `forwards'.
 */
void
for_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  orbit_read_window();
  orbit_data_refresh();
  selected_read_window();
  prop_read_window();
  pm(EXEC, "Flow.Forwards", NULL);
  cmd_data_refresh();
  sel_data_refresh();
  prop_data_refresh();
}

/*
 * Notify callback function for `backwards'.
 */
void
back_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  orbit_read_window();
  orbit_data_refresh();
  selected_read_window();
  prop_read_window();
  pm(EXEC, "Flow.Backwards", NULL);
  cmd_data_refresh();
  sel_data_refresh();
  prop_data_refresh();
}

/*
 * Notify callback function for `contin'.
 */
void
cont_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  orbit_read_window();
  orbit_data_refresh();
  selected_read_window();
  sel_data_refresh();
  prop_read_window();
  pm(EXEC, "Flow.Continue", NULL);
  cmd_data_refresh();
  sel_data_refresh();
  prop_data_refresh();
}

/*
 * Notify callback function for `clearlast'.
 */
void
clearlast_handler(item, event)
	Panel_item	item;
	Event		*event;
{

  orbit_read_window();
  orbit_data_refresh();
  pm(EXEC, "Flow.Clear_Last", NULL);
  cmd_data_refresh();
}

/*
 * Notify callback function for `clearall'.
 */
void
clearall_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  orbit_read_window();
  orbit_data_refresh();
  pm(EXEC, "Flow.Clear_All", NULL);
  cmd_data_refresh();
  refresh_all_win();
  reset_color();
}

/*
 * Notify callback function for `interrupt'.
 */
void
interrupt_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  set_interrupt();
}


/*
 * Notify callback function for text fields.
 */
Panel_setting
orbit_text_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  orbit_read_window();
  orbit_data_refresh();

  return panel_text_notify(item, event);
}


/*
 * Notify callback function for settings
 */
int
orbit_setting_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  orbit_read_window();
  orbit_data_refresh();
}



/*
 * Notify callback function for `propagation'.
 */
void
prop_handler(item, event)
	Panel_item	item;
	Event		*event;
{	
  prop_open(DEFAULT_WIN_CONFIG,0,0,0,0);
}

/* callback for FRAME_DONE */
void
    orbit_done_proc(frame)
Frame
    frame;
{
    orbit_close();
}	
