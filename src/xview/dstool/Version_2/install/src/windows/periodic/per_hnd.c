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
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>

#include "periodic_ui.h"
#include <memory.h>
#include <pm.h>


/*
 * Notify callback function for generic panel text field
 * in the periodic window
 */
Panel_setting
periodic_generic_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	periodic_read_window();
	periodic_data_refresh();
	return panel_text_notify(item, event);
}

/*
 * Notify callback function for `algorithm'.
 */
int
periodic_setting_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  periodic_read_window();
  periodic_data_refresh();
}


/*
 * Notify callback function for `find'.
 */
void
periodic_find_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  int	found = *((int *) pm(GET, "Fixed.Found", NULL));

  selected_read_window();
  sel_data_refresh();
  periodic_read_window();
  periodic_data_refresh();
  pm(EXEC, "Fixed.Find_Fixpts", NULL);
  if (found < *((int *) pm(GET, "Fixed.Found", NULL)))
    mem_all_win( (memory) pm(GET, "Memory.Fixed", NULL));
  periodic_data_refresh();
  cmd_data_refresh();
}


/*
 * Notify callback function for `clear'.
 */
void
periodic_clear_notify(item, event)
	Panel_item	item;
	Event		*event;
{
        periodic_read_window();
	/* clear fixed point data only from fixed point memory object */
	pm(EXEC, "Fixed.Clear_Points", NULL);
	periodic_data_refresh();
	cmd_data_refresh();
	refresh_all_win();
}

/*
 * Notify callback function for `clear_mans'.
 */
void
periodic_clear_mans_notify(item, event)
	Panel_item	item;
	Event		*event;
{
        periodic_read_window();
	periodic_data_refresh();
	pm(EXEC, "Fixed.Clear_Mans", NULL);
	cmd_data_refresh();
	refresh_all_win();
}


/*
 * Notify callback function for `reset'.
 */
void
periodic_reset_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  periodic_read_window();
  fixed_reset();
  periodic_data_refresh();
  cmd_data_refresh();
  refresh_all_win();
}


/*
 * Notify callback function for `oneDman'.
 */
void
periodic_oneDman_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  periodic_read_window();
  periodic_data_refresh();
  orbit_read_window();
  orbit_data_refresh();
/*  prop_read_window();
  prop_data_refresh(); */
  pm(EXEC, "Fixed.1dman", NULL);
  cmd_data_refresh();
	
}


/*
 * Notify callback function for `twoDman'.
 */
void
periodic_twoDman_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  periodic_read_window();
  periodic_data_refresh();
  orbit_read_window();
  orbit_data_refresh();
/*  prop_read_window();
  prop_data_refresh(); */
  system_mess_proc(0,"Computation of 2-d manifolds not yet implemented.");
  cmd_data_refresh();
}

/* callback for FRAME_DONE */
void
    periodic_done_proc(frame)
Frame
    frame;
{
    periodic_close();
}	
