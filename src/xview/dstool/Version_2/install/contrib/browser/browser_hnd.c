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
 * browser_hnd.c - Notify and event callback functions.
 */

#include <xview/xview.h>
#include <xview/panel.h>
#include "pm.h"

/*
 * Event callback function for `pan'.
 */
Notify_value
browser_event_notify(win, event, arg, type)
	Xv_window	win;
	Event		*event;
	Notify_arg	arg;
	Notify_event_type type;
{

  if (event_action(event) == LOC_WINENTER) 
    {
      browser_read_window();
      browser_data_refresh();
    }
  else if (event_action(event) == LOC_WINEXIT) 
    {
      browser_read_window();
      browser_data_refresh();
      browser_highlight_erase();
    }

  return notify_next_event_func(win, (Notify_event) event, arg, type);
}


/*
 * Notify callback function for settings.
 */
void
browser_settings_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  browser_read_window();
  browser_data_refresh();
}

/*
 * Notify callback function for `highlight'.
 */
void
browser_highlight_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  browser_read_window();
  browser_data_refresh();

  if (*((int *) pm(GET, "Browser.Highlight", NULL)))
    browser_highlight_draw();
  else browser_highlight_erase();
}

/*
 * Notify callback function for text fields.
 */
Panel_setting
browser_text_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  browser_read_window();
  browser_data_refresh();
  return panel_text_notify(item, event);
}

/*
 * Notify callback function for `copy'.
 */
void
browser_copy_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  browser_read_window();
  browser_data_refresh();
  selected_read_window();
  pm(EXEC, "Browser.Copy", NULL);
  sel_data_refresh();
}

/*
 * Notify callback function for `output'.
 */
void
browser_output_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  browser_read_window();
  browser_data_refresh();
  pm(EXEC, "Browser.Output", NULL);
}


/*
 * Notify callback function for `output'.
 */
void
browser_delete_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  browser_read_window();
  pm(EXEC, "Browser.Delete", NULL);
  browser_data_refresh();
  cmd_data_refresh();
}

/* callback for FRAME_DONE */
void
    browser_done_proc(frame)
Frame
    frame;
{
    browser_close();
}	





