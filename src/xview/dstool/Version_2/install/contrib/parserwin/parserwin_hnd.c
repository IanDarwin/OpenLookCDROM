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
#include <xview/xview.h>
#include <xview/panel.h>
#include "parserwin_ui.h"

/*
 * parserwin_text_notify
 *
 * callback function for text fields on the parser window
 *
 */
Panel_setting
  parserwin_text_notify(item,event)
Panel_item item;
Event *event;
{
  parserwin_read_window();
  parserwin_data_refresh();
  return panel_text_notify(item,event);
}


/*
 * Notify callback function for settings on parserwin.
 */
void
parserwin_settings_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  parserwin_read_window();
  parserwin_data_refresh();
}


/*
 * Notify callback function for `build'.
 */
void
parserwin_build_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  parserwin_read_window();
  parserwin_data_refresh();
  parserwin_build_go();
}

/*
 * Notify callback function for `writec'.
 */
void
parserwin_writec_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  parserwin_read_window();
  parserwin_data_refresh();
  parserwin_writec_go();
}

