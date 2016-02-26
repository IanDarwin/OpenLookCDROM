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
#include <pm.h>
#include "geomview_cui.h"


/*
 * geomview_settings_notify()
 *
 * notify callback function for settings type panel item.
 */
void
  geomview_settings_notify(item, value, event)
Panel_item	item;
int value;
Event *event;
{
  geomview_read_window();
  geomview_data_refresh();
}

/*
 * geomview_text_notify()
 *
 * notify callback function for text field type panel item.
 */
Panel_setting
  geomview_text_notify(item, event)
Panel_item item;
Event *event;
{
  geomview_read_window();
  geomview_data_refresh();
  return panel_text_notify(item, event);
}

/*
 * geomview_button1_notify()
 *
 * notify callback function for `button1'.
 */
void
  view_button_notify(item, event)
Panel_item item;
Event *event;
{
  geomview_read_window();
  pm(EXEC,"Geomview.View", NULL);
  geomview_data_refresh();
}

