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
#include "trial_cui.h"

/*
 * trial_settings_notify()
 *
 * notify callback function for settings type panel item.
 */
void
  trial_settings_notify(item, value, event)
Panel_item	item;
int value;
Event *event;
{
  trial_read_window();
  trial_data_refresh();
}

/*
 * trial_text_notify()
 *
 * notify callback function for text field type panel item.
 */
Panel_setting
  trial_text_notify(item, event)
Panel_item item;
Event *event;
{
  trial_read_window();
  trial_data_refresh();
  return panel_text_notify(item, event);
}

/*
 * trial_button1_notify()
 *
 * notify callback function for `button1'.
 */
void
  trial_button1_notify(item, event)
Panel_item item;
Event *event;
{
  trial_read_window();
  selected_read_window();
  trial_button1_go();
  trial_data_refresh();
  sel_data_refresh();
}

