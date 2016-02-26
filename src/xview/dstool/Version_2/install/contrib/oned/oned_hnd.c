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
#include "oned_ui.h"

/* 
 * oned_settings_notify()
 *
 * notify callback function for settings type panel item.
 */
void
  oned_settings_notify(item, value, event)
Panel_item	item;
int value;
Event *event;
{
  oned_read_window();
  oned_data_refresh();
}


/*
 * oned_text_notify()
 *
 * notify callback function for text field type panel item.
 */
Panel_setting
  oned_text_notify(item, event)
Panel_item item;
Event *event;
{
  oned_read_window();
  oned_data_refresh();
  return panel_text_notify(item, event);
}

/*
 * oned_sketch_notify()
 *
 * notify callback function for `sketch'.
 */
void
  oned_sketch_notify(item, event)
Panel_item item;
Event *event;
{
  selected_read_window();
  sel_data_refresh();
  oned_read_window();
  pm(EXEC, "OneD.Sketch", NULL);
  oned_data_refresh();

}

/*
 * oned_diagonal_notify()
 *
 * notify callback function for `diagonal'.
 */
void
  oned_diagonal_notify(item, event)
Panel_item item;
Event *event;
{
  oned_read_window();
  pm(EXEC, "OneD.Diagonal", NULL);
  oned_data_refresh();

}

/*
 * oned_clear_notify()
 *
 * notify callback function for `clear'.
 */
void
  oned_clear_notify(item, event)
Panel_item item;
Event *event;
{
  oned_read_window();
  pm(EXEC, "OneD.Clear", NULL);
  oned_data_refresh();

}


/*
 * oned_forward_notify()
 *
 * notify callback function for `forward'.
 */
void
  oned_forward_notify(item, event)
Panel_item item;
Event *event;
{
  selected_read_window();
  oned_read_window();
  pm(EXEC, "OneD.Forward", NULL);
  oned_data_refresh();
  sel_data_refresh();

}

/*
 * oned_backward_notify()
 *
 * notify callback function for `backward'.
 */
void
  oned_backward_notify(item, event)
Panel_item item;
Event *event;
{
  selected_read_window();
  oned_read_window();
  pm(EXEC, "OneD.Backward", NULL);
  oned_data_refresh();
  sel_data_refresh();

}

/*
 * oned_continue_notify()
 *
 * notify callback function for `continue'.
 */
void
  oned_continue_notify(item, event)
Panel_item item;
Event *event;
{
  selected_read_window();
  oned_read_window();
  pm(EXEC, "OneD.Continue", NULL);
  oned_data_refresh();
  sel_data_refresh();

}

/* callback for FRAME_DONE */
void
    oned_done_proc(frame)
Frame
    frame;
{
    oned_close();
}	
