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
 * selected_handlers.c - User interface object initialization functions.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/icon_load.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/svrimage.h>
#include <xview/termsw.h>
#include <xview/text.h>
#include <xview/tty.h>
#include <xview/xv_xrect.h>
#include <gcm.h>

#include <pm.h>


/*
 * Notify callback function for text items.
 */
Panel_setting
selected_text_notify(item, event)
	Panel_item      item;
	Event           *event;
{
  void selected_read_window(), sel_data_refresh();

  selected_read_window();
  sel_data_refresh();
  return panel_text_notify(item, event);
}




/*
 * Notify callback function for `copy'.
 */
void
copy_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  void sel_data_refresh();

  pm(EXEC, "Selected.Copy", NULL);
  sel_data_refresh();
}


/* callback for FRAME_DONE */
void
    sel_done_proc(frame)
Frame
    frame;
{
    sel_close();
}	
