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





/*
 * Notify callback function for text fields 
 */
Panel_setting
default_text_notify(item, event)
	Panel_item	item;
	Event		*event;
{
  def_read_window();
  def_data_refresh();
  return panel_text_notify(item, event);
}

int
default_setting_notify(item,value,event)
     Panel_item item;
     int value;
     Event event;
{
  def_read_window();
  def_data_refresh();
}


/* callback for FRAME_DONE */
void
    defaults_done_proc(frame)
Frame
    frame;
{
    defaults_close();
}	
