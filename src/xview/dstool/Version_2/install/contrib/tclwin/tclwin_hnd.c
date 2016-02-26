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
 * tclwin_handlers.c - Notify and event callback functions.
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>

#include <constants.h>
#include "tclwin_ui.h"





/*
 * Notify callback function for `tclwin'.
 */
void
tclwin_button_handler(item, event)
	Panel_item	item;
	Event		*event;
{
  tclwin_read_window();
  tclwin_data_refresh();
  tclwin_driver(0);		/* 0 <==> display errors on tclwin window */
  return;
}

int
tclwin_setting_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  tclwin_read_window();
  tclwin_data_refresh();
}


/*
 * Notify callback function for text field type panel item.
 */
Panel_setting
tclwin_text_notify(item, event)
	Panel_item	item;
	Event		*event;
{
        tclwin_read_window();
/*	complete_filename(Load_Cntl); */
	tclwin_data_refresh();
	if (event_action(event) == '\t' )
	  return( PANEL_NONE );	/* don't advance cursor if TAB */
	else
	  return panel_text_notify(item, event);
}

/* callback for FRAME_DONE */
void
    tcl_done_proc(frame)
Frame
    frame;
{
    tcl_close();
}	
