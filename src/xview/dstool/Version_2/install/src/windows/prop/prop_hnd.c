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
#include <malloc.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>

#include <ui_init.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>
#include <prop.h>

/*
 * Notify callback function for text items.
 */
Panel_setting
prop_text_notify(item, event)
	Panel_item      item;
	Event           *event;
{
  prop_read_window();
  prop_data_refresh();
  return panel_text_notify(item, event);
}




/*
 * Notify callback function for `prop_select'.
 */
int
prop_select_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  extern void	rebuild_rw_fields();

  if( *((int *) pm( GET, "Model.Mapping_Flag", NULL )) ) 
     {
       prop_read_window();
       prop_data_refresh();
     }
  else
     {
       pm(PUT, "Flow.Int_Num", value,
	  EXEC, "Flow.Load_Int",
	  NULL);
       rebuild_rw_fields();
     }
      
}

/*
 * Notify callback function for `prop_usr_select'.
 */
int
prop_usr_select_notify(item, value, event)
	Panel_item	item;
	int		value;
	Event		*event;
{
  prop_read_window();
  prop_data_refresh();
}

/* callback for FRAME_DONE */
void
    prop_done_proc(frame)
Frame
    frame;
{
    prop_close();
}	

/*
 * Used by batch (e.g. load) command to open prop window.
 */
Menu_item
    prop_batch_handler(item, op)
Menu_item	
    item;
Menu_generate	
    op;
{
    int
	locn[4];
    if (op == MENU_NOTIFY)
	if (item != (Menu_item) NULL)
	    prop_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	else {
	    pm(GET_LIST,"Win.Locn.Prop",0,3,locn,NULL);
	    if ((locn[0] != NO_LOCATION))
		prop_open(SET_WIN_CONFIG,locn[0],locn[1],locn[2],locn[3]);
	    else
		prop_open(DEFAULT_WIN_CONFIG,0,0,0,0);
	}
  return item;
}



