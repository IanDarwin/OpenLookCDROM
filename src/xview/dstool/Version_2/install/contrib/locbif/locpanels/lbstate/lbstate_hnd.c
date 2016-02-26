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
#include <constants.h>
#include <pm.h>
#include "lbstate_cui.h"
#include "lbstate.h"

void	ctype_notify();

extern lcstate_locstatepu_objects	*Lcstate_locstatepu;
extern struct Lbstate_Ds                lbstate_ds;

/*
 * Notify callback function for `lcsel'.
 */
void
lcstate_locstatepu_lcsel_notify_callback(item, value, event)
	Panel_item	item;
	unsigned int	value;
	Event		*event;
{
	lcstate_locstatepu_objects *ip = (lcstate_locstatepu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	short	i;

	lbstate_ds.State_On = FALSE;
	lbstate_ds.Param_On = FALSE;
	lbstate_ds.Func_On  = FALSE;
	lbstate_ds.Char_On  = FALSE;
	
	for (i = 0; i < 4; i++) {
		if (value & 01)
		  {
		   switch(i)
		     {
		      case 0:
			lbstate_ds.State_On = TRUE;
			break;
		      case 1:
			lbstate_ds.Param_On = TRUE;
			break;
		      case 2:
			lbstate_ds.Func_On = TRUE;
			break;
		      case 3:
			lbstate_ds.Char_On = TRUE;
			break;
                     }
                  }
		
		if (i == 3 && (value & 01))
		{
			ctype_notify(item, value, event);
		}
		value >>= 1;
	}
        lbstate_field_manager();
}

/*
 * User-defined action for `lcsel'.
 */
void
ctype_notify(item, value, event)
	Panel_item	item;
	unsigned int	value;
	Event		*event;
{
}

/*
 * Notify callback function for `lcupdate'.
 */
void
lcupdate_notify(item, event)
	Panel_item	item;
	Event		*event;
{
	lcstate_locstatepu_objects *ip = (lcstate_locstatepu_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);
	update_state_locbif();
}

/* callback for FRAME_DONE */
void
    lbstate_done_proc(frame)
Frame
    frame;
{
    lbstate_close();
}	
