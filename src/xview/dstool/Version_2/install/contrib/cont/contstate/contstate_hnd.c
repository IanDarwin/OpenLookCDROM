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
#include "contstate_cui.h"
#include "contstate.h"

void	cntstate_ctype_notify();

extern cntstate_objects			*Cntstate_cntstatepu;
extern struct Cntstate_Ds                cntstate_ds;

/*
 * Notify callback function for `sel'.
 */
void
cntstate_sel_notify_callback(item, value, event)
	Panel_item	item;
	unsigned int	value;
	Event		*event;
{
	cntstate_objects *ip = (cntstate_objects*) xv_get(item, XV_KEY_DATA, INSTANCE);
	int   n_func = *((int *) pm(GET, "Model.Funct_Dim", NULL));
	short	i;

	cntstate_ds.State_On = FALSE;
	cntstate_ds.Param_On = FALSE;
	cntstate_ds.Func_On  = FALSE;
	cntstate_ds.Char_On  = FALSE;
	
	for (i = 0; i < 4; i++) {
		if (value & 01)
		  {
		   switch(i)
		     {
		      case 0:
			cntstate_ds.State_On = TRUE;
			break;
		      case 1:
			cntstate_ds.Param_On = TRUE;
			break;
		      case 2:
                        if(n_func > 0) cntstate_ds.Func_On = TRUE;
			break;
		      case 3:
			cntstate_ds.Char_On = TRUE;
			break;
                     }
                  }
		
		if (i == 3 && (value & 01))
		{
			cntstate_ctype_notify(item, value, event);
		}
		value >>= 1;
	}
        cntstate_field_manager();
}

/*
 * User-defined action for `sel'.
 */
void
cntstate_ctype_notify(item, value, event)
	Panel_item	item;
	unsigned int	value;
	Event		*event;
{
}

/*
 * Notify callback function for `update'.
 */
void
cntstate_update_notify(item, event)
	Panel_item	item;
	Event		*event;
{
    double        *dvector(), *state, *parameters;

    int   n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL)) - 1;
    int   n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
	  
    cntstate_objects *ip = (cntstate_objects *) xv_get(item, XV_KEY_DATA, INSTANCE);

    state = dvector(0,n_varb);
    parameters = dvector(0,n_param);
		    
    pm( GET_LIST, "Cont.Fc", 0, n_varb-1, state, NULL);
    pm( GET_LIST, "Cont.Param_Fc", 0, n_param-1, parameters, NULL);

    pm( PUT_LIST, "Selected.Varb_Ic", 0, n_varb-1, state, NULL);
    pm( PUT_LIST, "Selected.Param_Ic", 0, n_param-1, parameters, NULL);
    sel_data_refresh();

    free_dvector(state, 0, n_varb);
    free_dvector(parameters, 0, n_param);
}

/* callback for FRAME_DONE */
void
    contstate_done_proc(frame)
Frame
    frame;
{
    contstate_close();
}	
