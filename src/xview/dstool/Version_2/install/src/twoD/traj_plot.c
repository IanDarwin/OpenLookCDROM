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
#include <math.h>

#include <complib.h>
#include <constants.h>
#include <ui_init.h>
#include <xview/cms.h>
#include "twoD.h"
#include "twoD_opt.h"
#include <filters.h>

/* ----------------------------------------------------------------------
   proc traj_plot displays the generated trajectory points to the 2D canvases 

   ---------------------------------------------------------------------- */

void
  traj_plot(prop_cntl, start_ptr, stop_ptr, func_values)
struct  Prop_DataS     *prop_cntl;
int	start_ptr, stop_ptr;
double	*func_values;
{
  extern int get_ds_func();
  int	i, window_number;
  int     get_min_twoD(), get_max_twoD(), valid_twoD_id(), get_color();
  double  x,y;
  struct  Filter_DataS     filter_cntl;
  
  filter_cntl.parameters = prop_cntl->parameters;
  filter_cntl.function = prop_cntl->aux_function;
  filter_cntl.alt_color_index = prop_cntl->table_color;
  filter_cntl.pick_color_index = prop_cntl->sys_color;
  filter_cntl.func = func_values;
  
  for(window_number=get_min_twoD(); window_number<=get_max_twoD(); 
      window_number++)
    {
      if(valid_twoD_id(window_number) && 
	 valid_mem_to_win( TRAJ_MEMORY, window_number ))
	{
	  filter_cntl.window_id = window_number;
	  for(i=start_ptr;i<=stop_ptr;i++)
	    {
	      filter_cntl.state = prop_cntl->traj_segment[i];
	      switch ( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->
		      Active_Hor_Type) 
		{
		case PHASE_SPACE_VARB:
		  x = prop_cntl->traj_segment[i]
		    [(int)TwoD_Ds[window_number]->TwoD_Win_Ds->
		     Active_Hor_Index];
		  break;
		case PARAMETER_VARB:
		  x = prop_cntl->parameters[(int)TwoD_Ds[window_number]->
					    TwoD_Win_Ds->Active_Hor_Index];
		  break;
		case FUNCTION_VARB:
		  filter_cntl.function(func_values, prop_cntl->traj_segment[i], 
				       prop_cntl->parameters);
		  x = func_values[TwoD_Ds[window_number]->TwoD_Win_Ds->
				  Active_Hor_Index];
		  break;
		}
	      switch ( (int) TwoD_Ds[window_number]->TwoD_Win_Ds->
		      Active_Ver_Type) 
		{
		case PHASE_SPACE_VARB:
		  y = prop_cntl->traj_segment[i]
		    [(int)TwoD_Ds[window_number]->TwoD_Win_Ds->
		     Active_Ver_Index];
		  break;
		case PARAMETER_VARB:
		  y = prop_cntl->parameters[(int) TwoD_Ds[window_number]->
					    TwoD_Win_Ds->Active_Ver_Index];
		  break; 
		case FUNCTION_VARB:
		  get_ds_func(func_values, prop_cntl->traj_segment[i], 
			      prop_cntl->parameters);
		  y = func_values[TwoD_Ds[window_number]->TwoD_Win_Ds->
				  Active_Ver_Index];
		  break;
		}
	      if(prop_cntl->table_color < 0) /* add check for range-fjw */
		plot_symbol(window_number, x, y, -prop_cntl->sys_color , 
			    prop_cntl->symbol); 
	      else
		plot_symbol(window_number, x, y, 
			    (int) get_color(&filter_cntl), 
			    prop_cntl->symbol);
	    }
	}
    }
}
