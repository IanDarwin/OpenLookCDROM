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
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>

#include <gcm.h>
#include <gdd.h>
#include <constants.h>
#include <pm.h>
#include "twoD.h"
#include "twoD_opt.h"
#include <filters.h>
#include <math.h>

#define	ALT_COLOR	0
#define	PICK_COLOR	1
#define	DEPTH		2

/*
  Return the index into the colormap (e.g. TwoD_Ds[window_number]->TwoD_Win_Ds->ColorTable)
  which is then an index into the CMS.
  Assumes that the following fields of filter_cntl are filled:
  ALT_COLOR case:
  	alt_color_index
  PICK_COLOR case:
	pick_color_index
  DEPTH case:
	window_id
	state 	
	parameters
*/
int
  get_color( filter_cntl )
struct  Filter_DataS     *filter_cntl;
{
  double	test;
  
  switch (TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->Cmap_Type_Index) 
    {
    case ALT_COLOR:
      return( (int) filter_cntl->alt_color_index) ; 
    case PICK_COLOR:
      return( (int) filter_cntl->pick_color_index );
    case DEPTH:
      switch ( (int) TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->Active_Depth_Type) 
	{
	case PHASE_SPACE_VARB:
	  if(filter_cntl->state == NULL) return(0);
	  test = (double) filter_cntl->state[(int) TwoD_Opt_Ds[filter_cntl->window_id]->
					     TwoD_Opt_Win_Ds->Depth_Coord_Index];
	  break;
	case PARAMETER_VARB:
	  test = (double) filter_cntl->parameters[(int) TwoD_Opt_Ds[filter_cntl->window_id]->
						  TwoD_Opt_Win_Ds->Depth_Coord_Index];
	  break;
	case FUNCTION_VARB:
	  if(filter_cntl->state == NULL) return(0);
	  filter_cntl->function(filter_cntl->func, filter_cntl->state, filter_cntl->parameters);
	  test = filter_cntl->func[TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->
			     Depth_Coord_Index];
	  break;
	}
      if(test<=TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->Depth_Coord_Min)
	return(0);
      else if(test>=TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->Depth_Coord_Max)
/*	return( (int) TwoD_Ds[filter_cntl->window_id]->TwoD_Win_Ds->Total_Traj_Colors - 1 ); */
	return( (int) TwoD_Ds[filter_cntl->window_id]->TwoD_Win_Ds->Total_Traj_Colors); /* ab 12/7/93 */
      else
	test = (double) (TwoD_Ds[filter_cntl->window_id]->TwoD_Win_Ds->Total_Traj_Colors - 1) * 
	  ( (test - TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->Depth_Coord_Min)/
	   (TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->Depth_Coord_Max-
	    TwoD_Opt_Ds[filter_cntl->window_id]->TwoD_Opt_Win_Ds->Depth_Coord_Min));
      return( (int) floor(test));
      break;
    }
  return(0);
}
