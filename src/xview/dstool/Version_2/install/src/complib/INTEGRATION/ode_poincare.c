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
#include <constants.h>
#include <defaults.h>
#include <complib.h>

int
ode_poincare(integ_cntl)
struct  Prop_DataS     *integ_cntl;
{
  int 		ode_stop(),
   		status=NO_ERROR, 
		iter_request_save,
		start_to_save_save,
  		prop_segsize_save,
		plot_switch,
  		count,					/* max number of integrator steps */
  		start = 0,				/* point to start plotting */
		skip_start,				/* remaining number of plottable points to skip */
		save_count;				/* index into traj_seg to write the next point */
  double **traj_seg_save, step_save;

  integ_cntl->iterations= -1;
  if( !integ_cntl->enable_fstop) return(MAJOR_ERROR);

  iter_request_save = integ_cntl->iter_request;
  traj_seg_save = integ_cntl->traj_segment;
  start_to_save_save = integ_cntl->start_to_save;
  prop_segsize_save = integ_cntl->prop_segsize;
  step_save = integ_cntl->estim_step;

  if (integ_cntl->prop_segsize>0) 
    {
      plot_switch = TRUE;
      integ_cntl->prop_segsize=0;
    }
  else plot_switch = FALSE;

  if (integ_cntl->start_to_save==0)
    {
      skip_start=0;
      save_count=1;
      count = integ_cntl->iter_request;
    }
  else
    {
      skip_start=abs(integ_cntl->start_to_save)-1;
      save_count=0;
      count = integ_cntl->iter_request+skip_start;
      integ_cntl->start_to_save = -1;
    }

  /* modify integration control structure so we may use ode_stop */
  integ_cntl->prop_mode = PROP_FSTOP;
  integ_cntl->iter_request = 1;

  /* loop over points */
  while (count > 0 && status==NO_ERROR)
    {
      if ((status = ode_stop(integ_cntl)) == NO_ERROR)
	{
	  /* OK, we got a point! */
	  count--;

	  dcopy(integ_cntl->ph_space_dim, traj_seg_save[save_count], 1, integ_cntl->state, 1);

	  /* have we skipped enought starting points? */
	  if (skip_start > 0) /* don't keep this */
	    {
	      skip_start--;
	    }
	  else  /* keep this point */
	    {
	      save_count++;
	      integ_cntl->traj_segment=traj_seg_save+save_count;
	    }

	  /* have we saved enough to plot some points? */
	  if (plot_switch && save_count-start >= integ_cntl->prop_segsize)
	    {
	      integ_cntl->traj_segment = traj_seg_save;
	      integ_cntl->plot_traj(integ_cntl, start, save_count-1, integ_cntl->workspace);
	      start = save_count;
	      integ_cntl->traj_segment = traj_seg_save+save_count;
	    }
	}
      integ_cntl->start_to_save = -1;
      integ_cntl->time_step = integ_cntl->estim_step = step_save;
    }

  /* restore structure (some of this may not be necessary!) */
  integ_cntl->prop_mode = PROP_POINCARE;
  integ_cntl->iter_request = iter_request_save;
  integ_cntl->traj_segment = traj_seg_save;
  integ_cntl->start_to_save = start_to_save_save;
  integ_cntl->prop_segsize = prop_segsize_save;

  /* plot any remaining points */
  if (plot_switch && save_count-start>0)              
    integ_cntl->plot_traj(integ_cntl, start, save_count-1, integ_cntl->workspace);
  
  /* The value of integ_cntl->iterations gives the index of the final point in the traj_segment */
  integ_cntl->iterations = save_count-1;

  return(status);
}

