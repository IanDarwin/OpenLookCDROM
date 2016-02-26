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
#include <math.h>

int
ode_nsteps(integ_cntl)
struct  Prop_DataS     *integ_cntl;
{
  int 		i, 
		status = NO_ERROR, 
		stop_flag = FALSE, 
		mode=VARB_STEP_INIT,
  		plot_switch = FALSE,			/* determines whether to plot or not */
  		count,					/* max number of integrator steps */
  		start = 0,				/* point to start plotting */
		skip_count = 0,				/* counts how many integrator steps have been skipped */
		skip_start,				/* remaining number of plottable points to skip */
		save_count,				/* index into traj_seg to write the next point */
		init_save_count, 			/* initial value of save_count */
  		interrupt();
  double	*v, *dwork_unused;

  integ_cntl->iterations= -1;
  if( !integ_cntl->enable_nsteps ) return (MAJOR_ERROR);	/* error */

  v = integ_cntl->workspace;
  dwork_unused = v + integ_cntl->ph_space_dim;
  dcopy(integ_cntl->ph_space_dim, integ_cntl->state, 1, v, 1);

  if( integ_cntl->prop_segsize > 0 ) plot_switch = TRUE; 	/* PLOT the points */

  if (integ_cntl->start_to_save==0)
    {
      skip_start=0;
      save_count=init_save_count=1;
      count = integ_cntl->iter_request * integ_cntl->f_skip;
    }
  else
    {
      skip_start=abs(integ_cntl->start_to_save)-1;
      save_count=init_save_count=0;
      count = (integ_cntl->iter_request+skip_start) * integ_cntl->f_skip;
    }

  while ( count>0  && !stop_flag && status == NO_ERROR)
    {
      status = integ_cntl->integ_driver(integ_cntl->traj_segment[save_count], v, dwork_unused, integ_cntl, mode);
      mode = VARB_STEP;
      project( integ_cntl->ph_space_dim-1, integ_cntl->traj_segment[save_count], integ_cntl->manifold );

/*      for(i=0; i<integ_cntl->ph_space_dim; i++)
	v[i] = integ_cntl->traj_segment[save_count][i];*/
	
      if (status == NO_ERROR)
	  for(i=0; i<integ_cntl->ph_space_dim; i++)
	      v[i] = integ_cntl->traj_segment[save_count][i];
      else if (save_count > init_save_count) /* unsure about save_count == init_save_count case */
	  save_count--;
  
      /* see if new point has diverged */
      for(i=0;i<integ_cntl->ph_space_dim-1;i++)
	{
/*	  if(v[i] > integ_cntl->diverg_cutoff || v[i] < -integ_cntl->diverg_cutoff)*/ 
	  if(! finite(v[i]) || 
	     v[i] > integ_cntl->diverg_cutoff || 
	     v[i] < -integ_cntl->diverg_cutoff) 
	   {
	     system_mess_proc(1,"Orbits appear to diverge off to an infinity! Stop!");
	     integ_cntl->iterations = save_count-1;
	     if (save_count-start>0 && plot_switch)              
	       integ_cntl->plot_traj(integ_cntl, start, save_count-1, dwork_unused);
	     return(MINOR_ERROR); 
	   }
	}

      /* so we have a valid new point */
      count--;
      skip_count++;

      /* have we skipped enough to make it count? */
      if(skip_count >= integ_cntl->f_skip)
	{
	  skip_count = 0;
	  if (skip_start > 0) skip_start--;		/* paw  4/14/92 */
	  else save_count++;
	}

      /* have we saved enough to plot some points? */
      if (plot_switch && save_count-start >= integ_cntl->prop_segsize)
	{
	  integ_cntl->plot_traj(integ_cntl, start, save_count-1, dwork_unused);
	  start = save_count;
	}

      /* has the user tried to stop us? */
      if( interrupt() )
	{
	 stop_flag = TRUE;
	 status = MAJOR_ERROR;
        } 
    }

  /* lets plot the rest of the points */
  if (plot_switch && save_count-start >0)              
    integ_cntl->plot_traj(integ_cntl, start, save_count-1, dwork_unused);

  /* The value of integ_cntl->iterations gives the index of the final point in the traj_segment */
  integ_cntl->iterations = save_count-1;

  return(status);
}

