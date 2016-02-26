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

/* Integrate until fixed time. */
int
ode_time(integ_cntl)
struct  Prop_DataS     *integ_cntl;
{
  int 		i, n, 
		status = NO_ERROR, 
		stop_flag = FALSE, 
		mode=VARB_STEP_INIT,
  		plot_switch = FALSE,			/* determines whether to plot or not */
  		count,					/* max number of integrator steps */
  		start = 0,				/* point to start plotting */
		skip_count = 0,				/* counts how many integrator steps have been skipped */
		skip_start,				/* remaining number of plottable points to skip */
		save_count,				/* index into traj_seg to write the next point */
		start_to_sv,				/* modified start_to_save */
  		interrupt(),
  		get_total_fstop();
  double	*v, *u, *w, *dwork_unused, dt, t_diff, min_dt;

  int *stop_status, *stop_action, *ivector();
  int direction = abs(integ_cntl->direction);

  integ_cntl->iterations = -1;  /* this is the initial error return value */

  if( !integ_cntl->enable_tstop) return(MAJOR_ERROR);
	
  v = integ_cntl->workspace;				/* v[ph_space_dim] */
  u = v + integ_cntl->ph_space_dim;			/* u[total_fstop] */
  w = u + get_total_fstop(integ_cntl);			/* w[function_dim] */
  dwork_unused = w + integ_cntl->function_dim;
  stop_status = ivector(0, get_total_fstop(integ_cntl)-1);
  stop_action = ivector(0, get_total_fstop(integ_cntl)-1);

  min_dt = integ_cntl->panel_dp_values[2];
  dcopy(integ_cntl->ph_space_dim, integ_cntl->state, 1, v, 1);
  if( integ_cntl->prop_segsize > 0 ) plot_switch = TRUE;

  start_to_sv = integ_cntl->start_to_save;
  
  if (start_to_sv == 0)
    {
      skip_start=0;
      save_count=1;
      count = integ_cntl->iter_request * integ_cntl->f_skip;
    }
  else
    {
      if (start_to_sv < 0)
	{
	  start_to_sv = - start_to_sv;
	}
      skip_start=start_to_sv-1;
      save_count=0;
      count = (integ_cntl->iter_request+skip_start) * integ_cntl->f_skip;
    }

  /* if we are doing prop to a final time with a fixed step,
     then compute the time step and number of integrator steps */
  if (integ_cntl->fixed_step_flag)
     {
       t_diff = integ_cntl->final_time-v[integ_cntl->ph_space_dim-1];

       /* error checking */
       if (t_diff == 0)
	 {
	   system_mess_proc(1, "Final time equals initial time.");
	   return(MAJOR_ERROR);
	 }
       else if (t_diff < 0)
	 {
	   if (direction == BACKWARD)
	     t_diff = fabs(t_diff);
	   else
	     {
	       system_mess_proc(1, "Final time before initial time - go BACKWARDS!");
	       return(MAJOR_ERROR);
	     }
	 }
       else if (direction == BACKWARD)
	 {
	   system_mess_proc(1, "Final time after initial time - go FORWARDS!");
	   return(MAJOR_ERROR);
	 }

       if ( integ_cntl->time_step < min_dt )
	 {
	   system_mess_proc(1,"Stepsize too small! Stop!");
	   return(MAJOR_ERROR); 
	 }

       n = (int) ceil( t_diff/integ_cntl->time_step);
       if (n<=count)		/* should work OK */
	 {
	   dt = t_diff / ((double) n) ;
	   if (dt >= min_dt)
	     integ_cntl->time_step = dt;
	   else
	     {
	       system_mess_proc(1,"Computed time step too small! Stop!");
	       return(MAJOR_ERROR); 
	     }
	   count = n;
	 }
       else			/* won't make it to stopping pt, so take full steps */
	 {
	   dt = integ_cntl->time_step;
	   system_mess_proc(1,"Increase number of steps to flow to final time.");
	 }
       
     }

  while( count >0 && !stop_flag)
    {

      status = integ_cntl->integ_driver(integ_cntl->traj_segment[save_count], v, 
					dwork_unused, integ_cntl, mode);
      mode = VARB_STEP;

      if (status != NO_ERROR)		/* error integrating, plot and exit with what we have! */
	{
	  integ_cntl->iterations = save_count-1;
	  if (plot_switch && save_count-start > 0)              
	    integ_cntl->plot_traj(integ_cntl, start, save_count-1, w);
	  return(status);
	}

      project( integ_cntl->ph_space_dim-1, integ_cntl->traj_segment[save_count], integ_cntl->manifold );

      /* see if new point has diverged */
      for(i=0; i<integ_cntl->ph_space_dim-1; i++)
	{
/*	  if (integ_cntl->traj_segment[save_count][i] > integ_cntl->diverg_cutoff || 
	      integ_cntl->traj_segment[save_count][i] < -integ_cntl->diverg_cutoff)*/
	  if (! finite(integ_cntl->traj_segment[save_count][i]) ||
	      integ_cntl->traj_segment[save_count][i] > integ_cntl->diverg_cutoff || 
	      integ_cntl->traj_segment[save_count][i] < -integ_cntl->diverg_cutoff)
	    {
	      system_mess_proc(1,"Orbits appear to diverge off to an infinity! Stop!");
	      integ_cntl->iterations = save_count-1;
	      if (plot_switch && save_count-start > 0)              
		integ_cntl->plot_traj(integ_cntl, start, save_count-1, w);
	      return(MINOR_ERROR);
	    }
	}
      
      if(!integ_cntl->fixed_step_flag)
	{
	  /* check to see if we have passed through time for non-fixed integrators */
	  t_diff = integ_cntl->traj_segment[save_count][integ_cntl->ph_space_dim-1] - integ_cntl->final_time;
	  if (t_diff == 0) stop_flag = TRUE;
	  else if( ( direction == FORWARD ? (t_diff > 0) : (t_diff < 0) ) )
	    {
	     stop_flag = TRUE;
	     integ_cntl->time_step = fabs(integ_cntl->final_time-v[integ_cntl->ph_space_dim-1]);
	     status = integ_cntl->integ_driver(integ_cntl->traj_segment[save_count], 
					       v, dwork_unused, integ_cntl, FIXED_STEP_INIT);
	   }
	}

      for(i=0; i<integ_cntl->ph_space_dim; i++)
	v[i] = integ_cntl->traj_segment[save_count][i];
      
      /* so we have a valid new point */
      count--;
      skip_count++;

      /* Was it a stopping condition point? */
      if (stop_flag)
	{
	  skip_count = 0;
	  if (skip_start>0) 
	    {
	      skip_start--;
	      stop_flag = FALSE;
	    }
	  else save_count++;
	}
      /* Or have we skipped enough to make it count? */
      else if (skip_count >= integ_cntl->f_skip)
	{
	  skip_count = 0;
	  if (skip_start>0) skip_start--;
	  else save_count++;
	}

      /* have we saved enough to plot some points? */
      if (plot_switch && save_count-start >= integ_cntl->prop_segsize)
	{
	  integ_cntl->plot_traj(integ_cntl, start, save_count-1, w);
	  start = save_count;
	}
      
      /* has the user tried to stop us? */
      if(!stop_flag && interrupt() ) 
	{
	  stop_flag = TRUE;
	  status = MAJOR_ERROR;
	}
    }
  
  /* check if we hit max iters so return a flag */
  if (!stop_flag && status==NO_ERROR && !integ_cntl->fixed_step_flag) status = MINOR_ERROR;

  if (plot_switch && save_count-start>0)              
    integ_cntl->plot_traj(integ_cntl, start, save_count-1, w);
  
  /* The value of integ_cntl->iterations gives the index of the final point in the traj_segment */
  integ_cntl->iterations = save_count-1;

  return( status );
}

