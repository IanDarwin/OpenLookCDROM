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

/* Integrate until crossing event */
int
ode_stop(integ_cntl)
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
  		delay_flag = TRUE,			/* used so that there is no delay,
							   pass in negative start_to_save */
		start_to_sv,				/* modified start_to_save */
  		interrupt(),
  		get_total_fstop(),
                vdim, projected;   
  double	*old, *u, *w, *dwork_unused, *new;

  int *stop_status, *stop_action, *ivector();

  integ_cntl->iterations = -1;  /* this is the initial error return value */

  if( !integ_cntl->enable_fstop) return(MAJOR_ERROR);
	
  old = integ_cntl->workspace;				/* old[ph_space_dim] */
  u = old + integ_cntl->ph_space_dim;			/* u[total_fstop] */
  w = u + get_total_fstop(integ_cntl);			/* w[function_dim] */
  dwork_unused = w + integ_cntl->function_dim;
  stop_action = ivector(0, get_total_fstop(integ_cntl)-1);
  stop_status = ivector(0, get_total_fstop(integ_cntl)-1);

  dcopy(integ_cntl->ph_space_dim, integ_cntl->state, 1, old, 1);
  if( integ_cntl->prop_segsize > 0 ) plot_switch = TRUE;
  vdim = integ_cntl->ph_space_dim - 1;

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
	  delay_flag = FALSE;
	  start_to_sv = - start_to_sv;
	}
      skip_start=start_to_sv-1;
      save_count=0;
      count = (integ_cntl->iter_request+skip_start) * integ_cntl->f_skip;
    }


  /* determines when to start checking for stopping condition */
  if (delay_flag) n = count + 1 - start_to_sv*integ_cntl->f_skip;
  else n = count + 1;
  if (n > count)
    stop_flag = prop_fstop_init(integ_cntl, stop_action, stop_status, old);

  new = integ_cntl->traj_segment[save_count];
  while( count >0 && !stop_flag)
    {
      status = integ_cntl->integ_driver(new, old, dwork_unused, integ_cntl, mode);

      mode = VARB_STEP;

      if (status != NO_ERROR)		/* paw - error integrating, plot and exit with what we have! */
	{
	  integ_cntl->iterations = save_count-1;
	  if (plot_switch && save_count-start > 0)              
	    integ_cntl->plot_traj(integ_cntl, start, save_count-1, w);
	  free_ivector(stop_action, 0, get_total_fstop(integ_cntl)-1);
	  free_ivector(stop_status, 0, get_total_fstop(integ_cntl)-1);
	  return(status);
	}

      /* see if new point has diverged */
      for(i=0; i<vdim; i++)
	{
/*	  if (new[i] > integ_cntl->diverg_cutoff || new[i] < -integ_cntl->diverg_cutoff)*/
	  if(! finite(new[i]) || 
	     new[i] > integ_cntl->diverg_cutoff || 
	     new[i] < -integ_cntl->diverg_cutoff)
	    {
	      system_mess_proc(1,"Orbits appear to diverge off to an infinity! Stop!");
	      integ_cntl->iterations = save_count-1;
	      if (plot_switch && save_count-start > 0)              
		integ_cntl->plot_traj(integ_cntl, start, save_count-1, w);
	      free_ivector(stop_action, 0, get_total_fstop(integ_cntl)-1);
	      free_ivector(stop_status, 0, get_total_fstop(integ_cntl)-1);
	      return( MINOR_ERROR );
	    }
	}
      

      if (count < n)
	{
	  /* check to see if we have met the stopping condition */
	  projected = project_and_flag( vdim, new, integ_cntl->manifold );
	  if (!projected)
	    {
	      if (stop_flag = prop_fstop_test( integ_cntl, stop_action, stop_status, new))
		{
		  integ_cntl->time_step = fabs(new[vdim] - old[vdim]);
		  status = integ_last_step( integ_cntl, old, new, integ_cntl->time_step, 
					   stop_status, stop_action, dwork_unused);
		  if (status != NO_ERROR)
		    system_mess_proc(1,"Event detected, but no convergence.  Try reducing integration step size.");
		}
	    }
	  else
	    {
	      if (stop_flag = prop_fstop_lift_test(integ_cntl, stop_action, stop_status, new, old,
						   dwork_unused))
		{
		  integ_cntl->time_step = fabs(new[vdim] - old[vdim]);
		  status = integ_last_step_lift( integ_cntl, old, new, integ_cntl->time_step, 
						stop_status, stop_action, dwork_unused);
		  project( vdim, new, integ_cntl->manifold ); /* fjw 9/20/92 */
		  if (status != NO_ERROR)
		    system_mess_proc(1,"Event detected, but no convergence.  Try reducing integration step size.");
		}
	    }
	}
      else
	{
	  project( vdim, new, integ_cntl->manifold );
	  if (count == n)  	      /* initialize stopping condition */
	    stop_flag = prop_fstop_init( integ_cntl, stop_action, stop_status, new);
	}
      
      
      if (status == NO_ERROR)
	{
	  for(i=0; i<integ_cntl->ph_space_dim; i++)
	    old[i] = new[i];
      
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
	      else 
		{
		  new = integ_cntl->traj_segment[++save_count];
		}
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
    }

  if (!stop_flag && status==NO_ERROR) status = MINOR_ERROR; /* this means we hit max iters so return a flag, before event */

  if (plot_switch && save_count-start>0)              
    integ_cntl->plot_traj(integ_cntl, start, save_count-1, w);
  
  /* The value of integ_cntl->iterations gives the index of the final point in the traj_segment */
  integ_cntl->iterations = save_count-1;

  free_ivector(stop_action, 0, get_total_fstop(integ_cntl)-1);
  free_ivector(stop_status, 0, get_total_fstop(integ_cntl)-1);

  return( status );
}



/*
 * procedure to return whether a desired crossing has occurred
 *
 * the action/status chart:
 *
 *                               old_status
 *                        ZERO              PLUS                MINUS
 *
 *   value > target   NO_CROSSING/PLUS   NO_CROSSING/PLUS        CROSSING/MP(PLUS)
 *
 *   value < target   NO_CROSSING/MINUS  CROSSING/PM(MINUS)   NO_CROSSING/MINUS
 *
 *   value = target   NO_CROSSING/ZERO   CROSSING/PM(ZERO)    CROSSING/MP(ZERO)
 *
 * the items in parenthesis indicate the new status if this crossing
 * was of the wrong orientation.
 *
 * The procedure prop_fstop_lift_stop should be used for periodic vector fields
 * if a boundary has been crossed.
 *
 * The array stop_status is assumed to have been set before the function is called. 
 * Both stop_status and stop_action are updated by the routine.
 * Legal values of stop_status[i] include PLUS, MINUS, ZERO, PM, and MP.
 * Legal values of stop_action[i] include CROSSING and NO_CROSSING. 
 *
 * Status values returned include TRUE and FALSE. TRUE indicates at least
 * one stop_action[i] has been set to CROSSING.
 */
int
prop_fstop_test( prop_cntl, stop_action, stop_status, current_state )
struct  Prop_DataS *prop_cntl;
int *stop_action, *stop_status;
double *current_state;
{
   int count, group, entry, desired_orientation, status = FALSE;
   double value;
   int ntotal = (int) get_total_fstop(prop_cntl);

   for (count=0; count<ntotal; count++)
       {
	 get_fstop_grp_entry(prop_cntl, count, &group, &entry);
	 prop_cntl->fstop_grp_list[group]->
	   fstop_func_ptr(current_state, prop_cntl->parameters,
			  prop_cntl->fstop_grp_list[group]->fstop_indices[entry], &value);
	 desired_orientation = prop_cntl->fstop_grp_list[group]->cross_orient[entry];
	 value = value - prop_cntl->fstop_grp_list[group]->target_values[entry];

	 if (value>0)
	   {
	     if ( (stop_status[count] == MINUS) && 
		 ( (desired_orientation == PM_and_MP) || (desired_orientation == MP)))
	       {
		 stop_action[count] = CROSSING;
		 stop_status[count] = MP;
		 status = TRUE;
	       }
	     else
	       {
		 stop_action[count] = NO_CROSSING;
		 stop_status[count] = PLUS;
	       }
	   }

	 else if (value<0)
	   {
	     if ( (stop_status[count] == PLUS) && 
		 ( (desired_orientation == PM_and_MP) || (desired_orientation == PM)))
	       {
		 stop_action[count] = CROSSING;
		 stop_status[count] = PM;
		 status = TRUE;
	       }
	     else
	       {
		 stop_action[count] = NO_CROSSING;
		 stop_status[count] = MINUS;
	       }
	   }

	 else
	   {
	     if ( (stop_status[count] == MINUS) &&
		 ( (desired_orientation == PM_and_MP) || (desired_orientation == MP)))
	       {
		 stop_action[count] = CROSSING;
		 stop_status[count] = MP;
		 status = TRUE;
	       }
	     else if ( (stop_status[count] == PLUS) &&
		 ( (desired_orientation == PM_and_MP) || (desired_orientation == PM)))
	       {
		 stop_action[count] = CROSSING;
		 stop_status[count] = PM;
		 status = TRUE;
	       }
	     else
	       {
		 stop_action[count] = NO_CROSSING;
		 stop_status[count] = ZERO;
	       }
	   }
	 
       }
   
   return(status);
 }



/*
 * procedure to return whether a desired crossing has occurred following
 * a boundary crossing for PERIODIC phase space.
 *
 * The array stop_status is assumed to have been set before the function is called. 
 * Both stop_status and stop_action are updated by the routine.
 * Legal values of stop_status[i] include PLUS, MINUS, ZERO, PM, and MP.
 * Legal values of stop_action[i] include CROSSING and NO_CROSSING. 
 *
 * Status values returned include TRUE and FALSE. TRUE indicates at least
 * one stop_action[i] has been set to CROSSING_NEW or CROSSING_OLD.
 *
 * The logic in this procedure is lengthy, but straightforward.  The efficiency
 * could be improved (plus length shortened) but mainly at the expense of clarity!
 */
int
prop_fstop_lift_test( prop_cntl, stop_action, stop_status, new, old, work )
struct  Prop_DataS *prop_cntl;
int *stop_action, *stop_status;
double *new, *old, *work;
{
   int count, group, entry, desired_orientation, status = FALSE;
   double *newlift, *oldlift, newvalue, newliftvalue, oldliftvalue, lift();
   int ntotal = (int) get_total_fstop(prop_cntl);
   int vdim = prop_cntl->ph_space_dim-1;

   newlift = work;
   oldlift = work + prop_cntl->ph_space_dim;

   /* lift old and new and copy over time */
   lift(vdim, new, old, oldlift, prop_cntl->manifold);
   oldlift[vdim] = old[vdim];
   lift(vdim, old, new, newlift, prop_cntl->manifold);
   newlift[vdim] = new[vdim];

   for (count=0; count<ntotal; count++)
     {
       get_fstop_grp_entry(prop_cntl, count, &group, &entry);
       desired_orientation = prop_cntl->fstop_grp_list[group]->cross_orient[entry];
       prop_cntl->fstop_grp_list[group]->
	 fstop_func_ptr(new, prop_cntl->parameters,
			prop_cntl->fstop_grp_list[group]->fstop_indices[entry], &newvalue);
       prop_cntl->fstop_grp_list[group]->
	 fstop_func_ptr(newlift, prop_cntl->parameters,
			prop_cntl->fstop_grp_list[group]->fstop_indices[entry], &newliftvalue);
       prop_cntl->fstop_grp_list[group]->
	 fstop_func_ptr(oldlift, prop_cntl->parameters,
			prop_cntl->fstop_grp_list[group]->fstop_indices[entry], &oldliftvalue);
       newvalue -= prop_cntl->fstop_grp_list[group]->target_values[entry];
       newliftvalue -= prop_cntl->fstop_grp_list[group]->target_values[entry];
       oldliftvalue -= prop_cntl->fstop_grp_list[group]->target_values[entry];

       if (stop_status[count] == ZERO)
	 {
	   if (newvalue > 0)
	     {
	       stop_action[count] = NO_CROSSING;
	       stop_status[count] = PLUS;
	     }
	   else if (newvalue < 0)
	     {
	       stop_action[count] = NO_CROSSING;
	       stop_status[count] = MINUS;
	     }
	   else
	     {
	       stop_action[count] = NO_CROSSING;
	       stop_status[count] = ZERO;
	     }
	 }
       else /* STATUS != ZERO */
	 {
	   if (newvalue > 0)
	     {
	       if (oldliftvalue < 0) /* MP crossing */
		 {
		   if (desired_orientation == PM_and_MP || desired_orientation == MP)
		     {
		       stop_action[count] = CROSSING_NEW;
		       stop_status[count] = MP;
		       status = TRUE;
		     }
		   else
		     {
		       stop_action[count] = NO_CROSSING;
		       stop_status[count] = PLUS;
		     }
		 }
	       else if (oldliftvalue > 0) /* check for crossing near old point */
		 {
		   if (newliftvalue > 0)
		     {
		       if (stop_status[count] == PLUS)
			 {
			   stop_action[count] = NO_CROSSING;
			   stop_status[count] = PLUS;
			 }
		       else /* MP crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == MP)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = MP;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = PLUS;
			     }
			 }
		     }
		   else if (newliftvalue < 0)
		     {
		       if (stop_status[count] == MINUS)
			 {
			   stop_action[count] = NO_CROSSING;
			   stop_status[count] = PLUS;
			 }
		       else /* PM crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == PM)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = PM;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = PLUS;
			     }
			 }
		     }
		   else /* newliftvalue = 0, CROSSING */
		     {
		       if (stop_status[count] == PLUS) /* PM crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == PM)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = PM;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = PLUS;
			     }
			 }
		       else /* MP crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == MP)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = MP;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = PLUS;
			     }
			 }
		     }
		 }
	       else /* INCONSISTENT stopping condition, skip! */
		 {
		   stop_action[count] = NO_CROSSING;
		   stop_status[count] = PLUS;
		 }
	     }
	   else if (newvalue < 0)
	     {
	       if (oldliftvalue > 0) /* PM crossing */
		 {
		   if (desired_orientation == PM_and_MP || desired_orientation == PM)
		     {
		       stop_action[count] = CROSSING_NEW;
		       stop_status[count] = PM;
		       status = TRUE;
		     }
		   else
		     {
		       stop_action[count] = NO_CROSSING;
		       stop_status[count] = MINUS;
		     }
		 }
	       else if (oldliftvalue < 0) /* check for crossing near old point */
		 {
		   if (newliftvalue > 0)
		     {
		       if (stop_status[count] == PLUS)
			 {
			   stop_action[count] = NO_CROSSING;
			   stop_status[count] = MINUS;
			 }
		       else /* MP crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == MP)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = MP;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = MINUS;
			     }
			 }
		     }
		   else if (newliftvalue < 0)
		     {
		       if (stop_status[count] == MINUS)
			 {
			   stop_action[count] = NO_CROSSING;
			   stop_status[count] = MINUS;
			 }
		       else /* PM crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == PM)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = PM;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = MINUS;
			     }
			 }
		     }
		   else /* newliftvalue = 0, CROSSING */
		     {
		       if (stop_status[count] == PLUS) /* PM crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == PM)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = PM;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = MINUS;
			     }
			 }
		       else /* MP crossing */
			 {
			   if (desired_orientation == PM_and_MP || desired_orientation == MP)
			     {
			       stop_action[count] = CROSSING_OLD;
			       stop_status[count] = MP;
			       status = TRUE;
			     }
			   else
			     {
			       stop_action[count] = NO_CROSSING;
			       stop_status[count] = MINUS;
			     }
			 }
		     }
		 }
	       else /* INCONSISTENT stopping condition, skip! */
		 {
		   stop_action[count] = NO_CROSSING;
		   stop_status[count] = MINUS;
		 }
	     }
	   else /* newvalue == 0 */
	     {
	       if (oldliftvalue > 0) /* PM crossing */
		 {
		   if (desired_orientation == PM_and_MP || desired_orientation == PM)
		     {
		       stop_action[count] = CROSSING_NEW;
		       stop_status[count] = PM;
		       status = TRUE;
		     }
		   else
		     {
		       stop_action[count] = NO_CROSSING;
		       stop_status[count] = ZERO;
		     }
		 }
	       else if (oldliftvalue < 0) /* MP crossing */
		 {
		   if (desired_orientation == PM_and_MP || desired_orientation == MP)
		     {
		       stop_action[count] = CROSSING_NEW;
		       stop_status[count] = MP;
		       status = TRUE;
		     }
		   else
		     {
		       stop_action[count] = NO_CROSSING;
		       stop_status[count] = ZERO;
		     }
		 }
	       else /* INCONSISTENT stopping condition, skip! */
		 {
		   stop_action[count] = NO_CROSSING;
		   stop_status[count] = ZERO;
		 }
	     }
	 }
     }
   return status;
}



/* procedure to initialize the action and status arrays
 *
 * if the initial value is within conv_err of a stopping condition
 * then we assume that we are starting ON the surface.
 *
 */
prop_fstop_init( prop_cntl, stop_action, stop_status, current_state)
struct Prop_DataS *prop_cntl;
int *stop_action, *stop_status;
double *current_state;
{
  int count, group, entry, get_total_fstop();
  double value;

  int ntotal = get_total_fstop(prop_cntl);
  double conv_err = prop_cntl->panel_dp_values[1];

  for (count=0; count<ntotal; count++)
    {
      get_fstop_grp_entry(prop_cntl,count,&group,&entry);
      prop_cntl->fstop_grp_list[group]->
	fstop_func_ptr(current_state, prop_cntl->parameters,
		       prop_cntl->fstop_grp_list[group]->fstop_indices[entry], &value);
      value = value - prop_cntl->fstop_grp_list[group]->target_values[entry];

      if (fabs(value) < conv_err) stop_status[count] = ZERO;
      else if (value > 0) stop_status[count] = PLUS;
      else stop_status[count] = MINUS;

      stop_action[count] = NO_CROSSING;
    }

  return( NO_ERROR );
}



/*
 * procedure to convert group and entry into index into full list of 
 * criteria arrays
 *
 *  last change:  01/14/92	(mrm)
*/
int
get_fstop_grp_entry( integ_cntl, index, group, entry )
int index, *group, *entry;
struct  Prop_DataS     *integ_cntl;
{
  int	i, status=NO_ERROR;

  *group = 0;
  for(i=0; i<integ_cntl->num_fstop_grps; i++)
     if( index >= integ_cntl->fstop_grp_list[i]->num_fstop_indices )	/* mod out the previous sets  */
       {								/* of entries by group...what */
        index -= integ_cntl->fstop_grp_list[i]->num_fstop_indices;	/* remains is the entry id    */
	++(*group);
       }
     else break;
  *entry = index;
   
  return(status);
}


/*
 * procedure to convert full list index number into group and
 * entry id's     
 *
 *  last change:  01/14/92	(mrm)
*/
int
get_fstop_index( integ_cntl, index, group, entry )
int *index, group, entry;
struct  Prop_DataS     *integ_cntl;
{
  int	i, status=NO_ERROR, total=0;

  for(i=0; i<group; i++)
    total += integ_cntl->fstop_grp_list[i]->num_fstop_indices;

  *index = entry + total;		/*  index = (# groups)*(# entries in each group) + entry  */
   
  return(status);
}



/*
 * procedure to convert full list index number into group and
 * entry id's     
 *
 *  last change:  01/14/92	(mrm)
*/
int
get_total_fstop( integ_cntl )
struct  Prop_DataS     *integ_cntl;
{
  int	i, total=0;

  for(i=0; i<integ_cntl->num_fstop_grps; i++)
    total += integ_cntl->fstop_grp_list[i]->num_fstop_indices;

  return(total);
}

