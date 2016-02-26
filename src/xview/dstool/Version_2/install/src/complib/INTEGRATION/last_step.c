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
#include <constants.h>
#include <defaults.h>
#include <complib.h>


/* ------------------------------------------------------------------------------------------
   integ_last_step :  procedure for computing final propagation step to a boundary, in the case
		  that stopping conditions, other than final time, have been specified.
		  The algorithm used is a simple series of Newton steps (started from a
		  secant step) - more sophisticated
		  algorithms may be substituted provided they respect the data flow convent-
		  ions.  If an event has occurred for a PERIODIC space phase at a boundary
		  then integ_last_step_lift should be used.

   input arguments: 	
			integ_cntl	(input)    pointer to prop subsection data struct
			initial_state   (input)    initial traj state (inside boundary)
			workspace       (input)    pre-allocated workspace array of doubles
			final_state     (output)   final trajectory state (on boundary to 
						   tolerance)
			stop_status     (input)    indicates orientation of crossing
			stop_action     (input)    indicates which function crossed
			stepsize	(output)   stepsize taken to boundary

   retruned value:
		  Routine returns a integer status termination flag whose value indicates:
						0	normal
						+	warning
						-	failure

   called routines:
			fabs(), functions pointers passed in integ_cntl structure 
   
   last change:   6/27/92  paw

   ------------------------------------------------------------------------------------------ */



int
integ_last_step( integ_cntl, initial_state, final_state, stepsize, 
		stop_status, stop_action, workspace) 
struct Prop_DataS *integ_cntl;
double *initial_state, *final_state, stepsize, *workspace;
int *stop_status, *stop_action;
{
  int count, index, group, entry, status = 0;
  double diff, new_step, target, last_value, next_value, 
    test, *u, *unused_work;
  int ntotal = (int) get_total_fstop(integ_cntl);
  double fd_step = integ_cntl->panel_dp_values[0];  /* fetch control parameters from list items. */
  double conv_err = integ_cntl->panel_dp_values[1]; /* These typically (but not always) result   */
  int max_iter = integ_cntl->panel_int_values[0];   /*  from prop panel entries.	         */
  
  u = workspace;
  unused_work = u + integ_cntl->ph_space_dim;

  /* find first crossing event */
  index = -1;
  for (count=0; (count < ntotal) && index==-1; count++)
    if (stop_action[count] == CROSSING) index = count;
  
  if (index == -1)
    {
      /* no crossing in action array - error */
      return BAD_CROSSING_CONDITION;
    }

  /* get fstop function value at start and end */
  get_fstop_grp_entry(integ_cntl, index, &group, &entry);
  integ_cntl->fstop_grp_list[group]->
    fstop_func_ptr(initial_state, integ_cntl->parameters,
	 integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &last_value);
  integ_cntl->fstop_grp_list[group]->
    fstop_func_ptr(final_state, integ_cntl->parameters,
		   integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &next_value);
  target = integ_cntl->fstop_grp_list[group]->target_values[entry];
  
  /* may want to check that target is not exactly on section - paw */

  /* estimate the occurrence of the first crossing */
  diff = fabs( next_value - last_value); 	
  if (diff <= DIVISOR_TOO_SMALL ) return(BAD_CROSSING_CONDITION);	/* can't solve for last step */
  new_step = fabs(target - last_value)*stepsize/diff;

  if( integ_cntl->fstop_grp_list[group]->cross_mode[entry] == PRE_STEP)
    dcopy(integ_cntl->ph_space_dim, initial_state, 1, final_state, 1);

  else if ( integ_cntl->fstop_grp_list[group]->cross_mode[entry] == POST_STEP)
    {
    /* we don't need to do anything!  mrm 8/26/92 */
    }
  
  else if ( integ_cntl->fstop_grp_list[group]->cross_mode[entry] == SOLVE_STEP)
    {
      /* use secant method to find a new step */
      integ_cntl->time_step = new_step;
      status = integ_cntl->integ_driver(final_state, initial_state, unused_work, integ_cntl, FIXED_STEP_INIT);
      integ_cntl->fstop_grp_list[group]->
	fstop_func_ptr(final_state, integ_cntl->parameters,
		       integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &last_value);
      test = fabs( last_value - target );
      for (count=0; count < max_iter; count++ )
	{
	  if(test < conv_err) return(status);	/* we're done! */
	  integ_cntl->time_step = new_step + fd_step;
	  status = integ_cntl->integ_driver(u, initial_state, unused_work, integ_cntl, FIXED_STEP_INIT);
	  integ_cntl->fstop_grp_list[group]->
	    fstop_func_ptr(u, integ_cntl->parameters,
			   integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &next_value);
	  diff = next_value - last_value;
	  if( fabs(diff) < DIVISOR_TOO_SMALL ) return(LAST_STEP_TOO_SMALL);  	/* catch errors  ... */
	  new_step = new_step - (last_value - target)*fd_step/diff; 		/* estimate a new step */
	  if ( (new_step > stepsize) || (new_step < 0) ) return(STEP_OUTSIDE_INTERVAL);
	  integ_cntl->time_step = new_step;
	  status = integ_cntl->integ_driver(final_state, initial_state, unused_work, 
					    integ_cntl, FIXED_STEP_INIT);
	  integ_cntl->fstop_grp_list[group]->
	    fstop_func_ptr(final_state, integ_cntl->parameters,
			   integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &last_value);
	  test = fabs( last_value - target ); 
	}

      system_mess_proc(1,"Event stopping: Number of Newton steps exceeded.");
      status = EXCEED_MAX_ITER;
      }
  else
    {
      system_mess_proc(1,"Propagation to event failed.");
      status = BAD_CROSSING_CONDITION;
    }
  
  return(status);
}


/* ------------------------------------------------------------------------------------------
   integ_last_step_list :  procedure for computing final propagation step to a boundary, 
                  in the case that stopping conditions, other than final time, have been specified.
		  The algorithm used is a simple series of Newton steps (started with a
		  secant step) - more sophisticated
		  algorithms may be substituted provided they respect the data flow convent-
		  ions.  This algorithm is used when an event has occured when stepping
		  over a boundary.

   input arguments: 	
			integ_cntl	(input)    pointer to prop subsection data struct
			initial_state   (input)    initial traj state (inside boundary)
			workspace       (input)    pre-allocated workspace array of doubles
			final_state     (output)   final trajectory state (on boundary to 
						   tolerance)
			stop_status     (input)    indicates orientation of crossing
			stop_action     (input)    indicates which function crossed
			stepsize	(output)   stepsize taken to boundary

   retruned value:
		  Routine returns a integer status termination flag whose value indicates:
						0	normal
						+	warning
						-	failure

   called routines:
			fabs(), functions pointers passed in integ_cntl structure 

   side effects: the initial_state may be lifted!
   
   ------------------------------------------------------------------------------------------ */



int
integ_last_step_lift( integ_cntl, initial_state, final_state, stepsize, 
		stop_status, stop_action, workspace) 
struct Prop_DataS *integ_cntl;
double *initial_state, *final_state, stepsize, *workspace;
int *stop_status, *stop_action;
{
  int count, index, group, entry, status = 0;
  double diff, new_step, target, last_value, next_value, 
    test, *u, *unused_work, lift();
  int ntotal = (int) get_total_fstop(integ_cntl);
  double fd_step = integ_cntl->panel_dp_values[0];  /* fetch control parameters from list items. */
  double conv_err = integ_cntl->panel_dp_values[1]; /* These typically (but not always) result   */
  int max_iter = integ_cntl->panel_int_values[0];   /*  from prop panel entries.	         */
  
  u = workspace;
  unused_work = u + integ_cntl->ph_space_dim;

  /* find first crossing event */
  index = -1;
  for (count=0; (count < ntotal) && index==-1; count++)
    if (stop_action[count] == CROSSING_NEW || stop_action[count] == CROSSING_OLD) 
      index = count;
  
  if (index == -1)
    {
      /* no crossing in action array - error */
      return BAD_CROSSING_CONDITION;
    }

  if (stop_action[index] == CROSSING_NEW)
    lift(integ_cntl->ph_space_dim-1, final_state, initial_state, initial_state, integ_cntl->manifold);
  else
    lift(integ_cntl->ph_space_dim-1, initial_state, final_state, final_state, integ_cntl->manifold);

  /* get fstop function value at start and end */
  get_fstop_grp_entry(integ_cntl, index, &group, &entry);
  integ_cntl->fstop_grp_list[group]->
    fstop_func_ptr(initial_state, integ_cntl->parameters,
	 integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &last_value);
  integ_cntl->fstop_grp_list[group]->
    fstop_func_ptr(final_state, integ_cntl->parameters,
		   integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &next_value);
  target = integ_cntl->fstop_grp_list[group]->target_values[entry];
  
  /* may want to check that target is not exactly on section */

  /* estimate the occurrence of the first crossing */
  diff = fabs( next_value - last_value); 	
  if (diff <= DIVISOR_TOO_SMALL ) return(BAD_CROSSING_CONDITION);	/* can't solve for last step */
  new_step = fabs(target - last_value)*stepsize/diff;

  if( integ_cntl->fstop_grp_list[group]->cross_mode[entry] == PRE_STEP)
    dcopy(integ_cntl->ph_space_dim, initial_state, 1, final_state, 1);

  else if ( integ_cntl->fstop_grp_list[group]->cross_mode[entry] == POST_STEP) 
     {
      /* we don't need to do anything! */
     }
  
  else if ( integ_cntl->fstop_grp_list[group]->cross_mode[entry] == SOLVE_STEP)
    {
      /* use secant method to find a new step */
      integ_cntl->time_step = new_step;
      status = integ_cntl->integ_driver(final_state, initial_state, unused_work, integ_cntl, FIXED_STEP_INIT);
      integ_cntl->fstop_grp_list[group]->
	fstop_func_ptr(final_state, integ_cntl->parameters,
		       integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &last_value);
      test = fabs( last_value - target );
      for (count=0; count < max_iter; count++ )
	{
	  if(test < conv_err) return(status);	/* we're done! */
	  integ_cntl->time_step = new_step + fd_step;
	  status = integ_cntl->integ_driver(u, initial_state, unused_work, integ_cntl, FIXED_STEP_INIT);
	  integ_cntl->fstop_grp_list[group]->
	    fstop_func_ptr(u, integ_cntl->parameters,
			   integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &next_value);
	  diff = next_value - last_value;
	  if( fabs(diff) < DIVISOR_TOO_SMALL ) return(LAST_STEP_TOO_SMALL);  	/* catch errors  ... */
	  new_step = new_step - (last_value - target)*fd_step/diff; 		/* estimate a new step */
	  if ( (new_step > stepsize) || (new_step < 0) ) return(STEP_OUTSIDE_INTERVAL);
	  integ_cntl->time_step = new_step;
	  status = integ_cntl->integ_driver(final_state, initial_state, unused_work, 
					    integ_cntl, FIXED_STEP_INIT);
	  integ_cntl->fstop_grp_list[group]->
	    fstop_func_ptr(final_state, integ_cntl->parameters,
			   integ_cntl->fstop_grp_list[group]->fstop_indices[entry], &last_value);
	  test = fabs( last_value - target ); 
	}

      system_mess_proc(1,"Event stopping: Number of Newton steps exceeded.");
      status = EXCEED_MAX_ITER;
      }
  else
    {
      system_mess_proc(1,"Propagation to event failed.");
      status = BAD_CROSSING_CONDITION;
    }

  project(integ_cntl->ph_space_dim-1, final_state, integ_cntl->manifold);

  return(status);
}


