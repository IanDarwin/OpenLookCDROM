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
#include <complib.h>
#include <iterate.h>
#include <math.h>

int  jac_trans[]={ANALYTIC, FORW_DIFF, CEN_DIFF};

/*
  iterate() propagates a trajectory in a specified direction.  It
  propagate for a fixed number of steps OR until some condition is satisfied.
*/

int
iterate(iter_cntl)
struct  Prop_DataS     *iter_cntl;
{
  int 		i, n, jacobian_switch, status = 0,
  	        inv_failed = TRUE,
  		v_dim = iter_cntl->ph_space_dim-1,
  		newt_iter, mc_iter, 
		inverse_type = iter_cntl->inverse_flag,		/* EXPLICIT_INV, APPROX_INV, or NULL */
		inverse_guess,					/* method being used: APPROX_INV or MONTE_CARLO */
		stop_flag = FALSE,
  		plot_switch = FALSE,
  		count = 0,				/* max number of iterator steps */
  		start = 0,				/* point to start plotting */
		skip_start,				/* remaining number of plottable points to skip */
		save_count,				/* index into traj_seg to write the next point */
                delay_flag = TRUE,
                start_to_sv,
		interrupt();
  double	* x, *v, *h,
		**jac_matrix,**dmatrix(),
		tolx, tolf, fd_step;
  int *stop_status, *stop_action, *ivector();
  
  jac_matrix = dmatrix(0, v_dim-1, 0, v_dim-1);
  x = iter_cntl->workspace;				/* x[ph_space_dim] */
  v = x + iter_cntl->ph_space_dim;			/* v[ph_space_dim] */
  h = v + iter_cntl->ph_space_dim;			/* h[ph_space_dim-1] */
  stop_status = ivector(0, get_total_fstop(iter_cntl)-1);
  stop_action = ivector(0, get_total_fstop(iter_cntl)-1);

  if( iter_cntl->jacobian_flag )
     jacobian_switch = jac_trans[iter_cntl->panel_option];
  else jacobian_switch = jac_trans[iter_cntl->panel_option+1];

  /* eliminated overloading of inverse_guess by adding inverse_type -
     ahb 11/25/91 */

  /* NOTE APPROX_INV used to indicate both a property of a dynamical system
     ("approximate inverse defined") and a way of computing inverses
     ("use approximate inverse rather than Monte Carlo")    */

  if (inverse_type != EXPLICIT_INV){
      if ((inverse_type == APPROX_INV) &&
	  /* Magic Number interpretation of second prop panel choice field -
	     means APPROX_INV rather than MONTE_CARLO has been chosen here.
	   */
	  (iter_cntl->panel_choice_values[0] == 0)) {
	  inverse_guess = APPROX_INV;
	  mc_iter = 1;
      }
      else {
	  inverse_guess = MONTE_CARLO;
	  mc_iter = iter_cntl->panel_int_values[0];
      }
      newt_iter = iter_cntl->panel_int_values[1];
      fd_step = iter_cntl->panel_dp_values[0];
      tolx = iter_cntl->panel_dp_values[1];
      tolf = iter_cntl->panel_dp_values[2];
    }

  for ( i=0; i < v_dim; i++ ) h[i] = fd_step;	  /* for now, h(i) is the same for all i */

  for ( i=0; i < iter_cntl->ph_space_dim; i++ )
    x[i] = iter_cntl->state[i];
  for ( i=0; i < v_dim; i++ )								
    if( ! finite(x[i]) || x[i] > iter_cntl->diverg_cutoff || x[i] < -iter_cntl->diverg_cutoff ) 		
      {											
	system_mess_proc(1,"Orbits appear to diverge off to an infinity! Stop!");	
	iter_cntl->iterations = 0;
	free_dmatrix(jac_matrix, 0, v_dim-1, 0, v_dim-1 );
	free_ivector(stop_action, 0, get_total_fstop(iter_cntl)-1);
	free_ivector(stop_status, 0, get_total_fstop(iter_cntl)-1);
	return ( -2 );		/* change this to +2? fjw */
      }

  if( iter_cntl->prop_segsize > 0 ) plot_switch = TRUE;

  start_to_sv = iter_cntl->start_to_save;

  if (start_to_sv==0)
    {
      skip_start=0;
      save_count=1;
      count = iter_cntl->iter_request;
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
      count = iter_cntl->iter_request+skip_start;
    }
     
  /* determines when to start checking for stopping condition */
  if (iter_cntl->prop_mode == PROP_FSTOP)
    {
      if (delay_flag) n = count + 1 - start_to_sv*iter_cntl->f_skip;
      else n = count+1;
      if (n>count)
	stop_flag = prop_fstop_init(iter_cntl, stop_action, stop_status, x );
    }
  
  while( count>0 && !stop_flag)
    {
      if ( iter_cntl->direction == FORWARD )	  			/* forwards iteration */
	iter_forw(iter_cntl->function, iter_cntl->f_iter, 
		  iter_cntl->traj_segment[save_count], x, 
		  iter_cntl->parameters, iter_cntl->ph_space_dim, 
		  iter_cntl->time_step, v, iter_cntl->manifold);
      else					 			/* backwards iteration */
	{   
	  if ( inverse_type == EXPLICIT_INV ) 				/* explicit inverse */ 
	    iter_forw( iter_cntl->inverse, iter_cntl->f_iter, 
		      iter_cntl->traj_segment[save_count], x, 
		      iter_cntl->parameters, iter_cntl->ph_space_dim, 
		      iter_cntl->time_step, v, iter_cntl->manifold);
	  else					  	/* implicit inverse; mc_iter = 1 for APPROX_INV */
	    {
	      inv_failed = TRUE;
	      for( i=0; i<mc_iter && inv_failed; i++)
		{
		  if ( inverse_guess == APPROX_INV ) 			/* get guess using approx inv */
		    iter_forw(iter_cntl->inverse, iter_cntl->f_iter, 
			      iter_cntl->traj_segment[save_count], x, 
			      iter_cntl->parameters, iter_cntl->ph_space_dim, 
			      iter_cntl->time_step, v, 
			      iter_cntl->manifold); 
		  else				  		/* get monte-carlo guess */
		    {
		      iter_cntl->traj_segment[save_count][v_dim] = x[v_dim] - (double)iter_cntl->f_iter; /* fjw 8/14/92 */
		      rnd_vector(v_dim, iter_cntl->traj_segment[save_count], iter_cntl->min, iter_cntl->max);
		    }
		  inv_failed = implicit_iter(iter_cntl->function, iter_cntl->dfdx, 
					     jacobian_switch, newt_iter, iter_cntl->f_iter, 
					     jac_matrix, iter_cntl->traj_segment[save_count], x,
					     iter_cntl->parameters, iter_cntl->ph_space_dim, 
					     iter_cntl->time_step, h, tolx, tolf, 
					     iter_cntl->manifold, v, iter_cntl->i_wkspace );
		}
	      if ( inv_failed )
		{
		  if ( inv_failed == NO_CONVERGENCE )
		    system_mess_proc(1,"Implicit backwards iteration failed. No convergence within Newton's method.");
		  else if (inv_failed == SINGULAR )
		    system_mess_proc(1, "Implicit backwards iteration failed. Singular matrix in Newton's method");
                  if (plot_switch && save_count-start>0)
		    iter_cntl->plot_traj(iter_cntl, start, save_count-1, v);
		  iter_cntl->iterations = save_count-1;
		  free_dmatrix(jac_matrix, 0, v_dim-1, 0, v_dim-1 );
		  free_ivector(stop_action, 0, get_total_fstop(iter_cntl)-1);
		  free_ivector(stop_status, 0, get_total_fstop(iter_cntl)-1);
		  return ( 2 ); 			/* is this OK? - paw */
		}
	    }
	}

      for ( i=0; i < v_dim; i++ )							
	{
	  if (! finite(iter_cntl->traj_segment[save_count][i]) ||

	      iter_cntl->traj_segment[save_count][i] > iter_cntl->diverg_cutoff || 
	      iter_cntl->traj_segment[save_count][i] < -iter_cntl->diverg_cutoff) 
	    {											
	      system_mess_proc(1,"Orbits appear to diverge off to an infinity! Stop!");	
	      if (plot_switch && save_count-start >0)
		iter_cntl->plot_traj(iter_cntl, start, save_count-1, v);
	      iter_cntl->iterations = save_count-1;
	      free_dmatrix(jac_matrix, 0, v_dim-1, 0, v_dim-1 );
	      free_ivector(stop_action, 0, get_total_fstop(iter_cntl)-1);
	      free_ivector(stop_status, 0, get_total_fstop(iter_cntl)-1);
	      return ( 2 );	/* error codes? paw */
	    }
	}

      if(iter_cntl->prop_mode == PROP_FSTOP)
	{
	  if (count < n)
	    stop_flag = prop_fstop_test( iter_cntl, stop_action, stop_status,
					iter_cntl->traj_segment[save_count]);
	  else if (count == n)
	    stop_flag = prop_fstop_init( iter_cntl, stop_action, stop_status,
					iter_cntl->traj_segment[save_count] );
	}
      
      for ( i=0; i < iter_cntl->ph_space_dim; i++ )
	x[i] = iter_cntl->traj_segment[save_count][i];

      /* so we have a valid new point */
      count--;
      if (skip_start>0)
	{
	  skip_start--;
	  stop_flag = FALSE;
	}
      else save_count++;

      /* have we saved enough to plot some points? */
      if (plot_switch && save_count-start >= iter_cntl->prop_segsize)
	{
          iter_cntl->plot_traj(iter_cntl, start, save_count-1, v);
	  start = save_count;
	}

      /* has the user tried to stop us? */
      if(!stop_flag && interrupt() )
	{
	 stop_flag = TRUE;
	 status = -2;
       }
    }

  if(plot_switch && save_count-start>0)
      iter_cntl->plot_traj(iter_cntl, start, save_count-1, v);

  iter_cntl->iterations = save_count-1;

  free_dmatrix(jac_matrix, 0, v_dim-1, 0, v_dim-1 );
  free_ivector(stop_action, 0, get_total_fstop(iter_cntl)-1);
  free_ivector(stop_status, 0, get_total_fstop(iter_cntl)-1);

  return ( status );	
}








