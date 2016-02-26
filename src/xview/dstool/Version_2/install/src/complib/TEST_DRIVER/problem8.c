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
#include <prop.h>
#include <complib.h>
#include "complib_test.h"

#ifdef NOT_SUN
#include <not_sun_supplements.h>
#endif

/* ----------------------------------------------------------------------------------------------------------------

   problem 8:  A Period Vector Field: The Nonlinear Pendulum

               This dynamical system is defined on S^1xRxS^1:

	       theta' = v
	       v' = epsilon cos(2*PI*psi) - sin(theta) - damping v
	       psi' = omega / (2*PI)

	       We seek to stop on the boundary of the first periodic variable, theta=PI.
	       

   --------------------------------------------------------------------------------------------------------------- */

/* Set up prop_cntl structure for test problem 2 using
   method (RK4, RK4QC, BS or EULER) and data_type (VALUES or ARRAYS) */
    

problem8( prop_cntl, method, data_type )
struct  Prop_DataS      *prop_cntl;
int			method, data_type;
{
  int			i, j;
  extern int		pb8_fp();
  static double		state[]={-3.0,4.0,0.0,0.0}, 
			parameters[]={0.0,0.0,0.1},
  			min[]=NULL,
			max[]=NULL,
			period_start[]={-PI,0.,0.},
			period_end[]={PI,0.,1.},
			panel_dp_values[]={1.0e-6,1.0e-5,1.0e-10},
			panel_dp_values_bs[]={1.0e-6,1.0e-5,1.0e-10,0.5,1.e-5,0.95,1.2},
  			panel_dp_values_rkqc[]={1.0e-6,1.0e-5,1.0e-10,1.0,1.e-5,-0.2,-0.25,0.06666,0.9};  
  static int		panel_int_values[]={15},
			panel_int_values_bs[]={15,7},
			panel_choice_values[]={0},
			periodic_varb[]={TRUE,FALSE,TRUE};


  int  			prob8_varb_watch_func();

  static int
      prob8_fstop_indices_array[]={0},
      prob8_cross_orient_array[]={PM_and_MP},
      prob8_cross_mode_array[]={SOLVE_STEP}; 

  static double
      prob8_target_values_array[]={PI}, 
      prob8_target_tols_array[]={0.00001};

  static fstop_group
      prob8_var_fstop_group_values = 
  { prob8_varb_watch_func, 1, 
	prob8_fstop_indices_array, prob8_cross_orient_array, prob8_cross_mode_array,
	prob8_target_values_array, prob8_target_tols_array }; 


  if( data_type == VALUES )
     {
      prop_cntl->function		= pb8_fp;
      prop_cntl->inverse		= NULL;
      prop_cntl->dfdx			= NULL; 
      prop_cntl->dxdt			= NULL;
      prop_cntl->fstop			= NULL;
      prop_cntl->ph_space_dim		= 3 + 1;		/* phase space dim plus time */
      prop_cntl->parameter_dim		= 3;
      prop_cntl->function_dim		= 0;
      prop_cntl->enable_nsteps          = TRUE;
      prop_cntl->enable_fstop           = TRUE;
      prop_cntl->enable_tstop           = TRUE;
      prop_cntl->fixed_step_flag        = TRUE;
      prop_cntl->direction		= FORWARD; 
      prop_cntl->iter_request		= 500;
      prop_cntl->iterations		= 0;
      prop_cntl->prop_mode		= PROP_FSTOP;
      prop_cntl->prop_segsize		= 100;
      prop_cntl->symbol			= 0;
      prop_cntl->table_color		= 0;
      prop_cntl->sys_color		= 0;
      prop_cntl->mapping_flag		= FALSE;
      prop_cntl->inverse_flag		= FALSE;
      prop_cntl->f_iter			= 1;
      prop_cntl->f_skip			= 1;
      prop_cntl->start_to_save		= 0;
      prop_cntl->time_step		= 0.01;
      prop_cntl->estim_step		= 0.01;
      prop_cntl->final_time		= 1.5;
      prop_cntl->diverg_cutoff		= 1.e10;
      prop_cntl->panel_option           = 0;
     }
  else
     {
      prop_cntl->manifold->type		= PERIODIC;
      prop_cntl->panel_int_values	= panel_int_values;
      prop_cntl->panel_choice_values	= panel_choice_values;
      prop_cntl->panel_dp_values	= panel_dp_values;
      for(i=0; i<prop_cntl->ph_space_dim; i++) prop_cntl->state[i] = state[i]; 
      for(i=0; i<prop_cntl->parameter_dim; i++) prop_cntl->parameters[i] = parameters[i];
  
      prop_cntl->num_fstop_grps = 1;
      prop_cntl->fstop_grp_list = (fstop_group **) calloc(prop_cntl->num_fstop_grps,
							sizeof(fstop_group *));
      prop_cntl->fstop_grp_list[0] = &prob8_var_fstop_group_values;

      for(i=0; i< nint( (double) (sizeof(min)/sizeof(double)) ); i++) prop_cntl->min[i] = min[i];
      for(i=0; i< nint( (double) (sizeof(max)/sizeof(double)) ); i++) prop_cntl->max[i] = max[i];	
      for(i=0; i< nint( (double) (sizeof(periodic_varb)/sizeof(int)) ); i++)
	prop_cntl->manifold->periodic_varb[i] = periodic_varb[i]; 
      for(i=0; i< nint( (double) (sizeof(period_start)/sizeof(double)) ); i++)
	prop_cntl->manifold->period_start[i] = period_start[i]; 
      for(i=0; i< nint( (double) (sizeof(period_end)/sizeof(double)) ); i++)
	prop_cntl->manifold->period_end[i] = period_end[i];


      switch (method)
        {
         case RK4:
         case EULER:
         case RK4QC:
            prop_cntl->fixed_step_flag        = FALSE;
            prop_cntl->panel_dp_values	= panel_dp_values_rkqc;
    	    break;
	 case BS:
	    prop_cntl->fixed_step_flag        = FALSE;
	    prop_cntl->panel_dp_values  = panel_dp_values_bs;
	    prop_cntl->panel_int_values = panel_int_values_bs;
	    break;
        }
     }
}



/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
/* nonlinear pendulum */
int pb8_fp(f,x,p)
double f[],x[],p[];
{


  f[0] = x[1];
  f[1] = p[0]*cos(TWOPI*x[2]) - sin( x[0] ) - p[2] * x[1];
  f[2] = p[1]/TWOPI;
  return(0);

}




/* substitute for varb_watch_func in compute_orbit.c */
int
    prob8_varb_watch_func(x,p,index,result)
int	
    index;
double  
    *result,x[],p[];

{
    int
	n_varbs = 3,
	status = 0;

	if( index < 0 || index >= n_varbs )
	   status = -1;
	else *result = x[index];

	return(status);
}

