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
#include <prop.h>
#include <complib.h>
#include "complib_test.h"

#ifdef NOT_SUN
#include <not_sun_supplements.h>
#endif

/* ----------------------------------------------------------------------------------------------------------------
   problem 6:  This example is described in:

	       Hahlquist, G. and A. Bjorck, `Numerical Methods,' Prentics-Hall Series in
	       Automatic Computation, New Jersey, 1974, pg. 349.

	       and illustrates the special requirements of integrating stiff ODE's.

               The defining equation is given by:

		   y' = 100 ( sin(t) - y )

		   y(0) = 0

               The closed-form solution for this differential equations is given by:

			  sin(t) - 0.01 cos(t) + 0.01 exp(-100 t)
		   y(t) = ---------------------------------------
				         1.0001

   --------------------------------------------------------------------------------------------------------------- */

/* Set up prop_cntl structure for test problem 2 using
   method (RK4, RK4QC, BS or EULER) and data_type (VALUES or ARRAYS) */


problem6( prop_cntl, method, data_type )
struct  Prop_DataS      *prop_cntl;
int			method, data_type;
{
  int			i, j;
  extern int		pb6_fp();
  static double		state[]={0.0,0.0}, 
			parameters[]={100.0},
  			min[]=NULL,
			max[]=NULL,
			period_start[]=NULL,
			period_end[]=NULL,
			panel_dp_values[]={1.0e-6,1.0e-5,1.0e-10},
			panel_dp_values_bs[]={1.0e-6,1.0e-5,1.0e-10,1.0,1.e-5,0.95,1.2},
  			panel_dp_values_rkqc[]={1.0e-6,1.0e-5,1.0e-10,1.0,1.e-5,-0.2,-0.25,0.06666,0.9};  
  static int		panel_int_values[]={15},
			panel_int_values_bs[]={15,7},
			panel_choice_values[]={0},
			periodic_varb[]=NULL;


  int  			prob6_varb_watch_func();

  static int
      prob6_fstop_indices_array[]={0},
      prob6_cross_orient_array[]={PM_and_MP},
      prob6_cross_mode_array[]={SOLVE_STEP}; 

  static double
      prob6_target_values_array[]={0.5}, 
      prob6_target_tols_array[]={0.001};

  static fstop_group
      prob6_var_fstop_group_values = 
  { prob6_varb_watch_func, 1, 
	prob6_fstop_indices_array, prob6_cross_orient_array, prob6_cross_mode_array,
	prob6_target_values_array, prob6_target_tols_array }; 


  if( data_type == VALUES )
     {
      prop_cntl->function		= pb6_fp;
      prop_cntl->inverse		= NULL;
      prop_cntl->dfdx			= NULL; 
      prop_cntl->dxdt			= NULL;
      prop_cntl->fstop			= NULL;
      prop_cntl->ph_space_dim		= 1 + 1;		/* phase space dim plus time */
      prop_cntl->parameter_dim		= 1;
      prop_cntl->function_dim		= 0;
      prop_cntl->enable_nsteps          = TRUE;
      prop_cntl->enable_fstop           = TRUE;
      prop_cntl->enable_tstop           = TRUE;
      prop_cntl->fixed_step_flag        = TRUE;
      prop_cntl->direction		= FORWARD; 
      prop_cntl->iter_request		= 1000;
      prop_cntl->iterations		= 0;
      prop_cntl->prop_mode		= PROP_TF;
      prop_cntl->prop_segsize		= 100;
      prop_cntl->symbol			= 0;
      prop_cntl->table_color		= 0;
      prop_cntl->sys_color		= 0;
      prop_cntl->mapping_flag		= FALSE;
      prop_cntl->inverse_flag		= FALSE;
      prop_cntl->f_iter			= 0;
      prop_cntl->f_skip			= 1;
      prop_cntl->start_to_save		= 0;
      prop_cntl->time_step		= 0.025;		/* stable stepsize for RK4 */
      prop_cntl->time_step		= 0.030;		/* UNSTABLE stepsize for RK4 !! */
      prop_cntl->estim_step		= 0.01;
      prop_cntl->final_time		= 3.0;
      prop_cntl->diverg_cutoff		= 1.e20;		/* for unstable case, must set limit higher than default */
     }
  else
     {
      prop_cntl->manifold->type		= EUCLIDEAN;
      prop_cntl->panel_int_values	= panel_int_values;
      prop_cntl->panel_choice_values	= panel_choice_values;
      prop_cntl->panel_dp_values	= panel_dp_values;
      for(i=0; i<prop_cntl->ph_space_dim; i++) prop_cntl->state[i] = state[i]; 
      for(i=0; i<prop_cntl->parameter_dim; i++) prop_cntl->parameters[i] = parameters[i];

      prop_cntl->num_fstop_grps = 1;
      prop_cntl->fstop_grp_list = (fstop_group **) calloc(prop_cntl->num_fstop_grps,
							sizeof(fstop_group *));
      prop_cntl->fstop_grp_list[0] = &prob6_var_fstop_group_values;

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



/* vector field for test problem 6 */
pb6_fp(f, x, p)
double f[],x[],p[];
{
  double	t = x[1];
  extern double sin();

  f[0] = p[0]*(sin(t)-x[0]);

  return(0);
}


/* exact solution to test problem 6 */
int
exact_pb6( y, t, p )
double  t, *y, *p;
{
  extern double sin(), cos(), exp();

  y[0] = (sin(t) - 0.01*cos(t) + 0.01*exp(-100.0*t) )/1.001; 
 
  return(0);
}



/* substitute for varb_watch_func in compute_orbit.c */
int
    prob6_varb_watch_func(x,p,index,result)
int	
    index;
double  
    *result,x[],p[];

{
    int
	n_varbs = 1,
	status = 0;

	if( index < 0 || index >= n_varbs )
	   status = -1;
	else *result = x[index];

	return(status);
}

