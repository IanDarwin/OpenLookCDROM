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

   problem 7:  An Iterated Mapping:  The Standard Map
               This dynamical system is defined as the iterated application of a diffeomorphism of
	       the 2-torus to itself:

		  f(x,y) = (x - p[0] - y,  (1/p[1])*( y+(p[2]/2 Pi)*sin( 2 Pi x ) )

	       We seek the 15th iterate of (0.5,0.646696840644) under this map.  The solution
	       of this problem is:

		  f^{15}(x0,y0) = (x0,y0)

               since these initial conditions lie on a period-5 periodic obit for this mapping.

   --------------------------------------------------------------------------------------------------------------- */

/* Set up prop_cntl structure for test problem 2 using
   method (RK4, RK4QC, BS or EULER) and data_type (VALUES or ARRAYS) */
    

problem7( prop_cntl, method, data_type )
struct  Prop_DataS      *prop_cntl;
int			method, data_type;
{
  int			i, j;
  extern int		pb7_fp(), standard_inv();
  static double		state[]={0.5,0.646696840644,0.0}, 
			parameters[]={0.0,0.97,1.0},
  			min[]=NULL,
			max[]=NULL,
			period_start[]={0.,0.},
			period_end[]={1.,1.},
			panel_dp_values[]={1.0e-6,1.0e-5,1.0e-10},
			panel_dp_values_bs[]={1.0e-6,1.0e-5,1.0e-10,1.0,1.e-5,0.95,1.2},
  			panel_dp_values_rkqc[]={1.0e-6,1.0e-5,1.0e-10,1.0,1.e-5,-0.2,-0.25,0.06666,0.9};  
  static int		panel_int_values[]={15},
			panel_int_values_bs[]={15,7},
			panel_choice_values[]={0},
			periodic_varb[]={TRUE,TRUE};


  int  			prob7_varb_watch_func();

  static int
      prob7_fstop_indices_array[]={0},
      prob7_cross_orient_array[]={PM_and_MP},
      prob7_cross_mode_array[]={SOLVE_STEP}; 

  static double
      prob7_target_values_array[]={0.5}, 
      prob7_target_tols_array[]={0.001};

  static fstop_group
      prob7_var_fstop_group_values = 
  { prob7_varb_watch_func, 1, 
	prob7_fstop_indices_array, prob7_cross_orient_array, prob7_cross_mode_array,
	prob7_target_values_array, prob7_target_tols_array }; 


  if( data_type == VALUES )
     {
      prop_cntl->function		= pb7_fp;
      prop_cntl->inverse		= standard_inv;
      prop_cntl->dfdx			= NULL; 
      prop_cntl->dxdt			= NULL;
      prop_cntl->fstop			= NULL;
      prop_cntl->ph_space_dim		= 2 + 1;		/* phase space dim plus time */
      prop_cntl->parameter_dim		= 3;
      prop_cntl->function_dim		= 0;
      prop_cntl->enable_nsteps          = TRUE;
      prop_cntl->enable_fstop           = TRUE;
      prop_cntl->enable_tstop           = TRUE;
      prop_cntl->fixed_step_flag        = TRUE;
      prop_cntl->direction		= FORWARD; 
      prop_cntl->iter_request		= 15;
      prop_cntl->iterations		= 0;
      prop_cntl->prop_mode		= PROP_TF;
      prop_cntl->prop_segsize		= 100;
      prop_cntl->symbol			= 0;
      prop_cntl->table_color		= 0;
      prop_cntl->sys_color		= 0;
      prop_cntl->mapping_flag		= TRUE;
      prop_cntl->inverse_flag		= TRUE;
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
      prop_cntl->fstop_grp_list[0] = &prob7_var_fstop_group_values;

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
/* Diss. Standard Map */
int pb7_fp(f,x,p)
double f[],x[],p[];
{

  f[1] = p[2] * x[1] - p[1] / TWOPI * sin(TWOPI * x[0]);
  f[0] = x[0] + p[0] + f[1];

  return(0);
}


/* ------------------------------------------------------------------------
   proc used to define inverse or approximate inverse
   ------------------------------------------------------------------------ */
int standard_inv(f,x,p)
double *f, *x, *p;
{
  f[0] = x[0] - p[0] -x[1];
  f[1] = (x[1] + p[1]/TWOPI * sin(TWOPI * f[0]))/p[2];

  return(0);
}


/* exact solution to test problem 7 */
int
exact_pb7( y, t, p )
double  t, *y, *p;
{
  return(0);
}

/* substitute for varb_watch_func in compute_orbit.c */
int
    prob7_varb_watch_func(x,p,index,result)
int	
    index;
double  
    *result,x[],p[];

{
    int
	n_varbs = 2,
	status = 0;

	if( index < 0 || index >= n_varbs )
	   status = -1;
	else *result = x[index];

	return(status);
}


