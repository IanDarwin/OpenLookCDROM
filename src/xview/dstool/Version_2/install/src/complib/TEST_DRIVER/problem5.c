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
   problem 5:  The phase portrait for this vector field contains a separatrix, the orbit for which can be
	       expressed, in closed-form, in terms of sech() and tanh().
	       
               The defining equations are given by:

		   y1' = y2
		   y2' = -p[0] ( 1 - (y1)^2 ) 

		   y1(0) = -2     y2(0) = 0

               The closed-form solution for the connecting orbit is given by:

		   y1(t) = 1 - 3 ( sech( t/sqrt(2) )  )^2
                   y2(t) = 3 sqrt(2) ( sech( t/sqrt(2) )  )^2 tanh( t/sqrt(2) )


   --------------------------------------------------------------------------------------------------------------- */

/* Set up prop_cntl structure for test problem 2 using
   method (RK4, RK4QC, BS or EULER) and data_type (VALUES or ARRAYS) */


problem5( prop_cntl, method, data_type )
struct  Prop_DataS      *prop_cntl;
int			method, data_type;
{
  int			i, j;
  extern int		pb5_fp();
  static double		state[]={1.414213562,0.0,0.0}, 
			parameters[]={1.0},
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


  int  			prob5_varb_watch_func();

  static int
      num_fstop_indices = 1,	
      prob5_fstop_indices_array[]={0},
      prob5_cross_orient_array[]={PM_and_MP},
      prob5_cross_mode_array[]={POST_STEP}; 

  static double
      prob5_target_values_array[]={0.01}, 
      prob5_target_tols_array[]={0.001};

  static fstop_group
      prob5_var_fstop_group_values = 
  { prob5_varb_watch_func, 1, 
	prob5_fstop_indices_array, prob5_cross_orient_array, prob5_cross_mode_array,
	prob5_target_values_array, prob5_target_tols_array }; 


  if( data_type == VALUES )
     {
      prop_cntl->function		= pb5_fp;
      prop_cntl->inverse		= NULL;
      prop_cntl->dfdx			= NULL; 
      prop_cntl->dxdt			= NULL;
      prop_cntl->fstop			= NULL;
      prop_cntl->ph_space_dim		= 2 + 1;		/* phase space dim plus time */
      prop_cntl->parameter_dim		= 1;
      prop_cntl->function_dim		= 0;
      prop_cntl->enable_nsteps          = TRUE;
      prop_cntl->enable_fstop           = TRUE;
      prop_cntl->enable_tstop           = TRUE;
      prop_cntl->fixed_step_flag        = TRUE;
      prop_cntl->direction		= BACKWARD; 
      prop_cntl->iter_request		= 100;
      prop_cntl->iterations		= 0;
      prop_cntl->prop_mode		= PROP_FSTOP;
      prop_cntl->prop_segsize		= 100;
      prop_cntl->symbol			= 0;
      prop_cntl->table_color		= 0;
      prop_cntl->sys_color		= 0;
      prop_cntl->mapping_flag		= FALSE;
      prop_cntl->inverse_flag		= FALSE;
      prop_cntl->f_iter			= 0;
      prop_cntl->f_skip			= 1;
      prop_cntl->start_to_save		= 0;
      prop_cntl->time_step		= 0.1;
      prop_cntl->estim_step		= 0.1;
      prop_cntl->final_time		= 10.0;
      prop_cntl->diverg_cutoff		= 1.e10;
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
      prop_cntl->fstop_grp_list[0] = &prob5_var_fstop_group_values;

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



/* vector field for test problem 5 */
pb5_fp(f, x, p)
double f[],x[],p[];
{
  double	t = x[2];

  f[0] = x[1];
  f[1] = (p[0] - x[0]*x[0])*x[0];

  return(0);
}


/* ------------------------------------------------------------------------
   proc used to define functions of the variable, parameters or derivatives
   ------------------------------------------------------------------------ */
int pb5_func(f,x,p)
double f[],x[],p[];
{

  f[0] = sqrt( x[0]*x[0] + x[1]*x[1] );
  return 0;
}


/* exact solution to test problem 5 */
int
exact_pb5( y, t, p )
double  t, *y, *p;
{
  extern double	sech(), tanh();
  double	sqrt2;

  sqrt2 = sqrt(2.0);

  y[0] = sqrt2*sech(t);
  y[1] = -sqrt2*sech(t)*tanh(t);

  return(0);
}



/* substitute for varb_watch_func in compute_orbit.c */
int
    prob5_varb_watch_func(x,p,index,result)
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


/* -------------------------------------------------------------------
 
   utility routines used to supply the required hyperbolic trig 
   functions sech() and tanh()

   ------------------------------------------------------------------- */


double
sech( x )
double	x;
{
  double   value;

  value = 2.0/( exp(x) + exp(-x) );
  return( value );
}


double
tanh( x )
double	x;
{
  double   value;

  value = ( exp(x) - exp(-x) )/( exp(x) + exp(-x) );
  return( value );
}

