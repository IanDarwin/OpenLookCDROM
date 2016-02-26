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
   problem 2:  This is a 4-dimensional example, specified in: 

	       Vetterling, W.T., Teukolsky, S.A., Press, W.H. and B.P. Flannery, `Numerical Recipes Examples (C)',
	       Cambridge University Press, Cambridge, 1988, pps. 566 ff.

	       is used to illustrate the different behaviors of the fixed-step Runge-Kutta, variable-step
	       Runge-Kutta and Bulirsch-Stoer methods.

		   y1' = -y2
		   y2' = y1 - (1/t) y2
		   y3' = y2 - (2/t) y3
		   y4' = y3 - (3/t) y4

               The stopping critera used is for a fixed number of steps (200).  

   --------------------------------------------------------------------------------------------------------------- */

/* Set up prop_cntl structure for test problem 2 using
   method (RK4, RK4QC, BS or EULER) and data_type (VALUES or ARRAYS) */


problem2( prop_cntl, method, data_type )
struct  Prop_DataS      *prop_cntl;
int			method, data_type;
{
  int			i, j;
  extern int		pb2_fp();
  extern double		bessj0(), bessj1(), bessj();
  static double		state[]={0.0,0.0,0.0,0.0,1.0}, 
			parameters[]={0.0},
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


  int  			prob2_varb_watch_func();

  static int
      prob2_fstop_indices_array[]={0},
      prob2_cross_orient_array[]={PM_and_MP},
      prob2_cross_mode_array[]={SOLVE_STEP}; 

  static double
      prob2_target_values_array[]={0.5}, 
      prob2_target_tols_array[]={0.001};

  static fstop_group
      prob2_var_fstop_group_values = 
  { prob2_varb_watch_func, 1, 
	prob2_fstop_indices_array, prob2_cross_orient_array, prob2_cross_mode_array,
	prob2_target_values_array, prob2_target_tols_array }; 


  if( data_type == VALUES )
     {
      state[0] = bessj0(state[4]);				/* set initial conditions using */
      state[1] = bessj1(state[4]);				/* Bessel function routines     */
      state[2] = bessj(2,state[4]);				/* supplied below.              */
      state[3] = bessj(3,state[4]);

      prop_cntl->function		= pb2_fp;
      prop_cntl->inverse		= NULL;
      prop_cntl->dfdx			= NULL; 
      prop_cntl->dxdt			= NULL;
      prop_cntl->fstop			= NULL;
      prop_cntl->ph_space_dim		= 4 + 1;		/* phase space dim plus time */
      prop_cntl->parameter_dim		= 0;
      prop_cntl->function_dim		= 0;
      prop_cntl->enable_nsteps          = TRUE;
      prop_cntl->enable_fstop           = TRUE;
      prop_cntl->enable_tstop           = TRUE;
      prop_cntl->fixed_step_flag        = TRUE;
      prop_cntl->direction		= FORWARD; 
      prop_cntl->iter_request		= 200;
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
      prop_cntl->time_step		= 0.1;
      prop_cntl->estim_step		= 0.1;
      prop_cntl->final_time		= 20.0;
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
      prop_cntl->fstop_grp_list[0] = &prob2_var_fstop_group_values;

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



/* vector field for test problem 2 */
pb2_fp(f, x, p)
double f[],x[],p[];
{
  double	t = x[4];

  f[0] = -x[1];
  f[1] = x[0] - (1.0/t)*x[1];
  f[2] = x[1] - (2.0/t)*x[2];
  f[3] = x[2] - (3.0/t)*x[3];

  return(0);
}


/* exact solution to test problem 2 */
int
exact_pb2( y, t, p )
double  t, *y, *p;
{
  extern double  bessj0(), bessj1(), bessj();

  y[0] = bessj0(t); 
  y[1] = bessj1(t); 
  y[2] = bessj(2,t);
  y[3] = bessj(3,t);

  return(0);
}



/* substitute for varb_watch_func in compute_orbit.c */
int
    prob2_varb_watch_func(x,p,index,result)
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


/* ---------------------------------------------------------------------------------------

   The following routines are (slightly modified) versions of Bessel function routines
   described in the text:

	Press, W.H., Flannery, B.P., Teukolsky, S.A. and W.I. Vetterling, `Numerical
	Recipes in C', Cambridge University Press, Cambridge, 1988.

   and called by examples codes described in the companion text:

	Vetterling, W.T., Teukolsky, S.A., Press, W.H. and B.P. Flannery, `Numerical
	Recipes Examples (C)', Cambridge University Press, Cambridge, 1988, pps. 566 ff.

   which we include to make the complib_test collection self-contained.

   --------------------------------------------------------------------------------------- */



double bessj0(x)
double x;
{
	double ax,z;
	double xx,y,ans,ans1,ans2;

	if ((ax=fabs(x)) < 8.0) {
		y=x*x;
		ans1=57568490574.0+y*(-13362590354.0+y*(651619640.7
			+y*(-11214424.18+y*(77392.33017+y*(-184.9052456)))));
		ans2=57568490411.0+y*(1029532985.0+y*(9494680.718
			+y*(59272.64853+y*(267.8532712+y*1.0))));
		ans=ans1/ans2;
	} else {
		z=8.0/ax;
		y=z*z;
		xx=ax-0.785398164;
		ans1=1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4
			+y*(-0.2073370639e-5+y*0.2093887211e-6)));
		ans2 = -0.1562499995e-1+y*(0.1430488765e-3
			+y*(-0.6911147651e-5+y*(0.7621095161e-6
			-y*0.934935152e-7)));
		ans=sqrt(0.636619772/ax)*(cos(xx)*ans1-z*sin(xx)*ans2);
	}
	return ans;
}


double bessj1(x)
double x;
{
	double ax,z;
	double xx,y,ans,ans1,ans2;

	if ((ax=fabs(x)) < 8.0) {
		y=x*x;
		ans1=x*(72362614232.0+y*(-7895059235.0+y*(242396853.1
			+y*(-2972611.439+y*(15704.48260+y*(-30.16036606))))));
		ans2=144725228442.0+y*(2300535178.0+y*(18583304.74
			+y*(99447.43394+y*(376.9991397+y*1.0))));
		ans=ans1/ans2;
	} else {
		z=8.0/ax;
		y=z*z;
		xx=ax-2.356194491;
		ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4
			+y*(0.2457520174e-5+y*(-0.240337019e-6))));
		ans2=0.04687499995+y*(-0.2002690873e-3
			+y*(0.8449199096e-5+y*(-0.88228987e-6
			+y*0.105787412e-6)));
		ans=sqrt(0.636619772/ax)*(cos(xx)*ans1-z*sin(xx)*ans2);
		if (x < 0.0) ans = -ans;
	}
	return ans;
}



#define ACC 40.0
#define BIGNO 1.0e10
#define BIGNI 1.0e-10

double bessj(n,x)
int n;
double x;
{
	int j,jsum,m;
	double ax,bj,bjm,bjp,sum,tox,ans;
	double bessj0(),bessj1();

	if (n < 2) 
	   {
	    fprintf(stderr,"Index n less than 2 in BESSJ\n");
	    exit(0);
           }
	ax=fabs(x);
	if (ax == 0.0)
		return 0.0;
	else if (ax > (double) n) {
		tox=2.0/ax;
		bjm=bessj0(ax);
		bj=bessj1(ax);
		for (j=1;j<n;j++) {
			bjp=j*tox*bj-bjm;
			bjm=bj;
			bj=bjp;
		}
		ans=bj;
	} else {
		tox=2.0/ax;
		m=2*((n+(int) sqrt(ACC*n))/2);
		jsum=0;
		bjp=ans=sum=0.0;
		bj=1.0;
		for (j=m;j>0;j--) {
			bjm=j*tox*bj-bjp;
			bjp=bj;
			bj=bjm;
			if (fabs(bj) > BIGNO) {
				bj *= BIGNI;
				bjp *= BIGNI;
				ans *= BIGNI;
				sum *= BIGNI;
			}
			if (jsum) sum += bj;
			jsum=!jsum;
			if (j == n) ans=bjp;
		}
		sum=2.0*sum-bj;
		ans /= sum;
	}
	return  x < 0.0 && n%2 == 1 ? -ans : ans;
}

#undef ACC
#undef BIGNO
#undef BIGNI
