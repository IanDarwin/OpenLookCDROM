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
#include <model_headers.h>

/*
        A map from the cylinder to itself defined by
                x -> x + w + (y_new)
                y -> b y - k/(2 pi) sin(2 pi x)
        This map has an explicit inverse given by 
                x -> x - w - y
                y -> (y + k/(2 pi) sin(2 pi x_new))/b
        The parameters are  w,  k,  and  b.
        The variable  x  is periodic with period 1.

*/
/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
/* Diss. Standard Map */
int standard(f,x,p)
double f[],x[],p[];
{
	f[1] = p[2] * x[1] - p[1] / TWOPI * sin(TWOPI * x[0]);
	f[0] = x[0] + p[0] + f[1];
}

/* ------------------------------------------------------------------------
   proc used to define inverse or approximate inverse
   ------------------------------------------------------------------------ */
int standard_inv(f,x,p)
double *f, *x, *p;
{
  f[0] = x[0] - p[0] -x[1]; 
  f[1] = (x[1] + p[1]/TWOPI * sin(TWOPI * f[0]))/p[2];
}

/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int
  standard_jac(m,x,p)
double          **m,*x,*p;
{
        double temp;

        temp =  p[1] * cos( TWOPI*x[0] );
        m[0][0] = 1. - temp;
        m[0][1] = p[2];
        m[1][0] = -temp;
        m[1][1] = p[2];
}


/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int standard_init()
{

  /* define the dynamical system in this segment 
     --------------------------------------------------------------------------------------- */
  int	        n_varb=2;				  /* dim of the phase spase */
  static char	*variable_names[]={"x","y"};		  /* list of phase varb names */
  static double	variables[]={0.5,0.5};			  /* initial conditions for the variables */
  static double	variable_min[]={0.,0.,0.,0.};		  /* default varb min for display */
  static double	variable_max[]={1.,1.,1.,1.};		  /* default varb max for display */

  static char   *indep_varb_name="iter";		  /* name of indep variable  */
  double        indep_varb_min=0.;			  /* default indep varb min r display */
  double        indep_varb_max=10000.;			  /* default indep varb max r display */

  int	        n_param=3;				  /* dim of the parameter space */
  static char	*parameter_names[]={"w","k","b"};	  /* list of param names */
  static double	parameters[]={0.,0.97,1.0};		  /* initial parameter values */
  static double	parameter_min[]={0.,0.,0.};		  /* default param min for display */
  static double	parameter_max[]={1.,1.,1.};		  /* default param max for display */

  int	        n_funct=0;				  /* number of user-defined functions */  
  static char	*funct_names[]={""};			  /* list of funct names */
  static double	funct_min[]={0};			  /* default funct min for display */
  static double	funct_max[]={0};			  /* default funct max for display */

  int	        mapping_toggle=TRUE;			  /* is this a map? */
  int	        inverse_toggle=EXPLICIT_INV;		  /* if so, is inverse FALSE, APPROX_INV, or EXPLICIT_INV?*/

  int	        manifold_type = PERIODIC;		  /* EUCLIDEAN or PERIODIC (periodic variables) */
  static int	periodic_varb[] = {TRUE, FALSE};	  /* if PERIODIC, which variables are periodic */
  static double period_start[] = {0.,0.};		  /* if PERIODIC, beginning of fundamental domain */
  static double period_end[] = {1., 1.};		  /* if PERIODIC, end of fundamental domain */

  /*  In this section, input NULL or the name of the function which contains... */
  int           (*def_name)()=standard;			  /* the eqns of motion */
  int           (*jac_name)()=standard_jac;		  /* the jacobian (deriv w.r.t. space) */
  int           (*aux_func_name)()=NULL;		  /* the auxiliary functions */
  int           (*inv_name)()=standard_inv;		  /* the inverse or approx inverse */
  int           (*dfdt_name)()=NULL;			  /* the deriv w.r.t time */
  int           (*dfdparam_name)()=NULL;		  /* the derivs w.r.t. parameters */


  /* end of dynamical system definition 
     --------------------------------------------------------------------------------------- */

#include <ds_define.c>

}

