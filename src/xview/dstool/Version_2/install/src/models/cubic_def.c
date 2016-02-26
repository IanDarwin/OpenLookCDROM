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

/* planar cubic system 
   
   x' = y
   y' = -( x^3 + r x^2 + n x + m ) + y( b - x^2 )

*/

/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
int cubic(f,x,p)
double f[],x[],p[];
{
  f[0] = x[1];
  f[1] = -( x[0]*x[0]*x[0] + p[0]*x[0]*x[0] + p[1]*x[0] + p[2] ) + x[1]*(p[3] - x[0]*x[0]);

  return(NO_ERROR);

}


/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int cubic_jac(m,x,p)
double	**m, *x, *p;
{
  m[0][0] = 0.0;
  m[0][1] = 1.0;
  m[1][0] = -( 3.0*x[0]*x[0] + 2.0*p[0]*x[0] + p[1] ) - 2.0*x[0]*x[1];
  m[1][1] = p[3] - x[0]*x[0];
}

/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int cubic_init()
{

  /* define the dynamical system in this segment 
     ------------------------------------------------------------------------------------------- */
  int	 n_varb=2;					  /* dim of the phase spase */
  static char	*variable_names[]={"x","y"};		  /* list of phase varb names */
  static double	variables[]={0.1,0.1};			  /* initial conditions for the variables */
  static double	variable_min[]={-5.0,-5.0};		  /* default varb min for display */
  static double	variable_max[]={5.0,5.0};		  /* default varb max for display */
  static char   *indep_varb_name="time";		  /* name of indep variable  */
  double indep_varb_min=0.;				  /* default indep varb min r display */
  double indep_varb_max=100.;				  /* default indep varb max r display */

  int	 n_param=4;					  /* dim of the parameter space */
  static char	*parameter_names[]={"r","n","m","b"};     /* list of param names */
  static double	parameters[]={0.1,0.1,0.1,0.1};           /* initial parameter values */
  static double	parameter_min[]={-5.,-5.,-5.,-5.};        /* default param min for display */
  static double	parameter_max[]={5.,5.,5.,5.};            /* default param max for display */

  int	 n_funct=0;					  /* number of user-defined functions */
  static char	*funct_names[]={""};			  /* list of param names */
  static double	funct_min[]={0};			  /* default varb max for display */
  static double	funct_max[]={0};			  /* default varb max for display */

  int	 manifold_type = EUCLIDEAN;			  /* EUCLIDEAN or PERIODIC (periodic variables) */
  static int	periodic_varb[] = {FALSE, FALSE};	  /* if PERIODIC, which variables are periodic */
  static double period_start[] = {0.,0.};		  /* if PERIODIC, beginning of fundamental domain */
  static double period_end[] = {0., 0.};		  /* if PERIODIC, end of fundamental domain */

  int 	 mapping_toggle=FALSE;				  /* is this a map? */
  int	 inverse_toggle=FALSE;				  /* if so, is inverse FALSE, APPROX_INV, or EXPLICIT_INV?*/

  /*  In this section, input NULL or the name of the function which contains... */
  int           (*def_name)()=cubic;			  /* the eqns of motion */
  int           (*jac_name)()=cubic_jac;		  /* the jacobian (deriv w.r.t. space) */
  int           (*aux_func_name)()=NULL;		  /* the auxiliary functions */
  int           (*inv_name)()=NULL;			  /* the inverse or approx inverse */
  int           (*dfdt_name)()=NULL;			  /* the deriv w.r.t time */
  int           (*dfdparam_name)()=NULL;		  /* the derivs w.r.t. parameters */

  /* end of dynamical system definition 
     --------------------------------------------------------------------------------------------- */
#include <ds_define.c>

}

