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
/*  ------------------------------------------------------------------------------  
Reference: Guckenheimer and Mahalov, Phys. Rev. Lett. 68, 2257-2260 (1992)  
--------------------------------------------------------------------------------  */
#include <model_headers.h>

/*
	x' = -y (lambda + epsilon - xsq) - delta x
	y' =  x (lambda - epsilon - xsq) - delta y
where
	xsq = x*x + y*y

	Translation table:

		x[0] <--> x
		x[1] <--> y
		p[0] <--> lambda
		p[1] <--> epsilon
		p[2] <--> delta
*/
/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
int symm_red(f,x,p)
double f[],x[],p[];
{
double xsq;
xsq = x[0]*x[0]+x[1]*x[1];
  f[0] = -1.0*(p[0] + p[1] -xsq)*x[1] - p[2]*x[0];
  f[1] = (p[0] -p[1] - xsq)*x[0] - p[2]*x[1];
  return(0);

}

/* function used to define jacobian. 
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/
int symm_red_jac(m,x,p)
double **m,*x,*p;
{
  m[0][0] = 2.*x[0]*x[1] - p[2];
  m[0][1] = -p[0] - p[1] + x[0]*x[0] + 3.*x[1]*x[1];
  m[1][0] =  p[0] - p[1] - 3.*x[0]*x[0] - x[1]*x[1];
  m[1][1] = -2.*x[0]*x[1] - p[2];
	return;
}


/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int symm_red_init()
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

  int	 n_param=3;					  /* dim of the parameter space */
  static char	*parameter_names[]={"lambda","epsilon","delta"}; /* list of param names */
  static double	parameters[]={0.1,1.0,0.0};	  /* initial parameter values */
  static double	parameter_min[]={-5.,-5.,-5.};	  /* default param min for display */
  static double	parameter_max[]={5.,5.,5.};	  /* default param max for display */

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
  int           (*def_name)()=symm_red;			  /* the eqns of motion */
  int           (*jac_name)()=symm_red_jac;		  /* the jacobian (deriv w.r.t. space) */
  int           (*aux_func_name)()=NULL;		  /* the auxiliary functions */
  int           (*inv_name)()=NULL;			  /* the inverse or approx inverse */
  int           (*dfdt_name)()=NULL;			  /* the deriv w.r.t time */
  int           (*dfdparam_name)()=NULL;		  /* the derivs w.r.t. parameters */

  /* end of dynamical system definition 
     --------------------------------------------------------------------------------------------- */
#include <ds_define.c>

}


