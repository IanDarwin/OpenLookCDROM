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
#include <math.h>
/*
        A mapping on the two-dimensional torus, ie, the unit
        square with edges identified.
        The mapping is defined by a nonlinear perturbation of
        a translation:
                f(x,y) = ( f1(x,y), f2(x,y) )  where
                f1(x,y)= x + wx - a(asymm)/(2 pi) sin(2 pi y);
                f2(x,y)= y + wy - a/(2 pi asymm) sin(2 pi x);

        The parameters are  wx,  wy,  a, and  asymm.

        The variables  x  and  y   are periodic with period 1.
*/

/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
/* Kim-Ostlund strongly coupled torus map */

int ko(f,x,p)
double f[],x[],p[];
{

	f[0] = x[0] + p[0] - p[2] * p[3] / TWOPI * sin(TWOPI * x[1]);
	f[1] = x[1] + p[1] - p[2] / p[3] / TWOPI * sin(TWOPI * x[0]);
}

/* ------------------------------------------------------------------------
   proc used to define functions of the variable, parameters or derivatives
   ------------------------------------------------------------------------ */

int ko_func(f,x,p)
double f[],x[],p[];
{
  return 0;
}

/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int ko_jac(m,x,p)
double  **m, *x, *p;
{
  
  m[0][0] = 1.;
  m[0][1] = -p[2] * p[3] * cos(TWOPI * x[1]);
  m[1][0] = -p[2] / p[3] * cos(TWOPI * x[0]);
  m[1][1] = 1.;
}

/* ------------------------------------------------------------------------
   proc used to define inverse or approximate inverse
   ------------------------------------------------------------------------ */
int ko_approx_inv(y,x,p)
double          *y,*x,*p;
{
  y[0] = x[0] - p[0];
  y[1] = x[1] - p[1];
}


/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
   DO NOT change the list of items.  If a variable is
   unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int ko_init()
{
  
  /* define the dynamical system in this segment 
     --------------------------------------------------------------------------------------- */
  int	        n_varb=2;			       /* dim of the phase spase */
  static char	*variable_names[]={"x","y"};	       /* list of phase varb names */
  static double	variables[]={0.,0.};		       /* initial conditions for the variables */
  static double	variable_min[]={0.,0.};		       /* default varb min for display */
  static double	variable_max[]={1.,1.};		       /* default varb max for display */
  static char   *indep_varb_name="time";	       /* name of indep variable  */
  double        indep_varb_min=0.;		       /* default indep varb min for display */
  double        indep_varb_max=10000.;		       /* default indep varb max for display */

  int	        n_param=4;			       /* dim of the parameter space */
  static char	*parameter_names[]={"wx","wy","a","asymm"}; /* list of param names */
  static double	parameters[]={0.6,0.805,0.7,1.0};      /* initial parameter values */
  static double	parameter_min[]={0.,0.,0.,0.};	       /* default param min for display */
  static double	parameter_max[]={1.,1.,1.,1.};	       /* default param max for display */

  int	        n_funct=0;			       /* number of user-defined functions */
  static char	*funct_names[]={""};		       /* list of funct names */
  static double	funct_min[]={0};		       /* default funct min for display */
  static double	funct_max[]={1};		       /* default funct max for display */

  int	        manifold_type = PERIODIC;	       /* EUCLIDEAN or PERIODIC (periodic variables) */
  static int	periodic_varb[] = {TRUE, TRUE};	       /* if PERIODIC, which variables are periodic */
  static double period_start[] = {0.,0.};	       /* if PERIODIC, beginning of fundamental domain */
  static double period_end[] = {1., 1.};	       /* if PERIODIC, end of fundamental domain */

  int	        mapping_toggle=TRUE;		       /* is this a map? */
  int	        inverse_toggle=APPROX_INV;	       /* if so, is inverse FALSE, APPROX_INV, or EXPLICIT_INV?*/

  /*  In this section, input NULL or the name of the function which contains... */
  int           (*def_name)()=ko;		       /* the eqns of motion */
  int           (*jac_name)()=ko_jac;		       /* the jacobian (deriv w.r.t. space) */
  int           (*aux_func_name)()=ko_func;	       /* the auxiliary functions */
  int           (*inv_name)()=ko_approx_inv;	       /* the inverse or approx inverse */
  int           (*dfdt_name)()=NULL;		       /* the deriv w.r.t time */
  int           (*dfdparam_name)()=NULL;	       /* the derivs w.r.t. parameters */

  /* end of dynamical system definition 
     --------------------------------------------------------------------------------------- */

#include <ds_define.c>

}

