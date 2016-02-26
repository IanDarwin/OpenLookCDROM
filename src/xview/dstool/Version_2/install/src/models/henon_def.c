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
  complex henon map
/*
/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
int
  henon(y,x,p)
double          *y,*x,*p;
{
  y[0] = x[0]*x[0]-x[1]*x[1]+p[2]-p[0]*x[2]+p[1]*x[3];
  y[1] = 2*x[0]*x[1]+p[3]-p[1]*x[2]-p[0]*x[3];
  y[2] = x[0];
  y[3] = x[1];
  
}


/* ------------------------------------------------------------------------
   proc used to define inverse or approximate inverse
   ------------------------------------------------------------------------ */
int
  henon_inv(y,x,p)
double          *y,*x,*p;
{
  double norma;
  
  norma = p[0]*p[0]+p[1]*p[1];
  
  y[0] = x[2];
  y[1] = x[3];
  y[2] = (p[0]*(x[2]*x[2]-x[3]*x[3]-x[0]+p[2])+p[1]*(2*x[2]*x[3]+p[3]-x[1]))/norma;
  y[3] = (-p[1]*(x[2]*x[2]-x[3]*x[3]-x[0]+p[2])+p[0]*(2*x[2]*x[3]+p[3]-x[1]))/norma;
}


/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int
  henon_jac(m,x,p)
double          **m,*x,*p;
{

        m[0][0] = 2*x[0];
        m[0][1] = -2*x[1];
	m[0][2] = -p[0];
	m[0][3] = p[1];
	m[1][0] = 2*x[1];
        m[1][1] = 2*x[0];
	m[1][2] = -p[1];
	m[1][3] = -p[0];
	m[2][0] = 1.;
	m[2][1] = 0.;
	m[2][2] = 0.;
	m[2][3] = 0.;
	m[3][0] = 0.;
	m[3][1] = 1.;
	m[3][2] = 0.;
	m[3][3] = 0.;
}


/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int henon_init()
{

  /* define the dynamical system in this segment 
     --------------------------------------------------------------------------- */
  int            n_varb=4;				   /* dim of the phase spase */
  static char	 *variable_names[]={"x_real","x_imag","y_real","y_imag"}; /* list of phase varb names */
  static double	 variables[]={0.,0.,0.,0.};		   /* initial conditions for the variables */
  static double	 variable_min[]={-3.,-3.,-3.,-3.};	   /* default varb min for display */
  static double	 variable_max[]={3.,3.,3.,3.};		   /* default varb max for display */

  static char    *indep_varb_name="iter";		   /* name of indep variable  */
  double         indep_varb_min=0.;			   /* default indep varb min r display */
  double         indep_varb_max=10000.;			   /* default indep varb max r display */
  
  int            n_param=4;				   /* dim of the parameter space */
  static char	 *parameter_names[]={"a_real","a_imag","c_real","c_imag"}; /* list of param names */
  static double	 parameters[]={-0.3,0.,-1.34,0.};	   /* initial parameter values */
  static double	 parameter_min[]={0.,0.,0.,0.};		   /* default param min for display */
  static double	 parameter_max[]={1.,1.,1.,1.};		   /* default param max for display */
  
  int            n_funct=0;				   /* number of user-defined functions */
  static char	 *funct_names[]={""};			   /* list of funct names */
  static double	 funct_min[]={0};			   /* default funct min for display */
  static double	 funct_max[]={0};			   /* default funct max for display */
  
  int            mapping_toggle=TRUE;			   /* is this a map? */
  int            inverse_toggle=EXPLICIT_INV;		   /* if so, is inverse FALSE, APPROX_INV, or EXPLICIT_INV?*/

  int            manifold_type = EUCLIDEAN;		   /* EUCLIDEAN or PERIODIC (periodic variables) */
  static int 	 periodic_varb[] = {FALSE, FALSE};	   /* if PERIODIC, which variables are periodic */
  static double  period_start[] = {0.,0.,0.,0.};	   /* if PERIODIC, beginning of fundamental domain */
  static double  period_end[] = {0.,0.,0.,0.};		   /* if PERIODIC, end of fundamental domain */

  /*  In this section, input NULL or the name of the function which contains... */
  int            (*def_name)()=henon;			   /* the eqns of motion */
  int            (*jac_name)()=henon_jac;		   /* the jacobian (deriv w.r.t. space) */
  int            (*aux_func_name)()=NULL;		   /* the auxiliary functions */
  int            (*inv_name)()=henon_inv;		   /* the inverse or approx inverse */
  int            (*dfdt_name)()=NULL;			   /* the deriv w.r.t time */
  int            (*dfdparam_name)()=NULL;		   /* the derivs w.r.t. parameters */

  /* end of dynamical system definition 
     ------------------------------------------------------------------------------ */

#include <ds_define.c>

}

