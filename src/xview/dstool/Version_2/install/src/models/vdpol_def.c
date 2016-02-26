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

/* van der pol system
        This vector field describes the motion of the Van der Pol
        oscillator.  See Guckenheimer/Holmes and references within
        for a theoretical discussion of this system's dynamics.
        The vector field is given by
                x' = y 
                y' = alpha( 1 -x^2) y - x + beta cos(omega t)
        The parameters are  alpha,  beta, and  omega.


 */

/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
int vdpol(f,x,p)
double f[],x[],p[];
{

       	f[0] = x[1];
	f[1] = p[0]*(1.0-x[0]*x[0])*x[1]-x[0]+p[1]*cos(p[2]*x[2]); /* time driven */
        return(0);

}

/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int vdpol_jac(m,x,p)
double  **m, *x, *p;
{

  m[0][0] = 0.0;
  m[0][1] = 1.0;
  m[1][0] = -2.0*p[0]*x[0]*x[1] - 1.0;
  m[1][1] = p[0]*(1.0-x[0]*x[0]);
}

/* ------------------------------------------------------------------------
   proc used to define df/dt
   ------------------------------------------------------------------------ */
int vdpol_dfdt(y,x,p)
double  *y, *x, *p;
{

  y[0] = 0.0;	
  y[1] = p[1]*p[2]*sin(p[2]*x[2]);
}

/* ------------------------------------------------------------------------
   proc used to define functions of the variable, parameters or derivatives
   ------------------------------------------------------------------------ */
int vdpol_func(f,x,p)
double f[],x[],p[];
{
}



/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int vdpol_init()
{

  /* define the dynamical system in this segment 
     --------------------------------------------------------------------------------------- */
  int	        n_varb=2;				    /* dim of the phase spase */
  static char	*variable_names[]={"x","y"};		    /* list of phase varb names */
  static double	variables[]={0.,0.};			    /* initial conditions for the variables */
  static double	variable_min[]={-3.0,-3.0};		    /* default varb min for display */
  static double	variable_max[]={3.0,3.0};		    /* default varb max for display */

  int	        n_param=3;				    /* dim of the parameter space */
  static char	*parameter_names[]={"alpha","beta","omega"}; /* list of param names */
  static double	parameters[]={1.0,0.0,1.};		    /* initial parameter values */
  static double	parameter_min[]={0.,0.,0.};		    /* default param min for display */
  static double	parameter_max[]={1.,1.,1.};		    /* default param max for display */

  static char   *indep_varb_name="time";		    /* name of indep variable  */
  double        indep_varb_min=0.;			    /* default indep varb min r display */
  double        indep_varb_max=100.;			    /* default indep varb max r display */

  static int	n_funct=0;				    /* number of user-defined functions */
  static char	*funct_names[]={""};			    /* list of param names */
  static double	funct_min[]={0};			    /* default varb max for display */
  static double	funct_max[]={0};			    /* default varb max for display */

  static int	mapping_toggle=FALSE;			    /* is this a map? */
  static int	inverse_toggle=FALSE;			    /* if so, is inverse FALSE, APPROX_INV, or EXPLICIT_INV?*/

  static int	manifold_type = EUCLIDEAN;		    /* EUCLIDEAN or PERIODIC (periodic variables) */
  static int	periodic_varb[] = {FALSE, FALSE};	    /* if PERIODIC, which variables are periodic */
  static double period_start[] = {0.,0.};		    /* if PERIODIC, beginning of fundamental domain */
  static double period_end[] = {0., 0.};		    /* if PERIODIC, end of fundamental domain */

  /*  In this section, input NULL or the name of the function which contains... */
  int           (*def_name)()=vdpol;			    /* the eqns of motion */
  int           (*jac_name)()=vdpol_jac;		    /* the jacobian (deriv w.r.t. space) */
  int           (*aux_func_name)()=NULL;		    /* the auxiliary functions */
  int           (*inv_name)()=NULL;			    /* the inverse or approx inverse */
  int           (*dfdt_name)()=vdpol_dfdt;		    /* the deriv w.r.t time */
  int           (*dfdparam_name)()=NULL;		    /* the derivs w.r.t. parameters */

  /* end of dynamical system definition 
     --------------------------------------------------------------------------------------- */

#include <ds_define.c>

}

