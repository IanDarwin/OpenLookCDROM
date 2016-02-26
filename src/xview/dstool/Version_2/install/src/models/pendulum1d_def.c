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

/* 1 dof damped driven pendulum */
/*
        This vector field describes the motion of a damped
        and periodically forced pendulum.  
        The vector field is given by
	q' = p
	p' = -Damping*p-sin(q) + F_Ampl*sin(F_Freq*t) + Torque

	Translation table:

		x[0] <--> q
		x[1] <--> p
		x[2] <--> t
		p[0] <--> Damping
		p[1] <--> F_Ampl	(amplitude of AC forcing)
		p[2] <--> F_Freq	(frequency of AC forcing)
		p[3] <--> Torque	(amplitude of DC forcing)
*/
/* ------------------------------------------------------------------------
   proc used to define the vector field 
   ------------------------------------------------------------------------ */
int pendulum1d(f,x,p)
double f[],x[],p[];
{
  f[0] = x[1];
  f[1] = -p[0]*x[1]  - sin( x[0] ) + p[1]*sin(p[2]*x[2]) + p[3];
  return(0);

}

/* ------------------------------------------------------------------------
   proc used to define functions of the variable, parameters or derivatives
   ------------------------------------------------------------------------ */
int pendulum1d_func(f,x,p)
double f[],x[],p[];
{
  f[0] = sin( 0.5*p[2]*x[2] );	/* this is zero for every multiple of 2 Pi/ w */
				/* So use event stopping on this to "strobe" at */
				/* forcing frequency */
}
/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int pendulum1d_jac(m,x,p)
double	**m, *x, *p;
{
  m[0][0] = 0.0;
  m[0][1] = 1.0;
  m[1][0] = -cos( x[0] );
  m[1][1] = -p[0];
}

/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int pendulum1d_init()
{

  /* define the dynamical system in this segment 
     ------------------------------------------------------------------------------------------- */
  int	 n_varb=2;					  /* dim of the phase spase */
  static char	*variable_names[]={"q","p"};       	  /* list of phase varb names */
  static double	variables[]={0.1,0.1};			  /* initial conditions for the variables */
  static double	variable_min[]={-3.2,-5.0};		  /* default varb min for display */
  static double	variable_max[]={3.2,5.0};		  /* default varb max for display */
  static char   *indep_varb_name="time";		  /* name of indep variable  */
  double indep_varb_min=0.;				  /* default indep varb min r display */
  double indep_varb_max=100.;				  /* default indep varb max r display */

  int	 n_param=4;					  /* dim of the parameter space */
  static char	*parameter_names[]={"Damping","F_Ampl","F_Freq", "Torque"};  /* list of param names */
  static double	parameters[]={0.1,0.1,1,.1};		  /* initial parameter values */
  static double	parameter_min[]={0,0.,0.,0.};		  /* default param min for display */
  static double	parameter_max[]={1.,1.,1.,1.};		  /* default param max for display */

  int	 n_funct=1;					  /* number of user-defined functions */
  static char	*funct_names[]={"Period"};		  /* list of param names */
  static double	funct_min[]={-1};			  /* default varb max for display */
  static double	funct_max[]={1};			  /* default varb max for display */

  int	 manifold_type = PERIODIC;			  /* EUCLIDEAN or PERIODIC (periodic variables) */
  static int	periodic_varb[] = {TRUE, FALSE};	  /* if PERIODIC, which variables are periodic */
  static double period_start[] = {-PI,0.};		  /* if PERIODIC, beginning of fundamental domain */
  static double period_end[] = {PI, 0.};		  /* if PERIODIC, end of fundamental domain */

  int 	 mapping_toggle=FALSE;				  /* is this a map? */
  int	 inverse_toggle=FALSE;				  /* if so, is inverse FALSE, APPROX_INV, or EXPLICIT_INV?*/

  /*  In this section, input NULL or the name of the function which contains... */
  int           (*def_name)()=pendulum1d;		  /* the eqns of motion */
  int           (*jac_name)()=pendulum1d_jac;		  /* the jacobian (deriv w.r.t. space) */
  int           (*aux_func_name)()=pendulum1d_func;	  /* the auxiliary functions */
  int           (*inv_name)()=NULL;			  /* the inverse or approx inverse */
  int           (*dfdt_name)()=NULL;			  /* the deriv w.r.t time */
  int           (*dfdparam_name)()=NULL;		  /* the derivs w.r.t. parameters */

/* ------------------ end of dynamical system definition ------------------ */
#include <ds_define.c>
}

