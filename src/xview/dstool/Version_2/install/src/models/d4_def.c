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
        A dissipative vector field which is symmetric
        under D4, the symmetry group of the square.  
        The vector field on R^4 = {(x,y,z,w)} is given by 
                x' = y
                y' = x(mu-(x^2 + z^2)) + delta x z^2
                       + epsilon( y (x^2 + z^2) + nu y
                       + Axzw x(x y + z w) + Ayz2 y z^2
                z' = w
                w' = w(mu-(x^2 + z^2)) + delta z x^2
                       + epsilon( w (x^2 + z^2) + nu w
                       + Axzw z (x y + z w) + Ayz2 w x^2
        The parameters are:
                mu, delta, epsilon, nu, Axz2, Ayz2.

        This system has two auxiliary functions:
        Energy = 0.5(y^2 + w^2) - 0.5 mu(x^2+z^2) + 
                    0.25(x^2 + z^2)^2 - 0.5 delta x^2 z^2
        AngMom = yz - xw
        Note that in general the energy and angular momentum are 
        not preserved.

   See Armbruster, Guckenheimer, and Kim, [1989], Physics Letters A, 140, 416-420.
   The system here is the special case a = -1, c = 1 of AGK.

 */

/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */

int d4(f,x,p)
double f[],x[],p[];
{
        double  v0sq,v2sq;
	
	v0sq = x[0] * x[0];
	v2sq = x[2] * x[2];
	f[0] = x[1];
	f[1] = x[0] * (p[0]-(v0sq+v2sq)) + p[1] * x[0] * v2sq 
               + p[2] * ( (v0sq+v2sq) * x[1] + p[3] * x[1] + p[4] * x[0]*(x[0]*x[1]+x[2]*x[3]) + p[5] * x[1] * v2sq);
	f[2] = x[3];
	f[3] = x[2] *(p[0] - (v0sq+v2sq))+p[1]*x[2]*v0sq
	       + p[2] * ( (v0sq+v2sq) * x[3] + p[3] * x[3] + p[4] * x[2]*(x[0]*x[1]+x[2]*x[3]) + p[5] * x[3] * v0sq);

}

/* ------------------------------------------------------------------------
   proc used to define functions of the variable, parameters or derivatives
   ------------------------------------------------------------------------ */
int d4_func(f,x,p)
double f[],x[],p[];
{
        double v0sq,v2sq;

        v0sq = x[0] * x[0];
        v2sq = x[2] * x[2];
        f[0] = 0.5 * (( x[1] * x[1] + x[3] * x[3]) - p[0]*(v0sq+v2sq)
                + 0.5 * (v0sq+v2sq)*(v0sq+v2sq) - p[1] * v0sq* v2sq);
        f[1] =  x[1] * x[2] - x[0] * x[3];	
}

/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int d4_jac(m,x,p)
double	**m, *x, *p;
{
}

/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int d4_init()
{

  /* define the dynamical system in this segment 
     ---------------------------------------------------------------------- */
  int            n_varb=4;					/* dim of phase space */
  static char    *variable_names[]={"x","y","z","w"};		/* list of phase varb names */
  static double  variables[]={1.,1.5,0.,1.83};			/* default varb initial values */
  static double  variable_min[]={-5.,-5.,-5.,-5.};		/* default varb min for display */
  static double  variable_max[]={5.,5.,5.,5.};			/* default varb max for display */

  static char    *indep_varb_name="time";			/* name of indep variable  */
  double         indep_varb_min=0.;				/* default indep varb min for display */
  double         indep_varb_max=1000.;				/* default indep varb max for display */

  int            n_param=6;					/* dim of parameter space */
  static char    *parameter_names[]={"mu","delta","epsilon","nu","Axzw","Ayz2"}; /* list of param names */
  static double  parameters[]={2.,0.95,0.,-3.52,1.,0.};		/* initial parameter values */
  static double  parameter_min[]={-5.,-5.,-5.,-5.,-5.,-5.};	/* default param min for display */
  static double  parameter_max[]={5.,5.,5.,5.,5.,5.};		/* default param max for display */

  int            n_funct=2;					/* number of user-defined functions */
  static char    *funct_names[]={"Energy", "AngMom"};		/* list of funct names */
  static double  funct_min[]={-1.,-1.};				/* default funct min for display */
  static double  funct_max[]={1.,1.};				/* default funct max for display */

  int            manifold_type=EUCLIDEAN;			/* PERIODIC (a periodic varb) or EUCLIDEAN */
  static int     periodic_varb[]={FALSE, FALSE, FALSE, FALSE};	/* if PERIODIC, which varbs are periodic? */
  static double  period_start[]={0.,0.,0.,0.};			/* if PERIODIC, begin fundamental domain */
  static double  period_end[]={1.,1.,1.,1.};			/* if PERIODIC, end of fundamental domain */

  int            mapping_toggle=FALSE;				/* is this a map? TRUE or FALSE */
  int            inverse_toggle=FALSE;				/* if so, is inverse FALSE, APPROX_INV,
								   or EXPLICIT_INV? FALSE for vec field */
  /*  In this section, input NULL or the name of the function which contains... */
  int            (*def_name)()=d4;				/* the eqns of motion */
  int            (*jac_name)()=d4_jac;				/* the jacobian (deriv w.r.t. space) */
  int            (*aux_func_name)()=d4_func;			/* the auxiliary functions */
  int            (*inv_name)()=NULL;				/* the inverse or approx inverse */
  int            (*dfdt_name)()=NULL;				/* the deriv w.r.t time */
  int            (*dfdparam_name)()=NULL;			/* the derivs w.r.t. parameters */

  /* end of dynamical system definition 
     ----------------------------------------------------------------------- */

#include <ds_define.c>

}

