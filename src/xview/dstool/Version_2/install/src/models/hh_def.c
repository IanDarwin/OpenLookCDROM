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
Hodgkin Huxley Equations
*/
	
int hh(f,x,p)
double f[],x[],p[];
{
	double am,an,bm,bn,ah,bh,vo,m,n,h,psi(),gna,gk,gl,vna,vk,vl,temp,cur,phi;
	gna = p[0];
        gk = p[1];
        gl = p[2];
        vna = p[3];
        vk = p[4];
        vl = p[5];
        temp = p[6];
        cur = p[7];
        vo = x[0];
        m = x[1];
        n = x[2];
        h = x[3];
        am = psi((vo+25.0)/10.0);
        bm = 4.0*exp(vo/18.0);
        an = 0.1*psi((vo+10.0)/10.0);
        bn = 0.125*exp(vo/80.0);
        ah = 0.07*exp(vo/20.0);
        bh = 1.0/(1.0+exp((vo+30.0)/10.0));
        phi = exp(log(3.0)*(temp-6.3)/10.0);
        f[0] = -(gna*m*m*m*h*(vo-vna)+gk*n*n*n*n*(vo-vk)+gl*(vo-vl)+cur);
        f[1] = phi*((1.0-m)*am-m*bm);
        f[2] = phi*((1.0-n)*an-n*bn);
        f[3] = phi*((1.0-h)*ah-h*bh);
        return(0);

}
double psi(x)
        double x;
        {
	double x2,x4,x6,x8,x10,x12;
        if (fabs(x)<0.1) 
                {x2 = x*x; x4= x2*x2; x6 = x2*x4; x8 = x4*x4; x10 = x4*x6; x12 = x6*x6;
                return(1.0 - x/2.0 + x2/12.0 - x4/720.0 + x6/30240.0 - x8/1209600.0 + x10/47900160.0 -691.0*x12/1307674368000.0);}
        else return(x/(exp(x)-1.0));
        }

/* ------------------------------------------------------------------------
   proc used to define functions of the variable, parameters or derivatives
   ------------------------------------------------------------------------ */
int hh_func(f,x,p)
double f[],x[],p[];
{
  return 0;
}

/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int hh_jac(m,x,p)
double  **m, *x, *p;
{
  return 0;
}

/* ------------------------------------------------------------------------
   proc to define the default data for the dynamical system
   Note: You may change the entries for each variable but
	 DO NOT change the list of items.  If a variable is
	 unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int hh_init()
 {

   /* define the dynamical system in this segment 
      ---------------------------------------------------------------------------------- */
   int	         n_varb=4;				    /* dim of the phase spase */
   static char	 *variable_names[]={"v","m","n","h"};	    /* list of phase varb names */
   static double variables[]={0.1,0.1,0.1,0.1};		    /* initial conditions for the variables */
   static double variable_min[]={-200.,0.,0.,0.};	    /* default varb min for display */
   static double variable_max[]={200.,1.,1.,1.};	    /* default varb max for display */
   static char   *indep_varb_name="time";		    /* name of indep variable  */
   double        indep_varb_min=0.;			    /* default indep varb min r display */
   double        indep_varb_max=100.;			    /* default indep varb max r display */

   int	         n_param=8;				    /* dim of the parameter space */
   static char	 *parameter_names[]={"gna","gk","gl","vna",
				       "vk","vl","temp","cur"}; /* list of parameter names */
   static double parameters[]={120.0,36.0,0.3,-115.0,
				 12.,10.599,6.3,2.0};	    /* initial parameter values */
   static double parameter_min[]={60.,-10.,0.1,-200.,
				    -10.,8.,0.,-10.};	    /* default param min for display */
   static double parameter_max[]={180.,40.,1.,0.,
				    24.,12.,35.,20.};	    /* default param max for display */
   int	         n_funct=0;				    /* number of user-defined functions */
   static char	 *funct_names[]={""};			    /* list of funct names */
   static double funct_min[]={0};			    /* default funct min for display */
   static double funct_max[]={0};			    /* default funct max for display */

   int	         manifold_type = EUCLIDEAN;		    /* EUCLIDEAN or PERIODIC (periodic variables) */
   static int	 periodic_varb[] = {FALSE, FALSE, FALSE, FALSE}; /* if PERIODIC, which variables are periodic */
   static double period_start[] = {0.,0.,0.,0.};	    /* if PERIODIC, beginning of fundamental domain */
   static double period_end[] = {0., 0., 0., 0.};	    /* if PERIODIC, end of fundamental domain */

   int	         mapping_toggle=FALSE;			    /* is this a map? */
   int	         inverse_toggle=FALSE;			    /* if so, is inverse FALSE, APPROX_INV, or EXPLICIT_INV?*/

   /*  In this section, input NULL or the name of the function which contains... */
   int           (*def_name)()=hh;			    /* the eqns of motion */
   int           (*jac_name)()=NULL;			    /* the jacobian (deriv w.r.t. space) */
   int           (*aux_func_name)()=NULL;		    /* the auxiliary functions */
   int           (*inv_name)()=NULL;			    /* the inverse or approx inverse */
   int           (*dfdt_name)()=NULL;			    /* the deriv w.r.t time */
   int           (*dfdparam_name)()=NULL;		    /* the derivs w.r.t. parameters */

   /* end of dynamical system definition 
      --------------------------------------------------------------------------------------- */

#include <ds_define.c>

 }

