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

This program is the property of:

                             Cornell University 
                        Center of Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the following
restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

dstool is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of FITNESS FOR A PARTICULAR PURPOSE.
The software is provided as is without any obligation on the part of Cornell 
faculty, staff or students to assist in its use, correction, modification or
enhancement.

  --------------------------------------------------------------------------------  */
#include <model_headers.h>

/* ------------------------------------------------------------------------
   function used to define the vector field or map
   ------------------------------------------------------------------------ */
int four_param(f,x,p)
double *f,*x,*p;
{

  f[0] = x[1];
  f[1] = -(x[0]*x[0]*x[0]+p[0]*x[0]*x[0]+p[1]*x[0]+p[2])+p[4]*x[1]*(p[3]-x[0]*x[0]);
}

/* ------------------------------------------------------------------------
   function used to define the Jacobian
   ------------------------------------------------------------------------ */
int four_param_jac(m,x,p)
double  **m, *x, *p;
{
}

/* ------------------------------------------------------------------------
   function used to define inverse or approximate inverse
   ------------------------------------------------------------------------ */
int four_param_inv(y,x,p)
double    *y,*x,*p;
{
}

/* ------------------------------------------------------------------------
   function used to define aux functions of the varbs, time, or params
   ------------------------------------------------------------------------ */
int four_param_aux_func(f,x,p)
double *f,*x,*p;
{
  double temp;
 
  temp = p[2]+x[0]*(p[1]+x[0]*(p[0]+x[0]));
  f[0] = temp*temp + x[1]*x[1];
}

/* ------------------------------------------------------------------------
   Procedure to define default data for the dynamical system. NOTE: You
   may change the entries for each variable but DO NOT change the list of
   items.  If a variable is unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int four_param_init()
{
/* ------------ define the dynamical system in this segment --------------- */

int            n_varb=2;                       /* dim of phase space */
static char    *variable_names[]={"x","y"};    /* list of phase varb names */
static double  variables[]={-4,20};            /* default varb initial values */
static double  variable_min[]={-10.,-10.};         /* default varb min for display */
static double  variable_max[]={10.,10.};         /* default varb max for display */

static char    *indep_varb_name="time";        /* name of indep variable  */
double         indep_varb_min=0.;              /* default indep varb min for display */
double         indep_varb_max=10000.;          /* default indep varb max for display */

int            n_param=5;                      /* dim of parameter space */
static char    *parameter_names[]={"r","n","m","b","delta"};   /* list of param names */
static double  parameters[]={1,-3,-3,3,1};           /* initial parameter values */
static double  parameter_min[]={-10,-10,-10,-10,-10};
static double  parameter_max[]={10,10,10,10,10};        /* default param max for display */

int            n_funct=1;                      /* number of user-defined functions */
static char    *funct_names[]={"H"};    /* list of funct names; {""} if none */
static double  funct_min[]={0};            /* default funct min for display */
static double  funct_max[]={10};            /* default funct max for display */

int            manifold_type=EUCLIDEAN;        /* PERIODIC (a periodic varb) or EUCLIDEAN */
static int     periodic_varb[]={FALSE, FALSE}; /* if PERIODIC, which varbs are periodic? */
static double  period_start[]={0.,0.};         /* if PERIODIC, begin fundamental domain */
static double  period_end[]={1., 1.};          /* if PERIODIC, end of fundamental domain */

int            mapping_toggle=FALSE;            /* this is a map? TRUE or FALSE */
int            inverse_toggle=FALSE;           /* if so, is inverse FALSE, APPROX_INV, */
					       /* or EXPLICIT_INV? FALSE for vec field */

/*  In this section, input NULL or the name of the function which contains... */
int            (*def_name)()=four_param;             /* the eqns of motion */
int            (*jac_name)()=NULL;             /* the jacobian (deriv w.r.t. space) */
int            (*aux_func_name)()=four_param_aux_func;        /* the auxiliary functions */
int            (*inv_name)()=NULL;             /* the inverse or approx inverse */
int            (*dfdt_name)()=NULL;            /* the deriv w.r.t time */
int            (*dfdparam_name)()=NULL;        /* the derivs w.r.t. parameters */

/* ------------------ end of dynamical system definition ------------------ */
#include <ds_define.c>
}

