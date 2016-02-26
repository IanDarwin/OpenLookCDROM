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

/* ------------------------------------------------------------------------
   Required function used to define the vector field or map.
   The values of the vector field mapping at point x with parameter
   values p are returned in the pre-allocated array f. For vector fields,
   the last components of both f and x are time components. All arrays are
   indexed starting from 0.
   ------------------------------------------------------------------------ */
int user_ds_func(f,x,p)
double *f,*x,*p;
{
}

/* ------------------------------------------------------------------------
   Optional function used to define the Jacobian m at point x with
   parameters p. The matrix m is pre-allocated (by the routine dmatrix);
   At exit, m[i][j] is to be the partial derivative of the i'th component
   of f with respect to the j'th component of x.
   ------------------------------------------------------------------------ */
/*
int user_jac(m,x,p)
double  **m, *x, *p;
{
}
*/

/* ------------------------------------------------------------------------
   Optional function used to define the inverse or approximate inverse y at
   the point x with parameter values p. The array y is pre-allocated.
   ------------------------------------------------------------------------ */
/*
int user_inv(y,x,p)
double    *y,*x,*p;
{
}
*/

/* ------------------------------------------------------------------------
   Optional function used to define aux functions f of the varbiables x
   and parameters p. The array f is pre-allocated. Time is available as the
   last component of x.
   ------------------------------------------------------------------------ */
/*
int user_aux_func(f,x,p)
double *f,*x,*p;
{
}
*/

/* ------------------------------------------------------------------------
   Required procedure to define default data for the dynamical system. NOTE: You
   may change the entries for each variable but PLEASE DO NOT change the list of
   items.  If a variable is unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int user_init()
{
/* ------------ define the dynamical system in this segment --------------- */

int            n_varb=2;                       /* dim of phase space */
static char    *variable_names[]={"x","y"};    /* list of phase varb names */
static double  variables[]={0.,0.};            /* default varb initial values */
static double  variable_min[]={0.,0.};         /* default varb min for display */
static double  variable_max[]={1.,1.};         /* default varb max for display */

static char    *indep_varb_name="time";        /* name of indep variable  */
double         indep_varb_min=0.;              /* default indep varb min for display */
double         indep_varb_max=10000.;          /* default indep varb max for display */

int            n_param=2;                      /* dim of parameter space */
static char    *parameter_names[]={"a","b"};   /* list of param names */
static double  parameters[]={0.,0.};           /* initial parameter values */
static double  parameter_min[]={0.,0.};        /* default param min for display */
static double  parameter_max[]={1.,1.};        /* default param max for display */

int            n_funct=2;                      /* number of user-defined functions */
static char    *funct_names[]={"f1", "f2"};    /* list of funct names; {""} if none */
static double  funct_min[]={0.,0.};            /* default funct min for display */
static double  funct_max[]={1.,1.};            /* default funct max for display */

int            manifold_type=EUCLIDEAN;        /* PERIODIC (a periodic varb) or EUCLIDEAN */
static int     periodic_varb[]={FALSE, FALSE}; /* if PERIODIC, which varbs are periodic? */
static double  period_start[]={0.,0.};         /* if PERIODIC, begin fundamental domain */
static double  period_end[]={1., 1.};          /* if PERIODIC, end of fundamental domain */

int            mapping_toggle=TRUE;            /* this is a map? TRUE or FALSE */
int            inverse_toggle=FALSE;           /* if so, is inverse FALSE, APPROX_INV, */
					       /* or EXPLICIT_INV? FALSE for vec field */

/*  In this section, input NULL or the name of the function which contains... */
int            (*def_name)()=NULL;             /* the eqns of motion */
int            (*jac_name)()=NULL;             /* the jacobian (deriv w.r.t. space) */
int            (*aux_func_name)()=NULL;        /* the auxiliary functions */
int            (*inv_name)()=NULL;             /* the inverse or approx inverse */
int            (*dfdt_name)()=NULL;            /* the deriv w.r.t time */
int            (*dfdparam_name)()=NULL;        /* the derivs w.r.t. parameters */

/* ------------------ end of dynamical system definition ------------------ */
#include <ds_define.c>
}

