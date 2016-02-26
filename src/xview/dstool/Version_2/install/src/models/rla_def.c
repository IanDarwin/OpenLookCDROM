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
#include  <model_headers.h>

/* ------------------------------------------------------------------------
   function used to define the vector field or map
   ------------------------------------------------------------------------ */
int rlag(f,x,p)
double *f,*x,*p;
{
        double am,an,bm,bn,ah,bh,vo,m,n,h,gca,gkca,gna,gk,gl,vca;
	double vna,vk,vl,b,rho,tz,pv,ca,z,kca,zv,lambda,zb,expma,ma;
	double expha1,expha2,ha,gka,hai,har;
        extern double exp(),log();
 	b = 2.0;
        gna = p[1];
        gk = p[2];
        gl = p[3];
        gca = p[4];
        gkca = p[5];
        vna = p[6];
        vk = p[7];
        vl = p[8];
        vca = p[9];
        kca = p[10];
	gka = p[0];
        vo = x[0];
        ca = x[1];
        n = x[2];
        h = x[3];
        z = x[4];
	ha = x[5];
am = (127.0/105.0*vo+201.0/7.0)/(10.0-10.0*exp(-201.0/70.0-127.0/1050.0*vo));
bm = 4.0*exp(-188.0/63.0-127.0/1890.0*vo);
an = (127.0/105.0*vo+166.0/7.0)/(100.0-100.0*exp(-83.0/35.0-127.0/1050.0*vo));  
bn = exp(-59.0/140.0-127.0/8400.0*vo)/8.0;
ah = 7.0/100.0*exp(-94.0/35.0-127.0/2100.0*vo);
bh = 1.0/(1.0+exp(-83.0/35.0-127.0/1050.0*vo));
zv = 1.0/(1.0+exp(-0.15*(vo+50.0)));
ma = 1/(1+exp(-(vo+12.0)/26));
hai = 1/(1+exp((vo+62.0)/6));
        m = am/(am+bm);
        f[0] = -(gna*m*m*m*h*(vo-vna)+gca*z*(vo-vca)/(0.5+ca)+gk*n*n*n*n*(vo-vk)+gkca*(vo-vk)*ca/(0.5+ca)+50.0*gka*ma*ma*ma*ha*(vo-vk)+gl*(vo-vl));
        f[1] = 0.003*(kca*z*(vca-vo)/(1.0+2.0*ca) -ca);
        f[2] = 0.8*((1.0-n)*an-n*bn);
        f[3] = 0.8*((1.0-h)*ah-h*bh);
        f[4] = (zv -z)/23.5;
	f[5] = (hai - ha);
}

/* ------------------------------------------------------------------------
   function used to define the Jacobian
   ------------------------------------------------------------------------ */
/* 
int rlag_jac(m,x,p)
double  **m, *x, *p;
{
}
*/
/* ------------------------------------------------------------------------
   function used to define inverse or approximate inverse
   ------------------------------------------------------------------------ */
/*
int rlag_inv(y,x,p)
double    *y,*x,*p;
{
}
*/
/* ------------------------------------------------------------------------
   function used to define aux functions of the varbs, time, or params
   ------------------------------------------------------------------------ */
/*
int rlag_aux_func(f,x,p)
double *f,*x,*p;
{
}
*/
/* ------------------------------------------------------------------------
   Procedure to define default data for the dynamical system. NOTE: You
   may change the entries for each variable but DO NOT change the list of
   items.  If a variable is unused, NULL or zero the entry, as appropriate.
   ------------------------------------------------------------------------ */
int rlag_init()
{
/* ------------ define the dynamical system in this segment --------------- */

int            n_varb=6;                       /* dim of phase space */
static char    *variable_names[]={"v","ca","n","h","z","ha"};    /* list of phase varb names */
static double  variables[]={-50.,0.5,0.5,0.5,0.5,0.5};            /* default varb initial values */
static double  variable_min[]={-60.,0.,0.,0.,0.,0.};         /* default varb min for display */
static double  variable_max[]={30.,1.,1.,1.,1.,1.};         /* default varb max for display */

static char    *indep_varb_name="time";        /* name of indep variable  */
double         indep_varb_min=0.;              /* default indep varb min for display */
double         indep_varb_max=10000.;          /* default indep varb max for display */

int            n_param=11;                      /* dim of parameter space */
static char    *parameter_names[]={"gka","gna","gk","gl","gca","gkca","vna","vk","vl","vca","kca"};   /* list of param names */
static double  parameters[]={2., 15., 8., 0.0854, 0.04, 0.25, 30.0, -75.0, -40.0, 140.0, 0.0078};           /* initial parameter values */
static double  parameter_min[]={0.,0.,0.,0.,0.,0.,0.,-100.,-50.,0.,0.};        /* default param min for display */
static double  parameter_max[]={15.,100.,15.,1.,1.,1.,50.,0.,0.,150.,1.};        /* default param max for display */

int            n_funct=0;                      /* number of user-defined functions */
static char    *funct_names[]={"f1"};    /* list of funct names; {""} if none */
static double  funct_min[]={0.};            /* default funct min for display */
static double  funct_max[]={1.};            /* default funct max for display */

int            manifold_type=EUCLIDEAN;        /* PERIODIC (a periodic varb) or EUCLIDEAN */
static int     periodic_varb[]={0}; /* if PERIODIC, which varbs are periodic? */
static double  period_start[]={1.};         /* if PERIODIC, begin fundamental domain */
static double  period_end[]={1.};          /* if PERIODIC, end of fundamental domain */

int            mapping_toggle=FALSE;            /* this is a map? TRUE or FALSE */
int            inverse_toggle=FALSE;           /* if so, is inverse FALSE, APPROX_INV, */
					       /* or EXPLICIT_INV? FALSE for vec field */

/*  In this section, input NULL or the name of the function which contains... */
int            (*def_name)()=rlag;             /* the eqns of motion */
int            (*jac_name)()=NULL;             /* the jacobian (deriv w.r.t. space) */
int            (*aux_func_name)()=NULL;        /* the auxiliary functions */
int            (*inv_name)()=NULL;             /* the inverse or approx inverse */
int            (*dfdt_name)()=NULL;            /* the deriv w.r.t time */
int            (*dfdparam_name)()=NULL;        /* the derivs w.r.t. parameters */

/* ------------------ end of dynamical system definition ------------------ */
#include <ds_define.c>
}

