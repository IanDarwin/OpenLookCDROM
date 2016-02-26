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
        A mapping on the two-dimensional torus, ie, the unit
        square with edges identified.
        The mapping is defined by 

	x' = x+epsilon*(wx+cos(TWOPI*x)+a*cos(TWOPI*y))
	y' = y+epsilon*(wy+sin(TWOPI*x)+a*sin(TWOPI*y))

	Translation table:

		x[0] <--> x
		x[1] <--> y
		p[0] <--> wx
		p[1] <--> wy
		p[2] <--> a
		p[3] <--> epsilon
*/

/* function used to define dynamical system */
int simple_torus_map_ds_func(f,x,p)
double *f,*x,*p;
{

	f[0] = x[0]+p[3]*(p[0]+cos(TWOPI*x[0])+p[2]*cos(TWOPI*x[1]));
	f[1] = x[1]+p[3]*(p[1]+sin(TWOPI*x[0])+p[2]*sin(TWOPI*x[1]));

	return;
}

/* function used to define aux functions */
/*
int simple_torus_map_aux_func(f,x,p)
double *f,*x,*p;
{

	return;
}
*/

/* function used to define jacobian. 
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/

int simple_torus_map_jac(m,x,p)
double **m,*x,*p;
{
  m[0][0] = 1 - p[3]*TWOPI*sin(TWOPI*x[0]);
  m[0][1] = -p[3]*p[2]*TWOPI*sin(TWOPI*x[1]);
  m[1][0] = p[3]*TWOPI*cos(TWOPI*x[0]);
  m[1][1] = 1 + p[3]*p[2]*TWOPI*cos(TWOPI*x[1]);
	return;
}


/* function used to define default data */
int simple_torus_map_init()
{
	int n_varb=2;
	static char *variable_names[]={"x","y"};
	static double variables[]={0,0};
	static double variable_min[]={0,0};
	static double variable_max[]={1,1};

	static char *indep_varb_name="iter";
	double indep_varb_min=0;
	double indep_varb_max=10000;

	int n_param=4;
	static char *parameter_names[]={"wx","wy","a","epsilon"};
	static double parameters[]={1,.1,0.1,0.05};
	static double parameter_min[]={-1.2,-1.1,0,0};
	static double parameter_max[]={1.2,1.1,1,1};

	int n_funct=0;
	static char *funct_names[]={"(null)"};
	static double funct_min[]={-10};
	static double funct_max[]={10};

	int manifold_type=PERIODIC;
	static int periodic_varb[]={TRUE,TRUE};
	static double period_start[]={0,0};
	static double period_end[]={1,1};

	int mapping_toggle=TRUE;
	int inverse_toggle=FALSE;

	int (*def_name)()=simple_torus_map_ds_func;
	int (*jac_name)()=simple_torus_map_jac;
	int (*aux_func_name)()=NULL;
	int (*inv_name)()=NULL;
	int (*dfdt_name)()=NULL;
	int (*dfdparam_name)()=NULL;

#include <ds_define.c>
}

