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
/*
 * dstool  (Version 1.1) - automatically generated model file
 * Sat Sep 26 11:32:06 1992
 */
#include <model_headers.h>

/*
	u' = v
	v' = u*(beta-u*u)-delta*v+gamma*cos(theta)
	theta' = omega


	Translation table:

		x[0] <--> u
		x[1] <--> v
		x[2] <--> theta
		p[0] <--> gamma
		p[1] <--> delta
		p[2] <--> beta
		p[3] <--> omega
*/

/* function used to define dynamical system */
int duffing_ds_func(f,x,p)
double *f,*x,*p;
{

	f[0] = x[1];
	f[1] = x[0]*(p[2]-x[0]*x[0])-p[1]*x[1]+p[0]*cos(x[2]);
	f[2] = p[3];

	return;
}

/* function used to define aux functions */
/*
int duffing_aux_func(f,x,p)
double *f,*x,*p;
{

	return;
}
*/

/* function used to define jacobian. NOT AUTOMATICALLY GENERATED.
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/
int duffing_jac(m,x,p)
double **m,*x,*p;
{
  m[0][0] = m[0][2] = 0.0;
  m[0][1] = 1.0;
  m[1][0] = p[2] - 3.0*x[0]*x[0];
  m[1][1] = -p[1];
  m[1][2] = -p[0]*sin(x[2]);
  m[2][0] = m[2][1] = m[2][2] = 0.0;
	return;
}


/* function used to define default data */
int duffing_init()
{
	int n_varb=3;
	static char *variable_names[]={"u","v","theta"};
	static double variables[]={0,0,0};
	static double variable_min[]={-2,-2,0};
	static double variable_max[]={2,2,TWOPI};

	static char *indep_varb_name="time";
	double indep_varb_min=0;
	double indep_varb_max=10000;

	int n_param=4;
	static char *parameter_names[]={"gamma","delta","beta","omega"};
	static double parameters[]={0.3,0.2,1,1};
	static double parameter_min[]={-10,-10,-10,-10};
	static double parameter_max[]={10,10,10,10};

	int n_funct=0;
	static char *funct_names[]={"(null)"};
	static double funct_min[]={-10};
	static double funct_max[]={10};

	int manifold_type=PERIODIC;
	static int periodic_varb[]={FALSE,FALSE,TRUE};
	static double period_start[]={0,0,0};
	static double period_end[]={1,0,TWOPI};

	int mapping_toggle=FALSE;
	int inverse_toggle=FALSE;

	int (*def_name)()=duffing_ds_func;
	int (*jac_name)()=duffing_jac;
	int (*aux_func_name)()=NULL;
	int (*inv_name)()=NULL;
	int (*dfdt_name)()=NULL;
	int (*dfdparam_name)()=NULL;

#include <ds_define.c>
}

