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
 * dstool  (Floor Version) - automatically generated model file
 * Patrick A. Worfolk, 313 Sage Hall, 255-4195
 * Fri Sep 25 13:52:18 1992
 */
#include <model_headers.h>

/*
	x' = mu*x*(1-x)


	Translation table:

		x[0] <--> x
		p[0] <--> mu
*/

/* function used to define dynamical system */
int logistic_ds_func(f,x,p)
double *f,*x,*p;
{

	f[0] = p[0]*x[0]*(1-x[0]);

	return;
}

/* function used to define aux functions */
/*
int logistic_aux_func(f,x,p)
double *f,*x,*p;
{

	return;
}
*/

/* function used to define jacobian. NOT AUTOMATICALLY GENERATED.
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/
int logistic_jac(m,x,p)
double **m,*x,*p;
{
	m[0][0] = p[0] * (1 - 2 * x[0]);

	return;
}


/* function used to define default data */
int logistic_init()
{
	int n_varb=1;
	static char *variable_names[]={"x"};
	static double variables[]={0.5};
	static double variable_min[]={0};
	static double variable_max[]={1};

	static char *indep_varb_name="iter";
	double indep_varb_min=0;
	double indep_varb_max=10000;

	int n_param=1;
	static char *parameter_names[]={"mu"};
	static double parameters[]={3};
	static double parameter_min[]={1};
	static double parameter_max[]={4};

	int n_funct=0;
	static char *funct_names[]={"(null)"};
	static double funct_min[]={-10};
	static double funct_max[]={10};

	int manifold_type=EUCLIDEAN;
	static int periodic_varb[]={FALSE};
	static double period_start[]={0};
	static double period_end[]={1};

	int mapping_toggle=TRUE;
	int inverse_toggle=FALSE;

	int (*def_name)()=logistic_ds_func;
	int (*jac_name)()=logistic_jac;
	int (*aux_func_name)()=NULL;
	int (*inv_name)()=NULL;
	int (*dfdt_name)()=NULL;
	int (*dfdparam_name)()=NULL;

#include <ds_define.c>
}

