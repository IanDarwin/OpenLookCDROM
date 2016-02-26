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
 * Patrick A. Worfolk, 313 Sage Hall, 255-4195
 * Sat Sep 26 12:57:02 1992
 */
#include <model_headers.h>

/*
	x' = a*(1.5-z)*(-dH/dy)+b*z*(alpha*axby*(8-axby*axby)/16-beta*bxay)
	y' = a*(1.5-z)*(cy+dH/dx)+b*z*(beta*axby*(8-axby*axby)/116+alpha*bxay)
	z' = z*(1.5-z)*(z-1+H)+epsilon*(0.75-z)

   where
	H = (x-y)^4/64-(x^2-y^2)/2+(x+y)^2/4
	alpha = cos(theta)
	beta = sin(theta)
	axby = alpha*x+beta*y
	bxay = beta*x-alpha*y


	Translation table:

		x[0] <--> x
		x[1] <--> y
		x[2] <--> z
		p[0] <--> b
		p[1] <--> a
		p[2] <--> epsilon
		p[3] <--> theta
		p[4] <--> c
		TEMP[0] <--> H
		TEMP[1] <--> alpha
		TEMP[2] <--> beta
		TEMP[3] <--> axby
		TEMP[4] <--> bxay
		TEMP[5] <--> -dH/dy
		TEMP[6] <--> dH/dx+cy
*/

/* function used to define dynamical system */
int perturbed_dtoroid_ds_func(f,x,p)
double *f,*x,*p;
{

	double TEMP[7];
	TEMP[0] = (x[0]-x[1])*(x[0]-x[1])*(x[0]-x[1])*(x[0]-x[1])/64-(x[0]*x[0]-x[1]*x[1])/2+(x[0]+x[1])*(x[0]+x[1])/4;
	TEMP[1] = cos(p[3]);
	TEMP[2] = sin(p[3]);
	TEMP[3] = TEMP[1]*x[0]+TEMP[2]*x[1];
	TEMP[4] = TEMP[2]*x[0]-TEMP[1]*x[1];
	TEMP[5] = (x[0]-x[1])*(x[0]-x[1])*(x[0]-x[1])/16-1.5*x[1]-0.5*x[0];
	TEMP[6] = p[4]*x[1]+(x[0]-x[1])*(x[0]-x[1])*(x[0]-x[1])/16-0.5*x[0]+0.5*x[1];
	f[0] = p[1]*(1.5-x[2])*TEMP[5]+p[0]*x[2]*(TEMP[1]*TEMP[3]*(8-TEMP[3]*TEMP[3])/16-TEMP[2]*TEMP[4]);
	f[1] = p[1]*(1.5-x[2])*TEMP[6]+p[0]*x[2]*(TEMP[2]*TEMP[3]*(8-TEMP[3]*TEMP[3])/116+TEMP[1]*TEMP[4]);
	f[2] = x[2]*(1.5-x[2])*(x[2]-1+TEMP[0])/p[2]+(0.75-x[2]);

	return;
}

/* function used to define aux functions */
int perturbed_dtoroid_aux_func(f,x,p)
double *f,*x,*p;
{
  f[0] = cos(0.6) * x[0] + cos(0.9) * x[1];
  f[1] = -sin(0.6) * x[0] + sin(0.9) * x[1] + x[2];
  return;
}


/* function used to define jacobian. NOT AUTOMATICALLY GENERATED.
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/
/*
int perturbed_dtoroid_jac(m,x,p)
double **m,*x,*p;
{

	return;
}
*/

/* function used to define default data */
int perturbed_dtoroid_init()
{
	int n_varb=3;
	static char *variable_names[]={"x","y","z"};
	static double variables[]={0,0,0};
	static double variable_min[]={-5,-5,-5};
	static double variable_max[]={5,5,5};

	static char *indep_varb_name="time";
	double indep_varb_min=0;
	double indep_varb_max=10000;

	int n_param=5;
	static char *parameter_names[]={"b","a","epsilon","theta","c"};
	static double parameters[]={3.0,15.0,0.03,0.0,0.5};
	static double parameter_min[]={-10,-10,-10,-10,-10};
	static double parameter_max[]={10,10,10,10,10};

	int n_funct=2;
	static char *funct_names[]={"px","py"};
	static double funct_min[]={-5,-5};
	static double funct_max[]={5,5};

	int manifold_type=EUCLIDEAN;
	static int periodic_varb[]={FALSE,FALSE,FALSE};
	static double period_start[]={0,0,0};
	static double period_end[]={1,0,0};

	int mapping_toggle=FALSE;
	int inverse_toggle=FALSE;

	int (*def_name)()=perturbed_dtoroid_ds_func;
	int (*jac_name)()=NULL;
	int (*aux_func_name)()=perturbed_dtoroid_aux_func;
	int (*inv_name)()=NULL;
	int (*dfdt_name)()=NULL;
	int (*dfdparam_name)()=NULL;

#include <ds_define.c>
}

