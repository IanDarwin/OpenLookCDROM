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
        This is a mapping from the complex plane to itself
        which is symmetric under the group D3.  Let REAL
        and IMAG denote the operators which take the real
        and imaginary part of a complex number.  Let z=x+iy
        be complex and let 
           iv = alpha |z| + lambda + beta REAL(z^3).
        Then this map is defined by

	x' = iv*x+gamma*real_z2
	y' = iv*y-gamma*im_z2

   where
	real_z2 = x*x-y*y
	im_z2 = 2*x*y
	real_z3 = x*x*x - x*y*y - 2*x*y*y
	iv = alpha*(x*x+y*y)+lambda+beta*real_z3

   The auxiliary function is |z|:
	modulus = x*x+y*y

	Translation table:

		x[0] <--> x
		x[1] <--> y
		p[0] <--> gamma
		p[1] <--> beta
		p[2] <--> lambda
		p[3] <--> alpha
*/

/* function used to define dynamical system */
int d3_symm_map_ds_func(f,x,p)
double *f,*x,*p;
{

	double real_z2, im_z2, real_z3, iv;
	real_z2 = x[0]*x[0]-x[1]*x[1];
	im_z2 = 2*x[0]*x[1];
	real_z3 = x[0]*x[0]*x[0] - x[0]*x[1]*x[1] - 2*x[0]*x[1]*x[1];
	iv = p[3]*(x[0]*x[0]+x[1]*x[1])+p[2]+p[1]*real_z3;
	f[0] = iv*x[0]+p[0]*real_z2;
	f[1] = iv*x[1]-p[0]*im_z2;

	return;
}

/* function used to define aux functions */
int d3_symm_map_aux_func(f,x,p)
double *f,*x,*p;
{
	f[0] = x[0]*x[0]+x[1]*x[1];

	return;
}

/* function used to define jacobian. NOT AUTOMATICALLY GENERATED.
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/
int d3_symm_map_jac(m,x,p)
double **m,*x,*p;
{
  double real_z3,Dreal_z3_x,Dreal_z3_y,iv,Div_x,Div_y,g1,g2;
  
  real_z3    = x[0]*x[0]*x[0] - x[0]*x[1]*x[1] - 2*x[0]*x[1]*x[1];
  Dreal_z3_x = 3.0*x[0]*x[0]-3.0*x[1]*x[1];
  Dreal_z3_y = -6.0*x[0]*x[1];
  iv    =  p[3]*(x[0]*x[0]+x[1]*x[1])+p[2]+p[1]*real_z3;
  Div_x =  2.0*p[3]*x[0] + p[1]*Dreal_z3_x;
  Div_y =  2.0*p[3]*x[1] + p[1]*Dreal_z3_y;
  g1 = p[0]*2.0*x[0];
  g2 = p[0]*2.0*x[1];

  m[0][0] = Div_x * x[0] + iv + g1;
  m[0][1] = Div_y * x[0]      - g2;
  m[1][0] = Div_x * x[1]      - g2;
  m[1][1] = Div_y * x[1] + iv - g1;
  return;
}

/* function used to define default data */
int d3_symm_map_init()
{
	int n_varb=2;
	static char *variable_names[]={"x","y"};
	static double variables[]={0.01,0.057};
	static double variable_min[]={-1.5,-1.5};
	static double variable_max[]={1.5,1.5};

	static char *indep_varb_name="iter";
	double indep_varb_min=0;
	double indep_varb_max=10000;

	int n_param=4;
	static char *parameter_names[]={"gamma","beta","lambda","alpha"};
	static double parameters[]={-0.8,0.1,1.52,-1};
	static double parameter_min[]={-5,0,0,-5};
	static double parameter_max[]={5,1,5,5};

	int n_funct=1;
	static char *funct_names[]={"modulus"};
	static double funct_min[]={-10};
	static double funct_max[]={10};

	int manifold_type=EUCLIDEAN;
	static int periodic_varb[]={FALSE,FALSE};
	static double period_start[]={0,0};
	static double period_end[]={1,0};

	int mapping_toggle=TRUE;
	int inverse_toggle=FALSE;

	int (*def_name)()=d3_symm_map_ds_func;
	int (*jac_name)()=d3_symm_map_jac;
	int (*aux_func_name)()=d3_symm_map_aux_func;
	int (*inv_name)()=NULL;
	int (*dfdt_name)()=NULL;
	int (*dfdparam_name)()=NULL;

#include <ds_define.c>
}

