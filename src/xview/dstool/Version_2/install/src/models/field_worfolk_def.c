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
 * Fri Sep 25 15:22:09 1992
 */
#include <model_headers.h>

/*
	x' = x*(d*wsqr+c*zsqr+b*ysqr+a*rsqr+l)+ezw*y
	y' = y*(l+a*rsqr+b*zsqr+c*wsqr+d*xsqr)-ezw*x
	z' = z*(l+a*rsqr+b*wsqr+c*xsqr+d*ysqr)+exy*w
	w' = w*(l+a*rsqr+b*xsqr+c*ysqr+d*zsqr)-exy*z

   where
	xsqr = x*x
	ysqr = y*y
	zsqr = z*z
	wsqr = w*w
	rsqr = xsqr+ysqr+zsqr+wsqr
	exy = e*x*y
	ezw = e*z*w


	Translation table:

		x[0] <--> x
		x[1] <--> y
		x[2] <--> z
		x[3] <--> w
		p[0] <--> l
		p[1] <--> a
		p[2] <--> b
		p[3] <--> c
		p[4] <--> d
		p[5] <--> e
		TEMP[0] <--> xsqr
		TEMP[1] <--> ysqr
		TEMP[2] <--> zsqr
		TEMP[3] <--> wsqr
		TEMP[4] <--> rsqr
		TEMP[5] <--> exy
		TEMP[6] <--> ezw
*/

/* function used to define dynamical system */
int field_worfolk_ds_func(f,x,p)
double *f,*x,*p;
{

	double TEMP[7];
	TEMP[0] = x[0]*x[0];
	TEMP[1] = x[1]*x[1];
	TEMP[2] = x[2]*x[2];
	TEMP[3] = x[3]*x[3];
	TEMP[4] = TEMP[0]+TEMP[1]+TEMP[2]+TEMP[3];
	TEMP[5] = p[5]*x[0]*x[1];
	TEMP[6] = p[5]*x[2]*x[3];
	f[0] = x[0]*(p[4]*TEMP[3]+p[3]*TEMP[2]+p[2]*TEMP[1]+p[1]*TEMP[4]+p[0])+TEMP[6]*x[1];
	f[1] = x[1]*(p[0]+p[1]*TEMP[4]+p[2]*TEMP[2]+p[3]*TEMP[3]+p[4]*TEMP[0])-TEMP[6]*x[0];
	f[2] = x[2]*(p[0]+p[1]*TEMP[4]+p[2]*TEMP[3]+p[3]*TEMP[0]+p[4]*TEMP[1])+TEMP[5]*x[3];
	f[3] = x[3]*(p[0]+p[1]*TEMP[4]+p[2]*TEMP[0]+p[3]*TEMP[1]+p[4]*TEMP[2])-TEMP[5]*x[2];

	return;
}

/* function used to define aux functions */
/*
int field_worfolk_aux_func(f,x,p)
double *f,*x,*p;
{

	return;
}
*/

/* function used to define jacobian. NOT AUTOMATICALLY GENERATED.
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/
/*
int field_worfolk_jac(m,x,p)
double **m,*x,*p;
{

	return;
}
*/

/* function used to define default data */
int field_worfolk_init()
{
	int n_varb=4;
	static char *variable_names[]={"x","y","z","w"};
	static double variables[]={0.1,0.1,0.2,0.1};
	static double variable_min[]={-1.1,-1.1,-1.1,-1.1};
	static double variable_max[]={1.1,1.1,1.1,1.1};

	static char *indep_varb_name="time";
	double indep_varb_min=0;
	double indep_varb_max=10000;

	int n_param=6;
	static char *parameter_names[]={"l","a","b","c","d","e"};
	static double parameters[]={1,-1,0.1,-0.05,-0.1,1};
	static double parameter_min[]={-10,-10,-10,-10,-10,-10};
	static double parameter_max[]={10,10,10,10,10,10};

	int n_funct=0;
	static char *funct_names[]={"(null)"};
	static double funct_min[]={-10};
	static double funct_max[]={10};

	int manifold_type=EUCLIDEAN;
	static int periodic_varb[]={FALSE,FALSE,FALSE,FALSE};
	static double period_start[]={0,0,0,0};
	static double period_end[]={1,0,0,0};

	int mapping_toggle=FALSE;
	int inverse_toggle=FALSE;

	int (*def_name)()=field_worfolk_ds_func;
	int (*jac_name)()=NULL;
	int (*aux_func_name)()=NULL;
	int (*inv_name)()=NULL;
	int (*dfdt_name)()=NULL;
	int (*dfdparam_name)()=NULL;

#include <ds_define.c>
}

