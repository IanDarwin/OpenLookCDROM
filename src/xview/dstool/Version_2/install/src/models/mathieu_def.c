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
        This vector field describes the motion of a 
        pendulum with a periodically excited support.
        This system is described in Guckenheimer/Holmes
        pp. 29-32 and references within.
        The vector field is given by

	x' = y
	y' = -Damping*y-(omega*omega+Ampl*cos(t))*sin(x)

	Auxiliary functions are:

	Ham = 0.5*y*y-(omega*omega+Ampl*cos(t))*cos(x)
	Strobe = sin( 0.5*p[1]*x[2] );

	Translation table:

		x[0] <--> x
		x[1] <--> y
		x[2] <--> t
		p[0] <--> Ampl
		p[1] <--> omega
		p[2] <--> Damping
*/

/* function used to define dynamical system */
int mathieu_ds_func(f,x,p)
double *f,*x,*p;
{

	f[0] = x[1];
	f[1] = -p[2]*x[1]-(p[1]*p[1]+p[0]*cos(x[2]))*sin(x[0]);

	return;
}

/* function used to define aux functions */
int mathieu_aux_func(f,x,p)
double *f,*x,*p;
{
  f[0] = 0.5*x[1]*x[1]-(p[1]*p[1]+p[0]*cos(x[2]))*cos(x[0]);
  f[1] = sin( 0.5*p[1]*x[2] );	/* this is zero for every multiple of 2 Pi/ w */
				/* So use event stopping on this to "strobe" at */
				/* forcing frequency */
	return;
}

/* function used to define jacobian. 
	input explicit jacobian in the  form
	m[i][j] = d f_i / d x_j; (starting with 0)
*/

int mathieu_jac(m,x,p)
double **m,*x,*p;
{
 m[0][0] = 0.;
 m[0][1] = 1.;
 m[1][0] = -(p[1]*p[1]+p[0]*cos(x[2]))*cos(x[0]);
 m[1][1] = -p[2];
	return;
}


/* function used to define default data */
int mathieu_init()
{
	int n_varb=2;
	static char *variable_names[]={"x","y"};
	static double variables[]={1.1,0};
	static double variable_min[]={-PI,-3};
	static double variable_max[]={PI,3};

	static char *indep_varb_name="time";
	double indep_varb_min=0;
	double indep_varb_max=10000;

	int n_param=3;
	static char *parameter_names[]={"Ampl","omega","Damping"};
	static double parameters[]={0.1,.5,0.01};
	static double parameter_min[]={0,0,0};
	static double parameter_max[]={1,3,1};

	int n_funct=2;
	static char *funct_names[]={"Ham","Strobe"};
	static double funct_min[]={-10,-1};
	static double funct_max[]={10,1};

	int manifold_type=PERIODIC;
	static int periodic_varb[]={TRUE,FALSE};
	static double period_start[]={-PI,0};
	static double period_end[]={PI,0};

	int mapping_toggle=FALSE;
	int inverse_toggle=FALSE;

	int (*def_name)()=mathieu_ds_func;
	int (*jac_name)()=mathieu_jac;
	int (*aux_func_name)()=mathieu_aux_func;
	int (*inv_name)()=NULL;
	int (*dfdt_name)()=NULL;
	int (*dfdparam_name)()=NULL;

#include <ds_define.c>
}

