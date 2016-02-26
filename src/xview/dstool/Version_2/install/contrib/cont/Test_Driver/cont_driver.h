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
/* contains the local variable defs for the driver program */

#define INTEGER 0
#define DOUBLE  1

int	eigen_switch = FALSE,
	eigen_plot = FALSE,
	cont_mode = 0,
	cont_iters = 100,
	cont_direction = FORWARD,
	cont_param = 2,
	cont_vary_switch = 1,
	varb_dim = 3, 
	param_dim = 2,	
	function_dim = 0,	
	cont_jac_update = 0,
	cont_debug = TRUE,
	test_problem = FALSE,
   	(*ds_def)(),
	*active_param;


double	*varb_ic,
	*param,
	cont_minstp = 0.001,
	cont_maxstp = 0.1, 
	cont_stpsize = 0.01,
	cont_target = 0.0,
	cont_abserr = 1.e-5,
	cont_relerr = 1.e-5;

extern struct	Cont_Cntl_Ds 		cont_ds;      

int     pitchfork(), static_func();
int	test_active_param[1] = {TRUE};
double	test_varb_ic[3] = {0.1, 0.1, 0.0},
	test_param[2] = {-4.0, 0.25};

