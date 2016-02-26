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
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <constants.h>
#include <complib.h>
#include <iterate.h>
#include <prop_def.h>
#include "complib_test.h"

#ifdef NOT_SUN
#include <not_sun_supplements.h>
#endif


int     Method_List[]={RK4, EULER, RK4QC, BS};
char    *Method_Name[]={"Runge-Kutta (4)","Euler","Runge-Kutta (4,5)", "Bulirsch-Stoer"};


/* -------------------------------------------------------------------------------------------------------

   This routine serves as the driver for complib_test, the program used to test and verify the
   propagation subsection for dstool.  In construction, the main procedure performs the role of
   a Level 2 routine (see dstool PDS documemtation), which initializes the data stucture defined
   in complib.h used to pass data to the iteration routines (iterated maps) or integration 
   algorithms (vector fields).  This program can emulate any propagation calculation which can
   be performed by dstool and, in addition, allows the programmer or algorithm designer the 
   opportunity to study the implementation of a new routine apart from the interactive windowing
   system so that properties such as convergence and timing behavior can be accurately established. 

   For integration of vector fields, a number of testcases are provided.  Several of these are taken
   from two collections of standard test problems for comparing initial-value ODE solvers provided in 
   the following references:

   	Hull, T.E., W.H. Enright, B.M. Fellen and A.E. Sedgwick, "Comparing Numerical 
	Methods for Ordinary Differential Equations," SIAM Journal of Numerical Analysis,
	Vol. 9, No. 4, 1972, pps. 603-637.

	Enright, W.H., T.E> Hull and B. Lindberg, "Comparing Numerical Methods for Stiff
	Systems of O.D.E's", BIT, Vol. 15, 1975, pps. 10-48.

   Other test problems are taken from the systems of current interest in the study of dynamical 
   systems. 

   The program has the following command line options:

	-p <id number>		execute testcase number <id_number>, where <id_number> is an integer

	-a			execute all the appropriate algorithms on the specified testcase

	-m <id_number>		integration method selection, where <id_number> is an integer
				index into the Method_List array (see above).


   This program is the property of:
					Cornell University
					Center of Applied Mathematics
					305 Sage Hall, Ithaca  NY  14853

   and may be used and modified freely, provided that this legend is included on all tape media and
   as a part of the software program in whole or part.  This file is provided as is with no guarantees
   and without any obligation on the part of Cornell faculty, staff or students assist in its use, 
   correction, modification or enhancement.

   Author:   Mark Myers		Last modified:  July 27, 1992
                                

   ------------------------------------------------------------------------------------------------------- */



main(argc, argv)
int     argc;
char    *argv[];
{
  struct  Prop_DataS    prop_cntl;					
  int			count, i, status = 0, iterate(), integrate(), 
  			all_methods_switch = FALSE, (*exact_fp)(),
  			exact_sol_switch=FALSE, problem=0, method=RK4,
  			number_methods=1;
  char			ch;

  extern int		rk4_driver(), euler_driver(), rkqc_driver(), bs_driver();
  extern void		dump_data();

  while( --argc > 0 && (*++argv)[0] == '-') /* read command line arguments  */
    while( ch = *++argv[0] )	/* in the standard Kernighan &  */
      switch (ch)		/* Ritchie fashion...           */
	{
	case 'a':
	  all_methods_switch = TRUE;
          number_methods = nint( (double) (sizeof(Method_List)/sizeof(int)) );
	  break;
	case 'p':
	  problem = atoi( (char *) *++argv );
	  --argc;
	  break;
	case 'm':
	  method = Method_List[atoi( (char *) *++argv )];
	  --argc;
	  break;
	}

  for(count=0; count<number_methods; count++)
    {
      if( all_methods_switch ) method = Method_List[count];

      exact_sol_switch = load_setup(&prop_cntl, method, problem, &exact_fp, VALUES);
      if(prop_cntl.direction != FORWARD && prop_cntl.direction != BACKWARD) exit(-1);
	
      comp_alloc(&prop_cntl);
      allocate_integ(&prop_cntl, prop_cntl.iter_request);
      load_setup(&prop_cntl, method, problem, &exact_fp, ARRAYS);

      for(i=0; i<prop_cntl.ph_space_dim; i++)
	prop_cntl.traj_segment[0][i] = prop_cntl.state[i];

      if( prop_cntl.mapping_flag) /* mapping flag is TRUE */

	status = iterate(&prop_cntl); 

      else			/* vector field integration */
	{
	  switch (method)
	    {
	    case RK4:
	      prop_cntl.integ_driver = rk4_driver;
	      break;
	    case EULER:
	      prop_cntl.integ_driver = euler_driver;
	      break;				
	    case RK4QC:
	      prop_cntl.integ_driver = rkqc_driver;
	      break;				
	    case BS:
	      prop_cntl.integ_driver = bs_driver;
	      break;				
	    default:
	      fprintf(stderr,"\n Error in Alogrithm Selection!  Terminated...\n");
	      exit(-1);
	    }
	  status = (int) integrate(&prop_cntl);  
	}

      dump_data(&prop_cntl, method, exact_sol_switch, &exact_fp, prop_cntl.parameters);
      comp_free(&prop_cntl);
      release_integ(&prop_cntl, prop_cntl.iter_request);
    }
}


/* allocate space in *ic for trajectories, parameters, and state 
   set dimensions */
allocate_integ(ic, size)
struct Prop_DataS *ic;
int     size;
{
        int     status = 0;
        double  *dvector(), **dmatrix();

        ic->ph_space_dim  =    ic->ph_space_dim; 
        ic->parameter_dim =    ic->parameter_dim;
        ic->function_dim  =    ic->function_dim; 

        if ( ! (ic->state = dvector(0,ic->ph_space_dim-1)) ) status = -1;
        if ( ! (ic->parameters = dvector(0,ic->parameter_dim-1))  ) status = -1;
        if ( ! (ic->traj_segment = dmatrix(0, size,0,ic->ph_space_dim-1)) ) status = -1;
        if (status == -1) release_integ(ic,size);

        return(status);
}



/* free space allocated in allocate_integ */
release_integ(ic, size)
int     size;
struct Prop_DataS *ic;
{
        free_dmatrix(ic->traj_segment,0,size,0,ic->ph_space_dim-1);
        free_dvector(ic->parameters,0,ic->parameter_dim-1);
        free_dvector(ic->state,0,ic->ph_space_dim-1);
}


/* allocate space for routine items in prop_cntl struct */
int
comp_alloc(prop_cntl)
struct  Prop_DataS     *prop_cntl;
{
  int *ivector();
  double *dvector();
/*  char *calloc();*/
  int vdim = prop_cntl->ph_space_dim-1;

  /* factor of 10 in WORKSPACE removed */
  if(!(prop_cntl->workspace = dvector(0, vdim*vdim + WORKSPACE*prop_cntl->ph_space_dim))) return(-1);
  if(!(prop_cntl->i_wkspace = ivector(0,WORKSPACE*prop_cntl->ph_space_dim))) return(-1);
  if(!(prop_cntl->min = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->max = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->manifold = (Manifold *) calloc(1, sizeof(Manifold))))  return(-1);
  if(!(prop_cntl->manifold->periodic_varb = ivector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->manifold->period_start = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->manifold->period_end = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);

  return(0);
}	


/* Free storage for prop_cntl items allocated in comp_alloc. */
int
comp_free(prop_cntl)
struct  Prop_DataS     *prop_cntl;
{
  int vdim = prop_cntl->ph_space_dim - 1;

  free_dvector(prop_cntl->workspace, 0, vdim*vdim + WORKSPACE*prop_cntl->ph_space_dim);
  free_ivector(prop_cntl->i_wkspace, 0, WORKSPACE*prop_cntl->ph_space_dim);
  free_dvector(prop_cntl->min, 0,prop_cntl->ph_space_dim-1);
  free_dvector(prop_cntl->max, 0,prop_cntl->ph_space_dim-1);
  free_dvector(prop_cntl->manifold->period_start, 0, prop_cntl->ph_space_dim-1);
  free_dvector(prop_cntl->manifold->period_end, 0, prop_cntl->ph_space_dim-1);
  free_ivector(prop_cntl->manifold->periodic_varb, 0, prop_cntl->ph_space_dim-1);
/*
  free_dvector(prop_cntl->varb_fstop_values,0,prop_cntl->ph_space_dim-1);
  free_dvector(prop_cntl->func_fstop_values,0,prop_cntl->function_dim-1);
  free_ivector(prop_cntl->varb_fstop_active,0,prop_cntl->ph_space_dim-1);
  free_ivector(prop_cntl->func_fstop_active,0,prop_cntl->function_dim-1);
*/
  free(prop_cntl->manifold);
}	


/* 
  Print computation results in format (some fields optional) 
  iteration   this_time_step   this_time   exact_soln   this_computed_value   this_exact_value  difference
*/
void
dump_data(prop_cntl, method, exact_sol_switch, exact_fp, p)
struct  Prop_DataS      *prop_cntl;
double			*p;
int			exact_sol_switch, (**exact_fp)(), method;
{
   int		i, j;
   double	y[10];

   if( !prop_cntl->mapping_flag )
     {
      for(i=0; i< nint( (double) (sizeof(Method_List)/sizeof(int)) ); i++)
	 if( method == Method_List[i] ) printf("\n Integration Method:  %s \n",Method_Name[i]);
     }

   if (prop_cntl->iterations >= 0)
     for (i=0; i<=prop_cntl->iterations; i++)
       {
	printf(" %d   ",i);
	if( i>0 ) printf("%8.3lg    ",prop_cntl->traj_segment[i][prop_cntl->ph_space_dim-1]-
				      prop_cntl->traj_segment[i-1][prop_cntl->ph_space_dim-1]);
        printf("%15.7lg  ",prop_cntl->traj_segment[i][prop_cntl->ph_space_dim-1]);
	if( exact_sol_switch )
	    (*exact_fp)( y, prop_cntl->traj_segment[i][prop_cntl->ph_space_dim-1], p);	 
        for (j=0; j<prop_cntl->ph_space_dim-1;j++)
	  {
           printf("%15.7lg  ",prop_cntl->traj_segment[i][j]);
	   if( exact_sol_switch )
	     {
	      printf("%15.7lg  ", y[j] );
	      printf("%15.7lg  ", prop_cntl->traj_segment[i][j] - y[j] );
             }
           if( j<prop_cntl->ph_space_dim-2 )
	      printf("\n                                   ");
	   else printf("\n");
          }
       }
}


/* 
   fetch auxilary function values at a prescribed point 
*/

get_ds_func( y, x, param )
double	*x, *y, *param;
{
}


/* 
   dummy routines to satisfy linker
*/


int
prop_halt()
{
  return(FALSE);
}



void
traj_plot(prop_cntl, start_ptr, stop_ptr, func_values)
struct  Prop_DataS     *prop_cntl;
int     start_ptr, stop_ptr;
double  *func_values;
{
}


/* --------------------------  Test Problem Definition Code ----------------------------------- */


 
/* ------------------------------------------------------------------------------------------- 
   this proc controls the selection of the test problem to be evaluated.

   inputs:  problem      problem id number; for each valid number, a procedure must 
			 be defined which loads the structure prop_cntl with the correct
			 data, one which returns the derivatives evaluated at a prescribed
			 point and (optionally) one which returns the analytic solution
			 evaluated at a given point.
 
   outputs: prop_cntl    the passed data structure containing the required starting data
	  
	    exact_fp     the address pointing to the function which computes the exact
			 problem solution (if it exists).
   ------------------------------------------------------------------------------------------- */

int
load_setup(prop_cntl, method, problem, exact_fp, data_type)
struct  Prop_DataS	*prop_cntl;
int	method, problem, (**exact_fp)(), data_type;
{
  int		exact_switch = FALSE;
  extern int	exact_def(), exact_pb1(), exact_pb2(), exact_pb5(), exact_pb6();
  extern void   problem1(), problem2(), problem3(),
		problem4(), problem5(), problem6(),
		problem7(), def_problem();

  switch (problem)
    {
     case 1:
       problem1( prop_cntl, method, data_type );
       *exact_fp = exact_pb1;
       exact_switch = TRUE;
       break;
     case 2:
       problem2( prop_cntl, method, data_type );
       *exact_fp = exact_pb2;
       exact_switch = TRUE; 
       break;
     case 3:
       problem3( prop_cntl, method, data_type );
       *exact_fp = NULL;
       exact_switch = FALSE;
       break;
     case 4:
       problem4( prop_cntl, method, data_type );
       *exact_fp = NULL;
       exact_switch = FALSE;
       break;
     case 5:
       problem5( prop_cntl, method, data_type );
       *exact_fp = exact_pb5;
       exact_switch = TRUE;
       break;
     case 6:
       problem6( prop_cntl, method, data_type );
       *exact_fp = exact_pb6;
       exact_switch = TRUE;
       break;
     case 7:
       problem7( prop_cntl, method, data_type );
       *exact_fp = NULL;
       exact_switch = FALSE;
       break;
     case 8:
       problem8( prop_cntl, method, data_type );
       *exact_fp = NULL;
       exact_switch = FALSE;
       break;
     default:
       def_prob( prop_cntl, method, data_type );
       *exact_fp = exact_def;
       exact_switch = TRUE;
    }

  return( exact_switch );
}
