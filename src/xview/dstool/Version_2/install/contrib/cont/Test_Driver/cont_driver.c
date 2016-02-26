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
#include <constants.h>
#include <memory.h>
#include "/marcy/mrm/dstool/cont/continue_def.h"
#include "cont_driver.h"

struct  Cont_Cntl_Ds            cont_ds;


/* --------------------------------------------------------------------------------
   this routine is a standalone driver for the continuation subsystem, separate
   from dstool, but utilizing the same postmaster and memory structure.

   this routine MUST set the following variables:

   local:
   ------
     test_problem       int         TRUE		default testproblem switch
     cont_debug		int	    FALSE		debug print switch

   continue_def & Cont_DataS structs:
   ----------------------------------
     Eigen_Switch       int         FALSE		compute eigen switch
     Eigen_Plot         int         FALSE		plot eigen switch
     Active_Param       int *       FALSE		boolean param switch

   postmaster:
   -----------
     (Cont_Control) Cont_Iters				max # of cont points to generate
     (Cont_Control) Cont_Direction			cont direction along manifold
     (Cont_Control) Cont_Param				index of distinguished cont paramemer
     (Cont_Control) Cont_Vary_Switch			fix cont param or vary by step?
     (Cont_Control) Cont_Jac_Update			update jacobian at every step?
     (Cont_Control) Cont_Abserr				abs error tolerance
     (Cont_Control) Cont_Relerr				rel error tolerance
     (Cont_Control) Cont_Minstp				min allowed stepsize
     (Cont_Control) Cont_Maxstp				max allowed stepsize
     (Cont_Control) Cont_Stpsize			estimated stepsize  
     (Cont_Control) Cont_Target 			required target value        
     (Traj_Ds_Object) Param_Dim				dim of parameter space
     (Traj_Ds_Object) Varb_Dim				dim of phase space
     (Traj_Ds_Object) Function_Dim			number of user-defined functions
     (Traj_Ds_Object) Varb_Ic				phase space initial conditions
     (Traj_Ds_Object) Param				param space initial conditions
     (Model_Cntl) DS_Def				fnct ptr to dynamical system function

   ------------------------------------------------------------------------------------------------ */

void
main(argc, argv)
int     argc;
char    *argv[];
{
  int      ch, check_file_to_read();
  char     *fname;
  FILE	   *file_ptr;

  while( --argc > 0 && (*++argv)[0] == '-') /* read command line arguments  */
    while( ch = *++argv[0])	/* in the standard Kernighan &  */
      switch (ch)		/* Ritchie fashion...           */
	{
        case 'd':
          cont_debug = TRUE;
          fprintf(stderr,"Debug Print Option Selected...\n");
          break;
        case 't':
          test_problem = TRUE;
          cont_debug = TRUE;
          fprintf(stderr,"Executing Standard Testproblem: Pitchfork Bifurcation \n");
          break;
        case 'f' :
          fname =  (char *) calloc(SIZE_OF_DIR_PLUS_FNAME,sizeof(char));
	  fname = "INPUT";
          if (check_file_to_read(fname)) /* check if file exists */
	    file_ptr = fopen( fname, "r" );
          else
	    {
	      free(fname);
	      fprintf(stderr,"Invalid file or path or file cannot be read!");
	      exit(0);
            }
	}

  pm( INIT, Mem_Obj_Cntl , Cont_Mem_Ptr , NULL);

  init_proc( file_ptr, test_problem );
  cont_proc();
  output_proc( (memory)  pm( GET, Mem_Obj_Cntl , Cont_Mem_Ptr , NULL), (int) test_problem );

}


int
init_proc( file_ptr, test_problem )
FILE	*file_ptr;
int	test_problem;
{
  int		i, j, *ivector(), ds_system;
  double	*dvector();
  extern void   *fetch_value();

  if(test_problem)
    {
      ds_def = pitchfork;
      active_param = test_active_param;
      varb_ic = test_varb_ic;
      param = test_param;
    }
  else
    {
      ds_system   	= *( (int *) fetch_value( file_ptr, INTEGER) );
      eigen_switch	= *( (int *) fetch_value( file_ptr, INTEGER) );
      eigen_plot		= *( (int *) fetch_value( file_ptr, INTEGER) );
      cont_mode		= *( (int *) fetch_value( file_ptr, INTEGER) );
      varb_dim		= *( (int *) fetch_value( file_ptr, INTEGER) );
      param_dim		= *( (int *) fetch_value( file_ptr, INTEGER) );
      function_dim	= *( (int *) fetch_value( file_ptr, INTEGER) );
      cont_iters		= *( (int *) fetch_value( file_ptr, INTEGER) );
      cont_direction	= *( (int *) fetch_value( file_ptr, INTEGER) );
      cont_param		= *( (int *) fetch_value( file_ptr, INTEGER) );
      cont_vary_switch	= *( (int *) fetch_value( file_ptr, INTEGER) );
      cont_jac_update	= *( (int *) fetch_value( file_ptr, INTEGER) );
      cont_stpsize	= *( (double *) fetch_value( file_ptr, DOUBLE) );
      cont_abserr	= *( (double *) fetch_value( file_ptr, DOUBLE) );
      cont_relerr	= *( (double *) fetch_value( file_ptr, DOUBLE) );
      cont_maxstp	= *( (double *) fetch_value( file_ptr, DOUBLE) );
      cont_minstp	= *( (double *) fetch_value( file_ptr, DOUBLE) );
      cont_target	= *( (double *) fetch_value( file_ptr, DOUBLE) );

      varb_ic = dvector(0,varb_dim-1);
      active_param = ivector(0,param_dim-1);
      param = dvector(0,param_dim-1);

      for(i=0; i<varb_dim; i++)
        varb_ic[i] = *( (double *) fetch_value( file_ptr, DOUBLE) ); 
      for(i=0; i<param_dim; i++)
	{
	  param[i] = *( (double *) fetch_value( file_ptr, DOUBLE) ); 
	  active_param[i] = FALSE;
	}
      for(i=0; i<Cont_Sel[cont_mode].Num_Req_Param; i++)
        active_param[ *( (int *) fetch_value( file_ptr, INTEGER) ) ] = TRUE;  

      switch (ds_system)
	{
         case 0:
	   ds_def = pitchfork;
	   break;
         default:
	   fprintf(stderr,"Invalid dynamical systems choice! Terminated. ");
	   exit(0);
	}

    }

  cont_ds.Eigen_Switch = eigen_switch;
  cont_ds.Eigen_Plot = eigen_plot;
  cont_ds.Active_Param = active_param;

  pm( PUT, Cont_Control, Cont_Mode, cont_mode , NULL);
  pm( PUT, Traj_Ds_Object, Varb_Dim, varb_dim, NULL);
  pm( PUT, Traj_Ds_Object, Param_Dim, param_dim, NULL);
  pm( PUT, Traj_Ds_Object, Function_Dim, function_dim, NULL);
  pm( PUT, Cont_Control, Cont_Iters, cont_iters, NULL);
  pm( PUT, Cont_Control, Cont_Direction, cont_direction, NULL);
  pm( PUT, Cont_Control, Cont_Stpsize, cont_stpsize, NULL);
  pm( PUT, Cont_Control, Cont_Abserr, cont_abserr, NULL);
  pm( PUT, Cont_Control, Cont_Relerr, cont_relerr, NULL);
  pm( PUT, Cont_Control, Cont_Maxstp, cont_maxstp, NULL);
  pm( PUT, Cont_Control, Cont_Minstp, cont_minstp, NULL);
  pm( PUT, Cont_Control, Cont_Target, cont_target, NULL);
  pm( PUT, Cont_Control, Cont_Param, cont_param, NULL);
  pm( PUT, Cont_Control, Cont_Vary_Switch, cont_vary_switch, NULL);
  pm( PUT, Cont_Control, Cont_Jac_Update, cont_jac_update , NULL);
  pm( INIT, Traj_Ds_Object, Varb_Ic, varb_dim,
     PUT_LIST, Traj_Ds_Object, Varb_Ic, 0, varb_dim-1, varb_ic, NULL);
  pm( INIT, Traj_Ds_Object, Param, param_dim,
     PUT_LIST, Traj_Ds_Object, Param, 0, param_dim-1, param, NULL);
  pm( INIT, Model_Cntl, DS_Def,
     PUT, Model_Cntl, DS_Def, ds_def, NULL);
  pm( INIT, Cont_Control, Cont_Fc, varb_dim,
     INIT, Cont_Control, Cont_Param_Fc, param_dim, NULL);

}


void
*fetch_value(file_ptr, type)
int           type;
FILE          *file_ptr;
{
  static char   buffer[100];
  static char   *fmt[]={"%d","%lg"};

  fscanf(file_ptr, fmt[type], buffer);
  return( (void *) buffer);
}


int
output_proc( mem_obj, test_problem )
memory	mem_obj;
int	test_problem;
{
  int		i, j, count = 0;
  int		(*function)();
  double	*points, *params, *fx, *dvector();
  int		n_params, alt_color, pick_color, symbol, hold, dim, n;


  function = (void *) pm( GET, Model_Cntl, DS_Def, NULL );

  fprintf(stderr,"\n \n");
  if (!Mem_reset_read(mem_obj)) 
    while (!Mem_read_next_traj(mem_obj,&params,&n_params, &alt_color, &pick_color, &symbol, &hold))  
      while (points = Mem_read_next_seg(mem_obj,&dim,&n))  
	{
	  fx = dvector( 0, dim );
	  for (i=0; i<n; i++)  
	    {
	      for( j=0; j<dim; j++)
		fprintf(stderr," %11.8lg", points[j]);
	      for( j=0; j<n_params; j++)
		fprintf(stderr," %11.8lg", params[j]);
              fprintf(stderr,"    errors =");
	      function( fx, points, params);
	      for( j=0; j<dim; j++)
		fprintf(stderr," %11.8lg", fx[j]);

	      points += dim;
	      fprintf(stderr,"\n --  mem pt = %d     comp pt = %d ----------------------\n", i, ++count);
	    }
          free_dvector( fx, 0, dim );
	}
}
