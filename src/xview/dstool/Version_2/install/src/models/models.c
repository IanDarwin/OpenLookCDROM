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
/* 	code to setup a new model        */


#include <stdio.h>

#include <modellib.h>
#include <constants.h>
#include <pm.h>


void
load_model()
{
	int	n, i;
	/* char	s[200];
	int	system_mess_proc(); */
	int update_stored_points(); 
	void rebuild_windows();

	n = *((int *) pm(GET, "Model.Load_Number", NULL));
	/* sprintf(s,"Loading model %s",DS_Sel[n].DS_Name);
	system_mess_proc(0,s); */

	pm( INIT, "Model.Name", MAX_LEN_DS_TITLE,
	    INIT, "Model.Initialization", 
	    PUT,  "Model.Name", DS_Sel[n].DS_Name, 
	    PUT,  "Model.Initialization", DS_Sel[n].ds_init, NULL );

	DS_Sel[n].ds_init();

	pm(EXEC, "Control.Sys_Reset",
	   EXEC, "Control.User_Reset", NULL);
	
	/* refresh all open windows which contain model-dependent data */
	rebuild_windows();
}
	

/* ------------------------------------------------------------------------ 
   fetches the currently installed functional values.
   ------------------------------------------------------------------------ */
int
get_ds_func(func, varb, param)
double		*func,*varb,*param;
{
  int 	(*aux_f)();
  
  aux_f = (void *) pm( GET, "Model.Aux_Function", NULL );
  aux_f(func, varb, param);
}



/* ------------------------------------------------------------------------ 
   returns TRUE if the currently installed dynamical system has
   functions associated with it and FALSE otherwise.

   ------------------------------------------------------------------------ */
int
valid_func()
{
	if( (void *) pm ( GET, "Model.Aux_Function", NULL ) ) 
	   return(TRUE);
	else
	   return(FALSE);
}



/* -----------------------------------------------------------------------------  
   returns a ptr to the name of the current dynamical system  

   ----------------------------------------------------------------------------- */
static char the_name[MAX_LEN_DS_TITLE];
char *
  get_ds_name()
{
  pm (GET, "Model.Name", the_name, NULL);
  return ( the_name );
}



/* 
   initialize variables for the new model
*/
varb_setup( n_varb, variable_names, variables, variable_min, 
		variable_max, indep_varb_name, indep_varb_min, indep_varb_max)
int	n_varb;
char	**variable_names, *indep_varb_name;
double	*variables, *variable_min, *variable_max, indep_varb_min, indep_varb_max;
{
  int	i;

	pm( PUT, "Model.Varb_Dim", n_varb+1, NULL);
	pm( INIT, "Model.Varb_Names", n_varb+1, MAX_LEN_VARB_NAME, NULL);
	for (i=0;i<n_varb; i++)
            pm( PUT, "Model.Varb_Names", i, variable_names[i], NULL);
	pm( PUT, "Model.Varb_Names", n_varb, indep_varb_name, NULL);
	pm( INIT, "Model.Varb_Ic", n_varb+1,
	    INIT, "Selected.Varb_Ic", n_varb+1,
	    INIT, "Selected.Varb_Fc", n_varb+1,
	    PUT_LIST, "Model.Varb_Ic", 0, n_varb-1, variables,
	    PUT_LIST, "Selected.Varb_Ic", 0, n_varb-1, variables, 
	    PUT_LIST, "Selected.Varb_Fc", 0, n_varb-1, variables, 
	   NULL);
	pm( INIT, "Model.Varb_Min", n_varb+1, NULL);
	pm( INIT, "Model.Varb_Max", n_varb+1, NULL);
	pm( PUT_LIST, "Model.Varb_Min", 0, n_varb-1, variable_min, NULL);
	pm( PUT, "Model.Varb_Min", n_varb, indep_varb_min, NULL);
	pm( PUT_LIST, "Model.Varb_Max", 0, n_varb-1, variable_max, NULL);
	pm( PUT, "Model.Varb_Max", n_varb, indep_varb_max, NULL);
}


/*
  initialize parameters and auxiliary functions
*/
param_func_setup( n_param, n_funct, parameter_names, funct_names, parameters,
		 parameter_min, parameter_max, funct_min, funct_max )
int	n_param, n_funct;
char	**parameter_names, **funct_names;
double	*parameters, *parameter_min, *parameter_max, *funct_min, *funct_max;
{
  int i;

  pm( PUT, "Model.Param_Dim", n_param, NULL);
  pm( INIT, "Model.Param_Names", n_param, MAX_LEN_VARB_NAME, NULL);
  for( i=0; i<n_param; i++)
    pm( PUT, "Model.Param_Names", i, parameter_names[i], NULL);
 
  pm( PUT, "Model.Funct_Dim", n_funct, NULL);
  pm( INIT, "Model.Funct_Names", n_funct, MAX_LEN_VARB_NAME, NULL);
  for( i=0; i<n_funct; i++ )
    pm( PUT, "Model.Funct_Names", i, funct_names[i], NULL);

  pm( INIT, "Model.Param_Ic", n_param,
      INIT, "Selected.Param_Ic", n_param,
      INIT, "Selected.Param_Fc", n_param,
      PUT_LIST, "Model.Param_Ic", 0, n_param-1, parameters, 
      PUT_LIST, "Selected.Param_Ic", 0, n_param-1, parameters, 
      PUT_LIST, "Selected.Param_Fc", 0, n_param-1, parameters, 
      NULL);

  pm( INIT, "Model.Param_Min", n_param,
      INIT, "Model.Param_Max", n_param,
      INIT, "Model.Funct_Min", n_funct,
      INIT, "Model.Funct_Max", n_funct,
      PUT_LIST, "Model.Param_Min", 0, n_param-1, parameter_min,
      PUT_LIST, "Model.Param_Max", 0, n_param-1, parameter_max,
      PUT_LIST, "Model.Funct_Min", 0, n_funct-1, funct_min, 
      PUT_LIST, "Model.Funct_Max", 0, n_funct-1, funct_max, NULL);
}

/* 
   initialize phase space for the new model
*/
phase_sp_setup( n_varb, manifold_type, periodic_varb, period_start, period_end )
int	n_varb, manifold_type, *periodic_varb;
double	*period_start, *period_end;
{

  pm( INIT, "Manifold.Type", 
      INIT, "Manifold.Periodic_Varb", n_varb, 
      INIT, "Manifold.Period_Start", n_varb,
      INIT, "Manifold.Period_End", n_varb,
      PUT, "Manifold.Type", manifold_type,
      PUT_LIST, "Manifold.Periodic_Varb", 0, n_varb-1, periodic_varb,
      PUT_LIST, "Manifold.Period_Start", 0, n_varb-1, period_start,
      PUT_LIST, "Manifold.Period_End", 0, n_varb-1, period_end, NULL );
}


