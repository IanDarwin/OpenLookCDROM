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
#include <stdlib.h>
#include <stdio.h>

#include <constants.h>
#include <prop.h>
#include <complib.h>
#include <iterate.h>

#define TARGET_TOLS 1.0e-3

static void (*aux_fn_ptr)();
static double *aux_fn_vector;

/* --------------------------------------------------------------------------
   compute_orb.c :  level 2 procedure for controlling orbit propagation
	(see dstool pds document for decription of level 2 complib routines)

		    Variables set in prop_cntl structure:

	direction	direction of integration or iteration steps.  Allowed
			values are FORWARD and BACKWARD.  The sign of
			direction indicates whether this is a new trajectory
			segment, or the extention (CONTINUE) of the last one.

	inverse_flag	Boolean flag; TRUE if dynamical system is a mapping,
			FALSE if a vector field.

	jacobian_flag	Boolean flag; TRUE if analytic jacobian has been
			provided by user, FALSE otherwise.

	time_step	length of time step
	inverse		function pointer to inverse routine (for map), if
			defined.  Should be NULL otherwise.
	dxdt		function pointer to spatial jacobian routine,
			if defined by user.  SHould be NULL otherwise.
	f_iter		iteration exponent of map; ie., k if iterates of
			f^k are desired.
	estim_step	initial stepsize estimate, for integration.
	final_time	target time, if integrating to specified final time. 
	f_skip		Number of points to skip in plotting flow for a
			vector field.
		    

   input argumments:	prop_cntl	pre-allocated control structure for 
                                        the complib subsection routines
   output arguments:	status		termination status.  Allowed values 
                                        are:
					   0	normal


   called routines :	pm(), abs(), iterate(), integrate(), comp_alloc(),
			comp_def_setup(), comp_free()
   ------------------------------------------------------------------------- */


int
compute_orbit(prop_cntl)
struct  Prop_DataS     *prop_cntl;					
{
  int	i, status = 0, direction, iterate(), integrate();

  direction = abs( prop_cntl->direction );
  if(direction != FORWARD && direction != BACKWARD) return(-1);
	
  prop_cntl->prop_mode = *((int *) pm( GET, "Flow.Stopping_Condition", NULL ));
  comp_alloc(prop_cntl);	/* allocate storage for routine items in prop_cntl */

  comp_def_setup(prop_cntl);	/* set routine items in prop_cntl struct. */


  if (prop_cntl->start_to_save == 0)			/* paw  4/14/92 */
    for(i=0; i<prop_cntl->ph_space_dim; i++)
      prop_cntl->traj_segment[0][i] = prop_cntl->state[i];

  reset_interrupt();

  if( prop_cntl->mapping_flag) /* mapping flag is TRUE */
    {	
      prop_cntl->direction  = direction;
      prop_cntl->inverse_flag = 
	*((int *) pm( GET, "Model.Inverse_Flag", NULL ));
      prop_cntl->jacobian_flag = 
	*((int *) pm( GET, "Model.Jacobian_Flag", NULL ));
      prop_cntl->time_step = 
	( prop_cntl->direction == FORWARD ) ? STEP_FORW: STEP_BACK;
      prop_cntl->inverse = (int (*)()) pm( GET, "Model.Inverse", NULL );
      prop_cntl->dxdt = (int (*)()) pm(GET, "Model.DfDt", NULL);
      prop_cntl->fstop = NULL;
      prop_cntl->f_iter = *((int *) pm( GET, "Flow.Skip_Size", NULL ));

      status = iterate(prop_cntl); 
    }
  else				/* vector field integration */
    { 
      prop_cntl->estim_step = prop_cntl->time_step = 
	*((double *) pm( GET, "Flow.Stepsize", NULL ));
      prop_cntl->final_time = 
	*((double *) pm( GET, "Flow.Final_Time", NULL ));
      prop_cntl->f_skip = *((int *) pm( GET, "Flow.Skip_Size", NULL ));

      status = integrate(prop_cntl);  
    }

  comp_free(prop_cntl);
  return(status);
}

/* propagation routine created for one dimensional stable manifolds */
int
prop_cntl_orbit(prop_cntl)
struct  Prop_DataS     *prop_cntl;					
{
  int	i, status = 0, direction, iterate(), integrate();

  direction = abs( prop_cntl->direction );
  if(direction != FORWARD && direction != BACKWARD) return(-1);

  if (prop_cntl->start_to_save == 0)			/* paw  4/14/92 */
    for(i=0; i<prop_cntl->ph_space_dim; i++)
      prop_cntl->traj_segment[0][i] = prop_cntl->state[i];

  reset_interrupt();

  if ( prop_cntl->mapping_flag ) /* mapping flag is TRUE */
    {	
      prop_cntl->direction = direction;
      prop_cntl->inverse_flag = 
	*((int *) pm( GET, "Model.Inverse_Flag", NULL ));
      prop_cntl->jacobian_flag = 
	*((int *) pm( GET, "Model.Jacobian_Flag", NULL )); 
      prop_cntl->inverse = (int (*)()) pm( GET, "Model.Inverse", NULL );
      prop_cntl->dxdt = (int (*)()) pm(GET, "Model.DfDt", NULL);

      status = iterate(prop_cntl); 
    }
  else				/* vector field integration */
    { 
      prop_cntl->estim_step = prop_cntl->time_step;

      status = integrate(prop_cntl);  
    }

  return(status);
}

/* allocate space for routine items in prop_cntl struct 
   also calls watch_setup */
int
comp_alloc(prop_cntl)
struct  Prop_DataS     *prop_cntl;
{
  int *ivector();
  double *dvector();
  int vdim = prop_cntl->ph_space_dim-1;

  if(!(prop_cntl->workspace = dvector(0, vdim*vdim + WORKSPACE*prop_cntl->ph_space_dim))) return(-1);
  if(!(prop_cntl->i_wkspace = ivector(0,WORKSPACE*prop_cntl->ph_space_dim))) return(-1);
  if(!(prop_cntl->min = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->max = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->manifold = (Manifold *) calloc(1, sizeof(Manifold))))  return(-1);
  if(!(prop_cntl->manifold->periodic_varb = ivector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->manifold->period_start = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);
  if(!(prop_cntl->manifold->period_end = dvector(0,prop_cntl->ph_space_dim-1))) return(-1);
  prop_cntl->history_list = (History_Struct *) NULL;
  watch_setup(prop_cntl);

  return(0);
}	

/* Free storage for prop_cntl items allocated in comp_alloc. 
   Also calls watch_free. */
int
comp_free(prop_cntl)
struct  Prop_DataS     *prop_cntl;
{
  int vdim = prop_cntl->ph_space_dim - 1;

  watch_free(prop_cntl);
  free_dvector(prop_cntl->workspace, 0, vdim*vdim + WORKSPACE*prop_cntl->ph_space_dim);
  free_ivector(prop_cntl->i_wkspace, 0, WORKSPACE*prop_cntl->ph_space_dim);
  free_dvector(prop_cntl->min, 0,prop_cntl->ph_space_dim-1);
  free_dvector(prop_cntl->max, 0,prop_cntl->ph_space_dim-1);
  free_dvector(prop_cntl->manifold->period_start, 0, prop_cntl->ph_space_dim-1);
  free_dvector(prop_cntl->manifold->period_end, 0, prop_cntl->ph_space_dim-1);
  free_ivector(prop_cntl->manifold->periodic_varb, 0, prop_cntl->ph_space_dim-1);
  free(prop_cntl->manifold);
  if (prop_cntl->history_list != (History_Struct *) NULL) {
      free_history_struct(prop_cntl->history_list);
      prop_cntl->history_list = (History_Struct *) NULL;
  }

}

/* Free storage related to fstop groups. */
int
watch_free(prop_cntl)
struct Prop_DataS *prop_cntl;
{
  int i;

  if ( prop_cntl->prop_mode != PROP_FSTOP && 
       prop_cntl->prop_mode != PROP_POINCARE) return;

  free_dvector(aux_fn_vector, 0, prop_cntl->function_dim-1);

  for(i=0; i<prop_cntl->num_fstop_grps; i++)
    {
      free(prop_cntl->fstop_grp_list[i]->fstop_indices);
      free(prop_cntl->fstop_grp_list[i]->target_values);
      free(prop_cntl->fstop_grp_list[i]->target_tols);
      free(prop_cntl->fstop_grp_list[i]->cross_orient);
      free(prop_cntl->fstop_grp_list[i]->cross_mode);
      free(prop_cntl->fstop_grp_list[i]);
    }
  free(prop_cntl->fstop_grp_list);  
}	


/* Set routine items in prop_cntl. This includes plotting ranges and
   integration algorithm properties. */
int
comp_def_setup(prop_cntl)
struct  Prop_DataS     *prop_cntl;
{
  prop_cntl->function      =  (int (*)()) pm( GET, "Model.DS_Def", NULL );
  prop_cntl->aux_function      =  (int (*)()) pm( GET, "Model.Aux_Function", NULL );
  prop_cntl->dfdx          =  (int (*)()) pm( GET, "Model.DfDx", NULL );
  prop_cntl->mapping_flag = *((int *) pm( GET, "Model.Mapping_Flag", NULL ));
  prop_cntl->plot_traj = (void (*)()) pm(GET, "Flow.Plot_Function", NULL);
  pm( GET_LIST, "Model.Varb_Min", 0, prop_cntl->ph_space_dim-1, 
     prop_cntl->min, NULL);
  pm( GET_LIST, "Model.Varb_Max", 0, prop_cntl->ph_space_dim-1, 
     prop_cntl->max, NULL);
  prop_cntl->manifold->type = *((int *) pm( GET, "Manifold.Type", NULL ));
  pm( GET_LIST, "Manifold.Periodic_Varb", 0, 
     prop_cntl->ph_space_dim-2,            /* knock off one for indexing and */
     prop_cntl->manifold->periodic_varb, NULL);	  /* another to exclude time */
  pm( GET_LIST, "Manifold.Period_Start", 0, prop_cntl->ph_space_dim-2, 
     prop_cntl->manifold->period_start, NULL);
  pm( GET_LIST, "Manifold.Period_End", 0, prop_cntl->ph_space_dim-2, 
     prop_cntl->manifold->period_end, NULL);
  prop_cntl->integ_driver = Int_Algol.Int_Driver; 
  prop_cntl->enable_nsteps          = Int_Algol.Enable_Nsteps;;
  prop_cntl->enable_fstop           = Int_Algol.Enable_Fstop;
  prop_cntl->enable_tstop           = Int_Algol.Enable_Tstop;
  prop_cntl->fixed_step_flag        = Int_Algol.Fixed_Step_Flag;
  prop_cntl->panel_int_values    = Int_Algol.Ifields;
  prop_cntl->panel_dp_values     = Int_Algol.Dfields;
  prop_cntl->panel_choice_values = Int_Algol.Sel_Values;
  prop_cntl->panel_option	 = Int_Algol.Panel_Sel_Value; 
  prop_cntl->diverg_cutoff	 = 
    *((double *) pm( GET, "Flow.Diverg_Cutoff", NULL ));
  prop_cntl->prop_segsize         = 
    *((int *) pm( GET, "Defaults.Disp_Points", NULL ));
  reset_interrupt();

}



/* ------------------------------------------------------------------------
   proc used to set-watch functions for the variables
   The function calculates x[index] and returns this value as *result.
   ------------------------------------------------------------------------ */
int
varb_watch_func(x,p,index,result)
int	index;
double  *result,x[],p[];
{
	int	status = 0;
	int     n_varbs  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;

	*result = 0.0;

	if( index < 0 || index >= n_varbs )
	   status = -1;
	else *result = x[index];

	return(status);
}


/* ------------------------------------------------------------------------
   Proc used to set-watch functions for the auxiliary functions.
   The function calculates aux_f[index] and returns this value as *result.
   ------------------------------------------------------------------------ */
int
aux_fn_watch_func(x,p,index,result)
int	index;
double  *result,x[],p[];
{

  aux_fn_ptr(aux_fn_vector,x,p);
  *result = aux_fn_vector[index];
  return 0;
}



/*
 * Procedure to setup watch functions for FSTOP propagation
 * Sets the fields of prop_cntl->fstop_grp_list using information
 * from the postmaster and other fields of prop_cntl. Stopping information
 * is recorded in a linked list of fstop groups.
 */
int
watch_setup(prop_cntl)
struct  Prop_DataS     *prop_cntl;
{
  int			i, 
			vcount=0, /* counter through list of phase space variables  */
			fcount=0, /* counter through list of auxiliary functions */
			grp_index=0; /* counter through fstop groups */
  int			*varb_stop_active, /* list of booleans indicating which variables are active */
  			*func_stop_active; /* list of booleans indicating which aux functions are active */
  double		*stop_values, /* temporary array to hold numerical values whose crossing is to
				         result in stopping */
  *dvector();
  int			varb_watch_func(), /* used to generate the function variable = constant */
  			aux_fn_watch_func(), /* used to generate the function aux_function = constant */
  			*ivector();


  prop_cntl->num_fstop_grps = 0;
  
  if ( prop_cntl->prop_mode != PROP_FSTOP &&
       prop_cntl->prop_mode != PROP_POINCARE) return;

  /* Identify and record function events */
  func_stop_active = ivector(0,prop_cntl->function_dim-1);
  pm(GET_LIST, "Flow.Funct_Events", 0, prop_cntl->function_dim-1, 
     func_stop_active, NULL);
  for(i=0; i<prop_cntl->function_dim; i++)
    if(func_stop_active[i] == TRUE) ++fcount;
  if (fcount) ++prop_cntl->num_fstop_grps;
  
  /* Identify and record variable events */
  varb_stop_active = ivector(0,prop_cntl->ph_space_dim-1);
  pm(GET_LIST, "Flow.Varb_Events", 0, prop_cntl->ph_space_dim-1, 
     varb_stop_active, NULL);
  for(i=0; i<prop_cntl->ph_space_dim; i++)
    if(varb_stop_active[i] == TRUE) ++vcount;
  if(vcount) ++prop_cntl->num_fstop_grps;
 
  /* allocate memory for all fstop group structures */ 
  prop_cntl->fstop_grp_list = (fstop_group **) calloc(prop_cntl->num_fstop_grps,sizeof(fstop_group *));
  for(i=0; i<prop_cntl->num_fstop_grps; i++)
    prop_cntl->fstop_grp_list[i] = (fstop_group *) calloc(prop_cntl->num_fstop_grps,sizeof(fstop_group));

  /* Set details of event stopping for variable values */  
  if( vcount > 0 )
    {
      stop_values = dvector(0,prop_cntl->ph_space_dim-1);
      pm(GET_LIST, "Flow.Varb_Event_Values", 0, prop_cntl->ph_space_dim-1, 
	 stop_values, NULL);
      prop_cntl->fstop_grp_list[grp_index]->fstop_func_ptr = varb_watch_func;
      prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices = vcount;

      /* allocate lists within fstop group list for variables */
      prop_cntl->fstop_grp_list[grp_index]->fstop_indices =
	(int *) calloc(prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices,sizeof(int));
      prop_cntl->fstop_grp_list[grp_index]->target_values =
	(double *) calloc(prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices,sizeof(double));
      prop_cntl->fstop_grp_list[grp_index]->target_tols =
	(double *) calloc(prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices,sizeof(double));
      prop_cntl->fstop_grp_list[grp_index]->cross_orient =
	(int *) calloc(prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices,sizeof(int));
      prop_cntl->fstop_grp_list[grp_index]->cross_mode =
	(int *) calloc(prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices,sizeof(int));

      vcount = 0;
      for(i=0; i<prop_cntl->ph_space_dim; i++)
	if( varb_stop_active[i] == TRUE )
	  {
	    prop_cntl->fstop_grp_list[grp_index]->fstop_indices[vcount] = i;
	    prop_cntl->fstop_grp_list[grp_index]->target_values[vcount] = stop_values[i];
	    prop_cntl->fstop_grp_list[grp_index]->target_tols[vcount] = TARGET_TOLS;
	    prop_cntl->fstop_grp_list[grp_index]->cross_orient[vcount] = PM_and_MP; 
	    prop_cntl->fstop_grp_list[grp_index]->cross_mode[vcount] = SOLVE_STEP;
	    ++vcount;
	  }
      free_dvector(stop_values,0,prop_cntl->ph_space_dim-1);
      ++grp_index;
    }

  /* Set details of event stopping for auxiliary function values */        
  if( fcount > 0 )
    {
      /* setup kluge for auxiliary functions since you cannot 
	 access one at a time! */
      aux_fn_ptr = (void (*)()) pm(GET, "Model.Aux_Function", NULL);
      aux_fn_vector = dvector(0, prop_cntl->function_dim-1);
      
      stop_values = dvector(0,prop_cntl->function_dim-1);
      pm(GET_LIST, "Flow.Funct_Event_Values", 0, prop_cntl->function_dim-1, 
	 stop_values, NULL);
      prop_cntl->fstop_grp_list[grp_index]->fstop_func_ptr = aux_fn_watch_func;
      prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices = fcount;

      /* allocate lists within fstop group list for auxiliary functions*/
      prop_cntl->fstop_grp_list[grp_index]->fstop_indices =
	(int *) calloc(prop_cntl->fstop_grp_list[grp_index]->num_fstop_indices,
		       sizeof(int));
      prop_cntl->fstop_grp_list[grp_index]->target_values =
	(double *) calloc(prop_cntl->fstop_grp_list[grp_index]->
			  num_fstop_indices,sizeof(double));
      prop_cntl->fstop_grp_list[grp_index]->target_tols =
	(double *) calloc(prop_cntl->fstop_grp_list[grp_index]->
			  num_fstop_indices,sizeof(double));
      prop_cntl->fstop_grp_list[grp_index]->cross_orient =
	(int *) calloc(prop_cntl->fstop_grp_list[grp_index]->
		       num_fstop_indices,sizeof(int));
      prop_cntl->fstop_grp_list[grp_index]->cross_mode =
	(int *) calloc(prop_cntl->fstop_grp_list[grp_index]->
		       num_fstop_indices,sizeof(int));

      fcount = 0;
      for(i=0; i<prop_cntl->function_dim; i++)
	if( func_stop_active[i] == TRUE )
	  {
	    prop_cntl->fstop_grp_list[grp_index]->fstop_indices[fcount] = i;
	    prop_cntl->fstop_grp_list[grp_index]->target_values[fcount] = stop_values[i];
	    prop_cntl->fstop_grp_list[grp_index]->target_tols[fcount] = TARGET_TOLS;
	    prop_cntl->fstop_grp_list[grp_index]->cross_orient[fcount] = PM_and_MP; 
	    prop_cntl->fstop_grp_list[grp_index]->cross_mode[fcount] = SOLVE_STEP;
	    ++fcount;
	  }
      free_dvector(stop_values,0,prop_cntl->function_dim-1);
      ++grp_index;
    }
  
  free_ivector(varb_stop_active,0,prop_cntl->ph_space_dim-1);
  free_ivector(func_stop_active,0,prop_cntl->function_dim-1);
}

