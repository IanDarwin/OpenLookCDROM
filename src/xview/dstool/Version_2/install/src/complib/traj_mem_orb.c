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
#include <constants.h>
#include <prop.h>
#include <modellib.h>
#include <memory.h>
#include <complib.h>


/* ---------------------------------------------------------------------------
  
 traj_mem_proc :  level 1 procedure for controlling orbit propagation
	 (see dstool pds document for decription of level 1 complib routines)

  This routine MUST set the following elements in the integration 
  structure prop_cntl:

	ph_space_dim	phase space dimension
	parameter_dim	parameter space dimension
	function_dim	number of user-defined auxilary functions
	direction		{FORWARD, BACKWARD} ; negative values indicate
					   segment is a CONTINUE      
	prop_mode	propagation mode {PROP_NSTEP, PROP_FSTEP, PROP_TF}
	prop_segsize	display/save chunk size
	symbol		plotting symbol
	table_color	if positive & 2D View window is set to alt color 
			choice, table_color is colortable index.  If negative,
			indicates a system color is desired.
	sys_color	if table_color is not negative and 2D View window is 
	                set to pick color choice, 
			sys_color is colortable index.
			If table_color < 0, value indices desired 
                        system color index.
	iter_request	number iterations requested
	state		initial conditions
	parameters		paramemter values to be used
	traj_segment	return array for propagated points 
			   (see pds for instructions on dimensioning array)

   input argumments:	none
   output arguments:	none
   called routines :	pm, 
			system_mess_proc, bump_color, memory_start_new_flow,
			memory_add_point, allocate_integ, release_integ, 
			prop_pan_output, compute_orbit
   ------------------------------------------------------------------------ */

int
traj_mem_orbit()
{
  int		status=0, i, color[3], max_num_traj;
  static	int	last_dir=FORWARD;
  memory traj_mem = NULL;
  static struct  	Prop_DataS     prop_cntl;
  
	

/*  prop_cntl.plot_traj = (void *) pm(GET, "Flow.Plot_Function", NULL);*/
  prop_cntl.direction = *((int *) pm(GET, "Flow.Direction", NULL));
  pm(PUT, "Flow.Direction", CONTINUE, NULL);
  if (*((int *) pm(GET, "Defaults.Recording")) == 0)
    traj_mem = (memory)  pm( GET, "Memory.Traj" , NULL);
	
  prop_cntl.iterations = 0;
  if (prop_cntl.direction == CONTINUE) prop_cntl.start_to_save = -1;
  else prop_cntl.start_to_save = *((int *) pm(GET, "Flow.Start_Save_Points", NULL));
  max_num_traj = prop_cntl.iter_request = 
    *((int *) pm( GET, "Flow.Total_Iterates", NULL )) - abs(prop_cntl.start_to_save);
  if (prop_cntl.start_to_save != 0)
    prop_cntl.iter_request++;

  if (max_num_traj<0) return(status);

  if (0 != allocate_integ(&prop_cntl, max_num_traj)) 
    {system_mess_proc(1,"traj_mem_orb : Memory Allocation Failure \n");
     return(-1); }

  switch (prop_cntl.direction)
    {case CONTINUE:
       prop_cntl.direction = -last_dir;
       pm( GET_LIST, "Selected.Varb_Fc", 0, prop_cntl.ph_space_dim-1, 
	  prop_cntl.state, NULL);
       pm( GET_LIST, "Selected.Param_Fc", 0, prop_cntl.parameter_dim-1, 
	  prop_cntl.parameters, NULL);
       break;
     case FORWARD:
     case BACKWARD:
       bump_color();
       last_dir = prop_cntl.direction;
       pm( GET_LIST, "Selected.Varb_Ic", 0, prop_cntl.ph_space_dim-1, 
	  prop_cntl.state, NULL);
       pm( GET_LIST, "Selected.Param_Ic", 0, prop_cntl.parameter_dim-1, 
	  prop_cntl.parameters, NULL); 
       prop_cntl.symbol = get_symbol_code( 
		  *((int *) pm(GET, "Defaults.Symbol_Index", NULL)),
		  *((int *) pm(GET, "Defaults.Size_Index", NULL)));
       prop_cntl.table_color = (int) get_alt_color();
       prop_cntl.sys_color = 
	 *((int *) pm( GET, "Color.Pick_Color_Choice", NULL));
       memory_start_new_flow( traj_mem, 1, 0, 0, prop_cntl.iter_request+1, 
			     0, 0);
       color[0] = prop_cntl.table_color;
       color[1] = prop_cntl.sys_color;
       color[2] = prop_cntl.symbol;
       memory_add_point(traj_mem, NULL, prop_cntl.parameters, color, 
			NULL, NULL, NULL, NULL);
       break;
     }

  
  status = compute_orbit(&prop_cntl);

  if (prop_cntl.iterations >= 0)
    {
      memory_add_points( traj_mem, prop_cntl.iterations+1, prop_cntl.traj_segment, 
			NULL, NULL, NULL, NULL);
      for (i=0; i<prop_cntl.ph_space_dim;i++)
	pm( PUT, "Selected.Varb_Fc", i,
	   prop_cntl.traj_segment[prop_cntl.iterations][i], NULL); 
      for (i=0; i<prop_cntl.parameter_dim;i++)
	pm( PUT, "Selected.Param_Fc", i, prop_cntl.parameters[i], NULL); 
    }

  release_integ(&prop_cntl, max_num_traj);

  return(status);
}


	  
/* allocate space in *ic for trajectories, parameters, and state 
   set dimensions */
allocate_integ(ic, size)
struct Prop_DataS *ic;
int	size;
{
	int	status = 0;
	double	*dvector(), **dmatrix();

  	ic->ph_space_dim  = 	*((int *) pm( GET, "Model.Varb_Dim", NULL ));
  	ic->parameter_dim = 	*((int *) pm( GET, "Model.Param_Dim", NULL ));
  	ic->function_dim  = 	*((int *) pm( GET, "Model.Funct_Dim", NULL ));

	if ( ! (ic->state = dvector(0,ic->ph_space_dim-1)) ) 
	    status = -1;
	if (ic->parameter_dim == 0)
	    ic->parameters = NULL;
	else if ( ! (ic->parameters = dvector(0,ic->parameter_dim-1))  ) 
	    status = -1;
	if ( ! (ic->traj_segment = dmatrix(0, size,0,ic->ph_space_dim-1)) ) 
	    status = -1; 
	if (status == -1) 
	    release_integ(ic,size);

	return(status);
}
	


/* free space allocated in allocate_integ */
release_integ(ic, size)
int	size;
struct Prop_DataS *ic;
{
	free_dmatrix(ic->traj_segment,0,size,0,ic->ph_space_dim-1);
	free_dvector(ic->parameters,0,ic->parameter_dim-1);
	free_dvector(ic->state,0,ic->ph_space_dim-1);   
}





/* ---------------------------------------------------------------------------------------
   code to manage alternating color index
   last change:	1/22/91
   --------------------------------------------------------------------------------------- */
int	current_color_index = -1;

int
bump_color()
{
	int	n_traj;
	
	n_traj = *( (int *) pm( GET, "Color.Traj_Colormap_Size", NULL));
	current_color_index=(current_color_index>=n_traj-1)? 0 : current_color_index+1;
}



int
reset_color()
{
	current_color_index = -1;
}



int
get_alt_color()
{
	return(current_color_index);
}

prop_struct_dump(ic)
     struct Prop_DataS *ic;
{
	int	nvarb, nparam, nfunct, i;

	nvarb = ic->ph_space_dim;
	fprintf(stderr,"ph_space_dim = %d        ", nvarb);
	nparam = ic->parameter_dim;
	fprintf(stderr,"parameter_dim = %d        ", nparam);
	nfunct = ic->function_dim;
	fprintf(stderr,"function_dim = %d       \n ", nfunct);
	fprintf(stderr,"direction = %d        ", ic->direction);
	fprintf(stderr,"iterations = %d       \n ", ic->iterations);
	fprintf(stderr,"prop_mode = %d        ", ic->prop_mode);
	fprintf(stderr,"prop_segsize = %d       \n ", ic->prop_segsize);
	fprintf(stderr,"symbol = %d        ", ic->symbol);
	fprintf(stderr,"table_color = %d        ", ic->table_color);
	fprintf(stderr,"sys_color = %d       \n ", ic->sys_color);
	fprintf(stderr,"mapping_flag = %d        ", ic->mapping_flag);
	fprintf(stderr,"inverse_flag = %d        ", ic->inverse_flag);
/*	fprintf(stderr,"inverse_guess = %d       \n ", ic->inverse_guess); 
	fprintf(stderr,"mc_iter = %d        ", ic->mc_iter);
	fprintf(stderr,"newt_iter = %d        ", ic->newt_iter); */
	fprintf(stderr,"f_iter = %d       \n ", ic->f_iter);
	fprintf(stderr,"time_step = %lg      ",ic->time_step);
	fprintf(stderr,"final_time = %lg     \n ",ic->final_time);
	fprintf(stderr,"diverg_cutoff = %lg      ",ic->diverg_cutoff);
/*	fprintf(stderr,"tolx = %lg      ",ic->tolx);
	fprintf(stderr,"tolf = %lg     \n ",ic->tolf); */
	fprintf(stderr,"\n");
	fprintf(stderr,"state = %lg     \n ",ic->state[0]);
	for(i=1;i<nvarb;i++) fprintf(stderr,"        %lg     \n ",ic->state[i]);
	fprintf(stderr,"\n");
	fprintf(stderr,"parameters = %lg     \n ",ic->parameters[0]);
	for(i=1;i<nparam;i++) fprintf(stderr,"             %lg     \n ",ic->parameters[i]);
	fprintf(stderr,"\n");
	fprintf(stderr,"min = %lg     \n ",ic->min[0]);
	for(i=1;i<nvarb;i++) fprintf(stderr,"      %lg     \n ",ic->min[i]);
	fprintf(stderr,"\n");
	fprintf(stderr,"max = %lg     \n ",ic->max[0]);
	for(i=1;i<nvarb;i++) fprintf(stderr,"      %lg     \n ",ic->max[i]);
/*	fprintf(stderr,"\n");
	fprintf(stderr,"fd_step = %lg     \n ",ic->fd_step[0]);
	for(i=1;i<nvarb-1;i++) fprintf(stderr,"          %lg     \n ",ic->fd_step[i]);  */
	fprintf(stderr,"\n");
	fprintf(stderr,"\n");
	fprintf(stderr,"manifold type = %d       \n ", ic->manifold->type);
	fprintf(stderr,"\n");
	fprintf(stderr," periodic_varb= %d     \n ",ic->manifold->periodic_varb[0]);
	for(i=1;i<nvarb-1;i++) fprintf(stderr,"          %d     \n ",ic->manifold->periodic_varb[i]);
	fprintf(stderr,"\n");
	fprintf(stderr," period_start= %lg     \n ",ic->manifold->period_start[0]);
	for(i=1;i<nvarb-1;i++) fprintf(stderr,"          %lg     \n ",ic->manifold->period_start[i]);
	fprintf(stderr,"\n");
	fprintf(stderr," period_end= %lg     \n ",ic->manifold->period_end[0]);
	for(i=1;i<nvarb-1;i++) fprintf(stderr,"          %lg     \n ",ic->manifold->period_end[i]);
}
