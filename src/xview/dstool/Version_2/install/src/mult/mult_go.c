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
 *
 * Computational routines for mult panel.
 *
 */

#include <stdio.h>

#include <pm.h>
#include <constants.h>
#include <complib.h>
#include <prop.h>
#include <iterate.h>
#include <memory.h>
#include <mult.h>
#include <mult_proj.h>


/*
 * mult_copy()
 *
 * procedure to copy final conditions of last orbit into initial conditions
 *
 * last modified: 7/30/91  paw
 */
void
  mult_copy()
{
  int total,dim, i, n=0;
  double *data, *dvector();

  total = *((int *) pm(GET, "Mult.Total", NULL));
  dim = *((int *) pm(GET, "Model.Varb_Dim", NULL)) +
         *((int *) pm(GET, "Model.Param_Dim", NULL));
  data = dvector(0,dim-1);
  for (i=0; i<total; i++)
    {
      pm(GET_LIST, "Mult.Fc", n, n+dim-1, data, NULL);
      pm(PUT_LIST, "Mult.Ic", n, n+dim-1, data, NULL);
      n += dim;
    }
  free_dvector(data, 0, dim-1);
  
  system_mess_proc(0,"Mult: Final copied to first.");
}



/*
 * load_mult(mc)
 *           mc is a pointer to the mult data struct
 *
 * This procedure fills the struct from the postmaster
 *
 */
load_mult(mc)
MULT_CNTL_ITEM *mc;
{
  double *dvector();
  int *ivector();

  mc->dim = *((int *) pm(GET, "Model.Varb_Dim", NULL)) +
    *((int *) pm(GET, "Model.Param_Dim", NULL));
  mc->images = *((int *) pm(GET, "Mult.Images", NULL));
  mc->loadchoice = *((int *) pm(GET, "Mult.Load_Choice", NULL));
  mc->trans = *((int *) pm(GET, "Mult.Transformation", NULL));
  mc->transparam = *((double *) pm(GET, "Mult.Trans_Param", NULL));
  mc->total = *((int *) pm(GET, "Mult.Total", NULL));

  mc->radius = dvector(0,mc->dim-1);
  mc->points = ivector(0,mc->dim-1);
  mc->center = dvector(0,mc->dim-1);
  mc->ic = dvector(0,mc->dim*mc->total-1);
  mc->fc = dvector(0,mc->dim*mc->total-1);
  pm(GET_LIST, "Mult.Center", 0, mc->dim-1, mc->center, NULL);
  pm(GET_LIST, "Mult.Radius", 0, mc->dim-1, mc->radius, NULL);
  pm(GET_LIST, "Mult.Points", 0, mc->dim-1, mc->points, NULL);
  pm(GET_LIST, "Mult.Ic", 0, mc->dim*mc->total-1, mc->ic, NULL);
  pm(GET_LIST, "Mult.Fc", 0, mc->dim*mc->total-1, mc->fc, NULL);
  
}


/*
 * free_mult(mc)
 *           mc is a pointer to a mult struct
 *
 * this procedure frees memory allocated to this struct
 * and writes to the postmaster relevant info
 *
 * last modified:  7/30/91  paw
 */
free_mult(mc)
MULT_CNTL_ITEM *mc;
{
  /* write info into postmaster! */
  pm(PUT, "Mult.Total", mc->total, NULL);
  pm(INIT, "Mult.Ic", mc->total*mc->dim,
     INIT, "Mult.Fc", mc->total*mc->dim,
     INIT, "Mult.Center", mc->dim,
     PUT_LIST, "Mult.Center", 0, mc->dim-1, mc->center,
     PUT_LIST, "Mult.Ic", 0, mc->total*mc->dim-1, mc->ic,
     PUT_LIST, "Mult.Fc", 0, mc->total*mc->dim-1, mc->fc, NULL);

  free_dvector(mc->radius, 0, mc->dim-1);
  free_ivector(mc->points, 0, mc->dim-1);
  free_dvector(mc->center, 0, mc->dim-1);
  free_dvector(mc->ic, 0, mc->dim*mc->total-1);
  free_dvector(mc->fc, 0, mc->dim*mc->total-1);
}




/*
 * mult_go()
 *
 * procedure to initiate flowing multiple orbits.
 *
 * last modified: 7/30/91  paw
 */
mult_go(dir)
int dir;
{
  MULT_CNTL_ITEM mult_cntl;
  
  load_mult(&mult_cntl);
  mult_flow_go(dir,&mult_cntl);
  free_mult(&mult_cntl);
}


/*
 * mult_forwards()
 *
 */
void
  mult_forwards()
{
    int i;
  if (*(int *) pm(GET, "Control.Pvm_Host", NULL) == TRUE) {
#ifdef USING_PVM
    pvm_mult_forwards();
#endif
    i = 1;			/* kludge to avoid empty body */
  }
  else
    mult_go(FORWARD);
}


/*
 * mult_backwards()
 *
 */
void
  mult_backwards()
{
  mult_go(BACKWARD);
}


/*
 * mult_continue()
 *
 */
void
  mult_continue()
{
  mult_go(CONTINUE);
}



/*
 * mult_load()
 *
 * procedure to load initial conditions.
 *
 */
mult_load()
{
  int status;
  MULT_CNTL_ITEM mult_cntl;

  load_mult(&mult_cntl);
  if (mult_cntl.loadchoice == 0)
    status = mult_load_rect_go(&mult_cntl);
  else status = mult_load_region_go(&mult_cntl);
  free_mult(&mult_cntl);
  return(status);
}




/*
 * mult_free_ic_fc(mc)
 *                 mc is pointer to mult control struct
 *
 * procedure to free memory where the ic and fc are stored
 *
 * last modified: 8/13/91  paw
 */
int
mult_free_ic_fc(mc)
MULT_CNTL_ITEM *mc;
{
  if (mc->total > 0)
    {
      free_dvector(mc->ic,0,mc->dim*mc->total-1);
      free_dvector(mc->fc,0,mc->dim*mc->total-1);
      mc->ic = mc->fc = NULL;
      mc->total = 0;
    }
}



/*
 * mult_load_rect_go(mc)
 *                   mc pointer to mult control struct
 *
 * procedure to setup a rectangle of initial conditions.
 *
 * last modified: 8/13/91  paw
 */
mult_load_rect_go(mc)
MULT_CNTL_ITEM *mc;
{
  int i,j,first,second,total,n_varb,n_param,n_funct,status=0;
  double delta, *x, y, z, *dvector();

  get_n_all_types(&n_varb,&n_param,&n_funct);
  
  pm( GET_LIST, "Selected.Varb_Ic", 0, n_varb-1, mc->center, NULL);
  pm( GET_LIST, "Selected.Param_Ic", 0, n_param-1, mc->center+n_varb, NULL);

  first = second = -1;
  for (i=0; i<mc->dim; i++) 
    {
      if (mc->points[i]>1) 
	{
	  if (first==-1) first = i;
	  else if (second==-1) second = i;
	}
    }

  if (first == -1) 
    total = 1;
  else if (second == -1) 
    total = mc->points[first];
  else
      total = 2 * (mc->points[first] + mc->points[second])-4;

  if (mc->total > 0)
      mult_free_ic_fc(mc);
      
  if (!(mc->ic = dvector(0,total*mc->dim-1))) status = -1;
  if (!(mc->fc = dvector(0,total*mc->dim-1))) status = -1;
  if (status == -1) 
    {
      mult_free_ic_fc(mc);
      system_mess_proc(1,
	      "mult_load_rect_go: Memory allocation failure for storing initial conditions.");
    }
  else
    {
      mc->total = total;
      x = mc->ic;
      for (i=0; i<total; i++)
	{
	  for (j=0; j<mc->dim; j++)
	    *(x++) = mc->center[j];
	}
      if ((first != -1) && (second == -1))
	{
	  delta = mc->radius[first] / (mc->points[first]-1) * 2;
	  y = mc->center[first] - mc->radius[first];
	  x = mc->ic;
	  for (i=0; i<total; i++)
	    {
	      x[first] = y;
	      y = y+delta;
	      x += mc->dim;
	    }
	}
      else if ((first != -1) && (second != -1))
	{
	  delta = mc->radius[first] / (mc->points[first]-1) * 2;
	  y = mc->center[first] - mc->radius[first];
	  z = mc->center[second] - mc->radius[second];
	  x = mc->ic;
	  for (i=0; i<mc->points[first]-1; i++)
	    {
	      x[first] = y;
	      x[second] = z;
	      y += delta;
	      x += mc->dim;
	    }
	  delta = mc->radius[second] / (mc->points[second]-1) * 2;
	  y = mc->center[first] + mc->radius[first];
	  z = mc->center[second] - mc->radius[second];
	  for (i=0; i<mc->points[second]-1; i++)
	    {
	      x[first] = y;
	      x[second] = z;
	      z += delta;
	      x += mc->dim;
	    }
	  delta = mc->radius[first] / (mc->points[first]-1) * 2;
	  y = mc->center[first] + mc->radius[first];
	  z = mc->center[second] + mc->radius[second];
	  for (i=0; i<mc->points[first]-1; i++)
	    {
	      x[first] = y;
	      x[second] = z;
	      y -= delta;
	      x += mc->dim;	    
	    }
	  delta = mc->radius[second] / (mc->points[second]-1) * 2;
	  y = mc->center[first] - mc->radius[first];
	  z = mc->center[second] + mc->radius[second];
	  for (i=0; i<mc->points[second]-1; i++)
	    {
	      x[first] = y;
	      x[second] = z;
	      z -= delta;
	      x += mc->dim;
	    }
	}
      mult_apply_trans(mc);
    }
  
  return(status);
}

/*
 * mult_apply_trans()
 *
 * procedure to apply  transformation to initial points
 *
 * last modified:  7/29/91  paw
 */
mult_apply_trans(mc)
MULT_CNTL_ITEM *mc;
{
  int n_varb, n_param, n_funct, i, j;
  double *p, *x, *fx, *fp, *dvector();
  
  if (mc->trans > 0)
    {
      get_n_all_types(&n_varb,&n_param,&n_funct);
      fx = dvector(0,n_varb-1);
      fp = dvector(0,n_param-1);
      
      x = mc->ic;
      for (i=0; i<mc->total; i++)
	{
	  p = x+n_varb;
	  MULT_PROJS[mc->trans-1].proj_fn(x, fx, n_varb, p, fp, n_param, mc->center, 
					  mc->radius, mc->transparam);
	  for (j=0; j<n_varb; j++)
	    *(x++) = fx[j];
	  for (j=0; j<n_param; j++)
	    *(x++) = fp[j];
	}
      free_dvector(fx,0,n_varb-1);
      free_dvector(fp,0,n_param-1);
    }
}

      
      


/*
 * mult_load_region_go(mc)
 *                     mc pointer to mult control struct
 *
 * procedure to setup a region of initial conditions.
 *
 * last modified: 7/17/91  paw
 */
mult_load_region_go(mc)
MULT_CNTL_ITEM *mc;
{
  int *ivector(), i, j,*index,total,n_varb,n_param,n_funct,status = 0;
  double *dvector(), *x, *delta, *ic_ptr;

  x = dvector(0,mc->dim-1);
  delta = dvector(0,mc->dim-1);
  index = ivector(0,mc->dim-1);

  get_n_all_types(&n_varb, &n_param, &n_funct);
  pm( GET_LIST, "Selected.Varb_Ic", 0, n_varb-1, mc->center, NULL);
  pm( GET_LIST, "Selected.Param_Ic", 0, n_param-1, mc->center+n_varb, NULL);
  
  total = 1;
  for (i=0; i<mc->dim; i++) 
    {
      total = total * mc->points[i];
      index[i] = 1;
      delta[i] = 0.0;
      x[i] = mc->center[i];
    }

  mult_free_ic_fc(mc);
  
  if (!(mc->ic = dvector(0,total*mc->dim-1))) status = -1;
  if (!(mc->fc = dvector(0,total*mc->dim-1))) status = -1;
  if (status == -1) 
    {
      free_dvector(mc->ic,0,total*mc->dim-1);
      free_dvector(mc->fc,0,total*mc->dim-1);
      mc->ic = mc->fc = NULL;
      mc->total = 0;
      system_mess_proc(1,
	   "mult_load_region_go: Memory allocation failure for storing initial conditions.");
    }
  else
    {
      
      mc->total = total;
      mc->dim = mc->dim;
      ic_ptr = mc->ic;
      
      for (j=0; j<total; j++)          
	{
	  /* compute delta */
	  for (i=0; i<mc->dim; i++)
	    {
	      if (mc->points[i] != 1)
		{
		  delta[i] = mc->radius[i] * (2.0*index[i] - mc->points[i]-1.0) / (mc->points[i]-1.0);
		  ic_ptr[i] = mc->center[i] + delta[i];
		}
	      else
		{
		  ic_ptr[i] = mc->center[i];
		}
	    }
	  
	  /* increment indices */
	  i = 0;
	  while (i<mc->dim && index[i] == mc->points[i])
	    {
	      index[i] = 1;
	      i++;
	    }
	  if (i<mc->dim) index[i]++;
	  
	  ic_ptr += mc->dim;
	}
      
      mult_apply_trans(mc);
    }
  
  free_dvector(x,0,mc->dim-1);
  free_dvector(delta, 0, mc->dim-1);
  free_ivector(index, 0, mc->dim-1);
  return(status);
}


/*
 * mult_flow_go(direction,mc)
 *              direction tells the direction of the flow
 *              mc is a pointer to a control structure
 *
 * procedure to flow multiple orbits displaying all points
 * using mult_cntl struct
 *
 * last modified: 7/17/91  paw
 */
mult_flow_go(direction,mc)
int direction;
MULT_CNTL_ITEM *mc;
{
  int i,j,k,n_traj_colors,status,color[3],mem_stat=0, max_num_traj;
  double *ic_ptr,*fc_ptr;
  struct Prop_DataS prop_cntl;
  static int last_dir = FORWARD;
  memory mult_mem = NULL;

  if (mc->total <= 0)
    {
      if (mc->loadchoice == 0)
	status = mult_load_rect_go(mc);
      else status = mult_load_region_go(mc);
      if (status==-1) return;
    }

  /* setup propagation structure */
  prop_cntl.plot_traj = (void *) pm(GET, "Flow.Plot_Function", NULL);
  prop_cntl.ph_space_dim = *((int *) pm( GET, "Model.Varb_Dim", NULL ));
  prop_cntl.parameter_dim = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  prop_cntl.function_dim = *((int *) pm( GET, "Model.Funct_Dim", NULL ));
  prop_cntl.prop_mode = *((int *) pm(GET, "Flow.Stopping_Condition", NULL));
  prop_cntl.f_skip = prop_cntl.f_iter = 
    *((int *) pm(GET, "Flow.Skip_Size", NULL));

  if (direction == CONTINUE) 
    prop_cntl.start_to_save = -1;
  else 
    prop_cntl.start_to_save = 
      *((int *) pm(GET, "Flow.Start_Save_Points", NULL));
  
  max_num_traj = *((int *) pm(GET, "Flow.Total_Iterates", NULL));
  if (mc->images == 1) max_num_traj -= abs(prop_cntl.start_to_save);

  prop_cntl.iter_request = 
    *((int *) pm(GET, "Flow.Total_Iterates", NULL)) - 
      abs(prop_cntl.start_to_save);
  if (prop_cntl.start_to_save != 0) prop_cntl.iter_request++;

  if (max_num_traj<0) return;
  
  if (0 != allocate_integ(&prop_cntl,max_num_traj)) 
    {
      system_mess_proc(1,"traj_mem_orb : Memory Allocation Failure \n");
      return;
    }

  if (*((int *) pm(GET, "Defaults.Recording", NULL)) == 0)
    mult_mem = (memory) pm(GET, "Memory.Mult", NULL);
  color[1] = prop_cntl.sys_color = 
    *((int *) pm(GET, "Color.Pick_Color_Choice", NULL));
  color[2] = prop_cntl.symbol = 
    get_symbol_code( *((int *) pm(GET, "Defaults.Symbol_Index", NULL)), 
		    *((int *) pm(GET, "Defaults.Size_Index", NULL)));
  n_traj_colors = *((int *) pm(GET, "Color.Traj_Colormap_Size", NULL));
  
  switch (direction) 
    {
    case FORWARD:
    case BACKWARD:
      bump_color();
      prop_cntl.table_color = get_alt_color();
      color[0] = prop_cntl.table_color;
      last_dir = direction;
      pm(GET_LIST, "Selected.Param_Ic", 0,  prop_cntl.parameter_dim-1, 
	 prop_cntl.parameters, NULL);
      memory_start_new_flow(mult_mem, mc->total, 0, 0, 
			    prop_cntl.iter_request+1, 0, 0);
      ic_ptr = mc->ic;
      break;
    case CONTINUE:
      prop_cntl.table_color = get_alt_color();
      color[0] = prop_cntl.table_color;
      pm(GET_LIST, "Selected.Param_Ic", 0,  prop_cntl.parameter_dim-1, 
	 prop_cntl.parameters, NULL);
      /* paw NEED TO CHECK IF CURRENT TRAJECTORY IS OK, OR WHETHER SHOULD START NEW ONE! */
      ic_ptr = mc->fc;
      break;
    }
  
  prop_cntl.direction = last_dir;
  status = comp_alloc(&prop_cntl);
  if (status == - 1)
    {
      system_mess_proc(1,"mult_flow_go: comp_alloc failed - OUT OF MEMORY");
      return;
    }
  comp_def_setup(&prop_cntl);

  if ( prop_cntl.mapping_flag )
    {	
      prop_cntl.inverse_flag = *((int *)pm( GET, "Model.Inverse_Flag", NULL ));
      prop_cntl.time_step = ( prop_cntl.direction == FORWARD ) ? 
        STEP_FORW: STEP_BACK;
      prop_cntl.inverse = (void *) pm( GET, "Model.Inverse", NULL );
      prop_cntl.dxdt = (void *) pm(GET, "Model.DfDt", NULL);
      prop_cntl.fstop = NULL;
      prop_cntl.f_iter = *((int *) pm( GET, "Flow.Skip_Size", NULL ));
    }
  else				/* vector field integration */
    { 
      prop_cntl.final_time = *((double *) pm( GET, "Flow.Final_Time", NULL ));
      prop_cntl.f_skip = *((int *) pm( GET, "Flow.Skip_Size", NULL ));
    }

  fc_ptr = mc->fc;

  for (k=0; k<mc->images && status >= 0 && mem_stat==0; k++)
    {
      /* Set memory to first trajectory of current flow */
      memory_set_cur_traj(mult_mem,1);
  
      for (i=0; i<mc->total && status >= 0 && mem_stat==0; i++)
	{
	  /* THIS LINE DOES AUTOMATIC PICK COLORING,
	     REMOVE TO RESTORE REGULAR OPERATION - paw 
	  prop_cntl.sys_color = color[1] = i % n_traj_colors; */

	  /* copy initial condition into prop_cntl */
	  for (j=0; j<prop_cntl.ph_space_dim; j++)
	    {
	      prop_cntl.state[j] = ic_ptr[j];
	      prop_cntl.traj_segment[0][j] = ic_ptr[j];
	    }
	  for (j=0; j<prop_cntl.parameter_dim; j++)
	    prop_cntl.parameters[j] = ic_ptr[prop_cntl.ph_space_dim+j];
	  
	  /* now flow this initial condition */
	  if (prop_cntl.mapping_flag)
	    {
	      status = iterate(&prop_cntl);
	    }
	  else
	    {
	      prop_cntl.estim_step = prop_cntl.time_step = 
		*((double *) pm( GET, "Flow.Stepsize", NULL ));
	      status = integrate(&prop_cntl);
	    }

	  /* add the computed points to memory */
	  if (++prop_cntl.iterations > 0)
	    {
	      for (j=0; j<prop_cntl.iterations && mem_stat==0; j++)
		mem_stat = memory_add_point(mult_mem, 
					    prop_cntl.traj_segment[j], 
					    prop_cntl.parameters,
					    color, NULL, NULL, NULL, NULL);
	      for (j=0; j<prop_cntl.ph_space_dim; j++)
		*(fc_ptr++)= prop_cntl.traj_segment[prop_cntl.iterations-1][j];
	      for (j=0; j<prop_cntl.parameter_dim; j++)
		*(fc_ptr++) = prop_cntl.parameters[j];
	    }
	  
	  ic_ptr += mc->dim;
	  memory_next_traj(mult_mem);
	}
      ic_ptr = fc_ptr = mc->fc;
      
      /* next loop is a continue, so reset start_to_save and iter_request 
	 - paw 8/24/92 */
      if (k==0)
	{
	  prop_cntl.iter_request = *((int *) pm(GET, "Flow.Total_Iterates", 
						NULL));
	  prop_cntl.start_to_save = -1;
	}

      
    }
  
  comp_free(&prop_cntl);
  release_integ(&prop_cntl,max_num_traj);
}






