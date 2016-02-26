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
 * Code written to continue periodic orbits
 * based on Poincare map.
 *
 *
 * paw 4/22/92
 */

#include <stdio.h>
#include <pm.h>
#include <constants.h>
#include <defaults.h>
#include <iterate.h>
#include <fixptlib.h>

extern int porbit_var, porbit_param, porbit_period;
extern int porbit_numsteps, porbit_algorithm;
extern double porbit_value, porbit_stepsize;

static struct Prop_DataS pc;

/*
 * porbit_go()
 * 
 * Top routine for periodic orbit panel
 *
 */
int
  porbit_go()
{
  if (porbit_algorithm==0)
    porbit_newton_go();
  else if (porbit_algorithm==1)
    porbit_attracting_go();
  else
    system_mess_proc(0,"porbit_go: Invalid algorithm selected");
}


int
  porbit_newton_go()
{
  int i,status = 0;
  struct Fixpt_DataS fp;
  double pval = *((double *) pm(GET, "Selected.Param_Ic", porbit_param, NULL));

  /* print msg */
  fprintf(stdout,"\nFinding periodic orbits.\n");

  /* set up interrupts 
  pm( INIT, "Flow.Interrupt",            
      PUT, "Flow.Interrupt", FALSE, NULL ); */

  /* initialization of data structures */
  porbit_newton_init_data(&fp,&pc);

  /* FIRST find a periodic orbit */
  fp.fp_map_period = fp.map_period;
  status = porbit_newt(&fp);

  /* SECOND, use CONTINUATION according to instructions */
  for (i=0; i<porbit_numsteps && status==1; i++)
    {
      porbit_plot_save(&fp);
      /* increment parameter value */
      pval += porbit_stepsize;
      fp.prop_cntl.parameters[porbit_param] = pval; 

      /* find periodic orbit */
      fp.fp_map_period = fp.map_period;
      status = porbit_newt(&fp);
    }

  if (status==1) porbit_plot_save(&fp);
  else fprintf(stdout,"periodic orbit - no convergence ...\n");

  /* display the points */
  mem_all_win(fp.memory);
  
  release_fp(&fp,0); /* free assigned memory */
  comp_free(&pc);
  release_integ(&pc);
  comp_free(&pc);
  release_integ(&pc);

  return(status);
}


/* initialization of data structures */
int
  porbit_newton_init_data(fp,pc)
struct Fixpt_DataS *fp;
struct Prop_DataS *pc;
{
  int i,status=0, max_num_traj;
  extern int porbit_poincare_map(), porbit_jacobian();

  /* initialize fixed point structure */
  pm(PUT, "Fixed.Algorithm", NEWTON, NULL);
  pm(PUT, "Fixed.Guess", 1, NULL); /* try only given point */
  pm(PUT, "Fixed.Mc_Guesses", 1, NULL); 
  pm(PUT, "Fixed.Map_Period", 2*porbit_period, NULL); /* setup period */
  status = fp_load(fp, 0);  /* initialize fix point struct */

  fp->prop_cntl.function = porbit_poincare_map;
  fp->prop_cntl.dfdx = NULL; /* fp->prop_cntl.dfdx = porbit_jacobian; */
  fp->prop_cntl.mapping_flag = TRUE;
  /* use continuation memory object! */
  fp->memory = (memory) pm(GET, "Memory.Cont", NULL);
  if (!fp->memory) 
    {
      status =  *( (int *) pm(INIT, "Memory.Cont", NULL));
      if (status) return(-1);
      fp->memory = (memory) pm(GET, "Memory.Cont", NULL);
    }

  /* set up starting point */
  pm( GET_LIST, "Selected.Param_Ic", 0, fp->prop_cntl.parameter_dim-1, 
     fp->prop_cntl.parameters, NULL); 
  pm( GET_LIST, "Selected.Varb_Ic", 0, fp->prop_cntl.ph_space_dim-1,
     fp->x1, NULL); 
  fp->x1[porbit_var] = porbit_value; /* start on section by projection! (could flow to section) */

  /* initialize integration structure for Poincare map part */
  pc->direction = FORWARD;
  pc->iterations = 0;
  max_num_traj = pc->iter_request = *((int *) pm( GET, "Flow.Total_Iterates", NULL ));
  if (0 != allocate_integ(pc, max_num_traj)) 
    {system_mess_proc(1,"porbit_go : Memory Allocation Failure \n");
     return(-1); }

  /* setup correct stopping conditions */
  pc->prop_mode = PROP_FSTOP;
  for (i=0; i<pc->function_dim; i++) pm(PUT, "Flow.Funct_Events", i, FALSE, NULL);  
  for (i=0; i<pc->ph_space_dim; i++) pm(PUT, "Flow.Varb_Events", i, FALSE, NULL);
  pm(PUT, "Flow.Varb_Events", porbit_var, TRUE, NULL);
  pm(PUT, "Flow.Varb_Event_Values", porbit_var, porbit_value, NULL);

  comp_alloc(pc);
  comp_def_setup(pc);
  pc->prop_segsize = -pc->prop_segsize;

  return(status);

}

int
porbit_plot_save(fp)
struct Fixpt_DataS *fp;
{
  int i,j, color[3], var_dim = fp->prop_cntl.ph_space_dim-1;
  int format = *((int *) pm(GET, "Defaults.Precision", NULL));
  int status = 0;

  /* print information */
  fprintf(stdout,"Periodic orbit: ");
  for (j=0; j<=var_dim; j++) fprintf(stdout,"%.*lg  ",format,fp->x1[j]);
  fprintf(stdout,"\n    parameters: ");
  for (j=0; j<fp->prop_cntl.parameter_dim; j++) fprintf(stdout,"%.*lg  ",format,fp->prop_cntl.parameters[j]);
  fprintf(stdout,"\nPeriod =  %.*lg\n",format, fp->fx[var_dim]);

  /* eigenvalue information */
  /* fp_eval(fp);   */
  /* for a map, the jacobian computed was for  f^r(x)-x */
  for (j=0; j<var_dim-1; j++) fp->jacobian[j][j] += 1.0;
  for (i=0; i<var_dim-1; i++)
    for (j=0; j<var_dim-1; j++) fp->jact[i+1][j+1] = fp->jacobian[i][j];
  status = rg(var_dim-1, var_dim-1, fp->jact, fp->eval[0]-1, fp->eval[1]-1, 
	      0, NULL, fp->indx, fp->x3); 
  fprintf(stdout,"Eigenvalues: ");
  for (j=0; j<var_dim-1; j++) 
    fprintf(stdout,"%.*lg+%.*lgI ",format,fp->eval[0][j],format,fp->eval[1][j]);
  fprintf(stdout,"\n");
  porbit_get_type(fp);
  fprintf(stdout,"\n");

  /* setup color and symbol */
  color[0] = fp->prop_cntl.table_color;
  color[1] = fp->prop_cntl.sys_color;
  color[2] = fp->prop_cntl.symbol;

  /* add to memory object */
  for (j=0; j<var_dim-1; j++)
    {
      fp->prop_cntl.workspace[2*j] = fp->eval[0][j];
      fp->prop_cntl.workspace[2*j+1] = fp->eval[1][j];
    }
  if (memory_start_new_flow(fp->memory,1,0,0,fp->fp_map_period/2+1, 2*var_dim-2, 1)) 
    return(-1);
  memory_add_point(fp->memory, fp->x1, fp->prop_cntl.parameters, color, NULL, NULL,
		   fp->prop_cntl.workspace, &fp->fptype);
  for (j=1; j<fp->fp_map_period/2; j++)
    memory_add_point(fp->memory, fp->prop_cntl.traj_segment[j], fp->prop_cntl.parameters,
		     color, NULL, NULL, NULL, NULL);

  /* plot the point */
  traj_plot(&(fp->prop_cntl), 0, fp->fp_map_period/2-1, fp->prop_cntl.workspace);

}

int
porbit_get_type(fp)
struct Fixpt_DataS *fp;
{
  int i;
  double fabs(),mods, max_ev=0.0, min_ev=2.0;

  /* analyse algorithm of periodic orbit, look for period doubling and Hopf bifurcations */

  for (i=0; i<fp->prop_cntl.ph_space_dim-2; i++)
    {
      mods = fp->eval[0][i]*fp->eval[0][i]+fp->eval[1][i]*fp->eval[1][i];
      if (mods > max_ev) max_ev = mods;
      if (mods < min_ev) min_ev = mods;
    }

  if (max_ev<1.0-EVAL_TOLERANCE)
    {  
      fp->prop_cntl.sys_color = SYS_BLUE;
      fp->fptype = -1;
      fprintf(stdout,"Stability: attracting, %lg\n", max_ev);
      fp->prop_cntl.symbol = LARGE_POINT;
    }
  else if (min_ev>1.0+EVAL_TOLERANCE)
    {  
      fp->prop_cntl.sys_color = SYS_RED;
      fp->fptype = 1;
      fprintf(stdout,"Stability: repelling, %lg\n", min_ev);
      fp->prop_cntl.symbol = LARGE_POINT;
    }
  else if (min_ev<1.0-EVAL_TOLERANCE && max_ev>1.0+EVAL_TOLERANCE)
    {  
      fp->prop_cntl.sys_color = SYS_GREEN;
      fp->fptype = 1;
      fprintf(stdout,"Stability: saddle.\n");
      fp->prop_cntl.symbol = LARGE_POINT;
    }
  else
    {
      fp->prop_cntl.sys_color = SYS_GREEN;    
      fp->fptype = 0;
      fprintf(stdout,"Stability: indeterminate.\n");
      fp->prop_cntl.symbol = MED_CROSS;
    }

  fp->prop_cntl.table_color = -1;
}


/* Poincare map masquerading as a regular map! */
int
  porbit_poincare_map(f,x,p)
double f[],x[],p[];
{
  int i, status=0;
  extern struct Prop_DataS pc;

  /* setup for integration using x and p */
  for (i=0; i<pc.ph_space_dim; i++) pc.state[i] = pc.traj_segment[0][i] = x[i];
  for (i=0; i<pc.parameter_dim; i++) pc.parameters[i] = p[i];

  /* fprintf(stdout,"poincare map: initial \n");
  for (i=0; i<pc.ph_space_dim; i++) fprintf(stdout,"%lg ", x[i]);
  for (i=0; i<pc.parameter_dim; i++) fprintf(stdout,"%lg ",p[i]);
  fprintf(stdout,"\n"); */

  /* do integration */
  if( pc.mapping_flag) /* mapping flag is TRUE */
    {	
      pc.time_step 	     = ( pc.direction == FORWARD ) ? STEP_FORW: STEP_BACK;
      pc.fstop               = NULL;
      pc.f_iter              = *((int *) pm( GET, "Flow.Skip_Size", NULL ));
      status = iterate(&pc); 
    }
  else				/* vector field integration */
    { 
      pc.estim_step = pc.time_step = 
                *((double *) pm( GET, "Flow.Stepsize", NULL ));
      pc.final_time   = *((double *) pm( GET, "Flow.Final_Time", NULL ));
      pc.f_skip       = *((int *) pm( GET, "Flow.Skip_Size", NULL ));
      status = (int) integrate(&pc);  
    }

  /* take last point and put it in f */
  for (i=0; i<pc.ph_space_dim; i++) f[i] = pc.traj_segment[pc.iterations][i];

  /* fprintf(stdout,"poincare map: final \n");
  for (i=0; i<pc.ph_space_dim; i++) fprintf(stdout,"%lg ", f[i]);
  for (i=0; i<pc.parameter_dim; i++) fprintf(stdout,"%lg ",p[i]);
  fprintf(stdout,"\n\n");  */

  return(status);  
}


/* Compute jacobian of poincare map, ie the reduced system! */
/* The row and column of the jacobian matrix corresponding to the */
/* poincare section variable is simply eliminated from the jacobian matrix */
/* It also computes the iterates along the map for x1 and puts in traj_segment */
int
  porbit_jac(fp)
struct Fixpt_DataS *fp;
{
  /* this needs to communicate with the choice of cross-section ! */
  int i,j, status = 0, var_dim  = fp->prop_cntl.ph_space_dim - 1;
  double *x, *v, *work;

  x = fp->prop_cntl.workspace;
  v = x+var_dim+1;
  work = v+var_dim+1;

  /* compute image of point x1 and put in traj segment */
  for (i=0; i<=var_dim; i++) fp->prop_cntl.traj_segment[0][i] = fp->x1[i];
  for (i=0; i<fp->fp_map_period/2; i++)
    {
      /* iter_forw(fp->prop_cntl.function, 2, fp->prop_cntl.traj_segment[i+1],
	 fp->prop_cntl.traj_segment[i], fp->prop_cntl.parameters, 
	 fp->prop_cntl.ph_space_dim, 1.0, fp->prop_cntl.workspace, 
	 fp->prop_cntl.manifold); */
      status = fp->prop_cntl.function(x,fp->prop_cntl.traj_segment[i], fp->prop_cntl.parameters);
      project( var_dim, x, fp->prop_cntl.manifold );
      status = fp->prop_cntl.function(fp->prop_cntl.traj_segment[i+1], x, fp->prop_cntl.parameters);
      project( var_dim, fp->prop_cntl.traj_segment[i+1], fp->prop_cntl.manifold );
    }

  /* make fx contain final point */
  for (j=0; j<=var_dim; j++) fp->fx[j] = fp->prop_cntl.traj_segment[i][j];

  /* now do FORWARD finite differencing to compute the Jacobian */
  for (j=0; j<=var_dim; j++) x[j] = fp->x1[j]; /* include time */
  for (j=0; j<var_dim; j++)
    {
      if (j != porbit_var)
	{
	  x[j] += fp->x2[j];
	  iter_forw(fp->prop_cntl.function, fp->fp_map_period, v, x, fp->prop_cntl.parameters, 
		    fp->prop_cntl.ph_space_dim, 1.0, work, fp->prop_cntl.manifold);
          if (j<porbit_var)
	    {
	      for(i=0; i<porbit_var; i++)
		fp->jacobian[i][j] = (v[i]-fp->fx[i])/fp->x2[j];
              for(i++; i<var_dim; i++)
		fp->jacobian[i-1][j] = (v[i]-fp->fx[i])/fp->x2[j];
	    }
	  else
	    {
	      for(i=0; i<porbit_var; i++)
		fp->jacobian[i][j-1] = (v[i]-fp->fx[i])/fp->x2[j];
              for(i++; i<var_dim; i++)
		fp->jacobian[i-1][j-1] = (v[i]-fp->fx[i])/fp->x2[j];
	    }
	  x[j] = fp->x1[j];
	}
    }
  
  /* we want jacobian of F(x)-x, so fix jacobian and fx */
  for (i=0; i<var_dim; i++) 
    {
      fp->jacobian[i][i] -= 1.0;
      fp->fx[i] -= fp->x1[i];
    }
  fp->fx[i] -= fp->x1[i]; /* include time */
  fp->fx[porbit_var] = 0; /* should already be zero */
  
  return(status);
}


/* same as mnewt, but takes advantage of Poincare map knowledge ! */
/* the initial guess for the fixed point is in x1, and the
   computed solution is returned there  */
porbit_newt(fp)
     struct Fixpt_DataS *fp;
{
  int	i, k, var_dim;
  double 	errf, err, tolx10, fabs();
  
  int 	status = 0;
  
  var_dim = fp->prop_cntl.ph_space_dim - 1;
  tolx10 = fp->varb_conv / 10;
  
  for (k=0; k<fp->nsteps && status == 0; k++) 
    {
      for (i=0; i<var_dim; i++) fp->x2[i] = fp->fd_step;
      status = porbit_jac(fp);
      if (status) return(-1);
    
    /* Debugging code */
       fprintf(stderr,"NTRIAL=%d\n",k); fprintf(stderr,"usrfun x: ");
       for(i=0;i<=var_dim;i++) fprintf(stderr,"%lg ",fp->x1[i]);    
       fprintf(stderr,"\nusrfun f: ");
       for(i=0;i<=var_dim;i++) fprintf(stderr,"%lg ",fp->fx[i]); 
       fprintf(stderr,"\nusrfun jacobian:");
       { int j;
       for(j=0;j<var_dim-1;j++) {
       for(i=0;i<var_dim-1;i++) fprintf(stderr,"%lg ",fp->jacobian[j][i]);     
       fprintf(stderr,"\n");
       } }
     /*  */
        
      errf = 0.0;
      for(i=0; i<var_dim; i++) errf += fabs(fp->fx[i]);
    
      if (errf <= fp->funct_conv) 
	{
	  /* this is good enough ! */
	  fprintf(stdout,"porbit_newt: convergence in %d steps.\n",k);
	  status = 1;
	  k=fp->nsteps;
	}
      else 
	{
	  /* improve guess */
	  status = ludcmp(fp->jacobian,var_dim-1,fp->indx,&err);
	  if (status==-1) k=fp->nsteps;
	  else if (status==1) 
	    {
	      /* singular matrix, machine accuracy lost */
	      status=3;
	      k=fp->nsteps;
	    }
	  else 
	    {
	      for (i=0; i<porbit_var; i++) fp->x3[i] = fp->fx[i];
	      for (i++; i<var_dim; i++) fp->x3[i-1] = fp->fx[i];
	      lubksb(fp->jacobian,var_dim-1,fp->indx,fp->x3);
	      err = 0;
	      for (i=0; i<porbit_var; i++)
		{
		  err += fabs(fp->x3[i]);
		  fp->x1[i] -= fp->x3[i];
		}
	      for (i++; i<var_dim; i++)
		{
		  err += fabs(fp->x3[i-1]);
		  fp->x1[i] -= fp->x3[i-1];
		} 
	      if (err <= fp->varb_conv) 
		{
		  k=fp->nsteps;
		  status = 4;
		}
	      else 
		{
		  for (i=0; i<var_dim; i++) /* push you off a saddle ?! */
		    if (i != porbit_var && fabs(fp->fx[i]) < tolx10 ) fp->x1[i] -= tolx10;
		  project(var_dim, fp->x1, fp->prop_cntl.manifold);
		}
	    }
	}
      
    }
  if (status==0) status=2; /* hit max iters before converging */
  
  return (status);
}



int
  porbit_attracting_go()
{
  int status=0, i, color[3], max_num_traj;
  memory traj_mem;
  struct Prop_DataS prop_cntl;
  double time_save;

  int j, prop_mode_save;

  /* print msg */
  system_mess_proc(0,"Finding attracting orbits.");

  /* set up interrupts 
  pm( INIT, "Flow.Interrupt",            
      PUT, "Flow.Interrupt", FALSE, NULL ); */

  prop_cntl.direction = FORWARD;
  traj_mem = pm(GET, "Memory.Traj_Mem_Ptr", NULL);

  prop_cntl.iterations = 0;
  prop_cntl.start_to_save = *((int *) pm(GET, "Flow.Start_Save_Points", NULL));
  max_num_traj = prop_cntl.iter_request = 
    *((int *) pm( GET, "Flow.Total_Iterates", NULL )) - prop_cntl.start_to_save;
  if (prop_cntl.start_to_save > 0)
    prop_cntl.iter_request++;

  if (max_num_traj<0) return(status);

  if (0 != allocate_integ(&prop_cntl, max_num_traj)) 
    {
      system_mess_proc(1,"porbit_attracting_go: Memory Allocation Failure \n");
      return(-1); 
    }

  pm( GET_LIST, "Selected.Varb_Ic", 0, prop_cntl.ph_space_dim-1, 
     prop_cntl.state, NULL);
  pm( GET_LIST, "Selected.Param_Ic", 0, prop_cntl.parameter_dim-1, 
     prop_cntl.parameters, NULL); 
  prop_cntl.symbol = get_symbol_code( *((int *) pm(GET, "Defaults.Def_Symbol_Index", NULL)), 
				     *((int *) pm(GET, "Defaults.Def_Size_Index", NULL)));
  prop_cntl.table_color = (int) get_alt_color();
  prop_cntl.sys_color = *((int *) pm( GET, "Color.Pick_Color_Choice", NULL));
  color[0] = prop_cntl.table_color;
  color[1] = prop_cntl.sys_color;
  color[2] = prop_cntl.symbol;

  /* FIRST find flow forwards */
  time_save = prop_cntl.state[prop_cntl.ph_space_dim];
  status = compute_orbit(&prop_cntl);
  if (prop_cntl.iterations < 0) status = -1;

  /* SECOND, use CONTINUATION and last final condition */
  for (i=0; i<porbit_numsteps && status==0; i++)
    {
      memory_start_new_flow( traj_mem, 1, 0, 0, prop_cntl.iter_request+1, 0, 0);
      memory_add_point(traj_mem, NULL, prop_cntl.parameters, color, NULL, NULL, NULL, NULL);
      memory_add_points(traj_mem, prop_cntl.iterations+1, prop_cntl.traj_segment, NULL, NULL, NULL, NULL);
      /* copy final point to initial excluding time (this is  unnecessary) */
      for (j=0; j<prop_cntl.ph_space_dim-1; j++)
	prop_cntl.state[j] = prop_cntl.traj_segment[prop_cntl.iterations][j];
      prop_cntl.state[prop_cntl.ph_space_dim-1] = time_save;
      prop_cntl.parameters[porbit_param] += porbit_stepsize; 

      /* find next flow */
      status = compute_orbit(&prop_cntl);
      if (prop_cntl.iterations < 0) status = -1;
    }

  if (status==0)
    {
      memory_start_new_flow( traj_mem, 1, 0, 0, prop_cntl.iter_request+1, 0, 0);
      memory_add_point(traj_mem, NULL, prop_cntl.parameters, color, NULL, NULL, NULL, NULL);
      memory_add_points(traj_mem, prop_cntl.iterations+1, prop_cntl.traj_segment, NULL, NULL, NULL, NULL);
    }

  /* free up memory and restore postmaster */
  release_integ(&prop_cntl);

  return(status);
}





