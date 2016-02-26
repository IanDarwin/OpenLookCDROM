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
#include <stdlib.h>
#include <math.h>
#include <constants.h>
#include "continue.h"

extern struct  Cont_Cntl_Ds            cont_ds;

#define WORK_SIZE 8000

/* 
 * cont_proc() is the main driver for the continuation code.  It calls pitcon to find
 * and move along a 1D manifold of solutions to the vector equation F(x, lambda) = 0.
 * Refer to the pitcon code and references for more information on pitcon.  The
 * cont_proc() function is also used to locate certain degenerate singularities by
 * choosing monte-carlo initial conditions and hoping newton's method converges.
 * If the flag  *((int *) pm(GET, Cont.Search, NULL)) is TRUE,
 * the user wants to search for singularities;  otherwise, the algorithm will try to locate
 * and follow manifolds of solutions to F=0.
 */
int
cont_proc()
{
  int		i, j, nvar, liw, lrw, direction, varb_dim, aug_varb_dim, s;
  int		*ipar, *i_pass_varb;
  int		dslv(), pitcon();
  int		ierror=0, count=0;
  int		cont_dir_flag=FALSE;       /* are we continuing a previous object? */
  int		search_flag;	           /* search for singularity or track solution branch */
  int           ph_space_dim  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; /* time not included */
  int           parameter_dim = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  int		max_iters;
  static int	last_dir = FORWARD, first_pass = TRUE, color[3] = {-1,SYS_RED,MED_POINT};
  double	*fpar, *r_pass_varb, **xr, *state, *parameters,*max,*min;
  double	*dvector(), **dmatrix();
  memory	cont_mem=NULL;

  if ( ! (state = dvector(0,ph_space_dim)) ) return(-1);    /* Get time too (to stuff into memory!) */
  if ( ! (parameters = dvector(0,parameter_dim-1))  ) return(-1);  
	
  cont_data_refresh();					    /* refresh data and panel for continuation */
  cont_setup();
  max_iters   = *((int *) pm( GET, "Cont.Iters", NULL ));
  direction   = *((int *) pm(GET, "Cont.Direction", NULL));
  search_flag = *((int *) pm(GET, "Cont.Search", NULL)); /* continuation or scatter serach? */

  reset_interrupt();

  if ( (direction== FORWARD || direction== BACKWARD) || search_flag )
    {
      selected_read_window();	/* update postmaster */
      pm( GET_LIST, "Selected.Varb_Ic", 0, ph_space_dim, state, NULL);  /* Get time too */
      pm( GET_LIST, "Selected.Param_Ic", 0, parameter_dim-1, parameters, NULL); 
      Cont_Sel[Cont_Cur_Choice].Cont_Init(&aug_varb_dim,max_iters,&xr,&fpar,state,parameters);
      if(cont_ds.aug_varb_alloc)
	free(cont_ds.aug_varb_save);
      if(aug_varb_dim>0 )
	{
          cont_ds.aug_varb_save = (double *) calloc(aug_varb_dim, sizeof(double));
          cont_ds.aug_varb_alloc = TRUE;
	}
      last_dir = direction;
    }
  else				/* CONTINUE */
    {
      cont_dir_flag = TRUE;
      pm( GET_LIST, "Cont.Fc", 0, ph_space_dim, state, NULL);    /* Get time too */
      pm( GET_LIST, "Cont.Param_Fc", 0, parameter_dim-1, parameters, NULL); 
      Cont_Sel[Cont_Cur_Choice].Cont_Init(&aug_varb_dim,max_iters,&xr,&fpar,state,parameters);
      direction = last_dir;
      if(cont_ds.aug_varb_alloc) 
	for(i=0; i<aug_varb_dim; i++) xr[0][i+ph_space_dim+1] =  cont_ds.aug_varb_save[i];
    }

  nvar = ph_space_dim + aug_varb_dim + Cont_Sel[Cont_Cur_Choice].Num_Req_Param; /* setup pitcon varbs */
  liw  = nvar+29;			      /* Variables which are not defined have default values  */ 
  lrw  = 29+(6+nvar)*nvar;		      /* or are used internally by pitcon                     */

  r_pass_varb = (double *) calloc(lrw+1, sizeof(double));
  i_pass_varb = (int *) calloc(liw+1, sizeof(int));

  i_pass_varb[2] = *((int *) pm( GET, "Cont.Param", NULL))+1;
  if(i_pass_varb[2]>ph_space_dim) 
    i_pass_varb[2] += aug_varb_dim;
  i_pass_varb[3] = *((int *) pm( GET, "Cont.Vary_Switch", NULL));
  i_pass_varb[4] = *((int *) pm( GET, "Cont.Jac_Update", NULL));
  i_pass_varb[7] = cont_ds.Debug_Level;
  i_pass_varb[9] = 0;			      /* jacobian supplied */
  i_pass_varb[9] = 1;			      /* automatic forward diff of full jacobian */
  i_pass_varb[9] = 2;			      /* automatic cen diff of full jacobian */
  r_pass_varb[1] = *((double *) pm( GET, "Cont.Abserr", NULL));
  r_pass_varb[2] = *((double *) pm( GET, "Cont.Relerr", NULL));
  r_pass_varb[3] = *((double *) pm( GET, "Cont.Minstp", NULL));
  r_pass_varb[5] = r_pass_varb[4] = *((double *) pm( GET, "Cont.Maxstp", NULL));
  r_pass_varb[6] = (direction == FORWARD) ? 1.0:-1.0; 
  r_pass_varb[7] = 0.0;

  if(cont_ds.Debug_Level > 3) 
      cont_dump_input(nvar, r_pass_varb, i_pass_varb, xr[0], ph_space_dim, state, parameter_dim, parameters);
      
  if (search_flag )			      /* set up arrays for monte-carlo */
    {
      min = dvector(0,nvar-1);
      max = dvector(0,nvar-1);
      pm(GET_LIST, "Defaults.Varb_Min", 0, ph_space_dim-1, min, NULL);
      pm(GET_LIST, "Defaults.Varb_Max", 0, ph_space_dim-1, max, NULL);
      j=0;
      for(i=0; i<parameter_dim; i++)	      /* only properly handles case of n_aug_varb==0 */
	if(cont_ds.Active_Param[i])
	  {
	    min[ph_space_dim + j] = *((double *) pm(GET, "Defaults.Param_Min", i, NULL));
	    max[ph_space_dim + j] = *((double *) pm(GET, "Defaults.Param_Max", i, NULL));
	    ++j;
	  }
    }

  for(i=0; i<max_iters && (!ierror) ; i++) /* && !(prop_halt()); i++)	   /*
******* main loop ******** */
    {
      ierror = pitcon(Cont_Sel[Cont_Cur_Choice].Cont_Deriv,
		      fpar,
		      Cont_Sel[Cont_Cur_Choice].Cont_Driver,
		      ipar,i_pass_varb,liw,nvar,r_pass_varb,lrw,dslv,&xr[i][0]);
      
      if( cont_ds.Debug_Level > 1)
	{
	  if(i_pass_varb[1] == 1)     printf("corrected starting point \n");
	  if(i_pass_varb[1] == 3)     {printf("target point \n"); ierror=10;}
	  if(i_pass_varb[1] == 4)     printf("limit point \n");
	}

      if( !ierror )		   	      /* no errors; color-code, increment, get next pt */
	{
	  Cont_Sel[Cont_Cur_Choice].Cont_Update(&xr[i][0],fpar,aug_varb_dim);
	  ++count; 
	}

      /* searching does not yet work for aug_varb_dim>0, so skip it for now */
      if( search_flag && !aug_varb_dim)	      /* get next initial condition  */
	{
	  if( !ierror && (i_pass_varb[1] == 1) )  /* found a solution! */
	    for(j=1; j<=nvar; j++)	      /* fill up array of solution [0:count] in xr */
	      xr[count-1][j] = xr[i][j];	     
	  if( i < max_iters-1 )		      /* get next random point (remember: xr starts at 1! */
	    rnd_vector(nvar, &xr[i+1][1], min, max);   
	  ierror = 0;			      /* reset error flag and repeat with new guess */
	  i_pass_varb[1] = 0;		      /* set flag meaning "first guess" */
	}
    }  
      
  cont_mem = (memory)  pm( GET, "Memory.Cont",CONT_MEMORY , NULL);			/* start flow if found pts AND */
  if( count > 0 && (search_flag || (!cont_dir_flag || first_pass))) 			/* (1) doing a scatter search OR */
    memory_start_new_flow( cont_mem, 1, 0, 0, max_iters, 0, 0);     			/* (2) not continuing an object */

  j = *((int *) pm( GET, "Cont.Param", NULL))+1;  /* xr[][j] is distinguished continuation parameter */
  if(j>ph_space_dim) j += aug_varb_dim;			      /* use it below for computing turning point */

  for(i=0; i<count; i++)
    {
      update_ds_state( xr[i], state, parameters, ph_space_dim, aug_varb_dim, parameter_dim );
      if( Cont_Sel[Cont_Cur_Choice].Cont_Check != NULL) 
        Cont_Sel[Cont_Cur_Choice].Cont_Check(color,ph_space_dim,state, parameters);
      memory_add_point(cont_mem, state, parameters, color, NULL, NULL, NULL, NULL);
      
      if((i>0) && ( direction==FORWARD ? (xr[i-1][j] > xr[i][j]) : (xr[i-1][j] < xr[i][j]) ) ) /* if turning point  */ 
	if(direction==FORWARD)								       /* in curve passed,  */
	  last_dir = BACKWARD;								       /* alter notion of   */
	else last_dir = FORWARD;							       /* current direction */
    }
  
  if(cont_ds.Debug_Level > 1)
    printf("factorizations= %d \nsolves        = %d \nfunctions     = %d \n",
	    i_pass_varb[20],i_pass_varb[21],i_pass_varb[22]);

  if(cont_ds.Debug_Level > 3)
      cont_dump_input(nvar, r_pass_varb, i_pass_varb, xr[count-1], ph_space_dim, state, parameter_dim, parameters);

  mem_all_win(cont_mem);
  pm( PUT_LIST, "Cont.Fc", 0, ph_space_dim-1, state, NULL);
  pm( PUT_LIST, "Cont.Param_Fc", 0, parameter_dim-1, parameters, NULL); 
  pm( PUT, "Cont.Direction", CONTINUE, NULL);
  cntstate_panel_refresh();

  if(count>1)
     for(i=0; i<aug_varb_dim; i++) cont_ds.aug_varb_save[i] = xr[count-1][i+ph_space_dim+1];

  free_dvector(state,0,ph_space_dim-1);
  free_dvector(parameters,0,parameter_dim-1);
  if (search_flag)			      
    {
      free_dvector( min, 0, nvar-1);
      free_dvector( max, 0, nvar-1);
    }

  cont_exit(nvar,max_iters,xr,fpar,r_pass_varb,i_pass_varb);
  cmd_data_refresh();	
  first_pass = FALSE;
  if(ierror != 0)		
    printf("iteration halted, error flag= %d \n",ierror);
  return(ierror);
}

/*
 * cont_setup() define manifold structure of the cont_ds structure
 */
int 
cont_setup()
{
  int		*ivector();
  double	*dvector();
  int ph_space_dim	= 	*((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; /* time not included */

  cont_ds.manifold = (Manifold *) calloc(1, sizeof(Manifold));
  cont_ds.manifold->periodic_varb = ivector(0,ph_space_dim);
  cont_ds.manifold->period_start = dvector(0,ph_space_dim);
  cont_ds.manifold->period_end = dvector(0,ph_space_dim);
  cont_ds.manifold->type = *((int *) pm( GET, "Manifold.Type", NULL ));
  pm( GET_LIST, "Manifold.Periodic_Varb", 0, ph_space_dim-1, cont_ds.manifold->periodic_varb, NULL);
  pm( GET_LIST, "Manifold.Period_Start", 0, ph_space_dim-1, cont_ds.manifold->period_start, NULL);
  pm( GET_LIST, "Manifold.Period_End", 0, ph_space_dim-1, cont_ds.manifold->period_end, NULL);
}

/*
 * cont_alloc() memory allocation
 */
int
cont_alloc( max_iters, n_varbs, n_param, nvar, xr, fpar )
int	max_iters, n_varbs, n_param, nvar;
double	***xr, **fpar;
{
  double	**dmatrix();

  *xr = (double **) dmatrix(0,max_iters+1,0,nvar+1);
  *fpar = (double *) calloc(n_param+n_varbs+1, sizeof(double));
  cont_ds.r_workspace = (double *) calloc(500*nvar, sizeof(double));
  cont_ds.i_workspace = (int *) calloc(500*nvar, sizeof(int));
  cont_ds.jacobian_wrt_x = (double**) dmatrix(0,nvar,0,nvar); 
  cont_ds.jacobian_wrt_p = (double**) dmatrix(0,nvar,0,n_param);
}

/*
 * cont_exit() free allocated memory
 */
int
cont_exit(nvar,max_iters,xr,fpar,r_pass_varb,i_pass_varb)
int	nvar, *i_pass_varb, max_iters;
double	*r_pass_varb, **xr, *fpar; 				
{
  int ph_space_dim	= 	*((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; /* time not included */
  int parameter_dim     =	*((int *) pm( GET, "Model.Param_Dim", NULL ));

  free_dmatrix(xr,0,max_iters+1,0,nvar+1); /* free memory from cont_alloc */
  free(fpar);
  free(cont_ds.r_workspace);
  free(cont_ds.i_workspace);
  free(r_pass_varb);
  free(i_pass_varb);
  free_dmatrix(cont_ds.jacobian_wrt_x,0,nvar,0,nvar);
  free_dmatrix(cont_ds.jacobian_wrt_p,0,nvar,0,parameter_dim);
  free_ivector(cont_ds.manifold->periodic_varb,0,ph_space_dim);	/* free memory from cont_setup */
  free_dvector(cont_ds.manifold->period_start,0,ph_space_dim);
  free_dvector(cont_ds.manifold->period_end,0,ph_space_dim);
  free(cont_ds.manifold);
}

/*
 * update_cont_state() translates from dstool storage format into pitcon storage format.
 * In particular, state is filled (beginning at state[1]) with the phase space variables:
 * state[1:n_varb] = x[0:n_varb-1].  If n_aug_varb > 0, then space is left for them.  
 * Augmented parameters are placed at the end of the array.  
 */
update_cont_state( state, x, par, n_varb, n_aug_varb, n_param )
double	*state, *x, *par;
int	n_varb, n_aug_varb, n_param;
{
	int	i, offset=n_varb+n_aug_varb+1;
        
	for(i=0; i<n_varb; i++)
	   state[i+1] = x[i];

        for(i=0; i<n_param; i++)
	   if(cont_ds.Active_Param[i])
	      {
	       state[offset] = par[i];
	       ++offset;
	      }
}

/*
 * update_ds_state() translates from pitcon storage format into dstool storage format.
 * In particular, x is filled with the phase space variables:
 * x[0:n_varb-1] = state[1:n_varb].  The augmented parameters are updated approiately.
 */
int
update_ds_state( state, x, par, n_varb, n_aug_varb, n_param )
double	*state, *x, *par;
int	n_varb, n_aug_varb, n_param;
{
	int	i, offset=n_varb+n_aug_varb+1;

	for(i=0; i<n_varb; i++) 
	   x[i] = state[i+1];
        
	for(i=0; i<n_param; i++)
	   if(cont_ds.Active_Param[i])
	      {
	       par[i] = state[offset];
	       ++offset;
	      }
}



cont_dump_input(nvar, r_pass_varb, i_pass_varb, xstart, n_varb, ph_space_state, n_param, parameters)
int	nvar, *i_pass_varb, n_varb, n_param;
double	*r_pass_varb, *ph_space_state, *parameters, *xstart;
{
	int	i;

	fprintf(stderr,"\n \n ------------------ Continuation Conditions ------------------------------- \n");

	fprintf(stderr,"nvar = %d \n",nvar);
	fprintf(stderr,"ipoint = i_pass_varb(%d) = %d \n",1,i_pass_varb[1]);
	fprintf(stderr,"ipc = i_pass_varb(%d) = %d \n",2,i_pass_varb[2]);
	fprintf(stderr,"ipram = i_pass_varb(%d) = %d \n",3,i_pass_varb[3]);
	fprintf(stderr,"modnew = i_pass_varb(%d) = %d \n",4,i_pass_varb[4]);
	fprintf(stderr,"it = i_pass_varb(%d) = %d \n",5,i_pass_varb[5]);
	fprintf(stderr,"lim = i_pass_varb(%d) = %d \n",6,i_pass_varb[6]);
	fprintf(stderr,"iwrite = i_pass_varb(%d) = %d \n",7,i_pass_varb[7]);
	fprintf(stderr,"lounit = i_pass_varb(%d) = %d \n",8,i_pass_varb[8]);
	fprintf(stderr,"jacobian = i_pass_varb(%d) = %d \n",9,i_pass_varb[9]);
	fprintf(stderr,"abserr = r_pass_varb(%d) = %lg \n",1,r_pass_varb[1]);
	fprintf(stderr,"relerr = r_pass_varb(%d) = %lg \n",2,r_pass_varb[2]);
	fprintf(stderr,"hmin = r_pass_varb(%d) = %lg \n",3,r_pass_varb[3]);
	fprintf(stderr,"hmax = r_pass_varb(%d) = %lg \n",4,r_pass_varb[4]);
	fprintf(stderr,"htan = r_pass_varb(%d) = %lg \n",5,r_pass_varb[5]);
	fprintf(stderr,"dir = r_pass_varb(%d) = %lg \n",6,r_pass_varb[6]);
	fprintf(stderr,"xit = r_pass_varb(%d) = %lg \n",7,r_pass_varb[7]);
/*
	fprintf(stderr,"\n");
	for(i=1; i<=nvar; i++)
	   fprintf(stderr,"  x(%d) = %lg \n",i,xstart[i]);
*/
	fprintf(stderr,"\n");
        for(i=0; i<n_varb; i++)
	   fprintf(stderr,"  ph_space_state(%d) = %lg \n",i,ph_space_state[i]);

	fprintf(stderr,"\n");
        for(i=0; i<n_param; i++)
	   fprintf(stderr,"  parameters(%d) = %lg \n",i,parameters[i]);

	fprintf(stderr," ---------------- \n");
}


