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
#include "../continue.h"

/* For Hopf and Neutral Saddle Nodes */

extern struct  Cont_Cntl_Ds            cont_ds;

int
bp_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, nvar;
  double	**dmatrix(), delta;
  int           n_varbs  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       0;
  nvar          =       n_varbs + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  cont_alloc( max_iters, n_varbs, n_param, nvar, xr, fpar );

  update_cont_state( (*xr)[0], state, parameters, n_varbs, *n_aug_varb, n_param );

  for(i=1; i<= n_param; i++) (*fpar)[i] = parameters[i-1];
}


int
bp_update(x,parm,n_aug_varb)
int	n_aug_varb;
double	*x, *parm;
{
}



int  
bp_func(nvar,fpar,ipar,x,fx)
int	nvar, *ipar;
double	*fpar, *fx, *x;
{
  int		  ierror=0, i, biprod_dim;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  int             period=*((int *) pm( GET, "Flow.Skip_Size", NULL ));
  int             map_flag = *((int *) pm( GET, "Model.Mapping_Flag", NULL ));
  int             (*function)(), *index, status;
  double          det, det_comp[2], *v, *state, *params, **matrix, **dmatrix(), delta;

  state = cont_ds.r_workspace;
  params = state + n_varb + 1;
  v = params + n_params + 1;

  index = cont_ds.i_workspace;

  function = (void *) pm( GET, "Model.DS_Def", NULL );
  for(i=0; i<n_params; i++) params[i] = fpar[i+1];

  update_ds_state( x, state, params, n_varb, 0, n_params );
  delta = x[n_varb+1];

  if(map_flag)
     iter_forw( function, period, &fx[1], state, params, n_varb+1, 1.0, v, cont_ds.manifold);
  else
      function( &fx[1], state, params);

  get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, params );

  biprod_dim = (int)((n_varb-1)*n_varb/2);
  matrix = dmatrix(0,biprod_dim,0,biprod_dim);

  if(map_flag)
    {
     bialtprd( matrix, cont_ds.jacobian_wrt_x, n_varb );
     for(i=0; i<biprod_dim; i++) matrix[i][i] -= 1.0;
     status = ludcmp( matrix, biprod_dim, index, &det);
     det = 1.0;
     for(i=0; i<biprod_dim; i++) det *= matrix[i][i];
     for(i=0; i<n_varb; i++) fx[i+1] -= state[i];
    }
  else
     biprod( matrix, cont_ds.jacobian_wrt_x, n_varb, &det );

  fx[n_varb+1] = det;
  free_dmatrix(matrix, 0,biprod_dim,0,biprod_dim);

  return(ierror);
}


int
bp_dfunc(nvar,fpar,ipar,x,fprime)
int	nvar, *ipar;
double	*fpar, *x, **fprime;
{
}


#define TINY   1.e-6


int
bp_check( color, n_varb, state, param )
int		n_varb, *color;
double		*state, *param;
{
   int		i, j, mode, *iwork, ierror=0;
   int		hopf=FALSE, nsn=FALSE, degen=FALSE;
   int		n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
   int          map_flag = *((int *) pm( GET, "Model.Mapping_Flag", NULL ));
   double	**jac,  *wr, *wi, *dwork, fabs(), **dmatrix();

   color[1] = 1;
   if ( *((int *)pm(GET, "Cont.Search", NULL)) )
     color[2] = MED_TRI;
   else
     color[2] = MED_POINT;
   if( !cont_ds.Check_Switch ) return(ierror);

   iwork = cont_ds.i_workspace;
   wr = cont_ds.r_workspace;
   wi = wr + n_varb + 1;
   dwork = wi + n_varb + 1;
   
/*   jac = dmatrix(0,n_varb+1,0,n_varb+1); */

   get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, param ); 
   ierror = get_eigenval( cont_ds.jacobian_wrt_x, n_varb, wr, wi); /* fjw */

/*
   for(i=0; i<n_varb; i++)
     for(j=0; j<n_varb; j++)
	jac[i+1][j+1] = cont_ds.jacobian_wrt_x[i][j];
   ierror = rg(n_varb,n_varb,jac,&wr[0]-1,&wi[0]-1,0,NULL,iwork,dwork);
   if(ierror != 0)
      {
       free_dmatrix(jac,n_varb+1,0,n_varb+1);
       return(ierror);
      }
*/
   if(ierror != 0) return(ierror);

   if(cont_ds.Debug_Level > 0)
     {
      fprintf(stderr,"\nstate = ");
      for(i=0; i<n_varb; i++) fprintf(stderr," %lg",state[i]);
      fprintf(stderr,"\nparam = ");
      for(i=0; i<n_params; i++) fprintf(stderr," %lg",param[i]);
      for(i=0; i<n_varb; i++) fprintf(stderr,"\neig(%d) = (%lg,%lg)",i, wr[i],wi[i]);
     }
    
   if ( *((int *) pm( GET, "Model.Mapping_Flag", NULL )) )
     {				/* fjw 8/11/92. Determine type for mapping */
       mode = TRUE;
       for(i=0; i<n_varb && mode; i++)
	 for(j=i+1; j<n_varb && mode; j++)
	   if( (fabs(fabs(wr[i]*wr[j] - wi[i]*wi[j]) - 1.0) < TINY) 
	      && (fabs(wi[i]*wr[j] + wr[i]*wi[j] )< TINY) )/* i,j s.t. product of e-vals = 1.0 */
	     mode = FALSE;

       if (!mode)		/* sucessful detection */
	 {
	   i--; j--;		/* correct for going too far */
	   if( fabs(wr[i] + wr[j]) - 2.0 < TINY ) /* test selected i,j pair */
	     {
	       hopf = TRUE;
	       color[1] = 10;	/* SYS_SKY_BLUE */
	     }
	   else if( fabs(wr[i] + wr[j]) - 2.0 > TINY )
	     {
	       nsn = TRUE;
	       color[1] = 6;	/* SYS_ORANGE */
	     }
	   else 
	     {
	       degen = TRUE;
	       color[1] = 12;	/* SYS_MAGENTA */
	     }
	 }
       else
	 {
	   fprintf(stderr,"\nProblem in hopf?\n");
	   for(i=0; i<n_varb; i++) fprintf(stderr,"\neig(%d) = (%lg,%lg)",i, wr[i],wi[i]);
	 }
     }
   else				/* determine type for vector field */
     {
       for(i=0; i<n_varb; i++)
	 {
	   if( fabs(wr[i]) < TINY && fabs(wi[i])>TINY )
	     {
	       hopf = TRUE;
	       break;
	     }
	   if( fabs(wr[i])+fabs(wi[i]) < TINY )
	     {
	       degen=TRUE;
	       break;
	     }
	   for(j=i; j<n_varb; j++)
	     {
	       if( fabs(wr[i]) > TINY && (wr[i]+wr[j])<TINY &&
		  (wi[i]+wi[j])<TINY &&  fabs(wi[i])<TINY     )
		 {
		   nsn = TRUE;
		   break;
		 }
	     }
	   if( nsn || hopf || degen ) break;
	 }
       if(hopf) color[1] = 10;	/* SYS_SKY_BLUE */
       if(degen) color[1] = 12;	/* SYS_MAGENTA */
       if(nsn) color[1] = 6;	/* SYS_ORANGE */
     }

   /* free_dmatrix(jac,0,n_varb+1,0,n_varb+1);*/
   return(ierror);

}
