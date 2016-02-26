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

extern struct  Cont_Cntl_Ds            cont_ds;


/* ------------------------------------------------------------------------------------------------------- 

   This set of procedures defines an embedding for the quadratic foldpoint bifurcation outlined in the
   paper:

	    Spence, A. and A.D. Jepson, `The Numerical Calculation of Cusps, Bifurcation
	    Points and Isola Formation Points in Two Parameter Problems,' Numerical Methods
	    for Bifurcation Problems in the ISNM, Vol. 70, 1984, pps. 502-514.

   The augmented system for the vector field,
				 		      .
		  				      x = f(x,a)
   is given by:
							(           )
							(    f      )
					     F(x,v,a) = (  Dxf v    )
							( <v,l> - 1 )
							(           )

   where Dxf is the Jacobian matrix, l is a fixed normalizing vector, <> is the standard inner product 
   and v in an n-vector augmented to the independent variable set.

   This program is the property of:
					Cornell University
					Center of Applied Mathematics
					305 Sage Hall, Ithaca  NY  14853

   and may be used and modified freely, provided that this legend is included on all tape media and
   as a part of the software program in whole or part.  This file is provided as is with no guarantees
   and without any obligation on the part of Cornell faculty, staff or students assist in its use, 
   correction, modification or enhancement.

   Author:   Mark Myers		Last modified:  08 January 1992  

   ------------------------------------------------------------------------------------------------------- */


int
sn_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, j, nvar, mode, ierror, index;
  double	**dmatrix(), *dwork, *wr, *wi, **eig_vec, min, max, mag, sqrt(), fabs();
  int           n_varbs  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       n_varbs;
  nvar          =       n_varbs + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  cont_alloc( max_iters, n_varbs, n_params, nvar, xr, fpar );

  update_cont_state( (*xr)[0], state, parameters, n_varbs, *n_aug_varb, n_params );

  for(i=1; i<= n_params; i++) (*fpar)[i] = parameters[i-1];

  wr = cont_ds.r_workspace;
  wi = wr + n_varbs + 1;
  dwork = wi + n_varbs + 1;

  eig_vec = dmatrix(0,n_varbs,0,n_varbs);
  get_Dxf( cont_ds.jacobian_wrt_x, n_varbs, state, parameters );

  get_eigenvec(cont_ds.jacobian_wrt_x, n_varbs, wr, wi, eig_vec);

  index = 0;
  min = (wr[0]*wr[0]+wi[0]*wi[0]);
  for(i=1; i<n_varbs; i++)
    if( (wr[i]*wr[i]+wi[i]*wi[i]) < min )
      {
       min = (wr[i]*wr[i]+wi[i]*wi[i]);
       index = i;
      }

  mag = 0.0;
  for(i=1; i<=n_varbs; i++)
    mag = mag + eig_vec[i][index]*eig_vec[i][index];
  mag = sqrt(mag);
    
  for(i=n_varbs+1; i<=n_varbs+(*n_aug_varb); i++) (*xr)[0][i] = eig_vec[i-n_varbs][index]/mag;

  index = 1;
  max = fabs((*xr)[0][1+n_varbs]);
  for(i=2; i<=n_varbs; i++)
    {
     (*fpar)[i+n_params] = 0.0;
     if( fabs((*xr)[0][i+n_varbs]) > max )
        {
         min = fabs((*xr)[0][i+n_varbs]);
         index = i;
        }
    }

  (*fpar)[index+n_params] = 1.0;

  free_dmatrix(eig_vec,0,n_varbs,0,n_varbs);
}


int
sn_update(x,parm,n_aug_varb)
int	n_aug_varb;
double	*x, *parm;
{
  int		i, max_index, index;
  double	max, fabs();
  int           n_varbs  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  
  double	*wr, *wi, **eig_vec, **dmatrix();

/*  

  for(i=1; i<=n_varbs; i++)
     if( fabs(parm[i+n_params]) > 0.1 ) index = i;

  max = 0.0;
  max_index = index;

  for(i=1; i<=n_varbs; i++)
      if( 0.5*fabs(x[n_varbs+i]) > max ) 
	{ 
	 max_index = i;
	 max = 0.5*fabs(x[n_varbs+i]);
        }

  if( fabs(x[n_varbs+index]) < max )
     {
      for(i=n_params+1; i<=n_params+n_varbs; i++) parm[i] = 0.0;
      parm[n_params+max_index] = 1.0;
     }
  
  wr = cont_ds.r_workspace;
  wi = wr + n_varbs + 1;

  eig_vec = dmatrix(0,n_varbs,0,n_varbs);

  get_eigenvec(cont_ds.jacobian_wrt_x, n_varbs, wr, wi, eig_vec);

  free_dmatrix(eig_vec,0,n_varbs,0,n_varbs);
*/
}



int  
sn_func(nvar,fpar,ipar,x,fx)
int	nvar, *ipar;
double	*fpar, *fx, *x;
{
  int             (*function)();
  int		  ierror=0, i, j;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  double          *state, *params, *dwork;

  state = cont_ds.r_workspace;
  params = state + n_varb + 1;
  dwork = params + n_params + 1;


  function = (void *) pm( GET, "Model.DS_Def", NULL );
  for(i=0; i<n_params; i++) params[i] = fpar[i+1];

  update_ds_state( x, state, params, n_varb, n_varb, n_params );

/*
  fprintf(stderr,"\n ------------------------------------------------- \n");
  fprintf(stderr,"x = ");
  for(i=1; i<=nvar; i++) fprintf(stderr," %15.8g",x[i]);
  fprintf(stderr,"\n");
  fprintf(stderr,"fpar = ");
  for(i=1; i<=n_params+n_varb; i++) fprintf(stderr," %15.8g",fpar[i]);
  fprintf(stderr,"\n");
  fprintf(stderr,"state = ");
  for(i=0; i<n_varb; i++) fprintf(stderr," %15.8g",state[i]);
  fprintf(stderr,"\n");
  fprintf(stderr,"params = ");
  for(i=0; i<n_params; i++) fprintf(stderr," %15.8g",params[i]);
*/
  function( &fx[1], state, params);
/*
  fprintf(stderr,"\n");
  fprintf(stderr,"f = ");
  for(i=1; i<=2*n_varb+1; i++) fprintf(stderr," %15.8g",fx[i]);
*/
  get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, params );
  sn_aug_func( &fx[n_varb+1], x, fpar ); 
/*
  fprintf(stderr,"\n");
  fprintf(stderr,"f = ");
  for(i=1; i<=2*n_varb+1; i++) fprintf(stderr," %15.8g",fx[i]);
*/
  return(ierror);
}



sn_aug_func( g_vector, x, fpar )
double	*g_vector, *x, *fpar;
{
  int		i, j;
  int           n_varb  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int           n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  for(i=0; i<n_varb; i++)
    {
     g_vector[i] = 0.0;
     for(j=0; j<n_varb; j++)
        g_vector[i] += cont_ds.jacobian_wrt_x[i][j]*x[n_varb+j+1];
    }

  g_vector[n_varb] = -1.0;
  for(i=0; i<n_varb; i++)
     g_vector[n_varb] += x[n_varb+i+1]*fpar[n_param+i+1];
}



int
sn_dfunc(nvar,fpar,ipar,x,fprime)
int	nvar, *ipar;
double	*fpar, *x, **fprime;
{
  int		  ierror=0, i;
  double	  *state, *param, *omega;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  double		*dvector();

  state = cont_ds.r_workspace;
  param = state + n_varb + 1;
  omega = param + n_params + 1;

  for(i=0; i<n_params; i++) param[i] = fpar[i+1];

  update_ds_state( x, state, param, n_varb, n_varb, n_params );
  for(i=0; i<n_varb; i++)
    {
     omega[i] = x[n_varb+i+1];
     param[n_params+i-1] = fpar[n_params+i];
    }

  return(ierror);
}


int
sn_check( color, n_varb, state, param )
int		n_varb, *color;
double		*state, *param;
{
  int   ierror=0;

  color[2] = MED_POINT;
  if( !cont_ds.Check_Switch )
     {   
      color[1] = 1;
     }   
  else
     { 
      color[1] = SYS_GREEN;
     }

  return(ierror);
}

