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


int
jgr_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, j, i1=1, i2=1, nvar, mode, ierror, index=0;
  double	**dmatrix(), *dwork, *wr, *wi, **eig_vec, min, max, mag=0.0, magsign;
/*		sqrt(), fabs(), copysign(); */
  int           n_varbs  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       2*n_varbs+1;
  nvar          =       n_varbs + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  cont_alloc( max_iters, n_varbs, n_params, nvar, xr, fpar );

  update_cont_state( (*xr)[0], state, parameters, n_varbs, *n_aug_varb, n_params );

  for(i=1; i<= n_params; i++) (*fpar)[i] = parameters[i-1];

  wr = cont_ds.r_workspace;
  wi = wr + 2*n_varbs + 2;
  dwork = wi + 2*n_varbs + 2;

  eig_vec = dmatrix(0,n_varbs,0,n_varbs);
  get_Dxf( cont_ds.jacobian_wrt_x, n_varbs, state, parameters );

  get_eigenvec(cont_ds.jacobian_wrt_x, n_varbs, wr, wi, eig_vec);

  min = fabs(wr[0]);			/* which eigenvalue is closest to being */
  for(i=1; i<n_varbs; i++)		/* purely imaginary?                    */
    if( fabs(wr[i]) < min )
      {
       min = fabs(wr[i]);
       index = i;			/* index is referenced starting from -> 0 <- */
      }

  for(i=1; i<=n_varbs; i++)				/* get real and imag parts of eigenvectors */
    {
     (*xr)[0][i+n_varbs] = eig_vec[i][index+1];
     (*xr)[0][i+2*n_varbs] = eig_vec[i][index+2];
    }
  (*xr)[0][3*n_varbs+1] = wi[index];		/* get magnitude of "purely imag" eigenvalue */	

  for(i=1; i<=n_varbs; i++)
     mag += ((*xr)[0][i+n_varbs])*((*xr)[0][i+2*n_varbs]);	/* if (u+vi) is eigenvector, normalize */
  magsign=copysign(1.0,mag);					/* u & v so u*v=1                      */
  mag = sqrt(fabs(mag));
  for(i=1; i<=n_varbs; i++)
      (*xr)[0][i+n_varbs] /= mag;
  if ( magsign< 0.0 )
     {
      mag *= -1.0;						/* may need to choose cmplx conj vector */
      (*xr)[0][3*n_varbs+1] *= -1.0;
     }

  for(i=1; i<=n_varbs; i++)
      (*xr)[0][i+2*n_varbs] /= mag;

  for(i=2; i<=n_varbs; i++) 					/* what are the 2 largest components? */
    {
     if ( fabs(eig_vec[i][index+1]) > fabs(eig_vec[i1][index+1]) ) i1 = i;
     if ( fabs(eig_vec[i][index+1]) < fabs(eig_vec[i1][index+1]) ) i2 = i;
    }
  for(i=1; i<=n_varbs; i++)
     if ( fabs(eig_vec[i][index+1]) > fabs(eig_vec[i2][index+1]) && i != i1 ) i2 = i;

  for(i=1; i<=n_varbs; i++) (*fpar)[i+n_params] = 0.0;

  (*fpar)[i1+n_params] = (*xr)[0][i2+n_varbs]/( (*xr)[0][i1+n_varbs]*(*xr)[0][i2+n_varbs]);	/* set normalizing vector */
  (*fpar)[i2+n_params] = -(*xr)[0][i1+n_varbs]/( (*xr)[0][i1+n_varbs]*(*xr)[0][i2+n_varbs]);    /* orthog to eigenvector real part */

  free_dmatrix(eig_vec,0,n_varbs,0,n_varbs);
}


int
jgr_update(x,parm,n_aug_varb)
int	n_aug_varb;
double	*x, *parm;
{
  int		i, max_index, index;
  double	max, fabs();
  int           n_varbs  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  
  double	*wr, *wi, **eig_vec, **dmatrix();


}



int  
jgr_func(nvar,fpar,ipar,x,fx)
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

  update_ds_state( x, state, params, n_varb, 2*n_varb+1, n_params );
  function( &fx[1], state, params);

  get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, params );

  jgr_aug_func( &fx[n_varb+1], x, fpar ); 

  return(ierror);
}



jgr_aug_func( g_vector, x, fpar )
double	*g_vector, *x, *fpar;
{
  int		i, j;
  int           n_varb  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int           n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));

/* x[1] and fpar[1] are the first values! */

  multAv( &(g_vector[0]), cont_ds.jacobian_wrt_x, n_varb, n_varb, &(x[n_varb+1]) );
  multAv( &(g_vector[n_varb]), cont_ds.jacobian_wrt_x, n_varb, n_varb, &(x[2*n_varb+1]) );

  g_vector[2*n_varb] = -1.0;
  g_vector[2*n_varb+1] = 0.0;
  for(i=0; i<n_varb; i++)
    {
     g_vector[i] += x[3*n_varb+1]*x[2*n_varb+1+i];
     g_vector[n_varb+i] -= x[3*n_varb+1]*x[n_varb+1+i];
     g_vector[2*n_varb] += x[n_varb+1+i]*x[2*n_varb+1+i];
     g_vector[2*n_varb+1] += x[n_varb+1+i]*fpar[n_param+1+i]; 
    }

}



int
jgr_dfunc(nvar,fpar,ipar,x,fprime)
int	nvar, *ipar;
double	*fpar, *x, **fprime;
{
  int		  ierror=0, i;
  double	  *state, *param, *omega;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  double		*dvector();

  return(ierror);
}


int
jgr_check( color, n_varb, state, param )
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

