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
kub1_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, j, nvar;
  double	**dmatrix(), fabs(), det_comp[2], pow(), omega;
  int           n_varb  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       1;
  nvar          =       n_varb + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  cont_alloc( max_iters, n_varb, n_param, nvar, xr, fpar );

  update_cont_state( (*xr)[0], state, parameters, n_varb, *n_aug_varb, n_param );

  for(i=1; i<= n_param; i++) (*fpar)[i] = parameters[i-1];

  omega = .01;                                                  /* replace with estimate on min Re part! */
  (*xr)[0][n_varb+1] = omega;

}


int
kub1_update(x,parm,n_aug_varb)
int	n_aug_varb;
double	*x, *parm;
{
}



int  
kub1_func(nvar,fpar,ipar,x,fx)
int	nvar, *ipar;
double	*fpar, *fx, *x;
{
  int             (*function)();
  int		  ierror=0, i, j;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  double          *state, *params, *coeffs, *v, omega, b[3];

  int		iwork[2000];
  double        work[2000], dwork[2000];

/*   state = cont_ds.r_workspace; */
  state = dwork;
  params = state + n_varb + 2;
  coeffs = params + n_varb + 3;
  v = coeffs + n_params + 1;

  omega = x[n_varb+1];

  function = (void *) pm( GET, "Model.DS_Def", NULL );
  for(i=0; i<n_params; i++) params[i] = fpar[i+1];

  update_ds_state( x, state, params, n_varb, 1, n_params );
  function( &fx[1], state, params);

  for(i=0; i<n_varb+2; i++) coeffs[i] = 1.0;
  
  get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, params );	/* fetch jacobian of RHS */

  balance(cont_ds.jacobian_wrt_x, n_varb);			/* balance column norms */
  hesberg(cont_ds.jacobian_wrt_x, n_varb);			/* reduce to upper unit hessenberg */
  compan(cont_ds.jacobian_wrt_x, n_varb);			/* reduce to Frobenius form */

  for(i=0; i<n_varb; i++) 
    coeffs[i] = -cont_ds.jacobian_wrt_x[i][n_varb-1];		/* pull off coefficients of the characteristic poly */

  lin_barstow( b, coeffs, n_varb, omega );

  fx[n_varb+1] = coeffs[1] - omega*b[0];
  fx[n_varb+2] = coeffs[0] - omega*b[1];

  return(ierror);
}



lin_barstow( b, coeffs, n, omega )
int	n;
double	*b, *coeffs, omega;
{
  int	i;

  b[0] = 0.0;  b[1] = 1.0;

  for(i=1; i<=n-2; i++)
    {
     b[2] = coeffs[n-i] - omega*b[0];
     b[0] = b[1];  b[1] = b[2];
    }
}




int
kub1_dfunc(nvar,fpar,ipar,x,fprime)
int	nvar, *ipar;
double	*fpar, *x, **fprime;
{

	int		ierror, i, j, offset;
	int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
	int		n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));
	double		*params, *state; 

	ierror = 0;
 
        state = cont_ds.r_workspace;  
	params = state + n_varb + 2;  

        for(i=0; i<n_param; i++) params[i] = fpar[i+1];

	update_ds_state( x, state, params, n_varb, 0, n_param );

	get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, params );
	get_Dpf( cont_ds.jacobian_wrt_p, n_param, state, params ); /* used to be n_varb; fjw 8/16/92 */

	offset = n_varb;
	for(j=0; j<n_param; j++)
	  if(cont_ds.Active_Param[j])
	    {
             for(i=0; i<n_varb; i++)
	        cont_ds.jacobian_wrt_x[i][offset] = cont_ds.jacobian_wrt_p[i][j];
	     ++offset;
            }

        for(i=0; i<n_varb; i++)
	   for(j=0; j<=n_varb; j++)
	      fprime[i][j] = cont_ds.jacobian_wrt_x[i][j];

        return(ierror);
}


int
kub1_check( color, n_varb, state, param )
int		n_varb, *color;
double		*state, *param;
{
   int	ierror=0;

   color[2] = MED_POINT;
   if( !cont_ds.Check_Switch )
     {
      color[1] = 1;
     }
   else
     {
      color[1] = 10;	/* SYS_SKY_BLUE */
     }

   return(ierror);
}


