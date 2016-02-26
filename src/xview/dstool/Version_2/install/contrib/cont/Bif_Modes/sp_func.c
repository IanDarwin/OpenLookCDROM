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
sp_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, j, nvar;
  double	**dmatrix(), fabs(), det_comp[2], pow();
  int           n_varb  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       0;
  nvar          =       n_varb + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  cont_alloc( max_iters, n_varb, n_param, nvar, xr, fpar );

  update_cont_state( (*xr)[0], state, parameters, n_varb, *n_aug_varb, n_param );

  for(i=1; i<= n_param; i++) (*fpar)[i] = parameters[i-1];

}


int
sp_update(x,parm,n_aug_varb)
int	n_aug_varb;
double	*x, *parm;
{
}



int  
sp_func(nvar,fpar,ipar,x,fx)
int	nvar, *ipar;
double	*fpar, *fx, *x;
{
  int             (*function)();
  int		  ierror=0, i, j, bezout_dim;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  double          *state, *params, *coeffs, *v, det_comp[2], pow(), **dmatrix(), **m_inv, value=0.0;

  int		index[100], status;
  double	d, log(), fabs(), **jac, **dmatrix(), wr[50], wi[50];

  int		iwork[2000];
  double        work[2000], dwork[2000];

/*   state = cont_ds.r_workspace; */
  state = dwork;
  params = state + n_varb + 2;
  coeffs = params + n_varb + 3;
  v = coeffs + n_params + 1;
  function = (void *) pm( GET, "Model.DS_Def", NULL );
  for(i=0; i<n_params; i++) params[i] = fpar[i+1];

  update_ds_state( x, state, params, n_varb, 0, n_params );
  function( &fx[1], state, params);

  bezout_dim = ( ceil((double) (n_varb/2.0)) -
	  floor((double) (n_varb/2.0)) > 0.1 )? (n_varb-1)/2:n_varb/2;
  for(i=0; i<n_varb+2; i++) coeffs[i] = 1.0;
  
  get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, params );	/* fetch jacobian of RHS */

  balance(cont_ds.jacobian_wrt_x, n_varb);			/* balance column norms */
  hesberg(cont_ds.jacobian_wrt_x, n_varb);			/* reduce to upper unit hessenberg */
  compan(cont_ds.jacobian_wrt_x, n_varb);			/* reduce to Frobenius form */

  for(i=0; i<n_varb; i++) 
    coeffs[i] = -cont_ds.jacobian_wrt_x[i][n_varb-1];		/* pull off coefficients of the characteristic poly */

  for(i=0;i<bezout_dim;i++)
    for(j=0;j<bezout_dim;j++) cont_ds.jacobian_wrt_x[i][j] = 0.0;	/* construct bezout matrix */

  bezout( cont_ds.jacobian_wrt_x, coeffs, n_varb );


      jac = dmatrix(0,bezout_dim+1,0,bezout_dim+1);

      for(i=0; i<bezout_dim; i++)
        for(j=0; j<bezout_dim; j++)
           jac[i+1][j+1] = cont_ds.jacobian_wrt_x[i][j];
      ierror = rg(bezout_dim,bezout_dim,jac,&wr[0]-1,&wi[0]-1,0,NULL,iwork,dwork);

      free_dmatrix(jac,0,bezout_dim+1,0,bezout_dim+1);



  value = 100000.;
  fprintf(stderr,"\n \n");
  for(i=0; i<bezout_dim; i++)
     {
      fprintf(stderr,"( %15.7g + %17.7g i ) \n",wr[i],wi[i]);
      d = wr[i]*wr[i] + wi[i]*wi[i];
      if( d < value) value = d;
     }
  fprintf(stderr,"value = %18.12g \n", value);
  fx[n_varb+1] = value;

/*
  status = ludcmp( cont_ds.jacobian_wrt_x, bezout_dim, index, &d);

  value = 1.0e180;
  fprintf(stderr,"\n");
  for(i=0; i<bezout_dim; i++)
     {
      fprintf(stderr,"%10.3g ",cont_ds.jacobian_wrt_x[i][i]);
     if( fabs(cont_ds.jacobian_wrt_x[i][i]) < fabs(value) ) value = cont_ds.jacobian_wrt_x[i][i];
     }
  fprintf(stderr,"\n");
  fprintf(stderr,"value = %18.12g \n", value);
  fx[n_varb+1] = value;
*/

/*
  d = 1.0;
  for(i=0; i<bezout_dim; i++)
     {
      d *= cont_ds.jacobian_wrt_x[i][i];
      value += log( cont_ds.jacobian_wrt_x[i][i]*cont_ds.jacobian_wrt_x[i][i] )*
	       log( cont_ds.jacobian_wrt_x[i][i]*cont_ds.jacobian_wrt_x[i][i] );
     }
  fprintf(stderr,"det = %g        value = %g \n",value, d);
  fx[n_varb+1] = (1.0/value);

*/


/*
  m_inv = dmatrix(0,bezout_dim,0,bezout_dim);
  minvse( cont_ds.jacobian_wrt_x, m_inv, bezout_dim );
  frob_matnorm( m_inv, bezout_dim, &value );
  free_dmatrix(m_inv,0,bezout_dim,0,bezout_dim);
  
  fx[n_varb+1] = (1.0/value)*100.0;
*/


/*
  dgefa(cont_ds.jacobian_wrt_x,bezout_dim,bezout_dim,iwork);
  dgedi(cont_ds.jacobian_wrt_x,bezout_dim,bezout_dim,iwork,det_comp,work,10);   
  fx[n_varb+1] = det_comp[0]*pow(10.0,det_comp[1]);

  fx[n_varb+1] = fx[n_varb+1]/1.e170;
*/
  return(ierror);
}


int
sp_dfunc(nvar,fpar,ipar,x,fprime)
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
sp_check( color, n_varb, state, param )
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
      color[1] = SYS_GREEN;
     }

   return(ierror);
}


