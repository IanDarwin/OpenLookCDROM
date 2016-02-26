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

#define ZERO_EPS 1.0e-20

extern struct  Cont_Cntl_Ds            cont_ds;


int
static_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, nvar;
  double	**dmatrix();
  int           n_varb  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           n_param = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       0;
  nvar          =       n_varb + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  cont_alloc( max_iters, n_varb, n_param, nvar, xr, fpar );

  update_cont_state( (*xr)[0], state, parameters, n_varb, *n_aug_varb, n_param );

  for(i=1; i<= n_param; i++) (*fpar)[i] = parameters[i-1];
}


int
static_update(x,parm,n_aug_varb)
int     n_aug_varb;
double  *x, *parm;
{
}


int  
static_func(nvar,fpar,ipar,x,fx)
int	nvar, *ipar;
double	*fpar, *fx, *x;
{
	int		ierror, i, period=*((int *) pm( GET, "Flow.Skip_Size", NULL ));
	int		map_flag = *((int *) pm( GET, "Model.Mapping_Flag", NULL ));
	int		(*function)(), *ivector();
	int		n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
	int		n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
	double		*xhold, *state, *params, *v, *dvector();

	ierror = 0;

	state = cont_ds.r_workspace;
	params = state + n_varb;
	v = cont_ds.r_workspace + (n_varb + n_params + 2);

	function = (void *) pm( GET, "Model.DS_Def", NULL );
	for(i=0; i<n_params; i++) params[i] = fpar[i+1];

	update_ds_state( x, state, params, n_varb, 0, n_params );

	if( map_flag)
	   {
	    iter_forw( function, period, &fx[1], state, params, n_varb+1, 1.0, v, cont_ds.manifold);
	    for(i=0; i<n_varb; i++) fx[i+1] -= state[i];
           }
        else
	    function( &fx[1], state, params);

	return(ierror);
}


int
static_dfunc(nvar,fpar,ipar,x,fprime)
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
static_check( color, n_varb, state, param )
int		n_varb, *color;
double		*state, *param;
{
   int		i, j, mode, *iwork, ierror;
   int		sink=TRUE, source=TRUE, saddle=TRUE;
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
   
   jac = dmatrix(0,n_varb+1,0,n_varb+1); 

   get_Dxf( cont_ds.jacobian_wrt_x, n_varb, state, param ); 

   for(i=0; i<n_varb; i++)
     for(j=0; j<n_varb; j++)
	jac[i+1][j+1] = cont_ds.jacobian_wrt_x[i][j];
   ierror = rg(n_varb,n_varb,jac,&wr[0]-1,&wi[0]-1,0,NULL,iwork,dwork);
   if(ierror != 0) return(ierror);

   if(cont_ds.Debug_Level > 0)
     {
      fprintf(stderr,"\nstate = ");
      for(i=0; i<n_varb; i++) fprintf(stderr," %lg",state[i]);
      fprintf(stderr,"\nparam = ");
      for(i=0; i<n_params; i++) fprintf(stderr," %lg",param[i]);
      for(i=0; i<n_varb; i++) fprintf(stderr,"\neig(%d) = (%lg,%lg)",i, wr[i],wi[i]);
     }
    
   if(!map_flag)
     {
       for(i=0; i<n_varb; i++)
         {
          if( fabs(wr[i]) < ZERO_EPS )
    	    {saddle = FALSE;
    	     sink=FALSE;
    	     source=FALSE;
    	     if(fabs(wi[i]) < ZERO_EPS)
    	       color[2] = HUGE_POINT;}
          if( wr[i] < 0.0 ) source=FALSE;
          if( wr[i] > 0.0 ) sink=FALSE;
         }
     }
   else
     {
       for(i=0; i<n_varb; i++)
         {
          if( fabs( fabs(wr[i]) - 1.0) < ZERO_EPS )
       	    {saddle = FALSE;
       	     sink=FALSE;
       	     source=FALSE;
       	     if(fabs(wi[i]) < ZERO_EPS)
       	        color[2] = HUGE_POINT;}
          if( fabs(wr[i]) < 1.0 ) source=FALSE;
          if( fabs(wr[i]) > 1.0 ) sink=FALSE;
         }
     }

   free_dmatrix(jac,0,n_varb,0,n_varb);

   if(saddle) color[1] = SYS_GREEN;
   if(source) color[1] = SYS_RED;
   if(sink) color[1] = SYS_BLUE;

   return(ierror);

}
