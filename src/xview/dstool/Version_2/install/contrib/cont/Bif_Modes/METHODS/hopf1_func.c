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

#define WORK_SIZE  1000
#define CUTOFF 1.0e-2

extern struct  Cont_Cntl_Ds            cont_ds;


int
hopf1_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, j, nvar, mode, ierror, index,start,a_biprod_dim;
  double	**dmatrix(), *dvector(), *fd_step, *dwork, **a, det, fabs(), **a_biprod;
  int           ph_space_dim  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           parameter_dim = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       0;
  nvar          =       ph_space_dim + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  *xr = (double **) dmatrix(0,max_iters+1,0,nvar+1);
  *fpar = (double *) calloc(parameter_dim+ph_space_dim+1, sizeof(double));    

  update_cont_state( (*xr)[0], state, parameters, ph_space_dim, *n_aug_varb, parameter_dim );

  for(i=1; i<= parameter_dim; i++) (*fpar)[i] = parameters[i-1];

  fd_step = dvector(0,ph_space_dim);
  dwork = dvector(0,1000);

  for(i=0; i<ph_space_dim; i++) fd_step[i] = 1.0e-6;

  if( (void *) pm( GET, "Model.DfDx", NULL ) == NULL )
     mode = CEN_DIFF;
  else mode = ANALYTIC;

  a = dmatrix(0,ph_space_dim,0,ph_space_dim);
  ierror = dfdx( a, ph_space_dim, fd_step, state, parameters, 0, cont_ds.manifold, FALSE,
                 (void *) pm( GET, "Model.Ds_Def", NULL ),
                 (void *) pm( GET, "Model.DfDx", NULL ), mode, dwork );
  if(ierror != 0) return(ierror);

  a_biprod_dim = (int)((ph_space_dim-1)*ph_space_dim/2);
  a_biprod = dmatrix(0,a_biprod_dim,0,a_biprod_dim);
  biprod( a_biprod, a, ph_space_dim, &det );
  (*xr)[0][ph_space_dim+1] = det;

/*
  fprintf(stderr,"Derivative Matrix: \n------------------\n");
  for(i=0; i<ph_space_dim; i++)
     {
      fprintf(stderr,"\n");
      for(j=0; j<ph_space_dim; j++)
	fprintf(stderr," %10.6lg",a[i][j]);
     }
  fprintf(stderr,"\n \n");
  for(i=0; i<6; i++)
     {
      fprintf(stderr,"\n");
      for(j=0; j<6; j++)
	fprintf(stderr," %10.6lg",a_biprod[i][j]);
     }
  fprintf(stderr,"\n det = %lg \n",det);
  fprintf(stderr,"\n \n");
*/

  free_dmatrix(a,0,ph_space_dim,0,ph_space_dim);
  free_dmatrix(a_biprod,0,a_biprod_dim,0,a_biprod_dim);
  free_dvector(fd_step,0,ph_space_dim);
  free_dvector(dwork,0,ph_space_dim);
}


int
hopf1_update(x,parm,n_aug_varb)
int	n_aug_varb;
double	*x, *parm;
{
  int           ph_space_dim  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           parameter_dim = *((int *) pm( GET, "Model.Param_Dim", NULL ));

}



int  
hopf1_func(nvar,fpar,ipar,x,fx)
int	nvar, *ipar;
double	*fpar, *fx, *x;
{
  int		  ierror=0, i, j, mode, fprime_biprod_dim;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  int             (*function)();
  double          det, **fprime_biprod, *state, *params, *v, *fd_step, **fprime, **dmatrix();

  state = cont_ds.r_workspace;
  params = state + n_varb + 1;
  fd_step  = params + n_params +1;
  v = fd_step + n_varb + 1;
  function = (void *) pm( GET, "Model.Ds_Def", NULL );
  for(i=0; i<n_params; i++) params[i] = fpar[i+1];

  update_ds_state( x, state, params, n_varb, 0, n_params );
  function( &fx[1], state, params);

  for(i=0; i<n_varb; i++) fd_step[i] = 1.0e-6;

  fprime = dmatrix(0,n_varb,0,n_varb);

  if( (void *) pm( GET, "Model.DfDx", NULL ) == NULL )
     mode = CEN_DIFF;
  else mode = ANALYTIC;
     mode = CEN_DIFF;
  
  ierror = dfdx( fprime, n_varb, fd_step, state, params, 0, cont_ds.manifold, FALSE,
	         (void *) pm( GET, "Model.Ds_Def", NULL ),
		 (void *) pm( GET, "Model.DfDx", NULL ), mode, v );

  fprime_biprod_dim = (int)((n_varb-1)*n_varb/2);
  fprime_biprod = dmatrix(0,fprime_biprod_dim,0,fprime_biprod_dim);
  biprod( fprime_biprod, fprime, n_varb, &det );
  fx[n_varb+1] = det;
  free_dmatrix(fprime,0,n_varb,0,n_varb);
  free_dmatrix(fprime_biprod,0,fprime_biprod_dim,0,fprime_biprod_dim);

  return(ierror);
}


int
hopf1_dfunc(nvar,fpar,ipar,x,fprime)
int	nvar, *ipar;
double	*fpar, *x, **fprime;
{
}


int
hopf1_check( color, n_varb, state, param )
int		n_varb, *color;
double		*state, *param;
{
   int	ierror=0;

   color[1] = SYS_GREEN;
   color[2] = MED_POINT;

   return(ierror);

}



int
biprod( a_biprod, a, n, det )
double	**a_biprod, **a, *det;
int	n;
{
  int		row=0, col=0, p, q, r, s;  
  int		biprod_dim  = (int) ((n-1)*n)/2;
  double	det_comp[2], pow();

  int		ipvt[1000];
  double	work[1000];

  for(p=1; p<n; p++)
    {
     for(q=0; q<p; q++)
       {
        for(r=1; r<n; r++)
	  {
           for(s=0; s<r; s++)
	     {
	      if(r==q)
		a_biprod[row][col] = -a[p][s];
              else if( (r!=p) && (s==q) )
		a_biprod[row][col] = a[p][r];
              else if( (r==p) && (s==q) )
		a_biprod[row][col] = a[p][p]+a[q][q];
              else if( (r==p) && (s!=q) )
		a_biprod[row][col] = a[q][s];
	      else if(s==p)
		a_biprod[row][col] = -a[q][r];
              else
		a_biprod[row][col] = 0.0;
	      ++col;
	     }
          }
        ++row;
        col = 0;
       }
      }

  dgefa(a_biprod,biprod_dim,biprod_dim,ipvt);
  dgedi(a_biprod,biprod_dim,biprod_dim,ipvt,det_comp,work,10);	/* determinant only */

  *det = det_comp[0]*pow(10.0,det_comp[1]);
}
