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
hopf_init(n_aug_varb,max_iters,xr,fpar,state,parameters)
int	*n_aug_varb, max_iters;
double	***xr, **fpar, *state, *parameters;
{
  int		i, j, nvar, mode, ierror, index,start;
  double	**dmatrix(), *dvector(), *fd_step, *dwork, *wr, *wi, **a, **eig_vec, min, max, mag, sqrt(), fabs();
  int           ph_space_dim  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           parameter_dim = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  *n_aug_varb   =       2*ph_space_dim+1;
  nvar          =       ph_space_dim + (*n_aug_varb) + Cont_Sel[Cont_Cur_Choice].Num_Req_Param;

  *xr = (double **) dmatrix(0,max_iters+1,0,nvar+1);
  *fpar = (double *) calloc(parameter_dim+ph_space_dim+1, sizeof(double));    

  update_cont_state( (*xr)[0], state, parameters, ph_space_dim, *n_aug_varb, parameter_dim );

  for(i=1; i<= parameter_dim; i++) (*fpar)[i] = parameters[i-1];

  fd_step = dvector(0,ph_space_dim);
  wr = dvector(0,ph_space_dim);
  wi = dvector(0,ph_space_dim);
  dwork = dvector(0,1000);

  for(i=0; i<ph_space_dim; i++) fd_step[i] = 1.0e-6;

  if( (void *) pm( GET, "Model.DfDx", NULL ) == NULL )
     mode = CEN_DIFF;
  else mode = ANALYTIC;

  eig_vec = dmatrix(0,ph_space_dim,0,ph_space_dim);
  a = dmatrix(0,ph_space_dim,0,ph_space_dim);
  ierror = dfdx( a, ph_space_dim, fd_step, state, parameters, 0, cont_ds.manifold, FALSE,
                 (void *) pm( GET, "Model.Ds_Def", NULL ),
                 (void *) pm( GET, "Model.DfDx", NULL ), mode, dwork );
  if(ierror != 0) return(ierror);

  get_eigenvec(a, ph_space_dim, wr, wi, eig_vec);

  fprintf(stderr,"Derivative Matrix: \n------------------\n");
  for(i=0; i<ph_space_dim; i++)
     {
      fprintf(stderr,"\n");
      for(j=0; j<ph_space_dim; j++)
	fprintf(stderr," %10.6lg",a[i][j]);
     }
  fprintf(stderr,"\n \n");

  fprintf(stderr,"Eigen Matrix: \n----------------\n\n");
  for(i=0; i<ph_space_dim; i++)
   {
    fprintf(stderr,"\n(%10.6lg,%10.6lg)  ",wr[i],wi[i]);
    for(j=0; j<ph_space_dim; j++)
       fprintf(stderr," %10.6lg",eig_vec[i+1][j+1]);
   }    

  fprintf(stderr,"\n \n");

/*  (*xr)[0][5] = 0.6408;
  (*xr)[0][6] = -0.0087;
  (*xr)[0][7] = 0.002;
  (*xr)[0][8] = -0.0012;
  (*xr)[0][9] = 0.7672;
  (*xr)[0][10] = -0.0234;
  (*xr)[0][11] = -0.0036;
  (*xr)[0][12] = 0.0027;
  (*xr)[0][13] = 0.71571; */

  min = fabs(wr[0]);
  index = 0;
  for(i=1; i<ph_space_dim; i++)
     if( fabs(wr[i])<min ) 
       {
        index = i;
	min = fabs(wr[i]);
       }

  fprintf(stderr,"index = %d \n",index);

  if( fabs(wi[index]) < 1.0e-2 )
     for(i=1; i<=ph_space_dim; i++)
       {
        (*xr)[0][ph_space_dim+i] = eig_vec[i][index+1];
	  fprintf(stderr,"%lg \n",eig_vec[i][index+1]);
        (*xr)[0][2*ph_space_dim+i] = 0.0;
       }
  else
     for(i=1; i<=ph_space_dim; i++)
       {
        (*xr)[0][ph_space_dim+i] = eig_vec[i][index+1];
	  fprintf(stderr,"(%lg,%lg) \n",eig_vec[i][index+1],eig_vec[i][index+2]);
        (*xr)[0][2*ph_space_dim+i] = eig_vec[i][index+2];
       }
  
  max = 0.0;
  for(i=1; i<=ph_space_dim; i++)
    {
     (*fpar)[i+parameter_dim] = 0.0;
     if( fabs((*xr)[0][i+ph_space_dim]) > max )
        {
         max = fabs((*xr)[0][i+ph_space_dim]);
         index = i;
        }
    }

  (*fpar)[index+parameter_dim] = 1.0;

  free_dmatrix(a,0,ph_space_dim,0,ph_space_dim);
  free_dmatrix(eig_vec,0,ph_space_dim,0,ph_space_dim);
  free_dvector(fd_step,0,ph_space_dim);
  free_dvector(wr,0,ph_space_dim);
  free_dvector(wi,0,ph_space_dim);
  free_dvector(dwork,0,ph_space_dim);
}


int
hopf_update(x,parm,n_aug_varb)
int	n_aug_varb;
double	*x, *parm;
{
  int		i, max_index, index;
  double	max, fabs();
  int           ph_space_dim  = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1; 
  int           parameter_dim = *((int *) pm( GET, "Model.Param_Dim", NULL ));


  for(i=1; i<=ph_space_dim; i++)
     if( fabs(parm[i+parameter_dim]) > 0.1 ) index = i;

  max = 0.0;
  max_index = index;

  for(i=1; i<=ph_space_dim; i++)
      if( 0.5*fabs(x[ph_space_dim+i]) > max ) 
	{ 
	 max_index = i;
	 max = 0.5*fabs(x[ph_space_dim+i]);
        }

  if( fabs(x[ph_space_dim+index]) < max )
     {
      for(i=parameter_dim+1; i<=parameter_dim+ph_space_dim; i++) parm[i] = 0.0;
      parm[parameter_dim+max_index] = 1.0;
     }
}



int  
hopf_func(nvar,fpar,ipar,x,fx)
int	nvar, *ipar;
double	*fpar, *fx, *x;
{
  int		  ierror=0, i, j, mode;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));
  int             (*function)();
  double          *state, *params, *v, *fd_step, **fprime, **dmatrix();

  state = cont_ds.r_workspace;
  params = state + n_varb + 1;
  fd_step  = params + n_params +1;
  v = fd_step + n_varb + 1;
  function = (void *) pm( GET, "Model.Ds_Def", NULL );
  for(i=0; i<n_params; i++) params[i] = fpar[i+1];

  update_ds_state( x, state, params, n_varb, 2*n_varb+1, n_params );
  function( &fx[1], state, params);

  for(i=0; i<n_varb; i++) fd_step[i] = 1.0e-6;

  fprime = dmatrix(0,n_varb,0,n_varb);

  if( (void *) pm( GET, "Model.DfDx", NULL ) == NULL )
     mode = CEN_DIFF;
  else mode = ANALYTIC;
  
  ierror = dfdx( fprime, n_varb, fd_step, state, params, 0, cont_ds.manifold, FALSE,
	         (void *) pm( GET, "Model.Ds_Def", NULL ),
		 (void *) pm( GET, "Model.DfDx", NULL ), mode, v );

  for(i=0; i<n_varb; i++)
    {
     fx[n_varb+i+1] = 0.0;
     for(j=0; j<n_varb; j++)
        fx[n_varb+i+1] += fprime[i][j]*x[n_varb+j+1];
     fx[n_varb+i+1] -= x[3*n_varb+1]*x[2*n_varb+i+1];
    }

  for(i=0; i<n_varb; i++)
    {
     fx[2*n_varb+i+1] = 0.0;
     for(j=0; j<n_varb; j++)
        fx[2*n_varb+i+1] += fprime[i][j]*x[2*n_varb+j+1];
     fx[2*n_varb+i+1] += x[3*n_varb+1]*x[n_varb+i+1];
    }

  fx[3*n_varb+1] = -1.0;
  for(i=0; i<n_varb; i++)
     fx[3*n_varb+1] += x[n_varb+i+1]*fpar[n_params+i+1];

  fx[3*n_varb+2] = -1.0;
  for(i=0; i<n_varb; i++)
     fx[3*n_varb+2] += x[2*n_varb+i+1]*fpar[n_params+i+1];

  free_dmatrix(fprime,0,n_varb,0,n_varb);

  return(ierror);
}


int
hopf_dfunc(nvar,fpar,ipar,x,fprime)
int	nvar, *ipar;
double	*fpar, *x, **fprime;
{
  int		  ierror=0, i;
  double	  *state, *param, *omega;
  int             n_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL )) - 1;
  int             n_params = *((int *) pm( GET, "Model.Param_Dim", NULL ));

  double		*dvector();

  state = cont_ds.r_workspace;
  omega = state + n_varb + 1;
  param = omega + 2*n_varb + 3; 

  for(i=0; i<n_params; i++) param[i] = fpar[i+1];

  update_ds_state( x, state, param, n_varb, 2*n_varb+1, n_params );
  for(i=0; i<2*n_varb+1; i++)
    {
     omega[i] = x[n_varb+i+1];
     param[n_params+i-1] = fpar[n_params+i];
    }

  hh_hopf1_jac( fprime, state, omega, param );

  return(ierror);
}



int
hopf_check( color, n_varb, state, param )
int		n_varb, *color;
double		*state, *param;
{
   int	ierror=0;

   color[1] = SYS_GREEN;
   color[2] = MED_POINT;

   return(ierror);

}



int
hh_hopf1_jac( matrix, varbs, omega, param )
double  **matrix, *varbs, *omega, *param;
{
  int			i, j;
  extern double 	psi(), phi(), exp();
  double		v, m, n, h, a1, a2, a3, a4, b1, b2, b3, b4, lambda,
			gna, vna, gk, vk, gl, vl, I,
			T, am, bm, an, bn, ah, bh,
			l1,l2,l3,l4,phi_t,psi_v,temp;
  v = varbs[0];
  m = varbs[1];
  n = varbs[2];
  h = varbs[3];

  gna = param[0];
  gk  = param[1];
  gl  = param[2];
  vna = param[3];
  vk  = param[4];
  vl  = param[5];
  T   = param[6];
  I   = param[7];

  l1  = param[8];
  l2  = param[9];
  l3  = param[10];
  l4  = param[11];

  a1 = omega[0];
  a2 = omega[1];
  a3 = omega[2];
  a4 = omega[3];

  b1 = omega[4];
  b2 = omega[5];
  b3 = omega[6];
  b4 = omega[7];

  lambda = omega[8];

/*
  fprintf(stderr,"v,m,n,h = %lg %lg %lg %lg \n",v,m,n,h);
  fprintf(stderr,"gna,gk,gl,vna,vk,vl,T,I= %lg %lg %lg %lg %lg %lg %lg %lg \n",gna,gk,gl,vna,vk,vl,T,I);
  fprintf(stderr,"omega = %lg %lg %lg %lg \n",o1,o2,o3,o4);
*/

  matrix[0][0] = matrix[4][4] = matrix[8][8] = -(gl + gna*h*m*m*m + gk*n*n*n*n);
  matrix[0][1] = matrix[4][5] = matrix[8][9] = -3.0*gna*h*m*m*(v-vna);
  matrix[0][2] = matrix[4][6] = matrix[8][10] = -4.0*gk*n*n*n*(v-vk);
  matrix[0][3] = matrix[4][7] = matrix[8][11] =  -(gna*m*m*m*(v-vna));
  matrix[1][0] = matrix[5][4] = matrix[9][8] = phi(T)*( ((1.0-m)/10.0)/( exp(((25.0+v)/10.0))-1.0 ) -
			(2.0/9.0)*m*exp(v/18.0) - 
			psi( (25.0+v)/10.0 )*psi( (25.0+v)/10.0 )*((1.0-m)/10.0)*
			exp((25.0+v)/10.0)/((25.0+v)/10.0) );
  matrix[1][1] = matrix[5][5] = matrix[9][9] = phi(T)*(-4.0*exp(v/18.0) - psi( (25.0+v)/10.0 ) );
  matrix[1][2] = matrix[5][6] = matrix[9][10] = 0.0;
  matrix[1][3] = matrix[5][7] = matrix[9][11] = 0.0;
  matrix[2][0] = matrix[6][4] = matrix[10][8] = phi(T)*( ((1.0-n)/100.0)/(exp( (10.0+v)/10.0 ) - 1.0 ) -
			n*exp(v/80.0)/640.0 -
			((1.0-n)/100.0)*exp((10.0+v)/10.0)*psi( (10.0+v)/10.0 )/(exp( (10.0+v)/10.0 ) - 1.0 ) );
  matrix[2][1] = matrix[6][5] = matrix[10][9] = 0.0; 
  matrix[2][2] = matrix[6][6] = matrix[10][10] = -phi(T)*(exp(v/80.0)/8.0 + (10.0+v)/(exp((10.0+v)/10.0)-1.0)/100.0);
  matrix[2][3] = matrix[6][7] = matrix[10][11] = 0.0;
  matrix[3][0] = matrix[7][4] = matrix[11][8] = phi(T)*( exp(v/20.0)*(1.0-h)*7.0/2000.0 + 
	                (h/10.0)*exp((30.0+v)/10.0)/( (1.0+exp((30.0+v)/10.0))*(1.0+exp((30.0+v)/10.0))) );
  matrix[3][1] = matrix[7][5] = matrix[11][9] = 0.0;
  matrix[3][2] = matrix[7][6] = matrix[11][10] = 0.0;
  matrix[3][3] = matrix[7][7] = matrix[11][11] = -phi(T)*(7.0*exp(v/20.0)/100.0 + 1.0/(1.0+exp((30.0+v)/10.0)) );


  matrix[4][0] = -3.0*a2*gna*h*m*m - a4*gna*m*m*m - 4.0*a3*gk*n*n*n;
  matrix[4][1] = -3.0*a1*gna*h*m*m - 6.0*a2*gna*h*m*(v-vna) - 3.0*a4*gna*m*m*(v-vna);
  matrix[4][2] = -4.0*a1*gk*n*n*n - 12.0*a3*gk*n*n*(v-vk);
  matrix[4][3] = -a1*gna*m*m*m - 3.0*a2*gna*m*m*(v-vna);

  phi_t = phi(T);
  psi_v = psi((25.0+v)/10.0);
  temp = exp((25.0+v)/10.0);

  matrix[5][0] = phi_t*( 
   -(18.0*a2+a1*m)*exp(v/18.0)/81.0 
   + ( 10.0*a2*( temp*(15.0+v)+10.0)+ 
     a1*temp*(m*(v+45.0)-v-45.0) )/(1000.0*(temp-1.0)*(temp-1.0)) 
   + a1*exp((25.0+v)/5.0)*(1.0-m)*(25.0+v)/
   (500.0*(temp-1.0)*(temp-1.0)*(temp-1.0)) );

  matrix[5][1] = phi_t*a1*( psi_v*psi_v*temp/(25.0+v) - psi_v/(25.0+v) - (2.0/9.0)*exp(v/18.0) );
  matrix[5][2] = 0.0;
  matrix[5][3] = 0.0;

  matrix[9][0] = phi_t*( 
   -(18.0*b2+b1*m)*exp(v/18.0)/81.0 
   + ( 10.0*b2*( temp*(15.0+v)+10.0)+ 
     b1*temp*(m*(v+45.0)-v-45.0) )/(1000.0*(temp-1.0)*(temp-1.0)) 
   + b1*exp((25.0+v)/5.0)*(1.0-m)*(25.0+v)/
   (500.0*(temp-1.0)*(temp-1.0)*(temp-1.0)) );

  matrix[9][1] = phi_t*b1*( psi_v*psi_v*temp/(25.0+v) - psi_v/(25.0+v) - (2.0/9.0)*exp(v/18.0) );
  matrix[9][2] = 0.0;
  matrix[9][3] = 0.0;
  
  psi_v = psi((10.0+v)/10.0);
  temp = exp((10.0+v)/10.0);
  matrix[6][0] = a3*phi_t*( temp*psi_v*psi_v/(10.0*(10.0+v)) -
		 0.0015625*exp(v/80.0) - psi_v/(10.0*(10.0+v)) )+
		 a1*phi_t*( -temp*(1.0-n)*psi_v*psi_v/( 5.0*(10.0+v)*(10.0+v) ) -
		 0.00001953125*exp(v/80)*n +
		 exp((10.0+v)/5.0)*(1.0-n)*psi_v*psi_v*psi_v/(5.0*(10.0+v)*(10.0+v)) -
                 temp*(1.0-n)*psi_v*psi_v/(100.0*(10.0+v)));
  matrix[6][1] = 0.0;
  matrix[6][2] = a1*phi_t*( temp*psi_v*psi_v/(10.0*(10.0+v)) - 1.0/(100.0*(temp-1)) - exp(v/80.0)/640.0 );
  matrix[6][3] = 0.0;

  temp = exp((30.0+v)/10.0);
  matrix[7][0] = a4*phi_t*(temp/(10.0*(1+temp)*(1.0+temp))-7.0*exp(v/20.0)/2000.0) +
		 a1*phi_t*(0.000175*exp(v/20)*(1.0-h) - exp((30.0+v)/5.0)*h/(50.0*(1.0+temp)*(1.0+temp)*(1.0+temp)) +
		 temp*h/(100.0*(1.0+temp)*(1.0+temp)) );
  matrix[7][1] = 0.0;
  matrix[7][2] = 0.0;
  matrix[7][3] = a1*phi_t*(temp/( 10.0*(1.0+temp)*(1.0+temp) ) - 0.0035*exp(v/20.0));

  matrix[8][0] = -3.0*b2*gna*h*m*m - b4*gna*m*m*m - 4.0*b3*gk*n*n*n;
  matrix[8][1] = -3.0*b1*gna*h*m*m - 6.0*b2*gna*h*m*(v-vna) - 3.0*b4*gna*m*m*(v-vna);
  matrix[8][2] = -4.0*b1*gk*n*n*n - 12.0*b3*gk*n*n*(v-vk);
  matrix[8][3] = -b1*gna*m*m*m - 3.0*b2*gna*m*m*(v-vna);

  matrix[10][0] = b3*phi_t*( temp*psi_v*psi_v/(10.0*(10.0+v)) -
		 0.0015625*exp(v/80.0) - psi_v/(10.0*(10.0+v)) )+
		 b1*phi_t*( -temp*(1.0-n)*psi_v*psi_v/( 5.0*(10.0+v)*(10.0+v) ) -
		 0.00001953125*exp(v/80)*n +
		 exp((10.0+v)/5.0)*(1.0-n)*psi_v*psi_v*psi_v/(5.0*(10.0+v)*(10.0+v)) -
                 temp*(1.0-n)*psi_v*psi_v/(100.0*(10.0+v)));
  matrix[10][1] = 0.0;
  matrix[10][2] = b1*phi_t*( temp*psi_v*psi_v/(10.0*(10.0+v)) - 1.0/(100.0*(temp-1)) - exp(v/80.0)/640.0 );
  matrix[10][3] = 0.0;

  temp = exp((30.0+v)/10.0);
  matrix[11][0] = b4*phi_t*(temp/(10.0*(1+temp)*(1.0+temp))-7.0*exp(v/20.0)/2000.0) +
		 b1*phi_t*(0.000175*exp(v/20)*(1.0-h) - exp((30.0+v)/5.0)*h/(50.0*(1.0+temp)*(1.0+temp)*(1.0+temp)) +
		 temp*h/(100.0*(1.0+temp)*(1.0+temp)) );
  matrix[11][1] = 0.0;
  matrix[11][2] = 0.0;
  matrix[11][3] = b1*phi_t*(temp/( 10.0*(1.0+temp)*(1.0+temp) ) - 0.0035*exp(v/20.0));

  matrix[12][4] = matrix[13][8] = l1;
  matrix[12][5] = matrix[13][9] = l2;
  matrix[12][6] = matrix[13][10] = l3;
  matrix[12][7] = matrix[13][11] = l4;

  matrix[8][4] = lambda;
  matrix[9][5] = lambda;
  matrix[10][6] = lambda;
  matrix[11][7] = lambda;

  matrix[4][8] = -lambda;
  matrix[5][9] = -lambda;
  matrix[6][10] = -lambda;
  matrix[7][11] = -lambda;

  matrix[4][12] = -b1;
  matrix[5][12] = -b2;
  matrix[6][12] = -b3;
  matrix[7][12] = -b4;

  matrix[8][12] = a1;
  matrix[9][12] = a2;
  matrix[10][12] = a3;
  matrix[11][12] = a4;

  matrix[0][13] = gk*n*n*n*n;
  matrix[4][13] = 4.0*gk*n*n*n*a3;
  matrix[8][13] = 4.0*gk*n*n*n*b3;
  matrix[0][14] = -1.0;			
}

