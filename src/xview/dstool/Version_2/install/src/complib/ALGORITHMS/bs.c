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
#include <prop.h>
#include <complib.h>

#define IMAX		11
#define MAX_COUNT	20
#define UNDERFLOW_VAL	1.e-50
#define TINY		1.e-30


int
bs_driver(out_state, in_state, dwork, integ_cntl, mode)
int			mode;
double			*out_state, *in_state, *dwork;
struct  Prop_DataS	*integ_cntl;
{
  int		i, var_dim, nuse, status = NO_ERROR;
  double	min_step, max_step, grow, shrink, eps;
  double	t_step, htry, hnext, *yscal, *unused_work;


  var_dim = integ_cntl->ph_space_dim-1;

  min_step = integ_cntl->panel_dp_values[2];
  max_step = integ_cntl->panel_dp_values[3];
  htry = (abs(integ_cntl->direction ) == FORWARD) ? fabs(integ_cntl->estim_step):-fabs(integ_cntl->estim_step);

  if ((mode == FIXED_STEP ) || (mode == FIXED_STEP_INIT))
    {
/*     htry = integ_cntl->time_step;*/
     htry = (abs(integ_cntl->direction ) == FORWARD) ? integ_cntl->time_step:-integ_cntl->time_step;
     min_step = integ_cntl->time_step;
     max_step = integ_cntl->time_step;
    }
  eps = integ_cntl->panel_dp_values[4];
  grow = integ_cntl->panel_dp_values[5];
  shrink = integ_cntl->panel_dp_values[6];
  nuse = integ_cntl->panel_int_values[1];

  yscal = dwork;
  unused_work = yscal + var_dim + 1;
  (integ_cntl->function)(yscal,in_state,integ_cntl->parameters);

  if( integ_cntl->panel_choice_values[0] == 0 ) /* constant fractional error case */
     for(i=0; i<var_dim; i++) yscal[i] = fabs(in_state[i]) + fabs(htry*yscal[i]) + TINY;
  else /* bounded global error case */
      for(i=0; i<var_dim; i++) yscal[i] *= htry;

  status = bs(out_state, in_state, var_dim, integ_cntl->parameters, 
	      htry, min_step, max_step, eps, grow, shrink, nuse,
	      yscal, &t_step, &hnext, integ_cntl->function, unused_work);

  if ( status==NO_ERROR ) 
    {
     integ_cntl->estim_step = hnext;
     out_state[var_dim] = in_state[var_dim] + t_step;
    }

  return(status);
}



/* ---------------------------------------------------------------------------------------------
   bs() is the one-step routine for the Bulirsch-Stoer algorithm with Richardson
   Exrtapolation.  This implementation is adapted from

       Press, W.H., et. al., "Numerical Recipes in C," Cambridge University Press,
       New York, 1989, pps. 582-588.


   inputs:      y_in            inital state (indep. varb value in last position!)
                dim             dimension of phase space + 1
                params          parameter values for dynamical system
                htry            initial stepsize estimate
                min_step        minimum allowed stepsize
                max_step        maximum allowed stepsize
                eps             error tolerance
                grow            stepsize growth factor (see reference)
                shrink          stepsize reduction factor (see reference)
		nuse            number of intervals to use (see reference)
                yscal           scale factors for components in error calc
                derivs          func ptr to derivative routine
                workspace       pre-allocated workspace for routine to use

   output:      y_out           final state
                hdid            actual stepsize taken
                hnext           estimated next stepsize

   author: mrm          last change: 05-20-92

   --------------------------------------------------------------------------------------------- */



double **d=0,*x=0;	/* defining declaration */

int
bs( y_out, y_in, dim, params, htry, min_step, max_step, eps, 
    grow, shrink, nuse, yscal, hdid, hnext, derivs, workspace)
int 	dim, nuse;
int 	(*derivs)();
double 	min_step, max_step, grow, shrink, eps, htry;
double 	*y_in, *y_out, *params, *yscal, *hdid, *hnext, *workspace;
{
  int 		i, j, count;
  double 	xsav, xest, h, errmax, temp, hsign;
  double 	*y1, *ysav, *dysav, *yseq, *yerr, *unused_work;
  static int 	nseq[IMAX+1]={2,4,6,8,12,16,24,32,48,64,96};
  extern void   mmid(), rzextr();
  double	**dmatrix(); /* copysign(); */

  y1 = workspace;
  ysav=y1 + dim + 1;
  dysav=ysav + dim + 1;
  yseq = dysav + dim + 1;
  yerr = yseq + dim + 1;
  x=yerr + dim + 1;
  unused_work = x + IMAX + 1;
  d=dmatrix(0,dim,0,nuse);
  hsign = copysign(1.0,htry);

  h = (fabs(htry) < fabs(min_step)) ? hsign*fabs(min_step):htry;
  h = (fabs(htry) > fabs(max_step)) ? hsign*fabs(max_step):htry;

  derivs(dysav, y_in, params);
  xsav=y_in[dim];
  for (i=0; i<=dim; i++) ysav[i]=y_in[i];
   
  for (count=0; count<MAX_COUNT; count++) 
    {
     for (i=0; i<IMAX; i++) 
       {
        mmid(ysav,dysav,dim,xsav,h,nseq[i],yseq,params,derivs,unused_work);
        xest=(temp=h/nseq[i],temp*temp);
        rzextr(i,xest,yseq,y1,yerr,dim,nuse,unused_work);
        errmax=0.0;
        for (j=0; j<dim; j++)
	  if (errmax < fabs(yerr[j]/yscal[j])) errmax=fabs(yerr[j]/yscal[j]);
        errmax /= eps;
        if (errmax < 1.0) 
	  {
	   *hdid=h;
	   *hnext = i==nuse? hsign*fabs(h*shrink) : i==nuse-1?
	            hsign*fabs(h*grow) : hsign*fabs((h*nseq[nuse-1])/nseq[i]);
           for (i=0; i<dim; i++) y_out[i]=y1[i];
           free_dmatrix( d, 0, dim, 0, nuse);
	   return( NO_ERROR );
          }
       }
     h *= 0.25;
     for (i=0; i<(IMAX-nuse)/2; i++) h /= 2.0;
     if ( fabs(y_in[dim]+h - y_in[dim]) < UNDERFLOW_VAL ) 
       {
        system_mess_proc(1,"Step size underflow in BSSTEP"); /* fjw 9/7/92 */
        free_dmatrix( d, 0, dim, 0, nuse);
        return( MINOR_ERROR );
       }
    }
  free_dmatrix( d, 0, dim, 0, nuse);
  return( NO_ERROR );
}

/* 
  Step iest in using rational function extrapolation to evaluate yz
  at 0. In step iest, new values yest at xest are introduced and added 
  to the global array d each column of which is being interprolated. 
  (d[iest][j] contains the j'th value from step iest.) The number of 
  components of yest is dim, and only the last nuse values are used in the 
  interpolation. At output, yz contains the interpolated value at 0 and
  dy the estimate of the error.
  */
void 
rzextr(iest,xest,yest,yz,dy,dim,nuse,workspace)
int	iest,dim,nuse;
double	 xest, *yest, *yz, *dy, *workspace;
{
  int m1,k,j;
  double yy,v,ddy,c,b1,b,*fx;

  fx=workspace;
  x[iest]=xest;
  if (iest == 0)
    for (j=0; j<dim; j++) 
      {
       yz[j]=yest[j];
       d[j][0]=yest[j];
       dy[j]=yest[j];
      }
  else 
   {
    m1=(iest+1 < nuse ? iest+1 : nuse);
    for (k=1; k<=m1-1; k++)
      fx[k]=x[iest-k]/xest;
    for (j=0; j<dim; j++) 
      {
       yy=yest[j];
       v=d[j][0];
       c=yy;
       d[j][0]=yy;
       for (k=2; k<=m1; k++) 
	 {
	  b1=fx[k-1]*v;
	  b=b1-c;
	  if (b) 
	    {
	     b=(c-v)/b;
	     ddy=c*b;
	     c=b1*b;
	    } 
	  else ddy=v;
	  if (k != m1) v=d[j][k-1];
	  d[j][k-1]=ddy;
	  yy += ddy;
         }
       dy[j]=ddy;
       yz[j]=yy;
      }
   }
}

/* 
  Modified midpoint method integration from initial value y at time xs to
  final value yout at time xs + htot. The number of substeps is nstep.
  The initial value fo the derivative is dydx, and derivs is a function
  describing the vector field. Nvar is the number of variables excluding time.
   */
void 
mmid(y, dydx, nvar, xs, htot, nstep, yout, params, derivs, workspace)
int	 nvar,nstep;
double	 xs, htot;
double	 *y, *dydx, *yout, *params, *workspace;
void	(*derivs)();
{
  int 		n,i;
  double	swap,h2,h;
  double	*ym,*yn;

  ym=workspace;     
  yn=ym + nvar + 1;
  h=htot/nstep;
  for (i=0; i<nvar; i++) 
    {
     ym[i]=y[i];
     yn[i]=y[i]+h*dydx[i];
    }
/*  x=xs+h;*/
  yn[nvar] = xs + h;
  derivs(yout,yn,params);
  h2=2.0*h;
  for (n=1; n<nstep; n++) 
    {
     for (i=0; i<nvar; i++) 
       {
        swap=ym[i]+h2*yout[i];
        ym[i]=yn[i];
        yn[i]=swap;
       }
/*     x += h;*/
     yn[nvar] += h;
     derivs(yout,yn,params);
    }
  for (i=0; i<nvar; i++)
    yout[i]=0.5*(ym[i]+yn[i]+h*yout[i]);
}



int
bs_init()
{
  static int		ifields[]={15,7};
  static double	dfields[]={1.e-6,1.0e-5,1.0e-10,0.2,1.e-5,0.95,1.2};
  static int		sel_values[] = {2};
  static char		*ifield_names[]={"Newton Iter","BS Intervals: "};
  static char		*dfield_names[]={FINITE_DIFFERENCE_STEP_STRING, STOPPING_ERROR_STRING, "Min Step: ",
					   "Max Step: ","Err Tol: ","Shrink: ","Grow: "};
  static char		*usr_sel_labels[] = {"Error Bound: "};
  static char          *choice0[] = {"Const. Fractional Err.","Bounded Global Errors"}, **choice_array[2];
  static int		num_choices[] = {2};

  Int_Algol.Num_Ifields = 2;	/* these three MUST be set */
  Int_Algol.Num_Dfields = 7;
  Int_Algol.Num_Sel_Items = 1;

  Int_Algol.Ifield_Names = ifield_names;
  Int_Algol.Dfield_Names = dfield_names;
  Int_Algol.Sel_Labels = usr_sel_labels;
  Int_Algol.Ifields = ifields;
  Int_Algol.Dfields = dfields;
  Int_Algol.Num_Sel_Choices = num_choices;
  Int_Algol.Sel_Values = sel_values;
  choice_array[0] = choice0;
  Int_Algol.Sel_Choices = choice_array;

}


