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

#define TINY  1.e-30

int
rkqc_driver(out_state, in_state, dwork, integ_cntl, mode)
int			mode;
double			*out_state, *in_state, *dwork;
struct  Prop_DataS	*integ_cntl;
{
  int		i, var_dim, status = NO_ERROR;
  double	min_step, max_step, pgrow, pshrink, fcor, safety;
  double	t_step, htry, hnext, *yscal, eps, *unused_work;

  var_dim = integ_cntl->ph_space_dim-1;

  if (( mode == FIXED_STEP ) || (mode == FIXED_STEP_INIT))
    {
     t_step = (abs(integ_cntl->direction ) == FORWARD) ? integ_cntl->time_step:-integ_cntl->time_step;
     status = rk4(out_state, in_state, integ_cntl->parameters, t_step, var_dim, integ_cntl->function, dwork);
    }
  else
    {
     min_step = integ_cntl->panel_dp_values[2]; 
     max_step = integ_cntl->panel_dp_values[3];
     eps = integ_cntl->panel_dp_values[4];
     pgrow = integ_cntl->panel_dp_values[5];
     pshrink = integ_cntl->panel_dp_values[6];
     fcor = integ_cntl->panel_dp_values[7];
     safety = integ_cntl->panel_dp_values[8];
     htry = (abs(integ_cntl->direction ) == FORWARD) ? fabs(integ_cntl->estim_step):-fabs(integ_cntl->estim_step);
     
     yscal = dwork;
     unused_work = yscal + var_dim + 1;
     (integ_cntl->function)(yscal,in_state,integ_cntl->parameters);

     if( integ_cntl->panel_choice_values[0] == 0 ) /* constant fractional error case */
	 for(i=0; i<var_dim; i++) yscal[i] = fabs(in_state[i]) + fabs(htry*yscal[i]) + TINY;
     else /* bounded global error case */
	 for(i=0; i<var_dim; i++) yscal[i] *= htry;
  
     status = rkqc(out_state, in_state, var_dim, integ_cntl->parameters, 
                   htry, min_step, max_step, eps, pgrow, pshrink, fcor, safety,
	           yscal, &t_step, &hnext, integ_cntl->function, unused_work);

     if ( status == NO_ERROR ) integ_cntl->estim_step = hnext;

    }

  if ( status == NO_ERROR ) out_state[var_dim] = in_state[var_dim] + t_step;

  return( status );

}




/* ---------------------------------------------------------------------------------------------
   rkqc is the one-step routine for the forth-order Runge-Kutta algorithm with fifth-order
   stepsize regulation.  This implementation is adapted from
       
       Press, W.H., et. al., "Numerical Recipes in C," Cambridge University Press,
       New York, 1989, pps. 569-580.


   inputs: 	in_y		inital state (indep. varb value in last position!)
		dim		dimension of phase space + 1
		param		parameter values for dynamical system
		htry		initial stepsize estimate
		min_step	minimum allowed stepsize
		max_step	maximum allowed stepsize
		eps		error tolerance
		pgrow		stepsize growth exponent (see reference)
		pshrink		stepsize reduction exponent (see reference)
		fcor		5th order error correction factor
		safety		factor to produce slight reduction of full step
		yscal		scale factors for components in error calc 
		derivs		func ptr to derivative routine
		workspace	pre-allocated workspace for routine to use

   output:	out_y		final state 
		hdid		actual stepsize taken
		hnext		estimated next stepsize

   author: mrm		last change: 05-19-92


   --------------------------------------------------------------------------------------------- */
   

#define	MAX_COUNT  20

int 
rkqc(out_y, in_y, dim, param, htry, min_step, max_step, eps, pgrow, pshrink, fcor, safety,
     yscal, hdid, hnext, derivs, workspace)
int	dim, (*derivs)();
double  pgrow, pshrink, fcor, safety, min_step, max_step, htry, eps;
double	*workspace, *in_y, *out_y, *param, *yscal, *hdid, *hnext;
{
  int		i, count, status = NO_ERROR;
  extern int	rk4();
  double	hh,h,temp,errmax,errcon;
  double	*dydx1, *dydx2,*y1,*y2,*y3, *work, hsign;
/*  double	copysign(); */

  hsign = copysign(1.0,htry);
  dydx2 = workspace;
  dydx1 = dydx2 + dim + 2;
  y1 = dydx1 + dim + 2;
  y2 = y1 + dim + 2;
  y3 = y2 + dim + 2;
  work = y3 + dim + 2;

  errcon = pow( 4.0/safety, (1.0/pgrow) );
  for (i=0;i<=dim;i++) y2[i]=in_y[i];
  derivs(dydx2,in_y,param);

  h=htry;
  for (count=0; count<MAX_COUNT; count++)
    {
     hh=0.5*h;						/* take two half steps */
     if( status = rk4( y3, y2, param, hh, dim, derivs, work ) != NO_ERROR) return(status);
     derivs(dydx1,y3,param);
     if( status = rk4( y1, y3, param, hh, dim, derivs, work ) != NO_ERROR) return(status);

     if (fabs(h) < fabs(min_step))
       {
        system_mess_proc(1,"Step size too small in routine rkqc!");
	return( MINOR_ERROR );
       }      
     if( status = rk4( y3, y2, param, h, dim, derivs, work ) != NO_ERROR) return(status);	
     errmax=0.0;
     for (i=0;i<dim;i++)				/* estimate error */
       {
        y3[i]=y1[i]-y3[i];
        temp=fabs(y3[i]/yscal[i]);
        if (errmax < temp) errmax=temp;
       }
     errmax /= eps;
     if (errmax <= 1.0)					/* step succeeded; estimate size of */
       {						/* next step                        */
        *hdid=h;
        *hnext=(errmax > errcon) ? hsign*fabs(safety*h*exp(pgrow*log(errmax))) : 4.0*h;
	if ( fabs(*hnext) > fabs(max_step) ) 
	   *hnext = hsign*fabs(max_step);
	else if ( fabs(*hnext) < fabs(min_step) ) *hnext = hsign*fabs(min_step);
        for (i=0;i<dim;i++) out_y[i] = y1[i] + y3[i]*fcor;
	return( NO_ERROR );
       }
     else
        h=hsign*fabs(safety*h*exp(pshrink*log(errmax)));		/* step failed; reduce stepsize */
    }

  system_mess_proc(1,"Too many iterations required in rkqc!");
  return( MINOR_ERROR );
}




int
rkqc_init()
{
  static int		ifields[]={15};
  static double	dfields[]={1.e-6,1.0e-5,1.0e-10,0.2,1.e-5,-0.2,-0.25,0.06666,0.9};
  static int		sel_values[] = {2};
  static char		*ifield_names[]={"Newton Iter"};
  static char		*dfield_names[]={FINITE_DIFFERENCE_STEP_STRING, STOPPING_ERROR_STRING, "Min Step: ",
					 "Max Step: ","Err Tol: ","Pgrow: ","Pshrink: ","Fcor: ","Safety: "};
  static char		*usr_sel_labels[] = {"Error Bound: "};
  static char          *choice0[] = {"Const. Fractional Err.","Bounded Global Errors"}, **choice_array[2];
  static int		num_choices[] = {2};

  Int_Algol.Num_Ifields = 1;	/* these three MUST be set */
  Int_Algol.Num_Dfields = 9;
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
