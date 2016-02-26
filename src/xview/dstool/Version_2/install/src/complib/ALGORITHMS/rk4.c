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
#include <constants.h>
#include <prop.h>
#include <complib.h>

int
rk4_driver(out_state, in_state, dwork, integ_cntl, mode)
int			mode;
double			*out_state, *in_state, *dwork;
struct  Prop_DataS	*integ_cntl;
{
	int	var_dim, status = NO_ERROR;
	double	t_step;


	var_dim = integ_cntl->ph_space_dim-1;
	t_step = (abs(integ_cntl->direction ) == FORWARD) ?
		     integ_cntl->time_step:-integ_cntl->time_step;

	status = rk4(out_state, in_state, integ_cntl->parameters,
		     t_step, var_dim, integ_cntl->function, dwork);

	if ( status == NO_ERROR )
	    out_state[var_dim] = in_state[var_dim] + t_step;

        return(status);

}


int
rk4(vx1,vx,param,time_step,dim,f_p,workspace)
double *vx1,*vx,*param,time_step,*workspace;
int dim,(*f_p)();
{
	int i;
	double *v1,*v2,*sum;
	double time_step0,time_step2;
	
	v1 = workspace;
	v2 = workspace + dim + 1;
	sum = v2 + dim + 1;
	
	time_step0 = time_step * 2.0;
	time_step2 = time_step/2.0;
	  
	(int) f_p(v1,vx,param);
	for(i=0;i<dim;i++)
	    {sum[i] = time_step * v1[i];
	     v1[i] = vx[i] + time_step2 * v1[i]; }
	
	v1[dim] = vx[dim] + time_step2;
	(int) f_p(v2,v1,param);
	for(i=0;i<dim;i++) 
	    {sum[i] += time_step0 * v2[i];
	     v2[i] = vx[i] + time_step2 * v2[i]; }
	
	v2[dim] = v1[dim];
	(int) f_p(v1,v2,param);
	for(i=0;i<dim;i++)
	    {sum[i] += time_step0 * v1[i];
	     v1[i] = vx[i] + time_step * v1[i]; }
	
	v1[dim] =v2[dim] + time_step2;
	(int) f_p(v2,v1,param);
	for(i=0;i<dim;i++)
	   {sum[i] += time_step * v2[i];
	    vx1[i] = vx[i] + sum[i]/6.0; }
	
	vx1[dim] = v1[dim];

	return( NO_ERROR );
}


int
rk4_init()
{
   static int		ifields[]={15};
   static double	dfields[]={1.e-6,1.0e-5,1.0e-10};
   static int		sel_values[] = {0};
   static char		*ifield_names[]={"Newton Iter"};
   static char		*dfield_names[]={FINITE_DIFFERENCE_STEP_STRING, STOPPING_ERROR_STRING,"Min dt: "};
   static char		*usr_sel_labels[] = {" "};
   static int		num_choices[] = {0};
   /* static char		*choice1[]={" "};  paw */

   Int_Algol.Num_Ifields = 1;			/* these three MUST be set */
   Int_Algol.Num_Dfields = 3;
   Int_Algol.Num_Sel_Items = 0;

   Int_Algol.Ifield_Names = ifield_names;
   Int_Algol.Dfield_Names = dfield_names;
   Int_Algol.Sel_Labels = usr_sel_labels;
   Int_Algol.Ifields = ifields;
   Int_Algol.Dfields = dfields;
   Int_Algol.Num_Sel_Choices = num_choices;
   Int_Algol.Sel_Values = sel_values;

}
