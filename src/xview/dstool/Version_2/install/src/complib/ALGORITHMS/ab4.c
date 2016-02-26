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
#include <complib.h>
#include <prop.h>



#define MAX_ITEMS 4

int
ab4_driver(out_state, in_state, dwork, integ_cntl, mode)
int			mode;
double			*out_state, *in_state, *dwork;
struct  Prop_DataS	*integ_cntl;
{
	int	var_dim, status = NO_ERROR;
	double	t_step;

   
	var_dim = integ_cntl->ph_space_dim-1;

	/* manage history list if necessary */
	if ((mode == VARB_STEP_INIT) || (mode == FIXED_STEP_INIT))
	    if (integ_cntl->history_list == (History_Struct *) NULL) {
		integ_cntl->history_list  = create_history_struct(MAX_ITEMS, var_dim ); 
	    }
	    else if ((integ_cntl->history_list->data_dim != var_dim) ||
		     (integ_cntl->history_list->max_items != MAX_ITEMS)) {
		free_history_struct(integ_cntl->history_list);
		create_history_struct(MAX_ITEMS, var_dim);
	    }
	else
	    empty_history_struct(integ_cntl->history_list);


	t_step = (abs(integ_cntl->direction ) == FORWARD) ? integ_cntl->time_step:-integ_cntl->time_step;

	status = ab4(out_state, in_state, integ_cntl->parameters, t_step,
		     var_dim, integ_cntl->function, dwork,integ_cntl->history_list);

        if(status == NO_ERROR)
	    out_state[var_dim] = in_state[var_dim] + t_step;

        return (status);

}

/* ------------------------------------------------------
   forward linear 4-step Adams-Bashforth integrator 

   ------------------------------------------------------ */
int
ab4(vx1,vx,param,time_step,dim,f_p,workspace,history_list)
double *vx1,*vx,*param,time_step,*workspace;
int dim,(*f_p)();

History_Struct
    *history_list;
{
    int i,s,
    pos[4];			/* locations of first 3 elements in
				 history queue */
  
    s = 4; /* Number of steps in the integration method */
  


    if (num_elements(history_list) < s - 1) {
	/* Routine starting up. Use Euler to create seed. */
	f_p(vx1,vx,param);
	enqueue(history_list,vx1);
	rk4(vx1,vx,param,time_step,dim,f_p,workspace);
/*
	for(i=0;i<dim;i++)
	    vx1[i] = vx[i] + time_step * vx1[i];
*/

    }
    else {
	f_p(vx1,vx,param);

	if (num_elements(history_list) == s)
	    dequeue(history_list);
	enqueue(history_list,vx1);

	/* record locations of previous vector field values 
	   earlier values come first
	   */
	pos[0] = history_list->first;
	for (i=1; i < s; i++)
	    pos[i] = succ(history_list,pos[i-1]);

	for(i=0;i<dim;i++) 
	    vx1[i] = vx[i] + time_step * (55 * vx1[i] - 
					  59 * history_list->data[pos[2]][i] + 
					  37 * history_list->data[pos[1]][i] - 
					  9 * history_list->data[pos[0]][i])/24;
    }
    /* update time */
    vx1[dim] = vx[dim] + time_step;
    return(NO_ERROR);

}


/* -------------------------------------------------------
   proc used to initialized the control fields on the
   integration panel 

   ------------------------------------------------------- */


int
ab4_init()
{
   static int		ifields[]={15};
   static double	dfields[]={1.e-6,1.0e-5,1.0e-10};
   static int		sel_values[] = {0};
   static char		*ifield_names[]={"Newton Iter"};
   static char		*dfield_names[]={FINITE_DIFFERENCE_STEP_STRING, STOPPING_ERROR_STRING, "Min dt: "};
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
