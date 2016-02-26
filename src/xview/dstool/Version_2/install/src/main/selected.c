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
/*
 * Create SELECTED postmaster object
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>

static char *SELECT_OBJ_NAME = "Selected";

static char *SELECT[] = {
  "Selected.Varb_Ic", "Selected.Varb_Fc", 
  "Selected.Param_Ic", "Selected.Param_Fc",
  "Selected.Copy"
  };

typedef enum {
  VARB_IC=0, VARB_FC, PARAM_IC, PARAM_FC, COPY
  } SELECT_t;


void
selected_install()
{
  void copy_final_to_initial();

  pm(CREATE_OBJ, SELECT_OBJ_NAME,
     CREATE_ELEM, SELECT[VARB_IC], DBL_LIST,
     CREATE_ELEM, SELECT[VARB_FC], DBL_LIST,
     CREATE_ELEM, SELECT[PARAM_IC], DBL_LIST,
     CREATE_ELEM, SELECT[PARAM_FC], DBL_LIST,
     CREATE_ELEM, SELECT[COPY], FNCT,
     NULL);

  pm(INIT, SELECT[COPY],
     PUT, SELECT[COPY], copy_final_to_initial,
     NULL);
}

void
  selected_reset()
{
  int dim, i;
  double d;

  dim = *((int *) pm(GET, "Model.Varb_Dim", NULL));;
  pm(INIT, SELECT[VARB_IC], dim,
     INIT, SELECT[VARB_FC], dim,
     NULL);
  for (i=0; i<dim; i++)
    {
      d = *((double *) pm(GET, "Model.Varb_Ic", i, NULL));
      pm(PUT, SELECT[VARB_IC], i, d,
	 PUT, SELECT[VARB_FC], i, d,
	 NULL);
    }
  dim = *((int *) pm(GET, "Model.Param_Dim", NULL));;
  pm(INIT, SELECT[PARAM_IC], dim,
     INIT, SELECT[PARAM_FC], dim,
     NULL);
  for (i=0; i<dim; i++)
    {
      d = *((double *) pm(GET, "Model.Param_Ic", i, NULL));
      pm(PUT, SELECT[PARAM_IC], i, d,
	 PUT, SELECT[PARAM_FC], i, d,
	 NULL);
    }
  
}


void
copy_final_to_initial()
{
  int 		i, n_varb, n_param;
  double 	value;
	
  n_varb = *((int *) pm(GET, "Model.Varb_Dim", NULL));
  n_param = *((int *) pm(GET, "Model.Param_Dim", NULL));
  for (i=0; i<n_varb; i++)
    {
      value = *((double *) pm(GET, SELECT[VARB_FC], i, NULL));
      pm(PUT, SELECT[VARB_IC], i, value, NULL); 
    }
  for (i=0; i<n_param; i++)
    {
      value = *((double *) pm(GET, SELECT[PARAM_FC], i, NULL));
      pm(PUT, SELECT[PARAM_IC], i, value, NULL); 
    }
}
