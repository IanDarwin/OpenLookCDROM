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
#include <pm.h>


/* These procedure are intended to provide
   small but useful functions which are used
   repeatedly in the code
   */

  extern int get_n_all_types();

/* The variables, parameters, functions, special functions, ...
   are all given an index from 0 to max.
   The following routines do the coding and decoding as
   well as providing some common utilities.

   */

int
get_n_all_types(pn_varb, pn_param, pn_funct)
int *pn_varb, *pn_param, *pn_funct;
{
	*pn_varb = *((int *) pm( GET, "Model.Varb_Dim", NULL));
	*pn_param = *((int *) pm( GET, "Model.Param_Dim", NULL));
	*pn_funct = *((int *) pm( GET, "Model.Funct_Dim", NULL));

	return;
}


int
max_varb_index()
{
	int n_varb, n_param, n_funct;

	get_n_all_types(&n_varb, &n_param, &n_funct);
	return(n_varb + n_param + n_funct - 1);
}


int
index_to_type(index)
int index;
{
	int n_varb, n_param, n_funct;

	get_n_all_types(&n_varb, &n_param, &n_funct);

	if ((index -= n_varb) < 0) return(PHASE_SPACE_VARB);
	if ((index -= n_param) < 0) return(PARAMETER_VARB);
	if ((index -= n_funct) < 0) return(FUNCTION_VARB);
	return(0);

}

int
index_to_count(index)
int index;
{
	int n_varb, n_param, n_funct;

	get_n_all_types(&n_varb, &n_param, &n_funct);

	if (index < n_varb) return(index);
	if ((index -= n_varb) < n_param) return(index);
	if ((index -= n_param) < n_funct) return(index);
	/* error ! */
	return(-1);
}

int
type_count_to_index(type,count)
int type,count;
{
	int n_varb, n_param, n_funct;

	get_n_all_types(&n_varb, &n_param, &n_funct);

	if (type == PHASE_SPACE_VARB) return(count);
	if (type == PARAMETER_VARB) return(count+n_varb);
	if (type == FUNCTION_VARB) return(count+n_varb+n_param);
	/* error ! */
	return(-1);
}

