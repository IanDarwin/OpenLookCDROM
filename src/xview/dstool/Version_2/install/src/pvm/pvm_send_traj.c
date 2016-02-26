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
 * pvm_send_traj.c
 *
 * Procedures:
 *   pvm_send_traj()
 *
 */
#include <complib.h>
#include "pvmuser.h"
#include "pvm_messages.h"
#include <defaults.h>

void
  pvm_send_traj(prop_cntl, start_ptr, stop_ptr, func_values)
struct  Prop_DataS     *prop_cntl;
int	start_ptr, stop_ptr;
double	*func_values;
{
	int i, tot, op, inst;
	int n_varb = prop_cntl->ph_space_dim;
	char host[MAX_CMD_LINE_ARG_LEN];

	rcvinfo(&tot, &op, host, &inst);
	initsend();
	putndfloat(prop_cntl->parameters, prop_cntl->parameter_dim);
	tot = stop_ptr-start_ptr+1;
	putnint(&tot,1);
	for (i=start_ptr; i<=stop_ptr; i++)
	  putndfloat(prop_cntl->traj_segment[i], n_varb);
	snd(host, inst, pvm_TRAJ);
}
