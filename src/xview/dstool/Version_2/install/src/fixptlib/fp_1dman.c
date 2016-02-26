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
/* ------------------------------
   driver for computing one-d manifolds 
   of fixed points
*/
#include <stdio.h>
#include <memory.h>
#include <constants.h>
#include <fixptlib.h>


int
fp_1dman(fp)
struct Fixpt_DataS *fp;
{
  int status = 0;
  int num = 0;
  double *points, *params;
  int flow_type(),j,k,var_dim, *extra_iparams, n_extra_iparams;

  system_mess_proc(0,"Computing 1-d manifolds of fixed points.");
 
  var_dim = fp->prop_cntl.ph_space_dim-1;

  /* search memory object for any fixed points */
  if (memory_reset_read(fp->memory) == 0)
    {
      while (memory_read_next_flow(fp->memory, NULL, NULL, NULL, &extra_iparams, &n_extra_iparams)
	     == 0  && status>=0)
	{
	  while (memory_read_next_traj(fp->memory, &fp->fp_map_period, NULL, NULL) == 0 && 
		 status >= 0)
	    {
	      fp->fptype = extra_iparams[0];
	      if (fp->fptype == SADDLE)
		{
		  num++;
		  while (memory_read_next_point(fp->memory, &points, &params, 
						NULL, NULL, NULL) == 0 && status >= 0)
		    {
		      fp->x1[var_dim] = points[var_dim];
		      for (j=0; j<var_dim; j++) 
			{
			  fp->x1[j] = points[j];
			  fp->x2[j] = fp->fd_step;
			}
		      for (j=0; j<fp->prop_cntl.parameter_dim; j++)
			fp->prop_cntl.parameters[j] = params[j];
		      fp_jac(fp,0);
		      if (fp->prop_cntl.mapping_flag)
			for (j=0; j<var_dim; j++) fp->jacobian[j][j] += 1.0;
		      if (var_dim==1)
			{
			  fp->eval[0][0] = fp->jacobian[0][0];
			  fp->eval[0][1] = 0.0;
			  fp->evectors[0][0] = 1.0;
			}
		      else
			{
			  /* DEBUG - print out jacobian 
			  fprintf(stdout,"Jacobian: ");
			  for (j=0; j<var_dim; j++)
			    {
			      for (k=0; k<var_dim; k++)
				fprintf(stdout,"%lf ",fp->jacobian[j][k]);
			      fprintf(stdout,"\n          ");
			    }
			  fprintf(stdout,"\n");  */
			  
			  /* use converted Fortran routines to copute evals and evectors */
			  for (j=0; j<var_dim; j++)
			    for (k=0; k<var_dim; k++)
			      fp->jact[j+1][k+1] = fp->jacobian[j][k];
			  status = rg(var_dim,var_dim,fp->jact,fp->eval[0]-1,fp->eval[1]-1,1,fp->evectors,fp->indx,fp->x3);
			  for (j=0; j<var_dim; j++)
			    for (k=0; k<var_dim; k++)
			      fp->evectors[j][k] = fp->evectors[j+1][k+1];
			}
		      if (fp->prop_cntl.mapping_flag)
			status = trkman_map(fp);
		      else
			status = trkman_ode(fp);
		    }
		}
	    }
	}
    }

  if (num==0)
    {
      system_mess_proc(1,"fp_1dman: No 1-d manifolds to draw. (ie, no saddles in fixed point memory)");
    }

  return(status);
}

