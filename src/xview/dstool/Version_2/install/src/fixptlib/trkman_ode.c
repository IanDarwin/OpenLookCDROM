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
----------------------------------------------------------------------
     Compute a set of manifolds for saddle equilibria of ODE
----------------------------------------------------------------------
     compute manifolds of ANY given set of hyperbolic fixed points
     with eigenvalues and eigenvectors
*/
#include <stdio.h>
#include <fixptlib.h>
#include <constants.h>
#include <defaults.h>

/*
 * trkman_ode(fp)
 *            fp fixed point data struct
 *
 * procedure to compute store and display a manifold using the
 * data stored in the fixed point data struct.  It requires
 * that the fixed point is in the state and that eigenvalues and
 * eigenvectors have been computed.
 * For vector fields.
 *
 * last modified: 7/13/91 paw
 */
int
trkman_ode(fp)
struct Fixpt_DataS *fp;
{
  int i,ic,jc,var_dim,status=0, color[3];
  double time_step,delman;
  int format = *((int *) pm(GET, "Defaults.Precision", NULL));

  var_dim = fp->prop_cntl.ph_space_dim - 1;
  time_step = fp->prop_cntl.time_step;

  for (ic=0; ic<var_dim && status >= 0; ic++)
    {
      if ((fp->eval[1][ic] == 0) && (fp->eval[0][ic] != 0)) /* do only nonzero real eigenvalues */
	{
	  if (fp->eval[0][ic] > 0) /* unstable manifold */
	    {
	      fp->prop_cntl.sys_color = UNSTAB_MAN_COLOR;
	      fp->prop_cntl.symbol = UNSTAB_MAN_SYMBOL;
	      fp->prop_cntl.time_step = time_step / fp->unstab_pts;
	      fp->prop_cntl.direction = FORWARD;
	      fp->prop_cntl.iter_request = fp->unstab_steps*fp->unstab_pts;
	      fp->fptype = UNSTAB_MAN;
	    }
	  else  /* stable manifold */
	    {
	      fp->prop_cntl.sys_color = STAB_MAN_COLOR;
	      fp->prop_cntl.symbol = STAB_MAN_SYMBOL;
	      fp->prop_cntl.time_step = time_step / fp->stab_pts;
	      fp->prop_cntl.direction = BACKWARD;
	      fp->prop_cntl.iter_request = fp->stab_steps*fp->stab_pts;
	      fp->fptype = STAB_MAN;
	    }
	  if (fp->prop_cntl.iter_request > 0)
	    {
	      fprintf(stdout, "Computing manifold:  Ev = %.*lg\n",format,fp->eval[0][ic]);
	      fprintf(stdout, "  Eigenvector: ");
	      for (i=0; i<var_dim; i++) fprintf(stdout, "%.*lg ",format,fp->evectors[i][ic]);
	      fprintf(stdout, "\n");

	      delman = fp->eigen_dist;
	      
	      for (jc=0; jc<2  && status >= 0 ; jc++)
		{
		  fp->prop_cntl.state[var_dim] = fp->x1[var_dim];
		  for (i=0; i<var_dim; i++) 
		    fp->prop_cntl.state[i] = fp->x1[i] + delman*fp->evectors[i][ic];
		  project(var_dim, fp->prop_cntl.state, fp->prop_cntl.manifold);
		  status = prop_cntl_orbit(&fp->prop_cntl);
		  if (++(fp->prop_cntl.iterations) > 0)
		    {
		      memory_start_new_flow(fp->memory, 1, 0, 0, fp->prop_cntl.iterations, 0, 1);
		      color[0] = fp->prop_cntl.table_color;
		      color[1] = fp->prop_cntl.sys_color;
		      color[2] = fp->prop_cntl.symbol;
		      memory_add_point(fp->memory, NULL, fp->prop_cntl.parameters, color,
				       NULL, NULL, NULL, &(fp->fptype));
		      memory_add_points(fp->memory, fp->prop_cntl.iterations,
					fp->prop_cntl.traj_segment, NULL, NULL, NULL, NULL);
		    }
		  delman = -delman;  /* so we do both directions ! */
		}
	    }
	}
    }

  fp->prop_cntl.time_step = time_step;

  return(status);
}








