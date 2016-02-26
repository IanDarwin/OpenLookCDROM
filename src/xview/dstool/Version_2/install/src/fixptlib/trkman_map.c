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

#include <fixptlib.h>
#include <iterate.h>
#include <constants.h>
#include <defaults.h>

/*
 * trkman_map(fp)
 *            fp fixed point data struct
 *
 * procedure to compute store and display a manifold using the
 * data stored in the fixed point data struct.  It requires
 * that the fixed point is in the state and that eigenvalues and
 * eigenvectors have been computed.
 * For mappings.
 *
 * last modified: 7/16/91 paw
 */
int
  trkman_map(fp)
struct Fixpt_DataS *fp;
{
  int i,ic,jc,mc,var_dim,divs,std_pts,status=0;
  int color[3];
  double delman,factor0,factor;
  int format = *((int *) pm(GET, "Defaults.Precision", NULL));
  
  var_dim = fp->prop_cntl.ph_space_dim-1;
  
  for (ic=0; ic<var_dim && status >= 0; ic++)
    {
      if ((fp->eval[1][ic] == 0.) &&(fp->eval[0][ic] != 1.)) /* do only real hyperbolic directions.  */
	{
	  if (fabs(fp->eval[0][ic]) > 1.) /* unstable manifold */
	    {
	      fp->prop_cntl.sys_color = UNSTAB_MAN_COLOR;
	      fp->prop_cntl.symbol = UNSTAB_MAN_SYMBOL;
	      fp->prop_cntl.direction = FORWARD;
	      fp->prop_cntl.time_step = STEP_FORW;
	      fp->prop_cntl.iter_request = fp->unstab_steps;
	      divs = fp->unstab_pts;
	      fp->fptype = UNSTAB_MAN;
	    }
	  else			/* stable manifold */
	    {
	      fp->prop_cntl.sys_color = STAB_MAN_COLOR;
	      fp->prop_cntl.symbol = STAB_MAN_SYMBOL;
	      fp->prop_cntl.direction = BACKWARD;
	      fp->prop_cntl.time_step = STEP_BACK;
	      fp->prop_cntl.iter_request = fp->stab_steps;
	      divs = fp->stab_pts;
	      fp->fptype = STAB_MAN;
	    }
	  
	  if (fp->eval[0][ic] < 0)
	    {
	      factor0 = exp(log(-fp->eval[0][ic])*2.0/divs);
	    }
	  else
	    {
	      factor0 = exp(log(fp->eval[0][ic])/divs);
	    }
	  
	  
	  if (fp->prop_cntl.iter_request > 0)
	    {
	      fprintf(stdout,"Computing manifold:  Ev = %.*lg\n",format,fp->eval[0][ic]);
	      fprintf(stdout, "  Eigenvector: ");
	      for (i=0; i<var_dim; i++) fprintf(stdout, "%.*lg ",format,fp->evectors[i][ic]);
	      fprintf(stdout, "\n");
	      
	      delman = fp->eigen_dist;
	      
	      for (jc=0; jc<2 && status >= 0; jc++)
		{
		  std_pts = 0;
		  factor = 1.0;
		  for (mc=0; mc<divs; mc++)
		    {
		      fp->prop_cntl.state[var_dim] = fp->x1[var_dim];
		      if (fp->fptype == UNSTAB_MAN) /* fjw */
			{
			  for (i=0; i<var_dim; i++) 
			    fp->prop_cntl.state[i] = fp->x1[i] + delman*fp->evectors[i][ic]*factor;
			}
		      else
			{
			  for (i=0; i<var_dim; i++) 
			    fp->prop_cntl.state[i] = fp->x1[i] + delman*fp->evectors[i][ic]/factor;
			}
		      project(var_dim, fp->prop_cntl.state, fp->prop_cntl.manifold);
		      factor *=factor0;
		      status = prop_cntl_orbit(&fp->prop_cntl);
		      if (++fp->prop_cntl.iterations > 0)
			{
			  if (std_pts == 0)
			    {
			      memory_start_new_flow(fp->memory, 1, 0, 0, 
						    fp->prop_cntl.iterations, 0, 1);
			      color[0] = fp->prop_cntl.table_color;
			      color[1] = fp->prop_cntl.sys_color;
			      color[2] = fp->prop_cntl.symbol;
			      memory_add_point(fp->memory, NULL, fp->prop_cntl.parameters, 
					       color, NULL, NULL, NULL, &(fp->fptype));
			      std_pts = 1;
			    }
			  memory_add_points(fp->memory, fp->prop_cntl.iterations,
					    fp->prop_cntl.traj_segment, NULL, NULL, 
					    NULL, NULL);
			}
		    }
		  delman = -delman;
		}	      
	    }
	}
    }
  return(status);
}

