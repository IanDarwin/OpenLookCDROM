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
/* ROUTINE TO COMPUTE JACOBIAN MATRIX
   FOR FIXED POINT ROUTINES

   This will be done by finite difference methods
   or by formula.

   */
#include <stdio.h>
#include <constants.h>
#include <fixptlib.h>


/* procedure to compute a jacobian matrix of F(x)
	where F(x) = f^r(x) - x  for a mapping
	      F(x) = f(x)        for a vector field	

	r is the period of the fixed point

	the point is taken to be x1
	the finite difference step is x2
	the jacobian is put in jacobian
	the function evaluation F(x1) is put in fx

	set fd_flag to be non-zero to force finite difference jacobian evaluation
*/

fp_jac(fp, fd_flag)
struct Fixpt_DataS *fp;
int fd_flag;
{
  int i, status = 0, var_dim  = fp->prop_cntl.ph_space_dim - 1;
  double lift();

  if (fp->prop_cntl.mapping_flag) 
    {	  
      /* it's a mapping */
      if (fp->prop_cntl.dfdx && fd_flag == 0) 
	{
	  /* jacobian is given */
	  status = dfdx(fp->jacobian, var_dim, NULL, fp->x1, fp->prop_cntl.parameters, 
			fp->fp_map_period, fp->prop_cntl.manifold, 
			fp->prop_cntl.mapping_flag, fp->prop_cntl.function, fp->prop_cntl.dfdx,
			ANALYTIC, fp->prop_cntl.workspace);
	}
      else 
	{
	  /* compute finite difference jacobian */
	  status = dfdx(fp->jacobian, var_dim, fp->x2, fp->x1, fp->prop_cntl.parameters, 
			fp->fp_map_period, fp->prop_cntl.manifold, 
			fp->prop_cntl.mapping_flag, fp->prop_cntl.function, fp->prop_cntl.dfdx,
			CEN_DIFF, fp->prop_cntl.workspace);
	}
      iter_forw(fp->prop_cntl.function, fp->fp_map_period, fp->fx, fp->x1, fp->prop_cntl.parameters, 
		fp->prop_cntl.ph_space_dim, 1.0, fp->prop_cntl.workspace, fp->prop_cntl.manifold);
      lift(var_dim, fp->x1, fp->fx, fp->fx, fp->prop_cntl.manifold);
      for (i=0; i<var_dim; i++) 
	{
	  fp->jacobian[i][i] -= 1.0;
	  fp->fx[i] -= fp->x1[i];
	}
    }
  else 
    {
      /* vector field */
      if (fp->prop_cntl.dfdx && fd_flag == 0) 
	{	  
	  /* jacobian is given */
	  status = dfdx(fp->jacobian, var_dim, NULL, fp->x1, fp->prop_cntl.parameters, 
			0, fp->prop_cntl.manifold, 
			fp->prop_cntl.mapping_flag, fp->prop_cntl.function, fp->prop_cntl.dfdx,
			ANALYTIC, fp->prop_cntl.workspace);
	}
      else 
	{
	  /* compute finite difference jacobian */
	  status = dfdx(fp->jacobian, var_dim, fp->x2, fp->x1, fp->prop_cntl.parameters, 
			0, fp->prop_cntl.manifold, 
			fp->prop_cntl.mapping_flag, fp->prop_cntl.function, fp->prop_cntl.dfdx,
			CEN_DIFF, fp->prop_cntl.workspace);
	}
      fp->prop_cntl.function(fp->fx, fp->x1, fp->prop_cntl.parameters);
    }
  return(status);
}


