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
/* Order eigenvalues in decreasing order in magnitude */
#include <stdio.h>
#include <math.h>

#include <defaults.h>
#include <fixptlib.h>

fp_eval(fp)
     struct Fixpt_DataS	*fp;
{
  int	i, j, var_dim;
  int 	status = 0;

  var_dim = fp->prop_cntl.ph_space_dim - 1;

  for (i=0; i<var_dim; i++) fp->x2[i] = fp->fd_step;

  /* compute jacobian */
  fp_jac(fp,0);	
  /* for a map, the jacobian computed was for  f^r(x)-x */
  if (fp->prop_cntl.mapping_flag)
    for (i=0; i<var_dim; i++) fp->jacobian[i][i] += 1.0;

  /* Debugging code: print Jacobian 
     fprintf(stderr,"Jacobian: \n");
     for (i=0; i<var_dim; i++) {
     for (j=0; j<var_dim; j++) fprintf(stderr, "%lf ",fp->jacobian[i][j]);
     fprintf(stderr,"\n");
     }
     */
			
	if (var_dim==1) {
		fp->eval[0][0] = fp->jacobian[0][0];
		}
	else {
		for (i=0; i<var_dim; i++)
			for (j=0; j<var_dim; j++) fp->jact[i+1][j+1] = fp->jacobian[i][j];
		status = rg(var_dim, var_dim,
			    fp->jact, fp->eval[0]-1, fp->eval[1]-1, 0, NULL, fp->indx, fp->x3); 
		}

	return (status);
}

