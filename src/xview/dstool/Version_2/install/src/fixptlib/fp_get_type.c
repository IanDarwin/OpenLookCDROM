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
### get an index for the type of periodic orbits ###

Note: Order eigenvalues in decreasing order in magnitude
*/
#include <stdio.h>
#include <math.h>
#include <fixptlib.h>
#include <constants.h>
#include <defaults.h>

fp_get_type(fp)
struct Fixpt_DataS *fp;
{

  int i,stable_man_dim,unstable_man_dim,complex_ev;
  int var_dim = fp->prop_cntl.ph_space_dim - 1;

  if (var_dim==1) 
    {
      if (fp->prop_cntl.mapping_flag) 
	{
	  /* one dimensional map */
	  if (fabs(fp->eval[0][0])>1.0+EVAL_TOLERANCE) fp->fptype = SOURCE;
	  else if(fabs(fp->eval[0][0])<1.0-EVAL_TOLERANCE) fp->fptype = SINK;
	  else fp->fptype = INDETERMINATE;
	}
      else 
	{
	  /* one dimensional vector field */
	  if (fp->eval[0][0] > EVAL_TOLERANCE) fp->fptype = SOURCE;
	  else if (fp->eval[0][0] < -EVAL_TOLERANCE) fp->fptype = SINK;
	  else fp->fptype = INDETERMINATE;
	}
    }
  else if (var_dim == 2) 
    {
      if(fp->prop_cntl.mapping_flag)
	{
	  /* two dimensional map */
	  if(fp->eval[1][0] == 0.0 && fp->eval[1][1] == 0.0) 
	    {
	      /* purely real */
	      if (fabs(fp->eval[0][0])<1.0-EVAL_TOLERANCE)
		{
		  if (fabs(fp->eval[0][1])<1.0-EVAL_TOLERANCE) fp->fptype = SINK;
		  else if (fabs(fp->eval[0][1])>1.0+EVAL_TOLERANCE) fp->fptype = SADDLE;
		  else fp->fptype = INDETERMINATE;
		}
	      else if (fabs(fp->eval[0][0])>1.0+EVAL_TOLERANCE)
		{
		  if (fabs(fp->eval[0][1])<1.0-EVAL_TOLERANCE) fp->fptype = SADDLE;
		  else if (fabs(fp->eval[0][1])>1.0+EVAL_TOLERANCE) fp->fptype = SOURCE;
		  else fp->fptype = INDETERMINATE;
		}
	      else fp->fptype = INDETERMINATE;
	    }
	  else 
	    {
	      if((fp->eval[0][0]*fp->eval[0][0]+fp->eval[1][0]*fp->eval[1][0])>1.0+EVAL_TOLERANCE)
		fp->fptype = SPIRAL_SOURCE;
	      else if((fp->eval[0][0]*fp->eval[0][0]+fp->eval[1][0]*fp->eval[1][0])<1.0-EVAL_TOLERANCE)
		fp->fptype = SPIRAL_SINK;
	      else
		fp->fptype = INDETERMINATE;
	    }
	}
      else 
	{
	  /* two dimensional vector field */
	  if (fp->eval[1][0] == 0.0 && fp->eval[1][1] == 0.0)
	    {
	      /* purely real */
	      if (fp->eval[0][0] < -EVAL_TOLERANCE)
		{
		  if (fp->eval[0][1] < -EVAL_TOLERANCE) fp->fptype = SINK;
		  else if (fp->eval[0][1] > EVAL_TOLERANCE) fp->fptype = SADDLE;
		  else fp->fptype = INDETERMINATE;
		}
	      else if (fp->eval[0][0] > EVAL_TOLERANCE)
		{
		  if (fp->eval[0][1] < -EVAL_TOLERANCE) fp->fptype = SADDLE;
		  else if (fp->eval[0][1] > EVAL_TOLERANCE) fp->fptype = SOURCE;
		  else fp->fptype = INDETERMINATE;
		}
	      else fp->fptype = INDETERMINATE;
	    }
	  else 
	    {
	      if(fp->eval[0][0] > EVAL_TOLERANCE) fp->fptype = SPIRAL_SOURCE;
	      else if(fp->eval[0][0] < -EVAL_TOLERANCE) fp->fptype = SPIRAL_SINK;
	      else fp->fptype = INDETERMINATE;
	    }
	}
    }
  else 
    {
      /* compute the dimensions of stable and unstable manifolds */
      unstable_man_dim = 0;
      stable_man_dim = 0;
      complex_ev = 0;
      if (fp->prop_cntl.mapping_flag)
	{
	  for(i=0;i<var_dim;i++)
	    {
	      /* Note: May need to relax this test for complex eigenvalue */
	      if (fp->eval[1][i]==0.0)
		{
		  if (fabs(fp->eval[0][i])>1+EVAL_TOLERANCE) unstable_man_dim++;
		  else if (fabs(fp->eval[0][i]<1-EVAL_TOLERANCE)) stable_man_dim++;
		}
	      else 
		{
		  complex_ev = 1;
		  if ((fp->eval[0][i]*fp->eval[0][i] + fp->eval[1][i]*fp->eval[1][i]) >1+EVAL_TOLERANCE) 
		    unstable_man_dim++;
		  else if ((fp->eval[0][i]*fp->eval[0][i] + fp->eval[1][i]*fp->eval[1][i]) <1-EVAL_TOLERANCE) 
		    stable_man_dim++;
		}
	    }
	}
      else 
	{
	  for(i=0;i<var_dim;i++)
	    {
	      if (fp->eval[1][i]!=0.0) complex_ev = 1;
	      if (fp->eval[0][i] > EVAL_TOLERANCE) unstable_man_dim++;
	      else if (fp->eval[0][i] < -EVAL_TOLERANCE) stable_man_dim++;
	    }
	}
      if (complex_ev == 1)
	{
	  if (unstable_man_dim == var_dim) fp->fptype = SPIRAL_SOURCE;
	  else if (stable_man_dim == var_dim) fp->fptype = SPIRAL_SINK;
	  else if (unstable_man_dim>0 && stable_man_dim>0) fp->fptype = SADDLE;
	  else fp->fptype = INDETERMINATE;
	}
      else 
	{
	  if (unstable_man_dim == var_dim) fp->fptype = SOURCE;
	  else if (stable_man_dim == var_dim) fp->fptype = SINK;
	  else if (unstable_man_dim>0 && stable_man_dim>0) fp->fptype = SADDLE;
	  else fp->fptype = INDETERMINATE;
	}
    }
}







































