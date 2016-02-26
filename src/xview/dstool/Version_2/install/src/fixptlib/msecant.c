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
/* Computes fixed points using Secant method:

	For a vector field, this computes only points where
	the vector field is null.  It assumes that the period is 0!.
	ie, solves f(x) = 0

	Still need to implement periodic orbits.


	For a mapping, it will find fixed points with integral periods.
	ie solves F(x) = f^r(x) - x = 0


	returns: -1 on error
		  1 if it found a fixed point
                  2 if did not converge due to number of iteration limit
                  3 if it did not converge due to singular matrix
                  4 if it did not converge because stepsize became too small

*/


#include <stdio.h>
#include <math.h>

#include <defaults.h>
#include <fixptlib.h>


int
msecant(fp)
struct Fixpt_DataS *fp;
{
  int i, k, var_dim;
  double errf, err, tolx10, lift();
  
  int status = 0;

  var_dim = fp->prop_cntl.ph_space_dim - 1;
  tolx10 = fp->varb_conv / 10;

  for (i=0; i<var_dim; i++)
    {
      fp->x2[i] = -fp->fd_step;
      fp->x1[i] += fp->fd_step;
    }

  for (k=0; k<fp->nsteps; k++) 
    {
      status = fp_jac(fp,1);
      if (status) 
	{
	  system_mess_proc(0,"msecant: Jacobian computation failed.");
	  return(0);
	}

      /* Debugging code */
      /*  fprintf(stderr,"NTRIAL=%d\n",k);
      fprintf(stderr,"usrfun x: ");
      for(i=0;i<=var_dim;i++) fprintf(stderr,"%lf ",fp->x1[i]);    
      fprintf(stderr,"\n");
      fprintf(stderr,"usrfun f: ");
      for(i=0;i<=var_dim;i++) fprintf(stderr,"%lf ",fp->fx[i]); 
      fprintf(stderr,"\n");
      {  int j;
	 fprintf(stderr,"usrfun jacobian:");
        for(j=0;j<var_dim;j++) 
	  {
	    for(i=0;i<var_dim;i++) fprintf(stderr,"%lf ",(fp->jacobian)[j][i]);     
	    fprintf(stderr,"\n");
	  } 
       }
      */

      /* compute "error" */
      errf = 0.0;
      for(i=0; i<var_dim; i++) errf += fabs(fp->fx[i]);
	
      if (errf <= fp->funct_conv) 
	{
	  /* this is good enough ! */
	  status = 1;
	  k=fp->nsteps;
	}
      else 
	{
	  /* improve guess */
	  status = ludcmp(fp->jacobian,var_dim,fp->indx,&err);
	  if (status==-1) k=fp->nsteps;
	  else if (status==1) 
	    {
	      /* singular matrix, machine accuracy lost */
	      status=3;
	      k=fp->nsteps;
	    }
	  else 
	    {
	      lubksb(fp->jacobian,var_dim,fp->indx,fp->fx);
	      
	      err = 0.0;
	      for (i=0; i<var_dim; i++) 
		{
		  err += fabs(fp->fx[i]);
		  fp->x2[i] += fp->x1[i];
		  fp->x1[i] -= fp->fx[i];
		}
	      if (err <= fp->varb_conv) 
		{
		  k=fp->nsteps;
		  status = 4;
		}
	      else 
		{
		  for (i=0; i<var_dim; i++)
		    if (fabs(fp->fx[i]) < tolx10 )
		      {
			fp->x1[i] -= tolx10;
			fp->x2[i] += tolx10;
		      }
		  project(var_dim, fp->x1, fp->prop_cntl.manifold);
		  lift(var_dim, fp->x1, fp->x2, fp->x2, fp->prop_cntl.manifold);
		  for (i=0; i<var_dim; i++)
		    fp->x2[i] -= fp->x1[i];
		}
	    }
	}
    }
  if (status==0) status=2; /* hit max iters before converging */

  return (status);
}

