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


 
/* ---------------------------------------------------------------------
    
      subroutine tangnt computes a unit tangent vector to the solution
      curve of the underdetermined nonlinear system fx = 0.  the
      tangent vector tan is the solution of the linear system
    
            dfa(x,ipl)*tan = e(nvar)
    
      here, e(i) always denotes the i-th basis natural basis vector
      in nvar-space; that is, the vector with a 1 in the i-th
      component and zeros elsewhere. then dfa(x,ipl) is the
      augmented jacobian whose first nvar-1 rows are dfx/dx (x), the
      derivative of fx eveluated at x, and whose last row is
      the transpose of e(ipl).
    
      the tangent vector is then normalized, but the adjustment
      of its sign is performed outside the routine.
    
      error returns  kerror=1  data or storage error
                     kerror=2  error in derivative call
                     kerror=3  solver failed
                     kerror=6  tangent vector zero
    
      subprograms called
    
         df, conmsg, sdist2, slv
    
  ---------------------------------------------------------------------- */


int
tangnt(detsn,fx,df,fpar,ip,ipar,iwork,nvar,rwork,slv,tan,xr)
int	(*fx)(), (*df)();
int 	(*slv)();
double	*detsn,*fpar,*rwork,*tan,*xr;
int	ip, *ipar, *iwork, nvar;
{
      int	i,ie,job,liw,lounit,lrw;
      double	sdist2(), tnorm;
 
      for(i=1; i<=nvar; i++)		/* set right hand side of tangent system */
         tan[i]=0.0;
      tan[nvar]=1.0;
 
      job=0;				/* call solver */
      liw = iwork[14];
      lrw = iwork[16];
      ie = slv(detsn,fx,df,fpar,ip,ipar,iwork,liw,job,nvar,rwork,lrw,xr,tan);
      if (ie == 1 || ie == 2) 
          return(ie);
      if (ie >= 3) 
          return(3);
 
      tnorm=sdist2(nvar,tan,tan,1);	/* obtain euclidean norm of tangent vector */
      if (tnorm == 0.0) 
	  return(6);
      for(i=1; i<=nvar; i++) 		/* normalize the vector */
         tan[i] = tan[i]/tnorm;
      return(0);
}
