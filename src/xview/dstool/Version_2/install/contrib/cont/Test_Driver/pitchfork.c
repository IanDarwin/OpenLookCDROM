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
#include <constants.h>


/* ------------------------------------------------------------------------
   proc used to define the vector field or map
   ------------------------------------------------------------------------ */
int pitchfork(f,x,p)
double f[],x[],p[];
{
  f[0] = x[1];
  f[1] = p[1] + p[0]*x[0] - x[0]*x[0]*x[0];
  return(0);

}


/* ------------------------------------------------------------------------
   proc used to define jacobian
   ------------------------------------------------------------------------ */
int pitchfork_jac(m,x,p)
double	**m, *x, *p;
{
  m[0][0] = 0.0;
  m[0][1] = 1.0;
  m[1][0] = p[0] - 3.0 * x[0] * x[0];
  m[1][1] = 0.0;
}

