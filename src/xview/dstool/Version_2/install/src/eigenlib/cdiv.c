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
#include <math.h>

cdiv(ar,ai,br,bi,cr,ci)

double ar,ai,br,bi,*cr,*ci;
{
 
/*    complex division, (cr,ci) = (ar,ai)/(br,bi)   */
 
double s,ars,ais,brs,bis;

s = fabs(br) + fabs(bi);
ars = ar/s;
ais = ai/s;
brs = br/s;
bis = bi/s;
s = brs*brs + bis*bis;
*cr = (ars*brs + ais*bis)/s;
*ci = (ais*brs - ars*bis)/s;
}
