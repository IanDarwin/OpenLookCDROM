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
#include <malloc.h>
#include <stdio.h>

/*
 * dvector()
 *
 * memory allocation for a vector of doubles
 * returns m so that m[nl],...,m[nh] are the valid locations
 *
 * last modified:  8/21/91  paw
 */
double 
  *dvector(nl,nh)
int nl,nh;
{
  double *v;

  if (nh < nl) return(NULL);
  if ( (v = (double *) malloc((unsigned) (nh - nl + 1) * sizeof(double))) == NULL)
    {
      system_mess_proc(1,"dvector: allocation failure!");
    }
  else 
    {
      v -= nl;
    }
  return(v);
}


/*
 * free_dvector()
 *
 * frees memory allocated by dvector()
 *
 * last modified: 8/21/91  paw
 */
int
  free_dvector(v,nl,nh)
double *v;
int nl,nh;
{
  if (v == NULL) return;
  free((char *) (v + nl));
}
