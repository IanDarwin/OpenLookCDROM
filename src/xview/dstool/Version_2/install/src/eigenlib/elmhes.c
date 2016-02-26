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

elmhes(nm,n,low,igh,a,intch)
 
int n,nm,*igh,*low;
double **a;
int *intch;
{
int i,j,m,la,kp1,mm1,mp1;
double x,y;
 
/*    this subroutine is a translation of the algol procedure elmhes,
      num. math. 12, 349-368(1968) by martin and wilkinson.
      handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
 
      given a real general matrix, this subroutine
      reduces a submatrix situated in rows and columns
      low through igh to upper hessenberg form by
      stabilized elementary similarity transformations.
 
      on input
 
         nm must be set to the row dimension of two-dimensional
           array parameters as declared in the calling program
           dimension statement.
 
         n is the order of the matrix.
 
         low and igh are integers determined by the balancing
           subroutine  balanc.  if  balanc  has not been used,
           set low=1, igh=n.
 
         a contains the input matrix.
 
      on output
 
         a contains the hessenberg matrix.  the multipliers
           which were used in the reduction are stored in the
           remaining triangle under the hessenberg matrix.
 
         int contains information on the rows and columns
           interchanged in the reduction.
           only elements low through igh are used.
 
      this routine is a C-translation of the FORTRAN 77 source code
      written by the mathematics and computer science division,
      argonne national laboratory
      last change :   september 1989.

      mark myers
      Center for Applied Mathematics 
      Cornell University    (607) 255-4195
 
      ----------------------------------------------------------    */
 
 la = *igh - 1;
 kp1 = *low + 1;
 if (la < kp1)
   return;
 for (m=kp1;m<=la;m++) 
    {mm1 = m - 1;
     x = 0.e0;
     i = m;
 
     for (j=m;j<=*igh;j++) 
       if (fabs(a[j][mm1]) > fabs(x)) 
          {x = a[j][mm1];
           i = j;}
 
     intch[m] = i;
     if (i !=  m) 
       {for (j=mm1;j<=n;j++)    /* interchange rows & col of a */
          {y = a[i][j];
           a[i][j] = a[m][j];
           a[m][j] = y; }
 
        for (j=1;j<=*igh;j++) 
          {y = a[j][i];
           a[j][i] = a[j][m];
           a[j][m] = y;}
       }

     if (x == 0.e0) 
       continue;
     mp1 = m + 1;

     for (i=mp1;i<=*igh;i++) 
       {y = a[i][mm1];
        if (y == 0.e0)
	  continue;
        y = y / x;
        a[i][mm1] = y;
 
        for (j=m;j<=n;j++)
          a[i][j] = a[i][j] - y * a[m][j]; 

        for (j=1;j<=*igh;j++)
          a[j][m] = a[j][m] + y * a[j][i];
     } }     
}
