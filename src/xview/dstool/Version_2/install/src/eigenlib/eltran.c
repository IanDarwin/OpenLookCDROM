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
eltran(nm,n,low,igh,a,intch,z)

int nm,*igh,*low,*intch;
double **a,**z;

{
int i,j,mm,mp,mp1,kl;
 
/*    this subroutine is a translation of the algol procedure elmtrans,
      num. math. 16, 181-204(1970) by peters and wilkinson.
      handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
 
      this subroutine accumulates the stabilized elementary
      similarity transformations used in the reduction of a
      real general matrix to upper hessenberg form by  elmhes.
 
      on input
 
         nm must be set to the row dimension of two-dimensional
           array parameters as declared in the calling program
           dimension statement.
 
         n is the order of the matrix.
 
         low and igh are integers determined by the balancing
           subroutine  balanc.  if  balanc  has not been used,
           set low=1, igh=n.
 
         a contains the multipliers which were used in the
           reduction by  elmhes  in its lower triangle
           below the subdiagonal.
 
         intch contains information on the rows and columns
           interchanged in the reduction by  elmhes.
           only elements low through igh are used.
 
      on output
 
         z contains the transformation matrix produced in the
           reduction by  elmhes.


      this routine is a C-translation of the FORTRAN 77 source code
      written by the mathematics and computer science division,
      argonne national laboratory
      last change :   september 1989.

      mark myers
      Center for Applied Mathematics 
      Cornell University    (607) 255-4195
 
      ------------------------------------------------------------------  */
 
 for (j=1;j<=n;j++)              /* initialize z to the identity matrix */
   for (i=1;i<=n;i++)
     if (i==j)
       z[j][j] = 1.e0;
     else
       z[i][j] = 0.e0;
 
 kl = *igh - *low - 1;
 if (kl >= 1) 
 for (mm=1;mm<=kl;mm++)         /* for mp=igh-1 step -1 until low+1 do */           
   {mp = *igh - mm;
    mp1 = mp + 1;

    for (i=mp1;i<=*igh;i++) 
      z[i][mp] = a[i][mp-1];
 
    i = intch[mp];
    if (i != mp) 
      {for (j=mp;j<=*igh;j++) 
        {z[mp][j] = z[i][j];
         z[i][j] = 0.e0;}
   
       z[i][mp] = 1.0e0; }
   }
} 
