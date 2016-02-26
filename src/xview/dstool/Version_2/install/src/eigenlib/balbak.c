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
balbak(nm,n,low,igh,scale,m,z)

int n,nm,m,*low,*igh;
double *scale,**z;
{
int i,j,k,ii;
double s;
 
/*    this subroutine is a translation of the algol procedure balbak,
      num. math. 13, 293-304(1969) by parlett and reinsch.
      handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
 
      this subroutine forms the eigenvectors of a real general
      matrix by back transforming those of the corresponding
      balanced matrix determined by  balanc.
 
      on input
 
         nm must be set to the row dimension of two-dimensional
           array parameters as declared in the calling program
           dimension statement.
 
         n is the order of the matrix.
 
         low and igh are integers determined by  balanc.
 
         scale contains information determining the permutations
           and scaling factors used by  balanc.
 
         m is the number of columns of z to be back transformed.
 
         z contains the real and imaginary parts of the eigen-
           vectors to be back transformed in its first m columns.
 
      on output
 
         z contains the real and imaginary parts of the
           transformed eigenvectors in its first m columns.
 
 
      this routine is a C-translation of the FORTRAN 77 source code
      written by the mathematics and computer science division,
      argonne national laboratory
      last change :   september 1989.

      mark myers
      Center for Applied Mathematics 
      Cornell University    (607) 255-4195
      ------------------------------------------------------------------  */
 
 if (m != 0)
  { if (*igh != *low) 
     {
      for (i = *low;i <= *igh;i++) 
        {s = scale[i];    /* left hand eigenvectors are back transformed */
         for (j=1;j<=m;j++)    /* if the foregoing statement is replaced by   */
            z[i][j] = z[i][j]*s;}    /* s=1.0d0/scale(i). */
     }
     for (ii=1;ii<=n;ii++)           /* for i=low-1 step -1 until 1,  */
        {i = ii;                    /* igh+1 step 1 until n do       */
         if (i >= *low & i <= *igh) 
	   continue;
         if (i < *low) 
	    i = *low - ii;
         k = scale[i];
         if (k == i) 
	   continue;
 
         for (j=1;j<=m;j++) 
           {s = z[i][j];
            z[i][j] = z[k][j];
            z[k][j] = s;}
 
        }              
  }
}
