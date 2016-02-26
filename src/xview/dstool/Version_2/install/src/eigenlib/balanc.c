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

balanc(nm,n,a,low,igh,scale)
 
int n,nm,*igh,*low;
double **a,*scale;

{
int i,j,k,l,m,jj;
double c,f,g,r,s,b2,radix;
char noconv,skip,cntl; 

/*    this subroutine is a translation of the algol procedure balance,
      num. math. 13, 293-304(1969) by parlett and reinsch.
      handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
 
      this subroutine balances a real matrix and isolates
      eigenvalues whenever possible.
 
      on input
 
         nm must be set to the row dimension of two-dimensional
           array parameters as declared in the calling program
           dimension statement.
 
         n is the order of the matrix.
 
         a contains the input matrix to be balanced.
 
      on output
 
         a contains the balanced matrix.
 
         low and igh are two integers such that a(i,j)
           is equal to zero if
            (1) i is greater than j and
            (2) j=1,...,low-1 or i=igh+1,...,n.
 
         scale contains information determining the
            permutations and scaling factors used.
 
      suppose that the principal submatrix in rows low through igh
      has been balanced, that p(j) denotes the index interchanged
      with j during the permutation step, and that the elements
      of the diagonal matrix used are denoted by d(i,j).  then
         scale(j) = p(j),    for j = 1,...,low-1
                  = d(j,j),      j = low,...,igh
                  = p(j)         j = igh+1,...,n.
      the order in which the interchanges are made is n to igh+1,
      then 1 to low-1.
 
      note that 1 is returned for igh if igh is zero formally.
 
      the algol procedure exc contained in balance appears in
      balanc  in line.  (note that the algol roles of identifiers
      k,l have been reversed.)
 
 
      this routine is a C-translation of the FORTRAN 77 source code
      written by the mathematics and computer science division,
      argonne national laboratory
      last change :   september 1989.

      mark myers
      Center for Applied Mathematics 
      Cornell University    (607) 255-4195
      ------------------------------------------------------------------    */

 
      radix = 16.e0;
      b2 = radix * radix;
      k = 1;
      l = n;
      cntl = 's';

exch: ;
      if (cntl == 's' | cntl == 'r')
	{if (cntl == 'r')
          {if (l == 1)          /* search for rows isolating an eigenvalue */
             {*low = k;          /* and push them down */
	      *igh = l;
	      return;}
           l = l - 1;}
         for (jj=1;jj<=l;jj++)     /* for j=1 step -1 until 1 do ... */
           {j = l + 1 - jj;
	    skip = 'f';
            for (i=1;i<=l;i++)
              {if (i == j) 
	        continue ;
               if (a[j][i] != 0.e0)
		 {skip = 't';
	          break;}}}
         if (skip == 'f')
           {m = l;
	    scale[m] = j;
            cntl = 'r';
            if (j != m)
              {for (i=1;i<=l;i++)  
                {f = a[i][j];
                 a[i][j]=a[i][m];
                 a[i][m] = f;}
               for (i=k;i<=n;i++)
                {f=a[j][i];
                 a[j][i]=a[m][i];
                 a[m][i]=f;} 
              }
            goto exch;
           }
        }
      else 
        k = k + 1;   /* search for columns isolating an eigenvalue */
                     /* and push them left */

      skip = 'f';
      for (j=k;j<=l;j++) 
        {for (i=k;i<=l;i++)
           {if (i == j) 
              continue;
            if (a[i][j] != 0.e0) 
	     {skip = 't';
	      break;}}
         if (skip == 'f')
           {m = k;
	    scale[m] = j;
            cntl = 'c';
            if (j != m)
              {for (i=1;i<=l;i++)  
                 {f = a[i][j];
                  a[i][j]=a[i][m];
                  a[i][m] = f;}
               for (i=k;i<=n;i++)
                 {f=a[j][i];
                  a[j][i]=a[m][i];
                  a[m][i]=f;} }
            goto exch;
           }
        }
      for (i=k;i<=l;i++)                 /* balance submatrix in rows k to l  */
        scale[i] = 1.e0;
      noconv = 't';
/*      while (noconv == 't') */           /* iterative loop for norm reduction */
      /* (k <= l) added below to defend against infinite loop */
      while ((noconv == 't') && (k <=l))
        {for (i=k;i<=l;i++)
           {c = 0.e0;
            r = 0.e0;
 
            for(j=k;j<=l;j++)
	      {if (j != i) 
                 {c = c + fabs(a[j][i]);
                  r = r + fabs(a[i][j]);}  }

            if ( c == 0.e0 | r == 0.e0 )        /* guard against zero c or */
	      {noconv = 'f';
	       continue;}
            g = r / radix;                     /* or due to underflow */ 
            f = 1.e0;
            s = c + r;
            while (c < g) 
              {f = f * radix;
               c = c * b2;}
            g = r * radix;
            while (c>=g) 
              {f = f / radix;
               c = c / b2; }
            if ((c + r) / f >= 0.95e0 * s)         /* now balance */
              {noconv = 'f';
	       continue;}
            g = 1.e0 / f;
            scale[i] = scale[i] * f;
          
            for (j=k;j<=n;j++)
               a[i][j] = a[i][j] * g;
          
            for (j=1;j<=l;j++)
               a[j][i]= a[j][i]* f;  
        } }
 *low = k;
 *igh = l;
}
