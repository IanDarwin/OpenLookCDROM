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
int
hqr(nm,n,low,igh,h,wr,wi)

int n,nm,*igh,*low;
double **h,*wr,*wi;
{
int i,j,k,l,m,en,ll,mm,na,itn,its,mp2,enm2,ierr;
double p,q,r,s,t,w,x,y,zz,norm,tst1,tst2;
/*double copysign(); */
char notlas;
/*
      this subroutine is a translation of the algol procedure hqr,
      num. math. 14, 219-231(1970) by martin, peters, and wilkinson.
      handbook for auto. comp., vol.ii-linear algebra, 359-371(1971).
 
      this subroutine finds the eigenvalues of a real
      upper hessenberg matrix by the qr method.
 
      on input
 
         nm must be set to the row dimension of two-dimensional
           array parameters as declared in the calling program
           dimension statement.
 
         n is the order of the matrix.
 
         low and igh are integers determined by the balancing
           subroutine  balanc.  if  balanc  has not been used,
           set low=1, igh=n.
 
         h contains the upper hessenberg matrix.  information about
           the transformations used in the reduction to hessenberg
           form by  elmhes  or  orthes, if performed, is stored
           in the remaining triangle under the hessenberg matrix.
 
      on output
 
         h has been destroyed.  therefore, it must be saved
           before calling  hqr  if subsequent calculation and
           back transformation of eigenvectors is to be performed.
 
         wr and wi contain the real and imaginary parts,
           respectively, of the eigenvalues.  the eigenvalues
           are unordered except that complex conjugate pairs
           of values appear consecutively with the eigenvalue
           having the positive imaginary part first.  if an
           error exit is made, the eigenvalues should be correct
           for indices ierr+1,...,n.
 
         ierr is set to
           zero       for normal return
           j          if the limit of 30*n iterations is exhausted
                      while the j-th eigenvalue is being sought.


      this routine is a C-translation of the FORTRAN 77 source code
      written by the mathematics and computer science division,
      argonne national laboratory
      last change :   september 1989.

      mark myers
      Center for Applied Mathematics 
      Cornell University    (607) 255-4195
      --------------------------------------------------------- */ 

        ierr = 0;
        norm = 0.e0;
        k = 1;
        for (i=1;i<=n;i++)                   /* store roots isolated by balanc */
          {for (j=k;j<=n;j++)                /* and compute matrix norm.       */
             norm = norm + fabs(h[i][j]);
           k = i;
           if (i < *low | i > *igh)
             {wr[i] = h[i][i];
              wi[i] = 0.e0;}  }
        en = *igh;
        t = 0.e0;
        itn = 30*n;
cycle1:  ;
        if (en < *low)
          return(ierr);
        its = 0;
        na = en - 1;
        enm2 = na - 1;
cycle2: ;
        for (ll = *low;ll <= en;ll++)    /* look for single small sub-diagonal element  */
          {l = en + *low - ll;      /* for l=en step -1 until low do               */
           if (l == *low) 
             break;
           s = fabs(h[l-1][l-1]) + fabs(h[l][l]);
           if (s == 0.e0)
	     s = norm;
           tst1 = s;
           tst2 = tst1 + fabs(h[l][l-1]);
           if (tst2 == tst1) 
	    break;}

        x = h[en][en];                 /* form shift */
        if (l == en)
          {wr[en] = x + t;
           wi[en] = 0.e0;
           en = na;
           goto cycle1;}
        y = h[na][na];
        w = h[en][na] * h[na][en];
        if (l == na) 
          {p = (y - x) / 2.e0;
           q = p * p + w;
           zz = sqrt(fabs(q));
           x = x + t;
	   if(q<0.e0)
             {wr[na] = x + p;           /* complex pair */
              wr[en] = x + p;
              wi[na] = zz;
              wi[en] = -zz;
              en = enm2;
              goto cycle1;}
           zz = p + copysign(zz,p);        /* real pair */
           wr[na] = x + zz;
           wr[en] = wr[na];
           if (zz != 0.e0)
	     wr[en] = x - w / zz;
           wi[na] = 0.e0;
           wi[en] = 0.e0;
	   en = enm2;
	   goto cycle1;}
        if (itn == 0) 
          return(en);
        if (its == 10 | its == 20)
         {t = t + x;                  /* form exceptional shift */
          for (i = *low;i<=en;i++)
            h[i][i] = h[i][i] - x;
          s = fabs(h[en][na]) + fabs(h[na][enm2]);
          x = 0.75e0 * s;
          y = x;
          w = -0.4375e0 * s * s;}
        its = its + 1;
        itn = itn - 1;
        for (mm=l;mm<=enm2;mm++)       /* look for two consecutive small */ 
          {m = enm2 + l - mm;         /* sub-diagonal elements.         */
           zz = h[m][m];              /* for m=en-2 step -1 until l...  */
           r = x - zz;
           s = y - zz;
           p = (r * s - w) / h[m+1][m] + h[m][m+1];
           q = h[m+1][m+1] - zz - r - s;
           r = h[m+2][m+1];
           s = fabs(p) + fabs(q) + fabs(r);
           p = p / s;
           q = q / s;
           r = r / s;
           if (m == l)
	     break;
           tst1 = fabs(p)*(fabs(h[m-1][m-1]) + fabs(zz) + fabs(h[m+1][m+1]));
           tst2 = tst1 + fabs(h[m][m-1])*(fabs(q) + fabs(r));
           if (tst2 == tst1)
	     break;}
        mp2 = m + 2;
 
        for (i=mp2;i<=en;i++)
          {h[i][i-2] = 0.e0;
           if (i == mp2)
             continue;
           h[i][i-3] = 0.e0;}
        for (k=m;k<=na;k++)        /* double qr step involving rows l to en & */
          {if (k==na)             /* columns m to en                         */
             notlas = 'f';
           else
  	     notlas = 't';
           if (k != m) 
             {p = h[k][k-1];
              q = h[k+1][k-1];
              r = 0.e0;
              if (notlas == 't')
  	        r = h[k+2][k-1];
              x = fabs(p) + fabs(q) + fabs(r);
              if (x == 0.e0) 
  	        continue;    
              p = p / x;
              q = q / x;
              r = r / x; }
           s = copysign(sqrt(p*p+q*q+r*r),p);
           if (k != m) 
             h[k][k-1] = -s * x;
	   else 
             if (l != m)
	       h[k][k-1] = -h[k][k-1];
           p = p + s;
           x = p / s;
           y = q / s;
           zz = r / s;
           q = q / p;
           r = r / p;
           if (notlas != 't')
             {for (j=k;j<=n;j++)                      /* row modification */
               {p = h[k][j] + q * h[k+1][j];
                h[k][j] = h[k][j] - p * x;
                h[k+1][j] = h[k+1][j] - p * y;}
   
  	      j = en;
  	      if(en>k+3)
  	        j = k+3;
              for (i=1;i<=j;i++)                     /* column modification */
                {p = x * h[i][k] + y * h[i][k+1];
                 h[i][k] = h[i][k] - p;
                 h[i][k+1] = h[i][k+1] - p * q;}}
           else      
             {for (j=k;j<=n;j++)                     /* row modification */
               {p = h[k][j] + q * h[k+1][j] + r * h[k+2][j];
                h[k][j] = h[k][j] - p * x;
                h[k+1][j] = h[k+1][j] - p * y;
                h[k+2][j] = h[k+2][j] - p * zz;}
    
	      j = en;
	      if (k+3<en)
	        j = k+3;
              for (i=1;i<=j;i++)                    /* column modification */
                {p = x * h[i][k] + y * h[i][k+1] + zz * h[i][k+2];
                 h[i][k] = h[i][k] - p;
                 h[i][k+1] = h[i][k+1] - p * q;
                 h[i][k+2] = h[i][k+2] - p * r;}}
	}
      goto cycle2;
}
