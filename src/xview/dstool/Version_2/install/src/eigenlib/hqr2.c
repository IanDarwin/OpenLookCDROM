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
hqr2(nm,n,low,igh,h,wr,wi,z)

int n,nm,*igh,*low;
double **h,*wr,*wi,**z;
{
int i,j,k,l,m,en,ii,jj,ll,mm,na,nn,itn,its,mp2,enm2,ierr;
double p,q,r,s,t,w,x,y,ra,sa,vi,vr,zz,norm,tst1,tst2;
/* double copysign(); */
int cdiv();
char notlas;
 
/*    this subroutine is a translation of the algol procedure hqr2,
      num. math. 16, 181-204(1970) by peters and wilkinson.
      handbook for auto. comp., vol.ii-linear algebra, 372-395(1971).
 
      this subroutine finds the eigenvalues and eigenvectors
      of a real upper hessenberg matrix by the qr method.  the
      eigenvectors of a real general matrix can also be found
      if  elmhes  and  eltran  or  orthes  and  ortran  have
      been used to reduce this general matrix to hessenberg form
      and to accumulate the similarity transformations.
 
      on input
 
         nm must be set to the row dimension of two-dimensional
           array parameters as declared in the calling program
           dimension statement.
 
         n is the order of the matrix.
 
         low and igh are integers determined by the balancing
           subroutine  balanc.  if  balanc  has not been used,
           set low=1, igh=n.
 
         h contains the upper hessenberg matrix.
 
         z contains the transformation matrix produced by  eltran
           after the reduction by  elmhes, or by  ortran  after the
           reduction by  orthes, if performed.  if the eigenvectors
           of the hessenberg matrix are desired, z must contain the
           identity matrix.
 
      on output
 
         h has been destroyed.
 
         wr and wi contain the real and imaginary parts,
           respectively, of the eigenvalues.  the eigenvalues
           are unordered except that complex conjugate pairs
           of values appear consecutively with the eigenvalue
           having the positive imaginary part first.  if an
           error exit is made, the eigenvalues should be correct
           for indices ierr+1,...,n.
 
         z contains the real and imaginary parts of the eigenvectors.
           if the i-th eigenvalue is real, the i-th column of z
           contains its eigenvector.  if the i-th eigenvalue is complex
           with positive imaginary part, the i-th and (i+1)-th
           columns of z contain the real and imaginary parts of its
           eigenvector.  the eigenvectors are unnormalized.  if an
           error exit is made, none of the eigenvectors has been found.
 
         ierr is set to
           zero       for normal return,
           j          if the limit of 30*n iterations is exhausted
                      while the j-th eigenvalue is being sought.
 
      calls cdiv for complex division.
 
 
      this routine is a C-translation of the FORTRAN 77 source code
      written by the mathematics and computer science division,
      argonne national laboratory
      last change :   september 1989.

      mark myers
      Center for Applied Mathematics 
      Cornell University    (607) 255-4195
      ------------------------------------------------------------------ */
 
      ierr = 0;
      norm = 0.e0;
      k = 1;
      for (i=1;i<=n;i++)                  /* store roots isolated bay balanc */
        {for (j=k;j<=n;j++)               /* and compute matrix norm         */
           norm = norm + fabs(h[i][j]);
 
         k = i;
         if (i < *low | i>*igh)
           {wr[i] = h[i][i];
            wi[i] = 0.e0;} }
 
      en = *igh;	
      t = 0.e0;
      itn = 30*n;
      while (en>= *low)           /* search for next eigenvalues */
        {its = 0;
         na = en - 1;
         enm2 = na - 1;
inner:   for (ll= *low;ll<=en;ll++) /* look for a single sub-diag element */
           {l = en + *low - ll;   /* for l=en step -1 until low do... */
            if (l == *low) 
	      break;
            s = fabs(h[l-1][l-1]) + fabs(h[l][l]);
            if (s == 0.e0)
	      s = norm;
            tst1 = s;
            tst2 = tst1 + fabs(h[l][l-1]);
            if (tst2 == tst1)
	      break;}

         x = h[en][en];                      /* form shift */
         if (l == en) 
	   {h[en][en] = x + t;       /* one root found */
	    wr[en] = h[en][en];
	    wi[en] = 0.e0;
	    en = na;
	    goto outer;}
         y = h[na][na];
         w = h[en][na] * h[na][en];
         if (l == na) 
	   {p = (y - x) / 2.e0;      /* two roots found ... */
            q = p * p + w;
            zz = sqrt(fabs(q));
            h[en][en] = x + t;
            x = h[en][en];
            h[na][na] = y + t;
            if (q < 0.e0) 
	      {wr[na] = x + p;
	       wr[en] = x + p;
	       wi[na] = zz;
	       wi[en] = -zz;
	       en = enm2;
	       goto outer;}
            zz = p + copysign(zz,p);  /* real roots */
            wr[na] = x + zz;
            wr[en] = wr[na];
            if (zz != 0.e0) 
	       wr[en] = x - w / zz;
            wi[na] = 0.e0;
            wi[en] = 0.e0;
            x = h[en][na];
            s = fabs(x) + fabs(zz);
            p = x / s;
            q = zz / s;
            r = sqrt(p*p+q*q);
            p = p / r;
            q = q / r;
            for (j=na;j<=n;j++)       /* row modification */
              {zz = h[na][j];
               h[na][j] = q * zz + p * h[en][j];
               h[en][j] = q * h[en][j] - p * zz;}
            for (i=1;i<=en;i++)      /* column modification */
              {zz = h[i][na];
               h[i][na] = q * zz + p * h[i][en];
               h[i][en] = q * h[i][en] - p * zz;}
            for (i= *low;i<= *igh;i++)   /* accumulate transformations */ 
              {zz = z[i][na];
               z[i][na] = q * zz + p * z[i][en];
               z[i][en] = q * z[i][en] - p * zz;}
            en = enm2;
	    goto outer;}
         if (itn == 0) 
	    return(en);
         if (its == 10 | its == 20)
           {t = t + x;    /* form exceptional shift */
            for (i= *low;i<=en;i++)
               h[i][i] = h[i][i] - x;
            s = fabs(h[en][na]) + fabs(h[na][enm2]);
            x = 0.75e0 * s;
            y = x;
            w = -0.4375e0 * s * s; }
         its = its + 1;
         itn = itn - 1; 

         for (mm=l;mm<=enm2;mm++)   /* look for 2 consecutive small */
           {m = enm2 + l - mm;     /* sub-diagonal elements */
            zz = h[m][m];          /* for m=en-2 step -1 until l do ... */
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
         for (k=m;k<=na;k++)   /* double qr step involving rows l to en & */
           {notlas = 'f';     /* columns m to en */
	    if (k != na)
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
               r = r / x;}
            s = copysign(sqrt(p*p+q*q+r*r),p);
            if (k !=  m)
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
              {for (j=k;j<=n;j++)               /* row modification */
	         {p = h[k][j] + q * h[k+1][j];
                  h[k][j] = h[k][j] - p * x;
                  h[k+1][j] = h[k+1][j] - p * y;}
               j = en;
	       if ( en>k+3 )
	          j = k+3;
               for (i=1;i<=j;i++)              /* column modification */
	         {p = x * h[i][k] + y* h[i][k+1];
	          h[i][k] = h[i][k] - p;
	          h[i][k+1] = h[i][k+1] - p * q;}

               for (i= *low;i<= *igh;i++)  /* accumulate transformations */
                 {p = x * z[i][k] + y * z[i][k+1];
                  z[i][k] = z[i][k] - p;
                  z[i][k+1] = z[i][k+1] - p * q;}    }
            else
              {for (j=k;j<=n;j++)        /* row modification */
	         {p = h[k][j] + q * h[k+1][j] + r * h[k+2][j];
                  h[k][j] = h[k][j] - p * x;
                  h[k+1][j] = h[k+1][j] - p * y;
	          h[k+2][j] = h[k+2][j] - p * zz;}
               j = en;
	       if ( en>k+3 )
	          j = k+3;
               for (i=1;i<=j;i++)       /* column modification */
	         {p = x * h[i][k] + y* h[i][k+1] + zz * h[i][k+2];
	          h[i][k] = h[i][k] - p;
	          h[i][k+1] = h[i][k+1] - p * q;
	          h[i][k+2] = h[i][k+2] - p * r;}

               for (i= *low;i<= *igh;i++)  /* accumulate transformations */
                 {p = x * z[i][k] + y * z[i][k+1] + zz * z[i][k+2];
                  z[i][k] = z[i][k] - p;
                  z[i][k+1] = z[i][k+1] - p * q;
                  z[i][k+2] = z[i][k+2] - p * r;} }
           }
             goto inner; 

outer:    ;}

/* all roots found.  backsubstitute to find vectors of upper triangular form */
      if (norm == 0.e0)            
	return(ierr);
      for (nn=1;nn<=n;nn++)   /* for en = n step -1 until 1 do ..  */
        {en = n + 1 - nn;
         p = wr[en];
         q = wi[en];
         na = en - 1;
	 if (q > 0.e0)
	    goto dout;  
         else if (q == 0.e0) 
           {m = en;             /* real vector */ 
            h[en][en] = 1.e0;
            if (na == 0)
	       goto dout;

            for (ii=1;ii<=na;ii++)       /* for i=en-1 step -1 until 1 do .. */
              {i = en - ii;
               w = h[i][i] - p;
               r = 0.e0;

               for (j=m;j<=en;j++)
                  r = r + h[i][j] * h[j][en];
 
               if (wi[i] < 0.e0)
                 {zz = w;
                  s = r;
		  continue;}
               m = i;
               if (wi[i] == 0.e0)
                 {t = w;
                  if (t == 0.e0) 
                    {tst1 = norm;
                     t = tst1;
                     while (tst2 > tst1)
                       {t = 0.01e0 * t;
                        tst2 = norm + t;}
                    }
               h[i][en] = -r / t;}
            else
              {x = h[i][i+1];       /* solve real equations */
               y = h[i+1][i];
               q = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i];
               t = (x * s - zz * r) / q;
               h[i][en] = t;
               if (fabs(x) > fabs(zz)) 
	          h[i+1][en] = (-r - w * t) / x;
               else
                  h[i+1][en] = (-s - y * t) / zz;
               t = fabs(h[i][en]);
               if (t != 0.e0)
                 {tst1 = t;
                  tst2 = tst1 + 1.e0/tst1;
                  if (tst2 <= tst1)
                    for (j=i;j<=en;j++)
                      h[j][en] = h[j][en]/t;}
          }   }  }

       else if (q < 0.e0)        /* complex vector */
	 {m = na;

/* last vector component chosen imaginary so that
   eigenvector matrix is triangular                */

         if (fabs(h[en][na]) > fabs(h[na][en]))
           {h[na][na] = q / h[en][na];
            h[na][en] = -(h[en][en] - p) / h[en][na];}
         else       
            cdiv(0.e0,-h[na][en],h[na][na]-p,q,&h[na][na],&h[na][en]);
         h[en][na] = 0.e0;
         h[en][en] = 1.e0;
         enm2 = na - 1;
         if (enm2 == 0) 
	    goto dout;
         for (ii=1;ii<=enm2;ii++)     /* for i=en-2 step -1 until 1 do... */
           {i = na - ii;
            w = h[i][i] - p;
            ra = 0.e0;
            sa = 0.e0;
            for (j=m;j<=en;j++) 
              {ra = ra + h[i][j] * h[j][na];
               sa = sa + h[i][j] * h[j][en];}
 
            if (wi[i] < 0.e0) 
              {zz = w;
               r = ra;
               s = sa;
               continue;}
            m = i;
            if (wi[i] == 0.e0)
               cdiv(-ra,-sa,w,q,&h[i][na],&h[i][en]);
            else      
              {x = h[i][i+1];      /* solve complex equations */
               y = h[i+1][i];
               vr = (wr[i] - p) * (wr[i] - p) + wi[i] * wi[i] - q * q;
               vi = (wr[i] - p) * 2.e0 * q;
               if (vr == 0.e0 & vi == 0.e0)           
                 {tst1 = norm * (fabs(w)+fabs(q)+fabs(x)+fabs(y)+fabs(zz));
                  vr = tst1;
		  while (tst2 > tst1)
                    {vr = 0.01e0 * vr;
                     tst2 = tst1 + vr;}}
               cdiv(x*r-zz*ra+q*sa,x*s-zz*sa-q*ra,vr,vi,&h[i][na],&h[i][en]);
               if (fabs(x) > fabs(zz) + fabs(q)) 
                {h[i+1][na] = (-ra - w * h[i][na] + q * h[i][en]) / x;
                 h[i+1][en] = (-sa - w * h[i][en] - q * h[i][na]) / x;}
               else      
                 cdiv(-r-y*h[i][na],-s-y*h[i][en],zz,q,&h[i+1][na],&h[i+1][en]);}
            t = fabs(h[i][na]);
	    if (fabs(h[i][en]) > t)
               t = fabs(h[i][en]);
            if (t != 0.e0) 
              {tst1 = t;
               tst2 = tst1 + 1.e0/tst1;
               if (tst2 <= tst1) 
                 {for (j=i;j<=en;j++)
                    {h[j][na] = h[j][na]/t;
                     h[j][en] = h[j][en]/t;}}}
                 
        }   } 

dout:  ;
     }
  
      for (i=1;i<=n;i++)
        {if (i >= *low & i <= *igh) 
	   continue;
         for (j=1;j<=n;j++)
           z[i][j] = h[i][j];}

      for (jj= *low;jj<=n;jj++) /* multiply by transformation matrix to give */
        {j = n + *low - jj;     /* vectors of original full matrix */
         m = j;                /* for j=n step -1 until low do ... */
         if (*igh < j)
	    m = *igh;
         for (i= *low;i<= *igh;i++)
           {zz = 0.e0;
            for (k= *low;k<=m;k++)
               zz = zz + z[i][k] * h[k][j];
            z[i][j] = zz;}
        }
return(ierr);
}
