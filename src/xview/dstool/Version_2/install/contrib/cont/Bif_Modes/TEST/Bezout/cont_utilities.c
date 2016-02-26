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
#include <math.h>
#include <constants.h>

#define WORK_SIZE  1000


/* proc to multiply two matricies */

int
multAB( C, A, a_col, B, b_row)
double	**A, **B, **C;
int	a_col, b_row;
{
  int	i, r, s;

  
  for( r=0; r<b_row; r++ )
    for( s=0; s<a_col; s++ )
      {
       C[r][s] = 0.0;
       for( i=0; i<b_row; i++ )
          C[r][s] += A[r][i]*B[i][s];
      }
}


int
biprod( a_biprod, a, n, det )
double	**a_biprod, **a, *det;
int	n;
{
  int		row=0, col=0, p, q, r, s;  
  int		biprod_dim  = (int) ((n-1)*n)/2;
  double	det_comp[2], pow();

  int		ipvt[1000];
  double	work[1000];

  for(p=1; p<n; p++)
    {
     for(q=0; q<p; q++)
       {
        for(r=1; r<n; r++)
	  {
           for(s=0; s<r; s++)
	     {
	      if(r==q)
		a_biprod[row][col] = -a[p][s];
              else if( (r!=p) && (s==q) )
		a_biprod[row][col] = a[p][r];
              else if( (r==p) && (s==q) )
		a_biprod[row][col] = a[p][p]+a[q][q];
              else if( (r==p) && (s!=q) )
		a_biprod[row][col] = a[q][s];
	      else if(s==p)
		a_biprod[row][col] = -a[q][r];
              else
		a_biprod[row][col] = 0.0;
	      ++col;
	     }
          }
        ++row;
        col = 0;
       }
      }

  dgefa(a_biprod,biprod_dim,biprod_dim,ipvt);
  dgedi(a_biprod,biprod_dim,biprod_dim,ipvt,det_comp,work,10);	/* determinant only */

  *det = det_comp[0]*pow(10.0,det_comp[1]);
}


#define SWAP(g,h) {y=(g);(g)=(h);(h)=y;}
#define RADIX 2.0


/* ---------------------------------------------------------------------
   This procedure computes the Bezout matrix associated with the char-
   acteristic polynomial for the Jacobian matrix.  The basic reference
   for this algorithm is:

     Barrett, S., `Polynomials and Linear Control Systems,' Monographs
     in Pure and Applied Mathematics, Marcel Dekker, Inc, New York, 
     1983, pps. 44-46.

   Note, however, that the index functions have been specialized for the
   case that a(x) and b(x) are interlaced in a single coefficient vector,
   c(x).  

   Arguments:
   ----------

      n     (input)    int         dimension of the Jacobian
				   (equiv. to phase space dimension)
      c     (input)    *double     coefficient vector in the form:
				     c = (a0,b0,a1,b1, ... ,an,bn)
                                   Note: expected length of coeff.
				   vector is (n+1)
     
      B     (output)   **double    Bezout matrix

   Function Value:
   ---------------

   This procedure returns an integer status flag:

	    status = bezout( B, c, n );

   where:
	    status = 0         normal termination
	    status = -1        procedure failure

   last change:  6/4/92  (mrm)

   --------------------------------------------------------------------- */

#define ODD    0
#define EVEN   1

int
bezout( B, c, n )
int	n;
double	*c, **B;
{
  int		status = 0;
  int		i, j, k, m, index, kmin, kmax;
  double	entry, ar, as, br, bs;
  double	floor(), ceil();

  if(n<3){
     status = -1;
     return(status);}

  m = ( ceil((double) (n/2.0)) -
	floor((double) (n/2.0)) > 0.1 )? (n-1)/2:n/2;  

  for(i=1; i<=m; i++)
    for(j=i; j<=m; j++)
     {
      entry = 0.0;
      kmax = i-1;
      kmin = (i+j-m-1>0)? i+j-m-1:0;
      for(k=kmin; k<=kmax; k++) 
	{
         index = i+j-k-1;
	 ar = c[2*index];
	 as = c[2*k];
	 bs = (2*k+1<=n)? c[2*k+1]:0.0;
	 br = (2*index+1<=n)? c[2*index+1]:0.0;
         entry = entry + ar*bs - as*br;
        } 
      B[i-1][j-1] = entry;
      if(i!=j) B[j-1][i-1] = entry;
     }

   return(status);
}



void balance(a,n)
double **a;
int n;
{
	int last,j,i;
	double s,r,g,f,c,sqrdx,fabs();

	sqrdx=RADIX*RADIX;
	last=0;
	while (last == 0) {
		last=1;
		for (i=0;i<n;i++) {
			r=c=0.0;
			for (j=0;j<n;j++)
				if (j != i) {
					c += fabs(a[j][i]);
					r += fabs(a[i][j]);
				}
			if (c && r) {
				g=r/RADIX;
				f=1.0;
				s=c+r;
				while (c<g) {
					f *= RADIX;
					c *= sqrdx;
				}
				g=r*RADIX;
				while (c>g) {
					f /= RADIX;
					c /= sqrdx;
				}
				if ((c+r)/f < 0.95*s) {
					last=0;
					g=1.0/f;
					for (j=0;j<n;j++) a[i][j] *= g;
					for (j=0;j<n;j++) a[j][i] *= f;
				}
			}
		}
	}
}



void hesberg(a,n)
double **a;
int n;
{
	int m,j,i;
	double y,x,fabs();

	for (m=1;m<n-1;m++) {
		x=0.0;
		i=m;
		for (j=m;j<n;j++) {
			if (fabs(a[j][m-1]) > fabs(x)) {
				x=a[j][m-1];
				i=j;
			}
		}
		if (i != m) {
			for (j=m-1;j<n;j++) SWAP(a[i][j],a[m][j])
			for (j=0;j<n;j++) SWAP(a[j][i],a[j][m])
		}
		if (x) {
			for (i=m+1;i<n;i++) {
				if (y=a[i][m-1]) {
					y /= x;
					a[i][m-1]=y;
					for (j=m;j<n;j++)
						a[i][j] -= y*a[m][j];
					for (j=0;j<n;j++)
						a[j][m] += y*a[j][i];
				}
			}
		}
	}
	for(j=0; j<n-2; j++)
	  for(i=j+2; i<n; i++) a[i][j] = 0.0;
}


/* ---------------------------------------------------------------------------
   Given a square (nxn) UPPER Hesenberg matrix, A, this procedure computes the
   associated companion matrix.  The new computed entries are overwritten in A.  

   The algorithm is transcribed from:

      Wilkinson, J.H., `The Algebraic Eigenvalue Problem,' Monographs on
      Numerical Analysis, Oxford Scientific Publications, Clarendon Press,
      Oxford, 1965, pps. 405-407.

   (Notice that in the text cited there is a typographical error in statement
    of Step (iii);  this algorithmic change is annotated in the code below )

    Remark:  This process is known to be unstable!  The following algorithm
	     should be read as  W*A = C*W where W is a product of non-unitary
	     transformations.  


    Arguments: 
    ----------

	A   (input/output)         **double         pointer to input matrix
	n   (input)                int		    matrix dimension

    last change:  6/3/92  (mrm)

    --------------------------------------------------------------------------- */

    


void compan( A, n )
int	n;
double	**A;
{
  int		i, j, k;
  double	factor;

    for(k=1; k<n; k++) 					/* Each transformation preforms a   */			
     for(i=0; i<k; i++)					/* row elimination WITHOUT pivoting */
	{
	 factor = A[i][k-1] / A[k][k-1];
	 for(j=i; j<n; j++)
           A[i][j] = A[i][j] - factor*A[k][j];
	 A[i+1][k] = A[i+1][k] + A[i+1][i]*factor;	/* Apply column-wise inverse to get */
        }						/* similarity transformation        */

  factor = 1.0;
  for(i=2; i<=n; i++)
     {
      factor *= A[n-i+1][n-i];
      A[n-i][n-1] *= factor;  		/* scale to produce unit subdiagonal */
      A[n-i+1][n-i] = 1.0;		
     }  
}


#undef SWAP 
#undef RADIX
