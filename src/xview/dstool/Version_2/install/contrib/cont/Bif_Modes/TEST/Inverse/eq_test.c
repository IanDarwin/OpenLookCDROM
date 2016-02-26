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
#include <malloc.h>
#include <stdio.h>


#define WORK_SIZE  1000

/* 
   proc used to test the Hodgkin-Huxley equations and their derivatives
*/


FILE		*fp;

main()
{
  static double	f[4], param[]={120.0,36.0,0.3,-115.,84.1991,10.599,6.3,114.522}, det[3],
		varbs[]={-7.36247,0.120727,0.433917,0.339679}, **dfdx, **m, **dmatrix();
  double	work[1000], det_value, coeffs[10], **m1, **m2, **m3, value;
  int		iwork[1000], n_varb=4;
  int		func(), i, j, bezout_dim;
  void		balance(), hesberg(), compan();

  fp = fopen( "OUTPUT", "w" );

  func( f, varbs, param );
  
  dfdx = (double **) dmatrix(0,4,0,4);
  m1 = (double **) dmatrix(0,4,0,4);
  m2 = (double **) dmatrix(0,4,0,4);
  m3 = (double **) dmatrix(0,4,0,4);
  dfunc( dfdx, varbs, param );

  dfdx[0][0] = 1.0; dfdx[0][1] = 3.0; dfdx[0][2] = 7.0; dfdx[0][3] = 2.0;
  dfdx[1][0] = 5.0; dfdx[1][1] = 9.0; dfdx[1][2] = 3.0; dfdx[1][3] = 5.0;
  dfdx[2][0] = 4.0; dfdx[2][1] = 3.0; dfdx[2][2] = 0.0; dfdx[2][3] = 2.0;
  dfdx[3][0] = 6.0; dfdx[3][1] = 4.0; dfdx[3][2] = 2.0; dfdx[3][3] = 9.0;

  for(i=0; i<4; i++) 
    for(j=0; j<4; j++)
      {
       m1[i][j] = m2[i][j] = m3[i][j] = 0.0;
       m1[i][j] = dfdx[i][j];
      }

  fprintf(stderr,"\n Matrix: \n");
  m_output( m1, 4 );
  minvse( m1, m2, 4 );
  fprintf(stderr,"\n Matrix Inverse: \n");
  m_output( m2, 4 );
  frob_matnorm( m2, 4, &value );
  fprintf(stderr,"\n Inverse Matrix Norm:  %18.12g \n",value);

  multAB( m3, dfdx, 4, m2, 4);
  m_output( m3, 4 );

  output( f, dfdx, varbs, param );

  m = (double **) dmatrix(0,6,0,6);

  fprintf(stderr,"\n Biproduct Matrix \n");
  biprod( m, dfdx, 4, &det_value );
  m_output( m, 6 );

  fprintf(stderr,"\n \n Biproduct Determinant:  %20.12g \n\n",det_value);

  bezout_dim = ( ceil((double) (n_varb/2.0)) -
	  floor((double) (n_varb/2.0)) > 0.1 )? (n_varb-1)/2:n_varb/2;
  for(i=0; i<n_varb+2; i++) coeffs[i] = 1.0;
  
  balance(dfdx, n_varb);					/* balance column norms */
  fprintf(stderr,"\n balanced jacobian: \n");
  m_output( dfdx, 4 );
  
  hesberg(dfdx, n_varb);			/* reduce to upper unit hessenberg */
  fprintf(stderr,"\n hessenberg: \n");
  m_output( dfdx, 4 );

  compan(dfdx, n_varb);			/* reduce to Frobenius form */
  fprintf(stderr,"\n companion: \n");
  m_output( dfdx, 4 );

  for(i=0; i<n_varb; i++) 
    coeffs[i] = -dfdx[i][n_varb-1];		/* pull off coefficients of the characteristic poly */

  for(i=0;i<bezout_dim;i++)
    for(j=0;j<bezout_dim;j++) dfdx[i][j] = 0.0;			/* construct bezout matrix */


  fprintf(stderr,"\n");
  for(i=0; i<n_varb+1; i++) fprintf(stderr,"%18.10g ",coeffs[i]);
  fprintf(stderr,"\n");


  bezout( dfdx, coeffs, n_varb );
  fprintf(stderr,"\n Bezout Matrix: \n");
  m_output( dfdx, bezout_dim );

  dgefa(dfdx,bezout_dim,bezout_dim,iwork);
  dgedi(dfdx,bezout_dim,bezout_dim,iwork,det,work,10);     /* determinant only */
  det_value = det[0]*pow(10.0,det[1]);
  fprintf(stderr,"\n \n Bezout Determinant:  %20.12g \n\n",det_value);

  fclose(fp);
  free_dmatrix(m,0,6,0,6);
}

int
func( f, varbs, param )
double	*f, *varbs, *param;
{
  extern double 	psi(), phi(), exp();
  double		v, m, n, h,
			gna, vna, gk, vk, gl, vl, I,
			T, am, bm, an, bn, ah, bh;

  v = varbs[0];
  m = varbs[1];
  n = varbs[2];
  h = varbs[3];

  gna = param[0];
  gk  = param[1];
  gl  = param[2];
  vna = param[3];
  vk  = param[4];
  vl  = param[5];
  T   = param[6];
  I   = param[7];

  am = psi( (v+25.0)/10.0 );
  bm = 4.0*exp( v/18.0 );
  an = psi( (v+10.0)/10.0 )/10.0;
  bn = exp( v/80.0 )/8.0;
  ah = 7.0*exp( v/20.0 )/100.0;
  bh = 1.0/( 1.0+exp( (v+30.0)/10.0 ) );

  f[0] = -( gna*m*m*m*h*(v-vna) + gk*n*n*n*n*(v-vk) + gl*(v-vl) + I );
  f[1] = phi(T)*( (1.0-m)*am - m*bm ); 
  f[2] = phi(T)*( (1.0-n)*an - n*bn ); 
  f[3] = phi(T)*( (1.0-h)*ah - h*bh ); 

}

int
dfunc( dfdx, varbs, param )
double  **dfdx, *varbs, *param;
{
  extern double 	psi(), phi(), exp();
  double		v, m, n, h,
			gna, vna, gk, vk, gl, vl, I,
			T, am, bm, an, bn, ah, bh;

  v = varbs[0];
  m = varbs[1];
  n = varbs[2];
  h = varbs[3];

  gna = param[0];
  gk  = param[1];
  gl  = param[2];
  vna = param[3];
  vk  = param[4];
  vl  = param[5];
  T   = param[6];
  I   = param[7];

  dfdx[0][0] = -(gl + gna*h*m*m*m + gk*n*n*n*n);
  dfdx[0][1] = -3.0*gna*h*m*m*(v-vna);
  dfdx[0][2] = -4.0*gk*n*n*n*(v-vk);
  dfdx[0][3] = -(gna*m*m*m*(v-vna));
  dfdx[1][0] = phi(T)*( ((1.0-m)/10.0)/( exp(((25.0+v)/10.0))-1.0 ) -
			(2.0/9.0)*m*exp(v/18.0) - 
			psi( (25.0+v)/10.0 )*psi( (25.0+v)/10.0 )*((1.0-m)/10.0)*
			exp((25.0+v)/10.0)/((25.0+v)/10.0) );
  dfdx[1][1] = phi(T)*(-4.0*exp(v/18.0) - psi( (25.0+v)/10.0 ) );
  dfdx[1][2] = 0.0;
  dfdx[1][3] = 0.0;
  dfdx[2][0] = phi(T)*( ((1.0-n)/100.0)/(exp( (10.0+v)/10.0 ) - 1.0 ) -
			n*exp(v/80.0)/640.0 -
			((1.0-n)/100.0)*exp((10.0+v)/10.0)*psi( (10.0+v)/10.0 )/(exp( (10.0+v)/10.0 ) - 1.0 ) );
  dfdx[2][1] = 0.0; 
  dfdx[2][2] = -phi(T)*(exp(v/80.0)/8.0 + (10.0+v)/(exp((10.0+v)/10.0)-1.0)/100.0);
  dfdx[2][3] = 0.0;
  dfdx[3][0] = phi(T)*( exp(v/20.0)*(1.0-h)*7.0/2000.0 + 
	       (h/10.0)*exp((30.0+v)/10.0)/( (1.0+exp((30.0+v)/10.0))*(1.0+exp((30.0+v)/10.0))) );
  dfdx[3][1] = 0.0;
  dfdx[3][2] = 0.0;
  dfdx[3][3] = -phi(T)*(7.0*exp(v/20.0)/100.0 + 1.0/(1.0+exp((30.0+v)/10.0)) );

}


int
est_dfunc( dfdx, varbs, param, delta )
double  dfdx[][4], *varbs, *param, delta;
{
  int    i, j;
  double m1[4], m2[4], v[4];

  for(i=0; i<4; i++)
    {
     for(j=0; j<4; j++) v[j] = varbs[j];

     v[i] += delta;
     func( m1, v, param );
     v[i] -= 2.0*delta;
     func( m2, v, param );

     for(j=0; j<4; j++)
     dfdx[j][i] = (m1[j] - m2[j])/(2.0*delta);
    }
}

double psi(x)
double x;
{
  extern double 	exp(),fabs();
  double 		x2,x4,x6,x8,x10,x12;

  if (fabs(x)<0.1)
    {x2 = x*x; x4= x2*x2; x6 = x2*x4; x8 = x4*x4; x10 = x4*x6; x12 = x6*x6;
     return(1.0 - x/2.0 + x2/12.0 - x4/720.0 + x6/30240.0 - x8/1209600.0 +
	    x10/47900160.0 -691.0*x12/1307674368000.0);}
  else return(x/(exp(x)-1.0));
}

double phi(T)
double	T;
{
  extern double		exp(), log();

  return (exp(log(3.0)*(T-6.3)/10.0) );
}

int
est_dxx( matrix, varbs, omega, param, delta )
double  matrix[][4], *varbs, *omega, *param, delta;
{
  int    		i, j, k;
  extern double         psi(), phi(), exp();
  double 		v1[4], v2[4], vec[4], mat[4][4];
  double                v, m, n, h,
                        gna, vna, gk, vk, gl, vl, I,
			T, am, bm, an, bn, ah, bh;

  v = varbs[0];
  m = varbs[1];
  n = varbs[2];
  h = varbs[3];

  gna = param[0];
  vna = param[1];
  gk  = param[2];
  vk  = param[3];
  gl  = param[4];
  vl  = param[5];
  I   = param[6];
  T   = param[7];

  for(i=0; i<4; i++)
    {
     for(j=0; j<4; j++) vec[j] = varbs[j];

     vec[i] += delta;
     dfunc( mat, vec, param );
     for(j=0; j<4; j++)
       {
        v1[j] =  0.0;
        for(k=0; k<4; k++)
  	  v1[j] += mat[j][k]*omega[k];
       }
     vec[i] -= 2.0*delta;
     dfunc( mat, vec, param );
     for(j=0; j<4; j++)
       {
        v2[j] =  0.0;
        for(k=0; k<4; k++)
  	  v2[j] += mat[j][k]*omega[k];
       }

     for(j=0; j<4; j++)
       matrix[j][i] = (v1[j] - v2[j])/(2.0*delta);
    }
}





output( f, dfdx, varbs, param )
double	*f, **dfdx, *varbs, *param;
{
  extern double 	psi(), phi(), exp();
  double		v, m, n, h,  
			gna, vna, gk, vk, gl, vl, I,
			T, am, bm, an, bn, ah, bh;

  printf("\n \n \n---- Test Hodgkin-Huxley Equations ---- \n \n");
  printf("Variables:    V   = %10.4lg   m   = %10.4lg   \n",varbs[0],varbs[1]);
  printf("              n   = %10.4lg   h   = %10.4lg \n\n",varbs[2],varbs[3]);
  printf("Parameters :  gna = %10.4lg   vna = %10.4lg   \n",param[0],param[1]);
  printf("              gk  = %10.4lg   vk  = %10.4lg   \n",param[2],param[3]);
  printf("              gl  = %10.4lg   vl  = %10.4lg   \n",param[4],param[5]);
  printf("              I   = %10.4lg   T   = %10.4lg \n\n",param[6],param[7]);
  printf("(dV,dm,dn,dh) : ( %10.4lg, %10.4lg, %10.4lg, %10.4lg ) \n \n",f[0],f[1],f[2],f[3]);

  printf("D_(x) f       :   %10.4lg    %10.4lg    %10.4lg    %10.4lg \n",dfdx[0][0],dfdx[0][1],dfdx[0][2],dfdx[0][3]);
  printf("                  %10.4lg    %10.4lg    %10.4lg    %10.4lg \n",dfdx[1][0],dfdx[1][1],dfdx[1][2],dfdx[1][3]);
  printf("                  %10.4lg    %10.4lg    %10.4lg    %10.4lg \n",dfdx[2][0],dfdx[2][1],dfdx[2][2],dfdx[2][3]);
  printf("                  %10.4lg    %10.4lg    %10.4lg    %10.4lg \n \n",dfdx[3][0],dfdx[3][1],dfdx[3][2],dfdx[3][3]);

}


double
  **dmatrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
  int i, n_cols, n_rows, total_pts;
  double **m;
  int free_dmatrix();

  n_cols = nch-ncl+1;
  n_rows = nrh-nrl+1;
  if (n_rows<=0 || n_cols<=0) return(NULL);
  total_pts = n_cols*n_rows;
  
  if ( (m = (double **) malloc( (unsigned) (n_rows * sizeof(double *)))) == NULL)
    {
      printf("dmatrix: memory allocation failure!");
    }
  else
    {
      if ( (m[0] = (double *) malloc( (unsigned) (total_pts * sizeof(double)))) == NULL)
	{
	  free(m);
	  m = NULL;
	  printf(1,"dmatrix: memory allocation failure!");
	}
      else
	{
	  m[0] = m[0] - ncl;
	  for (i=1; i<n_rows; i++) m[i] = m[i-1] + n_cols;
	  m = m-nrl;
	}
    }
  return(m);
}



int
  free_dmatrix(m,nrl,nrh,ncl,nch)
double **m;
int nrl,nrh,ncl,nch;
{
  if (m==NULL) return;
  free( (char *) (m[nrl] + ncl) );
  free( (char *) (m + nrl) );
}





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

m_output( m , n )
double	**m;
int	n;
{
  int	i, j;

  for(i=0; i<n; i++) 
    {
     for(j=0; j<n; j++) 
	fprintf(stderr,"%18.12g ",m[i][j]);
     fprintf(stderr,"\n");
    }

  fprintf(fp,"M = [ ");
  for(i=0; i<n; i++) 
    {
     for(j=0; j<n; j++) 
	fprintf(fp,"%18.12g, ",m[i][j]);
     fprintf(fp,"\n");
    }
  fprintf(fp,"] ");
}




/*
 *  proc to return the inverse of a matrix 
 *  (Note:  This should NEVER be used to solve the linear system Ax=b !!
 *
 *  last change:  7/15/92  (mrm)
*/

minvse( m, m_inverse, dim )
double	**m, **m_inverse;
int	dim;
{
    int		i, j;
    double	*dvector(), *column, d;
    int		*ivector(), *index;

    column = dvector(0, dim);
    index = ivector(0, dim);

    ludcmp( m, dim, index, &d);

    m_output( m, 4 );

    for(j=0; j<dim; j++)
     {
      for(i=0; i<dim; i++) column[i] = 0.0;
      column[j] = 1.0;
      lubksb(m,dim,index,column);
      for(i=0; i<dim; i++) m_inverse[i][j] = column[i];
     }

    free_dvector(column, 0, dim);
    free_ivector(index, 0, dim);
}


/*
 *  proc to compute the Frobenius norm of a matrix
 *
 *  last change:  7/15/92  (mrm)
*/

frob_matnorm( matrix, n, value )
int	n;
double	**matrix, *value;
{
  int		i, j;
  double	sqrt();

  for(i=0; i<n; i++)
     for(j=0; j<n; j++)
	*value = matrix[i][j]*matrix[i][j];

  *value = sqrt(*value);
}




/*
ludcmp() performs an LU decomposition on a matrix.  This function is taken from
	NUMERICAL RECIPES, Press et al, p. 43.
	Note, however, that our indices go from 0,...n-1, and we pass in wkspace.
	Given an n x n matrix a[0,..n-1][0,..n-1], this routine replaces it by the
	LU decomposition of a rowwise permutation of itself.  indx[0,..n-1] is an
	output vector which records the row permutation effected by the partial 
	pivoting; d returns +/- 1 depending on whether the number of permutations was
	even or odd, respectively. 
	Use this routine with lubksb.
*/


int
ludcmp(a,n,indx,d)
int 	n;					/* dim of soln vector */
int	*indx;					/* integer workspace used to record permutation */
double 	**a;					/* matrix to solve; contain LU dcmp upon return */
double	*d;					/* ptr to (only one) double; used to record +/-1 */
{
  int i,imax=0,j,k;
  double big,dum,sum,temp;
  double *vv,*dvector(),fabs();
  int status = 0;

  if ( !(vv=dvector(0,n-1)) )			/* memory failure = -1 */
    return ( -1 );

  *d = 1.0;
  for(i=0;i<n;i++) {
    big = 0.0;
    for(j=0;j<n;j++)
      if((temp = fabs(a[i][j])) > big) big = temp;
    if(big == 0.0) {
      status = 1;				/* singular matrix = 1 */
      goto done;
    }
    vv[i] = 1.0/big;
  }
  for(j=0;j<n;j++){
    for(i=0;i<j;i++) {
      sum = a[i][j];
      for(k=0;k<i;k++) sum -= a[i][k] * a[k][j];
      a[i][j] = sum;
    }
    big = 0.0;
    for (i=j;i<n;i++) {
      sum = a[i][j];
      for(k=0;k<j;k++) sum -= a[i][k] * a[k][j];
      a[i][j] = sum;
      if((dum = vv[i] * fabs(sum)) >=big) {
	big = dum;
	imax = i;
      }
    }
    if(j !=imax) {
      for(k=0;k<n;k++){
	dum = a[imax][k];
	a[imax][k] = a[j][k];
	a[j][k] = dum;
      }
      *d = -(*d);
      vv[imax] = vv[j];
    }
    indx[j] = imax;

    /* modified for our special application */
    if(a[j][j] == 0) {
      status = 1;				/* singular matrix = 1 */
      goto done;
    }
    if(j != n-1) {
      dum = 1.0 / (a[j][j]);
      for(i=j+1;i<n;i++) a[i][j] *= dum;
    }
  }
  
 done:
  (void) free_dvector(vv,0,n-1);
  return(status);
}


/*
lubksb() performs LU back substitution.  This function is taken from NUMERICAL 
	RECIPES, Press, et al, p. 44.
	This solves a set of n linear equations a.x = b.  Here a[0,..n-1][0,..n-1]
	(Note: our indices are different than those in Press et al!)
	is input, not as the matrix a, but as its LU decomp, performed by ludcmp.
	indx[0,..n-1] is input as the permutation vector returned by ludcmp.
	b[0,...n-1] is input as the RHS vector and upon return it contains the 
	solution vector x.
*/

int
lubksb(a,n,indx,b)
double **a,b[];
int n,indx[];
{
	int i,ii= -1,ip,j;
	double sum;

	for(i=0;i<n;i++){
		ip = indx[i];
		sum = b[ip];
		b[ip] = b[i];
		if(ii >= 0)
			for(j=ii;j<=i-1;j++) sum -= a[i][j] * b[j];
		else if (sum) ii = i;
		b[i] = sum;
	}
	for(i=n-1;i>=0;i--){
		sum = b[i];
		for(j=i+1;j<n;j++) sum -= a[i][j] * b[j];
		b[i] = sum/a[i][i];
	}
	return(0);
}




/*
 * dvector()
 *
 * memory allocation for a vector of doubles
 * returns m so that m[nl],...,m[nh] are the valid locations
 *
 */
double 
  *dvector(nl,nh)
int nl,nh;
{
  double *v;

  if (nh < nl) return(NULL);
  if ( (v = (double *) malloc((unsigned) (nh - nl + 1) * sizeof(double))) == NULL)
    {
      fprintf(stderr,"dvector: allocation failure!");
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
 */
int
  free_dvector(v,nl,nh)
double *v;
int nl,nh;
{
  if (v == NULL) return;
  free((char *) (v + nl));
}

/*
 * ivector()
 *
 * memory allocation for a vector of integers
 * returns m so that m[nl],...,m[nh] are the valid locations
 *
 */
int
  *ivector(nl,nh)
int nl,nh;
{
  int *v;

  if (nh < nl) return(NULL);
  if ( (v = (int *) malloc((unsigned) (nh - nl + 1) * sizeof(int))) == NULL)
    {
      fprintf(stderr,"ivector: allocation failure!");
    }
  else 
    {
      v -= nl;
    }
  return(v);
}


/*
 * free_ivector()
 *
 * frees memory allocated by ivector()
 *
 * last modified: 8/21/91  paw
 */
int
  free_ivector(v,nl,nh)
int *v;
int nl,nh;
{
  if (v == NULL) return;
  free((char *) (v + nl));
}



