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

#include <math.h>

int
ludcmp(a,n,indx,d)
int 	n;					/* dim of soln vector */
int	*indx;					/* integer workspace used to record permutation */
double 	**a;					/* matrix to solve; contain LU dcmp upon return */
double	*d;					/* ptr to (only one) double; used to record +/-1 */
{
  int i,imax=0,j,k;
  double big,dum,sum,temp;
  double *vv,*dvector();
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
    if(a[j][j] == 0.0) {
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
