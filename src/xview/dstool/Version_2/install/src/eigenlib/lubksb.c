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
