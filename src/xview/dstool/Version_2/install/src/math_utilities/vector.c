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



/*  ------------------------------------------------------------------------
      copies a vector, x, into another vector, y.
   
    ------------------------------------------------------------------------ */

int
dcopy(n,dx,incx,dy,incy)
double	*dx, *dy;
int	incx, incy, n;
{
	int	i, ix, iy; 
	if(n < 0) return;
	ix = iy = 0;
 	if(incx < 0) ix = (-n+1)*incx;
	if(incy < 0) iy = (-n+1)*incy;
	for(i=0; i<n; i++) 
	   {dy[iy] = dx[ix];
	    ix += incx;
	    iy += incy; }
	return;
 
}



/*  ------------------------------------------------------------------------
    compute vector dot product

    ------------------------------------------------------------------------ */

double
ddot(n,dx,incx,dy,incy)
double *dx,*dy;
int incx,incy,n;
{
	double	dtemp; 
	int	i,ix,iy;
 
	dtemp = 0.0;				
	if(n < 0) return(0.0);		
	ix = iy = 0;
	if(incx < 0) ix = (-n+1)*incx;
	if(incy < 0) iy = (-n+1)*incy;
	for( i=0; i<n; i++)
	   {dtemp += dx[ix]*dy[iy];
	    ix += incx;
	    iy += incy;}
	return( dtemp );
}           




 
/*  ------------------------------------------------------------------------
      constant times a vector plus a vector.

    ------------------------------------------------------------------------ */
void
daxpy(n,da,dx,incx,dy,incy)
double	*dx, *dy, da;
int	incx, incy, n;
{
	int	i, ix, iy;

	if(n < 0 || da == 0.0) return;
	
	ix = iy = 0;
	if(incx < 0)ix = (-n+1)*incx;
	if(incy < 0)iy = (-n+1)*incy;
	for(i=0; i<n; i++)
	   {dy[iy] = dy[iy] + da*dx[ix];
	    ix += incx;
	    iy += incy;}
}


/*  ------------------------------------------------------------------------
     interchanges two vectors 
   
    ------------------------------------------------------------------------ */

void
dswap (n,dx,incx,dy,incy)
double	*dx, *dy;
int	incx, incy, n;
{
	double	dtemp;
	int	i, ix, iy;
 
	if(n < 0) return;

	ix = iy = 0;
	if(incx < 0) ix = (-n+1)*incx;
	if(incy < 0) iy = (-n+1)*incy;
	for( i=0; i<n; i++)
    	   {dtemp = dx[ix];
    	    dx[ix] = dy[iy];
    	    dy[iy] = dtemp;
    	    ix += incx;
    	    iy += incy;}
}



/*  ------------------------------------------------------------------------
    scales a vector by a constant
   
    ------------------------------------------------------------------------ */
void
dscal(n,da,dx,incx)
double	da, *dx;  
int	incx,n;
{
	int	i;
	/* int nincx;   nincx not used! -paw  */

	if(n < 0) return;
	/* nincx = n*incx;         - paw */
	for(i=0; i<n; i++) 
	   dx[i] *= da;
}


/*
	finds the index of element having max. absolute value.
*/

int
idamax(n,dx,incx)
int	n, incx;
double	*dx;
{
      double	dmax;
      int	i,ix,result; 
 
      result = -1;
      if( n < 1 ) return(result);
      result = 0;
      if(n == 1) return(result);
 
      ix = 0;
      dmax = fabs(dx[0]);
      ix = ix + incx;
      for(i=1; i<n; i++)
         {if(fabs(dx[ix]) > dmax) 
             {result = i;
              dmax = fabs(dx[ix]);}
          ix = ix + incx;
         }       
      return(result);
}
