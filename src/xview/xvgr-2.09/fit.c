/* $Id: fit.c,v 1.9 92/08/02 17:44:59 pturner Exp Locker: pturner $
 *
 * curve fitting, and other numerical routines used in compose.
 *
 * Contents:
 *
 * void gauss() - simple gauss elimination for least squares poly fit
 * void stasum() - compute mean and variance
 * void leasqu() - entry to linear or polynoimial regression routines
 * double leasev() - evaluate least squares polynomial
 * void fitcurve() - compute coefficients for a polynomial fit of degree >1
 * void runavg() - compute a running average
 * void runstddev() - compute a running standard deviation
 * void runmedian() - compute a running median
 * void runminmax() - compute a running minimum or maximum
 * void filterser() - apply a digital filter
 * void linearconv() - convolve one set with another
 * int crosscorr() - cross/auto correlation
 * int linear_regression() - linear regression
 * void spline() - compute a spline fit
 * double seval() - evaluate the spline computed in spline()
 */

#include <stdio.h>
#include <math.h>
#include "defines.h"

static char buf[256];

/*
	simple gauss elimination - no pivoting or scaling strategies
	all matrices are sym-pos-def
*/
void gauss(n, a, adim, b, x)
    int n, adim;
    double a[], b[], x[];

{

    int i, k, j;
    double mult;

    for (k = 0; k < n - 1; k++) {
	for (i = k + 1; i < n; i++) {
	    mult = a[adim * i + k] / a[adim * k + k];
	    for (j = k + 1; j < n; j++) {
		a[adim * i + j] = a[adim * i + j] - mult * a[adim * k + j];
	    }
	    b[i] = b[i] - mult * b[k];
	}
    }
    for (i = n - 1; i >= 0; i--) {
	x[i] = b[i];
	for (j = i + 1; j < n; j++)
	    x[i] = x[i] - a[adim * i + j] * x[j];
	x[i] = x[i] / a[adim * i + i];
    }
}

/*
	compute mean and standard dev
*/
void stasum(x, n, xbar, sd, flag)
    double x[], *xbar, *sd;
    int n, flag;
{
    int i;

    *xbar = 0;
    *sd = 0;
    if (n < 1)
	return;
    for (i = 0; i < n; i++)
	*xbar = (*xbar) + x[i];
    *xbar = (*xbar) / n;
    for (i = 0; i < n; i++)
	*sd = (*sd) + (x[i] - *xbar) * (x[i] - *xbar);
    if (n - flag)
	*sd = sqrt(*sd / (n - flag));
    else {
	errwin("compmean: (n-flag)==0");
	*sd = 0;
    }
}

/*
	least squares polynomial fit - for polynomials
	of degree 5 or less
*/
void leasqu(n, x, y, degree, w, wdim, r)
    int n, degree, wdim;
    double x[], y[], w[], r[];

{
    double b[11];
    double sumy1, sumy2, ybar, ysdev, stemp, rsqu;
    double xbar, xsdev;
    int i, j, k;

    sumy1 = 0.0;
    sumy2 = 0.0;
    /* form the matrix with normal equations and RHS */
    for (k = 0; k <= degree; k++) {
	for (j = k; j <= degree; j++) {
	    w[wdim * k + j] = 0.0;
	    for (i = 0; i < n; i++) {
		if (x[i] != 0.0)
		    w[wdim * k + j] = pow(x[i], (double) (k)) * pow(x[i], (double) (j)) + w[wdim * k + j];
	    }
	    if (k != j)
		w[wdim * j + k] = w[wdim * k + j];
	}
    }
    for (k = 0; k <= degree; k++) {
	b[k] = 0.0;
	for (i = 0; i < n; i++) {
	    if (x[i] != 0.0)
		b[k] = b[k] + pow(x[i], (double) (k)) * y[i];
	}
    }
    gauss(degree + 1, w, wdim, b, r);	/* solve */
    stasum(y, n, &ybar, &ysdev, 1);	/* compute statistics on fit */
    stasum(x, n, &xbar, &xsdev, 1);
    for (i = 0; i < n; i++) {
	stemp = 0.0;
	for (j = 1; j <= degree; j++) {
	    if (x[i] != 0.0)
		stemp = stemp + r[j] * pow(x[i], (double) (j));
	}
	sumy1 = sumy1 + (stemp + r[0] - y[i]) * (stemp + r[0] - y[i]);
	sumy2 = sumy2 + y[i] * y[i];
    }
    rsqu = 1.0 - sumy1 / (sumy2 - n * ybar * ybar);
    if (rsqu < 0.0)
	rsqu = 0.0;
    sprintf(buf, "\nNumber of observations = %10d\n", n);
    for (i = 0; i <= degree; i++) {
	sprintf(buf, "A[%d] = %.5g\n", i, r[i]);
	stufftext(buf);
    }
    i += 4;
    sprintf(buf, "R square = %.5g\n", rsqu);
    stufftext(buf);
    sprintf(buf, "Avg Y    = %.5g\n", ybar);
    stufftext(buf);
    sprintf(buf, "Sdev Y   = %.5g\n", ysdev);
    stufftext(buf);
    sprintf(buf, "Avg X    = %.5g\n", xbar);
    stufftext(buf);
    sprintf(buf, "Sdev X   = %.5g\n\n", xsdev);
    stufftext(buf);
    stufftext("\n");
}

/*
	evaluate least squares polynomial
*/
double leasev(c, degree, x)
    double c[], x;
    int degree;
{
    double temp;
    int i;

    temp = 0.0;
    for (i = 0; i <= degree; i++) {
	if ((i == 0) && (x == 0.0))
	    temp = temp + c[i];	/* avoid 0.0^0 */
	else
	    temp = temp + c[i] * pow(x, (double) (i));
    }
    return (temp);
}

/*
	curve fitting
*/
void fitcurve(x, y, n, ideg, fitted)
    double x[], y[], fitted[];
int ideg, n;

{
    int i, ifail;
    double result[20], w[MAXFIT][MAXFIT], leasev();

    ifail = 1;
    if (ideg > 1) {
	leasqu(n, x, y, ideg, w, MAXFIT, result);
	for (i = 0; i < n; i++)
	    fitted[i] = leasev(result, ideg, x[i]);
	ifail = 0;
    } else {
	ifail = linear_regression(n, x, y, fitted);
	if (ifail == 1) {
	    errwin("Linear_regression entered with N <= 3");
	} else if (ifail == 2) {
	    errwin("Linear_regression - all values of x or y are the same");
	}
    }
}

/*
	compute a running average
*/
void runavg(x, y, ax, ay, n, ilen)
    double x[], y[], ax[], ay[];
int n, ilen;

{
    int i;
    double sumy = 0.0;
    double sumx = 0.0;

    for (i = 0; i < ilen; i++) {
	sumx = sumx + x[i];
	sumy = sumy + y[i];
    }
    ax[0] = sumx / ilen;
    ay[0] = sumy / ilen;
    for (i = 1; i < (n - ilen + 1); i++) {
	sumx = x[i + ilen - 1] - x[i - 1] + sumx;
	ax[i] = sumx / ilen;
	sumy = y[i + ilen - 1] - y[i - 1] + sumy;
	ay[i] = sumy / ilen;
    }
}

/*
	compute a running standard deviation
*/
void runstddev(x, y, ax, ay, n, ilen)
    double x[], y[], ax[], ay[];
int n, ilen;

{
    int i;
    double ybar, ysd;
    double sumx = 0.0;

    for (i = 0; i < ilen; i++) {
	sumx = sumx + x[i];
    }
    ax[0] = sumx / ilen;
    stasum(y, ilen, &ybar, &ysd, 0);
    ay[0] = ysd;
    for (i = 1; i < (n - ilen + 1); i++) {
	stasum(y + i, ilen, &ybar, &ysd, 0);
	sumx = x[i + ilen - 1] - x[i - 1] + sumx;
	ax[i] = sumx / ilen;
	ay[i] = ysd;
    }
}

/*
	compute a running median
*/
void runmedian(x, y, ax, ay, n, ilen)
    double *x, *y, *ax, *ay;
    int n, ilen;

{
    int i, j, nlen = n - ilen + 1;
    double *tmpx, *tmpy;

    tmpx = (double *) calloc(ilen, sizeof(double));
    if (tmpx == NULL) {
	errwin("Can't calloc tmpx in runmedian");
	return;
    }
    tmpy = (double *) calloc(ilen, sizeof(double));
    if (tmpy == NULL) {
	errwin("Can't calloc tmpy in runmedian");
	cxfree(tmpx);
	return;
    }
    for (i = 0; i < nlen; i++) {
	for (j = 0; j < ilen; j++) {
	    tmpx[j] = x[j + i];
	    tmpy[j] = y[j + i];
	}
	sort_xy(tmpx, tmpy, ilen, 1, 0);

	if (ilen % 2) {
	    ax[i] = x[i + (ilen / 2)];
	    ay[i] = tmpy[ilen / 2];
	} else {
	    ax[i] = (x[i + ilen / 2] + x[i + (ilen - 1) / 2]) * 0.5;
	    ay[i] = (tmpy[ilen / 2] + tmpy[(ilen - 1) / 2]) * 0.5;
	}
    }
    cxfree(tmpx);
    cxfree(tmpy);
}

/*
	compute a running minimum or maximum
*/
void runminmax(x, y, ax, ay, n, ilen, type)
    double x[], y[], ax[], ay[];
int n, ilen, type;

{
    int i, j;
    double min, max;
    double sumx = 0.0;

    min = max = y[0];
    for (i = 0; i < ilen; i++) {
	sumx = sumx + x[i];
	if (min > y[i])
	    min = y[i];
	if (max < y[i])
	    max = y[i];
    }
    ax[0] = sumx / ilen;
    if (type == 0) {
	ay[0] = min;
    } else if (type == 1) {
	ay[0] = max;
    } else {
	errwin("Unknown type in runminmax, setting type = min");
	type = 0;
    }
    for (i = 1; i < (n - ilen + 1); i++) {
	sumx = x[i + ilen - 1] - x[i - 1] + sumx;
	ax[i] = sumx / ilen;
	min = y[i];
	max = y[i];
	for (j = 0; j < ilen; j++) {
	    if (min > y[i + j])
		min = y[i + j];
	    if (max < y[i + j])
		max = y[i + j];
	}
	if (type == 0) {
	    ay[i] = min;
	} else if (type == 1) {
	    ay[i] = max;
	}
    }
}

/*
	Apply a digital filter of length len to a set in x, y,
	of length n with the results going to resx, resy.
	the length of the result is set by the caller
*/
void filterser(n, x, y, resx, resy, h, len)
    double x[], y[], resx[], resy[], h[];
int n, len;

{
    int i, j, outlen, eo, ld2;
    double sum;

    outlen = n - len + 1;
    eo = len % 2;
    ld2 = len / 2;
    for (i = 0; i < outlen; i++) {
	sum = 0.0;
	for (j = 0; j < len; j++) {
	    sum = sum + h[j] * y[j + i];
	}
	resy[i] = sum;
	if (eo)
	    resx[i] = x[i + ld2];
	else
	    resx[i] = (x[i + ld2] + x[i + ld2 - 1]) / 2.0;
    }
}

/*
	linear convolution of set x (length n) with h (length m) and
	result to y. the length of y is set by the caller
*/
void linearconv(x, h, y, n, m)
    double x[], h[], y[];
int n, m;

{
    int i, j, itmp;

    for (i = 0; i < n + m - 1; i++) {
	for (j = 0; j < m; j++) {
	    itmp = i - j;
	    if ((itmp >= 0) && (itmp < n)) {
		y[i] = y[i] + h[j] * x[itmp];
	    }
	}
    }
}

/*
 * cross correlation
 */
int crosscorr(x, y, n, lag, meth, xcov, xcor)
    double x[], y[], xcov[], xcor[];
int n, lag, meth;

{
    double xbar, xsd;
    double ybar, ysd;
    int i, j;

    if (lag >= n)
	return 1;
    stasum(x, n, &xbar, &xsd, 0);
    if (xsd == 0.0)
	return 2;
    stasum(y, n, &ybar, &ysd, 0);
    if (ysd == 0.0)
	return 3;
    for (i = 0; i < lag; i++) {
	xcor[i] = 0.0;
	xcov[i] = 0.0;
	for (j = 0; j < n - i; j++) {
	    xcov[i] = xcov[i] + (y[j] - ybar) * (x[j + i] - xbar);
	}
	if (meth)
	    xcov[i] = xcov[i] / (n - i);
	else
	    xcov[i] = xcov[i] / n;

	xcor[i] = xcov[i] / (xsd * ysd);
    }
    return 0;
}

/*
 * exponential, power fits
 */
int transfit(type, n, x, y, fitted)
    int type;
    double x[], y[], fitted[];
int n;

{
    int i;
    double *xtmp, *ytmp;
    char errbuf[128];

    xtmp = (double *) calloc(n, sizeof(double));
    if (xtmp == NULL) {
	errwin("Insufficient memory to perform operation");
	return 0;
    }
    ytmp = (double *) calloc(n, sizeof(double));
    if (ytmp == NULL) {
	cfree(xtmp);
	errwin("Insufficient memory to perform operation");
	return 0;
    }
    for (i = 0; i < n; i++) {
	xtmp[i] = x[i];
	ytmp[i] = y[i];
    }
    switch (type) {
    case 0:
	for (i = 0; i < n; i++) {
	    if (ytmp[i] <= 0.0) {
		cfree(xtmp);
		cfree(ytmp);
		sprintf(errbuf, "Y[%d] <= 0.0, operation cancelled", i + 1);
		errwin(errbuf);
		return 0;
	    } else {
		ytmp[i] = log(ytmp[i]);
	    }
	}
	linear_regression(n, xtmp, ytmp, fitted);
	for (i = 0; i < n; i++) {
	}
	break;
    case 1:
	break;
    case 2:
	break;
    case 3:
	break;
    case 4:
	break;
    }
}

/*
	References,

	_Aplied Linear Regression_, Weisberg
	_Elements of Statistical Computing_, Thisted

	Fits y = coef*x + intercept + e

	uses a 2 pass method for means and variances

*/

int linear_regression(n, x, y, fitted)
    double x[], y[], fitted[];
int n;

{
    double xbar, ybar;		/* sample means */
    double sdx, sdy;		/* sample standard deviations */
    double sxy, rxy;		/* sample covariance and sample correlation */
    double SXX, SYY, SXY;	/* sums of squares */
    double RSS;			/* residual sum of squares */
    double rms;			/* residual mean square */
    double sereg;		/* standard error of regression */
    double seslope, seintercept;
    double slope, intercept;	/* */
    double SSreg, F, R2;
    int i;

    if (n <= 3)
	return 1;
    xbar = ybar = 0.0;
    SXX = SYY = SXY = 0.0;
    for (i = 0; i < n; i++) {
	xbar = xbar + x[i];
	ybar = ybar + y[i];
    }
    xbar = xbar / n;
    ybar = ybar / n;
    for (i = 0; i < n; i++) {
	SXX = SXX + (x[i] - xbar) * (x[i] - xbar);
	SYY = SYY + (y[i] - ybar) * (y[i] - ybar);
	SXY = SXY + (x[i] - xbar) * (y[i] - ybar);
    }
    sdx = sqrt(SXX / (n - 1));
    sdy = sqrt(SYY / (n - 1));
    if (sdx == 0.0)
	return 2;
    if (sdy == 0.0)
	return 2;
    sxy = SXY / (n - 1);
    rxy = sxy / (sdx * sdy);
    slope = SXY / SXX;
    intercept = ybar - slope * xbar;
    RSS = SYY - slope * SXY;
    rms = RSS / (n - 2);
    sereg = sqrt(RSS / (n - 2));
    seintercept = sqrt(rms * (1.0 / n + xbar * xbar / SXX));
    seslope = sqrt(rms / SXX);
    SSreg = SYY - RSS;
    F = SSreg / rms;
    R2 = SSreg / SYY;
    sprintf(buf, "\nNumber of observations = %10d\n\
Mean of independent variable       = %.7g\n\
Mean of dependent variable         = %.7g\n\
Standard dev. of ind. variable     = %.7g\n\
Standard dev. of dep. variabl e = %.7g\n\
Correlation coefficient = %.7g\n\
Regression coefficient(slope) = %.7g\n\
Standard error of coefficient = %.7g\n\
t - value for coefficient = %.7g\n\
Regression constant(intercept) = %.7g\n\
Standard error of constant = %.7g\n\
t - value for constant = %.7g\n\
\nAnalysis of variance\n\
Source d.f Sum of squares Mean Square F\n\
Regression 1 %.7g %.7g %.7g\n\
Residual %5d %.7g %.7g\n\
Total %5d %.7g\n\n", n, xbar, ybar, sdx, sdy, rxy, slope, seslope, slope / seslope, intercept, seintercept, intercept / seintercept, SSreg, SSreg, F, n - 2, RSS, RSS / (n - 2), n - 1, SYY);
    stufftext(buf);
    for (i = 0; i < n; i++) {
	fitted[i] = slope * x[i] + intercept;
    }
    return 0;
}

/*
	a literal translation of the spline routine in
	Forsyth, Malcolm, and Moler
*/
void spline(n, x, y, b, c, d)
    int n;
    double *x, *y, *b, *c, *d;
{
/*
c
c  the coefficients b(i), c(i), and d(i), i=1,2,...,n are computed
c  for a cubic interpolating spline
c
c    s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
c
c    for  x(i) .le. x .le. x(i+1)
c
c  input..
c
c    n = the number of data points or knots (n.ge.2)
c    x = the abscissas of the knots in strictly increasing order
c    y = the ordinates of the knots
c
c  output..
c
c    b, c, d  = arrays of spline coefficients as defined above.
c
c  using  p  to denote differentiation,
c
c    y(i) = s(x(i))
c    b(i) = sp(x(i))
c    c(i) = spp(x(i))/2
c    d(i) = sppp(x(i))/6  (derivative from the right)
c
c  the accompanying function subprogram  seval	can be used
c  to evaluate the spline.
c
c
*/

    int nm1, ib, i;
    double t;

/*
Gack!
*/
    x--;
    y--;
    b--;
    c--;
    d--;

/*
Fortran 66
*/
    nm1 = n - 1;
    if (n < 2)
	return;
    if (n < 3)
	goto l50;
/*
c
c  set up tridiagonal system
c
c  b = diagonal, d = offdiagonal, c = right hand side.
c
*/
    d[1] = x[2] - x[1];
    c[2] = (y[2] - y[1]) / d[1];
    for (i = 2; i <= nm1; i++) {
	d[i] = x[i + 1] - x[i];
	b[i] = 2.0 * (d[i - 1] + d[i]);
	c[i + 1] = (y[i + 1] - y[i]) / d[i];
	c[i] = c[i + 1] - c[i];
    }
/*
c
c  end conditions.  third derivatives at  x(1)	and  x(n)
c  obtained from divided differences
c
*/
    b[1] = -d[1];
    b[n] = -d[n - 1];
    c[1] = 0.0;
    c[n] = 0.0;
    if (n == 3)
	goto l15;
    c[1] = c[3] / (x[4] - x[2]) - c[2] / (x[3] - x[1]);
    c[n] = c[n - 1] / (x[n] - x[n - 2]) - c[n - 2] / (x[n - 1] - x[n - 3]);
    c[1] = c[1] * d[1] * d[1] / (x[4] - x[1]);
    c[n] = -c[n] * d[n - 1] * d[n - 1] / (x[n] - x[n - 3]);
/*
c
c  forward elimination
c
*/
l15:;
    for (i = 2; i <= n; i++) {
	t = d[i - 1] / b[i - 1];
	b[i] = b[i] - t * d[i - 1];
	c[i] = c[i] - t * c[i - 1];
    }
/*
c
c  back substitution
c
*/
    c[n] = c[n] / b[n];
    for (ib = 1; ib <= nm1; ib++) {
	i = n - ib;
	c[i] = (c[i] - d[i] * c[i + 1]) / b[i];
    }
/*
c
c  c(i) is now the sigma(i) of the text
c
c  compute polynomial coefficients
c
*/
    b[n] = (y[n] - y[nm1]) / d[nm1] + d[nm1] * (c[nm1] + 2.0 * c[n]);
    for (i = 1; i <= nm1; i++) {
	b[i] = (y[i + 1] - y[i]) / d[i] - d[i] * (c[i + 1] + 2.0 * c[i]);
	d[i] = (c[i + 1] - c[i]) / d[i];
	c[i] = 3.0 * c[i];
    }
    c[n] = 3.0 * c[n];
    d[n] = d[n - 1];
    return;

l50:;
    b[1] = (y[2] - y[1]) / (x[2] - x[1]);
    c[1] = 0.0;
    d[1] = 0.0;
    b[2] = b[1];
    c[2] = 0.0;
    d[2] = 0.0;
    return;
}

double seval(n, u, x, y, b, c, d)
    int n;
    double u, x[], y[], b[], c[], d[];

{
/*
c
c  this subroutine evaluates the cubic spline function
c
c    seval = y(i) + b(i)*(u-x(i)) + c(i)*(u-x(i))**2 + d(i)*(u-x(i))**3
c
c    where  x(i) .lt. u .lt. x(i+1), using horner's rule
c
c  if  u .lt. x(1) then  i = 1	is used.
c  if  u .ge. x(n) then  i = n	is used.
c
c  input..
c
c    n = the number of data points
c    u = the abscissa at which the spline is to be evaluated
c    x,y = the arrays of data abscissas and ordinates
c    b,c,d = arrays of spline coefficients computed by spline
c
c  if  u  is not in the same interval as the previous call, then a
c  binary search is performed to determine the proper interval.
c
*/
    int j, k;
    double dx;
    int i;

/*
c
c  binary search
c
*/
    if (u < x[0]) {
	i = 0;
    } else if (u >= x[n - 1]) {
	i = n - 1;
    } else {
	i = 0;
	j = n;
l20:	;
	k = (i + j) / 2;
	if (u < x[k])
	    j = k;
	if (u >= x[k])
	    i = k;
	if (j > i + 1)
	    goto l20;
    }
/*
c
c  evaluate spline
c
*/
l30:;
    dx = u - x[i];
    return (y[i] + dx * (b[i] + dx * (c[i] + dx * d[i])));
}
