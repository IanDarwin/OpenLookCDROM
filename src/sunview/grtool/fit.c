/*
	fit.c - curve fitting module

	$Header: fit.c,v 1.4 89/08/27 11:55:55 pturner Locked $
*/

#include <math.h>
#include "defines.h"

/*
	simple gauss elimination - no pivoting or scaling strategies
	all matrices are sym-pos-def
*/
gauss(n, a, adim, b, x)
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
stasum(x, n, xbar, sd, flag)
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
leasqu(n, x, y, degree, w, wdim, r)
    int n, degree, wdim;
    double x[], y[], w[], r[];

{
    double b[11];
    double sumy1, sumy2, ybar, ysdev, stemp, rsqu;
    double xbar, xsdev;
    int i, j, k;
    char buf[255];

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
fitcurve(x, y, n, ideg, fitted)
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
runavg(x, y, ax, ay, n, ilen)
    double x[], y[], ax[], ay[];
int n, ilen;

{
    int i;
    double sum = 0.0;

    for (i = 0; i < ilen; i++) {
	sum = sum + y[i];
    }
    ax[0] = (x[0] + x[ilen - 1]) / 2.0;
    ay[0] = sum / ilen;
    for (i = 1; i < (n - ilen + 1); i++) {
	sum = y[i + ilen - 1] - y[i - 1] + sum;
	ay[i] = sum / ilen;
	ax[i] = (x[i] + x[i + ilen - 1]) * 0.5;
    }
}

/*
	Apply a digital filter of length len to a set in x, y,
	of length n with the results going to resx, resy.
	the length of the result is set by the caller
*/
filterser(n, x, y, resx, resy, h, len)
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
	    resx[i] = x[i + ld2 + 1];
	else
	    resx[i] = (x[i + ld2] + x[i + ld2 - 1]) / 2.0;
    }
}

/*
	linear convolution of set x (length n) with h (length m) and
	result to y. the length of y is set by the caller
*/
linearconv(x, h, y, n, m)
    double x[], h[], y[];
int n, m;

{
    int i, j, itmp;

    for (i = 0; i < n + m - 1; i++) {
	for (j = 0; j < m; j++) {
	    itmp = i - j;
	    if (!(itmp < 0) || (itmp > n - 1)) {
		y[i] = y[i] + h[j] * x[itmp];
	    }
	}
    }
}

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
    char buf[256];

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
    sprintf(buf, "Number of observations = %10d\n", n);
    stufftext(buf);
    sprintf(buf, "Mean of independent variable       = %.7g\n", xbar);
    stufftext(buf);
    sprintf(buf, "Mean of dependent variable         = %.7g\n", ybar);
    stufftext(buf);
    sprintf(buf, "Standard dev. of ind. variable     = %.7g\n", sdx);
    stufftext(buf);
    sprintf(buf, "Standard dev. of dep. variable     = %.7g\n", sdy);
    stufftext(buf);
    sprintf(buf, "Correlation coefficient            = %.7g\n", rxy);
    stufftext(buf);
    sprintf(buf, "Regression coefficient (slope)     = %.7g\n", slope);
    stufftext(buf);
    sprintf(buf, "Standard error of coefficient      = %.7g\n", seslope);
    stufftext(buf);
    sprintf(buf, "t-value for coefficient            = %.7g\n", slope / seslope);
    stufftext(buf);
    sprintf(buf, "Regression constant (intercept)    = %.7g\n", intercept);
    stufftext(buf);
    sprintf(buf, "Standard error of constant         = %.7g\n", seintercept);
    stufftext(buf);
    sprintf(buf, "t-value for constant               = %.7g\n", intercept / seintercept);
    stufftext(buf);
    sprintf(buf, "\nAnalysis of variance\n");
    stufftext(buf);
    sprintf(buf, "Source       d.f  Sum of squares  Mean Square      F\n");
    stufftext(buf);
    sprintf(buf, "Regression    1     %.7g        %.7g      %.7g\n", SSreg, SSreg, F);
    stufftext(buf);
    sprintf(buf, "Residual    %5d   %.7g        %.7g\n", n - 2, RSS, RSS / (n - 2));
    stufftext(buf);
    sprintf(buf, "Total       %5d   %.7g\n", n - 1, SYY);
    stufftext(buf);
    for (i = 0; i < n; i++) {
	fitted[i] = slope * x[i] + intercept;
    }
    return 0;
}
