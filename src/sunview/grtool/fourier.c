/*
	DFT by definition

	$Header: fourier.c,v 1.1 89/07/19 22:41:08 pturner Locked $
*/
#include	<math.h>

int fouriercoef(y, sx, sy, len)
    double y[], sx[], sy[];
int len;

{
    int i, j, itest;
    double sum, pi2;

    itest = len % 2;
    pi2 = M_PI * 2.0;
    sum = 0.0;
    for (i = 0; i < len; i++)
	sum = sum + y[i];
    sx[0] = sum / len;
    sy[0] = 0.0;
    for (j = 1; j < len / 2; j++) {
	sx[j] = 0.0;
	sy[j] = 0.0;
	for (i = 0; i < len; i++) {
	    sx[j] = sx[j] + y[i] * cos(j * pi2 / len * i);
	    sy[j] = sy[j] + y[i] * sin(j * pi2 / len * i);
	}
	sx[j] = sx[j] * 2.0 / len;
	sy[j] = sy[j] * 2.0 / len;
    }
    if (!itest) {
	sy[len / 2] = 0.0;
	sx[len / 2] = 0.0;
	for (i = 0; i < len; i++) {
	    sx[len / 2] = sx[len / 2] + y[i] / len * pow(-1.0, 1.0 * i);
	}
    }
    if (itest)
	return len / 2;
    return len / 2 + 1;
}

/*
   this came off the net - if you wrote it let me know and I'll
   give you credit - PJT
*/
/*
   real_data ... ptr. to real part of data to be transformed
   imag_data ... ptr. to imag  "   "   "   "  "      "
   inv ..... Switch to flag normal or inverse transform
   n_pts ... Number of real data points
   nu ...... logarithm in base 2 of n_pts e.g. nu = 5 if n_pts = 32.
*/

fft(real_data, imag_data, n_pts, nu, inv)
    double *real_data, *imag_data;
    int n_pts, nu, inv;
{
    int n2, j, l, i, ib, k, k1, k2;
    int sgn;
    double tr, ti, arg, nu1;	/* intermediate values in calcs. */
    double c, s;		/* cosine & sine components of Fourier trans. */
    double fac;

    n2 = n_pts / 2;
    nu1 = nu - 1.0;
    k = 0;
/*
* sign change for inverse transform
*/
    sgn = inv ? -1 : 1;
/*
* Calculate the componets of the Fourier series of the function
*/
    for (l = 0; l != nu; l++) {
	do {
	    for (i = 0; i != n2; i++) {
		j = k / (pow(2.0, nu1));
		ib = bit_swap(j, nu);
		arg = 2.0 * M_PI * ib / n_pts;
		c = cos(arg);
		s = sgn * sin(arg);
		k1 = k;
		k2 = k1 + n2;
		tr = *(real_data + k2) * c + *(imag_data + k2) * s;
		ti = *(imag_data + k2) * c - *(real_data + k2) * s;
		*(real_data + k2) = *(real_data + k1) - tr;
		*(imag_data + k2) = *(imag_data + k1) - ti;
		*(real_data + k1) = *(real_data + k1) + tr;
		*(imag_data + k1) = *(imag_data + k1) + ti;
		k++;
	    }
	    k += n2;
	} while (k < n_pts - 1);
	k = 0;
	nu1 -= 1.0;
	n2 /= 2;
    }
    for (k = 0; k != n_pts; k++) {
	ib = bit_swap(k, nu);
	if (ib > k) {
	    sswap((real_data + k), (real_data + ib));
	    sswap((imag_data + k), (imag_data + ib));
	}
    }
/*
* If calculating the inverse transform, must divide the data by the number of
* data points.
*/
    if (inv)
	fac = 2.0 / n_pts;
    else
	fac = 0.5;
    for (k = 0; k != n_pts; k++) {
	*(real_data + k) *= fac;
	*(imag_data + k) *= fac;
    }
}

/*
* Bit swaping routine in which the bit pattern of the integer i is reordered.
* See Brigham's book for details
*/
bit_swap(i, nu)
    int i, nu;
{
    int ib, i1, i2;

    ib = 0;

    for (i1 = 0; i1 != nu; i1++) {
	i2 = i / 2;
	ib = ib * 2 + i - 2 * i2;
	i = i2;
    }
    return (ib);
}

/*
* Simple exchange routine where *x1 & *x2 are swapped
*/
sswap(x1, x2)
    double *x1, *x2;
{
    double temp_x;

    temp_x = *x1;
    *x1 = *x2;
    *x2 = temp_x;
}
