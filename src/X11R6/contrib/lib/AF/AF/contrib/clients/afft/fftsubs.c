/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *		Maynard, Massachusetts
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* Fast Fourier Transform Routines */

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/fftsubs.c,v 1.3 1994/06/03 17:27:31 jg Exp $*/

#include    <stdio.h>
#include    <math.h>

/*  Set up the cosine/sine array for an FFT of points (complex) points
 *
 *  generates 2 * points entries, so that rfft will also
 *  function properly
 *
 *  Retains the size of the last table generated so clients can
 *  avoid redundant calls.  See cfft for example.
 */

static	double	*cos_sin = NULL;
static	int 	fftpoints = 0;

void fftinit (int points)

{
    register	indx;
    register	double	*wptr;
    double  	theta, divisor;

    if (cos_sin != NULL) {
    	free (cos_sin);
    }
    cos_sin = (double *) malloc (4 * points * sizeof (double));
    wptr = cos_sin;
    divisor = 2.0 * (double) points;
    for (indx = 0; indx < 4 * points; indx += 2) {
    	theta = (double) indx * M_PI / divisor;
    	*wptr++ = cos (theta);
	*wptr++ = sin (theta);
    }
    fftpoints = points;
}

/*  Complex Forward FFT
 *
 *  data: array of float item pairs - real part
 *  in even-numbered slots; imaginary part in odds.
 *
 *  points: number of pairs: power of 2
 *
 */

#define SWAP(a,b) costemp = (a);(a) = (b);(b) = costemp

void
cfft	(float *data, int points)

{
    register	i, j;
    int	    	limit, offset, gap, start, windex, wstep;
    double  	cosw, sinw;
    float   	costemp, sintemp;

    if (points != fftpoints) {
    	fftinit (points);
    }
    limit = points << 1;
    for (i = 0, j = 0; i < limit; i += 2) {
    	if (j > i) {
    	    SWAP (data[j], data[i]);
    	    SWAP (data[j+1], data[i+1]);
    	}
    	gap = limit >> 1;
    	while (gap >= 2 && j >= gap) {
    	    j -=  gap;
    	    gap >>=  1;
    	}
    	j += gap;
    }
    wstep = limit;
    for (offset = 2; offset < limit; offset <<= 1) {
    	gap = offset << 1;
	windex = 0;
	for (start = 0; start < offset; start += 2) {
	    cosw = cos_sin [windex];
	    sinw = cos_sin [windex + 1];
	    for (i = start; i < limit; i += gap) {
	    	j = i + offset;
		costemp = cosw * data [j] - sinw * data [j + 1];
		sintemp = cosw * data [j + 1] + sinw * data [j];
		data [j] = data [i] - costemp;
		data [j + 1] = data [i + 1] - sintemp;
		data [i] += costemp;
		data [i + 1] += sintemp;
	    }
	    windex += wstep;
	}
    	wstep >>= 1;
    }
}

/*
 *  Real Forward FFT
 *
 * data: array of float items
 *
 * points: number of items: power of 2
 *
 */

void
rfft	(float *data, int points)

{
    register	i, j;
    int	    	cpoints, limit, windex;
    float   	real1, real2, imag1, imag2, rtemp, itemp;
    double  	cosw, sinw;

    cpoints = points >> 1;  	    /* number of complex pairs	*/
    cfft (data, cpoints);   	    /* do complex fft	    	*/
    limit = cpoints >> 1;
    for (i = 2, windex = 2; i <= cpoints; i += 2) {
    	j = points - i;
    	cosw = cos_sin [windex++];
	sinw = cos_sin [windex++];
	real1 =  0.5 * (data [i] + data [j]);
	imag1 =  0.5 * (data [i + 1] - data [j + 1]);
	real2 =  0.5 * (data [i + 1] + data [j + 1]);
	imag2 = -0.5 * (data [i] - data [j]);
	data [i] = real1 + cosw * real2 - sinw * imag2;
	data [i + 1] = imag1 + cosw * imag2 + sinw * real2;
	data [j] = real1 - cosw * real2 + sinw * imag2;
	data [j + 1] = -imag1 + cosw * imag2 + sinw * real2;
    }
    data [0] = data [0] + data [1];
    data [1] = 0.0;
}
