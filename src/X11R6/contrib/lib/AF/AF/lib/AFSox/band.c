#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/band.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
#endif /* RCS_ID */
#endif /* LINT */
/***********************************************************
$Copyright$,1994 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

/*
 * Sound Tools Bandpass effect file.
 *
 * Algorithm:  2nd order recursive filter.
 * Formula stolen from MUSIC56K, a toolkit of 56000 assembler stuff.
 * Quote:
 *   This is a 2nd order recursive band pass filter of the form.                
 *   y(n)= a * x(n) - b * y(n-1) - c * y(n-2)   
 *   where :    
 *        x(n) = "IN"           
 *        "OUT" = y(n)          
 *        c = EXP(-2*pi*cBW/S_RATE)             
 *        b = -4*c/(1+c)*COS(2*pi*cCF/S_RATE)   
 *   if cSCL=2 (i.e. noise input)               
 *        a = SQT(((1+c)*(1+c)-b*b)*(1-c)/(1+c))                
 *   else       
 *        a = SQT(1-b*b/(4*c))*(1-c)            
 *   endif      
 *   note :     cCF is the center frequency in Hertz            
 *        cBW is the band width in Hertz        
 *        cSCL is a scale factor, use 1 for pitched sounds      
 *   use 2 for noise.           
 */

#include <math.h>
#include "st.h"

/* Private data for Bandpass effect */
typedef struct bandstuff {
	float	center;
	float	width;
	double	A, B, C;
	double	out1, out2;
	short	noise;
	/* 50 bytes of data, 52 bytes long for allocation purposes. */
} *band_t;

/*
 * Process options
 */
band_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	band_t band = (band_t) effp->priv;

	band->noise = 0;
	if (n > 0 && !strcmp(argv[0], "-n")) {
		band->noise = 1;
		n--;
		argv++;
	}
	if ((n < 1) || !sscanf(argv[0], "%f", &band->center))
	{
		fail(ft, "Usage: band [ -n ] center [ width ]");
		exit(-1);
	}
	band->width = band->center / 2;
	if ((n >= 2) && !sscanf(argv[1], "%f", &band->width))
	{
		fail(ft, "Usage: band [ -n ] center [ width ]");
		exit(-1);
	}
}

/*
 * Prepare processing.
 */
band_start(ft)
ft_t ft;
{
	band_t band = (band_t) ft->eff.priv;
	if (band->center > ft->info.rate/2)
	{
		fail(ft, "Band: center must be < minimum data rate/2\n");
		exit (-1);
	}

	band->C = exp(-2*M_PI*band->width/ft->info.rate);
	band->B = -4*band->C/(1+band->C)*
		cos(2*M_PI*band->center/ft->info.rate);
	if (band->noise)
		band->A = sqrt(((1+band->C)*(1+band->C)-band->B *
			band->B)*(1-band->C)/(1+band->C));
	else
		band->A = sqrt(1-band->B*band->B/(4*band->C))*(1-band->C);
	band->out1 = band->out2 = 0.0;
}

/*
 * Processed signed long samples from ibuf to obuf.
 * Return number of samples processed.
 */

band_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	eff_t effp = &ft->eff;
	band_t band = (band_t) effp->priv;
	int len, done;
	double d;
	long l;

	len = ((*isamp > *osamp) ? *osamp : *isamp);

	/* yeah yeah yeah registers & integer arithmetic yeah yeah yeah */
	for(done = 0; done < len; done++) {
		l = *ibuf++;
		d = (band->A * l - band->B * band->out1) - band->C * band->out2;
		band->out2 = band->out1;
		band->out1 = d;
		*obuf++ = d;
	}
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 */
band_stop(ft)
ft_t ft;
{
	/* nothing to do */
}

