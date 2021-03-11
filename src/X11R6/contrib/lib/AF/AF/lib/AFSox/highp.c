#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/highp.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * Sound Tools High-Pass effect file.
 *
 * Algorithm:  1nd order filter.
 * From Fugue source code:
 *
 * 	output[N] = B * (output[N-1] - input[N-1] + input[N])
 *
 * 	A = 2.0 * pi * center
 * 	B = exp(-A / frequency)
 */

#include <math.h>
#include "st.h"

/* Private data for Highpass effect */
typedef struct highpstuff {
	float	center;
	double	A, B;
	double	in1, out1;
} *highp_t;

/*
 * Process options
 */
highp_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	highp_t highp = (highp_t) effp->priv;

	if ((n < 1) || !sscanf(argv[0], "%f", &highp->center))
		fatal(ft, "Usage: highp center");
}

/*
 * Prepare processing.
 */
highp_start(ft)
ft_t ft;
{
	highp_t highp = (highp_t) ft->eff.priv;
	if (highp->center > ft->info.rate*2)
		fatal(ft, "Highpass: center must be < minimum data rate*2\n");
	
	highp->A = (M_PI * 2.0 * highp->center) / ft->info.rate;
	highp->B = exp(-highp->A / ft->info.rate);
	highp->in1 = 0.0;
	highp->out1 = 0.0;
}

/*
 * Processed signed long samples from ibuf to obuf.
 * Return number of samples processed.
 */

highp_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	eff_t effp = &ft->eff;
	highp_t highp = (highp_t) effp->priv;
	int len, done;
	double d;
	long l;

	len = ((*isamp > *osamp) ? *osamp : *isamp);
	d = highp->out1;

	/* yeah yeah yeah registers & integer arithmetic yeah yeah yeah */
	for(done = 0; done < len; done++) {
		l = *ibuf++;
		d = (highp->B * ((d - highp->in1) + (double) l)) / 65536.0;
		d *= 0.8;
		if (d > 32767)
			d = 32767;
		if (d < - 32767)
			d = - 32767;
		highp->in1 = l;
		*obuf++ = d * 65536;
	}
	highp->out1 = d;
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 */
highp_stop(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	/* nothing to do */
}

