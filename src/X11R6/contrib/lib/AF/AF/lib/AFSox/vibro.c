#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/vibro.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * Sound Tools Vibro effect file.
 *
 * Modeled on world-famous Fender(TM) Amp Vibro knobs.
 * 
 * Algorithm: generate a sine wave ranging from
 * 0 + depth to 1.0, where signal goes from -1.0 to 1.0.
 * Multiply signal with sine wave.  I think.
 */

#include <math.h>
#include "st.h"

/* Private data for Vibro effect */
typedef struct vibrostuff {
	float 		speed;
	float 		depth;
	short		*sinetab;		/* sine wave to apply */
	int		mult;			/* multiplier */
	unsigned	length;			/* length of table */
	int		counter;		/* current counter */
} *vibro_t;

/*
 * Process options
 */
vibro_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	vibro_t vibro = (vibro_t) effp->priv;

	vibro->depth = 0.5;
	if ((n == 0) || !sscanf(argv[0], "%f", &vibro->speed) ||
		((n == 2) && !sscanf(argv[1], "%f", &vibro->depth)))
		fatal(ft, "Usage: vibro speed [ depth ]");
	if ((vibro->speed <= 0.001) || (vibro->speed > 30.0) || 
			(vibro->depth < 0.0) || (vibro->depth > 1.0))
		fatal(ft, "Vibro: speed must be < 30.0, 0.0 < depth < 1.0");
}

/*
 * Prepare processing.
 */
vibro_start(ft)
ft_t ft;
{
	vibro_t vibro = (vibro_t) ft->eff.priv;

	vibro->length = ft->info.rate / vibro->speed;
	if (! (vibro->sinetab = (short*) malloc(vibro->length * sizeof(short))))
		fatal(ft, "Vibro: Cannot malloc %d bytes",
			vibro->length * sizeof(short));

	sine(vibro->sinetab, vibro->length, vibro->depth);
	vibro->counter = 0;
}

/*
 * Processed signed long samples from ibuf to obuf.
 * Return number of samples processed.
 */

vibro_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	eff_t effp = &ft->eff;
	vibro_t vibro = (vibro_t) effp->priv;
	register counter, tablen;
	int len, done;
	register short *sinetab;
	long l;

	len = ((*isamp > *osamp) ? *osamp : *isamp);

	sinetab = vibro->sinetab;
	counter = vibro->counter;
	tablen = vibro->length;
	for(done = 0; done < len; done++) {
		l = *ibuf++;
		/* 24x8 gives 32-bit result */
		*obuf++ = ((l / 256) * sinetab[counter++ % tablen]);
	}
	vibro->counter = counter;
	/* processed all samples */
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 */
vibro_stop(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	/* nothing to do */
}

/* This was very painful.  We need a sine library. */

sine(buf, len, depth)
short *buf;
int len;
float depth;
{
	int i;
	int scale = depth * 128;
	int base = (1.0 - depth) * 128;
	double val;

	for (i = 0; i < len; i++) {
		val = sin((float)i/(float)len * 2.0 * M_PI);
		buf[i] = (val + 1.0) * scale + base * 2;
	}
/*
	for (i = 0; i < len; i++)
		fprintf(stderr, "%d\n", buf[i]);
*/
}




