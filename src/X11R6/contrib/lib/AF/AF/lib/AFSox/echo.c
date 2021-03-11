#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/echo.c,v 1.6 1994/02/02 01:22:51 marvinw Exp $";
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
** 	Echo effect. based on:
**
** echoplex.c - echo generator
**
** Copyright (C) 1989 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

/*
 * Sound Tools echo effect file.
 */

#ifdef	AMIGA
#include <stdlib.h> /* Harmless, and prototypes atof() etc. --dgc */
#endif
#include <math.h>
#include "st.h"

#define FADE_THRESH 10
#define MYBUFSIZ 256
#define DELAYBUFSIZ ( 50L * MAXRATE )
#define MAXDELAYS 30

struct echoplex {
	int	counter;			
	int	numdelays;
	long	*delaybuf;
	float	delay[MAXDELAYS], atten[MAXDELAYS];
	long	samples[MAXDELAYS], maxsamples;
	long	pl, ppl, pppl;
};

/* Private data for SKEL file */
typedef struct echostuff {
	struct	echoplex *echoplex;
} *echo_t;

#ifndef abs
#define abs(a) ((a) >= 0 ? (a) : -(a))
#endif

long clip24();

IMPORT writing;

/*
 * Process options
 */
echo_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	echo_t echo = (echo_t) effp->priv;
	struct echoplex *e;
	int i;

	if (!(echo->echoplex=(struct echoplex*)malloc(sizeof(struct echoplex))))
	{
		fatal(ft, "Echo: Cannot malloc %d bytes\n",sizeof(struct echoplex));
		exit(-1);
	}
	e = echo->echoplex;
	e->numdelays = 0;
	e->maxsamples = 0;

	if ((n == 0) || (n & 1))
	{
	    fatal(ft, "Usage: echo delay attenuation [ delay attenuation ... ]");
	    exit(-1);
	}

	i = 0;
	while (i < n) {
		/* Linux bug and it's cleaner. */
		sscanf(argv[i++], "%f", &e->delay[e->numdelays]);
		sscanf(argv[i++], "%f", &e->atten[e->numdelays]);

		e->numdelays++;
	}
}

/*
 * Prepare for processing.
 */
echo_start(ft)
ft_t ft;
{
	echo_t echo = (echo_t) ft->eff.priv;
	struct	echoplex *e = echo->echoplex;
	int i;

	for(i = 0; i < e->numdelays; i++) {
		e->samples[i] = e->delay[i] * ft->info.rate;
		if ( e->samples[i] < 1 )
		    fatal(ft, " delay must positive, aye!");
		if ( e->samples[i] > DELAYBUFSIZ )
			fatal(ft, "Echo: delay must be less than %g seconds",
				DELAYBUFSIZ / (float) ft->info.rate );
		if ( e->atten[i] < 0.0 )
		    fatal(ft, "attenuation must positive, aye!\n" );
		if ( e->samples[i] > e->maxsamples )
		    e->maxsamples = e->samples[i];
	}
	if (! (e->delaybuf = (long *) malloc(sizeof (long) * e->maxsamples)))
		fatal(ft, "Echo: Cannot malloc %d bytes\n", 
			sizeof(long) * e->maxsamples);
	for ( i = 0; i < e->maxsamples; ++i )
		e->delaybuf[i] = 0;
	e->pppl = e->ppl = e->pl = 0x7fffff;		/* fade-outs */
	e->counter = 0;
}

/*
 * Processed signed long samples from ibuf to obuf.
 * Return number of samples processed.
 */

echo_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	eff_t effp = &ft->eff;
	echo_t echo = (echo_t) effp->priv;
	struct	echoplex *e = echo->echoplex;
	int len, done;
	int i, j;
	
	long l;

	i = e->counter;
	len = ((*isamp > *osamp) ? *osamp : *isamp);
	for(done = 0; done < len; done++) {
		/* Store delays as 24-bit signed longs */
		l = *ibuf++ / 256;
		for ( j = 0; j < e->numdelays; ++j )
			l = l + 
e->delaybuf[( i + e->maxsamples - e->samples[j]) % e->maxsamples] * e->atten[j];
		l = clip24(l);
		e->delaybuf[i] = l;
		*obuf++ = l * 256;
		i++;		/* XXX need a % maxsamples here ? */
		i %= e->maxsamples;
	}
	e->counter = i;
	/* processed all samples */
}

/*
 * Drain out echo lines. 
 */
echo_drain(ft, obuf, osamp)
ft_t ft;
long *obuf;
long *osamp;
{
	eff_t effp = &ft->eff;
	echo_t echo = (echo_t) effp->priv;
	struct	echoplex *e = echo->echoplex;
	long l;
	int i, j, done;

	i = e->counter;
	done = 0;
	/* drain out delay samples */
	do {
		l = 0;
		for ( j = 0; j < e->numdelays; ++j )
			l += 
e->delaybuf[(i + e->maxsamples - e->samples[j]) % e->maxsamples] * e->atten[j];
		l = clip24(l);
		e->delaybuf[i] = l;
		obuf[done++] = l * 256;
		e->pppl = e->ppl;
		e->ppl = e->pl;
		e->pl = l;
		i++;		/* need a % maxsamples here ? */
		i %= e->maxsamples;
	} while((done < *osamp) && 
		((abs(e->pl) + abs(e->ppl) + abs(e->pppl)) > FADE_THRESH));
	e->counter = i;
	*osamp = done;
	/* drain will not be called again */
}

/*
 * Clean up echo effect.
 */
echo_stop(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	echo_t echo = (echo_t) effp->priv;

	free((char *) echo->echoplex);
	echo->echoplex = (struct echoplex *) -1;   /* guaranteed core dump */
}

long
clip24(l)
long l;
{
	if (l >= ((long)1 << 24))
		return ((long)1 << 24) - 1;
	else if (l <= -((long)1 << 24))
		return -((long)1 << 24) + 1;
	else
		return l;
}


