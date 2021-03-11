#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/lib/AFSox/RCS/dyn.c,v 1.3 1994/04/04 12:47:21 tml Exp $";
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
 * Sound Tools dynamic compander effect file.
 *
 * Compresses or expands dynamic range, i.e. range between
 * soft and loud sounds.  U-law compression basically does this.
 *
 * Doesn't work.  Giving up for now.
 */

#include <math.h>
#include "st.h"

/* Private data for DYN.C file */
typedef struct dyn{
	int	rest;			/* bytes remaining in current block */
} *dyn_t;

/*
 * Process options
 */
dyn_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	if (n)
	{
		fail(ft, "Copy effect takes no options.");
		exit(-1);
	}
}

/*
 * Prepare processing.
 */
dyn_start(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	/* nothing to do */
	/* stuff data into delaying effects here */
}

/*
 * Processed signed long samples from ibuf to obuf.
 * Return number of samples processed.
 */

dyn_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	eff_t effp = &ft->eff;
	dyn_t dyn = (dyn_t) effp->priv;
	int len, done;
	
	char c;
	unsigned char uc;
	short s;
	unsigned short us;
	long l;
	unsigned long ul;
	float f;
	double d, tmp;
	int sign;

#define NORMIT (65536.0)

	len = ((*isamp > *osamp) ? *osamp : *isamp);
	for(done = 0; done < len; done++) {

		d = *ibuf++;
		if (d == 0.0)
			l = 0;
		else {
			if (d < 0.0) {
				d *= -1.0;
				sign = -1;
			} else
				sign = 1;
			d /= NORMIT;
			tmp = log10(d);
			tmp = pow(8.0, tmp);
			tmp = tmp * NORMIT;
			l = tmp * sign;
		}
		*obuf++ = l;
	}
}

/*
 * Drain out remaining samples if the effect generates any.
 */

dyn_drain(ft, obuf, osamp)
ft_t ft;
long *obuf;
int *osamp;
{
	eff_t effp = &ft->eff;
	*osamp = 0;
}

/*
 * Do anything required when you stop reading samples.  
 *	(free allocated memory, etc.)
 */
dyn_stop(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	/* nothing to do */
}


