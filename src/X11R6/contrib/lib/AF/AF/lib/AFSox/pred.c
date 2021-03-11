#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/pred.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * Sound Tools prediction-correction compression effect file.
 * Experiment with various simple equation systems.
 * 
 * This is not ready for prime time.  It's here for research purposes.
 * Sox will hang if you run this as is.  Define D0 or D1, recompile,
 * and try compressing the output with 'compress' and 'pack'.  
 *
 * Inspired by 2D PC gem in Graphics Gems II.
 */

#include "st.h"

#define	D0	/* output difference between successive samples */
/* #define	D1	/* guess by extending slope of last two samples */
/* #define	D2 	/* extend second derivate and guess signal turn */
/* Autocorrelation isn't worth pursuing.  D2 should do an excellent job */

/* Private data for Prediction-Correction state machine */
typedef struct predstuff {
	int	direction;		/* 0 for compress, 1 for decompress */
	int	first;			/* first time through? */
	u_i	error;			/* average error output */
	int	clipped;		/* # of clipped error values */
#ifdef	D0 
	long	in[1];			/* previous input sample */
#endif
#ifdef	D1
	long	in[2];			/* previous input samples */
#endif
} *pred_t;

long pred_init(), pred_ict(), pred_next();

/*
 * Process options
 */
pred_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	pred_t pred = (pred_t) effp->priv;

	if ((n != 1) || 
		    (strcmp(argv[0], "-c") && strcmp(argv[0], "-d")))
		fatal(ft, "Linp compression requires in or out options.");

	pred->direction = strcmp(argv[0], "-c");
}

/*
 * Start processing
 */
pred_start(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	pred_t pred = (pred_t) effp->priv;

	pred->error = 0;
	pred->first = 1;
	pred->clipped = 0;
}

/*
 * Process according to compression direction.
 * Both loops use the same state machine, 
 * but feed it from different streams.
 */
/*
 * If first time, emit first two samples.
 * Then,
 */

pred_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	eff_t effp = &ft->eff;
	int len, done;
	pred_t pred = (pred_t) effp->priv;
	register long predict, error;
	
	done = 0;
	if (pred->first) {
		done = pred_init(ft, ibuf, obuf);
		ibuf += done;
		obuf += done;
		pred->first = 0;
	}
	len = ((*isamp > *osamp) ? *osamp : *isamp);
	if (done > len)		/* it can't happen here */
		fatal(ft, "Prediction effect: not enough samples?");
	if (pred->direction) {			/* decompress */
		for(; done < len; done++) {
			/* reconstitute sample from prediction and error */
			predict = pred_ict(ft);
			error = *ibuf;
			pred_next(ft, predict + error);
			pred->error = pred->error/2 + abs(error)/2;
			*obuf++ = predict + error;
			ibuf++;
		}
	} else {				/* compress */
		for(; done < len; done++) {
			/* generate sample from prediction and error */
			predict = pred_ict(ft);
			error = *ibuf - predict;
			pred->error = pred->error/2 + abs(error)/2;
			if (predict + error != *ibuf)
				pred->clipped++;
			pred_next(ft, *ibuf);
			ibuf++;
			*obuf++ = error;
		}
	}
}

/* 
 * Linear Prediction state machine part A.
 * 
 * Initialize from buffer.  Return number of samples processed.
 * It will be the same for input and output streams.
 */
long
pred_init(ft, ibuf, obuf)
ft_t ft;
long *ibuf, *obuf;
{
	eff_t effp = &ft->eff;
	pred_t pred = (pred_t) effp->priv;

	/* 
	 * This is bogus!  
	 * Just pretend samples in negative time are 0, make a first few
	 * weird guesses.
	 */
#ifdef	D0
	/* same for compress and decompress */
	pred->in[0] = *obuf++ = *ibuf++;
	return 1;
#endif
#ifdef	D1
	/* same for compress and decompress */
	pred->in[0] = *obuf++ = *ibuf++;
	pred->in[1] = *obuf++ = *ibuf++;
	return 2;
#endif
}

/* 
 * Linear Prediction state machine part B.
 * 
 * Emit a predicted sample.
 */
long
pred_ict(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	pred_t pred = (pred_t) effp->priv;

#ifdef	D1
	avg = (pred->in[0]/2 + pred->in[1]/2);
	return pred->in[1] + (pred->in[1] - avg);
#endif
#ifdef	D0
	/* Assume flat data */
	return pred->in[0];
#endif
}

/* 
 * Linear Prediction state machine, part C.
 * 
 * Process next sample.
 */
long
pred_next(ft, samp)
ft_t ft;
long samp;
{
	eff_t effp = &ft->eff;
	pred_t pred = (pred_t) effp->priv;

#ifdef	D1
	pred->in[0] = pred->in[1];
	pred->in[1] = samp;
#endif
#ifdef	D0
	/* Assume flat data */
	pred->in[0] = samp;
#endif
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 */
pred_stop(ft)
ft_t ft;
{
	pred_t pred = (pred_t) ft->eff.priv;
	int error;
	int size;

	/* XXX Or should it always be the input size? */
	if (pred->direction)
		size = ft->info.size;
	else
		size = ft->outfo.size;
	switch(size) {
		case WORD:
			error = pred->error / ((long)1 << 16);
			break;
		case BYTE:
			error = pred->error / ((long)1 << 24);
			break;
		default:
			error = pred->error;
			break;
	}
	/* nothing to do */
	fprintf(stderr, "Prediction\n\tAverage Error outputs: %d\n", error);
	fprintf(stderr, "\tClipped error outputs: %d\n", pred->clipped);
}





