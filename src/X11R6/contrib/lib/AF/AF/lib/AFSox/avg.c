#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/avg.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * Sound Tools stereo/quad -> mono mixdown effect file.
 *
 * Does not mix up to more channels.
 *
 * What's in a center channel?
 */

#include "st.h"

/* Private data for SKEL file */
typedef struct avgstuff {
	int	mix;			/* How are we mixing it? */
} *avg_t;

#define MIX_CENTER	0
#define MIX_LEFT	1
#define MIX_RIGHT	2

/*
 * Process options
 */
avg_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	avg_t avg = (avg_t) effp->priv;

	avg->mix = MIX_CENTER;
	if (n)
		if(!strcmp(argv[0], "-l"))
			avg->mix = MIX_LEFT;
		else if (!strcmp(argv[0], "-r"))
			avg->mix = MIX_RIGHT;
		else
		{
			fail(ft, "Averaging effect takes options '-l' or '-r'.");
			exit(-1);
		}
}

/*
 * Start processing
 */
void
avg_start(ft)
ft_t ft;
{
	switch (ft->outfo.channels) {
		case 1: switch (ft->info.channels) {
			case 2: 
			case 4:
				return;
		}
		case 2: switch (ft->info.channels) {
			case 4:
				return;
		}
	}
	fail(ft, "Can't average %d channels into %d channels",
		ft->info.channels, ft->outfo.channels);
	exit (-1);
}

/*
 * Process either isamp or osamp samples, whichever is smaller.
 */

avg_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	avg_t avg = (avg_t) ft->eff.priv;
	int len, done;
	
	switch (ft->outfo.channels) {
		case 1: switch (ft->info.channels) {
			case 2:
				len = ((*isamp/2 > *osamp) ? *osamp : *isamp/2);
				for(done = 0; done < len; done++) {
					switch(avg->mix) {
					    case MIX_CENTER:
						*obuf++ = ibuf[0]/2 + ibuf[1]/2;
						break;
					    case MIX_LEFT:
						*obuf++ = ibuf[0];
						break;
					    case MIX_RIGHT:
						*obuf++ = ibuf[1];
						break;
					}
					ibuf += 2;
				}
				*isamp = len * 2;
				*osamp = len;
				break;
			case 4:
				len = ((*isamp/4 > *osamp) ? *osamp : *isamp/4);
				for(done = 0; done < len; done++) {
					*obuf++ = ibuf[0]/4 + ibuf[1]/4 +
						ibuf[2]/4 + ibuf[3]/4;
					ibuf += 4;
				}
				*isamp = len * 4;
				*osamp = len;
				break;
				
		}
		break;
		case 2: switch (ft->info.channels) {
			/*
			 * After careful inspection of CSOUND source code,
			 * I'm mildly sure the order is:
			 * 	front-left, front-right, rear-left, rear-right
			 */
			case 4:
				len = ((*isamp/2 > *osamp) ? *osamp : *isamp/2);
				len &= ~1;
				for(done = 0; done < len; done++) {
					obuf[0] = ibuf[0]/2 + ibuf[2]/2;
					obuf[1] = ibuf[1]/2 + ibuf[3]/2;
					ibuf += 4;
					obuf += 2;
				}
				*isamp = len * 2;
				*osamp = len;
				break;
		}
	}
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 *
 * Should have statistics on right, left, and output amplitudes.
 */
avg_stop(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	/* nothing to do */
}

