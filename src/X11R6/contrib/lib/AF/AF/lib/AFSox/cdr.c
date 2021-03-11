#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/cdr.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * CD-R format handler
 *
 * David Elliott, Sony Microsystems
 *
 * This code automatically handles endianness differences
 */

#include "st.h"

#define SECTORSIZE	(2352 / 2)

/* Private data for SKEL file */
typedef struct cdrstuff {
	int	samples;	/* number of samples written */
} *cdr_t;

IMPORT float volume, amplitude;
IMPORT int summary, verbose;

/*
 * Do anything required before you start reading samples.
 * Read file header. 
 *	Find out sampling rate, 
 *	size and style of samples, 
 *	mono/stereo/quad.
 */

int
cdrstartread(ft) 
ft_t ft;
{

	int     littlendian = 1;
	char    *endptr;

	endptr = (char *) &littlendian;
	if (!*endptr) ft->swap = 1;

	ft->info.rate = 44100;
	ft->info.size = WORD;
	ft->info.style = SIGN2;
	ft->info.channels = 2;
	ft->comment = NULL;
	return AF_SUCCESS;
}

/*
 * Read up to len samples from file.
 * Convert to signed longs.
 * Place in buf[].
 * Return number of samples read.
 */

cdrread(ft, buf, len) 
ft_t ft;
long *buf, len;
{

	return rawread(ft, buf, len);
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 */
cdrstopread(ft) 
ft_t ft;
{
}

cdrstartwrite(ft) 
ft_t ft;
{
	cdr_t cdr = (cdr_t) ft->priv;

	int     littlendian = 1;
	char    *endptr;

	endptr = (char *) &littlendian;
	if (!*endptr) ft->swap = 1;

	cdr->samples = 0;

	ft->info.rate = 44100;
	ft->info.size = WORD;
	ft->info.style = SIGN2;
	ft->info.channels = 2;
	
}

cdrwrite(ft, buf, len) 
ft_t ft;
long *buf, len;
{
	cdr_t cdr = (cdr_t) ft->priv;

	cdr->samples += len;

	rawwrite(ft, buf, len);
}

/*
 * A CD-R file needs to be padded to SECTORSIZE, which is in terms of
 * samples.  We write -32768 for each sample to pad it out.
 */

cdrstopwrite(ft) 
ft_t ft;
{
	cdr_t cdr = (cdr_t) ft->priv;
	int padsamps = SECTORSIZE - (cdr->samples % SECTORSIZE);
	short pad[2];

	pad[0] = 0;

	if (padsamps == SECTORSIZE) {
		return;
	}

	while (padsamps > 0) {
		rawwrite(ft, pad, 1);
		padsamps--;
	}
}

