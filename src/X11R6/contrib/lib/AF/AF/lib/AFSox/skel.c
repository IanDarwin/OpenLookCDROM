#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/skel.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * Sound Tools skeleton file format driver.
 */

#include "st.h"

/* Private data for SKEL file */
typedef struct skelstuff {
	int	rest;			/* bytes remaining in current block */
} *skel_t;

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
skelstartread(ft) 
ft_t ft;
{
	skel_t sk = (skel_t) ft->priv;

	/* If you need to seek around the input file. */
	if (! ft->seekable)
		return fail(ft, "SKEL input file must be a file, not a pipe");

	/*
	 * If your format specifies or your file header contains
	 * any of the following information. 
	 */
	ft->info.rate = 
	ft->info.size = BYTE or WORD ...;
	ft->info.style = UNSIGNED or SIGN2 ...;
	ft->info.channels = 1 or 2 or 4;
	ft->comment = any comment in file header.
	return AF_SUCCESS;
}

/*
 * Read up to len samples from file.
 * Convert to signed longs.
 * Place in buf[].
 * Return number of samples read.
 */

skelread(ft, buf, len) 
ft_t ft;
long *buf, len;
{
	skel_t sk = (skel_t) ft->priv;
	int abs;
	float amp;
	int done = 0;
	
	char c;
	unsigned char uc;
	short s;
	unsigned short us;
	long l;
	unsigned long ul;
	float f;
	double d;

	for(; done < len; done++) {
		if no more samples
			break
		get a sample
		l = sample converted to signed long
		*buf++ = l;
	}
	return done;
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 */
skelstopread(ft) 
ft_t ft;
{
}

skelstartwrite(ft) 
ft_t ft;
{
	skel_t sk = (skel_t) ft->priv;

	/* If you have to seek around the output file */
	if (! ft->seekable)
		return fail(ft, "Output .skel file must be a file, not a pipe");

	/* If your format specifies any of the following info. */
	ft->info.rate = 
	ft->info.size = BYTE or WORD ...;
	ft->info.style = UNSIGNED or SIGN2 ...;
	ft->info.channels = 1 or 2 or 4;
	/* Write file header, if any */
	/* Write comment field, if any */
	
}

skelwrite(ft, buf, len) 
ft_t ft;
long *buf, len;
{
	skel_t sk = (skel_t) ft->priv;
	register int datum;
	int abs;
	int done = 0;

	while(len--)
		putc((*buf++ >> 24) ^ 0x80, ft->fp);
	/* If you cannot write out all of the supplied samples, */
	/*	fail(ft, "SKEL: Can't write all samples to %s", ft->filename); */
	
}

skelstopwrite(ft) 
ft_t ft;
{
	/* All samples are already written out. */
	/* If file header needs fixing up, for example it needs the */
 	/* the number of samples in a field, seek back and write them here. */
}

