#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/reverse.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * June 1, 1992
 * Copyright 1992 Guido van Rossum And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Guido van Rossum And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

/*
 * "reverse" effect, uses a temporary file created by tmpfile().
 */

#include <math.h>
#include "st.h"

IMPORT FILE *tmpfile();

#ifndef SEEK_SET
#define SEEK_SET        0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR        1
#endif
#ifndef SEEK_END
#define SEEK_END        2
#endif

/* Private data */
typedef struct reversestuff {
	FILE *fp;
	long pos;
	int phase;
} *reverse_t;

#define WRITING 0
#define READING 1

/*
 * Process options: none in our case.
 */

reverse_getopts(ft, n, argv) 
ft_t ft;
int n;
char **argv;
{
	eff_t effp = &ft->eff;
	if (n)
		fatal(ft, "Reverse effect takes no options.");
}

/*
 * Prepare processing: open temporary file.
 */

reverse_start(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	reverse_t reverse = (reverse_t) effp->priv;
	reverse->fp = tmpfile();
	if (reverse->fp == NULL)
		fatal(ft, "Reverse effect can't create temporary file\n");
	reverse->phase = WRITING;
}

/*
 * Effect flow: a degenerate case: write input samples on temporary file,
 * don't generate any output samples.
 */

reverse_flow(ft, ibuf, obuf, isamp, osamp)
ft_t ft;
long *ibuf, *obuf;
int *isamp, *osamp;
{
	eff_t effp = &ft->eff;
	reverse_t reverse = (reverse_t) effp->priv;

	if (reverse->phase != WRITING)
		fatal(ft, "Internal error: reverse_flow called in wrong phase");
	if (fwrite((char *)ibuf, sizeof(long), *isamp, reverse->fp)
	    != *isamp)
		fatal(ft, "Reverse effect write error on temporary file\n");
	*osamp = 0;
}

/*
 * Effect drain: generate the actual samples in reverse order.
 */

reverse_drain(ft, obuf, osamp)
ft_t ft;
long *obuf;
int *osamp;
{
	eff_t effp = &ft->eff;
	reverse_t reverse = (reverse_t) effp->priv;
	int len, nbytes;
	register int i, j;
	register long temp;

	if (reverse->phase == WRITING) {
		fflush(reverse->fp);
		fseek(reverse->fp, 0L, SEEK_END);
		reverse->pos = ftell(reverse->fp);
		if (reverse->pos % sizeof(long) != 0)
			fatal(ft, "Reverse effect finds odd temporary file\n");
		reverse->phase = READING;
	}
	len = *osamp;
	nbytes = len * sizeof(long);
	if (reverse->pos < nbytes) {
		nbytes = reverse->pos;
		len = nbytes / sizeof(long);
	}
	reverse->pos -= nbytes;
	fseek(reverse->fp, reverse->pos, SEEK_SET);
	if (fread((char *)obuf, sizeof(long), len, reverse->fp) != len)
		fatal(ft, "Reverse effect read error from temporary file\n");
	for (i = 0, j = len-1; i < j; i++, j--) {
		temp = obuf[i];
		obuf[i] = obuf[j];
		obuf[j] = temp;
	}
	*osamp = len;
}

/*
 * Close and unlink the temporary file.
 */
reverse_stop(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	reverse_t reverse = (reverse_t) effp->priv;

	fclose(reverse->fp);
}

