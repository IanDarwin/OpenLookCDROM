#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/lib/AFSox/RCS/raw.c,v 1.3 1994/04/04 12:47:21 tml Exp $";
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
 * Sound Tools raw format file.
 *
 * Includes .ub, .uw, .sb, .sw, and .ul formats at end
 */

/*
 * Notes: most of the headerless formats set their handlers to raw
 * in their startread/write routines.  
 *
 */

#include "st.h"
#include "libst.h"

IMPORT int summary, verbose;

rawstartread(ft) 
ft_t ft;
{
}

rawstartwrite(ft) 
ft_t ft;
{
}

rawread(ft, buf, nsamp) 
ft_t ft;
long *buf, nsamp;
{
	register long datum;
	int done = 0;

	nsamp *= ft->info.channels;

	switch(ft->info.size) {
		case BYTE: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					datum = getc(ft->fp);
					if (feof(ft->fp))
						goto outout;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 24);
					done++;
				}
				goto outout;
			case UNSIGNED:
				while(done < nsamp) {
					datum = getc(ft->fp);
					if (feof(ft->fp))
						goto outout;
					/* Convert to signed */
					datum ^= 128;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 24);
					done++;
				}
				goto outout;
			case ULAW:
				/* grab table from Posk stuff */
				while(done < nsamp) {
					datum = getc(ft->fp);
					if (feof(ft->fp))
						goto outout;
					datum = st_ulaw_to_linear(datum);
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				goto outout;
			case ALAW:
				fatal(ft, "No A-Law support");
				goto outout;
			}
		case WORD: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					datum = rshort(ft);
					if (feof(ft->fp))
						goto outout;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				goto outout;
			case UNSIGNED:
				while(done < nsamp) {
					datum = rshort(ft);
					if (feof(ft->fp))
						goto outout;
					/* Convert to signed */
					datum ^= 0x8000;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				goto outout;
			case ULAW:
				fatal(ft, "No U-Law support for shorts");
				goto outout;
			case ALAW:
				fatal(ft, "No A-Law support");
				goto outout;
			}
		}
	fatal(ft, "Drop through in rawread!");
outout:
	return done / ft->info.channels;
}
rawbuf(ft, nsamp) 
ft_t ft;
long nsamp;
{
	register long datum;
	unsigned char *cbuf = (unsigned char *) ft->ibuf;
	unsigned short *sbuf = (unsigned short *) ft->ibuf;
	long *buf = (long *) ft->obuf;
	int done = 0;
	nsamp *= ft->info.channels;

	switch(ft->info.size) {
		case BYTE: switch(STYLE(ft)) {
			case SIGN2:
				while(done < nsamp) {
				/*******
					datum = getc(ft->fp);
				*******/
					datum = *cbuf++;
					if (feof(ft->fp))
						goto outout;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 24);
					done++;
				}
				goto outout;
			case UNSIGNED:
				while(done < nsamp) {
				/***********
					datum = getc(ft->fp);
				***********/
					datum = *cbuf++;
					if (feof(ft->fp))
						goto outout;
					/* Convert to signed */
					datum ^= 128;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 24);
					done++;
				}
				goto outout;
			case ULAW:
				/* grab table from Posk stuff */
				while(done < nsamp) {
				/***********
					datum = getc(ft->fp);
				***********/
					datum = *cbuf++;
					if (feof(ft->fp))
						goto outout;
					datum = st_ulaw_to_linear(datum);
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				goto outout;
			case ALAW:
				fatal(ft, "No A-Law support");
				goto outout;
			}
		case WORD: switch(STYLE(ft)) {
			case SIGN2:
				while(done < nsamp) {
				/**********
					datum = rshort(ft);
				**********/
					datum = *sbuf++;
					if (feof(ft->fp))
						goto outout;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				goto outout;
			case UNSIGNED:
				while(done < nsamp) {
				/**********
					datum = rshort(ft);
				**********/
					datum =  *sbuf++;
					if (feof(ft->fp))
						goto outout;
					/* Convert to signed */
					datum ^= 0x8000;
					/* scale signed up to long's range */
					*buf++ = LEFT(datum, 16);
					done++;
				}
				goto outout;
			case ULAW:
				fatal(ft, "No U-Law support for shorts");
				goto outout;
			case ALAW:
				fatal(ft, "No A-Law support");
				goto outout;
			}
		}
	fatal(ft, "Drop through in rawbuf!");
outout:
	return done / ft->info.channels;
}

void
rawwrite(ft, buf, nsamp) 
ft_t ft;
long *buf, nsamp;
{
	char *tmp = (char *) buf;
	short *tmp2 = (short *) buf;
	register int datum;
	int done = 0;

	nsamp *= ft->outfo.channels;

	switch(ft->outfo.size) {
		case BYTE: switch(ft->outfo.style) {
			case SIGN2:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 24);
					*tmp++ = datum;
				/****
					putc(datum, ft->fp);
				***/
					done++;
				}
				return;
			case UNSIGNED:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 24);
					/* Convert to unsigned */
					datum ^= 128;
					*tmp++ = datum;
				/****
					putc(datum, ft->fp);
				***/
					done++;
				}
				return;
			case ULAW:
				/* grab table from Posk stuff */
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					datum = st_linear_to_ulaw(datum);
					*tmp++ = datum;
				/****
					putc(datum, ft->fp);
				***/
					done++;
				}
				return;
			case ALAW:
				fatal(ft, "No A-Law support");
				return;
			}
		case WORD: switch(ft->info.style) {
			case SIGN2:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					*tmp2++ = datum;
				/******
					wshort(ft, datum);
				******/
					done++;
				}
				return;
			case UNSIGNED:
				while(done < nsamp) {
					/* scale signed up to long's range */
					datum = RIGHT(*buf++, 16);
					/* Convert to unsigned */
					datum ^= 0x8000;
					*tmp2++ = datum;
				/******
					wshort(ft, datum);
				******/
					done++;
				}
				return;
			case ULAW:
				fatal(ft, "No U-Law support for shorts");
				return;
			case ALAW:
				fatal(ft, "No A-Law support");
				return;
			}
		}
	/* My, there's a lot of code missing! */
	fatal(ft, "Drop through in rawwrite!");
}

bufread(ft, buf, nsamp)
ft_t ft;
char *buf;
long nsamp;
{
	if (ft->swap)
		return (swapread(ft, buf, nsamp));
	else
		return (fread(buf, ft->info.size*ft->info.channels, 
			      nsamp, ft->fp));
}
swapread(ft, buf, nsamp)
ft_t ft;
char *buf;
long nsamp;
{
	int i, done = 0;
	char tmp;
	int ret_val;

	ret_val = fread(buf, ft->info.size*ft->info.channels, nsamp, ft->fp);
	switch(ft->outfo.size) {
	case BYTE: 
		break;
	case WORD: switch(ft->info.style) {
		case SIGN2:
		case UNSIGNED:
			while(done < nsamp) {
			   for (i = 0; i < ft->info.channels; i++)
			   {
				tmp = *buf;
				*buf = *(buf+1);
				*(buf+1) = tmp;
				buf += 2;
			   }
			   done++;
			}
			break;
		case ULAW:
			fatal(ft, "No U-Law support for shorts");
			break;
		case ALAW:
			fatal(ft, "No A-Law support");
			break;
		}
	}
	return ret_val;
}
bufwrite(ft, buf, nsamp)
ft_t ft;
char *buf;
long nsamp;
{
	return (fwrite(buf, ft->outfo.size*ft->outfo.channels, nsamp, ft->fp));
}
bufcp(ft, buf, nsamp)
ft_t ft;
long *buf, nsamp;
{
	char *tmp;
	int tmpsize;

        tmp = ft->ibuf;
        tmpsize = ft->isize;
        ft->ibuf = ft->obuf;
        ft->isize = ft->osize;
        ft->obuf = tmp;
        ft->osize = tmpsize;
}

/*
 * Set parameters to the fixed parameters known for this format,
 * and change format to raw format.
 */

static  rawdefaults();

/* Signed byte */
sbstartread(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

sbstartwrite(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

ubstartread(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

ubstartwrite(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

uwstartread(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

uwstartwrite(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = UNSIGNED;
	rawdefaults(ft);
}

swstartread(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

swstartwrite(ft) 
ft_t ft;
{
	ft->info.size = WORD;
	ft->info.style = SIGN2;
	rawdefaults(ft);
}

ulstartread(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = ULAW;
	if (ft->info.rate < 0)
		ft->info.rate = 8000;
	rawdefaults(ft);
}

ulstartwrite(ft) 
ft_t ft;
{
	ft->info.size = BYTE;
	ft->info.style = ULAW;
	rawdefaults(ft);
}

static
rawdefaults(ft)
ft_t ft;
{
	if (ft->info.rate == 0)
		ft->info.rate = 8000;
	if (ft->info.channels == -1)
		ft->info.channels = 1;
}


