#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/sndrtool.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * Sounder/Sndtool format handler: W V Neisius, February 1992
 *
 * June 28, 93: force output to mono.
 */

#include <math.h>
#include "st.h"
#ifdef	VMS
#include <errno.h>
#include <perror.h>
#endif

/* Private data used by writer */
struct sndpriv {
        unsigned long nsamples;
};

#ifndef	SEEK_CUR
#define	SEEK_CUR	1
#endif

IMPORT sndtwriteheader(P2(ft_t ft,long nsamples));

/*======================================================================*/
/*                         SNDSTARTREAD                                */
/*======================================================================*/

int
sndtstartread(ft)
ft_t ft;
{
struct sndpriv *p = (struct sndpriv *) ft->priv;

char buf[97];

long rate;

rate = 0;

/* determine file type */
        /* if first 5 bytes == SOUND then this is probably a sndtool sound */
        /* if first word (16 bits) == 0 
         and second word is between 4000 & 25000 then this is sounder sound */
        /* otherwise, its probably raw, not handled here */

if (fread(buf, 1, 2, ft->fp) != 2)
	return fail(ft, "SND: unexpected EOF");
if (strncmp(buf,"\0\0",2) == 0)
	{
	/* sounder */
	rate = rlshort(ft);
	if (rate < 4000 || rate > 25000 )
		return fail (ft, "SND: sample rate out of range");
	fseek(ft->fp,4,SEEK_CUR);
	}
else
	{
	/* sndtool ? */
	fread(&buf[2],1,6,ft->fp);
	if (strncmp(buf,"SOUND",5))
		return fail (ft, "SND: unrecognized SND format");
	fseek(ft->fp,12,SEEK_CUR);
	rate = rlshort(ft);
	fseek(ft->fp,6,SEEK_CUR);
	if (fread(buf,1,96,ft->fp) != 96)
		return fail (ft, "SND: unexpected EOF in SND header");
	report ("%s",buf);
	}

	ft->info.channels = 1;
	ft->info.rate = rate;
	ft->info.style = UNSIGNED;
	ft->info.size = BYTE;
	return AF_SUCCESS;
}

/*======================================================================*/
/*                         SNDTSTARTWRITE                               */
/*======================================================================*/
sndtstartwrite(ft)
ft_t ft;
{
struct sndpriv *p = (struct sndpriv *) ft->priv;

/* write header */
ft->info.channels = 1;
ft->info.style = UNSIGNED;
ft->info.size = BYTE;
p->nsamples = 0;
sndtwriteheader(ft, 0);

}
/*======================================================================*/
/*                         SNDRSTARTWRITE                               */
/*======================================================================*/
sndrstartwrite(ft)
ft_t ft;
{
/* write header */
ft->info.channels = 1;
ft->info.style = UNSIGNED;
ft->info.size = BYTE;

/* sounder header */
wlshort (ft,0); /* sample size code */
wlshort (ft,(int) ft->info.rate);     /* sample rate */
wlshort (ft,10);        /* volume */
wlshort (ft,4); /* shift */
}

/*======================================================================*/
/*                         SNDTWRITE                                     */
/*======================================================================*/

sndtwrite(ft, buf, len)
ft_t ft;
long *buf, len;
{
	struct sndpriv *p = (struct sndpriv *) ft->priv;
	p->nsamples += len;
	rawwrite(ft, buf, len);
}

/*======================================================================*/
/*                         SNDTSTOPWRITE                                */
/*======================================================================*/

sndtstopwrite(ft)
ft_t ft;
{
struct sndpriv *p = (struct sndpriv *) ft->priv;

/* fixup file sizes in header */
if (fseek(ft->fp, 0L, 0) != 0)
	return fail(ft, "can't rewind output file to rewrite SND header");
sndtwriteheader(ft, p->nsamples);
}

/*======================================================================*/
/*                         SNDTWRITEHEADER                              */
/*======================================================================*/
sndtwriteheader(ft,nsamples)
ft_t ft;
long nsamples;
{
char name_buf[97];

/* sndtool header */
fputs ("SOUND",ft->fp); /* magic */
fputc (0x1a,ft->fp);
wlshort (ft,(long)0);  /* hGSound */
wllong (ft,nsamples);
wllong (ft,(long)0);
wllong (ft,nsamples);
wlshort (ft,(int) ft->info.rate);
wlshort (ft,0);
wlshort (ft,10);
wlshort (ft,4);
sprintf (name_buf,"%s - File created by Sound Exchange",ft->filename);
fwrite (name_buf, 1, 96, ft->fp);

}


