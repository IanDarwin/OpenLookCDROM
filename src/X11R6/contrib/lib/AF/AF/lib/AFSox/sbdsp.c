#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/sbdsp.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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

char ansi_c_is_very_stupid_and_needs_a_variable_here;

#if	defined(BLASTER) || defined(SBLAST) || defined(LINUXSOUND)
/*
 * Copyright 1992 Rick Richardson
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Rick Richardson, Lance Norskog And Sundry Contributors are not
 * responsible for the consequences of using this software.
 */

/*
 * Direct to Sound Blaster device driver.
 * SBLAST patches by John T. Kohl.
 */

#include <sys/types.h>
#ifdef SBLAST
#include <i386/isa/sblast.h>
#else
#ifdef LINUXSOUND
#include <linux/soundcard.h>
#else
#include <sys/sb.h>
#endif
#endif
#include <signal.h>
#include "st.h"

/* Private data for SKEL file */
typedef struct sbdspstuff {
	int	samples;		/* bytes remaining in current block */
} *sbdsp_t;

IMPORT float volume, amplitude;
IMPORT int summary, verbose;

static got_int = 0;

static void
sigint(s)
{
	if (s) got_int = 1;
	else signal(SIGINT, sigint);
}

/*
 * Do anything required before you start reading samples.
 * Read file header. 
 *	Find out sampling rate, 
 *	size and style of samples, 
 *	mono/stereo/quad.
 */
int
sbdspstartread(ft) 
ft_t ft;
{
	sbdsp_t sbdsp = (sbdsp_t) ft->priv;
	int off = 0;

	/* If you need to seek around the input file. */
	if (0 && ! ft->seekable)
		return fail(ft, "SKEL input file must be a file, not a pipe");

	if (!ft->info.rate)
		ft->info.rate = 11000;
	ft->info.size = BYTE;
	ft->info.style = UNSIGNED;
	ft->info.channels = 1;
#ifdef LINUXSOUND
	ioctl(fileno(ft->fp), SNDCTL_DSP_RESET, 0);
#else
	ioctl(fileno(ft->fp), DSP_IOCTL_RESET, 0);
#endif
#ifdef SBLAST
	ioctl(fileno(ft->fp), DSP_IOCTL_VOICE, &off);
	ioctl(fileno(ft->fp), DSP_IOCTL_SPEED, &ft->info.rate);
#elif defined(LINUXSOUND)
	ioctl(fileno(ft->fp), SNDCTL_DSP_SPEED, ft->info.rate);
#else
	ioctl(fileno(ft->fp), DSP_IOCTL_VOICE, 0);
	ioctl(fileno(ft->fp), DSP_IOCTL_SPEED, ft->info.rate);
#endif
	sigint(0);	/* Prepare to catch SIGINT */
	return AF_SUCCESS;
}

/*
 * Read up to len samples from file.
 * Convert to signed longs.
 * Place in buf[].
 * Return number of samples read.
 */

sbdspread(ft, buf, len) 
ft_t ft;
long *buf, len;
{
	sbdsp_t sbdsp = (sbdsp_t) ft->priv;
	int		rc;

	if (got_int) return (0);
	rc = rawread(ft, buf, len);
	if (rc < 0) return 0;
	return (rc);
}

/*
 * Do anything required when you stop reading samples.  
 * Don't close input file! 
 */
sbdspstopread(ft) 
ft_t ft;
{
#ifdef SBLAST
	ioctl(fileno(ft->fp), DSP_IOCTL_FLUSH, 0);
#endif
}

sbdspstartwrite(ft) 
ft_t ft;
{
	sbdsp_t sbdsp = (sbdsp_t) ft->priv;
	int on = 1;

	/* If you have to seek around the output file */
	if (0 && ! ft->seekable)
		return fail(ft, "Output .sbdsp file must be a file, not a pipe");

	if (!ft->info.rate)
		ft->info.rate = 11000;
	ft->info.size = BYTE;
	ft->info.style = UNSIGNED;
	ft->info.channels = 1;
#ifdef LINUXSOUND
	ioctl(fileno(ft->fp), SNDCTL_DSP_RESET, 0);
#else
	ioctl(fileno(ft->fp), DSP_IOCTL_RESET, 0);
#endif
#ifdef SBLAST
	ioctl(fileno(ft->fp), DSP_IOCTL_FLUSH, 0);
	ioctl(fileno(ft->fp), DSP_IOCTL_VOICE, &on);
	ioctl(fileno(ft->fp), DSP_IOCTL_SPEED, &ft->info.rate);
#elif defined(LINUXSOUND)
	ioctl(fileno(ft->fp), SNDCTL_DSP_SYNC, 0);
	ioctl(fileno(ft->fp), SNDCTL_DSP_SPEED, ft->info.rate);
#else
	ioctl(fileno(ft->fp), DSP_IOCTL_VOICE, 1);
	ioctl(fileno(ft->fp), DSP_IOCTL_SPEED, ft->info.rate);
#endif
}

sbdspwrite(ft, buf, len) 
ft_t ft;
long *buf, len;
{
	sbdsp_t sbdsp = (sbdsp_t) ft->priv;

	if (len == 0) return 0;
	return (rawwrite(ft, buf, len));
}

sbdspstopwrite(ft) 
ft_t ft;
{
	/* All samples are already written out. */
	/* If file header needs fixing up, for example it needs the */
 	/* the number of samples in a field, seek back and write them here. */
	fflush(ft->fp);
#ifdef LINUXSOUND
	ioctl(fileno(ft->fp), SNDCTL_DSP_SYNC, 0);
#else
	ioctl(fileno(ft->fp), DSP_IOCTL_FLUSH, 0);
#endif
}
#endif
