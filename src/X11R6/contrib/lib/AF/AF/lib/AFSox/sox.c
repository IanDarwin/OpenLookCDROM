#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /crl/audio/AF/lib/AFSox/RCS/sox.c,v 1.3 1994/04/04 12:47:21 tml Exp $";
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

#include "st.h"
#if	defined(unix) || defined(AMIGA) || defined(__OS2__)
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef	__STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <ctype.h>
#include <string.h>
#ifdef VMS
#include <errno.h>
#include <perror.h>
#define LASTCHAR        ']'
#else
#include <errno.h>
#define LASTCHAR        '/'
#endif
#if 0
static char *myname;
EXPORT struct soundstream informat, outformat;
extern ft_t ft;
struct effect eff;
EXPORT int writing = 0;	/* are we writing to a file? */
#endif /* 0 */
void	gettype(), geteffect(), checkeffect();

void
#ifdef	__STDC__
report(char *fmt, ...)
#else
report(va_alist) 
va_dcl
#endif
{
	va_list args;
#ifndef	__STDC__
	char *fmt;
#endif

#if 0
	if (! verbose)
		return;
#ifndef	DOS
	/* single-threaded machines don't really need this */
	fprintf(stderr, "%s: ", myname);
#endif
#ifdef	__STDC__
	va_start(args, fmt);
#else
	va_start(args);
	fmt = va_arg(args, char *);
#endif
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
#endif /* 0 */
}

int
#ifdef	__STDC__
fail(ft_t ft, char *fmt, ...)
#else
fail(va_alist) 
va_dcl
#endif
{
	va_list args;
#ifndef	__STDC__
	ft_t ft;
	char *fmt;
#endif

#ifndef	DOS
	/* single-threaded machines don't really need this */
	fprintf(stderr, "libAFSox: ");
#endif
#ifdef	__STDC__
	va_start(args, fmt);
#else
	va_start(args);
	ft = va_arg(args, ft_t);
	fmt = va_arg(args, char *);
#endif
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	/* Close the input file and outputfile before exiting*/
	if (ft->fp)
		fclose(ft->fp);
	ft->fp = NULL;
	if (ft->ibuf)
		free(ft->ibuf);
	ft->ibuf = NULL;
	ft->isize = 0;
	if (ft->obuf)
		free(ft->obuf);
	ft->obuf = NULL;
	ft->osize = 0;

	/* remove the output file because we fataled */
	if (ft->info.mode == RECORDMODE)
	    REMOVE(ft->filename);
	ft->filename = NULL;
	return AF_FAILURE;
}


void
#ifdef	__STDC__
fatal(ft_t ft, char *fmt, ...)
#else
fatal(va_alist) 
va_dcl
#endif
{
	va_list args;
#ifndef	__STDC__
	ft_t ft;
	char *fmt;
#endif

#ifndef	DOS
	/* single-threaded machines don't really need this */
	fprintf(stderr, "libAFSox: ");
#endif
#ifdef	__STDC__
	va_start(args, fmt);
#else
	va_start(args);
	ft = va_arg(args, ft_t);
	fmt = va_arg(args, char *);
#endif
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
	/* Close the input file and outputfile before exiting*/
	if (ft->fp)
		fclose(ft->fp);
	if (ft->ibuf)
		free(ft->ibuf);
	ft->ibuf = NULL;
	ft->isize = 0;
	if (ft->obuf)
		free(ft->obuf);
	ft->obuf = NULL;
	ft->osize = 0;

	/* remove the output file because we fataled */
	if (ft->info.mode == RECORDMODE)
	        REMOVE(ft->filename);
	ft->filename = NULL;
	if (ft->errfunc)
	{
		(*ft->errfunc)();
	}
	else
	{
	    exit(2);
	}
}

strcmpcase(s1, s2)
char *s1, *s2;
{
	while(*s1 && *s2 && (tolower(*s1) == tolower(*s2)))
		s1++, s2++;
	return *s1 - *s2;
}
#if !defined(__osf__) && !defined(ultrix)
char *
strerror(i)
{
	return "strerror";
}
#endif
/*
 * If no effect given, decide what it should be.
 */
void
checkeffect(ft)
ft_t ft;
{
	int already = (ft->eff.name != (char *) 0);
	char *rate = 0, *chan = 0;
	int i;

	for (i = 0; effects[i].name; i++) {
		if (!chan && (effects[i].flags & EFF_CHAN))
			chan = effects[i].name;
		if (! rate && (effects[i].flags & EFF_RATE))
			rate = effects[i].name;
	}

	if (ft->eff.name)
		return;

	/* 
	 * Require mixdown for channel mismatch.
	 * XXX Doesn't handle channel expansion.  Need an effect for this.
	 * Require one of the rate-changers on a rate change.
	 * Override a rate change by explicitly giving 'copy' command.
	 */
	if (ft->info.channels != ft->outfo.channels) {
		if (ft->eff.name && !(ft->eff.h.flags & EFF_CHAN))
			fatal(ft, "Need to do change number of channels first.  Try the '%s' effect.", chan);
		if (! ft->eff.name) {
			ft->eff.name = chan;
			report(
"Changing %d input channels to %d output channels with '%s' effect\n",
			ft->info.channels, ft->outfo.channels, chan);
			geteffect(ft);
		}
	} 
	/* 
	 * Be liberal on rate difference errors.
	 * Note that the SPARC 8000-8192 problem
	 * comes in just under the wire.  XXX
	 *
 	 * Bogus.  Should just do a percentage.
	 */
	if (abs(ft->info.rate - ft->outfo.rate) > 200) {
		if (ft->eff.name && !(ft->eff.h.flags & EFF_RATE))
		    fatal(ft, "Need to do rate change first.  Try the '%s' effect.", 
			rate);
		if (! ft->eff.name) {
			ft->eff.name = rate;
			report(
"Changing sample rate %lu to rate %lu via noisy 'rate' effect\n",
			ft->info.rate, ft->outfo.rate);
			geteffect(ft);
		}
	}
	/* don't need to change anything */
	if (! ft->eff.name)
		ft->eff.name = "copy";
	if (! already) {
		geteffect(ft);
		/* give default opts for manufactured effect */
		(* ft->eff.h.getopts)(ft, 0, (char *) 0);
	}
}

/*
 * Check that we have a known effect name.
 */
void
geteffect(ft)
ft_t ft;
{
	eff_t effp = &ft->eff;
	int i;

	for(i = 0; effects[i].name; i++) {
		char *s1 = effects[i].name, *s2 = effp->name;
		while(*s1 && *s2 && (tolower(*s1) == tolower(*s2)))
			s1++, s2++;
		if (*s1 || *s2)
			continue;	/* not a match */
		/* Found it! */
		effp->h = effects[i];
		return;
	}
	/* Guido Van Rossum fix */
	fprintf(stderr, "Known effects:");
	for (i = 0; effects[i].name; i++)
		fprintf(stderr, "\t%s\n", effects[i].name);
	fatal(ft, "\nEffect '%s' is not known!", effp->name);
}
/*
 * Check that we have a known format suffix string.
 */
void
gettype(ft)
ft_t ft;
{
	char **list;
	int i;

	if (! ft->filetype)
fatal(ft, "Must give file type for %s file, either as suffix or with -t option",
ft->filename);
	for(i = 0; formats[i].names; i++) {
		for(list = formats[i].names; *list; list++) {
			char *s1 = *list, *s2 = ft->filetype;
			if (! strcmpcase(s1, s2))
				break;	/* not a match */
		}
		if (! *list)
			continue;
		/* Found it! */
		ft->h = formats[i];
		return;
	}
	if (! strcmpcase(ft->filetype, "snd")) {
		fatal(ft, "Use the sequence '-t .au file.snd'\n");
	}
	fatal(ft, "File type '%s' of %s file is not known!",
		ft->filetype, ft->filename);
}
