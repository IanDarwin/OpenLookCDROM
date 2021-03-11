#ifndef LINT
#ifdef RCS_ID
static char *rcsid = "@(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/dat.c,v 1.5 1994/02/02 01:22:51 marvinw Exp $";
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
 * Sound Tools text format file.  Tom Littlejohn, March 93.
 *
 * Reads/writes sound files as text for use with fft and graph.
 *
 * June 28, 93: force output to mono.
 */

#include "st.h"
#include "libst.h"

IMPORT int summary, verbose;

static double time, deltat;

long roundoff(x)
double x;
{
    if (x < 0.0) return(x - 0.5);
    else return(x + 0.5);
    }

int
datstartread(ft)
ft_t ft;
{
   char inpstr[82];
   char sc;

   while (ft->info.rate == 0) {
      fgets(inpstr,82,ft->fp);
      sscanf(inpstr," %c",&sc);
      if (sc != ';') return fail(ft, "Cannot determine sample rate.");
      sscanf(inpstr," ; Sample Rate %ld",&ft->info.rate);
      }

   /* size and style are really not necessary except to satisfy caller. */

   ft->info.size = DOUBLE;
   ft->info.style = SIGN2;
   return AF_SUCCESS;
}

int
datstartwrite(ft)
ft_t ft;
{
   double srate;

   /*
   if (ft->info.channels > 1)
        return fail(ft, "Cannot create .dat file with more than one channel.");
   */
   ft->info.channels = 1;
   time = 0.0;
   srate = ft->info.rate;
   deltat = 1.0 / srate;
   fprintf(ft->fp,"; Sample Rate %ld\015\n",ft->info.rate);
}

datread(ft, buf, nsamp)
ft_t ft;
long *buf, nsamp;
{
    char inpstr[82];
    double sampval;
    int retc;
    int done = 0;
    char sc;

    while (done < nsamp) {
        do {
          fgets(inpstr,82,ft->fp);
          if feof(ft->fp) return done;
          sscanf(inpstr," %c",&sc);
          }
          while(sc == ';');  /* eliminate comments */
        retc = sscanf(inpstr,"%*s %lg",&sampval);
        if (retc != 1) return fail(ft, "Unable to read sample.");
        *buf++ = roundoff(sampval * 2.147483648e9);
        ++done;
    }
    return done;
}

void
datwrite(ft, buf, nsamp)
ft_t ft;
long *buf, nsamp;
{
 int done = 0;
    double sampval;

    while(done < nsamp) {
       sampval = *buf++ ;
       sampval = sampval / 2.147483648e9;  /* normalize to approx 1.0 */
       fprintf(ft->fp," %15.8g  %15.8g \015\n",time,sampval);
       time += deltat;
       done++;
       }
    return;
}

