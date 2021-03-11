/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *       Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *		Maynard, Massachusetts
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* file.c - file management for realtime fft program */

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/file.c,v 1.7 1994/06/03 17:27:31 jg Exp $*/

#include    "afft.h"

#include	<X11/Xlib.h>
#include    	<signal.h>
#include    	<sys/time.h>
#include    	<AF/AFlib.h>
#include    	<AF/AFUtils.h>

#define	uchar	unsigned char
#define MIN( a, b)  (((a) < (b)) ? (a) : (b))


static	void	FileWithLofi (void);
static	void	FileNoLofi  (void);
static	void	DoFFTFrame  (int);
static	void	ReadSegment (uchar *, long);
static	ATime	AudioSync   (void);

static	ATime	buffertime; 	    /* Audio time of start of buffer */
static	ATime	timeoffset; 	    /* Offset between system and audio time */
static  struct	itimerval   itime;  /* Interval Timer values */

void
FileFFT	()

{
    if (ac != NULL) {
    	FileWithLofi ();
    } else {
    	FileNoLofi();
    }
}

void
SetFileTime ()

{
    itime.it_value.tv_sec = 1;
    itime.it_value.tv_usec = 0;
    itime.it_interval.tv_sec = 0;
    itime.it_interval.tv_usec =
      MIN (stride, fftlength) * 125;   /* assumes 8K sample rate! */
    setitimer (ITIMER_REAL, &itime, NULL);
}

static void
FileNoLofi()

{
    static int	iter = 0;
    float   	*afp, *wfp, *ofp;
    int	    	i;

    while (tk_NumMainWindows > 0) {
	fread (fbuff, fftlength, 1, soundfile);

/* Put samples into fft buffer, with windowing */

	ofp = &oscbuf [0];
    	afp = &afbuf [0];
    	wfp = &window [0];
    	for (i = 0; i < fftlength; i += 1) {
	    *ofp = (float) AF_exp_u[fbuff [i]];
    	    *afp++ = *ofp++ * *wfp++;
	}

/* Update the display, either spectrogram or waterfall */

    	if (spectro_flag) {
    	    spectrogram (iter);
    	} else {
    	    waterfall (iter);
    	}
    	iter += 1;

/* Check for intercessions from the user */
	
	while (Tk_DoOneEvent (1) != 0) {
	    ;
	}
    }
}

static void
FileWithLofi()

{
    ATime   	ctime;
    unsigned	insert;

    signal (SIGALRM, DoFFTFrame);
    timeoffset = AudioSync();
    SetFileTime();
    insert = 0;
    while (tk_NumMainWindows > 0) {
    	ReadSegment (&fbuff [insert], 16384);
	ctime = AFGetTime (ac);
	if (atime < ctime) {    	/* Did we get behind?	    */
	    atime = ctime + 50; 	/* Skip ahead in audio time */
	}
	AFPlaySamples (acPlay, atime, 16384, &fbuff [insert]);
	if (insert == 0) {
	    buffertime = atime;
	}
	atime += 16384;
	insert += 16384;
	if (insert >= FBSIZE) {
	    insert = 0; 	    	/* wrap it around   	*/
	}
    }
}

/*
 *  Periodic task to display an FFT frame from data being
 *  read from soundfile
 *
 */

static void
DoFFTFrame(int xxx)
{
    static  	int	    iter = 0;
    float   	*afp, *wfp, *ofp;
    char    	*abp;
    struct  	timeval	    now;
    ATime   	ctime;
    int	        offset;
    int	    	i;

    if (tk_NumMainWindows == 0) {   	/* if application has quit  */
    	return;	    	    	    	/* bag it   	    	    */
    }

/* Estimate audio time from system time */

    gettimeofday (&now, NULL);
    ctime = now.tv_usec / 125 + now.tv_sec * 8000 + timeoffset;

/* Compute offset in sample buffer where audio is playing *now* */

    offset = ctime - buffertime;
    while (offset < 0) {
    	offset += FBSIZE;
    }
    while (offset >= FBSIZE) {
    	offset -= FBSIZE;
    }

/* Put next fftlength samples into fft buffer, with windowing */

    ofp = &oscbuf [0];
    afp = &afbuf [0];
    wfp = &window [0];
    for (i = 0; i < fftlength; i += 1) {
	*ofp = (float) AF_exp_u [fbuff [offset]];
    	*afp++ = *ofp++ * *wfp++;
	offset += 1;
	if (offset >= FBSIZE) {
	    offset = 0;
	}
    }

/* Update the display, either spectrogram or waterfall */

    if (spectro_flag) {
    	spectrogram (iter);
    } else {
    	waterfall (iter);
    }
    iter += 1;

/* Check for intercessions from the user */

    while (Tk_DoOneEvent (1) != 0) {
	;
    }
}

/*
 *  Read next setment of file into buffer
 *
 */

static void
ReadSegment (uchar *buffer, long count)

{
    long    offset;
    long    fcount;

    offset = 0;
    while (count > 0) {
    	fcount = fread (&buffer [offset], 1, count, soundfile);
    	if (fcount == 0) {
    	    rewind (soundfile);
    	    fcount = fread (&buffer [offset], 1, count, soundfile);
	    if (fcount == 0) {
	    	fprintf (stderr, "Couldn't rewind file\n");
		exit (1);
	    }
    	}
    	count -= fcount;
    	offset += fcount;
    }
}

/*
 *  Compute offset between system time and the
 *  Audio Server time.
 *
 */

static ATime
AudioSync()

{
    ATime   stime;
    struct  timeval	now;

    atime = AFGetTime (ac); 	    /* Server version of time	*/
    gettimeofday (&now, NULL);
    stime = now.tv_usec / 125	    /* System version of time	*/
    	    + now.tv_sec * 8000;
    return atime - stime;   	    /* Return difference    	*/
}
