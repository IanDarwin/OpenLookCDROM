/* $XConsortium: stopwatch.c,v 5.4 94/04/17 20:44:44 eswu Exp $ */
/*

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/
/***********************************************************
Copyright(c) 1989,1990, 1991 by Sun Microsystems, Inc.

						All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that Sun Microsystems
not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	jmz / SimGraphics Engineering Corportation
|
| File          :	stopwatch.c
| Date          :	8/27/88
| Project       :	PLB
| Description   :	Stopwatch functions.
| Status        :	Version 1.0
|
| Revisions     :	
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	float stopwatch(int)
|		:	a standard stop watch with split capabilities
|	double bif_time()
|		:	Get the time (system dependent portion)
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Includes, Defines, and Statics
\*--------------------------------------------------------------------*/

#include <X11/Xosdefs.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>

#include "stopwatch.h"

#ifndef X_NOT_STDC_ENV
#include <time.h>
#else /* X_NOT_STDC_ENV */
#ifdef SYSV
#include <time.h>		/* necessary? */
#endif /* SYSV */
struct tm *localtime();
#endif /* X_NOT_STDC_ENV */

#ifdef SGI4D
#include <sys/param.h>
#define CLK_TCK HZ
#endif
#ifdef sparc
#include <sys/param.h>
#define CLK_TCK HZ
#else 
#ifdef SYSV
#include <limits.h>
#endif
#endif

/* If we don't have a CLK_TCK value, make one up, 60 is reasonable */
#ifndef CLK_TCK
#define CLK_TCK 60
#endif

#define real_time(a,b) (float) (a - b) 

/* define the watch states */
#define BRAND_NEW 0
#define STOPPED 1
#define RUNNING 2
			
/*--------------------------------------------------------------------*\
| Procedure     :	float stopwatch(int)
|---------------------------------------------------------------------
| Description   :	a standard stop watch with split capabilities
|
|	Returns the the current reading of the watch in seconds
|	Reset returns 0.
|	
|	Precision hands back the smallest unit of time the
|	watch can measure
|	
|	NOTE: stopwatch should be fully portable to other systems
|	as the hardware dependancies have been put in bif_time.
|---------------------------------------------------------------------
| Return        :	The reading from the stopwatch
\*--------------------------------------------------------------------*/

float stopwatch(option)
int option;
{
/* what is the watch doing? .... */
	static int watchstate = BRAND_NEW;
/* when was the last start (or reset) */
	static double watchzero = 0.0; /* in real seconds */
/* what is the (offset) current time? */
	static double watchtime = 0.0; /* in real seconds */
/* how much time was run before the last start */
	static double watchsumm = 0.0; /* in system ticks */
/* what is the current time? */
	static double watchreal = 0.0; /* in seconds */

	float watchprec = 0.0; /* in seconds */

	double bif_time();

#ifdef TEST_TIMER
	fprintf(stderr,"Stopwatch called with option %d\n",option);
	fflush(stderr);
#endif

	if ( watchstate == BRAND_NEW )
		watchzero = bif_time();

	switch (option)
	{
	case WATCH_STOP :
		if ( watchstate != STOPPED )
		{
		/* Report the current time and update the summation */
			watchstate = STOPPED;
			watchtime = bif_time();
			watchreal = real_time(watchtime+watchsumm,watchzero);
			watchsumm += watchtime - watchzero;
		}
		break;

	case WATCH_START :
		if ( watchstate != RUNNING )
		{
		/* Give a new base to subtract off */
		/* Report the last reported time */
			watchstate = RUNNING;
			watchzero = bif_time();
		}
		else
		{
		/* A start on a start watch is the same as a split */
			watchtime = bif_time();
			watchreal = real_time(watchtime+watchsumm,watchzero);
		}
		break;

	case WATCH_SPLIT :
	/* Give the current time report last calculated time when stopped*/
		if ( watchstate == RUNNING )
		{
		

			watchtime = bif_time();
			watchreal = real_time(watchtime+watchsumm,watchzero);
		}
		break;

	case WATCH_RESET :
	/* clear the summary reset zero / time /and real time */
		watchsumm = 0;
		watchzero = bif_time();
		watchtime = watchzero;
		watchreal = 0.;
		break;

	case WATCH_PRECISION :
	/* Report the smallest measurable unit of time returned by
	the hardware clock. This should be milliseconds for all known
	implimentations of localtime() and gettimeofday(). */
		watchprec = 1000.0/(float)CLK_TCK;
		break;
	}

	if ( option != WATCH_PRECISION )
		return ( watchreal );
	else
		return ( watchprec );
}


/*--------------------------------------------------------------------*\
| Procedure     :	double bif_time()
|---------------------------------------------------------------------
| Description   :	Get the time (system dependent portion)
|
|	PROGRAMMER NOTE. Your system must support localtime() and 
|	gettimeofday() calls. These are standard UNIX system calls. If
|	your system is non-standard, replacements for these calls
|	need to be added to the routine bif_time at the end of this
|	file.
|---------------------------------------------------------------------
| Return        :	 The current time.
\*--------------------------------------------------------------------*/
double bif_time()

{
#ifdef EXTERNALNOTE


	ATTENTION END PORT PROGRAMMERS!!!!!!!

	  This time function routine is based on the function gettimeofday(),
	which is a standard call on most UNIX systems. It uses defines
	in the include file sys/time.h. For non-UNIX systems, the gettimeofday()
	will have to be replaced.

#endif /* endif EXTERNALNOTE */

	/* Return a double value of seconds and fractions of seconds to the
	greatest accuracy allowed by the particular hardware implimentation.
	System dependant changes or changes to the UNIX time functions
	are all made here. */
     
#ifndef sgi			/* SGI compiler complains about redef */
	int gettimeofday();
#endif
        static struct timeval tval, *tp;
        static struct timezone tzone, *tzp;
        struct tm *clock;

	int status;
	int offset;
	double whole;
	double fraction;
	double sum;

	tp = &tval;
	tzp = &tzone;

	tzone.tz_minuteswest = 0; /* time zone doesn't matter here */
	tzone.tz_dsttime = 0; /* Neither does daylight savings time */
        status = gettimeofday(tp, tzp);
	if(status == -1)
	{
		fprintf(stderr,"OOPS: Failure on gettimeofday.\n");
		fflush(stderr);
		return(-1);
	}

	/* you are probably going to ask yourself why we are 
	creating a hold value and subtracting from seconds.
	Well, even if you aren't, here is why. It seems that UNIX, in its
	infinite wisdom, starts their arbitrary seconds clock on 1/1/1970.
	I am writing this code on 1/15/1990, 20 years after the fact. This
	means that tp->tv_sec is returning a long value of roughly 630000000
	which still fits within a 32 bit long word. However, the 
	conversion to float cannot handle this value without
	truncating the least significant digits, which, as a timer device,
	happen to be the only ones we are really interested in!   
	The brute force approach is to subtract 630000000 from the returned
	time and let someone else deal with the sudden bug 
	about the problem in 2010. But, let's
	actually read the date, find how many years have passed
	since 1970, and multiply by 365.25 * 24 * 60 * 60 seconds. 
	This value gets subtracted from the tp->tv_sec return, keeping our
	timer usable, even when it and this machine are running in the
	Smithsonian in 2050. BUG. Don't run this code right on the stroke
	of midnight at New Years.
	*/


	clock = localtime((time_t *) &tp->tv_sec);
	offset = (clock->tm_year - 70) * 365 * 24 * 60 * 60; 

	whole = (float)(tp->tv_sec - offset);
 	fraction = (float)tp->tv_usec * .000001;  
	sum = whole + fraction;


	return(sum);

}

