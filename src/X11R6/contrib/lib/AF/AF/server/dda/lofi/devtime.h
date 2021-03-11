/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
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

#ifndef	DEVTIME_H
#define	DEVTIME_H


/* Handle 32 bit time aliasing, convention future 3.1 days and past 3.1 */
#define	FUTURE(time,current)	( ((time)-(current)) < 0x80000000 ? 1 : 0)
#define	PAST(time,current)	!FUTURE(time,current)
#define	DELTA(future,time,current)	((future) ? ((time)-(current)) : \
					(current)-(time) )

/* Given a codec audio device, read the time register on the hardware. */
#define CODEC_GET_TIME(aDev)	((* ((CodecPrivate *)((aDev)->privPtr))\
					->devTimePtr)&0xFFFFFF00L)

/* Give a codec audio device, update current time in the audio device. */
#define CODEC_UPDATE_TIME(aDev)	\
	(aDev)->dsptime = CODEC_GET_TIME(aDev); \
	(aDev)->time0 += ((((aDev)->dsptime))-(((aDev)->oldDevTime)))>>8; \
	(aDev)->oldDevTime = (aDev)->dsptime;

	
/* Given a hifi audio device, read the time register on the hardware. */
#define HIFI_GET_TIME(aDev)	((* ((HiFiPrivate *)((aDev)->privPtr))->\
					devTimePtr)&0xFFFFFF00L)

/* Give a hifi audio device, update current time in the audio device. */
#define HIFI_UPDATE_TIME(aDev)	\
	(aDev)->dsptime = HIFI_GET_TIME(aDev); \
	(aDev)->time0 += ((((aDev)->dsptime))-(((aDev)->oldDevTime)))>>8; \
	(aDev)->oldDevTime = (aDev)->dsptime;
	

#endif
