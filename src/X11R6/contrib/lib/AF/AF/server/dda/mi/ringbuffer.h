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
/* $header$ */

#ifndef	RINGBUFFER_H
#define	RINGBUFFER_H

#include "sampletypes.h"
#include "timearith.h"

/* A ring buffer is a contiguous extent of memory (the buffer) which
 * contains stereo LIN16 samples corresponding to the extent of time
 * from baseTime to baseTime+size.
 * zeroTime is the timestamp of a sample once occupied
 * buf[0]. zeroTime is always earlier than baseTime, but not much earlier.
 *
 * The correct way to calculate the buffer index for a particular
 * time is ((time - zeroTime) % size).
 *
 */

typedef struct {
  STEREOSAMP *buf;
  int size;
  int zeroTime;
  int baseTime;
} ring_buffer;

/* macros for mixing and gain for LIN16 audio */
#define SUM(a, b) (t = (a) + (b), max(-32768, min(t, 32767)))
#define GSUM(a, b) (ft = fg * ((float) (a) + (float) (b)), \
		    ((short) max(-32768.0, min(ft, 32767.0))))
#define GMOV(a) (ft = fg * ((float) (a)), \
		    ((short) max(-32768.0, min(ft, 32767.0))))

/* Procedure forward definitions */

/* wrapcopy is an unbundled ring buffer procedure.
 *
 * wrapcopy(from->buf, to->buf,
 *          from->size, to->size,
 *          (fromtime - from->zeroTime) % from->size,
 *          (totime - to->zeroTime) % to->size,
 *          count)
 * is equivalent to
 * WrapCopy(from, to, fromtime, totime, count);
 */
extern void wrapcopy(STEREOSAMP *from, STEREOSAMP *to, 
	      int fromsize, int tosize,
	      int fromindex, int toindex,
	      int count);

/* wrapzero is an unbundled ring buffer procedure.
 *
 * wrapzero(rb->buf, rb->size, (time-rb->zeroTime) % rb->size, count)
 * is equivalent to
 * WrapZero(rb, time, count)
 */

extern void wrapzero(STEREOSAMP *to, 
	      int tosize,
	      int toindex,
	      int count);

/* Copy from ring buffer from to ring buffer to, for count samples */
extern void WrapCopy(ring_buffer *from, ring_buffer *to,
		     int fromTime, int toTime,
		     int count);

/* Write stereo silence into rb, at time toTime, for count samples */
extern void WrapZero(ring_buffer *to, int toTime, int count);

/* Once baseTime has been advanced, call the following to make sure
 * that zeroTime remains valid.
 */
extern void UpdateZeroTime(ring_buffer *rb);

/* Procedures whose target is a ringbuffer */

/* WriteStereo copies stereo LIN16 data from an array to a ringbuffer.
 * 
 * if preempt, WriteStero overwrites, otherwise it mixes with limiting
 */
extern void WriteStereo(ring_buffer *to, int time, STEREOSAMP *from, int count, 
			int preempt, double gain);

/* Copy Monophonic data from an array to a ringbuffer.
 * channel 0 is LEFT.
 * from is a pointer to an array of LIN16 samples.
 * if preempt, replace the samples already there, otherwise mix
 * if init, store silence to the other channel, otherwise ignore
 * the other channel.
 */
extern void WriteMono(ring_buffer *to, int time, HSAMP *from, int count, 
		      int channel, int preempt, int init, double gain);

/* procedures whose source is a ringbuffer *
/* 
 * ReadStereo copies stereo LIN16 data from a ringbuffer to an array
 */

extern void ReadStereo(ring_buffer *from, int time, 
		       STEREOSAMP *to, int count, double gain);

/* ReadMono copies monophonic data from a ring buffer to an array 
 * channel 0 is LEFT.
 */
extern void ReadMono(ring_buffer *from, int time, HSAMP *to, 
		     int count, int channel, double gain);

#endif
