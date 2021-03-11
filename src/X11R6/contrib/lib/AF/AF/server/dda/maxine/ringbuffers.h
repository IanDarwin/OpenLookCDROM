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
#ifndef _RINGBUFFERS_H_
#define _RINGBUFFERS_H_

#ifndef	MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif
#ifndef	MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

#define RING_SIZE   BCHANBUFFSIZE        /* size of each buffer          */
#define WDEADSPACE	(1*DMASIZE + 8)	/* leave room between fill and dsp unload */
#define RDEADSPACE	(1*DMASIZE + 8)	/* leave room between fill and dsp unload */
#define SILENCE_IS_GOLDEN 0xffffffff	/* send silence			    */

#define CODEC_TIME(info) ((ATime)(((info)->codec_time)))

#define WINDEX_TO_ADDR(bba,index) \
	  &((bba)->xmt_buff[BF][index])
#define RINDEX_TO_ADDR(bba,index) \
	  &((bba)->rcv_buff[BF][index]) 
extern	unsigned char *write_lofi_codec();

#endif
