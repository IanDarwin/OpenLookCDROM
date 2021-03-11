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
#if !defined(lint) && !defined(SABER)
static char write_c_rcsid[]="$Header: /crl/audio/AF/server/dda/mi/RCS/RB_WriteStereo.c,v 1.2 1994/03/29 08:44:19 tml Exp $";
#endif

#include "ringbuffer.h"
#include <audio.h>
#include <server/include/misc.h>


void WriteStereo(ring_buffer *to, int time, STEREOSAMP *from, int count, 
		   int preempt, double gain)
{
  int t, thistime, offset;
  HSAMP *dest, *src;
  float ft;
  float fg = (float) gain;
  while (count > 0) {
    src = (HSAMP *) from;
    offset = ((time + to->size) - to->zeroTime) % to->size;
    thistime = min(count, to->size - offset);
    dest = (HSAMP *) &to->buf[offset];
    count -= thistime;
    time += thistime;
    from += thistime;
    if(preempt == Preempt) {
      if (fg == 1.0) memcpy(dest, src, thistime * sizeof(STEREOSAMP));
      else {
	while (thistime--) {
	  *dest++ = GMOV(*src++);
	  *dest++ = GMOV(*src++);
	}
      }
    }
    else {
      if (fg == 1.0) {
	while(thistime--) {
	  *dest++ = SUM(*dest, *src++);
	  *dest++ = SUM(*dest, *src++);
	}
      }
      else {
	while(thistime--) {
	  *dest++ = GSUM(*dest, *src++);
	  *dest++ = GSUM(*dest, *src++);
	}
      }
    }
  }
}

