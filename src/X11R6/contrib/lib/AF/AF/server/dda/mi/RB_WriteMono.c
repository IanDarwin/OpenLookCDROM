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
static char write_c_rcsid[]="$Header: /crl/audio/AF/server/dda/mi/RCS/RB_WriteMono.c,v 1.1 1993/11/29 22:38:45 stewart Exp $";
#endif

#include "ringbuffer.h"
#include <audio.h>
#include <server/include/misc.h>

void WriteMono(ring_buffer *to, int time, HSAMP *from, int count,
		 int channel, int preempt, int init, double gain)
{
  int t, thistime, offset;
  HSAMP *dest;
  float ft;
  float fg = gain;
  while (count > 0) {
    offset = (time - to->zeroTime) % to->size;
    thistime = min(count, to->size - offset);
    dest = (HSAMP *) &to->buf[offset];
    count -= thistime;
    time += thistime;
    if (fg == 1.0) {
      if(!init) {
	dest += channel;
	if(preempt == Preempt) {
	  while(thistime--) {
	    *dest = *from++;
	    dest += 2;
	  }
	} else {
	  while(thistime--) {
	    *dest = SUM(*dest, *from++);
	    dest += 2;
	  }
	}
      } else {
	if (preempt == Preempt) {
	  if(channel == 0) {
	    while(thistime--) {
	      *dest++ = *from++;
	      *dest++ = 0;
	    }
	  } else {
	    while(thistime--) {
	      *dest++ = 0;
	      *dest++ = *from++;
	    }
	  }
	}
	else {
	  if(channel == 0) {
	    while(thistime--) {
	      *dest++ = SUM(*dest, *from++);
	      *dest++ = 0;
	    }
	  } else {
	    while(thistime--) {
	      *dest++ = 0;
	      *dest++ = SUM(*dest, *from++);
	    }
	  }
	}
      }
    }
    else {
      if(!init) {
	dest += channel;
	if(preempt == Preempt) {
	  while(thistime--) {
	    *dest = GMOV(*from++);
	    dest += 2;
	  }
	} else {
	  while(thistime--) {
	    *dest = GSUM(*dest, *from++);
	    dest += 2;
	  }
	}
      } else {
	if (preempt == Preempt) {
	  if(channel == 0) {
	    while(thistime--) {
	      *dest++ = GMOV(*from++);
	      *dest++ = 0;
	    }
	  } else {
	    while(thistime--) {
	      *dest++ = 0;
	      *dest++ = GMOV(*from++);
	    }
	  }
	}
	else {
	  if(channel == 0) {
	    while(thistime--) {
	      *dest++ = GSUM(*dest, *from++);
	      *dest++ = 0;
	    }
	  } else {
	    while(thistime--) {
	      *dest++ = 0;
	      *dest++ = GSUM(*dest, *from++);
	    }
	  }
	}
      }
    }
  }
}
