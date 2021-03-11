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
static char write_c_rcsid[]="$Header: /crl/audio/AF/server/dda/mi/RCS/RB_WrapCopy.c,v 1.2 1994/03/29 08:44:19 tml Exp $";
#endif

#include "ringbuffer.h"
#include <audio.h>
#include <server/include/misc.h>

void WrapCopy(ring_buffer *from, ring_buffer *to,
	      int fromTime, int toTime,
	      int count)
{
  int thistime;
  fromTime = ((fromTime + from->size) - from->zeroTime) % from->size;
  toTime = ((toTime + to->size) - to->zeroTime) % to->size;
  if ((count > from->size) || (count > to->size)) {
    ErrorF("wrapcopy too big: count %d, from.size %d, to.size %d\n",
	   count, from->size, to->size);
    return;
  }
  while (count > 0) {
    thistime = min(count, from->size - fromTime);
    thistime = min(thistime, to->size - toTime);
    memcpy(&to->buf[toTime], &from->buf[fromTime], sizeof(STEREOSAMP) * thistime);
    count -= thistime;
    fromTime += thistime;
    toTime += thistime;
    if (fromTime >= from->size) fromTime = 0;
    if (toTime >= to->size) toTime = 0;
  }
  MB();
}

