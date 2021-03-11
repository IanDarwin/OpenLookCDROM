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
static char write_c_rcsid[]="$Header: /crl/audio/AF/server/dda/mi/RCS/wrapcopy.c,v 1.2 1994/03/29 08:44:19 tml Exp $";
#endif

#include "ringbuffer.h"
#include <audio.h>
#include <server/include/misc.h>

/* copies , wrapping both */
void wrapcopy(STEREOSAMP *from, STEREOSAMP *to, 
	      int fromsize, int tosize,
	      int fromindex, int toindex,
	      int count)
{
  int thistime;
  if ((count > fromsize) || (count > tosize)) {
    ErrorF("wrapcopy too big: count %d, fromsize %d, tosize %d\n",
	   count, fromsize, tosize);
    return;
  }
  if ((fromindex < 0) || (toindex < 0)) {
    ErrorF("wrapcopy bad index: fromindex %d size %d, toindex %d size %d\n",
	   fromindex, fromsize, toindex, tosize);
    return;
  }
  if (fromindex >= fromsize) fromindex %= fromsize;
  if (toindex >= tosize) toindex %= tosize;
  while (count > 0) {
    thistime = min(count, fromsize-fromindex);
    thistime = min(thistime, tosize - toindex);
    memcpy(&to[toindex], &from[fromindex], thistime * sizeof(STEREOSAMP));
    count -= thistime;
    fromindex += thistime;
    toindex += thistime;
    if (fromindex >= fromsize) fromindex = 0;
    if (toindex >= tosize) toindex = 0;
  }
  MB();
}


