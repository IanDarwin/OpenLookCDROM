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

#include "AFUtils.h"
#include <math.h>

#define LIMIT(low, x, high) \
  ((((x) < low)) ? (low) : (((x) < (high)) ? (x) : (high)))

/* generate a floating point tone into a buffer, with a given peak value.
   The initial phase is given and the final phase is returned.  
   (With the phase represented as a number in [0..1024). The value is added
   into the buffer.   The "f" parameter should be frequency divided
   by sample per second. - necessarily in the range 0 to 0.5 to avoid
   aliasing. */

double AFSingleTone(double freq, double peak, double phase,
	    float *buffer, int length)
{
  if (phase < 0.0) phase = 0.0;  /* really an error */
  else if (phase >= 1024.0) phase = 0.0;  /* really an error */
  freq = LIMIT(0.0, freq, 0.5);
  freq *= 1024.0;   /* convert to phase increment */
  while (length--)
    {
      *buffer++ += AF_sine_float[(int) phase] * peak;
      phase += freq;
      if (phase >= 1024.0) phase -= 1024.0;
    }
  return (phase);
}
