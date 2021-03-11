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
#include <AF/AFUtils.h>
#include <math.h>

/*
  Compute power for signal in buffer.  Currently supports scalar formats only.
*/  
double AFPower(unsigned char *buf, int nUnits, AEncodeType fmt)
{
  double sum = 0.0;
  double f;
  int i;
  if (nUnits >= 0) 
  {
    switch (fmt) {
    case MU255:
      for (i = 0; i < nUnits; i += 1) sum += (double) AF_power_uf[buf[i]];
      break;
    case ALAW:
      for (i = 0; i < nUnits; i += 1) sum += (double) AF_power_af[buf[i]];
      break;
    case LIN16:
      for (i = 0; i < nUnits; i += 1) {
	f = ((double) ((AF_INT16 *) buf)[i]) * (1.0 / 32768.0);
	sum += f * f;
      }
      break;
    case LIN32:
      for (i = 0; i < nUnits; i += 1) {
	f = ((double) ((AF_INT32 *) buf)[i]) * (1.0 / 2147483648.0);
	sum += f * f;
      }
      break;
    case IEEES:
      for (i = 0; i < nUnits; i += 1) {
	f = ((double) ((float *) buf)[i]);
	sum += f * f;
      }
      break;
    case IEEED:
      for (i = 0; i < nUnits; i += 1) {
	f = ((double *) buf)[i];
	sum += f * f;
      }
      break;
    default: return(0.0);
    }
    sum /= (double) nUnits;
  }
  return(sum);
}




