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

/* generate a u-law tone pair into a buffer.  The
   two frequencies are f1 and f2.  Each tone will be dBgain
   down from the "digital milliwatt", which in turn is 3.16 dB
   down from digital clipping level.  The tones will ramp up to
   full volume in gainramp samples and ramp down at the end. */


int AFTonePair(double f1, double dBgain1, double f2, double dBgain2,
		double sampleRate, AEncodeType encodeType,
		int gainramp, unsigned char *buffer, int length)
{
  int i;
  double p1, p2, p1idx, p2idx;
  double ac, gain1, gain2, gainstep1, gainstep2;
  if (sampleRate < 10.0) return (0);
  p1 = 0.0;
  p2 = 0.0;
  ac = 1024.0 / sampleRate;
/*
 *
 *	Certified Lo-Fi to pt. 68 and DOC at DS&G. 
 *******************************************************************
 *****  WARNING WARNING WARNING WARNING WARNING WARNING WARNING ****
 *******************************************************************
 * These parameters have been set during testing for compliance with 
 * DOC and FCC Pt. 68.  These parameters should not be changed 
 * without awareness of the pertinent regulatory issues.
 */
/****************** FCC Part 68 ******************************************/
  if ((f1 > 2450.0) && (f1 < 2750.0)) f1 = 2750.0;
  if ((f2 > 2450.0) && (f2 < 2750.0)) f2 = 2750.0;
/****************** FCC Part 68 ******************************************/
  f1 = LIMIT(0.0, f1, sampleRate / 2.0);
  f2 = LIMIT(0.0, f2, sampleRate / 2.0);
  dBgain1 = LIMIT(-100.0, dBgain1, 20.0);
  dBgain2 = LIMIT(-100.0, dBgain2, 20.0);
  p1idx = ac * f1;
  p2idx = ac * f2;
  gainstep1 = AFdBtoLin(dBgain1 - 3.16);
  gainstep2 = AFdBtoLin(dBgain2 - 3.16);
  if (gainramp < 1) gainramp = 1;
  gainstep1 = gainstep1 / (double) gainramp;
  gainstep2 = gainstep2 / (double) gainramp;
  gain1 = gain2 = 0.0;
  for (i = 0; i < length; i += 1)
    {
      if (i < gainramp) 
	{
	  gain1 += gainstep1;
	  gain2 += gainstep2;
	}
      else if (i > (length - gainramp)) 
	{
	  gain1 -= gainstep1;
	  gain2 -= gainstep2;
	}
      ac = (AF_sine_float[(int) p1] * gain1) + 
	(AF_sine_float[(int) p2] * gain2);
      if (ac > 1.0) ac = 1.0;
      else if (ac < -1.0) ac = -1.0;
      switch (encodeType) {
      case MU255: buffer[i] = AF_comp_u[((int) (ac * 8191.0)) & 0x3fff];
	break;
      case ALAW: buffer[i] = AF_comp_a[((int) (ac * 8191.0)) & 0x3fff];
	break;
      case LIN16: ((short int *) buffer)[i] = (short int) (ac * 32767.0);
	break;
      case LIN32: ((int *) buffer)[i] = (int) (ac * 2147483647.0);
	break;
      case IEEES: ((float *) buffer)[i] = ac;
	break;
      case IEEED: ((double *) buffer)[i] = ac;
	break;
      default: return (0);
      }
      p1 = p1 + p1idx;
      if (p1 >= 1024.0) p1 -= 1024.0;
      p2 = p2 + p2idx;
      if (p2 >= 1024.0) p2 -= 1024.0;
    }
  return (1);
}
