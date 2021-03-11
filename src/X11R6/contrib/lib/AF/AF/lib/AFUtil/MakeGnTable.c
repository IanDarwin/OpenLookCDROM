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

/* fill in a 256 byte u-law to u-law table that changes the gain of a signal
    by gain */

#include <math.h>
#include "AFUtils.h"

#define	LIMIT(low, val, high) \
	   if (val < (low)) val = (low); \
	   else \
	   if ((val) > (high)) val = (high);


void AFMakeGainTableU(unsigned char *table, double dBgain)
{
  int i, lin;
  double gf = AFdBtoLin(dBgain);
  for( i = 0; i < 256; i += 1)
    {
      lin = (int)(AF_exp_u[i] * gf); 
      LIMIT(-8192, lin, 8191);
      table[i] = AF_comp_u[0x3fff & lin];
    }
}

void AFMakeGainTableA(unsigned char *table, double dBgain)
{
  int i, lin;
  double gf = AFdBtoLin(dBgain);
  for( i = 0; i < 256; i += 1)
    {
      lin = (int)(AF_exp_a[i] * gf); 
      LIMIT(-8192, lin, 8191);
      table[i] = AF_comp_a[0x3fff & lin];
    }
}
