/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *		Maynard, Massachusetts
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

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/mycolor.c,v 1.3 1994/06/03 17:27:31 jg Exp $*/

#include <math.h>
#include "mycolor.h"

#define MIN(a, b) ((a > b) ? b : a)
#define MAX(a, b) ((a < b) ? b : a)

static RGB White = { 1.0, 1.0, 1.0 };
static RGB Black = { 0.0, 0.0, 0.0 };


static double Ry =  0.2390;
static double Rp =  0.0470;
static double Rq =  0.0170;
static double Gy =  0.6860;
static double Gp = -0.0500;
static double Gq =  0.0180;
static double By =  0.0750;
static double Bp =  0.0030;
static double Bq = -0.0350;

static double Pr;
static double Pg;
static double Pb;
static double Qr;
static double Qg;
static double Qb;

RGB_InitColor()
{
  Pr = -(Gy*Bq - Gq*By);
  Pg =  (Ry*Bq - Rq*By);
  Pb = -(Ry*Gq - Rq*Gy);
  Qr =  (Gy*Bp - Gp*By);
  Qg = -(Ry*Bp - Rp*By);
  Qb =  (Ry*Gp - Rp*Gy);
}

RGB RGB_Grey(double y)
{
  RGB c;
  c.r = y;
  c.g = y;
  c.b = y;
  return(c);
}


RGB RGB_FromHue(double h)
{
  RGB c;
  double p, q, ma, mi;
  p = cos(h * M_PI * 2.0);
  q = sin(h * M_PI * 2.0);
  c.r = p * Pr +  q * Qr;
  c.g = p * Pg +  q * Qg;
  c.b = p * Pb +  q * Qb;
  mi = MIN(c.r, MIN(c.g, c.b));
  c.r = c.r - mi;
  c.g = c.g - mi;
  c.b = c.b - mi;
  ma = MAX(c.r, MAX(c.g, c.b));
  c.r = c.r/ma;
  c.g = c.g/ma;
  c.b = c.b/ma;
  return(c);
}


RGB RGB_Mix(RGB a, double alpha, RGB b, double beta)
{
  a.r = alpha * a.r + beta * b.r;
  a.g = alpha * a.g + beta * b.g;
  a.b = alpha * a.b + beta * b.b;
  return(a);
}

RGB RGB_Interpolate(RGB a, double alpha, RGB b, double beta)
{
  double w;
  w = alpha + beta; alpha = alpha/w; beta = beta/w;
  a.r = beta * a.r + alpha * b.r;
  a.g = beta * a.g + alpha * b.g;
  a.b = beta * a.b + alpha * b.b;
  return (a);
}

RGB RGB_RGBFromHSV(HSV hsv)
{
  RGB pure, rgb;
  double sat, lum;
  pure = RGB_FromHue(hsv.h);

  sat = MIN(1.0, MAX(0.0, hsv.s));
  rgb = RGB_Mix(pure, sat, RGB_Grey(0.5), 1.0 - sat);
  lum = MIN(1.0, MAX(0.0, hsv.v));
  if (lum > 0.5)
  {
    rgb = RGB_Mix(White, (lum - 0.5)/0.5, rgb, (1.0 - lum)/0.5);
  }
  else if (lum < 0.5)
  {
    rgb =  RGB_Mix(Black, (0.5 - lum)/0.5, rgb, (lum - 0.0)/0.5);
  }
  return( rgb);
}


