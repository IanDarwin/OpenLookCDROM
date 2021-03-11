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

/* mycolor.h
 */

/*$Header: /crl/audio/AF/contrib/clients/afft/RCS/mycolor.h,v 1.3 1994/06/03 17:27:31 jg Exp $*/

typedef struct _HSV {
  double h, s, v;
  } HSV;

typedef struct _RGB {
  double r, g, b;
  } RGB;


extern RGB_InitColor();
extern RGB RGB_Grey(double y);
extern RGB RGB_FromHue(double h);
extern RGB RGB_Mix(RGB a, double alpha, RGB b, double beta);
extern RGB RGB_Interpolate(RGB a, double alpha, RGB b, double beta);
extern RGB RGB_RGBFromHSV(HSV hsv);
