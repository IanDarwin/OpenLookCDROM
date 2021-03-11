/* Copyright 1992 Carnegie Mellon University All rights reserved.
  $Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
 */
 
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/cmenu/RCS/shadows.c,v 1.1 1993/02/15 17:55:13 rr2b Exp $";

#include <shadows.h>

static void Lighten(br, bg, bb, rr, rg, rb, inc)
unsigned short br, bg, bb;
unsigned short *rr, *rg, *rb;
unsigned short inc;
{
    int r, g, b;
    r=((int)inc) + ((int)br);
    g=((int)inc) + ((int)bg);
    b=((int)inc) + ((int)bb);
    if(r>65535) r=65535;
    if(g>65535) g=65535;
    if(b>65535) b=65535;
    *rr=(unsigned short)r;
    *rg=(unsigned short)g;
    *rb=(unsigned short)b;
}

static void Darken(br, bg, bb, rr, rg, rb, inc)
unsigned short br, bg, bb;
unsigned short *rr, *rg, *rb;
unsigned short inc;
{
    int r, g, b;
    r=((int)br) - ((int)inc);
    g=((int)bg) - ((int)inc);
    b=((int)bb) - ((int)inc);
    if(r<0) r=0;
    if(g<0) g=0;
    if(b<0) b=0;
    *rr=(unsigned short)r;
    *rg=(unsigned short)g;
    *rb=(unsigned short)b;
}

static unsigned short lite=62258 /* 95% */, dark=6553 /* 10% */;
static unsigned short litedarkentop=3276 /* 5% */, litedarkenbottom=16384 /* 25% */;
static unsigned short darklightentop=26214 /* 40% */, darklightenbottom=13107 /* 20% */;
static unsigned short mediumlightentop=9830 /* 15% */, mediumdarkenbottom=9830 /* 15% */;
static unsigned short litedarkenpressed=6553;
static unsigned short mediumdarkenpressed=3277;
static unsigned short darklightenpressed=19660;

void shadows_SetPreferences(getint)
int (*getint)();
{
    lite=(getint("ShadowsLiteVal", 95)*65535)/100;
    dark=(getint("ShadowsDarkVal", 10)*65535)/100;
    litedarkentop=( getint("ShadowsLiteDarkenTop", 5)*65535)/100;
    litedarkenbottom=( getint("ShadowsLiteDarkenBottom", 25)*65535)/100;
    darklightentop=( getint("ShadowsDarkLightenTop", 40)*65535)/100;
    darklightenbottom=( getint("ShadowsDarkLightenBottom", 20)*65535)/100;
    mediumlightentop=( getint("ShadowsMediumLightenTop", 15)*65535)/100;
    mediumdarkenbottom=( getint("ShadowsMediumDarkenBottom", 15)*65535)/100;
    litedarkenpressed =(getint("ShadowsLiteDarkenPressed", 10)*65535)/100;
    mediumdarkenpressed=(getint("ShadowsMediumDarkenPressed", 5)*65535)/100;
    darklightenpressed=(getint("ShadowsDarkLightenPressed", 30)*65535)/100;
}

void shadows_ComputeColor(br, bg, bb, rr, rg, rb, color)
unsigned short br, bg, bb;
unsigned short *rr, *rg, *rb;
int color;
{
    unsigned long lum;
    lum=(30*br/100)+ (59*bg/100) + (11*bb/100);
    if(lum > lite) {
	switch(color) {
	    case shadows_PRESSED:
		Darken(br, bg, bb, rr, rg, rb, litedarkenpressed);
		break;
	    case shadows_TOPSHADOW:
		Darken(br, bg, bb, rr, rg, rb, litedarkentop);
		break;
	    case shadows_BOTTOMSHADOW:
		Darken(br, bg, bb, rr, rg, rb, litedarkenbottom);
		break;	    
	}
    } else if(lum < dark) {
	switch(color) {
	    case shadows_PRESSED:
		Lighten(br, bg, bb, rr, rg, rb, darklightenpressed);
		break;
	    case shadows_TOPSHADOW:
		Lighten(br, bg, bb, rr, rg, rb, darklightentop);
		break;
	    case shadows_BOTTOMSHADOW:
		Lighten(br, bg, bb, rr, rg, rb, darklightenbottom);
		break;	    
	}
    } else {
	switch(color) {
	    case shadows_PRESSED:
		Darken(br, bg, bb, rr, rg, rb, mediumdarkenpressed);
		break;
	    case shadows_TOPSHADOW:
		Lighten(br, bg, bb, rr, rg, rb, mediumlightentop);
		break;
	    case shadows_BOTTOMSHADOW:
		Darken(br, bg, bb, rr, rg, rb, mediumdarkenbottom);
		break;	    
	}
    }
}
