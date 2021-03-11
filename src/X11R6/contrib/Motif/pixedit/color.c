/* $Header: color.c,v 1.2 88/06/30 09:58:36 mikey Exp $ */

/* 
 * color.c - color helper routines
 * 
 * Author:	Christopher A. Kent
 * 		Western Research Laboratory
 * 		Digital Equipment Corporation
 * Date:	Sun Dec 13 1987
 * Copyright (c) 1987 Christopher A. Kent
 */

/* 
 * See David F. Rogers, "Procedural Elements for Computer Graphics",
 * McGraw-Hill, for the theory behind these routines.
 */

/*
 * $Log:	color.c,v $
 * Revision 1.2  88/06/30  09:58:36  mikey
 * Handles CMY also.
 * 
 * Revision 1.1  88/06/30  09:10:32  mikey
 * Initial revision
 * 
 */

static char rcs_ident[] = "$Header: color.c,v 1.2 88/06/30 09:58:36 mikey Exp $";

#include <X11/Xlib.h>
#include "color.h"

#define	MAX_INTENSITY	65535			    /* for X11 */

#define	MIN(a, b)   ((a)<(b)?(a):(b))
#define	MAX(a, b)   ((a)>(b)?(a):(b))
#define	ABS(x)	    ((x)<0?-(x):(x))

RGB	RGBWhite = { MAX_INTENSITY, MAX_INTENSITY, MAX_INTENSITY };
RGB	RGBBlack = { 0, 0, 0 };

/*
 * Mix two RGBs, with scale factors alpha and beta, in RGB space.
 */

RGB
MixRGB(r, alpha, s, beta)
RGB	r, s;
float	alpha, beta;
{
	RGB	t;

	t.r = MAX(0, MIN(MAX_INTENSITY, (int)(alpha*(r.r) + beta*(s.r))));
	t.g = MAX(0, MIN(MAX_INTENSITY, (int)(alpha*(r.g) + beta*(s.g))));
	t.b = MAX(0, MIN(MAX_INTENSITY, (int)(alpha*(r.b) + beta*(s.b))));
	return t;
}

/*
 * Mix two RGBs with scale factors alpha and beta, in HSV space.
 */

RGB
MixHSV(r, alpha, s, beta)
RGB	r, s;
float	alpha, beta;
{
	HSV	rr, ss, tt;

	rr = RGBToHSV(r);
	ss = RGBToHSV(s);
	tt.h = alpha*rr.h + beta*ss.h;
	if (ABS(rr.h - ss.h) > 0.5) {
		tt.h = tt.h + 0.5;
		if (tt.h >= 1.0)
			tt.h = tt.h - 1.0;
	}
	tt.s = alpha*rr.s + beta*ss.s;
	tt.v = alpha*rr.v + beta*ss.v;
	return HSVToRGB(tt);
}

/*
 * Convert an HSV to an RGB.
 */

RGB
HSVToRGB(hsv)
HSV	hsv;
{
	RGB	rgb;
	float	p, q, t, f;
	int	i;
	
	if (hsv.s == 0.0)
		rgb = PctToRGB(hsv.v, hsv.v, hsv.v);
	else {
		if (hsv.s > 1.0)
			hsv.s = 1.0;
		if (hsv.s < 0.0)
			hsv.s = 0.0;
		if (hsv.v > 1.0)
			hsv.v = 1.0;
		if (hsv.v < 0.0)
			hsv.v = 0.0;
		if (hsv.h >= 1.0)
			hsv.h = 0.0;

		hsv.h = 6.0 * hsv.h;
		i = (int) hsv.h;
		f = hsv.h - (float) i;
		p = hsv.v * (1.0 - hsv.s);
		q = hsv.v * (1.0 - (hsv.s * f));
		t = hsv.v * (1.0 - (hsv.s * (1.0 - f)));

		switch(i) {
		case 0:	rgb = PctToRGB(hsv.v, t, p); break;
		case 1:	rgb = PctToRGB(q, hsv.v, p); break;
		case 2:	rgb = PctToRGB(p, hsv.v, t); break;
		case 3:	rgb = PctToRGB(p, q, hsv.v); break;
		case 4:	rgb = PctToRGB(t, p, hsv.v); break;
		case 5:	rgb = PctToRGB(hsv.v, p, q); break;
		}
	}
	return rgb;
}

/*
 * Convert an RGB to HSV.
 */

HSV
RGBToHSV(rgb)
RGB	rgb;

{
	HSV	hsv;
	float	rr, gg, bb;
	float	min, max;
	float	rc, gc, bc;
	
	rr = (float) rgb.r / (float) MAX_INTENSITY;
	gg = (float) rgb.g / (float) MAX_INTENSITY;
	bb = (float) rgb.b / (float) MAX_INTENSITY;
	
	max = MAX(MAX(rr, gg), bb);
	min = MIN(MIN(rr, gg), bb);
	hsv.v = max;
	if (max == 0.0)
		hsv.s = 0.0;
	else
		hsv.s = (max - min) / max;
	if (hsv.s == 0.0)
		hsv.h = 0.0;
	else {
		rc = (max - rr) / (max - min);
		gc = (max - gg) / (max - min);
		bc = (max - bb) / (max - min);
		if (rr == max)
			hsv.h = bc - gc;
		else if (gg == max)
			hsv.h = 2.0 + rc - bc;
		else if (bb == max)
			hsv.h = 4.0 + gc - rc;

		if (hsv.h < 0.0)
			hsv.h += 6.0;
		hsv.h = hsv.h / 6.0;
	}
	return hsv;
}

/*
 * Intensity percentages to RGB.
 */

RGB
PctToRGB(rr, gg, bb)
float	rr, gg, bb;
{
	RGB	rgb;
	
	if (rr > 1.0)
		rr = 1.0;
	if (gg > 1.0)
		gg = 1.0;
	if (bb > 1.0)
		bb = 1.0;
	
	rgb.r = (int)(0.5 + rr * MAX_INTENSITY);
	rgb.g = (int)(0.5 + gg * MAX_INTENSITY);
	rgb.b = (int)(0.5 + bb * MAX_INTENSITY);
	return rgb;
}

/*
 * Intensity percentages to HSV.
 */

HSV
PctToHSV(hh, ss, vv)
float	hh, ss, vv;
{
	HSV	hsv;

	if (hh > 1.0)
		hh = 1.0;
	if (ss > 1.0)
		ss = 1.0;
	if (vv > 1.0)
		vv = 1.0;

	hsv.h = hh;
	hsv.s = ss;
	hsv.v = vv;
	return hsv;
}

/*
 * The Manhattan distance between two colors, between 0.0 and 3.0.
 */

float
RGBDist(r, s)
RGB	r, s;
{
	return (
	    ABS((float)(r.r - s.r)) +
	    ABS((float)(r.g - s.g)) +
	    ABS((float)(r.b - s.b))) / (float)MAX_INTENSITY;
}

/*
 * Load an XColor with an RGB.
 */

RGBToXColor(r, x)
RGB	r;
XColor	*x;
{
	x->red = r.r;
	x->green = r.g;
	x->blue = r.b;
	x->flags = DoRed | DoGreen | DoBlue;
}

/*
 * Convert a CMY to RGB.
 */

RGB
CMYToRGB(cmy)
CMY	cmy;

{
	RGB	rgb;

	rgb.r = MAX_INTENSITY - cmy.c;
	rgb.g = MAX_INTENSITY - cmy.m;
	rgb.b = MAX_INTENSITY - cmy.y;
	return rgb;
}

/*
 * Convert an RGB to CMY.
 */

CMY
RGBToCMY(rgb)
RGB	rgb;

{
	CMY	cmy;

	cmy.c = MAX_INTENSITY - rgb.r;
	cmy.m = MAX_INTENSITY - rgb.g;
	cmy.y = MAX_INTENSITY - rgb.b;
	return cmy;
}
