/* $Header: color.h,v 1.2 88/06/30 09:58:56 mikey Exp $ */

/* 
 * color.h - color definitions
 * 
 * Author:	Christopher A. Kent
 * 		Western Research Laboratory
 * 		Digital Equipment Corporation
 * Date:	Sun Dec 13 1987
 * Copyright (c) 1987 Christopher A. Kent
 */

/*
 * $Log:	color.h,v $
 * Revision 1.2  88/06/30  09:58:56  mikey
 * Handles CMY also.
 * 
 * Revision 1.1  88/06/30  09:10:53  mikey
 * Initial revision
 * 
 */

typedef	struct _RGB {
	unsigned short r, g, b;
} RGB;

typedef	struct _HSV {
	float	h, s, v;	/* [0, 1] */
} HSV;

typedef struct _CMY {
	unsigned short c, m, y;
} CMY;

extern RGB	RGBWhite, RGBBlack;

RGB	MixRGB();
RGB	MixHSV();
RGB	HSVToRGB();
HSV	RGBToHSV();
float	RGBDist();
RGB	PctToRGB();
HSV	PctToHSV();
RGB	CMYToRGB();
CMY	RGBToCMY();
