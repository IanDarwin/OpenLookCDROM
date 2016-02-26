/* reduce.c - quantize the colors in a pixmap down to a specified number
**
** Copyright (C) 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include <stdio.h>
#include <X11/X.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include <X11/XWDFile.h>
#include "mem.h"

#define MAXCOLORS 32767

/* #define LARGE_NORM */
#define LARGE_LUM

/* #define REP_CENTER_BOX */
/* #define REP_AVERAGE_COLORS */
#define REP_AVERAGE_PIXELS

typedef struct box* box_vector;
struct box
{
    int ind;
    int colors;
    int sum;
};

typedef struct
{
    long        value;
    XColor      *color;
} ColorHist;

typedef ColorHist *colorhist_vector;

int VDMPixCmp (el1, el2)
ColorHist *el1, *el2;
{
/* Sort into DESCENDING order */
    return (el2->value - el1->value);
}

colorhist_vector
ComputeColorHist(image, maxcolors, pal, pNcolors)
XImage *image;
XColor *pal;
int maxcolors, *pNcolors;

{
    register int x, y, i;
    colorhist_vector hist;

    *pNcolors = 0;

    hist = (colorhist_vector) MemAllocN (sizeof(ColorHist) * maxcolors);

/* Initialize histogram */
    for (i=0; i<maxcolors; i++)
    {
	hist[i].value = 0;
	hist[i].color = &pal[i];
    }

/* Build histogram of color use */
    for (y = 0; y < image->height; y++)
	for (x = 0; x < image->width; x++)
	{
	    if (!hist[XGetPixel(image, x, y)].value++)
		(*pNcolors)++;
	}

/* Sort the histogram so all occurring values come first */
    qsort ((char*)hist, maxcolors, sizeof (hist[0]), VDMPixCmp);

    return hist;
}

static int
redcompare( ch1, ch2 )
colorhist_vector ch1, ch2;
{
    return (int) ch1->color->red - (int) ch2->color->red;
}

static int
greencompare( ch1, ch2 )
colorhist_vector ch1, ch2;
{
    return (int) ch1->color->green - (int) ch2->color->green;
}

static int
bluecompare( ch1, ch2 )
colorhist_vector ch1, ch2;
{
    return (int) ch1->color->blue - (int) ch2->color->blue;
}

static int
sumcompare( b1, b2 )
box_vector b1, b2;
{
    return b2->sum - b1->sum;
}

/*
** Here is the fun part, the median-cut colormap generator.  This is based
** on Paul Heckbert's paper "Color Image Quantization for Frame Buffer
** Display", SIGGRAPH '82 Proceedings, page 297.
*/

#if __STDC__
static XColor*
mediancut(colorhist_vector chv, int colors, int sum, unsigned short maxval, int
newcolors )
#else /*__STDC__*/
static XColor*
mediancut( chv, colors, sum, maxval, newcolors )
    colorhist_vector chv;
    int colors, sum, newcolors;
    unsigned short maxval;
#endif /*__STDC__*/
{
    XColor *colormap;
    box_vector bv;
    register int bi, i;
    int boxes;

    bv = (box_vector) MemAllocN( sizeof(struct box) * newcolors );
    colormap =
	(XColor*) MemAlloc( sizeof(XColor) * newcolors );

    /*
    ** Set up the initial box.
    */
    bv[0].ind = 0;
    bv[0].colors = colors;
    bv[0].sum = sum;
    boxes = 1;

    /*
    ** Main loop: split boxes until we have enough.
    */
    while ( boxes < newcolors )
    {
	register int indx, clrs;
	int sm;
	register int minr, maxr, ming, maxg, minb, maxb, v;
	int halfsum, lowersum;

	/*
	** Find the first splittable box.
	*/
	for ( bi = 0; bi < boxes; ++bi )
	    if ( bv[bi].colors >= 2 )
		break;
	if ( bi == boxes )
	    break;      /* ran out of colors! */
	indx = bv[bi].ind;
	clrs = bv[bi].colors;
	sm = bv[bi].sum;

	/*
	** Go through the box finding the minimum and maximum of each
	** component - the boundaries of the box.
	*/
	minr = maxr = chv[indx].color->red;
	ming = maxg = chv[indx].color->green;
	minb = maxb = chv[indx].color->blue;
	for ( i = 1; i < clrs; ++i )
	{
	    v = chv[indx + i].color->red;
	    if ( v < minr ) minr = v;
	    if ( v > maxr ) maxr = v;
	    v = chv[indx + i].color->green;
	    if ( v < ming ) ming = v;
	    if ( v > maxg ) maxg = v;
	    v = chv[indx + i].color->blue;
	    if ( v < minb ) minb = v;
	    if ( v > maxb ) maxb = v;
	}

	/*
	** Find the largest dimension, and sort by that component.  I have
	** included two methods for determining the "largest" dimension;
	** first by simply comparing the range in RGB space, and second
	** by transforming into luminosities before the comparison.  You
	** can switch which method is used by switching the commenting on
	** the LARGE_ defines at the beginning of this source file.
	*/
#ifdef LARGE_NORM
	if ( maxr - minr >= maxg - ming && maxr - minr >= maxb - minb )
	    qsort(
		(char*) &(chv[indx]), clrs, sizeof(ColorHist),
		redcompare );
	else if ( maxg - ming >= maxb - minb )
	    qsort(
		(char*) &(chv[indx]), clrs, sizeof(ColorHist),
		greencompare );
	else
	    qsort(
		(char*) &(chv[indx]), clrs, sizeof(ColorHist),
		bluecompare );
#endif /*LARGE_NORM*/
#ifdef LARGE_LUM
	{
	float rl, gl, bl;

	rl = (maxr - minr) * 0.299;
	gl = (maxg - ming) * 0.587;
	bl = (maxb - minb) * 0.114;

	if ( rl >= gl && rl >= bl )
	    qsort(
		(char*) &(chv[indx]), clrs, sizeof(ColorHist),
		redcompare );
	else if ( gl >= bl )
	    qsort(
		(char*) &(chv[indx]), clrs, sizeof(ColorHist),
		greencompare );
	else
	    qsort(
		(char*) &(chv[indx]), clrs, sizeof(ColorHist),
		bluecompare );
	}
#endif /*LARGE_LUM*/

	/*
	** Now find the median based on the counts, so that about half the
	** pixels (not colors, pixels) are in each subdivision.
	*/
	lowersum = chv[indx].value;
	halfsum = sm / 2;
	for ( i = 1; i < clrs - 1; ++i )
	    {
	    if ( lowersum >= halfsum )
		break;
	    lowersum += chv[indx + i].value;
	    }

	/*
	** Split the box, and sort to bring the biggest boxes to the top.
	*/
	bv[bi].colors = i;
	bv[bi].sum = lowersum;
	bv[boxes].ind = indx + i;
	bv[boxes].colors = clrs - i;
	bv[boxes].sum = sm - lowersum;
	++boxes;
	qsort( (char*) bv, boxes, sizeof(struct box), sumcompare );
    }

    /*
    ** Ok, we've got enough boxes.  Now choose a representative color for
    ** each box.  There are a number of possible ways to make this choice.
    ** One would be to choose the center of the box; this ignores any structure
    ** within the boxes.  Another method would be to average all the colors in
    ** the box - this is the method specified in Heckbert's paper.  A third
    ** method is to average all the pixels in the box.  You can switch which
    ** method is used by switching the commenting on the REP_ defines at
    ** the beginning of this source file.
    */
    for ( bi = 0; bi < boxes; ++bi )
    {
	register int indx = bv[bi].ind;
	register int clrs = bv[bi].colors;
	register long r = 0, g = 0, b = 0, sum = 0;

	for ( i = 0; i < clrs; ++i )
	{
	    r += chv[indx + i].color->red * chv[indx + i].value;
	    g += chv[indx + i].color->green * chv[indx + i].value;
	    b += chv[indx + i].color->blue * chv[indx + i].value;
	    sum += chv[indx + i].value;
	}
	r = r / sum;
	if ( r > (long)maxval ) r = maxval;     /* avoid math errors */
	g = g / sum;
	if ( g > (long)maxval ) g = maxval;
	b = b / sum;
	if ( b > (long)maxval ) b = maxval;
	colormap[bi].red = r;
	colormap[bi].green = g;
	colormap[bi].blue = b;
    }

    /*
    ** All done.
    */
    MemFree (bv);

    return colormap;
}

XColor *
reduce (image, ncolors, pal, n)
XImage *image;
int ncolors;
XColor *pal;
int n;

{
    colorhist_vector chv;
    XColor *newpal;
    int colors, i;
/*
** Step 1: attempt to make a histogram of the colors
*/
    chv = ComputeColorHist(image, ncolors, pal, &colors );

/*
** Step 3: apply median-cut to histogram, making the new colormap.
*/
    newpal = mediancut(chv, colors, image->width * image->height, 0xffff, n);
    MemFree (chv);

    return newpal;
}
