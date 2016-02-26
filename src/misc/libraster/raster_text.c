/* raster_text.c - text routines for raster library
**
** Copyright (C) 1991, 1993 by Jef Poskanzer <jef@netcom.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "raster.h"
#ifdef COLORFONT_CACHE
#include <malloc.h>
#endif /*COLORFONT_CACHE*/

/* The COLORFONT_CACHE option saves copies of color characters as they
** are first rendered, so that the 1-to-8 blit, a relatively expensive
** operation, doesn't have to be repeated.  After the first time, only
** 8-to-8 blits are necessary.  The cache uses maybe 30K and makes color
** text go about three times as fast, so it's a big win.
**
** At one time this module had a similar cache for 1-bit characters,
** storing pre-shifted copies at each different bit offset that was
** called for.  This used over 100K and provided almost no speedup - the
** shift is no big deal, it's the edge masking that takes the time.  So
** that code is gone.
**
** An optimization that would have a big payoff would be switch this
** to use raster_batchop(), and make an optimized version of that as
** described in that file.
*/


/* Draws text.  Returns 0 on success, -1 on failure. */
int
raster_text( r, x, y, rop, rf, text )
    register struct raster* r;
    int x, y;
    int rop;
    struct raster_font* rf;
    char* text;
    {
    return raster_textn( r, x, y, rop, rf, text, strlen( text ) );
    }

/* Draws n characters of text.  Returns 0 on success, -1 on failure. */
int
raster_textn( r, x, y, rop, rf, text, n )
    register struct raster* r;
    int x, y;
    int rop;
    struct raster_font* rf;
    char* text;
    int n;
    {
    int clip;
    int x1, y1;
    struct raster_char* c;
    struct raster* charrast;
    int i;
    register char ch;
    int thisx, thisy;

    /* Check whether we can avoid clipping. */
    clip = 0;
    if ( rf->flags & RASFONT_FIXEDWIDTH &&
	 rf->flags & RASFONT_NOVERTICALMOVEMENT )
	{
	/* This font is well-behaved, we can compute the extent cheaply. */
	c = &(rf->chars['@']);
	charrast = c->r;
	if ( x + c->homex < 0 || y + c->homey < 0 ||
	     x + c->homex + n * c->nextx > r->width ||
	     y + c->homey + charrast->height > r->height )
	    clip = 1;
	}
    else
	{
	/* Got to step through the string to compute the extent. */
	for ( i = 0, x1 = x, y1 = y;
	      i < n;
	      ++i, x1 += c->nextx, y1 += c->nexty )
	    {
	    c = &(rf->chars[text[i]]);
	    charrast = c->r;
	    if ( charrast != (struct raster*) 0 )
		{
		if ( x1 + c->homex < 0 || y1 + c->homey < 0 ||
		     x1 + c->homex + charrast->width > r->width ||
		     y1 + c->homey + charrast->height > r->height )
		    {
		    clip = 1;
		    break;
		    }
		}
	    }
	}

    /* Now display the text. */
    for ( i = 0, x1 = x, y1 = y;
	  i < n;
	  ++i, x1 += c->nextx, y1 += c->nexty )
	{
	ch = text[i];
	c = &(rf->chars[ch]);
	charrast = c->r;
	if ( charrast != (struct raster*) 0 )
	    {
	    thisx = x1 + c->homex;
	    thisy = y1 + c->homey;

#ifdef COLORFONT_CACHE
	    if ( r->depth != 1 )
		{
		/* Initialize color font cache if necessary. */
		struct raster_fontetc* rfe;

		rfe = (struct raster_fontetc*) rf->data;
		if ( rfe == (struct raster_fontetc*) 0 )
		    {
		    rfe = (struct raster_fontetc*) malloc(
			sizeof(struct raster_fontetc) );
		    if ( rfe != (struct raster_fontetc*) 0 )
			{
			rf->data = (caddr_t) rfe;
			rfe->cache = (struct raster_fontcache*) -1;
			}
		    }
		if ( rfe != (struct raster_fontetc*) 0 &&
		     rfe->cache == (struct raster_fontcache*) -1 )
		    {
		    int c;

		    rfe->cache = (struct raster_fontcache*) malloc(
			sizeof(struct raster_fontcache) );
		    if ( rfe->cache != (struct raster_fontcache*) 0 )
			for ( c = 0; c < 256; ++c )
			    rfe->cache->cr[c] = (struct raster*) 0;
		    }

		if ( rfe != (struct raster_fontetc*) 0 &&
		     rfe->cache != (struct raster_fontcache*) 0 )
		    {
		    int color;
		    struct raster* cr;

		    color = RAS_GETCOLOR( rop );
		    cr = rfe->cache->cr[ch];
		    /* Is this character cached yet? */
		    if ( cr != (struct raster*) 0 )
			{
			/* Yes, but is it the right color? */
			if ( rfe->cache->color[ch] == color )
			    {
			    /* Yes - switch rasters. */
			    charrast = cr;
			    }
			else
			    {
			    /* No, re-draw it. */
			    if ( raster_op_noclip(
				    cr, 0, 0, charrast->width, charrast->height,
				    rop, charrast, 0, 0 ) == 0 )
				{
				rfe->cache->color[ch] = color;
				charrast = cr;
				}
			    }
			}
		    else
			{
			/* It's not cached, so cache it. */
			cr = raster_alloc(
			    charrast->width, charrast->height, 8 );
			if ( cr != (struct raster*) 0 )
			    if ( raster_op_noclip(
				    cr, 0, 0, charrast->width, charrast->height,
				    rop, charrast, 0, 0 ) == 0 )
				{
				rfe->cache->color[ch] = color;
				charrast = rfe->cache->cr[ch] = cr;
				}
			}
		    }
		}
#endif /*COLORFONT_CACHE*/

	    if ( clip )
		{
		if ( raster_op(
			r, thisx, thisy, charrast->width, charrast->height,
			rop, charrast, 0, 0 ) < 0 )
		    return -1;
		}
	    else
		{
		if ( raster_op_noclip(
			r, thisx, thisy, charrast->width, charrast->height,
			rop, charrast, 0, 0 ) < 0 )
		    return -1;
		}
	    }
	}

    return 0;
    }

/* Measures n characters of text.  Returns 0 on success, -1 on failure. */
int
raster_textnsize( rf, text, n, xP, yP, wP, hP )
    struct raster_font* rf;
    char* text;
    int n;
    int* xP;
    int* yP;
    int* wP;
    int* hP;
    {
    struct raster_char* c;
    struct raster* charrast;

    if ( rf->flags & RASFONT_FIXEDWIDTH &&
	 rf->flags & RASFONT_NOVERTICALMOVEMENT )
	{
	/* This font is well-behaved, we can compute the extent cheaply. */
	c = &(rf->chars['@']);
	charrast = c->r;
	*xP = c->homex;
	*yP = c->homey;
	*wP = n * c->nextx;
	*hP = charrast->height;
	}
    else
	{
	/* Got to step through the string to compute the extent. */
	int i, x1, y1, left, top, right, bottom, mright, mbottom;

	*xP = 0;
	*yP = 0;
	mright = 0;
	mbottom = 0;
	for ( i = 0, x1 = 0, y1 = 0;
	      i < n;
	      ++i, x1 += c->nextx, y1 += c->nexty )
	    {
	    c = &(rf->chars[text[i]]);
	    charrast = c->r;
	    if ( charrast != (struct raster*) 0 )
		{
		left = x1 + c->homex;
		top = y1 + c->homey;
		right = left + charrast->width;
		bottom = top + charrast->height;
		if ( left < *xP ) *xP = left;
		if ( top < *yP ) *yP = top;
		if ( right > mright ) mright = right;
		if ( bottom > mbottom ) mbottom = bottom;
		}
	    }
	*wP = mright - *xP;
	*hP = mbottom - *yP;
	}

    return 0;
    }

/* Draws text with transparent background.  Returns 0 on success, -1 on
** failure.
*/
int
raster_ttext( r, x, y, rop, rf, text )
    register struct raster* r;
    int x, y;
    int rop;
    struct raster_font* rf;
    char* text;
    {
    return raster_ttextn( r, x, y, rop, rf, text, strlen( text ) );
    }

/* Draws n characters of text with transparent background.  Returns 0 on
** success, -1 on failure.
*/
int
raster_ttextn( r, x, y, rop, rf, text, n )
    register struct raster* r;
    int x, y;
    int rop;
    struct raster_font* rf;
    char* text;
    int n;
    {
    int tx, ty, tw, th;
    struct raster* tr;

    if ( raster_textnsize( rf, text, n, &tx, &ty, &tw, &th ) != 0 )
	return -1;
    tr = raster_alloc( tw, th, 1 );
    if ( tr == (struct raster*) 0 )
	return -1;
    if ( raster_op_noclip(
	    tr, 0, 0, tw, th, RAS_CLEAR, (struct raster*) 0, 0, 0 ) != 0 )
	{
	raster_free( tr );
	return -1;
	}
    if ( raster_textn( tr, -tx, -ty, RAS_SRC, rf, text, n ) != 0 )
	{
	raster_free( tr );
	return -1;
	}
    if ( raster_stencil(
	    r, x + tx, y + ty, tw, th, rop, tr, 0, 0, tr, 0, 0 ) != 0 )
	{
	raster_free( tr );
	return -1;
	}
    raster_free( tr );
    return 0;
    }
