/* raster_misc.c - simple raster and frame buffer routines
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
#include <malloc.h>


/* Raster routines. */

/* Allocates a raster.  Returns (struct raster*) 0 on failure. */
struct raster*
raster_alloc( width, height, depth )
    int width, height, depth;
    {
    struct raster* r;

    if ( width <= 0 || height <= 0 || ( depth != 1 && depth != 8 ) )
	return (struct raster*) 0;
    r = (struct raster*) malloc( sizeof(struct raster) );
    if ( r == (struct raster*) 0 )
	return (struct raster*) 0;

    r->width = width;
    r->height = height;
    r->depth = depth;
    r->linelongs = ( ( width * depth + 31 ) / 32 );
    r->pixels = (u_long*) malloc(
	(unsigned) ( height * r->linelongs * sizeof(u_long) ) );
    if ( r->pixels == (u_long*) 0 )
	{
	free( (char*) r );
	return (struct raster*) 0;
	}
    r->flags = 0;
    r->data = (caddr_t) 0;
    return r;
    }

/* Makes a raster that points to a region of another. */
struct raster*
raster_subregion( r, x, y, width, height )
    struct raster* r;
    int x, y, width, height;
    {
    register struct raster* sr;

    if ( x < 0 || y < 0 || width <= 0 || height <= 0 )
	return (struct raster*) 0;	/* bogus args */
    if ( x + width > r->width || y + height > r->height )
	return (struct raster*) 0;	/* out of bounds */

    if ( ( r->depth == 1 && ( x & 31 ) != 0 ) ||
         ( r->depth == 8 && ( x &  3 ) != 0 ) )
	return (struct raster*) 0; /* XXX must stay longword aligned, for now */

    sr = (struct raster*) malloc( sizeof(struct raster) );
    if ( sr == (struct raster*) 0 )
	return (struct raster*) 0;
    sr->width = width;
    sr->height = height;
    sr->depth = r->depth;
    sr->linelongs = r->linelongs;
    sr->pixels = RAS_ADDR( r, x, y );
    sr->flags = RAS_SUBREGION;
    sr->data = (caddr_t) r;		/* not currently used */
    return sr;
    }

/* Turns static data into a raster.  Returns (struct raster*) 0 on failure. */
struct raster*
raster_static( width, height, depth, data )
    int width, height, depth;
    u_long* data;
    {
    struct raster* r;

    if ( width <= 0 || height <= 0 || ( depth != 1 && depth != 8 ) )
	return (struct raster*) 0;
    r = (struct raster*) malloc( sizeof(struct raster) );
    if ( r == (struct raster*) 0 )
	return (struct raster*) 0;

    r->width = width;
    r->height = height;
    r->depth = depth;
    r->linelongs = ( ( width * depth + 31 ) / 32 );
    r->pixels = data;
    r->flags = RAS_STATIC;
    r->data = (caddr_t) 0;
    return r;
    }

/* Allocates a colormap structure, or 0 on failure. */
struct raster_colormap*
raster_colormap_alloc( len )
    register int len;
    {
    register struct raster_colormap* cm;

    cm = (struct raster_colormap*) malloc( sizeof(struct raster_colormap) );
    if ( cm == (struct raster_colormap*) 0 )
	return (struct raster_colormap*) 0;
    cm->ind = 0;
    cm->len = len;
    cm->red = (u_char*) malloc( (unsigned) len );
    if ( cm->red == (u_char*) 0 )
	{
	free( (char*) cm );
	return (struct raster_colormap*) 0;
	}
    cm->grn = (u_char*) malloc( (unsigned) len );
    if ( cm->grn == (u_char*) 0 )
	{
	free( (char*) cm->red );
	free( (char*) cm );
	return (struct raster_colormap*) 0;
	}
    cm->blu = (u_char*) malloc( (unsigned) len );
    if ( cm->blu == (u_char*) 0 )
	{
	free( (char*) cm->red );
	free( (char*) cm->grn );
	free( (char*) cm );
	return (struct raster_colormap*) 0;
	}
    return cm;
    }

/* Frees a colormap. */
void
raster_colormap_free( cm )
    register struct raster_colormap* cm;
    {
    if ( cm != (struct raster_colormap*) 0 )
	{
	free( (char*) cm->red );
	free( (char*) cm->grn );
	free( (char*) cm->blu );
	free( (char*) cm );
	}
    }
