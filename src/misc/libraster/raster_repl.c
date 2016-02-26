/* raster_repl.c - tiling routine for raster library
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

#define min(a,b) ( ((int)(a)) < ((int)(b)) ? (a) : (b) )


/* Tiles the src to fit the dst.  Only implements RAS_SRC. */
int
raster_replsrc( dst, dx, dy, w, h, rop, src, sx, sy )
    struct raster* dst;
    int dx, dy, w, h, rop, sx, sy;
    struct raster* src;
    {
    /* The idea here is to avoid many small rops by doing binary expansion. */
    int tw, th, ntw, nth;

    if ( RAS_GETOP( rop ) != RAS_SRC )
	return -1;

    tw = min( src->width - sx, w );
    th = min( src->height - sy, h );

    if ( raster_op( dst, dx, dy, tw, th, rop, src, sx, sy ) != 0 )
	return -1;

    while ( tw < w || th < h )
	{
	ntw = tw;
	nth = th;
	if ( tw < w )
	    {
	    if ( raster_op( dst, dx+tw, dy, tw, th, rop, dst, dx, dy ) != 0 )
		return -1;
	    ntw = min( tw * 2, w );
	    }
	if ( th < h )
	    {
	    if ( raster_op( dst, dx, dy+th, tw, th, rop, dst, dx, dy ) != 0 )
		return -1;
	    nth = min( th * 2, h );
	    }
	if ( tw < w && th < h )
	    if ( raster_op( dst, dx+tw, dy+th, tw, th, rop, dst, dx, dy ) != 0 )
		return -1;
	tw = ntw;
	th = nth;
	}

    return 0;
    }
