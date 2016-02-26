/* raster_stencil.c - stenciling routine for raster library
**
** Copyright (C) 1993 by Jef Poskanzer <jef@netcom.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "raster.h"

int
raster_stencil( dst, dx, dy, w, h, rop, sten, stx, sty, src, sx, sy )
    struct raster* dst;
    int dx, dy, w, h, rop, stx, sty, sx, sy;
    struct raster* sten;
    struct raster* src;
    {
    struct raster* temp;

    if ( sten->depth != 1 )
	return -1;

    if ( dx < 0 )
	{
	w += dx;
	stx -= dx;
	sx -= dx;
	dx = 0;
	}
    if ( dy < 0 )
	{
	h += dy;
	sty -= dy;
	sy -= dy;
	dy = 0;
	}
    if ( stx < 0 )
	{
	w += stx;
	dx -= stx;
	sx -= stx;
	stx = 0;
	}
    if ( sty < 0 )
	{
	h += sty;
	dy -= sty;
	sy -= sty;
	sty = 0;
	}
    if ( src != (struct raster*) 0 )
	{
	if ( sx < 0 )
	    {
	    w += sx;
	    dx -= sx;
	    stx -= sx;
	    sx = 0;
	    }
	if ( sy < 0 )
	    {
	    h += sy;
	    dy -= sy;
	    sty -= sy;
	    sy = 0;
	    }
	}

    if ( dx + w > dst->width )
	w = dst->width - dx;
    if ( dy + h > dst->height )
	h = dst->height - dy;
    if ( stx + w > sten->width )
	w = sten->width - stx;
    if ( sty + h > sten->height )
	h = sten->height - sty;
    if ( src != (struct raster*) 0 )
	{
	if ( sx + w > src->width )
	    w = src->width - sx;
	if ( sy + h > src->height )
	    h = src->height - sy;
	}

    if ( w <= 0 || h <= 0 )
	return 0;

    temp = raster_alloc( w, h, dst->depth );
    if ( temp == (struct raster*) 0 )
	return -1;

    /* temp = dst */
    if ( raster_op_noclip( temp, 0, 0, w, h, RAS_SRC, dst, dx, dy ) != 0 )
	{
	raster_free( temp );
	return -1;
	}

    /* temp = dst rop src */
    if ( raster_op_noclip( temp, 0, 0, w, h, rop, src, sx, sy ) != 0 )
	{
	raster_free( temp );
	return -1;
	}

    /* temp = ( dst rop src ) ^ dst */
    if ( raster_op_noclip( temp, 0, 0, w, h, RAS_XOR, dst, dx, dy ) != 0 )
	{
	raster_free( temp );
	return -1;
	}

    /* temp = ( ( dst rop src ) ^ dst ) & sten */
    if ( raster_op_noclip( temp, 0, 0, w, h, RAS_AND, sten, stx, sty ) != 0 )
	{
	raster_free( temp );
	return -1;
	}

    /* dst = ( ( ( dst rop src ) ^ dst ) & sten ) ^ dst */ 
    if ( raster_op_noclip( dst, dx, dy, w, h, RAS_XOR, temp, 0, 0 ) != 0 )
	{
	raster_free( temp );
	return -1;
	}

    raster_free( temp );
    return 0;
    }
