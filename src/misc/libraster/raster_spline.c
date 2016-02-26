/* raster_spline.c - spline routine for raster library
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

/* Routines. */

#define SPLINE_THRESH 3

/* Draws a three-point spline into a raster.  Returns 0 on success,
** -1 on failure.
*/
int
raster_spline3( r, x0, y0, x1, y1, x2, y2, rop )
    struct raster* r;
    int x0, y0, x1, y1, x2, y2;
    int rop;
    {
    register int xa, ya, xb, yb, xc, yc, xp, yp;

    xa = ( x0 + x1 ) / 2;
    ya = ( y0 + y1 ) / 2;
    xc = ( x1 + x2 ) / 2;
    yc = ( y1 + y2 ) / 2;
    xb = ( xa + xc ) / 2;
    yb = ( ya + yc ) / 2;

    xp = ( x0 + xb ) / 2;
    yp = ( y0 + yb ) / 2;
    if ( abs( xa - xp ) + abs( ya - yp ) > SPLINE_THRESH )
	(void) raster_spline3( r, x0, y0, xa, ya, xb, yb, rop );
    else
	(void) raster_line( r, x0, y0, xb, yb, rop );

    xp = ( x2 + xb ) / 2;
    yp = ( y2 + yb ) / 2;
    if ( abs( xc - xp ) + abs( yc - yp ) > SPLINE_THRESH )
	(void) raster_spline3( r, xb, yb, xc, yc, x2, y2, rop );
    else
	(void) raster_line( r, xb, yb, x2, y2, rop );

    return 0;
    }
