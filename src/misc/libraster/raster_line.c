/* raster_line.c - line routine for raster library
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

/* Draws a line into a raster.  Returns 0 on success, -1 on failure. */
int
raster_line( r, x0, y0, x1, y1, rop )
    struct raster* r;
    int x0, y0, x1, y1;
    int rop;
    {
    register int cx0, cy0, cx1, cy1;

    /* Special case zero-length lines. */
    if ( x0 == x1 && y0 == y1 )
	{
	if ( x0 < 0 || y0 < 0 || x0 >= r->width || y0 >= r->height )
	    return -1;
	else
	    return raster_point_noclip( r, x0, y0, rop );
	}

    /* Clip. */
    cx0 = x0;
    cy0 = y0;
    cx1 = x1;
    cy1 = y1;
    if ( cx0 < 0 )
	{
	if ( cx1 < 0 ) return -1;
	cy0 = cy0 + ( cy1 - cy0 ) * ( -cx0 ) / ( cx1 - cx0 );
	cx0 = 0;
	}
    else if ( cx0 >= r->width )
	{
	if ( cx1 >= r->width ) return -1;
	cy0 = cy0 + ( cy1 - cy0 ) * ( r->width - 1 - cx0 ) / ( cx1 - cx0 );
	cx0 = r->width - 1;
	}
    if ( cy0 < 0 )
	{
	if ( cy1 < 0 ) return -1;
	cx0 = cx0 + ( cx1 - cx0 ) * ( -cy0 ) / ( cy1 - cy0 );
	cy0 = 0;
	}
    else if ( cy0 >= r->height )
	{
	if ( cy1 >= r->height ) return -1;
	cx0 = cx0 + ( cx1 - cx0 ) * ( r->height - 1 - cy0 ) / ( cy1 - cy0 );
	cy0 = r->height - 1;
	}
    if ( cx1 < 0 )
	{
	cy1 = cy1 + ( cy0 - cy1 ) * ( -cx1 ) / ( cx0 - cx1 );
	cx1 = 0;
	}
    else if ( cx1 >= r->width )
	{
	cy1 = cy1 + ( cy0 - cy1 ) * ( r->width - 1 - cx1 ) / ( cx0 - cx1 );
	cx1 = r->width - 1;
	}
    if ( cy1 < 0 )
	{
	cx1 = cx1 + ( cx0 - cx1 ) * ( -cy1 ) / ( cy0 - cy1 );
	cy1 = 0;
	}
    else if ( cy1 >= r->height )
	{
	cx1 = cx1 + ( cx0 - cx1 ) * ( r->height - 1 - cy1 ) / ( cy0 - cy1 );
	cy1 = r->height - 1;
	}

    /* Draw. */
    return raster_line_noclip( r, cx0, cy0, cx1, cy1, rop );
    }

#define DDA_SCALE 8192

/* Draws a line into a raster, without clipping.  Returns 0 on success,
** -1 on failure.
*/
int
raster_line_noclip( r, x0, y0, x1, y1, rop )
    struct raster* r;
    int x0, y0, x1, y1;
    int rop;
    {
    /* Check for zero-length lines. */
    if ( x0 == x1 && y0 == y1 )
	return raster_point_noclip( r, x0, y0, rop );

    /* Draw, using a simple DDA. */
    if ( abs( x1 - x0 ) > abs( y1 - y0 ) )
	{ /* Loop over X domain. */
	register long dy, srow;
	register int dx, col, row, prevrow;

	if ( x1 > x0 )
	    dx = 1;
	else
	    dx = -1;
	dy = ( y1 - y0 ) * DDA_SCALE / abs( x1 - x0 );
	prevrow = row = y0;
	srow = row * DDA_SCALE + DDA_SCALE / 2;
	col = x0;
	for (;;)
	    {
	    (void) raster_point_noclip( r, col, row, rop );
	    if ( col == x1 )
		break;
	    srow += dy;
	    row = srow / DDA_SCALE;
	    col += dx;
	    }
	}
    else
	{ /* Loop over Y domain. */
	register long dx, scol;
	register int dy, col, row, prevcol;

	if ( y1 > y0 )
	    dy = 1;
	else
	    dy = -1;
	dx = ( x1 - x0 ) * DDA_SCALE / abs( y1 - y0 );
	row = y0;
	prevcol = col = x0;
	scol = col * DDA_SCALE + DDA_SCALE / 2;
	for (;;)
	    {
	    (void) raster_point_noclip( r, col, row, rop );
	    if ( row == y1 )
		break;
	    row += dy;
	    scol += dx;
	    col = scol / DDA_SCALE;
	    }
	}

    return 0;
    }
