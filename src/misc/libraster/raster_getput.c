/* raster_getput.c - get/put pixel routines for raster library
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


/* Externals. */

extern u_long raster_bitmask[];


/* Routines. */

/* Gets a single pixel from a raster.  Returns value on success, -1 on
** failure.
*/
int
raster_get( r, x, y )
    register struct raster* r;
    register int x, y;
    {
    register u_long* p;
    register int rightshift;

    if ( x < 0 || y < 0 || x >= r->width || y >= r->height )
	return -1;

    p = RAS_ADDR( r, x, y );
    switch ( r->depth )
	{
	case 1:
	if ( *p & raster_bitmask[x & 31] )
	    return 1;
	else
	    return 0;

	case 8:
#ifdef MSBYTE_FIRST
	rightshift = ( 3 - ( x & 3 ) ) * 8;
#else /*MSBYTE_FIRST*/
	rightshift = ( x & 3 ) * 8;
#endif /*MSBYTE_FIRST*/
	return ( *p >> rightshift ) & 0xff;

	default:
	return 0;	/* not used - just to make the compilers happy */
	}
    }

/* Puts a single pixel into a raster.  Compare with raster_point().
** Returns 0 on success, -1 on failure.
*/
int
raster_put( r, x, y, v )
    register struct raster* r;
    register int x, y, v;
    {
    register u_long* p;
    register int leftshift;

    if ( x < 0 || y < 0 || x >= r->width || y >= r->height )
	return -1;

    p = RAS_ADDR( r, x, y );
    switch ( r->depth )
	{
	case 1:
	if ( v )
	    *p |= raster_bitmask[x & 31];
	else
	    *p &= ~raster_bitmask[x & 31];
	break;

	case 8:
#ifdef MSBYTE_FIRST
	leftshift = ( 3 - ( x & 3 ) ) * 8;
#else /*MSBYTE_FIRST*/
	leftshift = ( x & 3 ) * 8;
#endif /*MSBYTE_FIRST*/
	*p &= 0xff << leftshift;
	*p |= ( v & 0xff ) << leftshift;
	break;
	}

    return 0;
    }
