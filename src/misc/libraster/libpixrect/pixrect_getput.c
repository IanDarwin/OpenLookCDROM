/* pixrect_getput.c
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

#include <raster.h>
#include <pixrect/pixrect.h>
#include <pixrect_priv.h>

int
pr_get( pr, x, y )
    struct pixrect* pr;
    int x, y;
    {
    struct raster* r = (struct raster*) pr->pr_data;

    if ( raster_get( r, x, y ) != 0 )
	return PIX_ERR;
    return 0;
    }

int
pr_put( pr, x, y, value )
    struct pixrect* pr;
    int x, y, value;
    {
    struct raster* r = (struct raster*) pr->pr_data;

    if ( raster_put( r, x, y, value ) != 0 )
	return PIX_ERR;
    return 0;
    }

int
pr_polypoint( dpr, dx, dy, npts, ptlist, op )
    struct pixrect* dpr;
    int dx, dy, npts;
    struct pr_pos* ptlist;
    int op;
    {
    struct raster* dst = (struct raster*) dpr->pr_data;
    int rop, i, ret;

    rop = PROP_TO_RASOP(op);
    ret = 0;
    for ( i = 0; i < npts; ++i )
	ret |= raster_point( dst, dx + ptlist[i].x, dy + ptlist[i].y, rop );

    if ( ret != 0 )
	return PIX_ERR;
    return 0;
    }
