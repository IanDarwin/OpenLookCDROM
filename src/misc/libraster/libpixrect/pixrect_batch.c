/* pixrect_batch.c
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
pr_batchrop( dpr, dx, dy, op, sprs, n )
    struct pixrect* dpr;
    int dx, dy, op, n;
    struct pr_prpos* sprs;
    {
    struct raster* dst;
    struct raster** srcs;
    int* sxs;
    int* sys;
    int i, ret;

    srcs = (struct raster**) malloc( n * sizeof(struct raster*) );
    sxs = (int*) malloc( n * sizeof(int) );
    sys = (int*) malloc( n * sizeof(int) );
    if ( srcs == (struct raster**) 0 || sxs == (int*) 0 || sys == (int*) 0 )
	return PIX_ERR;

    dst = (struct raster*) dpr->pr_data;
    for ( i = 0; i < n; ++i )
	{
	if ( sprs[i].pr == (struct pixrect*) 0 )
	    srcs[i] = (struct raster*) 0;
	else
	    srcs[i] = (struct raster*) sprs[i].pr->pr_data;
	sxs[i] = sprs[i].pos.x;
	sys[i] = sprs[i].pos.y;
	}

    ret = raster_batchop( dst, dx, dy, PROP_TO_RASOP(op), srcs, sxs, sys, n );

    free( (char*) srcs );
    free( (char*) sxs );
    free( (char*) sys );

    if ( ret != 0 )
	return PIX_ERR;
    return 0;
    }
