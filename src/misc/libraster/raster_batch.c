/* raster_batch.c - batchop routine for raster library
**
** This routine should be optimized at some point so that it folds in
** code from raster_op() and so avoids all the extraneous edge masking.
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
raster_batchop( dst, dx, dy, rop, srcs, sxs, sys, count )
    struct raster* dst;
    int dx, dy, rop, count;
    struct raster* srcs[];
    int sxs[];
    int sys[];
    {
    int i;

    for ( i = 0; i < count; ++i )
	{
	dx += sxs[i];
	dy += sys[i];

	if ( srcs[i] != (struct raster*) 0 ) 
	    if ( raster_op( dst, dx, dy, srcs[i]->width, srcs[i]->height, rop,
		    srcs[i], 0, 0 ) != 0 )
		return -1;
	}

    return 0;
    }
