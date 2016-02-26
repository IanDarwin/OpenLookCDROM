/* pixrect_rop.c
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
pr_rop( dpr, dx, dy, w, h, op, spr, sx, sy )
    struct pixrect* dpr;
    int dx, dy, w, h, op;
    struct pixrect* spr;
    int sx, sy;
    {
    struct raster* dst;
    struct raster* src;
    int ret;

    dst = (struct raster*) dpr->pr_data;
    if ( spr == (struct pixrect*) 0 )
	src = (struct raster*) 0;
    else
	src = (struct raster*) spr->pr_data;

    if ( op & PIX_DONTCLIP )
	ret = raster_op_noclip(
	    dst, dx, dy, w, h, PROP_TO_RASOP(op), src, sx, sy );
    else
	ret = raster_op( dst, dx, dy, w, h, PROP_TO_RASOP(op), src, sx, sy );
    if ( ret != 0 )
	return PIX_ERR;
    return 0;
    }
