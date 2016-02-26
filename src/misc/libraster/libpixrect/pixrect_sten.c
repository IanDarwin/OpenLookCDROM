/* pixrect_sten.c
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
pr_stencil( dpr, dx, dy, w, h, op, stpr, stx, sty, spr, sx, sy )
    struct pixrect* dpr;
    int dx, dy, w, h, op;
    struct pixrect* stpr;
    int stx, sty;
    struct pixrect* spr;
    int sx, sy;
    {
    struct raster* dst;
    struct raster* sten;
    struct raster* src;

    dst = (struct raster*) dpr->pr_data;
    sten = (struct raster*) stpr->pr_data;
    if ( spr == (struct pixrect*) 0 )
	src = (struct raster*) 0;
    else
	src = (struct raster*) spr->pr_data;

    if ( raster_stencil(
	     dst, dx, dy, w, h, PROP_TO_RASOP(op), sten, stx, sty, src,
	     sx, sy ) != 0 )
	return PIX_ERR;
    return 0;
    }
