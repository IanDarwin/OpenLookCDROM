/* pixrect_repl.c
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

#include <stdio.h>
#include <raster.h>
#include <pixrect/pixrect.h>
#include <pixrect_priv.h>

int
pr_replrop( dpr, dx, dy, dw, dh, op, spr, sx, sy )
    struct pixrect* dpr;
    int dx, dy, dw, dh, op;
    struct pixrect* spr;
    int sx, sy;
    {
    struct raster* dst;
    struct raster* src;

    if ( ( op & 0x1E ) != PIX_SRC )
	{
	(void) fprintf( stderr, "pr_replrop: only PIX_SRC implemented\n" );
	return PIX_ERR;
	}

    dst = (struct raster*) dpr->pr_data;
    if ( spr == (struct pixrect*) 0 )
	src = (struct raster*) 0;
    else
	src = (struct raster*) spr->pr_data;

    if ( raster_replsrc(
	    dst, dx, dy, dw, dh, PROP_TO_RASOP(op), src, sx, sy ) != 0 )
	return PIX_ERR;
    return 0;
    }
