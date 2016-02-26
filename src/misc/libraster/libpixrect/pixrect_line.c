/* pixrect_line.c
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
pr_vector( pr, x0, y0, x1, y1, op, value )
    struct pixrect* pr;
    int x0, y0, x1, y1, op, value;
    {
    struct raster* r = (struct raster*) pr->pr_data;
    int ret;

    /* Fixup op with value. */
    if ( PIX_OPCOLOR(op) != 0 )
	op |= PIX_COLOR(value);

    if ( op & PIX_DONTCLIP )
	ret = raster_line_noclip( r, x0, y0, x1, y1, PROP_TO_RASOP(op) );
    else
	ret = raster_line( r, x0, y0, x1, y1, PROP_TO_RASOP(op) );

    if ( ret != 0 )
	return PIX_ERR;
    return 0;
    }
