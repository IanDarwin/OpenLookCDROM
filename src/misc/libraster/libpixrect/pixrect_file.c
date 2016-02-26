/* pixrect_file.c
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
#include <pixrect/pr_io.h>
#include <pixrect_priv.h>

struct pixrect*
pr_load( f, cm )
    FILE* f;
    colormap_t* cm;
    {
    struct raster* r;
    struct raster_colormap* rcm;
    struct pixrect* pr;
    int i;

    r = raster_read( f, &rcm );
    if ( r == (struct raster*) 0 )
	return (struct pixrect*) 0;
    pr = raster_to_pixrect( r );
    if ( pr == (struct pixrect*) 0 )
	{
	raster_free( r );
	raster_colormap_free( rcm );
	}
    if ( rcm == (struct raster_colormap*) 0 )
	cm->type = RMT_NONE;
    else
	{
	cm->type = RMT_EQUAL_RGB;
	cm->length = rcm->len;
	for ( i = 0; i < rcm->len; ++i )
	    {
	    cm->map[0][i] = rcm->red[i + rcm->ind];
	    cm->map[1][i] = rcm->grn[i + rcm->ind];
	    cm->map[2][i] = rcm->blu[i + rcm->ind];
	    }
	}
    return pr;
    }

int
pr_dump( pr, f, cm, type, copy )
    struct pixrect* pr;
    FILE* f;
    colormap_t* cm;
    int type, copy;
    {
    struct raster* r;
    struct raster_colormap* rcm;
    int i, rret;

    r = (struct raster*) pr->pr_data;
    if ( cm == (colormap_t*) 0 )
	rcm = (struct raster_colormap*) 0;
    else
	{
	rcm = raster_colormap_alloc( cm->length );
	for ( i = 0; i < cm->length; ++i )
	    {
	    rcm->red[i + rcm->ind] = cm->map[0][i];
	    rcm->grn[i + rcm->ind] = cm->map[1][i];
	    rcm->blu[i + rcm->ind] = cm->map[2][i];
	    }
	}
    rret = raster_write( f, r, type, rcm );
    if ( rcm != (struct raster_colormap*) 0 )
	raster_colormap_free( rcm );
    if ( rret != 0 )
	return PIX_ERR;
    return 0;
    }
