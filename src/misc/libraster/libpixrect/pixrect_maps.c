/* pixrect_maps.c
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
pr_getcolormap( pr, ind, cnt, red, grn, blu )
    struct pixrect* pr;
    int ind, cnt;
    short* red;
    short* grn;
    short* blu;
    {
    struct raster* r = (struct raster*) pr->pr_data;
    struct raster_fb* rfb;
    struct raster_colormap* cm;
    int i;

    if ( ! ( r->flags & RAS_FRAMEBUFFER ) )
	return PIX_ERR;
    rfb = (struct raster_fb*) r->data;
    if ( ind < 0 || ind + cnt > rfb->cmsize )
	return PIX_ERR;
    cm = raster_colormap_get( r );
    if ( cm == (struct raster_colormap*) 0 )
	return PIX_ERR;
    for ( i = 0; i < cnt; ++i )
	{
	red[i] = cm->red[ind + i];
	grn[i] = cm->grn[ind + i];
	blu[i] = cm->blu[ind + i];
	}
    raster_colormap_free( cm );
    return 0;
    }

int
pr_putcolormap( pr, ind, cnt, red, grn, blu )
    struct pixrect* pr;
    int ind, cnt;
    short* red;
    short* grn;
    short* blu;
    {
    struct raster* r = (struct raster*) pr->pr_data;
    struct raster_colormap* cm;
    int i, ret;

    cm = raster_colormap_alloc( cnt );
    cm->ind = ind;
    for ( i = 0; i < cnt; ++i )
	{
	cm->red[i] = red[i];
	cm->grn[i] = grn[i];
	cm->blu[i] = blu[i];
	}
    ret = raster_colormap_set( r, cm );
    raster_colormap_free( cm );
    if ( ret != 0 )
	return PIX_ERR;
    return 0;
    }
