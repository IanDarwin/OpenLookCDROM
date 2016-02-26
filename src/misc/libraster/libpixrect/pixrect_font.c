/* pixrect_font.c
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
#include <pixrect/pixfont.h>
#include <pixrect_priv.h>

static struct pixfont*
rasterfont_to_pixfont( rf )
    struct raster_font* rf;
    {
    struct pixfont* pf;

    pf = (struct pixfont*) malloc( sizeof(struct pixfont) );
    if ( pf == (struct pixfont*) 0 )
	return (struct pixfont*) 0;
    pf->pf_defaultsize.x = rf->width;
    pf->pf_defaultsize.y = rf->height;
    pf->data = (caddr_t) rf;
    return pf;
    }

struct pixfont*
pf_open( name )
    char* name;
    {
    struct raster_font* rf;

    rf = raster_fontopen( name );
    if ( rf == (struct raster_font*) 0 )
	return (struct pixfont*) 0;
    return rasterfont_to_pixfont( rf );
    }

struct pixfont*
pf_open_private( name )
    char* name;
    {
    return pf_open( name );	/* no difference here */
    }

int
pf_close( pf )
    struct pixfont* pf;
    {
    struct raster_font* rf;

    rf = (struct raster_font*) pf->data;
    raster_fontclose( rf );
    free( pf );
    return 0;
    }

struct pixfont*
pf_default()
    {
    struct raster_font* rf;

    rf = raster_defaultfont();
    if ( rf == (struct raster_font*) 0 )
	return (struct pixfont*) 0;
    return rasterfont_to_pixfont( rf );
    }
