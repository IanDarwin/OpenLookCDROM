/* pixrect_misc.c - pixrect clone using libraster
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

struct pixrect*
raster_to_pixrect( r )
    struct raster* r;
    {
    struct pixrect* pr;

    pr = (struct pixrect*) malloc( sizeof(struct pixrect) );
    if ( pr == (struct pixrect*) 0 )
	return (struct pixrect*) 0;
    pr->pr_size.x = r->width;
    pr->pr_size.y = r->height;
    pr->pr_depth = r->depth;
    pr->pr_data = (caddr_t) r;
    return pr;
    }

struct pixrect*
pr_open( fbname )
    char* fbname;
    {
    struct raster* r;
    struct pixrect* pr;

    r = raster_open( fbname );
    if ( r == (struct raster*) 0 )
	return (struct pixrect*) 0;
    pr = raster_to_pixrect( r );
    if ( pr == (struct pixrect*) 0 )
	raster_free( r );
    return pr;
    }

struct pixrect*
pr_region( pr, x, y, w, h )
    struct pixrect* pr;
    int x, y, w, h;
    {
    struct raster* r = (struct raster*) pr->pr_data;
    struct raster* nr;
    struct pixrect* npr;

    nr = raster_subregion( r, x, y, w, h );
    if ( nr == (struct raster*) 0 )
	return (struct pixrect*) 0;
    npr = raster_to_pixrect( nr );
    if ( npr == (struct pixrect*) 0 )
	raster_free( nr );
    return npr;
    }

void
pr_destroy( pr )
    struct pixrect* pr;
    {
    struct raster* r = (struct raster*) pr->pr_data;

    raster_free( r );
    free( (char*) pr );
    }

struct pixrect*
mem_create( w, h, depth )
    int w, h, depth;
    {
    struct raster* r;
    struct pixrect* pr;

    r = raster_alloc( w, h, depth );
    if ( r == (struct raster*) 0 )
	return (struct pixrect*) 0;
    pr = raster_to_pixrect( r );
    if ( pr == (struct pixrect*) 0 )
	raster_free( r );
    return pr;
    }

struct pixrect*
mem_point( w, h, depth, data )
    int w, h, depth;
    short* data;
    {
    struct raster* r;
    struct pixrect* pr;

    r = raster_static( w, h, depth, (u_long*) data );
    if ( r == (struct raster*) 0 )
	return (struct pixrect*) 0;
    pr = raster_to_pixrect( r );
    if ( pr == (struct pixrect*) 0 )
	raster_free( r );
    return pr;
    }

int
mpr_prlinebytes( pr )
    struct pixrect* pr;
    {
    struct raster* r;

    r = (struct raster*) pr->pr_data;
    return r->linelongs * sizeof(u_long);
    }

short*
mpr_primage( pr )
    struct pixrect* pr;
    {
    struct raster* r;

    r = (struct raster*) pr->pr_data;
    return (short*) r->pixels;
    }

void
pr_flip( pr )
    struct pixrect* pr;
    {
    /* Not necessary with this implementation. */
    }
