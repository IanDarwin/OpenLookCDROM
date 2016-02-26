/* pixrect_text.c
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
#include <pixrect/pixfont.h>
#include <pixrect_priv.h>

int
pr_text( pr, x, y, op, pf, text )
    struct pixrect* pr;
    int x, y, op;
    struct pixfont* pf;
    char* text;
    {
    struct raster* r;
    struct raster_font* rf;

    r = (struct raster*) pr->pr_data;
    rf = (struct raster_font*) pf->data;
    if ( raster_text( r, x, y, PROP_TO_RASOP(op), rf, text ) != 0 )
	return PIX_ERR;
    return 0;
    }

int
pr_ttext( pr, x, y, op, pf, text )
    struct pixrect* pr;
    int x, y, op;
    struct pixfont* pf;
    char* text;
    {
    struct raster* r;
    struct raster_font* rf;

    r = (struct raster*) pr->pr_data;
    rf = (struct raster_font*) pf->data;
    if ( raster_ttext( r, x, y, PROP_TO_RASOP(op), rf, text ) )
	return PIX_ERR;
    return 0;
    }

struct pr_size
pf_textwidth( len, pf, text )
    int len;
    struct pixfont* pf;
    char* text;
    {
    struct raster_font* rf;
    int x, y, w, h;
    struct pr_size s;

    rf = (struct raster_font*) pf->data;
    (void) raster_textnsize( rf, text, len, &x, &y, &w, &h );

    s.x = w;
    s.y = h;

    return s;
    }

int
pf_textbound( bound, len, pf, text )
    struct pr_subregion* bound;
    int len;
    struct pixfont* pf;
    char* text;
    {
    struct raster_font* rf;
    int x, y, w, h;

    rf = (struct raster_font*) pf->data;
    if ( raster_textnsize( rf, text, len, &x, &y, &w, &h ) != 0 )
	return PIX_ERR;

    bound->pos.x = x;
    bound->pos.y = y;
    bound->size.x = w;
    bound->size.y = h;

    return 0;
    }
