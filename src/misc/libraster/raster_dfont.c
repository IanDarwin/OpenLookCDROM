/* raster_defaultfont.c - default font for raster library
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
#include "gallant19.h"


/* Returns the default font. */
struct raster_font*
raster_defaultfont( )
    {
    return &gallant19;
    }
