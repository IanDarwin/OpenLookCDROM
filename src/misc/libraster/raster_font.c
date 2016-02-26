/* raster_font.c - font routines for raster library
**
** Copyright (C) 1991, 1993 by Jef Poskanzer <jef@netcom.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#include "raster.h"
#include <stdio.h>
#include <malloc.h>


/* Definitions. */

/* Vfont header. */

struct dispatch {
    u_short addr;		/* &(glyph) - &(start of bitmaps) */
    short nbytes;		/* # bytes of glyphs (0 if no glyph) */
    char up, down, left, right;	/* widths from baseline point */
    short width;		/* logical width, used by troff */
    };

struct vfont {
    short magic;
#define VFONT_MAGIC 0436
    u_short size;		/* total # bytes of bitmaps */
    short maxx;			/* maximum horizontal glyph size */
    short maxy;			/* maximum vertical glyph size */
    short xtend;		/* unused */
    struct dispatch chars[256];
    };


/* Raster text routines */

/* Reads a big-endian shortword.  Returns 0 on success, -1 on failure. */
static int
raster_readbigshort( f, sP )
    FILE* f;
    short* sP;
    {
    int c;

    if ( (c = getc( f )) == EOF )
        return -1;
    *sP = ( c & 0xff ) << 8;
    if ( (c = getc( f )) == EOF )
        return -1;
    *sP |= c & 0xff;
    return 0;
    }

/* Opens a font. Returns (struct raster_font*) 0 on failure. */
struct raster_font*
raster_fontopen( fontname )
    char* fontname;
    {
    struct raster_font* rf;
    struct raster_fontetc* rfe;
    int c, l, y, anextx;
    int maxup, maxdown, maxleft, maxright;
    FILE* f;
    struct vfont vf;
    u_char* bp;
    u_char* bp2;
    u_long* rp;

    f = fopen( fontname, "r" );
    if ( f == (FILE*) 0 )
	return (struct raster_font*) 0;

    /* Read the header. */
    if ( raster_readbigshort( f, &vf.magic ) == -1 )
	return (struct raster_font*) 0;
    if ( vf.magic != VFONT_MAGIC )
	return (struct raster_font*) 0;
    if ( raster_readbigshort( f, (short*) &vf.size ) == -1 )
	return (struct raster_font*) 0;
    if ( raster_readbigshort( f, &vf.maxx ) == -1 )
	return (struct raster_font*) 0;
    if ( raster_readbigshort( f, &vf.maxy ) == -1 )
	return (struct raster_font*) 0;
    if ( raster_readbigshort( f, &vf.xtend ) == -1 )
	return (struct raster_font*) 0;
    
    /* Read the character table. */
    maxup = maxdown = maxleft = maxright = 0;
    for ( c = 0; c < 256; ++c )
	{
	if ( raster_readbigshort( f, (short*) &vf.chars[c].addr ) == -1 )
	    return (struct raster_font*) 0;
	if ( raster_readbigshort( f, &vf.chars[c].nbytes ) == -1 )
	    return (struct raster_font*) 0;
	vf.chars[c].up = getc( f );
	vf.chars[c].down = getc( f );
	vf.chars[c].left = getc( f );
	vf.chars[c].right = getc( f );
	if ( raster_readbigshort( f, &vf.chars[c].width ) == -1 )
	    return (struct raster_font*) 0;
	if ( vf.chars[c].up > maxup )
	    maxup = vf.chars[c].up;
	if ( vf.chars[c].down > maxdown )
	    maxdown = vf.chars[c].down;
	if ( vf.chars[c].left > maxleft )
	    maxleft = vf.chars[c].left;
	if ( vf.chars[c].right > maxright )
	    maxright = vf.chars[c].right;
	}
    
    /* Allocate and read the character bitmaps. */
    bp = (u_char*) malloc( (unsigned) vf.size );
    if ( bp == (u_char*) 0 )
	return (struct raster_font*) 0;
    if ( fread( (char*) bp, 1, (int) vf.size, f ) != vf.size )
	{
	free( (char*) bp );
	return (struct raster_font*) 0;
	}

    (void) fclose( f );

    /* Initialize the struct raster_font. */
    rf = (struct raster_font*) malloc( sizeof(struct raster_font) );
    if ( rf == (struct raster_font*) 0 )
	{
	free( (char*) bp );
	return (struct raster_font*) 0;
	}
    /* Ignore maxx, maxy, and xtend - use the computed max sizes instead. */
    rf->width = maxleft + maxright;
    rf->height = maxup + maxdown + 2;
    rf->flags = RASFONT_FIXEDWIDTH | RASFONT_NOVERTICALMOVEMENT;
    for ( c = 0; c < 256; ++c )
	rf->chars[c].r = (struct raster*) 0;
    rf->data = (caddr_t) 0;

    /* Now turn the vfont bitmaps into rasters. */
    anextx = 0;
    for ( c = 0; c < 256; ++c )
	{
	rf->chars[c].homex = vf.chars[c].left;
	rf->chars[c].homey = -vf.chars[c].up;
	rf->chars[c].nextx = vf.chars[c].width;
	if ( rf->chars[c].nextx != 0 )
	    {
	    if ( anextx == 0 )
		anextx = rf->chars[c].nextx;
	    else if ( rf->chars[c].nextx != anextx )
		rf->flags &= ~RASFONT_FIXEDWIDTH;
	    }
	rf->chars[c].nexty = 0;
	if ( vf.chars[c].nbytes != 0 )
	    {
	    rf->chars[c].r = raster_alloc(
		vf.chars[c].left + vf.chars[c].right,
		vf.chars[c].up + vf.chars[c].down, 1 );
	    if ( rf->chars[c].r == (struct raster*) 0 )
		{
		free( (char*) bp );
		raster_fontclose( rf );
		return (struct raster_font*) 0;
		}
	    l = ( rf->chars[c].r->width + 7 ) / 8;
	    for ( y = 0, bp2 = bp+vf.chars[c].addr, rp = rf->chars[c].r->pixels;
		  y < rf->chars[c].r->height;
		  ++y, bp2 += l, rp += rf->chars[c].r->linelongs )
		memcpy( (char*) rp, (char*) bp2, l );
		/* (Probably ought to do something about bit order here.) */
	    }
	}

    free( (char*) bp );
    return rf;
    }

/* Closes a font. */
void
raster_fontclose( rf )
    struct raster_font* rf;
    {
    struct raster_fontetc* rfe;
    register int c;

    rfe = (struct raster_fontetc*) rf->data;
    if ( ! ( rf->flags & RASFONT_STATIC ) )
	for ( c = 0; c < 256; ++c )
	    if ( rf->chars[c].r != (struct raster*) 0 )
		raster_free( rf->chars[c].r );
#ifdef COLORFONT_CACHE
    if ( rfe != (struct raster_fontetc*) 0 &&
	 rfe->cache != (struct raster_fontcache*) 0 &&
         rfe->cache != (struct raster_fontcache*) -1 )
	{
	for ( c = 0; c < 256; ++c )
	    if ( rfe->cache->cr[c] != (struct raster*) 0 )
		raster_free( rfe->cache->cr[c] );
        free( (char*) rfe->cache );
	}
#endif /*COLORFONT_CACHE*/
    if ( rfe != (struct raster_fontetc*) 0 )
	free( (char*) rfe );
    if ( ! ( rf->flags & RASFONT_STATIC ) )
	free( (char*) rf );
    }
