/* raster_file.c - file I/O for raster library
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

#include <stdio.h>
#include "raster.h"
#include <malloc.h>


/* Definitions. */

/* Sun rasterfile header. */
struct rasterfile {
    long ras_magic;
#define RAS_MAGIC       0x59a66a95
    long ras_width;
    long ras_height;
    long ras_depth;
    long ras_length;
    long ras_type;
    long ras_maptype;
#define RMT_NONE        0
#define RMT_EQUAL_RGB   1
#define RMT_RAW         2
    long ras_maplength;
    };


/* Variables. */

#ifndef MSBIT_FIRST
static u_char bit_reverse[256] = {
    0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0, 0x10, 0x90, 0x50, 0xd0,
    0x30, 0xb0, 0x70, 0xf0, 0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
    0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8, 0x04, 0x84, 0x44, 0xc4,
    0x24, 0xa4, 0x64, 0xe4, 0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
    0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec, 0x1c, 0x9c, 0x5c, 0xdc,
    0x3c, 0xbc, 0x7c, 0xfc, 0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
    0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2, 0x0a, 0x8a, 0x4a, 0xca,
    0x2a, 0xaa, 0x6a, 0xea, 0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
    0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6, 0x16, 0x96, 0x56, 0xd6,
    0x36, 0xb6, 0x76, 0xf6, 0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
    0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe, 0x01, 0x81, 0x41, 0xc1,
    0x21, 0xa1, 0x61, 0xe1, 0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
    0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9, 0x19, 0x99, 0x59, 0xd9,
    0x39, 0xb9, 0x79, 0xf9, 0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
    0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5, 0x0d, 0x8d, 0x4d, 0xcd,
    0x2d, 0xad, 0x6d, 0xed, 0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
    0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3, 0x13, 0x93, 0x53, 0xd3,
    0x33, 0xb3, 0x73, 0xf3, 0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
    0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb, 0x07, 0x87, 0x47, 0xc7,
    0x27, 0xa7, 0x67, 0xe7, 0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
    0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef, 0x1f, 0x9f, 0x5f, 0xdf,
    0x3f, 0xbf, 0x7f, 0xff};
#endif /*not MSBIT_FIRST*/


/* Raster file read/write routines. */

/* Reads a big-endian longword.  Returns 0 on success, -1 on failure. */
static int
raster_readbiglong( f, lP )
    FILE* f;
    long* lP;
    {
    int c;

    if ( (c = getc( f )) == EOF )
        return -1;
    *lP = ( c & 0xff ) << 24;
    if ( (c = getc( f )) == EOF )
        return -1;
    *lP |= ( c & 0xff ) << 16;
    if ( (c = getc( f )) == EOF )
        return -1;
    *lP |= ( c & 0xff ) << 8;
    if ( (c = getc( f )) == EOF )
        return -1;
    *lP |= c & 0xff;
    return 0;
    }

static int
raster_writebiglong( f, l )
    FILE* f;
    long l;
    {
    if ( putc( (char) ( ( l >> 24 ) & 0xff ), f ) == EOF )
	return -1;
    if ( putc( (char) ( ( l >> 16 ) & 0xff ), f ) == EOF )
	return -1;
    if ( putc( (char) ( ( l >> 8 ) & 0xff ), f ) == EOF )
	return -1;
    if ( putc( (char) ( l & 0xff ), f ) == EOF )
	return -1;
    return 0;
    }

/* Allocates a raster and reads the contents from a file.  Returns
** (raster*) 0 on failure.  If there's a colormap in the file, it gets
** allocated and returned in cmP; otherwise, cmP is 0.
*/
struct raster*
raster_read( f, cmP )
    FILE* f;
    struct raster_colormap** cmP;
    {
    struct rasterfile h;
    register struct raster* r;
    int i, j, ln, wlinebytes, lbdiff, lb, count;
    u_long* lp;
    u_char* beimage;
    u_char* bep;
    u_char c;
    u_char* bp;

    /* Read the header. */
    if ( raster_readbiglong( f, &h.ras_magic ) == -1 )
        return (struct raster*) 0;
    if ( h.ras_magic != RAS_MAGIC )
        return (struct raster*) 0;
    if ( raster_readbiglong( f, &h.ras_width ) == -1 )
        return (struct raster*) 0;
    if ( raster_readbiglong( f, &h.ras_height ) == -1 )
        return (struct raster*) 0;
    if ( raster_readbiglong( f, &h.ras_depth ) == -1 )
        return (struct raster*) 0;
    if ( raster_readbiglong( f, &h.ras_length ) == -1 )
        return (struct raster*) 0;
    if ( raster_readbiglong( f, &h.ras_type ) == -1 )
        return (struct raster*) 0;
    if ( raster_readbiglong( f, &h.ras_maptype ) == -1 )
        return (struct raster*) 0;
    if ( raster_readbiglong( f, &h.ras_maplength ) == -1 )
        return (struct raster*) 0;

    /* Read the colormap. */
    if ( h.ras_maptype == RMT_NONE )
        {
        for ( i = 0; i < h.ras_maplength; ++i )
            if ( getc( f ) == EOF )
                return (struct raster*) 0;
	*cmP = (struct raster_colormap*) 0;
        }
    else
        {
        switch ( h.ras_maptype )
            {
            case RMT_EQUAL_RGB:
	    *cmP = raster_colormap_alloc( h.ras_maplength / 3 );
	    if ( *cmP == (struct raster_colormap*) 0 )
		return (struct raster*) 0;
            if ( fread( (char*) (*cmP)->red, 1, (*cmP)->len, f ) !=
		   (*cmP)->len ||
                 fread( (char*) (*cmP)->grn, 1, (*cmP)->len, f ) !=
		   (*cmP)->len ||
                 fread( (char*) (*cmP)->blu, 1, (*cmP)->len, f ) !=
		   (*cmP)->len )
                {
                raster_colormap_free( *cmP );
                return (struct raster*) 0;
                }
            break;

            default:
            return (struct raster*) 0;		/* unknown colormap type */
            }
        }


    /* Read the image data. */
    r = raster_alloc( h.ras_width, h.ras_height, h.ras_depth );
    if ( r == (struct raster*) 0 )
	{
	if ( *cmP != (struct raster_colormap*) 0 )
	    raster_colormap_free( *cmP );
	return (struct raster*) 0;
	}
    /* Raster files pad each line of pixels to a word, not a longword. */
    wlinebytes = ( ( r->width * r->depth + 15 ) / 16 ) * 2;
    switch ( h.ras_type )
	{
        case RT_STANDARD:
        /* Ignore h.ras_length. */
	for ( ln = 0, lp = r->pixels; ln < r->height; ++ln, lp += r->linelongs )
	    {
	    if ( fread( (char*) lp, 1, wlinebytes, f ) != wlinebytes )
		{
		raster_free( r );
		if ( *cmP != (struct raster_colormap*) 0 )
		    raster_colormap_free( *cmP );
		return (struct raster*) 0;
		}
	    }
        break;

        case RT_BYTE_ENCODED:
	lbdiff = r->linelongs * 4 - wlinebytes;
        beimage = (u_char*) malloc( (unsigned) h.ras_length );
        if ( beimage == (u_char*) 0 )
            {
	    raster_free( r );
	    if ( *cmP != (struct raster_colormap*) 0 )
		raster_colormap_free( *cmP );
	    return (struct raster*) 0;
            }
        if ( fread( (char*) beimage, 1, (int) h.ras_length, f ) !=
	     h.ras_length )
            {
            free( (char*) beimage );
	    raster_free( r );
	    if ( *cmP != (struct raster_colormap*) 0 )
		raster_colormap_free( *cmP );
	    return (struct raster*) 0;
            }
        for ( bep = beimage, bp = (u_char*) r->pixels, lb = 0, i = h.ras_length;
	      i > 0; )
            {
            c = *bep++;
            if ( c == 128 )
                {
                count = ( *bep++ ) + 1;
                if ( count == 1 )
                    {
                    *bp++ = 128;
		    if ( ++lb >= wlinebytes )
			{
			bp += lbdiff;
			lb = 0;
			}
                    i -= 2;
                    }
                else
                    {
                    c = *bep++;
                    for ( j = 0; j < count; ++j )
			{
                        *bp++ = c;
			if ( ++lb >= wlinebytes )
			    {
			    bp += lbdiff;
			    lb = 0;
			    }
			}
                    i -= 3;
                    }
                }
            else
                {
                *bp++ = c;
		if ( ++lb >= wlinebytes )
		    {
		    bp += lbdiff;
		    lb = 0;
		    }
                --i;
                }
            }
        free( (char*) beimage );
        break;

        default:
	raster_free( r );
	if ( *cmP != (struct raster_colormap*) 0 )
	    raster_colormap_free( *cmP );
        return (struct raster*) 0;	/* unknown rasterfile type */
	}

#ifndef MSBIT_FIRST
    /* Raster files are by definition stored MSBIT_FIRST.  If we want
    ** the other order, we have to flip bits.
    */
    for ( i = 0, bp = (u_char*) r->pixels;
	  i < r->height;
	  bp += r->linelongs * 4 )
	{
	u_char* bp2;

	for ( j = 0, bp2 = bp; j < r->width; ++j, ++bp2 )
	    *bp2 = bit_reverse[*bp2];
	}
#endif /*not MSBIT_FIRST*/

    return r;
    }

/* Writes a raster to a file.  The colormap is optional.  Returns 0 on
** success, -1 on failure.
*/
int
raster_write( f, r, rt, cm )
    FILE* f;
    struct raster* r;
    int rt;
    struct raster_colormap* cm;
    {
    int wlinebytes, freecm;
    struct rasterfile h;
    int size, lbdiff, besize, count, i, j;
    u_char* beimage;
    u_char* bp;
    u_char c, pc;
    u_long* lp;
    int ln;

    wlinebytes = ( ( r->width * r->depth + 15 ) / 16 ) * 2;

    h.ras_magic = RAS_MAGIC;
    h.ras_width = r->width;
    h.ras_height = r->height;
    h.ras_depth = r->depth;
    h.ras_type = rt;
    freecm = 0;
    if ( cm == (struct raster_colormap*) 0 &&
	 r->depth == 8 && ( r->flags & RAS_FRAMEBUFFER ) != 0 )
	{
	cm = raster_colormap_get( r );
	freecm = 1;
	}
    if ( cm == (struct raster_colormap*) 0 )
	{
	h.ras_maptype = RMT_NONE;
	h.ras_maplength = 0;
	}
    else
	{
	h.ras_maptype = RMT_EQUAL_RGB;
	h.ras_maplength = cm->len * 3;
	}

    switch ( rt )
	{
	case RT_STANDARD:
	h.ras_length = wlinebytes * r->height;
	break;

	case RT_BYTE_ENCODED:
	size = wlinebytes * r->height;
	lbdiff = r->linelongs * 4 - wlinebytes;
	bp = (u_char*) r->pixels;
	beimage = (u_char*) malloc( (unsigned) ( size*3/2 ) );	/* worst case */
	if ( beimage == (u_char*) 0 )
	    return -1;
	besize = 0;
	count = 0;
	i = 0;
	while ( i < size )
	    {
	    c = *bp;
	    if ( count > 0 )
		{
		if ( pc != c )
		    {
		    if ( count == 1 && pc == 128 )
			{
			beimage[besize++] = 128;
			beimage[besize++] = 0;
			count = 0;
			}
		    else if ( count > 2 || pc == 128 )
			{
			beimage[besize++] = 128;
			beimage[besize++] = count - 1;
			beimage[besize++] = pc;
			count = 0;
			}
		    else
			{
			for ( j = 0; j < count; ++j )
			    beimage[besize++] = pc;
			count = 0;
			}
		    }
		}
	    pc = c;
	    ++count;
	    if ( count == 256 )
		{
		beimage[besize++] = 128;
		beimage[besize++] = count - 1;
		beimage[besize++] = c;
		count = 0;
		}
	    ++bp;
	    ++i;
	    if ( i % wlinebytes == 0 )
		bp += lbdiff;
	    }
	if ( count > 0 )
	    {
	    if ( count == 1 && c == 128 )
		{
		beimage[besize++] = 128;
		beimage[besize++] = 0;
		}
	    if ( count > 2 || c == 128 )
		{
		beimage[besize++] = 128;
		beimage[besize++] = count - 1;
		beimage[besize++] = c;
		}
	    else
		{
		for ( j = 0; j < count; ++j )
		    beimage[besize++] = c;
		}
	    }
	h.ras_length = besize;
	break;

	default:
	return -1;		/* other types not implemented */
	}

    /* Write the header. */
    if ( raster_writebiglong( f, h.ras_magic ) == -1 )
        return -1;
    if ( raster_writebiglong( f, h.ras_width ) == -1 )
        return -1;
    if ( raster_writebiglong( f, h.ras_height ) == -1 )
        return -1;
    if ( raster_writebiglong( f, h.ras_depth ) == -1 )
        return -1;
    if ( raster_writebiglong( f, h.ras_length ) == -1 )
        return -1;
    if ( raster_writebiglong( f, h.ras_type ) == -1 )
        return -1;
    if ( raster_writebiglong( f, h.ras_maptype ) == -1 )
        return -1;
    if ( raster_writebiglong( f, h.ras_maplength ) == -1 )
        return -1;

    if ( cm != (struct raster_colormap*) 0 )
	{
	/* Write the colormap. */
	if ( fwrite( (char*) cm->red, 1, cm->len, f ) != cm->len )
	    return -1;
	if ( fwrite( (char*) cm->grn, 1, cm->len, f ) != cm->len )
	    return -1;
	if ( fwrite( (char*) cm->blu, 1, cm->len, f ) != cm->len )
	    return -1;
	}

    /* Write the image. */
    switch ( rt )
	{
	case RT_STANDARD:
	for ( lp = r->pixels, ln = 0; ln < r->height; lp += r->linelongs, ++ln )
            {
#ifdef MSBIT_FIRST
	    if ( fwrite( (char*) lp, 1, wlinebytes, f ) != wlinebytes )
		return -1;
#else /*MSBIT_FIRST*/
	    /* Flip bits. */
	    int i;
	    u_char* bp;

	    for ( i = wlinebytes, bp = (u_char*) lp; i > 0; --i, ++bp )
		if ( putc( bit_reverse[*bp], f ) == EOF )
		    return -1;
#endif /*MSBIT_FIRST*/
	    }
	break;

	case RT_BYTE_ENCODED:
	if ( fwrite( (char*) beimage, 1, besize, f ) != besize )
	    {
	    free( (char*) beimage );
	    return -1;
	    }
	free( (char*) beimage );
	break;

	default:
	return -1;		/* other types not implemented */
	}

    if ( freecm )
	raster_colormap_free( cm );
    return 0;
    }
