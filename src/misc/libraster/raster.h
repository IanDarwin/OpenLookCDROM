#ifndef _RASTER_H_
#define _RASTER_H_

/* raster.h - simple raster and frame buffer routines
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

#include <sys/types.h>
#include <stdio.h>

/* Definitions. */

/* ANSI prototype conditionalizer. */
#ifndef ARGS
#if __STDC__
#define ARGS(alist) alist
#else /*__STDC__*/
#define ARGS(alist) ()
#endif /*__STDC__*/
#endif /*ARGS*/

/* Raster struct. */
struct raster {
    int width, height;	/* size in pixels */
    int depth;		/* bits per pixel - 1 or 8 */
    int linelongs;	/* longs from one line to the next - for padding */
    u_long* pixels;	/* pointer to the actual bits */
    int flags;
#define RAS_STATIC			0x1
#define RAS_SUBREGION			0x2
#define RAS_FRAMEBUFFER			0x4
    caddr_t data;	/* special value for frame buffers */
    };

/* Frame buffer auxillary struct. */
struct raster_fb {
    int cmsize;		/* colormap size */
    int fd;		/* file descriptor */
    };

/* Colormap struct. */
struct raster_colormap {
    int ind, len;
    u_char* red;
    u_char* grn;
    u_char* blu;
    };

/* Font character struct. */
struct raster_char {
    struct raster* r;
    int homex, homey;
    int nextx, nexty;
    };

/* Font struct. */
struct raster_font {
    int width, height;	/* nominal character size */
    int flags;
#define RASFONT_STATIC			0x1
#define RASFONT_FIXEDWIDTH		0x2
#define RASFONT_NOVERTICALMOVEMENT	0x4
    caddr_t data;	/* special value for in-core stuff */
    struct raster_char chars[256];
    };

/* Font auxillary struct. */
struct raster_fontetc {
#ifdef COLORFONT_CACHE
    struct raster_fontcache* cache;
#else /*COLORFONT_CACHE*/
    int notused;	/* make compiler happy */
#endif /*COLORFONT_CACHE*/
    };

#ifdef COLORFONT_CACHE
struct raster_fontcache {
    struct raster* cr[256];
    u_char color[256];
    };
#endif /*COLORFONT_CACHE*/

/* Defines for the rop parameter - the bitblit operation.  A rop can be
** some Boolean combination of RAS_SRC and RAS_DST.  For instance, just
** RAS_SRC means copy the source to the destination without modification.
** RAS_SRC|RAS_DST means "or" the source and destination together, while
** "xor" would be RAS_SRC^RAS_DST.  The RAS_NOT macro should be used to
** express negation - RAS_NOT(RAS_SRC)&RAS_DST would "and" the complement
** of the source with the destination.
**
** Or, you can just use one of the pre-defined ops besides the basic
** RAS_SRC and RAS_DST.  There are only 16 possible combinations, so
** all 16 are defined here.
**
** For color rasters, you specify the color of the operation by simply
** ORing RAS_COLOR(color) into the rop.
*/

#define RAS_NOT(op) ( 0xf & ( ~ (op) ) )

#define RAS_CLEAR		0x0	/* 0 */
#define RAS_NOTOR		0x1	/* !( src | dst ) */
#define RAS_NOTSRC_AND_DST	0x2	/* !src & dst */
#define RAS_INVERTSRC		0x3	/* !src */
#define RAS_SRC_AND_NOTDST	0x4	/* src & !dst */
#define RAS_INVERT		0x5	/* !dst */
#define RAS_XOR			0x6	/* src ^ dst */
#define RAS_NOTAND		0x7	/* !( src & dst ) */
#define RAS_AND			0x8	/* src & dst */
#define RAS_NOTXOR		0x9	/* !( src ^ dst ) */
#define RAS_DST			0xa	/* dst */
#define RAS_NOTSRC_OR_DST	0xb	/* !src | dst */
#define RAS_SRC			0xc	/* src */
#define RAS_SRC_OR_NOTDST	0xd	/* src | !dst */
#define RAS_OR			0xe	/* src | dst */
#define RAS_SET			0xf	/* 1 */

#define RAS_COLOR(color) ( ( (color) & 0xff ) << 4 )

#define RAS_GETOP(op) ( (op) & 0xf )
/* Gets the op from a rop. */

#define RAS_GETCOLOR(op) ( ( (op) >> 4 ) & 0xff )
/* Gets the color from a rop. */

#define RAS_ADDR( r, x, y ) \
    ( (r)->pixels + (y) * (r)->linelongs + (x) * (r)->depth / 32 )
/* Gets the longword address of a pixel. */


/* Bit-blit operations.  */

extern int raster_op ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop, struct raster* src, int sx, int sy ));
/* Performs a bitblit.  Returns 0 on success, -1 on failure.  */

extern int raster_op_noclip ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop, struct raster* src, int sx, int sy ));
/* Bitblit without clipping.  Returns 0 on success, -1 on failure. */

extern int raster_replsrc ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop, struct raster* src, int sx, int sy ));
/* Tiles the src to fit the dst.  Returns 0 on success, -1 on failure.  Only
** implements RAS_SRC.
*/

extern int raster_batchop ARGS(( struct raster* dst, int dx, int dy, int rop, struct raster* srcs[], int sxs[], int sys[], int count ));
/* Blits a bunch of sources sequentially into one destination. */

extern int raster_stencil ARGS(( struct raster* dst, int dx, int dy, int w, int h, int rop, struct raster* sten, int stx, int sty, struct raster* src, int sx, int sy ));
/* Draws src through sten onto dst.  sten must be of depth 1. */

extern int raster_point ARGS(( struct raster* r, int x, int y, int rop ));
/* Draws a single point into a raster.  Compare with raster_put().
** Returns 0 on success, -1 on failure.
*/

extern int raster_point_noclip ARGS(( struct raster* r, int x, int y, int rop ));
/* Draws a single point into a raster, without clipping.  Returns 0 on
** success, -1 on failure.
*/


/* Raster routines. */

extern struct raster* raster_alloc ARGS(( int width, int height, int depth ));
/* Allocates a raster.  Returns (struct raster*) 0 on failure. */

extern void raster_free ARGS(( struct raster* r ));
/* Frees/closes a raster. */

extern int raster_get ARGS(( struct raster* r, int x, int y ));
/* Gets a single pixel from a raster.  Returns value on success, -1 on
** failure.
*/

extern int raster_put ARGS(( struct raster* r, int x, int y, int v ));
/* Puts a single pixel into a raster.  Compare with raster_point().
** Returns 0 on success, -1 on failure.
*/

extern int raster_line ARGS(( struct raster* r, int x0, int y0, int x1, int y1, int rop ));
/* Draws a line into a raster.  Returns 0 on success, -1 on failure. */

extern int raster_line_noclip ARGS(( struct raster* r, int x0, int y0, int x1, int y1, int rop ));
/* Draws a line into a raster, without clipping.  Returns 0 on success,
** -1 on failure.
*/

extern int raster_spline3 ARGS(( struct raster* r, int x0, int y0, int x1, int y1, int x2, int y2, int rop ));
/* Draws a three-point spline into a raster.  Returns 0 on success,
** -1 on failure.
*/

extern struct raster* raster_subregion ARGS(( struct raster* r, int x, int y, int width, int height ));
/* Makes a raster that points to a region of another.  Returns
** (struct raster*) 0 on failure.
*/

extern struct raster* raster_static ARGS(( int width, int height, int depth, u_long* data ));
/* Turns static data into a raster.  Returns (struct raster*) 0 on failure. */


/* Text routines. */

extern struct raster_font* raster_fontopen ARGS(( char* fontname ));
/* Opens a font.  Returns (struct raster_font*) 0 on failure. */

extern struct raster_font* raster_defaultfont ARGS(( void ));
/* Returns the default font.  Returns (struct raster_font*) 0 on failure. */

extern int raster_text ARGS(( struct raster* r, int x, int y, int rop, struct raster_font* rf, char* text ));
/* Draws text.  Returns 0 on success, -1 on failure. */

extern int raster_textn ARGS(( struct raster* r, int x, int y, int rop, struct raster_font* rf, char* text, int len ));
/* Draws n characters of text.  Returns 0 on success, -1 on failure. */

extern int raster_textnsize ARGS(( struct raster_font* rf, char* text, int n, int* xP, int* yP, int* wP, int* hP ));
/* Measures n characters of text.  Returns 0 on success, -1 on failure. */

extern void raster_fontclose ARGS(( struct raster_font* rf ));
/* Closes a font. */

extern int raster_ttext ARGS(( struct raster* r, int x, int y, int rop, struct raster_font* rf, char* text ));
/* Draws text with transparent background.  Returns 0 on success, -1 on
** failure.
*/

extern int raster_ttextn ARGS(( struct raster* r, int x, int y, int rop, struct raster_font* rf, char* text, int len ));
/* Draws n characters of text with transparent background.  Returns 0 on
** success, -1 on failure.
*/


/* Frame buffer routines. */

extern struct raster* raster_open ARGS(( char* fbname ));
/* Opens a frame buffer as a raster.  Returns (struct raster*) 0 on failure. */

extern struct raster* raster_coloropen ARGS(( void ));
/* Opens a color frame buffer if there is one.  Returns (struct raster*) 0 on
** failure.
*/

extern int raster_video_off ARGS(( struct raster* r ));
/* Blanks the screen.  Returns 0 on success, -1 on failure. */

extern int raster_video_on ARGS(( struct raster* r ));
/* Re-enables video.  Returns 0 on success, -1 on failure. */

extern struct raster_colormap* raster_colormap_alloc ARGS(( int len ));
/* Allocates a colormap structure, returns 0 on failure. */

extern struct raster_colormap* raster_colormap_get ARGS(( struct raster* r ));
/* Allocates a colormap structure and returns the frame buffer's
** current colormap, or (struct raster_colormap*) 0 on failure.  The raster
** must be one returned by raster_open(), not raster_alloc().
*/

extern int raster_colormap_set ARGS(( struct raster* r, struct raster_colormap* cm ));
/* Sets a frame buffer's colormap.  The raster must be one returned
** by raster_open(), not raster_alloc().  Returns 0 on success, -1 on
** failure.
*/

extern void raster_colormap_free ARGS(( struct raster_colormap* cm ));
/* Frees a colormap. */


/* Rasterfile read/write routines. */

extern struct raster* raster_read ARGS(( FILE* f, struct raster_colormap** cmP ));
/* Allocates a raster and reads the contents from a file.  Returns
** (struct raster*) 0 on failure.  If there's a colormap in the file, it gets
** allocated and returned in cmP; otherwise, cmP is 0.
*/

extern int raster_write ARGS(( FILE* f, struct raster* r, int rt, struct raster_colormap* cm ));
/* Writes a raster to a file.  The colormap is optional.  Returns 0 on
** success, -1 on failure.  The file types are defined below - not all
** of them are implemented.
*/
#define RT_OLD          0       /* Raw image in 68000 byte order */
#define RT_STANDARD     1       /* Raw image in 68000 byte order */
#define RT_BYTE_ENCODED 2       /* Run-length compression of bytes */
#define RT_FORMAT_RGB   3       /* XRGB or RGB instead of XBGR or BGR */
#define RT_FORMAT_TIFF  4       /* tiff <-> standard rasterfile */
#define RT_FORMAT_IFF   5       /* iff (TAAC format) <-> standard rasterfile */
#define RT_EXPERIMENTAL 0xffff  /* Reserved for testing */


/* C struct write routines. */

extern int raster_dump ARGS(( FILE* f, struct raster* r, char* name ));
/* Writes a raster as C structs.  Returns 0 on success, -1 on failure.  */

extern int raster_fontdump ARGS(( FILE* f, struct raster_font* rf, char* name ));
/* Writes a font as C structs.  Returns 0 on success, -1 on failure.  */

#endif /*_RASTER_H_*/
