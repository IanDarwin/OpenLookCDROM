/* raster_op.c - bitblit routine for raster library
**
** This raster-op is machined to exacting tolerances by skilled native
** craftsmen with pride in their work.
**
** The various cases are broken down like this:
**
**   src required
**       1-bit to 1-bit
**       1-bit to 8-bits
**       8-bits to 8-bits
**   no src required
**       1-bit no-src
**       8-bits no-src
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

/* Definitions. */

/* Raster-op macros.  These encapsulate the switch statements and so make
** the source code 16 times smaller.  The pre and pst args are code
** fragments to put before and after the assignment in each case.  They
** can be the beginning and end of a loop.  If the pst fragment includes a
** masked assignment, for example to handle the left or right edge cases,
** a good optimizing compiler will simplify the boolean expressions very
** nicely - both cc and gcc on the SPARC will do this.
*/

#ifndef PARTIAL_LOGICAL_OPS

#define ROP_DST(op,pre,d,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_DST: \
	/* noop */ \
	break; \
	case RAS_SET: \
	pre \
	(d) = ~0; \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_DSTCOLOR(op,pre,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_DST: \
	/* noop */ \
	break; \
	case RAS_SET: \
	pre \
	(d) = (c); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDST(op,pre,s,d,pst) \
    switch ( op ) \
	{ \
	case RAS_NOTOR: \
	pre \
	(d) = ~( (s) | (d) ); \
	pst \
	break; \
	case RAS_NOTSRC_AND_DST: \
	pre \
	(d) = ~(s) & (d); \
	pst \
	break; \
	case RAS_INVERTSRC: \
	pre \
	(d) = ~(s); \
	pst \
	break; \
	case RAS_SRC_AND_NOTDST: \
	pre \
	(d) = (s) & ~(d); \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	(d) = (s) ^ (d); \
	pst \
	break; \
	case RAS_NOTAND: \
	pre \
	(d) = ~( (s) & (d) ); \
	pst \
	break; \
	case RAS_AND: \
	pre \
	(d) = (s) & (d); \
	pst \
	break; \
	case RAS_NOTXOR: \
	pre \
	(d) = ~( (s) ^ (d) ); \
	pst \
	break; \
	case RAS_NOTSRC_OR_DST: \
	pre \
	(d) = ~(s) | (d); \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	(d) = (s); \
	pst \
	break; \
	case RAS_SRC_OR_NOTDST: \
	pre \
	(d) = (s) | ~(d); \
	pst \
	break; \
	case RAS_OR: \
	pre \
	(d) = (s) | (d); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDSTCOLOR(op,pre,s,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_NOTOR: \
	pre \
	if ( s ) \
	    (d) = ~( (c) | (d) ); \
	else \
	    (d) = ~(d); \
	pst \
	break; \
	case RAS_NOTSRC_AND_DST: \
	pre \
	if ( s ) \
	    (d) = ~(c) & (d); \
	pst \
	break; \
	case RAS_INVERTSRC: \
	pre \
	if ( s ) \
	    (d) = ~(c); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_SRC_AND_NOTDST: \
	pre \
	if ( s ) \
	    (d) = (c) & ~(d); \
	else \
	    (d) = 0; \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	if ( s ) \
	    (d) = (c) ^ (d); \
	pst \
	break; \
	case RAS_NOTAND: \
	pre \
	if ( s ) \
	    (d) = ~( (c) & (d) ); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_AND: \
	pre \
	if ( s ) \
	    (d) = (c) & (d); \
	else \
	    (d) = 0; \
	pst \
	break; \
	case RAS_NOTXOR: \
	pre \
	if ( s ) \
	    (d) = ~( (c) ^ (d) ); \
	else \
	    (d) = ~(d); \
	pst \
	break; \
	case RAS_NOTSRC_OR_DST: \
	pre \
	if ( s ) \
	    (d) = ~(c) | (d); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	if ( s ) \
	    (d) = (c); \
	else \
	    (d) = 0; \
	pst \
	break; \
	case RAS_SRC_OR_NOTDST: \
	pre \
	if ( s ) \
	    (d) = (c) | ~(d); \
	else \
	    (d) = ~(d); \
	pst \
	break; \
	case RAS_OR: \
	pre \
	if ( s ) \
	    (d) = (c) | (d); \
	pst \
	break; \
	default: \
	return -1; \
	}

#else /*PARTIAL_LOGICAL_OPS*/

#define ROP_DST(op,pre,d,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_SET: \
	pre \
	(d) = ~0; \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_DSTCOLOR(op,pre,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_CLEAR: \
	pre \
	(d) = 0; \
	pst \
	break; \
	case RAS_INVERT: \
	pre \
	(d) = ~(d); \
	pst \
	break; \
	case RAS_SET: \
	pre \
	(d) = (c); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDST(op,pre,s,d,pst) \
    switch ( op ) \
	{ \
	case RAS_INVERTSRC: \
	pre \
	(d) = ~(s); \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	(d) = (s) ^ (d); \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	(d) = (s); \
	pst \
	break; \
	default: \
	return -1; \
	}

#define ROP_SRCDSTCOLOR(op,pre,s,d,c,pst) \
    switch ( op ) \
	{ \
	case RAS_INVERTSRC: \
	pre \
	if ( s ) \
	    (d) = ~(c); \
	else \
	    (d) = ~0; \
	pst \
	break; \
	case RAS_XOR: \
	pre \
	if ( s ) \
	    (d) = (c) ^ (d); \
	pst \
	break; \
	case RAS_SRC: \
	pre \
	if ( s ) \
	    (d) = (c); \
	else \
	    (d) = 0; \
	pst \
	break; \
	default: \
	return -1; \
	}

#endif /*PARTIAL_LOGICAL_OPS*/


/* Variables. */

static int needsrc[16] = { 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0 };
/*                       CLEAR          INVERT          DST            SET */

#ifdef MSBIT_FIRST

u_long raster_bitmask[32] = {
    0x80000000, 0x40000000, 0x20000000, 0x10000000,
    0x08000000, 0x04000000, 0x02000000, 0x01000000,
    0x00800000, 0x00400000, 0x00200000, 0x00100000,
    0x00080000, 0x00040000, 0x00020000, 0x00010000,
    0x00008000, 0x00004000, 0x00002000, 0x00001000,
    0x00000800, 0x00000400, 0x00000200, 0x00000100,
    0x00000080, 0x00000040, 0x00000020, 0x00000010,
    0x00000008, 0x00000004, 0x00000002, 0x00000001 };

#ifdef MSBYTE_FIRST
static u_long leftmask[32] = {
    0x00000000, 0x80000000, 0xc0000000, 0xe0000000,
    0xf0000000, 0xf8000000, 0xfc000000, 0xfe000000,
    0xff000000, 0xff800000, 0xffc00000, 0xffe00000,
    0xfff00000, 0xfff80000, 0xfffc0000, 0xfffe0000,
    0xffff0000, 0xffff8000, 0xffffc000, 0xffffe000,
    0xfffff000, 0xfffff800, 0xfffffc00, 0xfffffe00,
    0xffffff00, 0xffffff80, 0xffffffc0, 0xffffffe0,
    0xfffffff0, 0xfffffff8, 0xfffffffc, 0xfffffffe };
static u_long rightmask[32] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff };
#endif /*MSBYTE_FIRST*/

#else /*MSBIT_FIRST*/

u_long raster_bitmask[32] = {
    0x00000001, 0x00000002, 0x00000004, 0x00000008,
    0x00000010, 0x00000020, 0x00000040, 0x00000080,
    0x00000100, 0x00000200, 0x00000400, 0x00000800,
    0x00001000, 0x00002000, 0x00004000, 0x00008000,
    0x00010000, 0x00020000, 0x00040000, 0x00080000,
    0x00100000, 0x00200000, 0x00400000, 0x00800000,
    0x01000000, 0x02000000, 0x04000000, 0x08000000,
    0x10000000, 0x20000000, 0x40000000, 0x80000000 };

#ifndef MSBYTE_FIRST
static u_long leftmask[32] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff };
static u_long rightmask[32] = {
    0x00000000, 0x80000000, 0xc0000000, 0xe0000000,
    0xf0000000, 0xf8000000, 0xfc000000, 0xfe000000,
    0xff000000, 0xff800000, 0xffc00000, 0xffe00000,
    0xfff00000, 0xfff80000, 0xfffc0000, 0xfffe0000,
    0xffff0000, 0xffff8000, 0xffffc000, 0xffffe000,
    0xfffff000, 0xfffff800, 0xfffffc00, 0xfffffe00,
    0xffffff00, 0xffffff80, 0xffffffc0, 0xffffffe0,
    0xfffffff0, 0xfffffff8, 0xfffffffc, 0xfffffffe };
#endif /*not MSBYTE_FIRST*/

#endif /*MSBIT_FIRST*/

/* (The odd combinations MSBIT+~MSBYTE and ~MSBIT+MSBYTE could be added.) */

#ifdef MSBYTE_FIRST
static u_long bytemask[4] = { 0xff000000, 0x00ff0000, 0x0000ff00, 0x000000ff };
#else /*MSBYTE_FIRST*/
static u_long bytemask[4] = { 0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000 };
#endif /*MSBYTE_FIRST*/


/* Forward routines. */

static int raster_op_src_noclip();
static int raster_op_fakesrc_noclip();
static int raster_op_nosrc_noclip();
static u_long makefakesrclong();
static int raster_blit();
static int raster_fakeblit();


/* Raster operations.  */

/* Performs a bitblit.  Returns 0 on success, -1 on failure. */
int
raster_op( dst, dx, dy, w, h, rop, src, sx, sy )
    struct raster* dst;
    int dx, dy, w, h, rop;
    struct raster* src;
    int sx, sy;
    {
    if ( dst == (struct raster*) 0 )
	return -1;			/* no destination */

    if ( needsrc[RAS_GETOP(rop)] )
	{
	/* Two-operand blit. */
	if ( src != (struct raster*) 0 )
	    {
	    /* Clip against source. */
	    if ( sx < 0 )
		{
		w += sx;
		sx = 0;
		}
	    if ( sy < 0 )
		{
		h += sy;
		sy = 0;
		}
	    if ( sx + w > src->width )
		w = src->width - sx;
	    if ( sy + h > src->height )
		h = src->height - sy;
	    }

	/* Clip against dest. */
	if ( dx < 0 )
	    {
	    w += dx;
	    sx -= dx;
	    dx = 0;
	    }
	if ( dy < 0 )
	    {
	    h += dy;
	    sy -= dy;
	    dy = 0;
	    }
	if ( dx + w > dst->width )
	    w = dst->width - dx;
	if ( dy + h > dst->height )
	    h = dst->height - dy;

	if ( w <= 0 || h <= 0 )
	    return 0;			/* nothing to do */

	if ( src == (struct raster*) 0 )
	    return raster_op_fakesrc_noclip( dst, dx, dy, w, h, rop );
	else
	    return raster_op_src_noclip( dst, dx, dy, w, h, rop, src, sx, sy );
	}
    else
	{
	/* No source necessary - one-operand blit. */
	if ( src != (struct raster*) 0 )
	    return -1;			/* unwanted source */

	/* Clip against dest. */
	if ( dx < 0 )
	    {
	    w += dx;
	    dx = 0;
	    }
	if ( dy < 0 )
	    {
	    h += dy;
	    dy = 0;
	    }
	if ( dx + w > dst->width )
	    w = dst->width - dx;
	if ( dy + h > dst->height )
	    h = dst->height - dy;

	if ( w <= 0 || h <= 0 )
	    return 0;			/* nothing to do */

	return raster_op_nosrc_noclip( dst, dx, dy, w, h, rop );
	}
    }

/* Performs a bitblit without clipping.  Returns 0 on success,
** -1 on failure.
*/
int
raster_op_noclip( dst, dx, dy, w, h, rop, src, sx, sy )
    struct raster* dst;
    int dx, dy, w, h, rop;
    struct raster* src;
    int sx, sy;
    {
    if ( dst == (struct raster*) 0 )
	return -1;			/* no destination */

    if ( needsrc[RAS_GETOP(rop)] )
	{
	/* Two-operand blit. */
	if ( src == (struct raster*) 0 )
	    return raster_op_fakesrc_noclip( dst, dx, dy, w, h, rop );
	else
	    return raster_op_src_noclip( dst, dx, dy, w, h, rop, src, sx, sy );
	}
    else
	{
	/* No source necessary - one-operand blit. */
	if ( src != (struct raster*) 0 )
	    return -1;			/* unwanted source */

	return raster_op_nosrc_noclip( dst, dx, dy, w, h, rop );
	}
    }

/* Routine to do a src-bitblit without clipping.  Returns 0 on
** success, -1 on failure.
*/
static int
raster_op_src_noclip( dst, dx, dy, w, h, rop, src, sx, sy )
    struct raster* dst;
    int dx, dy, w, h, rop;
    struct raster* src;
    int sx, sy;
    {
    int op;

    op = RAS_GETOP(rop);

    if ( src->depth == 1 )
	{
	/* One-bit to ? blit. */
	if ( dst->depth == 1 )
	    {
	    /* One to one blit. */
	    u_long* srclin1;
	    u_long* dstlin1;
	    int srcleftignore, srcrightignore, srclongs;
	    int dstleftignore, dstrightignore, dstlongs;

	    srclin1 = RAS_ADDR( src, sx, sy );
	    dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef MEMCPY_FASTER
	    /* Special-case full-width to full-width copies. */
	    if ( op == RAS_SRC && src->width == w && dst->width == w &&
		 src->linelongs == dst->linelongs &&
		 src->linelongs * sizeof(u_long) * 8 == w )
		{
		memcpy(
		    (char*) dstlin1, (char*) srclin1,
		    h * src->linelongs * sizeof(u_long) );
		return 0;
		}
#endif /*MEMCPY_FASTER*/

	    srcleftignore = ( sx & 31 );
	    srclongs = ( srcleftignore + w + 31 ) / 32;
	    srcrightignore = ( srclongs * 32 - w - srcleftignore ) & 31;
	    dstleftignore = ( dx & 31 );
	    dstlongs = ( dstleftignore + w + 31 ) / 32;
	    dstrightignore = ( dstlongs * 32 - w - dstleftignore ) & 31;

	    return raster_blit(
		src, srclin1, srcleftignore, srcrightignore, srclongs,
		dst, dstlin1, dstleftignore, dstrightignore, dstlongs, h, op );
	    }

	else
	    {
	    /* One to eight, using the color in the rop.  This could
	    ** probably be sped up by handling each four-bit source nybble
	    ** as a group, indexing into a 16-element runtime-constructed
	    ** table of longwords.
	    */
	    u_long* srclin1;
	    u_long* dstlin1;
	    u_long* srclin2;
	    u_long* srclin;
	    u_long* dstlin;
	    register u_long* srclong;
	    register u_long* dstlong;
	    register u_long fakesrclong, dl;
	    register int srcbit, dstbyte, i;

	    /* Make 32 bits of color so we can do the ROP without shifting. */
	    fakesrclong = makefakesrclong( dst->depth, RAS_GETCOLOR( rop ) );

	    /* Don't have to worry about overlapping blits here. */
	    srclin1 = RAS_ADDR( src, sx, sy );
	    srclin2 = srclin1 + h * src->linelongs;
	    dstlin1 = RAS_ADDR( dst, dx, dy );
	    srclin = srclin1;
	    dstlin = dstlin1;
	    while ( srclin < srclin2 )
		{
		srclong = srclin;
		srcbit = sx & 31;
		dstlong = dstlin;
		dstbyte = dx & 3;
		i = w;

		/* WARNING: this code is KNOWN TO FAIL on Sun 3's / CG2's. */
		ROP_SRCDSTCOLOR(
		/*op*/  op,
		/*pre*/ while ( i > 0 )
			    {
			    dl = *dstlong;,
		/*s*/       *srclong & raster_bitmask[srcbit],
		/*d*/       dl,
		/*c*/       fakesrclong,
		/*pst*/     *dstlong = ( *dstlong & ~bytemask[dstbyte] ) |
				       ( dl & bytemask[dstbyte] );
			    if ( srcbit == 31 )
				{
				srcbit = 0;
				++srclong;
				}
			    else
				++srcbit;
			    if ( dstbyte == 3 )
				{
				dstbyte = 0;
				++dstlong;
				}
			    else
				++dstbyte;
			    --i;
			    } )

		srclin += src->linelongs;
		dstlin += dst->linelongs;
		}

	    return 0;
	    }
	}

    else
	{
	/* Eight to eight blit. */
	u_long* srclin1;
	u_long* dstlin1;
	int srcleftignore, srcrightignore, srclongs;
	int dstleftignore, dstrightignore, dstlongs;

	if ( dst->depth != 8 )
	    return -1;		/* depth mismatch */

	srclin1 = RAS_ADDR( src, sx, sy );
	dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef MEMCPY_FASTER
	/* Special-case full-width to full-width copies. */
	if ( op == RAS_SRC && src->width == w && dst->width == w &&
	     src->linelongs == dst->linelongs &&
	     src->linelongs * sizeof(u_long) == w ) )
	    {
	    memcpy(
		(char*) dstlin1, (char*) srclin1,
		h * src->linelongs * sizeof(u_long) );
	    return 0;
	    }
#endif /*MEMCPY_FASTER*/

	srcleftignore = ( sx & 3 ) * 8;
	srclongs = ( srcleftignore + w * 8 + 31 ) / 32;
	srcrightignore = ( srclongs * 32 - w * 8 - srcleftignore ) & 31;
	dstleftignore = ( dx & 3 ) * 8;
	dstlongs = ( dstleftignore + w * 8 + 31 ) / 32;
	dstrightignore = ( dstlongs * 32 - w * 8 - dstleftignore ) & 31;

	return raster_blit(
	    src, srclin1, srcleftignore, srcrightignore, srclongs,
	    dst, dstlin1, dstleftignore, dstrightignore, dstlongs, h, op );
	}
    }

/* Routine to do a fake-src-bitblit without clipping.  Returns 0 on
** success, -1 on failure.
*/
static int
raster_op_fakesrc_noclip( dst, dx, dy, w, h, rop )
    struct raster* dst;
    int dx, dy, w, h, rop;
    {
    int op;
    u_long fakesrclong;

    op = RAS_GETOP(rop);
    fakesrclong = makefakesrclong( dst->depth, RAS_GETCOLOR(rop) );

    if ( dst->depth == 1 )
	{
	/* One to one fakesrc blit. */
	u_long* dstlin1;
	int dstleftignore, dstrightignore, dstlongs;

	dstlin1 = RAS_ADDR( dst, dx, dy );

	dstleftignore = ( dx & 31 );
	dstlongs = ( dstleftignore + w + 31 ) / 32;
	dstrightignore = ( dstlongs * 32 - w - dstleftignore ) & 31;

	return raster_fakeblit(
	    fakesrclong, dst, dstlin1, dstleftignore, dstrightignore,
	    dstlongs, h, op );
	}
    else
	{
	/* Eight to eight fakesrc blit. */
	u_long* dstlin1;
	int dstleftignore, dstrightignore, dstlongs;

	dstlin1 = RAS_ADDR( dst, dx, dy );

	dstleftignore = ( dx & 3 ) * 8;
	dstlongs = ( dstleftignore + w * 8 + 31 ) / 32;
	dstrightignore = ( dstlongs * 32 - w * 8 - dstleftignore ) & 31;

	return raster_fakeblit(
	    fakesrclong, dst, dstlin1, dstleftignore, dstrightignore,
	    dstlongs, h, op );
	}
    }

/* Routine to do a no-src bitblit without clipping.  Returns 0
** on success, -1 on failure.
*/
static int
raster_op_nosrc_noclip( dst, dx, dy, w, h, rop )
    struct raster* dst;
    int dx, dy, w, h, rop;
    {
    int op;

    op = RAS_GETOP(rop);

    if ( dst->depth == 1 )
	{
	/* One-bit no-src blit. */
	u_long* dstlin1;
	u_long* dstlin2;
	u_long* dstlin;
	int dstleftignore, dstrightignore, dstlongs;
	u_long dl, lm, nlm, rm, nrm;
	register u_long* dstlong2;
	register u_long* dstlong;

	dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef MEMCPY_FASTER
	/* Special-case full-width clears. */
	if ( op == RAS_CLEAR && dst->width == w &&
	     dst->linelongs * sizeof(u_long) * 8 == w )
	    {
	    memset( (char*) dstlin1, 0, h * dst->linelongs * sizeof(u_long) );
	    return 0;
	    }
#endif /*MEMCPY_FASTER*/

	dstleftignore = ( dx & 31 );
	dstlongs = ( dstleftignore + w + 31 ) / 32;
	dstrightignore = ( dstlongs * 32 - w - dstleftignore ) & 31;

	dstlin2 = dstlin1 + h * dst->linelongs;
	dstlin = dstlin1;

	if ( dstlongs == 1 )
	    {
	    /* It fits into a single longword. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    while ( dstlin < dstlin2 )
		{
		ROP_DST(
		/*op*/  op,
		/*pre*/ dl = *dstlin;,
		/*d*/   dl,
		/*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		dstlin += dst->linelongs;
		}
	    }
	else
	    {
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;

	    while ( dstlin < dstlin2 )
		{
		dstlong = dstlin;
		dstlong2 = dstlong + dstlongs;
		if ( dstrightignore != 0 )
		    --dstlong2;

		/* Leading edge. */
		if ( dstleftignore != 0 )
		    {
		    ROP_DST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
		    ++dstlong;
		    }

		/* Main rop. */
		ROP_DST(
		/*op*/  op,
		/*pre*/ while ( dstlong < dstlong2 )
			    {,
		/*d*/       *dstlong,
		/*pst*/     ++dstlong;
			    } )

		/* Trailing edge. */
		if ( dstrightignore != 0 )
		    {
		    ROP_DST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
		    }

		dstlin += dst->linelongs;
		}
	    }
	}

    else
	{
	/* Eight-bit no-src blit. */
	register u_long fakesrclong;
	u_long* dstlin1;
	u_long* dstlin2;
	u_long* dstlin;
	int dstleftignore, dstrightignore, dstlongs;
	u_long dl, lm, nlm, rm, nrm;
	register u_long* dstlong2;
	register u_long* dstlong;

	dstlin1 = RAS_ADDR( dst, dx, dy );

#ifdef MEMCPY_FASTER
	/* Special-case full-width clears. */
	if ( op == RAS_CLEAR && dst->width == w &&
	     dst->linelongs * sizeof(u_long) == w )
	    {
	    memset( (char*) dstlin1, 0, h * dst->linelongs * sizeof(u_long) );
	    return 0;
	    }
#endif /*MEMCPY_FASTER*/

	/* Make 32 bits of color so we can do the ROP without shifting. */
	fakesrclong = makefakesrclong( dst->depth, RAS_GETCOLOR( rop ) );

	dstleftignore = ( dx & 3 ) * 8;
	dstlongs = ( dstleftignore + w * 8 + 31 ) / 32;
	dstrightignore = ( dstlongs * 32 - w * 8 - dstleftignore ) & 31;

	dstlin2 = dstlin1 + h * dst->linelongs;
	dstlin = dstlin1;

	if ( dstlongs == 1 )
	    {
	    /* It fits into a single longword. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    while ( dstlin < dstlin2 )
		{
		ROP_DSTCOLOR(
		/*op*/  op,
		/*pre*/ dl = *dstlin;,
		/*d*/   dl,
		/*c*/	fakesrclong,
		/*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		dstlin += dst->linelongs;
		}
	    }
	else
	    {
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;
	    while ( dstlin < dstlin2 )
		{
		dstlong = dstlin;
		dstlong2 = dstlong + dstlongs;
		if ( dstrightignore != 0 )
		    --dstlong2;

		/* Leading edge. */
		if ( dstleftignore != 0 )
		    {
		    ROP_DSTCOLOR(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*c*/   fakesrclong,
		    /*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
		    ++dstlong;
		    }

		/* Main rop. */
		ROP_DSTCOLOR(
		/*op*/  op,
		/*pre*/ while ( dstlong < dstlong2 )
			    {,
		/*d*/       *dstlong,
		/*c*/       fakesrclong,
		/*pst*/     ++dstlong;
			    } )

		/* Trailing edge. */
		if ( dstrightignore != 0 )
		    {
		    ROP_DSTCOLOR(
		    /*op*/  op,
		    /*pre*/ dl = *dstlong;,
		    /*d*/   dl,
		    /*c*/   fakesrclong,
		    /*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
		    }

		dstlin += dst->linelongs;
		}
	    }
	}

    return 0;
    }

static u_long
makefakesrclong( depth, color )
    int depth;
    u_long color;
    {
    if ( depth == 1 )
	{
	if ( color != 0 )
	    return ~0;
	else
	    return 0;
	}
    else
	if ( color != 0 )
	    return ( color << 24 ) | ( color << 16 ) | ( color << 8 ) | color;
	else
	    return ~0;
    }

/* This is a general bitblit routine, handling overlapping source and
** destination.  It's used for both the 1-to-1 and 8-to-8 cases.
*/
static int
raster_blit( src, srclin1, srcleftignore, srcrightignore, srclongs, dst, dstlin1, dstleftignore, dstrightignore, dstlongs, h, op )
    struct raster* src;
    u_long* srclin1;
    int srcleftignore, srcrightignore, srclongs;
    struct raster* dst;
    u_long* dstlin1;
    int dstleftignore, dstrightignore, dstlongs;
    int h, op;
    {
    u_long* srclin2;
    u_long* dstlin2;
    int srclininc, dstlininc;
    u_long* srclin;
    u_long* dstlin;
    register int prevleftshift, currrightshift;
    int longinc;
    register u_long* srclong;
    register u_long* dstlong;
    register u_long* dstlong2;
    register u_long dl, lm, nlm, rm, nrm;

    prevleftshift = ( srcleftignore - dstleftignore ) & 31;

    srclin2 = srclin1 + h * src->linelongs;
    dstlin2 = dstlin1 + h * dst->linelongs;
    srclininc = src->linelongs;
    dstlininc = dst->linelongs;
    longinc = 1;

    /* Check for overlaps. */
    if ( ( dstlin1 >= srclin1 && dstlin1 < srclin1 + srclongs ) ||
	 ( srclin1 >= dstlin1 && srclin1 < dstlin1 + dstlongs ) )
	{
	/* Horizontal overlap.  Should we reverse? */
	if ( srclin1 < dstlin1 )
	    {
	    longinc = -1;
	    srclin1 += srclongs - 1;
	    srclin2 += srclongs - 1;
	    dstlin1 += dstlongs - 1;
	    }
	}
    else if ( ( dstlin1 >= srclin1 && dstlin1 < srclin2 ) ||
	      ( srclin1 >= dstlin1 && srclin1 < dstlin2 ) )
	{
	/* Vertical overlap.  Should we reverse? */
	if ( srclin1 < dstlin1 )
	    {
	    srclin2 = srclin1 - srclininc;
	    srclin1 += ( h - 1 ) * srclininc;
	    dstlin1 += ( h - 1 ) * dstlininc;
	    srclininc = -srclininc;
	    dstlininc = -dstlininc;
	    }
	}
    srclin = srclin1;
    dstlin = dstlin1;

    if ( prevleftshift == 0 )
	{
	/* The bits line up, no shifting necessary. */
	if ( dstlongs == 1 )
	    {
	    /* It all fits into a single longword. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    while ( srclin != srclin2 )
		{
		ROP_SRCDST(
		/*op*/  op,
		/*pre*/ dl = *dstlin;,
		/*s*/   *srclin,
		/*d*/   dl,
		/*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		srclin += srclininc;
		dstlin += dstlininc;
		}
	    }
	else
	    {
	    /* Multiple longwords. */
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;
	    if ( longinc == 1 )
		{
		/* Left to right. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong + dstlongs;
		    if ( dstrightignore != 0 )
			--dstlong2;

		    /* Leading edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			++srclong;
			++dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong < dstlong2 )
				{,
		    /*s*/       *srclong,
		    /*d*/       *dstlong,
		    /*pst*/     ++srclong;
				++dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    else
		{
		/* Right to left. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong - dstlongs;
		    if ( dstleftignore != 0 )
			++dstlong2;

		    /* Leading edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			--srclong;
			--dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong > dstlong2 )
				{,
		    /*s*/       *srclong,
		    /*d*/       *dstlong,
		    /*pst*/     --srclong;
				--dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   *srclong,
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    }
	}

    else
	{
	/* General case, with shifting and everything. */
	register u_long sl, prevsl;

	currrightshift = 32 - prevleftshift;
	if ( srclongs == 1 && dstlongs == 1 )
	    {
	    /* It fits into a single longword, with a shift. */
	    lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	    nlm = ~lm;
	    if ( srcleftignore > dstleftignore )
		{
		while ( srclin != srclin2 )
		    {
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlin;,
		    /*s*/   *srclin << prevleftshift,
		    /*d*/   dl,
		    /*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    else
		{
		while ( srclin != srclin2 )
		    {
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ dl = *dstlin;,
		    /*s*/   *srclin >> currrightshift,
		    /*d*/   dl,
		    /*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    }
	else
	    {
	    /* Multiple longwords. */
	    lm = leftmask[dstleftignore];
	    rm = rightmask[dstrightignore];
	    nrm = ~rm;
	    nlm = ~lm;
	    if ( longinc == 1 )
		{
		/* Left to right. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong + dstlongs;
		    if ( srcleftignore > dstleftignore )
			prevsl = *srclong++ << prevleftshift;
		    else
			prevsl = 0;
		    if ( dstrightignore != 0 )
			--dstlong2;

		    /* Leading edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ sl = *srclong;
				dl = *dstlong;,
			/*s*/   prevsl | ( sl >> currrightshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			prevsl = sl << prevleftshift;
			++srclong;
			++dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong < dstlong2 )
				{
				sl = *srclong;,
		    /*s*/       prevsl | ( sl >> currrightshift ),
		    /*d*/       *dstlong,
		    /*pst*/     prevsl = sl << prevleftshift;
				++srclong;
				++dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   prevsl | ( *srclong >> currrightshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    else
		{
		/* Right to left. */
		while ( srclin != srclin2 )
		    {
		    srclong = srclin;
		    dstlong = dstlin;
		    dstlong2 = dstlong - dstlongs;
		    if ( srcrightignore > dstrightignore )
			prevsl = *srclong-- >> currrightshift;
		    else
			prevsl = 0;
		    if ( dstleftignore != 0 )
			++dstlong2;

		    /* Leading edge. */
		    if ( dstrightignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ sl = *srclong;
				dl = *dstlong;,
			/*s*/   prevsl | ( sl << prevleftshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
			prevsl = sl >> currrightshift;
			--srclong;
			--dstlong;
			}

		    /* Main rop. */
		    ROP_SRCDST(
		    /*op*/  op,
		    /*pre*/ while ( dstlong > dstlong2 )
				{
				sl = *srclong;,
		    /*s*/       prevsl | ( sl << prevleftshift ),
		    /*d*/       *dstlong,
		    /*pst*/     prevsl = sl >> currrightshift;
				--srclong;
				--dstlong;
				} )

		    /* Trailing edge. */
		    if ( dstleftignore != 0 )
			{
			ROP_SRCDST(
			/*op*/  op,
			/*pre*/ dl = *dstlong;,
			/*s*/   prevsl | ( *srclong << prevleftshift ),
			/*d*/   dl,
			/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
			}

		    srclin += srclininc;
		    dstlin += dstlininc;
		    }
		}
	    }
	}

    return 0;
    }

/* This is bitblit routine for fakesrc blits only.  It's used for both
** the 1-to-1 and 8-to-8 cases.
*/
static int
raster_fakeblit( fakesrclong, dst, dstlin1, dstleftignore, dstrightignore, dstlongs, h, op )
    u_long fakesrclong;
    struct raster* dst;
    u_long* dstlin1;
    int dstleftignore, dstrightignore, dstlongs;
    int h, op;
    {
    u_long* dstlin2;
    int dstlininc;
    u_long* dstlin;
    register u_long* dstlong;
    register u_long* dstlong2;
    register u_long dl, lm, nlm, rm, nrm;

    dstlin2 = dstlin1 + h * dst->linelongs;
    dstlininc = dst->linelongs;
    dstlin = dstlin1;

    /* The bits always line up in a fakesrc blit, no shifting necessary. */
    if ( dstlongs == 1 )
	{
	/* It all fits into a single longword. */
	lm = leftmask[dstleftignore] | rightmask[dstrightignore];
	nlm = ~lm;
	while ( dstlin != dstlin2 )
	    {
	    ROP_SRCDST(
	    /*op*/  op,
	    /*pre*/ dl = *dstlin;,
	    /*s*/   fakesrclong,
	    /*d*/   dl,
	    /*pst*/ *dstlin = ( *dstlin & lm ) | ( dl & nlm ); )

	    dstlin += dstlininc;
	    }
	}
    else
	{
	/* Multiple longwords. */
	lm = leftmask[dstleftignore];
	rm = rightmask[dstrightignore];
	nrm = ~rm;
	nlm = ~lm;
	while ( dstlin != dstlin2 )
	    {
	    dstlong = dstlin;
	    dstlong2 = dstlong + dstlongs;
	    if ( dstrightignore != 0 )
		--dstlong2;

	    /* Leading edge. */
	    if ( dstleftignore != 0 )
		{
		ROP_SRCDST(
		/*op*/  op,
		/*pre*/ dl = *dstlong;,
		/*s*/   fakesrclong,
		/*d*/   dl,
		/*pst*/ *dstlong = ( *dstlong & lm ) | ( dl & nlm ); )
		++dstlong;
		}

	    /* Main rop. */
	    ROP_SRCDST(
	    /*op*/  op,
	    /*pre*/ while ( dstlong < dstlong2 )
			{,
	    /*s*/       fakesrclong,
	    /*d*/       *dstlong,
	    /*pst*/     ++dstlong;
			} )

	    /* Trailing edge. */
	    if ( dstrightignore != 0 )
		{
		ROP_SRCDST(
		/*op*/  op,
		/*pre*/ dl = *dstlong;,
		/*s*/   fakesrclong,
		/*d*/   dl,
		/*pst*/ *dstlong = ( dl & nrm ) | ( *dstlong & rm ); )
		}

	    dstlin += dstlininc;
	    }
	}

    return 0;
    }

/* Draws a single point into a raster.  Compare with raster_put().
** Returns 0 on success, -1 on failure.
*/
int
raster_point( r, x, y, rop )
    struct raster* r;
    int x, y, rop;
    {
    if ( x < 0 || y < 0 || x >= r->width || y >= r->height )
	return -1;

    return raster_point_noclip( r, x, y, rop );
    }

/* Draws a single point into a raster, without clipping.  Returns 0 on
** success, -1 on failure.
*/
int
raster_point_noclip( r, x, y, rop )
    struct raster* r;
    int x, y, rop;
    {
    int op;
    int dstleftignore, dstrightignore;
    register u_long dl, m, nm;
    register u_long* dstlong;

    op = RAS_GETOP(rop);

    if ( r->depth == 1 )
	{
	/* One-bit point */
	dstlong = RAS_ADDR( r, x, y );
	dstleftignore = x & 31;
	dstrightignore = ( 31 - dstleftignore ) & 31;

	m = leftmask[dstleftignore] | rightmask[dstrightignore];
	nm = ~m;
	ROP_DST(
	/*op*/  op,
	/*pre*/ dl = *dstlong;,
	/*d*/   dl,
	/*pst*/ *dstlong = ( *dstlong & m ) | ( dl & nm ); )
	}

    else
	{
	/* Eight-bit point. */
	register u_long fakesrclong;

	dstlong = RAS_ADDR( r, x, y );

	/* Make 32 bits of color so we can do the ROP without shifting. */
	fakesrclong = makefakesrclong( r->depth, RAS_GETCOLOR( rop ) );

	dstleftignore = ( x & 3 ) * 8;
	dstrightignore = ( 24 - dstleftignore ) & 31;

	m = leftmask[dstleftignore] | rightmask[dstrightignore];
	nm = ~m;
	ROP_DSTCOLOR(
	/*op*/  op,
	/*pre*/ dl = *dstlong;,
	/*d*/   dl,
	/*c*/	fakesrclong,
	/*pst*/ *dstlong = ( *dstlong & m ) | ( dl & nm ); )
	}

    return 0;
    }
