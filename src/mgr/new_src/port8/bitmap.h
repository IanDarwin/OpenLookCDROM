/*                        Copyright (c) 1989 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*                        Copyright (c) 1987,1989, 1990 Bellcore
 */
/*	$Header: /home/sau/mgr/nsrc/port8/RCS/bitmap.h,v 1.2 91/03/01 11:55:12 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/port8/RCS/bitmap.h,v $
*/
static char	h_bitmap_[] = "$Source: /home/sau/mgr/nsrc/port8/RCS/bitmap.h,v $$Revision: 1.2 $";

/* header file for SUN 8-bit version of portable bitblit code (S. A. Uhler) */

#ifndef Min
#define Min(x,y)	((x)>(y)?y:x)
#endif

/* Machine configurations go here */

/*
 * DATA is the memory size of the frame buffer.  Usually "unsigned int",
 * but sometimes "unsigned {long,short,char}" is more appropriate
 */

typedef unsigned int DATA;				/* basic frame buffer word size */
#define LOGBITS		5					/* Log2 of bits in type DATA */

/*
 * The macro "GET Most Significant Bits" defines how the bits in each
 * word map from memory to pixels on the display.  The top left most
 * pixel on the display comes from either the *high* order or *low* order
 * bit of the first frame buffer word.  Use "<<" in the first case, ">>"
 * in the second.
 * 
 * The macro "GET Least Significant Bits" does the inverse of GETMSB
 */

#define GETMSB(word,shift)	\
	((word) << (shift))					/* get most significant bits in word */
#define GETLSB(word,shift) \
	((word) >> (shift))					/* get least significant bits in word */

/* these probably won't need changing */

#define BITS	(~(~(unsigned)0 << LOGBITS))		/* mask for bit# within word */
#define MSB		(~GETLSB((unsigned)~0,1))	/* most sig bit set */
#define LSB		(~GETMSB((unsigned)~0,1))	/* least sig bit set */

/*
 * bitmap data has 2 formats, an internal format and an external format.
 * (Sometimes the formats are the same).  The external format is native
 * 68020 SUN/3, DATA aligned 1=black, 0=white.  The internal format is
 * whatever the frame buffer is.  If DOFLIP is set, data is converted
 * from external to internal format the first time it is used.  Bitmap
 * data is recognized as being in external format if the _FLIP flag is
 * set in the type field.  The installation routine flip() does the
 * conversion.
 */

#define DOFLIP (MSB==1)							/* need to flip bytes */

/****************************/

#define ROP_INVERT(x) (x)							/* punt for now */

/* macros for picking proper line, point functions */

#define bit_point(map,x,y,op) mem_point(map,x,y,op)
#define bit_line(map,x1,y1,x2,y2,op) mem_line(map,x1,y1,x2,y2,op)
	
/* Macro to declare a 1 bit per pixel static bitmap */

#define bit_static(name,wide,high,data,n)	\
	BITMAP name = {(DATA *) data, &name, 0, 0, wide, high, \
			n, _STATIC , 0L, 0 }

#define NULL_DATA	((DATA *) 0)				/* NULL bitmap data */
#define BIT_NULL	((BITMAP *) 0)				/* NULL bitmap pointer */

#define IS_SCREEN(x)	((3&(x)->type)==_SCREEN)	/* bitmap is on the display */
#define IS_MEMORY(x)	((3&(x)->type)==_MEMORY)	/* bitmap space malloc'd */
#define IS_STATIC(x)	((3&(x)->type)==_STATIC)	/* bitmap space is static */
#define IS_PRIMARY(x)	((x)->primary == (x))
#define SET_FLIP(x)     ((x)->primary->type |= DOFLIP ? _FLIP : 0)

/*
 * OPCODE(expr), where expr is boolean expression involving SRC and DST,
 * is one of sixteen numbers encoding a rasterop opcode. The values for SRC
 * and DST are abitrary, as long as all 16 patterns may be produced by
 * boolean combinations of them.
 */

#define			DST 	0xA	/* 1010 */
#define			SRC	0xC	/* 1100 */
#define OPCODE(expr)	(0xF&(expr))

/* names for common bitblit functions */

#ifdef BIT_NOT
#undef BIT_NOT
#endif
#define BIT_NOT(x)	(0xF^(x))
#define BIT_SRC		BUILDOP(SRC,BLACK,WHITE)
#define BIT_DST		BUILDOP(DST,BLACK,WHITE)
#define BIT_SET		BUILDOP(~0,BLACK,WHITE)
#define BIT_CLR		BUILDOP(0,BLACK,WHITE)
#define BIT_OR			BUILDOP(BIT_SRC|BIT_DST,BLACK,WHITE)
#define BIT_NOR		BUILDOP(BIT_NOT(BIT_SRC)|BIT_DST,BLACK,WHITE)
#define BIT_XOR		BUILDOP(BIT_SRC^BIT_DST,BLACK,WHITE)
#define BIT_AND		BUILDOP(BIT_SRC&BIT_DST,BLACK,WHITE)
#define BIT_NAND		BUILDOP(BIT_NOT(BIT_SRC)&BIT_DST,BLACK,WHITE)
#define BIT_INV		(BIT_NOT(DST))
#define BIT_INVERT	(BIT_NOT(DST))

/* bitmap types */

#define _SCREEN		1		/* frame buffer */
#define _MEMORY		2		/* malloc'd space */
#define _STATIC		3		/* don't free space at destroy time */
#define _FLIP			4		/* data is in external format */

/* new "bits" in old bitmap, invalidate cache */

#define SET_DIRTY(x) \
			(bit_destroy(BIT_CACHE(x)),BIT_CACHE(x) = NULL)

/* member access macros */

#define BIT_X(x)	x->x0
#define BIT_Y(x)	x->y0
#define BIT_DATA(x)	x->data
#define BIT_WIDE(x)	x->wide
#define BIT_HIGH(x)	x->high
#define BIT_DEPTH(x)	((int)x->depth)

/* for 8-bit image cache */

#define BIT_CACHE(x)	x->primary->cache
#define BIT_CHCLR(x)	x->primary->color

#define BIT_SIZE(m) BIT_Size(BIT_WIDE(m), BIT_HIGH(m), BIT_DEPTH(m)) /* bytes */
#define BIT_Size(wide,high,d)     ((((d*wide+BITS)&~BITS)*high)>>3) /* bytes*/
#define BIT_LINE(x)	\
	((((x)->primary->depth*(x)->primary->wide+BITS)&~BITS)>>LOGBITS)/* words on scan line */

/* structure and type definitions */

typedef struct bitmap {
   DATA	*data;		/* bitmap data */
   struct bitmap	*primary;	/* pointer to primary bitmap */
   short		x0, y0;				/* starting coordinates, in bits */
   short		wide, high;			/* bitmap size, in bits */
   char	depth;					/* bitmap depth */
   char	type;						/* bitmap type */
	struct bitmap *cache;		/* cached 8 bit map for monochrome images */
	unsigned short color;		/* cached color (op>>4) */
   } BITMAP;

/* function declarations */

int mem_rop();
int mem_line();
int mem_point();
int bit_destroy();
int bit_blit();
BITMAP * bit_create();
BITMAP * bit_alloc();
BITMAP * bit_open();
BITMAP * bit_expand();
BITMAP * bit_shrink();

/* for soon to be existant color support */

#define WHITE		0				/* color table index for white */
#define BLACK		255			/* color table index for black */

#define DEPTH				8			/* bits per pixel */
#define NOCOLOR         0xF		/* mask for op code bits */
#define GETFCOLOR(op) \
	(((op)>>4)&0xff)	/* extract fg color from op */
#define GETBCOLOR(op)\
	(((op)>>12)&0xff)	/* extract bg color from op */
#define PUTFCOLOR(op,color)\
	((op)&0xff00f | ((color)&0xff)<<4)		/* insert fg color into op */
#define PUTBCOLOR(op,color)\
	((op)&0x00fff | ((color)&0xff)<<12)	/* insert bg color into op */
#define PUTOP(func,op) \
	((func)&0xf | (op)&~0xf)					/* insert new blit function into op */
#define BUILDOP(op,fg,bg)	\
	((op)&0xf | ((fg)&0xff)<<4 | ((bg)&0xff)<<12)	/* build op */

#define SWAPCOLOR(op)	\
			((op)&0xf | (op)>>8 & 0xff0 | (op)<<8 & 0xff000)	/* switch colors */
