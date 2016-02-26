/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/lib/RCS/dump.h,v 1.2 91/03/01 11:05:05 sau Exp Locker: sau $
	$Source: /home/sau/mgr/lib/RCS/dump.h,v $
*/
static char	h_dump_[] = "$Source: /home/sau/mgr/lib/RCS/dump.h,v $$Revision: 1.2 $";
/* format for saved bitmaps */

#define B_HSIZE		(sizeof(struct b_header))
#define B_OHSIZE		(sizeof(struct old_b_header))

#define NEW_BHDR	1	/* flag for bitmapwrite */
#define OLD_BHDR	0	/* " */

/* given bitmap header, get w[idth] and h[eight] */

#define B_GETOLDHDR(hdr,W,H) ( \
	W = ((int)((hdr)->h_wide - ' ') << 6) + (hdr)->l_wide - ' ', \
	H = ((int)((hdr)->h_high - ' ') << 6) + (hdr)->l_high - ' ')

#define B_GETHDR8(hdr,W,H,D) ( \
	W = ((int)((hdr)->h_wide - ' ') << 6) + (hdr)->l_wide - ' ', \
	H = ((int)((hdr)->h_high - ' ') << 6) + (hdr)->l_high - ' ', \
	D = ((int)((hdr)->depth - ' ')))

/* given w[idth] and h[eight], produce header */

#define B_PUTOLDHDR(hdr,w,h) \
	(hdr)->magic[0]='z', (hdr)->magic[1]='z', \
	(hdr)->h_wide = (((w)>>6)&0x3f) + ' ', \
	(hdr)->l_wide = ((w)&0x3f) + ' ', \
	(hdr)->h_high = (((h)>>6)&0x3f) + ' ', \
	(hdr)->l_high = ((h)&0x3f) + ' '

#define B8_PUTOLDHDR(hdr,w,h) \
	(hdr)->magic[0]='z', (hdr)->magic[1]='y', \
	(hdr)->h_wide = (((w)>>6)&0x3f) + ' ', \
	(hdr)->l_wide = ((w)&0x3f) + ' ', \
	(hdr)->h_high = (((h)>>6)&0x3f) + ' ', \
	(hdr)->l_high = ((h)&0x3f) + ' '

#define B_PUTHDR8(hdr,w,h,d) ( \
	(hdr)->magic[0]='y', (hdr)->magic[1]='z', \
	(hdr)->h_wide = (((w)>>6)&0x3f) + ' ', \
	(hdr)->l_wide = ((w)&0x3f) + ' ', \
	(hdr)->h_high = (((h)>>6)&0x3f) + ' ', \
	(hdr)->l_high = ((h)&0x3f) + ' ', \
	(hdr)->depth = ((d)&0x3f) + ' ', \
	(hdr)->_reserved = ' ' )


/*	Bitmap header magic numbers for new style bitmaps.
	The formats differ only in the amount of padding required at the end
	of each row.
*/

#define B_ISHDR8(hdr) \
	((hdr)->magic[0]=='y' && (hdr)->magic[1]=='z')

#define B_ISHDR16(hdr) \
	((hdr)->magic[0]=='z' && (hdr)->magic[1]=='z')

#define B_ISHDR32(hdr) \
	((hdr)->magic[0]=='x' && (hdr)->magic[1]=='z')

#define B_ISHDR(hdr)	B_ISHDR32(hdr)

/* Old 8 bit per pixel bitmaps */

#define B8_ISHDR(hdr) \
	((hdr)->magic[0]=='x' && (hdr)->magic[1]=='y')

/* squished bitmaps */

#define BS_ISHDR(hdr) \
	((hdr)->magic[0]=='y' && (hdr)->magic[1]=='x')

/*
	#ifdef COLOR
	#define B_ISANY(hdr) \
		(B_ISHDR(hdr) || B8_ISHDR(hdr))
	#else
	#define B_ISANY(hdr)	B_ISHDR(hdr)
	#endif
*/

/* number of bytes of data for bitmap */

#define B_SIZE8(w,h,d)	((h)*((((w*d)+7L)&~7L)>>3))
#define B_SIZE16(w,h,d)	((h)*((((w*d)+15L)&~15L)>>3))
#define B_SIZE32(w,h,d)	((h)*((((w*d)+31L)&~31L)>>3))
#define B_SIZE(w,h)	((h)*((((w)+BITS)&~BITS)>>3))
#define B8_SIZE(w,h)	((h)*(w))

struct old_b_header {
   char magic[2];
   char h_wide;
   char l_wide;
   char h_high;
   char l_high;
   };

struct b_header {
   char magic[2];
   char h_wide;
   char l_wide;
   char h_high;
   char l_high;
   char depth;
   char _reserved;	/* to pad b_header out to 8 bytes, which should be an
			exact alignment on any machine we are likely to
			encounter */
   };
