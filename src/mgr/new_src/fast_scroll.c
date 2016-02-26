/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/fast_scroll.c,v 1.3 91/03/01 11:06:04 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/fast_scroll.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/fast_scroll.c,v $$Revision: 1.3 $";

/*
 * fast scroll for 68010 assuming byte boundaries  (SAU)
 *	This code is highly machine dependent
 */

#include <stdio.h>
#include "bitmap.h"

#define BYTESWIDE(x)	((x->primary->wide+7)>>3)

#ifndef mc68020

/* these macros rely upon the proper register assignments */

#define START(x)	asm("x:	movl	d7,d0")
#define LOOP(l)		asm("l:	movw	a3@+,a4@+"); \
			asm("	dbf	d0,l")
#define GOTO(x)		asm("	dbf	d5,x")
#define SKIP()		asm("	addl	d6,a4"); \
			asm("	addl	d6,a3")
#define BYTE()		asm("	movb	a3@+,a4@+")
#define ADJ(x)		asm("	subql	#x,d7")

fast_scroll(map,x,y,wide,high,delta)
register BITMAP *map;					/* a5 */
int x,y,wide,high,delta;
   {
   register unsigned char *dst = (unsigned char *) 	/* a4 */
         ((long) (map->data) + (y*BYTESWIDE(map) + (x>>3)));
   register unsigned char *src =			/* a3 */
         dst + (delta*BYTESWIDE(map));
   register long count =				/* d7 */
         (wide>>4) - 1;			/* # of shorts - 1 for inner dbf */
   register long skip = 				/* d6 */
         BYTESWIDE(map)-(wide>>3);	/* bytes to skip at right edge */
   register long h = 					/* d5 */
         high-delta-1;			/* # of lines to scroll */
   int which = (((long) dst)&1) + ((skip&1)<<1);

   switch (which) {
      case 0:			/* skip&1 == 0 */
	 START(C0); LOOP(LP0); SKIP(); GOTO(C0);
         break;
      case 1:
         ADJ(1); START(C1); BYTE(); LOOP(LP1); BYTE(); SKIP(); GOTO(C1);
         break;

      case 2:			/* skip&1 == 1 */
	 START(C2); LOOP(LP2); BYTE(); SKIP(); GOTO(C2);
         break;
      case 3:
	 START(C3); BYTE(); LOOP(LP3); SKIP(); GOTO(C3);
         break;
      }
   }

/*
 * fast scroll for 68020 assuming byte boundaries  (SAU)
 *	This code is highly machine dependent
 */

#else

/* these macros rely upon the proper register assignments */

#define START(x)	asm("x:	movl	d7,d0")
#define LOOP(l)		asm("l:	movl	a3@+,a4@+"); \
			asm("	dbf	d0,l")
#define GOTO(x)		asm("	dbf	d5,x")
#define SKIP()		asm("	addl	d6,a4"); \
			asm("	addl	d6,a3")
#define WORD()		asm("	movw	a3@+,a4@+")
#define BYTE()		asm("	movb	a3@+,a4@+")
#define ADJ(x)		asm("	subql	#x,d7")

fast_scroll(map,x,y,wide,high,delta)
register BITMAP *map;					/* a5 */
int x,y,wide,high,delta;
   {
   register unsigned char *dst = (unsigned char *) 	/* a4 */
         ((long) (map->data) + (y*BYTESWIDE(map) + (x>>3)));
   register unsigned char *src =			/* a3 */
         dst + (delta*BYTESWIDE(map));
   register long count =				/* d7 */
         (wide>>5) - 1;			/* # of longs - 1 for inner dbf */
   register long skip = 				/* d6 */
         BYTESWIDE(map)-(wide>>3);	/* bytes to skip at right edge */
   register long h = 					/* d5 */
         high-delta-1;			/* # of lines to scroll */
   int which = (((long) dst)&3) + ((skip&3)<<2);

   switch (which) {
      case 0:			/* skip&3 == 0 */
	 START(C0); LOOP(LP0); SKIP(); GOTO(C0);
         break;
      case 1:
         ADJ(1); START(C1); BYTE(); WORD(); LOOP(LP1); BYTE(); SKIP(); GOTO(C1);
         break;
      case 2:
         ADJ(1); START(C2); WORD(); LOOP(LP2); WORD(); SKIP(); GOTO(C2);
         break;
      case 3:
         ADJ(1); START(C3); BYTE(); LOOP(LP3); WORD(); BYTE(); SKIP(); GOTO(C3);
         break;

      case 4:			/* skip&3 == 1 */
	 START(C4); LOOP(LP4); WORD(); BYTE(); SKIP(); GOTO(C4);
         break;
      case 5:
	 START(C5); BYTE(); WORD(); LOOP(LP5); SKIP(); GOTO(C5);
         break;
      case 6:
	 START(C6); WORD(); LOOP(LP6); BYTE(); SKIP(); GOTO(C6);
         break;
      case 7:
	 START(C7); BYTE(); LOOP(LP7); WORD(); SKIP(); GOTO(C7);
         break;

      case 8:			/* count%4 == 2 */
	 START(C8); LOOP(LP8); WORD(); SKIP(); GOTO(C8);
         break;
      case 9:
         ADJ(1); START(C9); BYTE(); WORD(); LOOP(LP9);
         WORD(); BYTE(); SKIP(); GOTO(C9);
         break;
      case 10:
	 START(C10); WORD(); LOOP(LP10); SKIP(); GOTO(C10);
         break;
      case 11:
	 START(C11); BYTE(); LOOP(LP11); BYTE(); SKIP(); GOTO(C11);
         break;

      case 12:			/* count%4 == 3 */
	 START(C12); LOOP(LP12); BYTE(); SKIP(); GOTO(C12);
         break;
      case 13:
         ADJ(1); START(C13); BYTE(); WORD(); LOOP(LP13);
         WORD(); SKIP(); GOTO(C13);
         break;
      case 14:
         ADJ(1); START(C14); WORD(); LOOP(LP14); WORD();
         BYTE(); SKIP(); GOTO(C14);
         break;
      case 15:
	 START(C15); BYTE(); LOOP(LP15); SKIP(); GOTO(C15);
         break;
      }
   }

/* normal bit-blit version of the above (for testing) */

Fast_scroll(map,x,y,wide,high,delta)
register BITMAP *map;					/* a5 */
int x,y,wide,high,delta;
   {
   bit_blit(map,x&7,y,wide&7,high,
            BIT_SRC,map,x&7,delta);
   }
#endif
