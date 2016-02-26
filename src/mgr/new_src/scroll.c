/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/scroll.c,v 1.3 91/03/01 11:05:58 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/scroll.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/scroll.c,v $$Revision: 1.3 $";

/*****************************************************************************
 *	scroll a bitmap
 */

#include <stdio.h>
#include "bitmap.h"
#include "defs.h"
#include "clip.h"

scroll(win,map,start,end,delta,op)
register WINDOW *win;	/* window to scroll */
register BITMAP *map;	/* bitmap in window to scroll */
int start,end,delta;	/* starting line, ending line, # of lines */
   {
   register int ems = end-start;
   if (delta > 0) {
      if (end-start > delta)
#ifdef ALIGN
         if (win->window == map) {
#ifdef DEBUG
            dprintf(F)(stderr,"fast scroll %s\r\n",W(tty));
#endif
            /* special high-speed byte-aligned scroller */

            fast_scroll(map,BIT_X(map),BIT_Y(map) + start,
                     (7&(BIT_X(map))) + BIT_WIDE(map) + SUM_BDR,
                     end-start,delta);
            }
         else
#endif ALIGN
            bit_blit(map,0,start,BIT_WIDE(map),ems-delta,BIT_SRC,map,0,start+delta);
      bit_blit(map,0,end-delta,BIT_WIDE(map),delta,op,0,0,0);
      }

   else if (delta < 0) {
      if (ems + delta > 0)
         bit_blit(map,0,start-delta,BIT_WIDE(map),ems+delta,
             BIT_SRC,map,0,start);
      bit_blit(map,0,start,BIT_WIDE(map),-delta,op,NULL_DATA,0,0);
      }
   

   if (Do_clip()) 
      Set_clip(W(text).x,
               W(text).y + start,
               W(text).x + BIT_WIDE(map),
               W(text).y + BIT_HIGH(map)
              );
   }
