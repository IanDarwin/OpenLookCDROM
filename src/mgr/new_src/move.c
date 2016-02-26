/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/move.c,v 1.3 91/03/01 11:05:54 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/move.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/move.c,v $$Revision: 1.3 $";

/* move a window */

#include "bitmap.h"
#include <stdio.h> 	/* temporary */
#include "defs.h"
#include "event.h"

int move_window()
   {
   int button;
   int dx = BIT_WIDE(ACTIVE(border));
   int dy = BIT_HIGH(ACTIVE(border));
   int sx = ACTIVE(x0);
   int sy = ACTIVE(y0);
   register WINDOW *win;

   move_box(screen,mouse,&sx,&sy,dx,dy,0);

   /* adjust window state */

   mousex += sx-ACTIVE(x0);
   mousey += sy-ACTIVE(y0);

   shape(sx,sy,dx,dy);
#ifdef ALIGN
   if (dx != BIT_WIDE(ACTIVE(border)) || dy != BIT_HIGH(ACTIVE(border)))
      do_event(EVENT_SHAPE,active,E_MAIN);
   else
#endif
      do_event(EVENT_MOVE,active,E_MAIN);

   /* wait till button is released */

   do {
      button=mouse_get(mouse,&sx,&sy);
      }
   while (button!=0);
   }
