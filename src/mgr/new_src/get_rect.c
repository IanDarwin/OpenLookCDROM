/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/get_rect.c,v 1.3 91/03/01 11:05:50 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/get_rect.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/get_rect.c,v $$Revision: 1.3 $";

/* sweep out a rectangle */

#include "bitmap.h"
#include "defs.h"

#define Box(screen,x,y,dx,dy) \
	type ? \
           bit_line(screen,x,y,x+dx,y+dy,BIT_NOT(BIT_DST)) : \
           box(screen,x,y,dx,dy) 


get_rect(screen,mouse,x,y,dx,dy,type)
BITMAP *screen;		/* where to sweep out the box */
int mouse;			/* file to get mouse coords from */
int x,y;			/* starting position */
register int *dx,*dy;		/* box width,height */
int type;			/* rectangle or line */
   {
   int x_mouse, y_mouse;
   register int button;
   register int newx;
   register int newy;
   register int skip=1;

   newx = *dx;
   newy = *dy;

   Box(screen,x,y,*dx,*dy);
   do {
      button=mouse_get(mouse,&x_mouse,&y_mouse);
      if (skip)
         Box(screen,x,y,*dx,*dy);
      newx += x_mouse<<1;
      newy -= y_mouse<<1;
      *dx = BETWEEN(-x,newx,(int) BIT_WIDE(screen)-x);
      *dy = BETWEEN(-y,newy,(int) BIT_HIGH(screen)-y);
      if (skip = !mouse_count())
         Box(screen,x,y,*dx,*dy);
      }
   while (button!=0);

   if (skip)
      Box(screen,x,y,*dx,*dy);
   }

/* draw a box */

box(screen,x1,y1,dx,dy)
BITMAP *screen;
int x1,y1,dx,dy;
   {
   if (dx<0)
      x1 += dx,dx = -dx;
   if (dy<0)
      y1 += dy,dy = -dy;
   if (dx<3)
      dx=3;
   if (dy<3)
      dy=3;

   bit_blit(screen,x1+1,y1,dx-1,1 ,BIT_NOT(BIT_DST),NULL_DATA,0,0);
   bit_blit(screen,x1+1,y1+dy,dx-1,1 ,BIT_NOT(BIT_DST),NULL_DATA,0,0);
   bit_blit(screen,x1,y1,1, dy,BIT_NOT(BIT_DST),NULL_DATA,0,0);
   bit_blit(screen,x1+dx,y1,1, dy,BIT_NOT(BIT_DST),NULL_DATA,0,0);
   }
