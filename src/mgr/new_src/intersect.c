/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/intersect.c,v 1.3 91/03/01 11:05:52 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/intersect.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/intersect.c,v $$Revision: 1.3 $";

/*
 *******************************************************************************
 *
 *	see if two windows intersect
 */

#include "bitmap.h"
#include <stdio.h>
#include "defs.h"

#define WIDE(w)	w->BIT_WIDE(border)
#define HIGH(w)	w->BIT_HIGH(border)

intersect(win1,win2)
register WINDOW *win1, *win2;
{
	int result;
	result = (
	    win1->x0 + WIDE(win1) < win2->x0 ||
	    win2->x0 + WIDE(win2) < win1->x0 ||
	    win1->y0 + HIGH(win1) < win2->y0 ||
	    win2->y0 + HIGH(win2) < win1->y0
	    ?0:1); 
        return(result);
}

/*
 *******************************************************************************
 *
 *	see if any window intersects any other
 */

int
alone(check)
register WINDOW *check;
{
	register WINDOW *win;
        for(win=active;win != (WINDOW *) 0;win=win->next)
          if (check!=win && intersect(check,win))
             return(0);
        return(1);
}

/***********************************************************************
 *	see if mouse is in window 
 */

mousein(x,y,win,how)
register int x,y;
register WINDOW *win;
int how;		/* how:  0-> intersect   1-> point */
{
   if (how == 0)
	return(
	    x+16 < W(x0) || x > W(x0) + WIDE(win) ||
            y+16 < W(y0) || y > W(y0) + HIGH(win)
            ?0:1);
   else
	return(
	    x < W(x0) || x > W(x0) + WIDE(win) ||
            y < W(y0) || y > W(y0) + HIGH(win)
            ?0:1);
}

/**********************************************************************
 *      see if mouse is in text region
 */

int
in_text(x,y,win)
register int x,y;
register WINDOW *win;
   {
   if (W(text.wide)) {
      int x0 = W(x0)+W(text.x);
      int y0 = W(y0)+W(text.y);
      return(
	    x < x0 || x > x0 + W(text.wide) ||
            y < y0 || y > y0 + W(text.high)
            ?0:1);
      }
   else
      return(mousein(x,y,win,1));
   }
