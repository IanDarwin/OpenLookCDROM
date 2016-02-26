/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/mouse_get.c,v 1.3 91/03/01 11:05:54 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/mouse_get.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/mouse_get.c,v $$Revision: 1.3 $";

/* get the mouse input */

#include <sys/ioctl.h>
#include <stdio.h>
#ifdef MOVIE
#include "bitmap.h"			/* only for DOFLUSH() */
#endif

#define MOUSE_BUFF	90
#define DECODE(X)	( (X) &0x80 ? (X) - 256 : (X) )
#define get_char(fd)	(index<cnt ? 0xff & mouse_buff[index++] :mouse_fill(fd))

static unsigned char mouse_buff[MOUSE_BUFF];
static int cnt=0;
static int index=0;
static int button_map[8] = { 0,1,2,3,4,5,6,7};

int
mouse_fill(mouse)
int mouse;
   {
#ifdef MOVIE
	DOFLUSH();
#endif

    /* this is a botch to correct for a change in read semantics for 4.1
     * that requires the mouse to in non-blocking mode
     */

    while ((cnt = read(mouse,mouse_buff,MOUSE_BUFF))<0) {
       int mask = 1<<mouse;
       select(32,&mask,0,0,NULL);
       }

   index = 1;
   return ( ((int)(*mouse_buff) & 0xff));
   }
   
/* primary mouse interface
   Mouse sample begins with a byte, 0x80 through 0x87, followed by pairs of
   bytes giving motion changes.  Currently we handle 3- or 5-byte mice.
*/

int
mouse_get(mouse,x_delta,y_delta)
register int mouse;
register int *x_delta, *y_delta;
   {
   static int mouse_type = 3;
   register int delta, button;

	if (mouse <=0) 
		return(0);

   while(((button = get_char(mouse))&0xf0) != 0x80)
      if( mouse_type < 5 )
         mouse_type++;
      ;
   delta = get_char(mouse); *x_delta = DECODE(delta);
   delta = get_char(mouse); *y_delta = DECODE(delta);
   if (mouse_type >= 5) {
      /* 5-or-more bytes per sample */
      delta = get_char(mouse), *x_delta += DECODE(delta);
      delta = get_char(mouse), *y_delta += DECODE(delta);
      }
#ifdef MOVIE
	DOFLUSH();
#endif
   return(button_map[(~button)&0x7]);
   }

/* map mouse buttons (for non-left handers) */

int *
map_mouse(button,map)
int button, map;
   {
   if (button > 0 && button < 8)
      button_map[button] = map;
   return(button_map);
   }

/* how many chars are sitting in the mouse buffer */

int
mouse_count()
   {
   return(cnt>index);
   }
