/*
     Copyright (c) 1994    Frank Adelstein

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */

#include "types.h"

static int RIGHTSTARTZAPX=250;
static int RIGHTSTARTZAPY=50;
static int RIGHTENDZAPX=100;
static int RIGHTENDZAPY=50;

static int LEFTSTARTZAPX=50;
static int LEFTSTARTZAPY=50;
static int LEFTENDZAPX=50;
static int LEFTENDZAPY=50;

void DrawZap(glob, zapper, zappee)
Glob *glob;
Player *zapper, *zappee;
{
  int startx, starty, endx, endy, x, y;
  int direction, i, stepx, stepy;
  Display *display1, *display2;

  display1 = XtDisplay(glob->Toplevel1);

  if (glob->numscreens == 2)
    display2 = XtDisplay(glob->Toplevel2);

  if (zapper->facing == LEFT) 
    direction = -1;
  else
    direction = 1;
  
  /* figure out the start and end values */

  startx = zapper->x + (zapper->facing == LEFT?LEFTSTARTZAPX:RIGHTSTARTZAPX) + 
           zapper->Images1[zapper->moves[zapper->sequence].
			   frameindex[zapper->seqnum]].offsetx;
  starty = zapper->y + (zapper->facing == LEFT?LEFTSTARTZAPY:RIGHTSTARTZAPY) + 
           zapper->Images1[zapper->moves[zapper->sequence].
			   frameindex[zapper->seqnum]].offsety;

  endx = zappee->x + (zapper->facing == LEFT ? LEFTENDZAPX : RIGHTENDZAPX) +
           zappee->Images1[zappee->moves[zappee->sequence].
			   frameindex[zappee->seqnum]].offsetx;
  endy = zappee->y + (zapper->facing == LEFT ? LEFTENDZAPY : RIGHTENDZAPY) + 
           zappee->Images1[zappee->moves[zappee->sequence].
			   frameindex[zappee->seqnum]].offsety;

  for (i = 0; i < 5; i++) {
    /* draw a few different lightening bolts */
    x = startx;
    y = starty;

    while ((direction == 1)?x < (endx - 10) : x > (endx + 10)) {
      /* take a small step (+15 to -5) in the y direction */
      /* take a medium step (+10 to +20) in the x direction */
      stepy = (random() % 20) - 10;
      stepx = ((random() % 10) + 10) * direction;
      
      /* draw the line segment */
      XDrawLine(display1, glob->temp1, glob->whitegc1, 
		x, y, x+stepx, y+stepy);
      if (glob->numscreens == 2) {
	XDrawLine(display2, glob->temp2, glob->whitegc2, 
		  x, y, x+stepx, y+stepy);
      }
      x += stepx;
      y += stepy;
    }
    XDrawLine(display1, glob->temp1, glob->whitegc1, 
	      x, y, endx, endy);
    if (glob->numscreens == 2) {
      XDrawLine(display2, glob->temp2, glob->whitegc2, 
		x, y, endx, endy);
    }
  }
}

void SpringZap(glob, body, head)
Glob *glob;
Player *body;
Listnode *head;
{
  int startx, starty, endx, endy, x, y;
  int i, stepx, stepy;
  Display *display1, *display2;

  display1 = XtDisplay(glob->Toplevel1);

  if (glob->numscreens == 2)
    display2 = XtDisplay(glob->Toplevel2);

  /* figure out the start and end values */

  startx = body->x + 
           body->Images1[body->moves[body->sequence].
			   frameindex[body->seqnum]].offsetx + 
	   body->Images1[body->moves[body->sequence].
			 frameindex[body->seqnum]].high.x2;

  starty = body->y + 
           body->Images1[body->moves[body->sequence].
			   frameindex[body->seqnum]].offsety +
	   body->Images1[body->moves[body->sequence].
			 frameindex[body->seqnum]].high.y2;


  endx = head->object.x +
         head->object.Images1[head->object.moves[head->object.sequence].
			   frameindex[head->object.seqnum]].offsetx +
	 head->object.Images1[head->object.moves[head->object.sequence].
				     frameindex[head->object.seqnum]].high.x2,
  endy = head->object.y + 
         head->object.Images1[head->object.moves[head->object.sequence].
			   frameindex[head->object.seqnum]].offsety +
	 head->object.Images1[head->object.moves[head->object.sequence].
				     frameindex[head->object.seqnum]].high.y2;

  /* take a zig-zag walk to simulate a spring */
  x = startx;
  y = starty;

  stepy = (starty - endy) / 10;		/* we'll have 10 zig-zags */
  if (stepy == 0) stepy++;		/* make sure it's not always zero */
  stepx = 10;				/* 1/2 the with of the coil */

  while (y > endy) {
    /* draw the right and left coil */

    /* draw the line segment */
    XDrawLine(display1, glob->temp1, glob->whitegc1, 
	      x - stepx, y, x+stepx, y-stepy/2);
    XDrawLine(display1, glob->temp1, glob->whitegc1, 
	      x + stepx, y - stepy/2, x-stepx, y-stepy);
    if (glob->numscreens == 2) {
      XDrawLine(display2, glob->temp2, glob->whitegc2, 
		x - stepx, y, x+stepx, y-stepy/2);
      XDrawLine(display2, glob->temp2, glob->whitegc2, 
		x + stepx, y - stepy/2, x-stepx, y-stepy);
    }
    y -= stepy;
  }
}
