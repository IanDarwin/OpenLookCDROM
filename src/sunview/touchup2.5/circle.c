
/**************************************************************************
   Touchup a bitmap graphics editor for the Sun Workstation running SunView
   Copyright (c) 1988 by Raymond Kreisel
   1/22/88 @ Suny Stony Brook

   This program may be redistributed without fee as long as this copyright
   notice is intact.

==> PLEASE send comments and bug reports to one of the following addresses:

	   Ray Kreisel
	   CS Dept., SUNY at Stony Brook, Stony Brook NY 11794

	   UUCP: {allegra, philabs, pyramid, research}!sbcs!rayk   
	   ARPA-Internet: rayk@sbcs.sunysb.edu			
	   CSnet: rayk@suny-sb
	   (If nobody is home at any of the above addresses try:
		S72QKRE@TOWSONVX.BITNET			        )

 "If I get home before daylight, I just might get some sleep tonight...."

**************************************************************************/

/**************************************************************************
	file: circle.c
	purpose: This file contains that routines that draw circles
		on the screen. A Sunview 3.4 function "pw_polypoint"
		is used to get the list of points that make the circle
		on to the screen AMAZINGLY FAST.
		The points that make up the circle are calculated with
		bresenhams (sp?) incremental circle algorithm.

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Fri Apr 22 17:11:02 EDT 1988
		author:	rayk
		changes:added pw_batching and bitops
**************************************************************************/

#include "header.h"

struct  pr_pos  ptlist[MAX_PTS];

draw_circle(center_x,center_y, radius,ROP)
int		center_x,center_y;
int                radius,ROP;
{
struct pr_pos      center;

        int             x,y,
                        error, numpts;

	if (radius==0) return;
	center.x = center_x;
	center.y = center_y;

           x = 0; y = radius; numpts = 0;
           error = 3 - (radius << 1);

           while (x < y)
           {
             ptlist[numpts].x=center.x+x;   ptlist[numpts++].y=center.y+y;
             ptlist[numpts].x=center.x-x;   ptlist[numpts++].y=center.y+y;
             ptlist[numpts].x=center.x+x;   ptlist[numpts++].y=center.y-y;
             ptlist[numpts].x=center.x-x;   ptlist[numpts++].y=center.y-y;
             ptlist[numpts].x=center.x+y;   ptlist[numpts++].y=center.y+x;
             ptlist[numpts].x=center.x-y;   ptlist[numpts++].y=center.y+x;
             ptlist[numpts].x=center.x+y;   ptlist[numpts++].y=center.y-x;
             ptlist[numpts].x=center.x-y;   ptlist[numpts++].y=center.y-x;
   
             if (error < 0)
                error = error + (x << 2) + 6;
             else
                error = error + ((x-y--) << 2) + 10;
             x++;
	     if (numpts > MAX_PTS-8)
 	     {
		ERROR("Circle is too larege !");
		return;
	     }
           }  /* end of while (x , y) */
   
           if (x == y)
           {
             ptlist[numpts].x=center.x+x;   ptlist[numpts++].y=center.y+y;
             ptlist[numpts].x=center.x-x;   ptlist[numpts++].y=center.y+y;
             ptlist[numpts].x=center.x+x;   ptlist[numpts++].y=center.y-y;
             ptlist[numpts].x=center.x-x;   ptlist[numpts++].y=center.y-y;
           }

	   my_pw_polypoint(0,0,numpts,ptlist,ROP);
}  /* end of function draw_circle() */   



my_pw_polypoint(off_x,off_y,count_pts,ptlist, ROP)
int off_x,off_y,count_pts;
struct pr_pos ptlist[];
int ROP;
{

  if (((ROP == (PIX_SRC)) || (ROP == (PIX_SRC | PIX_DST)))
      && (image_depth == 1))
	 ROP = PIX_SET;

    pw_batch_on(pw);
    while(--count_pts > 0)
    {
      pw_rop(pw,off_x+ptlist[count_pts].x,off_y+ptlist[count_pts].y,1,1
  	,PIX_COLOR(cur_color) | ROP ,pattern[0],0,0);
    }
    pw_batch_off(pw);
}
