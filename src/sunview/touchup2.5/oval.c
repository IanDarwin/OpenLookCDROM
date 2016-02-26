
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
	file: oval.c
	purpose: this file contain the functions that draw ovals

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments
**************************************************************************/

#include"header.h"


/*
 * draw an oval by making the sides by vectors and ends by semicircles
 */
     draw_oval(pw, center_x,center_y, curr_pos_x,curr_pos_y,ROP,flag)

       Pixwin            *pw;
       int		 center_x,center_y,curr_pos_x,curr_pos_y;
       int                flag,ROP;
   
     {    
       struct  pr_pos     center,
                         curr_pos;
        int             x, y, radius,
			error, npts,
		        h_width, h_height,
			x_off, y_off;

	center.x = center_x;
	center.y = center_y;
	curr_pos.x = curr_pos_x;
	curr_pos.y = curr_pos_y;

          h_width = abs(curr_pos.x - center.x);
          h_height = abs(curr_pos.y - center.y);
          radius = MIN(h_width, h_height);
  
         /*
	  * draw line section of oval
          */
          if (radius == h_height)
           { x_off = h_width-radius;   y_off = 0;
             pw_vector(pw, center.x-x_off, center.y+radius,
                       center.x+x_off, center.y+radius, ROP, cur_color);
             pw_vector(pw, center.x-x_off, center.y-radius,
                       center.x+x_off, center.y-radius, ROP, cur_color);
           }
          else
           { x_off = 0;   y_off = h_height-radius;
             pw_vector(pw, center.x-radius, center.y-y_off,
                       center.x-radius, center.y+y_off, ROP, cur_color);
             pw_vector(pw, center.x+radius, center.y-y_off,
                       center.x+radius, center.y+y_off, ROP, cur_color);
           }
	   /*
	    * draw rounded corners of oval
	    */
	   x = 0;  y = radius; npts = 0;
	   error = 3 - (radius << 1);

         while (x < y)
         {
           ptlist[npts].x=center.x+(x+x_off); ptlist[npts++].y=center.y+(y+y_off);
           ptlist[npts].x=center.x-(x+x_off); ptlist[npts++].y=center.y+(y+y_off);
           ptlist[npts].x=center.x+(x+x_off); ptlist[npts++].y=center.y-(y+y_off);
           ptlist[npts].x=center.x-(x+x_off); ptlist[npts++].y=center.y-(y+y_off);
           ptlist[npts].x=center.x+(y+x_off); ptlist[npts++].y=center.y+(x+y_off);
           ptlist[npts].x=center.x-(y+x_off); ptlist[npts++].y=center.y+(x+y_off);
           ptlist[npts].x=center.x+(y+x_off); ptlist[npts++].y=center.y-(x+y_off);
           ptlist[npts].x=center.x-(y+x_off); ptlist[npts++].y=center.y-(x+y_off);
             
           if (error < 0)
              error = error + (x << 2) + 6;
           else
              error = error + ((x-y--) << 2) + 10;
           x++;
          }  /* end of while (x , y) */
 
          if (x == y)
          {
           ptlist[npts].x=center.x+(x+x_off); ptlist[npts++].y=center.y+(y+y_off);
           ptlist[npts].x=center.x-(x+x_off); ptlist[npts++].y=center.y+(y+y_off);
           ptlist[npts].x=center.x+(x+x_off); ptlist[npts++].y=center.y-(y+y_off);
           ptlist[npts].x=center.x-(x+x_off); ptlist[npts++].y=center.y-(y+y_off);
          }
          if (flag)
		my_pw_polypoint(0,0,npts,ptlist,ROP);
}  /* end of function draw_oval() */

