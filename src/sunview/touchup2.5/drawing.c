
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
	file: drawing.c
	purpose: This file has most of the functions that draw stuff
	on the screen.

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Fri Apr 15 02:08:11 EDT 1988
		author:	rayk
		changes:added scale cut/paste

		date:	Tue Apr 19 14:05:10 EDT 1988
		author:	rayk
		changes:fixed no-borders on polygons

		date:	Fri Apr 22 03:05:28 EDT 1988
		author:	rainbow
		changes:added FAST rotate

		date:	Fri Apr 22 17:11:02 EDT 1988
		author:	rayk
		changes:added pw_batch to make things much FASTER

		date:	Wed Jun 15 14:40:48 EDT 1988
		author:	juang@aloft.att.com
		changes:worked around bug in pw_stencil that
			would not display color paint brushes properly
			on Sun 3/160 & Sun 3/260

		date:	Wed Jun 15 16:49:43 EDT 1988
		author:	rayk
		changes:added toggle to make it so that
			lasso will clear the area that has been selected

		date:	Fri Jul  1 22:17:49 EDT 1988
		author:	rainbow & rayk
		changes:added the ability to draw thick lines

		date:	Sat Jul  2 01:03:38 EDT 1988
		author:	rayk  & rainbow
		changes:made paint so that it would always paint continuous
			lines instead of discontinuous dots

		date:	Tue Jul  5 00:36:02 EDT 1988
		author:	rayk
		changes:changed TEXT command so that if you hold down shift
			it will draw with pw_ttext

		date:	Thu Jul  7 23:35:57 EDT 1988
		author:	rayk
		changes:added 'moving' lines box for select region command

               	date:   Fri Jul  8 18:51:48 MET 1988
               	author: Pell
               	changes:added 'register' in a couple of places, in case
                       	there is someone who doesn't use GNU CC...
                       	Shrunk the size of the eraser.
			Added support for cursor switching.

               	date:   Sun Jul 24 12:47:33 EDT 1988
               	author: rayk
               	changes:added interactive text input, and centering, left
			and right justified text

**************************************************************************/

#include"header.h"
#include<math.h>

struct pr_pos poly_points[MAX_POLY];
int brush_radius[] = { 1, 2, 3, 4, 6, 8, 10};

struct pixrect *brush_temp_pr = NULL;

/*
 * What we do that someone selects a new pattern
 */
select_pattern()
{
  panel_set(current_pattern,PANEL_LABEL_IMAGE,pattern[(int)panel_get_value(pattern_choice)],0);

}


/*
 * let the user define his own patterns to paint with
 * works for color and mono
 */
pattern_define()
{

  if (select_pt_x == -1)
     {
	ERROR("Select a point first, then select Define Pattern.");
	set_select_mode();
	return;
     }
  select_point(select_pt_x,select_pt_y);
  if (image_depth > 1)
   {
      pattern[39]  = my_mem_create(PATTERN_SIZE,PATTERN_SIZE,image_depth);
      pattern40_pr = *(pattern[39]);
   }
  pr_rop(pattern[39],0,0,PATTERN_SIZE,PATTERN_SIZE,
		PIX_SRC,pw->pw_prretained,select_pt_x-PATTERN_SIZE/2,
		select_pt_y-PATTERN_SIZE/2);

  panel_paint(pattern_choice,PANEL_NO_CLEAR);
  reset_point();
  print_msg("The user defined pattern is now stored in the last pattern element.");
}


/*
 * Take the current text string and put that Baby up on the bitmap
 * in the right font
 */
draw_text(x,y,str,ROP)
int x,y,ROP;
char *str;
{
   if (ROP == TRANSPARENT)
    {
      pw_ttext(pw,x,y,PIX_COLOR(cur_color) | PIX_SRC,
		real_font,
		str);
    }
   else
    {
      pw_text(pw,x,y,PIX_COLOR(cur_color) | ROP,
		real_font,
		str);
    }
}


/*
 * draw a line on the bitmap
 */
draw_line(x1,y1,x2,y2,ROP,color)
int x1,y1,x2,y2,ROP,color;
{
  pw_vector(pw,x1,y1,x2,y2,ROP,color);
}


/*
 * draw a thick line on the bitmap
 */
draw_thick_line(x1,y1,x2,y2,ROP,color,thickness)
int x1,y1,x2,y2,ROP,color,thickness;
{
double r,R,q,q1,q2,s;
int t1,t2;
int npts[1];

  if (thickness > 1)
  {
/*      R = (double)sqrt_fast((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2));  */

    R = (double)sqrt((double)((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)));

    r = ((double)thickness);
    q = r/R;
    q1 = q*(double)(y1-y2);
    q2 = q*(double)(x1-x2);
    t1 = (int)rint((q1>0)?q1+0.01:q1-0.01);
    t2 = (int)rint((q2>0)?q2+0.01:q2-0.01);
    s = fabs(q1)+fabs(q2)-(double)(abs(t1)+abs(t2));
    if (fabs(s) > 0.5)
      if (fabs(q1-(double)t1) > fabs(q2-(double)t2))
        t1 += ((q1-(double)t1)>0)?1:-1;
      else t2 += ((q2-(double)t2)>0)?1:-1;
    ptlist[0].x = x1+t1;
    ptlist[0].y = y1-t2;
    ptlist[1].x = x1-t1;
    ptlist[1].y = y1+t2;
    ptlist[2].x = x2-t1;
    ptlist[2].y = y2+t2;
    ptlist[3].x = x2+t1;
    ptlist[3].y = y2-t2;
    npts[0] = 4;
    pw_polygon_2(pw,0,0,1,npts,ptlist,ROP | PIX_COLOR(color),
  		pattern[(int)panel_get_value(pattern_choice)],0,0);
  }
  else
    pw_vector(pw,x1,y1,x2,y2,ROP,color);
}


/*
 * draw a rectangle on the bitmap
 */
draw_rectangle(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
int ROP,radius;
extern int rec_brush;

  top_x = x1;
  top_y = y1;
  bottom_x = x2;
  bottom_y = y2;
  region_fix();
  pw_batch_on(pw);
  if ((int)panel_get_value(command_choice)==RECT_F)
	fill_region(TRUE);
  else
  	select_region(pw,old_x,old_y,start_x,start_y);

  if (!(ROP = get_current_ROP()))
      ROP = PIX_SRC;
  if ((int)panel_get_value(border_cycle)
       && ((int)panel_get_value(command_choice)==RECT_H))
  {
    if (rec_brush)
    	radius = brush_radius[rec_brush];
    else
	radius = 0;

    draw_thick_line(top_x,top_y,bottom_x,top_y,
			ROP,cur_color,brush_radius[rec_brush]);
    draw_thick_line(bottom_x,top_y-radius,bottom_x,bottom_y+radius,
			ROP,cur_color,brush_radius[rec_brush]);
    draw_thick_line(bottom_x,bottom_y,top_x,bottom_y,
			ROP,cur_color,brush_radius[rec_brush]);
    draw_thick_line(top_x,bottom_y+radius,top_x,top_y-radius,
			ROP,cur_color,brush_radius[rec_brush]);
  }
  if ((int)panel_get_value(border_cycle)
       && ((int)panel_get_value(command_choice)==RECT_F))
  {
    pw_vector(pw,x1,y1,x2,y1,ROP,cur_color);
    pw_vector(pw,x2,y1,x2,y2,ROP,cur_color);
    pw_vector(pw,x2,y2,x1,y2,ROP,cur_color);
    pw_vector(pw,x1,y2,x1,y1,ROP,cur_color);
  }

  pw_batch_off(pw);
  reset_region();
}


/*
 * highlight the selected region on the drawing area by throwing up a
 * XORed rectangle
 */
select_region(pw,x1,y1,x2,y2)
struct pixwin *pw;
int x1,y1,x2,y2;
{
  pw_vector(pw,x1,y1,x2,y1,PIX_XOR,1);
  pw_vector(pw,x2,y1,x2,y2,PIX_XOR,1);
  pw_vector(pw,x2,y2,x1,y2,PIX_XOR,1);
  pw_vector(pw,x1,y2,x1,y1,PIX_XOR,1);
}


/*
 * reset the current REGION
 */
reset_region()
{
  top_x=0;
  top_y=0;
  bottom_x=0;
  bottom_y=0;
}


/*
 * draw up a point, how stupid ???
 */
draw_point(pw,x,y)
struct pixwin *pw;
int x,y;
{
  pw_put(pw,x,y,cur_color);
}


/*
 * Draw up the paint brush by copying the current pattern
 * through a stencil of the current brush
 */
draw_con_brush(pw,x1,y1,x2,y2)
struct pixwin *pw;
int x1,y1,x2,y2;
{
int ROP,brush_index;

  brush_index = (int)panel_get_value(brush_choice);
  if (!(ROP = get_current_ROP()))
    ROP = PIX_SRC;

  if (brush_index)
  {
      if ((x1 != x2) || (y1 != y2));
      {
        draw_thick_line(x1,y1,x2,y2,ROP,cur_color,
    		  brush_radius[brush_index]);
      }
        draw_brush(pw, x1,y1);
  }
  else
      draw_line(x1,y1,x2,y2,ROP,cur_color);
}


/*
 * Draw up the paint brush by copying the current pattern
 * through a stencil of the current brush
 */
draw_brush(pw,x,y)
struct pixwin *pw;
int x,y;
{
int ROP;

  if (!(ROP = get_current_ROP()))
    ROP = PIX_SRC;

  if (brush_temp_pr == NULL)
    brush_temp_pr = my_mem_create(PATTERN_SIZE,PATTERN_SIZE,image_depth);

  if (brush_temp_pr->pr_depth != image_depth)
  {
        MY_pr_destroy(brush_temp_pr);
        brush_temp_pr = my_mem_create(PATTERN_SIZE,PATTERN_SIZE,image_depth);
  }

  if (((int)panel_get_value(pattern_choice) != 39) || (image_depth == 1))
  {
        pr_replrop(brush_temp_pr,0,0,PATTERN_SIZE,PATTERN_SIZE, PIX_COLOR(cur_color) | PIX_SRC,pattern[(int)panel_get_value(pattern_choice)],x-PATTERN_SIZE/2,y-PATTERN_SIZE/2);
  }
  else
  {
        pr_replrop(brush_temp_pr,0,0,PATTERN_SIZE,PATTERN_SIZE, PIX_SRC,pattern[(int)panel_get_value(pattern_choice)],x,y);
  }

  pw_stencil(pw,x-PATTERN_SIZE/2,y-PATTERN_SIZE/2,PATTERN_SIZE,PATTERN_SIZE,
  ROP, brushes[(int)panel_get_value(brush_choice)],0,0,brush_temp_pr,0,0);
}


/*
 * flip-flop two varibles (ints)
 */
swap(x,y)
int *x,*y;
{
int temp;
  temp = *x;
  *x = *y;
  *y = temp;
}


region_fix()
{
  if (top_x > bottom_x)
     swap(&top_x,&bottom_x);
  if (top_y > bottom_y)
     swap(&top_y,&bottom_y);
} 


/*
 * put the eraser on the drawing area
 */
erase_brush(pw,x,y)
struct pixwin *pw;
int x,y;
{
  pw_batch_on(pw);
  select_region(pw,old_x,old_y,old_x+CURSOR_SIZE,old_y+CURSOR_SIZE);
  pw_rop(pw,x-CURSOR_SIZE/2,y-CURSOR_SIZE/2,CURSOR_SIZE,CURSOR_SIZE, PIX_SRC,0,0,0);
  old_x = x-CURSOR_SIZE/2; old_y= y-CURSOR_SIZE/2;
  select_region(pw,old_x,old_y,old_x+CURSOR_SIZE,old_y+CURSOR_SIZE);
  pw_batch_off(pw);
}


/*
 * draw up some cross hairs to be used to select a point
 */
#define CROSS_HAIR 20
select_point(x,y)
int x,y;
{
  pw_vector(pw,x-CROSS_HAIR,y,x+CROSS_HAIR,y,PIX_XOR,1);
  pw_vector(pw,x,y-CROSS_HAIR,x,y+CROSS_HAIR,PIX_XOR,1);
}


/*
 * reset the currently selected point
 */
reset_point()
{
  select_pt_x = 0-1;
  select_pt_y = 0-1;
}


/*
 * take a currently selected region and invert that baby ! FAST !!!
 */
inverse_region()
{
  if (top_x || top_y || bottom_x || bottom_y)
    {
     select_region(pw,top_x,top_y,bottom_x,bottom_y);
     region_fix();
     pw_replrop(pw,top_x,top_y,bottom_x-top_x,bottom_y-top_y, PIX_XOR, pattern[0],0,0);
     select_region(pw,top_x,top_y,bottom_x,bottom_y);
    }
  else
    {
     ERROR("Select a region first, then select INVERSE.");
     panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
     change_cursor(canvas, old_cur);
#endif
     mouse_parms();
    }
}



/*
 * take a currently selected region and rotate around the center
 * point.  New FAST version that rotates in 3n bitblt where n is
 * number of raster lines in the source
 */
rotate_region()
{
register int    i, width, height;
int             new_x, new_y, y1, y2, n;
struct pixrect	*tmp1, *tmp2;

  if (top_x || top_y || bottom_x || bottom_y)
    {
     move_box(CLEAR);
     region_fix();
     width = bottom_x - top_x;
     height = bottom_y - top_y;
     new_x = top_x + (width - height) / 2;
     new_y = top_y + (height - width ) /2;
     tmp1 = my_mem_create(width+height-1, height, image_depth);
     tmp2 = my_mem_create(width+height-1, width, image_depth);

     for(i=0; i<height; i++)
	   pr_rop(tmp1, i, i, width, 1, PIX_SRC, pw->pw_prretained, top_x, top_y+i);

     for(i=0; i<=width+height-1; i++)
        {
	   n = (i+1<height) ? i+1 : ((i>=width) ? (width+height-1-i) : height);
           y1 = (i<width) ? (width-1-i) : 0;
           y2 = (i>=width) ? (i-width+1) : 0;
           pr_rop(tmp2, i, y1, 1, n, PIX_SRC, tmp1, i, y2);
        }

     pw_batch_on(pw);
     for(i=0; i<width; i++)
	   pw_rop(pw, new_x, new_y+i, height, 1, PIX_SRC, tmp2, width-1-i, i);
     pw_batch_off(pw);
     MY_pr_destroy(tmp1);
     MY_pr_destroy(tmp2);

     top_x = new_x;
     top_y = new_y;
     bottom_x = new_x+height;
     bottom_y = new_y+width;
     move_box(CLEAR);
    }
  else
    {
     ERROR("Select a region first, then select ROTATE.");
     panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
     change_cursor(canvas, old_cur);
#endif
     mouse_parms();
    }
}



/*
 * take a currently selected region and make a mirror image of it
 */
flip_hor_region()
{
register i;
  if (top_x || top_y || bottom_x || bottom_y)
    {
     move_box(CLEAR);
     region_fix();
     MY_pr_destroy(cut_buffer_pr);
     cut_buffer_pr = my_mem_create(bottom_x-top_x,bottom_y-top_y,image_depth);
     pr_rop(cut_buffer_pr,0,0,bottom_x-top_x,bottom_y-top_y,
		PIX_SRC,pw->pw_prretained,top_x,top_y);
     pw_batch_on(pw);
     for (i = 0; i < cut_buffer_pr->pr_size.x; i++) {
	    pw_rop(pw, top_x+(cut_buffer_pr->pr_size.x - i)-1,top_y,
			1,cut_buffer_pr->pr_size.y, PIX_SRC, cut_buffer_pr,i,0);
	}
     move_box(CLEAR);
     pw_batch_off(pw);
    }
  else
    {
     ERROR("Select a region first, then select MIRROR.");
     panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
     change_cursor(canvas, old_cur);
#endif
     mouse_parms();
    }
}


/*
 * take a currently selected region and turn it upside down
 */
flip_ver_region()
{
register i;
  if (top_x || top_y || bottom_x || bottom_y)
    {
     move_box(CLEAR);
     region_fix();
     MY_pr_destroy(cut_buffer_pr);
     cut_buffer_pr = my_mem_create(bottom_x-top_x,bottom_y-top_y,image_depth);
     pw_batch_on(pw);
     pr_rop(cut_buffer_pr,0,0,bottom_x-top_x,bottom_y-top_y,
		PIX_SRC,pw->pw_prretained,top_x,top_y);
     for (i = 0; i < cut_buffer_pr->pr_size.y; i++) {
	    pw_rop(pw, top_x, top_y+(cut_buffer_pr->pr_size.y - i)-1,
			cut_buffer_pr->pr_size.x, 1, PIX_SRC, cut_buffer_pr, 0, i);
	}
     move_box(CLEAR);
     pw_batch_off(pw);
    }
  else
    {
     ERROR("Select a region first, then select FLIP VERTICAL.");
     panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
     change_cursor(canvas, old_cur);
#endif
     mouse_parms();
    }
}


/*
 * take a current Cut/Paste and scale it
 */
scale_region()
{
int ROP;
float inc,count;
register line;
struct pixrect  *temp_pix;

  if ((top_x || top_y || bottom_x || bottom_y) && cut_buffer_pr)
    {
     if (!(ROP = get_current_ROP()))
	ROP = PIX_SRC;
     region_fix();

     temp_pix = my_mem_create(cut_buffer_pr->pr_size.x,bottom_y-top_y,image_depth);
     inc = ((float)cut_buffer_pr->pr_size.y)/(float)(bottom_y-top_y);
     for (count = 0.0,line = 0; count < cut_buffer_pr->pr_size.y ; count +=inc, line++)
        {
	   pr_rop(temp_pix,0,line,
	     cut_buffer_pr->pr_size.x, 1, ROP, cut_buffer_pr, 0, (int)count);
	}
     inc = ((float)cut_buffer_pr->pr_size.x)/(float)(bottom_x-top_x);
     pw_batch_on(pw);
     move_box(CLEAR); 
     for (count = 0.0, line = 0; count < cut_buffer_pr->pr_size.x;  count +=inc, line++)
        {
	    pw_rop(pw,top_x + line,top_y,
		1,temp_pix->pr_size.y, ROP,temp_pix,(int)count,0);
	}
     move_box(CLEAR);
     pw_batch_off(pw);
     MY_pr_destroy(temp_pix);
    }
  else
    {
     ERROR("Select a destination region first, then select SCALE.");
     panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
     change_cursor(canvas, old_cur);
#endif
     mouse_parms();
    }
}



/*
 * grab what is in the Cut/Paste buffer and put it up on the drawing area
 */
paste_region()
{
int ROP;

  if (select_pt_x == -1)
     {
	ERROR("Select a point first, then select PASTE.");
	set_select_mode();
	return;
     }
  if (cut_buffer_pr)
  {
     if (!(ROP = get_current_ROP()))
	ROP = PIX_SRC;
     select_point(select_pt_x,select_pt_y);
     save_screen();
     pw_write(pw,select_pt_x,select_pt_y,cut_buffer_pr->pr_size.x,cut_buffer_pr->pr_size.y, ROP, cut_buffer_pr,0,0);
     reset_point();
  }
  else
	ERROR("The Cut/Paste buffer is empty.");
}


/*
 * grab the currently selected region on the drawing area and stuff
 * it into the cut/paste buffer AND destroy the source area by
 * filling in with the current paint pattern
 */
cut_region()
{
int t1,t2,t3,t4;
  if (top_x || top_y || bottom_x || bottom_y)
    {
      t1 = top_x;
      t2 = top_y;
      t3 = bottom_x;
      t4 = bottom_y;
      copy_region();
      top_x = t1;
      top_y = t2;
      bottom_x = t3;
      bottom_y = t4;
      move_box(CLEAR);
      run_box=FALSE;
      fill_region(FALSE);
      panel_set(command_choice,PANEL_VALUE, GET_PT,0);
#ifdef CHANGE_CURSOR
     change_cursor(canvas, sel_point_cur);
#endif
      mouse_left = GET_PT;
      print_msg("Region copied to Cut/Paste buffer, select a point and then press PASTE.");
    }
   else
    {
     ERROR("Select a region first, then select CUT.");
     panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
     change_cursor(canvas, old_cur);
#endif
     mouse_parms();
    }
}


/*
 * grab the currently selected region on the drawing area and stuff
 * it into the cut/paste buffer
 */
copy_region()
{
  if (top_x || top_y || bottom_x || bottom_y)
    {
     move_box(CLEAR);
     region_fix();
     MY_pr_destroy(cut_buffer_pr);
     cut_buffer_pr = my_mem_create(bottom_x-top_x,bottom_y-top_y,image_depth);
     pr_rop(cut_buffer_pr,0,0,bottom_x-top_x,bottom_y-top_y,
		PIX_SRC,pw->pw_prretained,top_x,top_y);
     move_box(CLEAR);
    }
   else
    {
	ERROR("Select a region first, then select COPY.");
        panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
        change_cursor(canvas, old_cur);
#endif
        mouse_parms();
    }
}


/*
 * take the cut/paste buffer and XOR it that stuff on to the drawing area
 * so that I can move that sucker around FAST
 */
move_region(old_x,old_y,new_x,new_y)
int old_x,old_y,new_x,new_y;
{
  if (cut_buffer_pr)
    {
     pw_batch_on(pw);
     pw_write(pw,old_x-cut_buffer_pr->pr_size.x/2,
		 old_y-cut_buffer_pr->pr_size.y/2,
		 cut_buffer_pr->pr_size.x,
		 cut_buffer_pr->pr_size.y,
		 PIX_XOR, cut_buffer_pr,0,0);

     pw_write(pw,new_x-cut_buffer_pr->pr_size.x/2,
		 new_y-cut_buffer_pr->pr_size.y/2,
		 cut_buffer_pr->pr_size.x,
		 cut_buffer_pr->pr_size.y,
		 PIX_XOR, cut_buffer_pr,0,0);
     pw_batch_off(pw);
    }
   else
    {
       ERROR("CUT or COPY a region first, then select MOVE.");
    }
}


/*
 * fill in a rectanglar region with the current paint pattern
 */
fill_region(sel_reg)
int sel_reg;
{
int ROP;

   if (top_x || top_y || bottom_x || bottom_y)
    {
      if (sel_reg)
      	select_region(pw,top_x,top_y,bottom_x,bottom_y);
      region_fix();
      if (!(ROP = get_current_ROP()))
	ROP = PIX_SRC;
      pw_replrop(pw,top_x,top_y,bottom_x-top_x,bottom_y-top_y,PIX_COLOR(cur_color) | ROP,pattern[(int)panel_get_value(pattern_choice)],0,0);
      reset_region();
    }
}


/*
 * let the user lasso any free form region on the drawing area
 * and stuff that into the cut/paste buffer
 */
laso_cut_paste()
{
register int i;
int no_points,npts[1];

  top_x = image_wid;
  top_y = image_hgt;
  bottom_x = 0;
  bottom_y = 0;

  i=0;
  while ((i<MAX_PTS) && (ptlist[i++].x != -1));
  no_points = --i;
  npts[0] = no_points; 

  /*
   * if the user booted with a '-l' option then clear the area in the lasso
   * if not then just XOR the area in the lasso
   */
  if (!(int)panel_get(lasso_remove, PANEL_TOGGLE_VALUE,0))
  {
    pw_polygon_2(pw,0,0,1,npts,ptlist,PIX_XOR,pattern[0],0,0);
    pw_polygon_2(pw,0,0,1,npts,ptlist,PIX_XOR,pattern[0],0,0);
  }

  for (i=0;i < no_points;i++)
    {
        top_x = MIN(top_x,ptlist[i].x);
        top_y = MIN(top_y,ptlist[i].y);
        bottom_x = MAX(bottom_x,ptlist[i].x);
        bottom_y = MAX(bottom_y,ptlist[i].y);
    	if (i>0)
	    pw_vector(pw,ptlist[i].x,ptlist[i].y,
    		     ptlist[i-1].x,ptlist[i-1].y,PIX_XOR,1);
    }
  for (i=0;i < no_points;i++)
    {
        ptlist[i].x -=top_x;
        ptlist[i].y -=top_y;
    }
  MY_pr_destroy(cut_buffer_pr);
  cut_buffer_pr = my_mem_create(bottom_x-top_x,bottom_y-top_y,image_depth);
  pr_polygon_2(cut_buffer_pr,0,0,1,npts,ptlist,PIX_SRC,pw->pw_prretained,top_x,top_y);

  if ((int)panel_get(lasso_remove, PANEL_TOGGLE_VALUE,0))
  {
    pw_polygon_2(pw,top_x,top_y,1,npts,ptlist,PIX_CLR,0,0,0);
  }
  reset_region();
  print_msg("The selected area is now in the Cut/Paste buffer.");
}


/*
 * the user can lasso any area on the screen by just encircling the
 * object on the bitmap
 * we do this by remembering all of the points the mouse moved to
 * and make a polygon stencil from these points
 */
laso_addpt(py_pts,x,y)
struct pr_pos py_pts[];
int x,y;
{
int found;
register int i;
int npts[1];

  found = FALSE;
  i=0;
  while ((i<MAX_PTS) && (!found))
    {
      if (py_pts[i++].x == -1)
	found=TRUE;
    }
  i--;
  npts[0] =i;
        
  py_pts[i].x = x;
  py_pts[i].y = y;
  if (i>0)
    pw_vector(pw,py_pts[i].x,py_pts[i].y,py_pts[i-1].x,py_pts[i-1].y,PIX_XOR,1);
  i++;
  py_pts[i].x = 0-1;
  py_pts[i].y = 0-1;
}


/*
 * add a point to the list of vetexs in the current polygon
 */
poly_addpt(py_pts,x,y)
struct pr_pos py_pts[];
int x,y;
{
int found;
register int i;

  found =0;
  i=0;
  while (i<MAX_POLY && !found)
    {
      if (py_pts[i++].x == -1)
	found=TRUE;
    }
  i--;
       {
         py_pts[i].x = x;
         py_pts[i].y = y;
	 i++;
	 py_pts[i].x = 0-1;
	 py_pts[i].y = 0-1;
       }
}


/*
 * take the list of current vertexs and draw up the
 * the polygon and fill it with the current paint pattern
 */
draw_poly(py_pts)
struct pr_pos py_pts[];
{
int npts[1];
int found,ROP;
register int i;


  if (py_pts[0].x == 0-1)
	return;

  found =0;
  i=0;
  while (i<MAX_POLY && !found)
    {
      if (py_pts[i++].x == -1)
	found=TRUE;
    }
  i--;

  npts[0] =i;

  /*
   * get the current ROP for drawing or default PIX_SRC
   */
  if (!(ROP = get_current_ROP()))
	ROP = PIX_SRC;

  /*
   * erase the orginal vectors in the poly
   */
  {
      i=1;
      while (i<MAX_POLY && (py_pts[i].x != -1))
        {
    	pw_vector(pw,py_pts[i].x,py_pts[i].y,
    		     py_pts[i-1].x,py_pts[i-1].y, PIX_XOR,1);
    	i++;
        }
  }


  /*
   * do we want this baby filled ????
   */ 
  if ((int)panel_get_value(command_choice)==POLY_F)
       {
           if (!(ROP = get_current_ROP()))
		ROP = PIX_SRC;

	   pw_polygon_2(pw,0,0,1,npts,py_pts,PIX_COLOR(cur_color) | ROP,pattern[(int)panel_get_value(pattern_choice)],0,0);
       }

  /*
   * do we want to wrap the polygon up in a vector border
   * but if we on unfilled-poly then always do vectors
   */
  if (((int)panel_get_value(border_cycle)) || 
	((int)panel_get_value(command_choice)==POLY_H))
  {
      i=1;
      while (i<MAX_POLY && (py_pts[i].x != -1))
        {
    	pw_vector(pw,py_pts[i].x,py_pts[i].y,
    		     py_pts[i-1].x,py_pts[i-1].y,PIX_COLOR(cur_color) | ROP,cur_color);
    	i++;
        }
      i--;
      pw_vector(pw,py_pts[i].x,py_pts[i].y,
    		     py_pts[0].x,py_pts[0].y,PIX_COLOR(cur_color) | ROP,cur_color);
  }
  clean_poly();
}


/*
 * reset the vertex list for the current polygon
 */
clean_poly()
{
  poly_points[0].x = 0-1;
  poly_points[0].y = 0-1;
}

