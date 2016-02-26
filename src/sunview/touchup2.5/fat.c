
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
	file: fat.c
	purpose: This file contains the functions that handle the
		magnifing class command.

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Sun Jun  5 23:47:42 EDT 1988
		author:	rayk
		changes:fixed bug in magnify, so that it does
			a save_screen when you go into fat_mode
**************************************************************************/

#include "header.h"

int old_cur_color=0-1;

/*
 * Let's go into magnify mode !!
 */
fat_mode()
{
    if (image_depth==1)
	old_cur_color = cur_color;
    if (select_pt_x == -1)
       {
	ERROR("Select point first then select Magnify");
	set_select_mode();
	return;
       }
    
    if (fat_source_x != -1)
       select_fat_region();
    fat_source_x = select_pt_x- (int)window_get(fat_canvas, CANVAS_WIDTH)/magnify_fac/2;
    fat_source_y = select_pt_y- (int)window_get(fat_canvas, CANVAS_HEIGHT)/magnify_fac/2;
    clean_point();
    save_screen();
    (void)window_set(fat_frame, WIN_SHOW, TRUE, 0);
    select_fat_region();
    fat_update(0,0);
}


/*
 * kill off the magnify window
 */
fat_done()
{
    if ((image_depth==1) && (old_cur_color != -1))
    {
	cur_color = old_cur_color;
        old_cur_color = 0-1;
    }
    if (fat_source_x != -1)
       select_fat_region();
    (void)window_set(fat_frame, WIN_SHOW, FALSE, 0);
    fat_source_x = 0-1;
    fat_source_y = 0-1;
}



/*
 * The event handler for the canvas of the magnifying class window
 * You can do 3 things.
 *  left: inverse the bit the mouse is on top of and continue
 *		to draw in that color
 *  middle: use this to draw the magnified view
 *  right: use to set the current color to the color of the pixel
 * 		under the cursor
 */
fat_handle_event(canvas_local, event)
Canvas  canvas_local;
Event   *event;
{
    if (event_is_up(event))
       return;
    switch (event_id(event)) {
	 case MS_LEFT:
		if (image_depth ==1)
                   {
		      cur_color = 1^pw_get(pw,fat_source_x + (int)event_x(event)/magnify_fac,fat_source_y + (int)event_y(event)/magnify_fac);
		   }
		fat_draw(fat_pw,pw, event_x(event), event_y(event));
		break;
	 case MS_MIDDLE:
		fat_x = event_x(event);
		fat_y = event_y(event);
		break;
	 case MS_RIGHT:
		 fat_match_color(event_x(event), event_y(event));
       		 break;
         case LOC_DRAG:
            if (window_get(canvas_local, WIN_EVENT_STATE, MS_LEFT))
		fat_draw(fat_pw,pw, event_x(event), event_y(event));
	    else if (window_get(canvas_local, WIN_EVENT_STATE, MS_MIDDLE))
		{
		fat_update(fat_x-event_x(event),fat_y-event_y(event));
		fat_x = event_x(event);
		fat_y = event_y(event);
		}
	    break;

	}
}


/*
 * Find out the color of the pixel under the cursor
 */
fat_match_color(x,y)
int x,y;
{
  x = fat_source_x + x/magnify_fac;
  y = fat_source_y + y/magnify_fac;
  if (image_depth > 1)
    update_cur_color(0,0,pw_get(pw,x,y));
}


/*
 * This is the function that updates the display of the magnifyied view
 * This function is called when the middle but is used to draw this window
 */
fat_update(offset_x,offset_y)
int offset_x,offset_y;
{
int w,h;

  w = (int) window_get(fat_canvas, CANVAS_WIDTH);
  h = (int) window_get(fat_canvas, CANVAS_HEIGHT);
  select_fat_region();
  fat_source_x += offset_x/magnify_fac;
  fat_source_y += offset_y/magnify_fac;
  if (w >= SCREEN_MAX_X)
     w=SCREEN_MAX_X-1;
  if (h >= SCREEN_MAX_Y)
     h=SCREEN_MAX_Y-1;

/*
 * check if you are within the pixrect
 */
#ifdef NO_FASTAN
  if (fat_source_x < 0)
	fat_source_x = 0;
  if (fat_source_y < 0)
	fat_source_y = 0;
  if (fat_source_x+w/magnify_fac > image_wid)
	fat_source_x = image_wid - w/magnify_fac;
  if (fat_source_y+h/magnify_fac > image_hgt)
	fat_source_y = image_hgt - h/magnify_fac;
#endif

  pw_mag(fat_pw, 0, 0, w, h, magnify_fac, pw->pw_prretained,
	 fat_source_x, fat_source_y);
  select_fat_region();
}


/*
 * get the current magnification level
 */
fat_parms()
{
  select_fat_region();
  magnify_fac = (int)panel_get_value(magnify_cycle) + 1;
  select_fat_region();
  fat_update(0,0);
}


/*
 * show which region of the drawing area bitmap is being magnified
 */
select_fat_region()
{
int w,h;
  w = (int) window_get(fat_canvas, CANVAS_WIDTH);
  h = (int) window_get(fat_canvas, CANVAS_HEIGHT);
  select_region(pw,fat_source_x-1,fat_source_y-1,
		  fat_source_x+w/magnify_fac+1,fat_source_y+h/magnify_fac+1);
}



/*
 * handle the drawing inside of the magnified view window
 */
fat_draw(fat_pw,pw,x,y)
struct pixwin *fat_pw,*pw;
int x,y;
{
  x = x/magnify_fac*magnify_fac;
  y = y/magnify_fac*magnify_fac;
  pw_write(fat_pw,x,y,magnify_fac,magnify_fac,PIX_COLOR(cur_color) | PIX_SRC,NILPR,0,0);
  x = fat_source_x + x/magnify_fac;
  y = fat_source_y + y/magnify_fac;
  pw_put(pw,x,y,cur_color);
}

