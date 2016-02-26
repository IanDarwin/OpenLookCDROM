
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
	file: touchup.c
	purpose: this file has misc functions that do different crap
		but mostly the event handler for the main drawing
		area
	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Fri Apr 15 00:33:52 EDT 1988
		author:	rayk
		changes:added user definable ROPs and default ROPs

		date:	Tue Apr 19 21:45:52 EDT 1988
		author:	rayk
		changes:added LOC_TRAJECTORY for paint command so that
			it does not skip that much when you move the cursor
			quickly

		date:	Thu Apr 21 20:14:58 EDT 1988
		author:	rayk
		changes:change the interface to text, so that you can
			drag the text

		date:	Thu Apr 25 3:02:23  EDT 1988
		author:	rainbow
		changes:made "undo" so that it toggle between buffers

		date:	Mon May  2 22:18:11 EDT 1988
		author:	rayk
		changes:corrected a bug of not setting the correct
			colormap fields on boot up, bug noted by:
			   mcgill-vision!amadeus!gamin

		date:	Fri Jun  3 22:13:11 EDT 1988
		author:	rayk
		changes:corrected a bug of returning the pointer
			to a pixrect in my_mem_create, bug noted by:
			   weiser.pa@Xerox.com
			
		date:	Wed Jun 15 22:04:25 EDT 1988
		author:	juang@aloft.att.com
		changes:fixed SunView command line arguments that
			are passsed to the base frame

		date:	Fri Jun 24 16:12:53 EDT 1988
		author:	rayk  (suggested by mesard@WILMA.BBN.COM)
		changes:fixed PENCIL command so that it would draw
			continious lines instead of single points

		date:	Tue Jun 28 12:34:29 EDT 1988
		author:	rainier!pell@uunet.UU.NET
		changes:corrected a declaration error for old_x ....

		date:	Sat Jul  2 01:03:38 EDT 1988
		author:	rayk & rainbow
		changes:made paint so that it would always paint continuous
			lines instead of discontinuous dots

		date:	Mon Jul  4 22:45:44 EDT 1988
		author:	rayk
		changes:added a new command line option to toggle if the
			window foreground and background color will ALWAYS
			be used, i.e. even if the colormap says different

		date:	Thu Jul  7 23:35:57 EDT 1988
		author:	rayk
		changes:added 'moving' lines box for select region command

		date:   Fri Jul  8 19:20:26 MET 1988
		author: pell@rainier.se
		changes:added support for cursor switching

		date:   Fri Jul 22 12:54:58 EDT 1988
		author: rayk
		changes:added a Black & White mode for mono bitmap editing
			on color Suns

		date:   Sun Jul 24 12:47:33 EDT 1988
		author: rainbow
		changes:fixed moving box so that looks like it snakes around
			the corners.

**************************************************************************/

extern int cur_text_x,cur_text_y,org_text_x, text_cursor;
extern char cur_text_str[];
#include "header.h"

unsigned char red[256],green[256],blue[256];
int image_wid=DEFAULT_IMAGE_WID, image_hgt=DEFAULT_IMAGE_HGT,image_depth=1;
int top_x=0,top_y=0,bottom_x=0,bottom_y=0;
int cur_color=1,grid_size=0;
int magnify_fac=8,fat_x,fat_y,fat_source_x=0-1,fat_source_y=0-1;
int mouse_left=PAINT,undo_flag=TRUE;
int select_pt_x=0-1,select_pt_y=0-1;
int old_x=0, old_y=0;
int start_x=0, start_y=0;
int fore_back_flag=FALSE;
char file_name[MAX_FILE_NAME];
struct pixrect *cut_buffer_pr=NULL,
		*undo_pr=NULL;
struct singlecolor fore_ground,back_ground;
int paint_brush=3,line_brush=2,rec_brush=1;

struct pixfont *main_font=NULL;
int box_off=0;
int run_box=FALSE;
int BW_mode=FALSE;

extern Notify_error notify_dispatch();
static Notify_value notice_destroy_event();


/*
 * this event proc will resize the canvas the size given in
 * width_text and height_text, in the EXPERT command panel
 */
resize_canvas()
{
int width, height;

  width = atoi((char*)panel_get_value(width_text));
  height = atoi((char*)panel_get_value(height_text));

  if ((width > 0) &&
     (height > 0) &&
     (width < 10000) &&
     (height < 10000))
  {
	image_wid = width;
	image_hgt = height;
        (void)window_set(canvas,
	  	CANVAS_WIDTH,		image_wid,
	    	CANVAS_HEIGHT,		image_hgt,
	    	0);
	if (undo_flag)
	  {
	     MY_pr_destroy(undo_pr);
	     undo_pr = my_mem_create(image_wid,image_hgt,image_depth);
	  }
  }
  else
 	ERROR("Bad canvas size, either too small or too large");
}


/*
 * Get_ROP: this function will get the check user specified bitmap OP
 * sot that we know how to paint the new objects on to the bitmap
 */
int get_current_ROP()
{
  switch((int)panel_get_value(ROP_cycle)) {
     case 0 : return(0);
     case 1 : return(PIX_XOR);
     case 2 : return(PIX_SRC & PIX_DST);
     case 3 : return(PIX_SRC | PIX_DST);
     case 4 : return(PIX_SRC);
  }
  return(0);
}


/*
 * When the brush size is changed this function will update the
 * correct brush storage, depending on what mode you were in
 */
change_brush()
{
  switch((int)panel_get_value(command_choice)) {
    case PAINT:
	paint_brush = (int)panel_get_value(brush_choice);
	break;
    case LINE:
	line_brush = (int)panel_get_value(brush_choice);
	break;
    case RECT_H:
	rec_brush = (int)panel_get_value(brush_choice);
	break;
  }
}



/*
 * this is the event handler for the special cut/paste command menu
 */
region_handle()
{
    switch((int)panel_get_value(region_choice)) {
	case CUT:
		cut_region();
			break;
	case COPY:
		print_msg("Region copied to Cut/Paste buffer. Hold down the RIGHT mouse button to drag the object.");
		panel_set(region_choice,PANEL_VALUE,MOVE,0);
		copy_region();
			break;
	case FLIP_HOR:
		flip_hor_region();
			break;
	case FLIP_VER:
		flip_ver_region();
			break;
	case ROTATE:
		rotate_region();
			break;
	case INVERSE:
		inverse_region();
			break;
	case PASTE:
                paste_region();
			break;
	case MOVE:
		print_msg("Hold down the RIGHT mouse button and drag the object.");
			break;
	case SCALE:
		scale_region();
                        break;
    }

}


/*
 * this is the event handler for the main comand menu
 */
command_handle(item, event)
Panel_item      item;
Event           *event;
{
  hide_msg();
  if ((int)panel_get_value(command_choice) != GET_PT)
	{
	  (void)window_set(region_panel, WIN_SHOW, FALSE, 0);
	  panel_set(region_choice,PANEL_VALUE,PASTE,0);
	}
  (void)window_set(brush_panel, WIN_SHOW, FALSE, 0);
  (void)window_set(text_panel, WIN_SHOW, FALSE, 0);
  finish_text();

    switch((int)panel_get_value(command_choice)) {
	case TEXT:
		  cur_text_str[0] = '\0';
		  print_msg("Press the LEFT mouse button to pick text position, then type.");
		  (void)window_set(text_panel, WIN_SHOW, TRUE, 0);

#ifdef CHANGE_CURSOR
		  change_cursor(canvas, text_cur);
#endif
		  mouse_parms();
			break;
	case LASO:
		  print_msg("Hold down the LEFT mouse button and encircle a object.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, laso_cur);
#endif
		  mouse_parms();
			break;
	case CIRCLE:
		  print_msg("Hold down the LEFT mouse button and extend to radius of the circle.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
		  mouse_parms();
			break;
	case DRAW:
		  print_msg("Press the LEFT mouse button to DRAW.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, draw_cur);
#endif
		  mouse_parms();
			break;
	case LINE:
		  print_msg("Hold down the LEFT mouse button and extend to end of the line.");
		  panel_set(brush_choice,PANEL_VALUE, line_brush,0);
		  (void)window_set(brush_panel, WIN_SHOW, TRUE, 0);
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
		  mouse_parms();
			break;
	case MAGNIFY:
		  print_msg("Use LEFT button to draw, hold down MIDDLE button to move.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
		  fat_mode();
			break;
	case FFILL:
		  fill_mode();
  		  panel_set(command_choice,PANEL_VALUE, GET_PT,0);
		  mouse_parms();
			break;
	case OVAL:
   		  print_msg("Hold down the LEFT mouse button and extend to edge of the oval.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
		  mouse_parms();
			break;
	case POLY_F:
	case POLY_H:
		  print_msg("Press the LEFT mouse button to select a vertex.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
		  mouse_parms();
			break;
	case RECT_H:
		  panel_set(brush_choice,PANEL_VALUE, rec_brush,0);
		  (void)window_set(brush_panel, WIN_SHOW, TRUE, 0);
	case RECT_F:
		  print_msg("Hold down the LEFT mouse button and extend to the opposite corner.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
		  mouse_parms();
			break;
	case PAINT:
		  print_msg("Press the LEFT mouse button to PAINT.");
		  mouse_parms();
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, paint_cur);
#endif
		  panel_set(brush_choice,PANEL_VALUE, paint_brush,0);
		  (void)window_set(brush_panel, WIN_SHOW, TRUE, 0);
			break;
	case SEL_REG:
		  print_msg("Hold down the LEFT mouse button and extend to the opposite corner.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
		  if (mouse_left == SEL_REG)
		    {
		      clean_region();
		      top_x = 0;
		      top_y = 0;
	    	      bottom_x = image_wid;
		      bottom_y = image_hgt;
		      move_box(CLEAR);
		      run_box = TRUE;
		      start_region();
		      print_msg("Entire drawing area is now selected.");
		    }
		  else
		    {
		      mouse_parms();
		    }
		  (void)window_set(region_panel, WIN_SHOW, TRUE, 0);
		  break;
	case GET_PT:
		  print_msg("Press the LEFT mouse button to select a point.");
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, sel_point_cur);
#endif
		  mouse_parms();
			break;
	case ERASE:
		  print_msg("Press the LEFT MOUSE button to ERASE.");
		  if ((mouse_left == ERASE) &&  (confirm("Erase entire drawing area ?")))
		    {
		    clear_screen();
		    }
		  else
                    if (top_x || top_y || bottom_x || bottom_y)
        	    {
		     move_box(CLEAR);
		     start_region();
  		     region_fix();
  	  	     pw_rop(pw,top_x,top_y,bottom_x-top_x,bottom_y-top_y,PIX_SRC,0,0,0);
		     move_box(CLEAR);
		     panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
		     (void)window_set(region_panel, WIN_SHOW, TRUE, 0);
		     return;
        	    }
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, erase_cur);
#endif
		  mouse_parms();
			break;

    }

}     


clear_text_cursor()
{
struct pr_size text_wid;

  if (cur_text_str[0]) 
   {	 
    text_wid = pf_textwidth(strlen(cur_text_str),
	real_font,cur_text_str);
    pw_replrop(pw,cur_text_x+ text_wid.x ,cur_text_y- TEXT_HGT,
	1,TEXT_HGT, PIX_XOR, pattern[0],0,0);
   }
  else if (text_cursor) 
   {	 
    pw_replrop(pw,cur_text_x,cur_text_y- TEXT_HGT,
	1,TEXT_HGT, PIX_XOR, pattern[0],0,0);
   }
}


/*
 * save the screen to a temp bitmap for the undo command
 */
save_screen()
{
struct pr_size text_wid;

  if (undo_flag)
  {
    if (undo_pr == NULL)
  	undo_pr =my_mem_create(image_wid,image_hgt,image_depth);

    clear_text_cursor();
    pr_rop(undo_pr,0,0,image_wid,image_hgt,PIX_SRC,pw->pw_prretained,0,0);
    clear_text_cursor();
  }
}     


/*
 * go back to the last saved bitmap
 */
undo_screen()
{
  if (undo_flag)
  {
    clean_region();
    clean_point();
    clean_poly();
    finish_text();
    fat_done();
    pw_batch_on(pw);
    pr_swap(pw,undo_pr);
    pw_batch_off(pw);
  }
}     


/*
 * this function was written by rainbow@sbcs.sunysb.edu (Hong Min)
 *
 * this function will swap the pixrect "pr2" with the pixwin "pw" by
 * coping 25 lines at a time. This is needed so that full size color
 * pixwins (1 meg of memory) can be exchanged with out the need of
 * a third buffer
 */
pr_swap(pw, pr2)
	Pixwin		 *pw;
	struct pixrect	 *pr2;
{
	struct pixrect		*buf;
	int 	i, start, loop, rem, width, height, buf_size;

	width = pr2->pr_size.x;
	height = pr2->pr_size.y;
	buf_size = 25;

	buf = my_mem_create(width, buf_size, image_depth);
	loop = height / buf_size;
	rem = height % buf_size;
	
	for(i=0; i<loop; i++)
	{
	   pr_rop(buf, 0, 0, width, buf_size, PIX_SRC, pw->pw_prretained, 0, i*buf_size);
	   pw_rop(pw, 0, i*buf_size, width, buf_size, PIX_SRC, pr2, 0, i*buf_size);
	   pr_rop(pr2, 0, i*buf_size, width, buf_size, PIX_SRC, buf, 0, 0);
	}

	if (rem != 0)
	{
	   start = (loop-1)*buf_size;
	   pr_rop(buf, 0, 0, width, rem, PIX_SRC, pw->pw_prretained, 0, start);
	   pw_rop(pw, 0, start, width, rem, PIX_SRC, pr2, 0, start);
	   pr_rop(pr2, 0, start, width, rem, PIX_SRC, buf, 0, 0);
	}
	MY_pr_destroy(buf);
}


/*
 * clear the drawing area
 */
clear_screen()
{
  clean_point();
  clean_region();
  finish_text();
  fat_done();
  save_screen();
  pw_write( pw,0,0,1280,1280,PIX_SRC,0,0,0);
  pw_write( fat_pw,0,0,1280,1280,PIX_SRC,0,0,0);
}


/*
 * if there is a region that is select the deselect it
 */
clean_region()
{
  if (top_x || top_y || bottom_x || bottom_y)
    {
     move_box(CLEAR);
     run_box=FALSE;
     start_region();
     reset_region();
    }
}


/*
 * if there is a point that is selected then deselecte it
 */
clean_point()
{
  if (select_pt_x != -1)
    {
     select_point(select_pt_x,select_pt_y);
     select_pt_x = -1;
     select_pt_y = -1;
    }
}


/*
 * deselect any points and regions and get the current command mode
 */
mouse_parms()
{
  clean_point();
  clean_region();
  fat_done();
  mouse_left = (int)panel_get_value(command_choice);
}


/*
 * set it to select a point mode
 */
set_select_mode()
{
  panel_set(command_choice,PANEL_VALUE, GET_PT,0);
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, sel_point_cur);
#endif  
  mouse_parms();
}



int first_time=TRUE;

/*
 * this the main event handler that makes the whole thing go
 * this event handle is for that canvas that is the main drawing area
 */
handle_event(canvas_local, event)
Canvas  canvas_local;
Event   *event;
{
    Pixwin      *pw     = canvas_pixwin(canvas_local);
static int ROP;


    if (grid_size)
     {
	event_set_x(event,event_x(event) + grid_size/2 - (event_x(event)%grid_size));
        event_set_y(event,event_y(event) + grid_size/2 - (event_y(event)%grid_size));
     }

    if (event_is_ascii(event))
	{
	  if ((mouse_left != TEXT) || (!text_cursor))
		return;
	  else
	    {
	  	new_draw_text(event);
		return;
	    }
	}


    if (event_is_up(event))
	{
   	 if (event_id(event) == MS_LEFT)
		{
		switch(mouse_left) {
		case RECT_H :
		case RECT_F :
		   draw_rectangle(start_x,start_y,event_x(event), event_y(event));
		   break;

		case LINE :
		   if (!(ROP = get_current_ROP()))
			ROP = PIX_SRC;
		   draw_line(start_x,start_y,event_x(event), event_y(event),PIX_XOR,1);
		   draw_thick_line(start_x,start_y,event_x(event),
			   event_y(event),ROP,cur_color,
			   brush_radius[(int)panel_get_value(brush_choice)]);
		   break;

		case ERASE :
	               select_region(pw,old_x,old_y,old_x+CURSOR_SIZE,old_y+CURSOR_SIZE);
		   break;

		case CIRCLE :
		   if (!(ROP = get_current_ROP()))
			ROP = PIX_SRC;
 		   draw_line(start_x,start_y,old_x,old_y,PIX_XOR,1);
		   draw_circle(start_x,start_y,distance(old_x,old_y,start_x,start_y),ROP);
		   break;

		case OVAL :
		   if (!(ROP = get_current_ROP()))
			ROP = PIX_SRC;
		   if (ROP == (PIX_XOR))
		        draw_oval(pw,start_x,start_y,old_x,old_y,ROP,FALSE);
		   draw_oval(pw,start_x,start_y,old_x,old_y,ROP,TRUE);
		   break;

		case GET_PT :
  		   select_pt_x = event_x(event);
		   select_pt_y = event_y(event);
		   break;

		case LASO :
		   laso_cut_paste();
		   panel_set(region_choice,PANEL_VALUE,MOVE,0);
		   panel_set(command_choice,PANEL_VALUE,SEL_REG,0);
#ifdef CHANGE_CURSOR
		  change_cursor(canvas, old_cur);
#endif
	           (void)window_set(brush_panel, WIN_SHOW,FALSE, 0);
	           (void)window_set(region_panel, WIN_SHOW,TRUE, 0);
		   print_msg("Object copied to Cut/Paste buffer. Hold down the RIGHT mouse button to drag the object.");
		   mouse_parms();
		   break;

		case TEXT :
		   finish_text();
		   text_cursor = TRUE;
		   org_text_x = cur_text_x = event_x(event);
		   cur_text_y = event_y(event);
		   cur_text_str[0] = '\0';
		   pw_replrop(pw,cur_text_x,cur_text_y-TEXT_HGT,1,TEXT_HGT, PIX_XOR, pattern[0],0,0);
		   break;

		}
                if (fat_source_x != -1)
                   fat_update(0,0);
	      }
   	 if (event_id(event) == MS_RIGHT)
		{
                  if (((int)panel_get_value(region_choice) == MOVE) && (cut_buffer_pr != NULL))
			{
		         if (!(ROP = get_current_ROP()))
				ROP = PIX_SRC | PIX_DST;
		         pw_write(pw,old_x- cut_buffer_pr->pr_size.x/2,
				     old_y- cut_buffer_pr->pr_size.y/2,
				     cut_buffer_pr->pr_size.x,
				     cut_buffer_pr->pr_size.y,
				     PIX_XOR, cut_buffer_pr,0,0);
		       pw_write(pw,event_x(event)-cut_buffer_pr->pr_size.x/2,
				   event_y(event)-cut_buffer_pr->pr_size.y/2,
				 cut_buffer_pr->pr_size.x,
				 cut_buffer_pr->pr_size.y,
				 ROP, cut_buffer_pr,0,0);
			}
		}
         return;
	}
    switch (event_id(event)) {
	 case MS_LEFT:

  		first_time = FALSE;

		if ((mouse_left != POLY_H) && (mouse_left != POLY_F)
		     && (mouse_left != GET_PT) && (mouse_left != SEL_REG))
			save_screen();
		old_x = event_x(event);
		old_y = event_y(event);
		start_x = old_x;
		start_y = old_y;
		switch(mouse_left) {

		case LINE:
			   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
			   break;
		case POLY_F:
		case POLY_H:
			   if (poly_points[0].x == -1)
				save_screen();			
			   poly_addpt(poly_points,start_x,start_y);
			   print_msg("Press the LEFT button to select a vertex or press the RIGHT mouse button to end the polygon.");
			   break;
		case DRAW:
			   draw_point(pw,event_x(event), event_y(event));
			   if (!(ROP = get_current_ROP()))
			       ROP = PIX_SRC;			   
			   break;
		case PAINT:
			   draw_brush(pw, event_x(event), event_y(event));
			   break;
		case ERASE:
			   old_x = event_x(event)-CURSOR_SIZE/2; old_y= event_y(event)-CURSOR_SIZE/2;
			   select_region(pw,old_x,old_y,old_x+CURSOR_SIZE,old_y+CURSOR_SIZE);
			   erase_brush(pw, event_x(event), event_y(event));
			   break;
		case CIRCLE:
 		           draw_line(start_x,start_y,old_x,old_y,PIX_XOR,1);
			   break;
		case OVAL:
		           draw_oval(pw,start_x,start_y,old_x,old_y,PIX_XOR,FALSE);
			   break;
		case GET_PT:
  		   	   if (select_pt_x != -1)
  			      select_point(select_pt_x,select_pt_y);
    		   	   old_x = event_x(event);
  		           old_y = event_y(event);
  		           select_point(old_x,old_y);
			   break;

		case SEL_REG:
		  	   if (top_x || top_y || bottom_x || bottom_y)
				move_box(CLEAR);
			   save_screen();
  	          	   top_x = event_x(event);
		 	   top_y = event_y(event);
  	          	   bottom_x = event_x(event);
		 	   bottom_y = event_y(event);
			   pw_batch_on(pw);
			   move_box(CLEAR);
			   pw_batch_off(pw);
			   run_box = TRUE;
	 	 	   start_region();
			   break;
		case LASO:
			   ptlist[0].x = -1;
			   ptlist[0].y = -1;
			   laso_addpt(ptlist,start_x,start_y);
			   break;
		}

		break;

          case MS_MIDDLE:

		if (event_shift_is_down(event) != FALSE)
		    {
			undo_screen();
			break;
		    }
		switch(mouse_left) {
		case SEL_REG:
			if (top_x || top_y || bottom_x || bottom_y)
			{
			  pw_batch_on(pw);
			  move_box(CLEAR);
  	        	  bottom_x = event_x(event);
			  bottom_y = event_y(event);
			  move_box(CLEAR);
			  pw_batch_off(pw);
			}
			break;
		}	  
		break;
          case MS_RIGHT:

		if (((mouse_left==POLY_F) || (mouse_left ==POLY_H))
		   && (poly_points[0].x != -1))
		 {
			   poly_addpt(poly_points,event_x(event),event_y(event));
			   draw_poly(poly_points);
		 }
                if (((int)panel_get_value(region_choice) == MOVE) && (cut_buffer_pr != NULL))
			{
			clean_region();
			clean_point();
			save_screen();
			old_x = event_x(event);
			old_y = event_y(event);
			start_x = old_x;
			start_y = old_y;
		        pw_write(pw,old_x - cut_buffer_pr->pr_size.x/2,
				    old_y - cut_buffer_pr->pr_size.y/2,
				    cut_buffer_pr->pr_size.x,
				    cut_buffer_pr->pr_size.y,
				    PIX_XOR, cut_buffer_pr,0,0);
			}
		break;
          case LOC_DRAG:
            if (window_get(canvas_local, WIN_EVENT_STATE, MS_LEFT))
		{
		if (first_time)
		{
		  first_time = FALSE;
		  old_x = event_x(event);
		  old_y = event_y(event);
		  return;
		}
		switch(mouse_left) {
		case PAINT:
			draw_con_brush(pw, event_x(event), event_y(event),old_x,old_y);
			old_x = event_x(event);
			old_y = event_y(event);
			break;
		case DRAW:
			pw_vector(pw,old_x,old_y,event_x(event), event_y(event),ROP,cur_color);
			old_x = event_x(event);
			old_y = event_y(event);
			break;
		case ERASE :
			erase_brush(pw, event_x(event), event_y(event));
			break;
		case LINE:
			   pw_batch_on(pw);
			   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
  			   old_x = event_x(event);
			   old_y = event_y(event);
			   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
			   pw_batch_off(pw);
			break;
		case RECT_F :
		case RECT_H :
			   pw_batch_on(pw);
			   select_region(pw,old_x,old_y,start_x,start_y);
	  		   old_x = event_x(event);
			   old_y = event_y(event);
			   select_region(pw,old_x,old_y,start_x,start_y);
			   pw_batch_off(pw);
			break;
		case CIRCLE:
			   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
	  		   old_x = event_x(event);
			   old_y = event_y(event);
			   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
			break;
		case OVAL:
			   pw_batch_on(pw);
			   draw_oval(pw,start_x,start_y,old_x,old_y,PIX_XOR,FALSE);
	  		   old_x = event_x(event);
			   old_y = event_y(event);
			   draw_oval(pw,start_x,start_y,old_x,old_y,PIX_XOR,FALSE);
			   pw_batch_off(pw);
			break;
		case GET_PT:
			   select_point(old_x,old_y);
  		   	   select_pt_x = event_x(event);
		   	   select_pt_y = event_y(event);
	  		   old_x = event_x(event);
			   old_y = event_y(event);
			   select_point(old_x,old_y);
			break;
		case SEL_REG:
			  pw_batch_on(pw);
			  move_box(CLEAR);
  	        	  bottom_x = event_x(event);
			  bottom_y = event_y(event);
			  move_box(CLEAR);
			  pw_batch_off(pw);
			break;
		case LASO:
			   laso_addpt(ptlist,event_x(event),event_y(event));
			}
		case POLY_F:
		case POLY_H:
			if (poly_points[0].x != -1)
		 	{
			   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
	  		   old_x = event_x(event);
			   old_y = event_y(event);
			   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
			}
		  }
            if (window_get(canvas_local, WIN_EVENT_STATE, MS_RIGHT))
		{
                if ((int)panel_get_value(region_choice) == MOVE)
			{
			  move_region(old_x,old_y,event_x(event),event_y(event));
	  		  old_x = event_x(event);
			  old_y = event_y(event);
			}
		}
	    break;
          case LOC_MOVE:
		if (((mouse_left == POLY_F) || (mouse_left == POLY_H)) &&
			 (poly_points[0].x != -1))
		 {
		   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
  		   old_x = event_x(event);
		   old_y = event_y(event);
		   draw_line(old_x,old_y,start_x,start_y,PIX_XOR,1);
		 }
	    break;
	}
}



/*
 * we got a "quit" button, say bye bye
 */
quit()
{
  window_done(base_frame);
}


/*
 * get all the current values for some stuff
 */
change_parms()
{
  if (image_depth == 1)
     cur_color = (int)panel_get_value(mono_cycle);
  magnify_fac = (int)panel_get_value(magnify_cycle) + 1;
  grid_size = (int)panel_get_value(grid_cycle)*5;
}



/*
 * this is the main that start the show and then goes into
 * window main loop
 */
main(argc,argv) int argc; char *argv[];
{
int targc;
char **targv;
char *s;
char temp_str[10];
extern colormap_t colormap;

/*
 * get the options
 */
        targc = argc; targv = argv;
        while (--argc > 0 && (*++argv)[0] == '-')
                for (s = argv[0]+1;*s != '\0';s++)
          switch (*s) {
                  case 'n':
                            undo_flag = FALSE;
                            break;
                  case 'f':
                            fore_back_flag = TRUE;
                            break;
                  case 'b':
                            BW_mode = TRUE;
                            break;
                  case 'p':
                            image_hgt = DEFAULT_IMAGE_WID;
                            image_wid = DEFAULT_IMAGE_HGT;
                            break;
                  case 'y':
                                ++argv; argc--;
                            image_hgt = atoi(argv[0]);
                            break;
                  case 'x':
                                ++argv; argc--;
                            image_wid = atoi(argv[0]);
                            break;
                  default:
                            break;
	  }
/*
 * leftovers are potential window option arguments
 */

  clean_poly();

  /*
   * get the font used in all of the panels
   */
  main_font = pf_open(MAIN_FONT);
  if (!main_font)
    {
     printf("ERROR loading the main font !!!!\n");
     exit(1);
    }

  init_font();
  getcwd(file_name,MAX_FILE_NAME-2);
  strcat(file_name,"/");
  init_windows(&targc,targv);
#ifdef CHANGE_CURSOR
  init_cursors();
#endif
  

/*
 * some sanity check for option arguments
 */
        while (--targc > 0 && (*++targv)[0] == '-')
                for (s = targv[0]+1;*s != '\0';s++)
          switch (*s) {
                  case 'n':
                  case 'f':
                  case 'b':
                  case 'p':
                            break;
                  case 'y':
                  case 'x':
                                ++targv; targc--;
                            break;
                  default:
                        printf("Usage: touchup [-x width] [-y height] [-n] [-p] [-f] [-b]\n");
                                exit(0);
                            break;
	  }
  if (targc > 0)
    {
      printf("Usage: touchup [-x width] [-y height] [-n] [-p] [-f] [-b]\n");
      exit(0);
    }
			    
  sprintf(temp_str,"%d",image_hgt);
  panel_set(height_text,PANEL_VALUE,temp_str,0);
  sprintf(temp_str,"%d",image_wid);
  panel_set(width_text,PANEL_VALUE,temp_str,0);
  /*
   * are we on a color machine ????
   */
  image_depth = pw->pw_pixrect->pr_depth;
  if (BW_mode)
	image_depth = 1;

  if (image_depth > 1)
    {
	init_colortable();

	fore_ground = *((struct singlecolor *) window_get(base_frame,
					FRAME_BACKGROUND_COLOR));
	back_ground = *((struct singlecolor *) window_get(base_frame,
					FRAME_BACKGROUND_COLOR));
        red[0] = fore_ground.red;
        green[0] = fore_ground.green;
        blue[0] = fore_ground.blue;  
        red[255] = back_ground.red;
        green[255] = back_ground.green;
        blue[255] = back_ground.blue;

	my_put_colormap();
	set_color();
	colormap.map[0] = red;
	colormap.map[1] = green;
	colormap.map[2] = blue;
	colormap.type = RMT_EQUAL_RGB;
	colormap.length = 256;
    }
  else
	set_mono();

  window_set(canvas, CANVAS_RETAINED, TRUE, 0);

  if (undo_flag)
     undo_pr = my_mem_create(image_wid,image_hgt,image_depth);
  else
     panel_set(undo_button,PANEL_SHOW_ITEM, FALSE,0);

  brush_temp_pr = my_mem_create(PATTERN_SIZE,PATTERN_SIZE,1);
  init_mag();
#ifdef CHANGE_CURSOR
  change_cursor(canvas, paint_cur);
#endif
  print_msg("Press the LEFT mouse button to PAINT.");

          
 /*
  * set up an interposed event handler so we know when to quit
  *	-mark weiser
  */
  (void)notify_interpose_destroy_func(base_frame, notice_destroy_event);

  window_main_loop(base_frame);
}



/***************************************************************
        hide_msg
        purpose: To clear the masssage display area.
        parameter:
        returns:
 ***************************************************************/
hide_msg()
{
         panel_set(msg_string,PANEL_LABEL_STRING,"",0);
}


/***************************************************************
        print_msg
        purpose: To print a message on the window and center it
        parameter:
                string: The string to be printed.
        returns:
 ***************************************************************/
print_msg(string)
char *string;
{
char temp_space[132];
char *temp_pt;
int i;
  
  if (strlen(string) < 132)
  {
    for(i=0;i<132;i++)
      temp_space[i]= ' ';
    temp_pt = temp_space + (132-strlen(string))/2;
    strcpy(temp_pt,string);
    panel_set(msg_string,PANEL_LABEL_STRING,temp_space,0);
  }
  else
    panel_set(msg_string,PANEL_LABEL_STRING,string,0);
}

ERROR(msg)
char *msg;
{ 
  print_msg(msg);
  window_bell(panel);
}

ERRORstr(msg,str)
char *msg,*str;
{
char temp[200];
  strcpy(temp,msg);
  print_msg(strcat(temp,str));
  window_bell(panel);
}



/***************************************************************
	sqrt_fast
        purpose: To do a fast integer square root
        parameter:
		n :  the int to take the sqrt of
        returns: 
		the integer square root of n
 ***************************************************************/
int sqrt_fast(n)
int n;
{
   int a,b,c;
   a = n;
   b = n;
   if (n>1){
        while (a>0) {
            a = a >> 2;
            b = b >> 1;
	}
        do {
            a = b;
            c = n / b;
            b = (c + a) >> 1;
	} while ( (a-c)<-1 || (a-c)>1 );
   }
    return(b);
}

/*
 * find the distance between any two points
 */
distance(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
  return(sqrt_fast((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)));
}


/*
 * check if we have enough memory to create those LARGE bitmaps
 */
struct pixrect *my_mem_create(wid,hgt,dep)
int wid,hgt,dep;
{
struct pixrect* temp_pr;

    temp_pr = mem_create(wid,hgt,dep);
    if (temp_pr== NULL)
        {
  	printf("Not enough memory, memory allocation problems!!!\n");
  	exit(0);
        }
    return(temp_pr);  /* this is the line that was missing in version 2.2 */
}




/*
 * pixrect data for horizontal dashed lines for 'moving box'
 */
short box_x_bits[] = {
        0xff00, 0xff00
};

/*
 * pixrect data for vertical dashed lines for 'moving box'
 */
short box_y_bits[] = {
        0xffff, 0xffff,
        0xffff, 0xffff,
        0xffff, 0xffff,
        0xffff, 0xffff,
        0x0000, 0x0000,
        0x0000, 0x0000,
        0x0000, 0x0000,
	0x0000, 0x0000
};
static mpr_static(bufx, 16, 1, 1, box_x_bits);
static mpr_static(bufy, 16, 16, 1, box_y_bits);


#define ITIMER_NULL ((struct itimerval *)0)
int box_running=FALSE;

/*
 * this is the function that is will get called when the sigalarm
 * goes off
 */
Notify_value move_boxz()
{
  pw_batch_on(pw);
  move_box(NO_CLEAR);
  pw_batch_off(pw);
  return(NOTIFY_DONE);
}

int my_client_object;
int *me = &my_client_object;



/*
 * call this function to get the box moving, once this function
 * has been called the box will continue to move
 *
 *    run_box:TRUE  -- this means that we want to move the box
 *    box_running:TRUE  -- this means the the box is already running
 */
start_region()
{
  if (run_box && !box_running)
    {
        struct itimerval timer;

        timer.it_interval.tv_usec = 1000;
        timer.it_interval.tv_sec = 0;
        timer.it_value.tv_usec = 1000;
        timer.it_value.tv_sec = 0;
        notify_set_itimer_func(me, move_boxz,
                ITIMER_REAL, &timer, ITIMER_NULL);
	box_running = TRUE;
    }
  else if (!run_box && box_running)
    {
	(void)notify_set_itimer_func(me,move_boxz,
		ITIMER_REAL,ITIMER_NULL, ITIMER_NULL);
	box_running = FALSE;
    }
}


/*
 * this function is called to move all of the dashed lines
 * 1 increment. if 'clear' is true that it this function will
 * just erase the box from the last time but it will not
 * draw up the new box
 */
move_box(clear)
int clear;
{
int ztop_x,ztop_y;
int zbottom_x,zbottom_y;

  if (top_x > bottom_x)
    {
     ztop_x = bottom_x;
     zbottom_x = top_x;
    }
  else
    {
     ztop_x = top_x;
     zbottom_x = bottom_x;
    }

  if (top_y > bottom_y)
    {
     ztop_y = bottom_y;
     zbottom_y = top_y;
    }
  else
    {
     ztop_y = top_y;
     zbottom_y = bottom_y;
    }

  pw_replrop(pw, ztop_x, ztop_y, zbottom_x-ztop_x, 1,
                        PIX_XOR, &bufx, box_off, 0);
  pw_replrop(pw, ztop_x, zbottom_y, zbottom_x-ztop_x, 1,
                        PIX_XOR, &bufx, box_off+(zbottom_y-ztop_y), 0);
  pw_replrop(pw, ztop_x, ztop_y, 1, zbottom_y-ztop_y,
                        PIX_XOR, &bufy, 0, box_off); 
  pw_replrop(pw, zbottom_x, ztop_y, 1, zbottom_y-ztop_y,
                        PIX_XOR, &bufy, 0, box_off+(zbottom_x-ztop_x));
  if (!clear)
     {
	  box_off++;
          pw_replrop(pw, ztop_x, ztop_y, zbottom_x-ztop_x, 1,
                        PIX_XOR, &bufx, ++box_off, 0);
          pw_replrop(pw, ztop_x, zbottom_y, zbottom_x-ztop_x, 1,
                        PIX_XOR, &bufx, box_off+(zbottom_y-ztop_y), 0);
          pw_replrop(pw, ztop_x, ztop_y, 1, zbottom_y-ztop_y,
                        PIX_XOR, &bufy, 0, box_off);
          pw_replrop(pw, zbottom_x, ztop_y, 1, zbottom_y-ztop_y,
                        PIX_XOR, &bufy, 0, box_off+(zbottom_x-ztop_x));
     }
}



clean_box()
{
  if (run_box)
	move_box(CLEAR);
  run_box = FALSE;
  start_region();
}



/* 
 * a service routine that gets called when the frame is destroyed
 * by someone selecting 'quit'.  this is basically right out of the 
 * manual.	-mark weiser
 */
static Notify_value
notice_destroy_event(frame, status)
        Frame *frame;
        Destroy_status status;
{
        if (status != DESTROY_CHECKING) {
                run_box = FALSE;
		start_region();
	}
        return (notify_next_destroy_func(frame,status));
}


