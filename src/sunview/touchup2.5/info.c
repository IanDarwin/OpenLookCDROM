
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
	file: info.c
	purpose: this file has the functions that had the "view" pop up
		window.  You can view the "man page", copyright notice
		with the picture of the author (me), and the cut/paste
   		buffer

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Wed Jun 15 18:12:16 EDT 1988
		author:	rayk
		changes:changed error messages for "info" and "help"
			suggested by ian@sq.com

**************************************************************************/

#include "header.h"

#include <suntool/textsw.h>

Frame view_frame;
Canvas view_canvas;
Pixwin *view_pw;
Panel view_panel;
int view_flag=FALSE;

#define VIEW_INFO_FLAG 2
#define VIEW_HELP_FLAG 3

/*
 * this function will read in the copyright notice
 * with my picture on it and display it in a new window
 */
info_init()
{
FILE *fp,*fopen();
int color_map=NULL;
struct pixrect *temp_pr;

   if (view_flag == VIEW_INFO_FLAG)
   {
      (void)window_set(view_frame, WIN_SHOW, TRUE, 0);
      return;
   }

   fp = fopen(INFO_IMAGE,"r");
   if (!fp)
   {
      ERRORstr("I Could not find the information file : ",INFO_IMAGE);
      view_flag = FALSE;
      fclose(fp);
      return;
   }
   temp_pr = (struct pixrect *)pr_load(fp,color_map);
   if (!temp_pr)
   {
      ERRORstr("I could not load the information file : ",INFO_IMAGE);
      view_flag = FALSE;
      fclose(fp);
      return;
   }
   view_init(temp_pr,"Information on Touchup");
   view_flag = VIEW_INFO_FLAG;
   MY_pr_destroy(temp_pr);
   fclose(fp);
}


/*
 * this function will let you view the cut/paste buffer
 * in a seperate window
 */
view_cut_paste()
{
   if (cut_buffer_pr)
   {
     view_init(cut_buffer_pr,"The current Cut/Paste Buffer");
   }
   else
   {
     ERROR("The CUT/PASTE buffer is empty.");
   }
}


/*
 * we got a button click on "view"
 * check out what we need to view
 */
#define VIEW_INFO 0
#define VIEW_HELP 1
#define VIEW_CUT_PASTE 2
viewer()
{
   switch((int)panel_get_value(view_cycle)) {
	case VIEW_INFO: info_init();
	  	   break;
	case VIEW_CUT_PASTE: view_cut_paste();
	  		break;
	case VIEW_HELP: view_help();
   }
}


/*
 * close up the view window
 */
view_done()
{
   if (view_flag)
   {
     window_set(view_frame, FRAME_NO_CONFIRM, TRUE, 0);
     window_destroy(view_frame);
     view_flag = FALSE;  
   }
}


/*
 * this function will take a pixrect and create a new window
 * which we can view it in
 */
view_init(view_pr,frame_label)
struct pixrect *view_pr;
char *frame_label;
{
Rect *r;
Pixwin *temp_pw;

   if (view_flag)
   {
      view_done();
   }
   view_flag = TRUE;

    r = (Rect *) window_get(base_frame, WIN_RECT);
    view_frame = window_create(base_frame,FRAME,
		FRAME_LABEL,    frame_label,
                FRAME_INHERIT_COLORS,   TRUE,
	        FRAME_SHOW_LABEL, TRUE,
		WIN_FONT,	main_font,
	        WIN_X,		(r->r_width - view_pr->pr_size.x+20)/2,
	        WIN_Y,		(r->r_height - view_pr->pr_size.y+50)/4,
		0);

    if (view_frame == NULL)
      {
	ERROR("Cannot open any more windows !");
	view_done();
        return;
      }

    view_panel = window_create(view_frame, PANEL,
             PANEL_LABEL_BOLD,           TRUE,
                WIN_FONT,       main_font,
                0);
   
    if (view_panel == NULL)
      {
	ERROR("Cannot open any more windows !");
	view_done();
        return;
      }

   (void)panel_create_item(view_panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_IMAGE, panel_button_image(view_panel, "done", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, view_done,
     0);
    window_fit(view_panel);

    view_canvas = 
        window_create(view_frame, CANVAS, 
            WIN_CONSUME_PICK_EVENTS,    WIN_NO_EVENTS,WIN_MOUSE_BUTTONS,
                                        LOC_DRAG, LOC_WINENTER,
                                        LOC_MOVE, 
                                        0,
                WIN_X,          0,
                WIN_BELOW,      view_panel,
            WIN_WIDTH,          view_pr->pr_size.x,
            WIN_HEIGHT,         view_pr->pr_size.y,
            CANVAS_WIDTH,               view_pr->pr_size.x,
            CANVAS_HEIGHT,              view_pr->pr_size.y,
            CANVAS_AUTO_SHRINK,         FALSE,
            CANVAS_FIXED_IMAGE,         TRUE,
            CANVAS_AUTO_EXPAND,         FALSE,
            CANVAS_RETAINED,            TRUE,
            0),
 

    view_pw = canvas_pixwin(view_canvas);


    if ((view_pw->pw_pixrect->pr_depth > 1) && (!BW_mode))
    {
	pw_setcmsname(view_pw, "ray kreisel");
	pw_putcolormap(view_pw, 0,256,red,green,blue);

  	temp_pw = (Pixwin *)window_get(view_frame, WIN_PIXWIN);
  	pw_setcmsname(temp_pw, "ray kreisel");
  	pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);

  	temp_pw = (Pixwin *)window_get(view_panel, WIN_PIXWIN);
  	pw_setcmsname(temp_pw, "ray kreisel");
  	pw_putcolormap(temp_pw, 0,256,temp_red,temp_green,temp_blue);
    }


  /* write the image files to the canvas */

   pw_write(view_pw,0,0,view_pr->pr_size.x,
		 view_pr->pr_size.y,PIX_SRC, view_pr,0,0);

   window_fit(view_frame);
   window_set(view_frame, WIN_SHOW, TRUE, 0);
}   


/*
 * this function is used to view the man page
 */
view_help()
{
Rect *r;

   if (!file_exist(HELP_FILE))
   {
      ERRORstr("I cannot find the man page for touchup : ",HELP_FILE);
      return;
   }

   if (view_flag == VIEW_HELP_FLAG)
   {
      (void)window_set(view_frame, WIN_SHOW, TRUE, 0);
      return;
   }
   else
   {
      if (view_flag)
 	 view_done();
   }
   view_flag = VIEW_HELP_FLAG;

    r = (Rect *) window_get(base_frame, WIN_RECT);
    view_frame = window_create(base_frame,FRAME,
		FRAME_LABEL,    "Help window",
	        FRAME_SHOW_LABEL, TRUE,
		WIN_FONT,	main_font,
	        WIN_X,		(r->r_width - 600+20)/2,
	        WIN_Y,		(r->r_height - 512+50)/4,
		0);

    if (view_frame == NULL)
      {
	ERROR("Cannot open any more windows !");
	view_done();
        return;
      }

    view_panel = window_create(view_frame, PANEL,
	     PANEL_LABEL_BOLD,    	 TRUE,
	     WIN_FONT,			 main_font,
	     WIN_X,			 0,
	     WIN_Y,			 3,
	     WIN_HEIGHT,		 ATTR_ROW(1)+4,
		0);
   
    if (view_panel == NULL)
      {
	ERROR("Cannot open any more windows !");
	view_done();
        return;
      }

   (void)panel_create_item(view_panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_IMAGE, panel_button_image(view_panel, "done", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, view_done,
     0);


    (void)window_create(view_frame, TEXTSW,
		WIN_ERROR_MSG, 		"I cannot find the man page for touchup",
                WIN_X,                  0,
		WIN_Y,			ATTR_ROW(2)+5,
                WIN_HEIGHT,             512,
                WIN_WIDTH,              620,
		TEXTSW_FONT,		main_font,
                TEXTSW_IGNORE_LIMIT,    TEXTSW_INFINITY,
                TEXTSW_AUTO_INDENT,     TRUE,
                TEXTSW_BROWSING,        TRUE,
                TEXTSW_DISABLE_LOAD,    TRUE,
                TEXTSW_DISABLE_CD,      TRUE,
        	TEXTSW_FILE,    	HELP_FILE,
                0);

  window_fit(view_frame);
  window_set(view_frame, WIN_SHOW, TRUE, 0);
}   


