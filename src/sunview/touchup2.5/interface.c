
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
	file: interface.c
	purpose: this file contains all of the window defs for touchup

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Fri Apr 15 01:18:05 EDT 1988
		author:	rayk
		changes:added scroll by line to drawing area
**************************************************************************/

#include"header.h"
extern int change_font();
extern int change_pt_size();
extern void yes_no();
Cursor fat_cursor;

extern Panel_item       red_slide, green_slide, blue_slide;
extern Panel_item       col_file;
extern Panel_item       cur_col_msg;
extern int col_change();
extern int col_load(), col_save(), make_new_col_name();

static short icon_data[] = {
#include "touchup.icon"
};
static mpr_static(touchup_pr, 64, 64, 1, icon_data);


    Frame base_frame,fat_frame,color_frame;
    Frame confirmer;
    Canvas canvas,fat_canvas,color_canvas;
    Pixwin *pw,*fat_pw,*color_pw;
    Panel       panel,fat_panel,color_panel,pattern_panel,
		brush_panel,region_panel,command_panel, text_panel;
    Panel_item magnify_cycle,grid_cycle,view_cycle,ROP_cycle;
    Panel_item  file_panel,brush_choice,mono_cycle,save_cycle,border_cycle,
		msg_string,color_button,current_pattern,
		load_cycle,pattern_choice,command_choice,region_choice,
		compress_cycle,text_size_item,undo_button,width_text,
		height_text,resize_button,yes_button,no_button,
		lasso_remove, text_choice;
   Panel	con_panel;
   Panel_item con_msg_string, font_button;

   struct pixrect *pattern[PATTERN_NO];
   struct pixrect *brushes[BRUSH_NO];
  



/*
 * Build all of the window that we need for touchup
 */
init_windows(argcp,argvp) int *argcp; char **argvp;
{

    base_frame = window_create(NULL,FRAME,
		FRAME_LABEL,    "TouchUp version 2.5",
		FRAME_ICON,	    icon_create(ICON_IMAGE, &touchup_pr, 0),
		FRAME_ARGC_PTR_ARGV,	argcp,argvp,
		FRAME_INHERIT_COLORS,  TRUE,
		WIN_FONT,	main_font,
		WIN_WIDTH,	1030,
		WIN_HEIGHT,	670+ATTR_ROW(10),
		WIN_X,		10,
		WIN_Y,		10,
		0);

    panel = window_create(base_frame, PANEL,
	     PANEL_LABEL_BOLD,    	 TRUE,
	     WIN_FONT,		 	 main_font,
	     WIN_X,			 0,
	     WIN_Y,			 3,
	     WIN_HEIGHT,		 ATTR_ROW(5),
 	     PANEL_HEIGHT,		 ATTR_ROW(11),
		0);

  if (BW_mode)

    canvas = 
        window_create(base_frame, CANVAS, 
	    WIN_VERTICAL_SCROLLBAR,	scrollbar_create(SCROLL_LINE_HEIGHT,20,0),
	    WIN_HORIZONTAL_SCROLLBAR,	scrollbar_create(SCROLL_LINE_HEIGHT,20,0),
	    WIN_CONSUME_PICK_EVENTS,	WIN_NO_EVENTS,WIN_MOUSE_BUTTONS,
					LOC_DRAG,
					LOC_MOVE,
					0,
	    WIN_CONSUME_KBD_EVENTS,	WIN_ASCII_EVENTS,
					0,
	    WIN_EVENT_PROC, 		handle_event,
	    WIN_WIDTH,			780,
/*	    WIN_HEIGHT,		 	650, */
	    WIN_X,			PATTERN_SIZE*2+70,
	    WIN_Y,			ATTR_ROW(8)+12,
	    CANVAS_AUTO_SHRINK,		FALSE,
	    CANVAS_AUTO_EXPAND,		FALSE,
	    CANVAS_FIXED_IMAGE,		FALSE,
	    CANVAS_RETAINED,		FALSE,
	    CANVAS_WIDTH,		image_wid,
	    CANVAS_HEIGHT,		image_hgt,
	    CANVAS_DEPTH,		1,
	    WIN_BELOW,			panel,
	    0);

  else

    canvas = 
        window_create(base_frame, CANVAS, 
	    WIN_VERTICAL_SCROLLBAR,	scrollbar_create(SCROLL_LINE_HEIGHT,20,0),
	    WIN_HORIZONTAL_SCROLLBAR,	scrollbar_create(SCROLL_LINE_HEIGHT,20,0),
	    WIN_CONSUME_PICK_EVENTS,	WIN_NO_EVENTS,WIN_MOUSE_BUTTONS,
					LOC_DRAG,
					LOC_MOVE,
					0,
	    WIN_CONSUME_KBD_EVENTS,	WIN_ASCII_EVENTS,
					0,
	    WIN_EVENT_PROC, 		handle_event,
	    WIN_WIDTH,			780,
/*	    WIN_HEIGHT,		 	650, */
	    WIN_X,			PATTERN_SIZE*2+70,
	    WIN_Y,			ATTR_ROW(8)+12,
	    CANVAS_AUTO_SHRINK,		FALSE,
	    CANVAS_AUTO_EXPAND,		FALSE,
	    CANVAS_FIXED_IMAGE,		FALSE,
	    CANVAS_RETAINED,		FALSE,
	    CANVAS_WIDTH,		image_wid,
	    CANVAS_HEIGHT,		image_hgt,
	    WIN_BELOW,			panel,
	    0);


    pw = canvas_pixwin(canvas);


    command_panel = window_create(base_frame, PANEL,
	     WIN_FONT,			main_font,
/*	     WIN_X,			 1000-(COMMAND_SIZE*2+32), */
	     WIN_Y,			 ATTR_ROW(7)+12,
	     WIN_WIDTH,			 COMMAND_SIZE*2+5,

	     WIN_HEIGHT,		 COMMAND_SIZE*COMMAND_NO/2+9,
	     WIN_RIGHT_OF,		 canvas,
	     WIN_BELOW,			 panel,
		0);

   command_choice = panel_create_item(command_panel, PANEL_CHOICE, 
	     PANEL_LABEL_BOLD,    	 TRUE,
	     PANEL_SHOW_MENU,		 FALSE,
	     PANEL_LAYOUT,		PANEL_VERTICAL,
	     PANEL_VALUE, 		6,
	     PANEL_LABEL_X,		10,
	     PANEL_LABEL_Y,		4,
	     PANEL_DISPLAY_LEVEL,	PANEL_ALL,
	     PANEL_FEEDBACK,		PANEL_INVERTED,
	     PANEL_CHOICE_IMAGES,	&command12_pr,&command5_pr,
					&command13_pr,&command14_pr,
					&command1_pr,&command6_pr,
					&command15_pr,&command3_pr,
					&command4_pr,&command16_pr,
					&command11_pr,&command10_pr,
					&command2_pr,&command7_pr,
					&command9_pr,&command8_pr,
					0,
	     PANEL_CHOICE_XS,		1,51,1,51,1,51,
					1,51,1,51,1,51,
					1,51,1,51,
					0,
	     
	     PANEL_CHOICE_YS,		1,1,
					50,50,
					99,99,
					148,148,
					197,197,
					246,246,
					295,295,
					344,344,
					0,
	     PANEL_NOTIFY_PROC, 	command_handle,
	     0);


    brush_panel = window_create(base_frame, PANEL,
	     WIN_FONT,			main_font,
	     WIN_SHOW,			TRUE,
	     WIN_X,			 1000-(COMMAND_SIZE*2+32),
	     WIN_Y,			 COMMAND_SIZE*COMMAND_NO/2+9+ATTR_ROW(7)+14,
	     WIN_WIDTH,			 COMMAND_SIZE*2+5,
/*	     WIN_HEIGHT,		 COMMAND_SIZE*4+15, */
	     WIN_BELOW,			 command_panel,
	     WIN_RIGHT_OF,		 canvas,
		0);

   brush_choice = panel_create_item(brush_panel, PANEL_CHOICE, 
	     PANEL_LABEL_BOLD,    	 TRUE,
	     PANEL_SHOW_MENU,		 FALSE,
	     PANEL_LAYOUT,		PANEL_VERTICAL,
	     PANEL_VALUE, 		3,
	     PANEL_LABEL_X,		10,
	     PANEL_LABEL_Y,		4,
	     PANEL_DISPLAY_LEVEL,	PANEL_ALL,
	     PANEL_FEEDBACK,		PANEL_MARKED,
	     PANEL_NOTIFY_PROC, 	change_brush,
	     PANEL_CHOICE_IMAGES,	&brush1_pr,&brush2_pr,
					&brush3_pr,&brush4_pr,
					&brush5_pr,&brush6_pr,
					&brush7_pr,
					0,
	     PANEL_CHOICE_XS,		51,51,51,51,51,51,51,
					0,
	     
	     PANEL_CHOICE_YS,		1,
					35,69,
					103,137,
					171,205,
					0,
	     PANEL_MARK_XS,		3,
					0,
	     
	     PANEL_MARK_YS,		1,
					35,69,
					103,137,
					171,205,
					0,
	     0);

  brushes[0] = &brush1_pr;
  brushes[1] = &brush2_pr;
  brushes[2] = &brush3_pr;
  brushes[3] = &brush4_pr;
  brushes[4] = &brush5_pr;
  brushes[5] = &brush6_pr;
  brushes[6] = &brush7_pr;


    region_panel = window_create(base_frame, PANEL,
	     WIN_FONT,			main_font,
	     WIN_SHOW,			FALSE,
	     WIN_X,			 1000-(COMMAND_SIZE*2+32),
	     WIN_Y,			 COMMAND_SIZE*COMMAND_NO/2+9+ATTR_ROW(7)+14,
	     WIN_WIDTH,			 COMMAND_SIZE*2+5,
/*	     WIN_HEIGHT,		 COMMAND_SIZE*4+4, */
	     WIN_BELOW,			 command_panel,
	     WIN_RIGHT_OF,		 canvas,
		0);

   region_choice = panel_create_item(region_panel, PANEL_CHOICE, 
	     PANEL_LABEL_BOLD,    	 TRUE,
	     PANEL_SHOW_MENU,		 FALSE,
	     PANEL_LAYOUT,		PANEL_VERTICAL,
	     PANEL_VALUE, 		PASTE,
	     PANEL_LABEL_X,		10,
	     PANEL_LABEL_Y,		4,
	     PANEL_DISPLAY_LEVEL,	PANEL_ALL,
	     PANEL_FEEDBACK,		PANEL_INVERTED,
	     PANEL_CHOICE_IMAGES,	&reg_command2_pr,&reg_command3_pr,
					&reg_command4_pr,&reg_command7_pr,
					&reg_command5_pr,&reg_command9_pr,
					&reg_command8_pr,&reg_command1_pr,
					&reg_command6_pr,
					0,
	     PANEL_CHOICE_XS,		1,51,1,51,1,51,1,51,51,
					0,
	     
	     PANEL_CHOICE_YS,		1,1,
					50,50,
					99,99,
					148,148,
					197,
					0,
	     PANEL_NOTIFY_PROC, 	region_handle,
	     0);


    text_panel = window_create(base_frame, PANEL,
	     WIN_FONT,			main_font,
	     WIN_SHOW,			FALSE,
	     WIN_X,			 1000-(COMMAND_SIZE*2+32),
	     WIN_Y,			 COMMAND_SIZE*COMMAND_NO/2+9+ATTR_ROW(7)+14,
	     WIN_WIDTH,			 COMMAND_SIZE*2+5,
/*	     WIN_HEIGHT,		 COMMAND_SIZE*4+4, */
	     WIN_BELOW,			 command_panel,
	     WIN_RIGHT_OF,		 canvas,
		0);

   text_choice = panel_create_item(text_panel, PANEL_CHOICE, 
	     PANEL_LABEL_BOLD,    	 TRUE,
	     PANEL_SHOW_MENU,		 FALSE,
	     PANEL_LAYOUT,		PANEL_VERTICAL,
	     PANEL_VALUE, 		1,
	     PANEL_LABEL_X,		10,
	     PANEL_LABEL_Y,		4,
	     PANEL_DISPLAY_LEVEL,	PANEL_ALL,
	     PANEL_FEEDBACK,		PANEL_INVERTED,
	     PANEL_CHOICE_IMAGES,	&text_center_pr,&text_left_pr,
					&text_right_pr,0,
	     PANEL_CHOICE_XS,		1,51,25,0,
	     PANEL_CHOICE_YS,		1,1,50,0,
	     0);


     font_button = panel_create_item(text_panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(1),
     PANEL_ITEM_Y, ATTR_ROW(5),
     PANEL_LABEL_IMAGE, panel_button_image(text_panel, "fonts", 8, (Pixfont *)0),
     PANEL_NOTIFY_PROC, change_font,
     0);

     panel_create_item(text_panel, PANEL_BUTTON,
     PANEL_ITEM_X, ATTR_COL(1),
     PANEL_ITEM_Y, ATTR_ROW(7),
     PANEL_LABEL_IMAGE, panel_button_image(text_panel, "pt size", 8,(Pixfont *)0),
     PANEL_NOTIFY_PROC, change_font,
     0);


    pattern_panel = window_create(base_frame, PANEL,
	     WIN_FONT,			main_font,
	     WIN_X,			 0,
	     WIN_Y,			ATTR_ROW(7)+12,
 	     WIN_BELOW,			panel,
	     WIN_WIDTH,			 PATTERN_SIZE*2+65,
/*	     WIN_HEIGHT,		 PATTERN_SIZE*PATTERN_NO/2+22, */
		0);

   (void)panel_create_item(pattern_panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, PATTERN_SIZE*PATTERN_NO/2+40,
     PANEL_LABEL_IMAGE, panel_button_image(panel, "define pattern", 14, (Pixfont *)0),
     PANEL_NOTIFY_PROC, pattern_define,
     0);

   pattern_choice = panel_create_item(pattern_panel, PANEL_CHOICE, 
	     PANEL_LABEL_BOLD,    	 TRUE,
	     PANEL_SHOW_MENU,		 FALSE,
	     PANEL_LAYOUT,		PANEL_VERTICAL,
	     PANEL_LABEL_X,		10,
	     PANEL_LABEL_Y,		4,
	     PANEL_VALUE, 		0,
	     PANEL_DISPLAY_LEVEL,	PANEL_ALL,
	     PANEL_FEEDBACK,		PANEL_MARKED,
	     PANEL_NOTIFY_PROC, 	select_pattern,
	     PANEL_CHOICE_IMAGES,	&pattern1_pr,&pattern2_pr,&pattern3_pr,
					&pattern4_pr,&pattern5_pr,
					&pattern6_pr,&pattern7_pr,
					&pattern8_pr,&pattern9_pr,
					&pattern10_pr,
	     				&pattern11_pr,&pattern12_pr,&pattern13_pr,
					&pattern14_pr,&pattern15_pr,
					&pattern16_pr,&pattern17_pr,
					&pattern18_pr,&pattern19_pr,
					&pattern20_pr,
	     				&pattern21_pr,&pattern22_pr,&pattern23_pr,
					&pattern24_pr,&pattern25_pr,
					&pattern26_pr,&pattern27_pr,
					&pattern28_pr,&pattern29_pr,
					&pattern30_pr,
	     				&pattern31_pr,&pattern32_pr,&pattern33_pr,
					&pattern34_pr,&pattern35_pr,
					&pattern36_pr,&pattern37_pr,
					&pattern38_pr,&pattern39_pr,
					&pattern40_pr,0,
	     PANEL_CHOICE_XS,		25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,88,88,88,88,88,88,88,88,88,88,88,88,88,88,88,88,88,88,88,88,0,
	     
	     PANEL_CHOICE_YS,		1+35,33+35,65+35,97+35,129+35,161+35,193+35,225+35,257+35,289+35,
					321+35,353+35,385+35,417+35,449+35,481+35,513+35,545+35,577+35,609+35,
	     				1+35,33+35,65+35,97+35,129+35,161+35,193+35,225+35,257+35,289+35,
					321+35,353+35,385+35,417+35,449+35,481+35,513+35,545+35,577+35,609+35,0,
	     PANEL_MARK_XS,		3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,0,
	     PANEL_MARK_YS,		8+35,40+35,72+35,104+35,136+35,168+35,200+35,232+35,266+35,296+35,
					328+35,360+35,392+35,426+35,456+35,488+35,520+35,552+35,584+35,616+35,
	     				8+35,40+35,72+35,104+35,136+35,168+35,200+35,232+35,266+35,296+35,
					328+35,360+35,392+35,426+35,456+35,488+35,520+35,552+35,584+35,616+35,0,

     0);

  pattern[0] = &pattern1_pr;
  pattern[1] = &pattern2_pr;
  pattern[2] = &pattern3_pr;
  pattern[3] = &pattern4_pr;
  pattern[4] = &pattern5_pr;
  pattern[5] = &pattern6_pr;
  pattern[6] = &pattern7_pr;
  pattern[7] = &pattern8_pr;
  pattern[8] = &pattern9_pr;
  pattern[9] = &pattern10_pr;
  pattern[10] = &pattern11_pr;
  pattern[11] = &pattern12_pr;
  pattern[12] = &pattern13_pr;
  pattern[13] = &pattern14_pr;
  pattern[14] = &pattern15_pr;
  pattern[15] = &pattern16_pr;
  pattern[16] = &pattern17_pr;
  pattern[17] = &pattern18_pr;
  pattern[18] = &pattern19_pr;
  pattern[19] = &pattern20_pr;
  pattern[20] = &pattern21_pr;
  pattern[21] = &pattern22_pr;
  pattern[22] = &pattern23_pr;
  pattern[23] = &pattern24_pr;
  pattern[24] = &pattern25_pr;
  pattern[25] = &pattern26_pr;
  pattern[26] = &pattern27_pr;
  pattern[27] = &pattern28_pr;
  pattern[28] = &pattern29_pr;
  pattern[29] = &pattern30_pr;
  pattern[30] = &pattern31_pr;
  pattern[31] = &pattern32_pr;
  pattern[32] = &pattern33_pr;
  pattern[33] = &pattern34_pr;
  pattern[34] = &pattern35_pr;
  pattern[35] = &pattern36_pr;
  pattern[36] = &pattern37_pr;
  pattern[37] = &pattern38_pr;
  pattern[38] = &pattern39_pr;
  pattern[39] = &pattern40_pr;

   current_pattern = panel_create_item(pattern_panel, PANEL_MESSAGE, 
     PANEL_ITEM_X, ATTR_COL(6),
     PANEL_ITEM_Y, 3,
     PANEL_LABEL_IMAGE, &pattern1_pr,
     0);

  image_depth = pw->pw_pixrect->pr_depth;

  if (BW_mode)
	image_depth = 1;

  if (image_depth > 1)
    {
         color_frame = window_create(base_frame,FRAME,
                FRAME_LABEL,    "Color Palette",
                FRAME_SHOW_LABEL, TRUE,
                FRAME_INHERIT_COLORS,  TRUE,
                WIN_FONT,       main_font,
#if 0
                WIN_WIDTH,      PALET_BLOCK*16+9,
                WIN_HEIGHT,     PALET_BLOCK*(16+3)+20+ATTR_ROW(2),
#endif
                WIN_X,          650,
                WIN_Y,          10,
                0);

    color_panel = window_create(color_frame, PANEL,
             PANEL_LABEL_BOLD,           TRUE,
                WIN_FONT,       main_font,
                0);
   (void)panel_create_item(color_panel, PANEL_BUTTON,
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_IMAGE, panel_button_image(color_panel, "done", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, color_done,
     0);
   panel_create_item(color_panel, PANEL_MESSAGE,
     PANEL_ITEM_X, ATTR_COL(8),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_STRING, "Current Color:",
     0);
   cur_col_msg= panel_create_item(color_panel, PANEL_MESSAGE,
     PANEL_LABEL_STRING, "1",
     0);
        red_slide= panel_create_item(color_panel, PANEL_SLIDER,
                PANEL_ITEM_Y,           ATTR_ROW(1),
                PANEL_ITEM_X,           ATTR_COL(0),
                PANEL_LABEL_STRING,     "Red:  ",
                PANEL_SHOW_RANGE,       FALSE,
                PANEL_MAX_VALUE,        255,
                PANEL_SLIDER_WIDTH,     100,
                PANEL_VALUE,            red[0],
                PANEL_NOTIFY_LEVEL,     PANEL_ALL,
                PANEL_NOTIFY_PROC,      col_change,
                PANEL_CLIENT_DATA,      0,
                0);
        green_slide= panel_create_item(color_panel, PANEL_SLIDER,
                PANEL_ITEM_Y,           ATTR_ROW(2),
                PANEL_ITEM_X,           ATTR_COL(0),
                PANEL_LABEL_STRING,     "Green:",
                PANEL_SHOW_RANGE,       FALSE,
                PANEL_MAX_VALUE,        255,
                PANEL_SLIDER_WIDTH,     100,
                PANEL_VALUE,            green[0],
                PANEL_NOTIFY_LEVEL,     PANEL_ALL,
                PANEL_NOTIFY_PROC,      col_change,
                PANEL_CLIENT_DATA,      1,
                0);
        blue_slide= panel_create_item(color_panel, PANEL_SLIDER,
                PANEL_ITEM_Y,           ATTR_ROW(3),
                PANEL_ITEM_X,           ATTR_COL(0),
                PANEL_LABEL_STRING,     "Blue: ",
                PANEL_SHOW_RANGE,       FALSE,
                PANEL_MAX_VALUE,        255,
                PANEL_SLIDER_WIDTH,     100,
                PANEL_VALUE,            blue[0],
                PANEL_NOTIFY_PROC,      col_change,
                PANEL_NOTIFY_LEVEL,     PANEL_ALL,
                PANEL_CLIENT_DATA,      2,
                0);

        panel_create_item(color_panel, PANEL_BUTTON,
                PANEL_ITEM_Y,           ATTR_ROW(4),
                PANEL_ITEM_X,           ATTR_COL(0),
                PANEL_NOTIFY_PROC,      col_load,
                PANEL_LABEL_IMAGE,      panel_button_image(
                        color_panel, "Load", 0, (Pixfont *) NULL),
                0);
        panel_create_item(color_panel, PANEL_BUTTON,
                PANEL_NOTIFY_PROC,      col_save,
                PANEL_LABEL_IMAGE,      panel_button_image(
                        color_panel, "Save", 0, (Pixfont *) NULL),
                0);
        col_file= panel_create_item(color_panel, PANEL_TEXT,
                PANEL_ITEM_Y,           ATTR_ROW(5),
                PANEL_ITEM_X,           ATTR_COL(0),
                PANEL_LABEL_STRING,     "Filename:",
                PANEL_VALUE_DISPLAY_LENGTH, 19,
                PANEL_NOTIFY_PROC, make_new_col_name,
                PANEL_NOTIFY_STRING, "\033",
                PANEL_VALUE_STORED_LENGTH, MAX_FILE_NAME,
                0);
    window_fit(color_panel);


          color_canvas =
            window_create(color_frame, CANVAS,
            WIN_CONSUME_PICK_EVENTS,    WIN_NO_EVENTS,WIN_MOUSE_BUTTONS,
                                        0,
            WIN_EVENT_PROC,             color_handle_event,
                WIN_X,          0,
                WIN_BELOW,      color_panel,
            WIN_WIDTH,                  PALET_BLOCK*16,
            WIN_HEIGHT,                 PALET_BLOCK*(16+3),
            CANVAS_WIDTH,               PALET_BLOCK*16,
            CANVAS_HEIGHT,              PALET_BLOCK*(16+3),
            CANVAS_AUTO_SHRINK,         FALSE,
            CANVAS_FIXED_IMAGE,         TRUE,
            CANVAS_AUTO_EXPAND,         FALSE,
            CANVAS_REPAINT_PROC,        draw_colormap,
            CANVAS_RESIZE_PROC,         draw_colormap,
            0),

        color_pw = canvas_pixwin(color_canvas);

        window_fit(color_frame);
    }


   
    fat_frame = window_create(base_frame,FRAME,
		FRAME_LABEL,    "Big bits",
	        FRAME_SHOW_LABEL, TRUE,
		FRAME_INHERIT_COLORS,  TRUE,
		WIN_FONT,	main_font,
		WIN_WIDTH,	ATTR_COL(31),
		WIN_HEIGHT,	ATTR_ROW(24),
	     WIN_X,			 0,
	     WIN_Y,			ATTR_ROW(8)+12,
		0);

    fat_canvas = 
        window_create(fat_frame, CANVAS, 
	    WIN_CONSUME_PICK_EVENTS,	WIN_NO_EVENTS,WIN_MOUSE_BUTTONS,
					LOC_DRAG,
					LOC_MOVE, 
					0,
	    WIN_EVENT_PROC, 		fat_handle_event,
		WIN_X,		0,
		WIN_Y,		ATTR_ROW(2)+5,
	    CANVAS_WIDTH,		ATTR_COL(30),
	    CANVAS_HEIGHT,		ATTR_ROW(14),
	    CANVAS_AUTO_SHRINK,		TRUE,
	    CANVAS_FIXED_IMAGE,		FALSE,
	    CANVAS_AUTO_EXPAND,		TRUE,
	    CANVAS_RETAINED,		TRUE,
	    0),

    fat_pw = canvas_pixwin(fat_canvas);

    fat_panel = window_create(fat_frame, PANEL,
	     PANEL_LABEL_BOLD,    	 TRUE,
		WIN_FONT,	main_font,
	     WIN_X,			 0,
	     WIN_Y,			 3,
	     WIN_WIDTH,			 ATTR_COL(30),
	     WIN_HEIGHT,		 ATTR_ROW(1)+4,
		0);
   
    magnify_cycle = 
	panel_create_item(fat_panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(8),
	    PANEL_ITEM_Y, ATTR_ROW(0),
	    PANEL_LABEL_STRING, "Magnification:",
	    PANEL_CHOICE_STRINGS, "1x", "2x","3x","4x",
				  "5x","6x","7x","8x","9x",
				  "10x","11x","12x","13x","14x",
				  "15x","16x","17x","18x","19x",
				  "20x", 0,
	    PANEL_VALUE, 8,
	    PANEL_NOTIFY_PROC, fat_parms,
	    0);

   (void)panel_create_item(fat_panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_IMAGE, panel_button_image(fat_panel, "done", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, fat_done,
     0);


   (void)panel_create_item(panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(8),
     PANEL_ITEM_Y, ATTR_ROW(1),
     PANEL_LABEL_IMAGE, panel_button_image(panel, "view", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, viewer,
     0);

    view_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(16),
	    PANEL_ITEM_Y, ATTR_ROW(1),
	    PANEL_LABEL_STRING, "View:",
	    PANEL_CHOICE_STRINGS, "Touchup info", "man page (help)", "Cut/Paste buffer", 0,
	    PANEL_VALUE, 1,
	    0);

   (void)panel_create_item(panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(8),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_IMAGE, panel_button_image(panel, "load", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, load_file,
     0);
    load_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(16),
	    PANEL_ITEM_Y, ATTR_ROW(0),
	    PANEL_LABEL_STRING, "Load:",
	    PANEL_CHOICE_STRINGS, "entire image", "Cut/Paste buffer", 0,
	    PANEL_VALUE, LOAD_ALL,
	    0);

   (void)panel_create_item(panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(45),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_IMAGE, panel_button_image(panel, "save", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, save_file,
     0);

    save_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(53),
	    PANEL_ITEM_Y, ATTR_ROW(0),
	    PANEL_LABEL_STRING, "Save:",
	    PANEL_CHOICE_STRINGS, "entire image", "Cut/Paste buffer", 0,
	    PANEL_VALUE, SAVE_ALL,
	    0);

    compress_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(53),
	    PANEL_ITEM_Y, ATTR_ROW(1),
	    PANEL_LABEL_STRING, "Save:",
	    PANEL_CHOICE_STRINGS, "Standard format","Byte_Encoded",0,
	    PANEL_VALUE, 0,
	    0);

   (void)panel_create_item(panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_LABEL_IMAGE, panel_button_image(panel, "quit", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, quit,
     0);

   undo_button = panel_create_item(panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, ATTR_ROW(2),
     PANEL_LABEL_IMAGE, panel_button_image(panel, "undo", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, undo_screen,
     0);

    grid_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(8),
	    PANEL_ITEM_Y, ATTR_ROW(2),
	    PANEL_LABEL_STRING, "Grid size:",
	    PANEL_CHOICE_STRINGS, "none","5","10","15","20","25","30",0,
	    PANEL_VALUE, grid_size,
	    PANEL_NOTIFY_PROC, change_parms,
	    0);

    border_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(30),
	    PANEL_ITEM_Y, ATTR_ROW(2),
	    PANEL_LABEL_STRING, "Borders:",
	    PANEL_CHOICE_STRINGS, "No","Yes",0,
	    PANEL_VALUE, 1,
	    0);

   file_panel=panel_create_item(panel, PANEL_TEXT, 
     PANEL_ITEM_X, ATTR_COL(80),
     PANEL_ITEM_Y, ATTR_ROW(0),
     PANEL_VALUE, file_name,
     PANEL_VALUE_DISPLAY_LENGTH, 35,
     PANEL_LABEL_STRING, "Filename:",
     PANEL_NOTIFY_PROC, make_new_name,
     PANEL_NOTIFY_STRING, "\033",
     PANEL_VALUE_STORED_LENGTH, MAX_FILE_NAME,
     PANEL_SHOW_MENU, FALSE,
     0);

    ROP_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(80),
	    PANEL_ITEM_Y, ATTR_ROW(1),
	    PANEL_LABEL_STRING, "Bitmap OP:",
	    PANEL_CHOICE_STRINGS, "default","XOR","AND","OR","SRC",0,
	    PANEL_VALUE, 0,
	    PANEL_NOTIFY_PROC, change_parms,
	    0);

   resize_button = panel_create_item(panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(80),
     PANEL_ITEM_Y, ATTR_ROW(2),
     PANEL_LABEL_IMAGE, panel_button_image(panel, "resize", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, resize_canvas,
     0);

   width_text=panel_create_item(panel, PANEL_TEXT, 
     PANEL_ITEM_X, ATTR_COL(89),
     PANEL_ITEM_Y, ATTR_ROW(2),
     PANEL_VALUE, "1152",
     PANEL_VALUE_DISPLAY_LENGTH, 6,
     PANEL_LABEL_STRING, "Width:",
     PANEL_VALUE_STORED_LENGTH, 6,
     PANEL_SHOW_MENU, FALSE,
     0);

   height_text=panel_create_item(panel, PANEL_TEXT, 
     PANEL_ITEM_X, ATTR_COL(103),
     PANEL_ITEM_Y, ATTR_ROW(2),
     PANEL_VALUE, "900",
     PANEL_VALUE_DISPLAY_LENGTH, 6,
     PANEL_LABEL_STRING, "Heigth:",
     PANEL_VALUE_STORED_LENGTH, 6,
     PANEL_SHOW_MENU, FALSE,
     0);


   lasso_remove=panel_create_item(panel, PANEL_TOGGLE, 
     PANEL_ITEM_X, ATTR_COL(111),
     PANEL_ITEM_Y, ATTR_ROW(1),
     PANEL_SHOW_MENU, FALSE,
     PANEL_LABEL_STRING, "lasso clear:",
     PANEL_CHOICE_STRINGS, "",0,
     PANEL_TOGGLE_VALUE, 0, TRUE,
     0);

    mono_cycle =
	panel_create_item(panel, PANEL_CYCLE,
	    PANEL_ITEM_X, ATTR_COL(48),
	    PANEL_ITEM_Y, ATTR_ROW(2),
	    PANEL_LABEL_STRING, "Draw color",
	    PANEL_CHOICE_STRINGS, "WHITE", "BLACK",
				   0,
	    PANEL_VALUE, 1,
	    PANEL_NOTIFY_PROC, change_parms,
	    0);

   color_button = panel_create_item(panel, PANEL_BUTTON, 
     PANEL_ITEM_X, ATTR_COL(52),
     PANEL_ITEM_Y, ATTR_ROW(2),
     PANEL_SHOW_ITEM, FALSE,
     PANEL_LABEL_IMAGE, panel_button_image(panel, "color", 5, (Pixfont *)0),
     PANEL_NOTIFY_PROC, color_mode,
     0);

   msg_string = panel_create_item(panel, PANEL_MESSAGE, 
     PANEL_ITEM_X, ATTR_COL(0),
     PANEL_ITEM_Y, ATTR_ROW(4),
     PANEL_LABEL_BOLD,    TRUE,
     PANEL_LABEL_STRING, "",
     0);


/*
 * Confirmer window
 */
   confirmer = window_create(base_frame, FRAME, 
                WIN_WIDTH,      300,
                WIN_HEIGHT,     150,
		WIN_SHOW,	FALSE,
		WIN_X,		1152/2 - 390/2,
		WIN_Y,		900/2 - 140/2,
                FRAME_SHOW_LABEL, FALSE,
                                  0);

   con_panel = window_create(confirmer, PANEL,
                WIN_WIDTH,      390,
                WIN_HEIGHT,     60,
                                 0);

   con_msg_string = panel_create_item(con_panel, PANEL_MESSAGE,
		  PANEL_ITEM_X, 0,
		  PANEL_ITEM_Y, 10,
                  PANEL_LABEL_STRING, "",
                                        0);

   panel_create_item(con_panel, PANEL_BUTTON,
                  PANEL_ITEM_X, 130,
                  PANEL_ITEM_Y, 35,
                  PANEL_LABEL_IMAGE, panel_button_image(con_panel,"NO",3,0),
                  PANEL_CLIENT_DATA, FALSE,
                  PANEL_NOTIFY_PROC, yes_no,
                  0);

   panel_create_item(con_panel, PANEL_BUTTON,
                  PANEL_ITEM_X, 205,
                  PANEL_ITEM_Y, 35,
                  PANEL_LABEL_IMAGE, panel_button_image(con_panel, "YES", 3, 0),
                  PANEL_CLIENT_DATA, TRUE,
                  PANEL_NOTIFY_PROC, yes_no,
                  0);

   window_fit(confirmer);

   
  fat_cursor = window_get(canvas,WIN_CURSOR);
  cursor_set(fat_cursor,CURSOR_OP,PIX_SRC^PIX_DST,0);
  window_set(canvas,WIN_CURSOR,fat_cursor,0);

#ifdef CHANGE_CURSOR
  old_cur = cursor_copy(fat_cursor); /* Save this for later... */
#endif

  fat_cursor = window_get(fat_canvas,WIN_CURSOR);
  cursor_set(fat_cursor,CURSOR_OP,PIX_SRC^PIX_DST,0);
  window_set(fat_canvas,WIN_CURSOR,fat_cursor,0);

}





