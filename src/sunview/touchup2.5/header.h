
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
	file: header.h
	purpose: this file has all of the globals that need to be defined
		and the proper include files
	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Tue Jun 28 12:34:29 EDT 1988
		author:	rainier!pell@uunet.UU.NET
		changes:corrected a declaration error for old_x ....
**************************************************************************/
#include <stdio.h>
#include <strings.h>
#include <sys/file.h>
#include <suntool/sunview.h>
#include <suntool/panel.h>
#include <suntool/canvas.h>
#include <suntool/walkmenu.h>
#include <suntool/scrollbar.h>
#include <pixrect/pixrect_hs.h>

#define CLEAR 1
#define NO_CLEAR 0

#define DEFAULT_IMAGE_WID  1152
#define DEFAULT_IMAGE_HGT  900


#define TEXT 0
#define MAGNIFY 1
#define SEL_REG 2
#define GET_PT 3
#define LASO 4
#define FFILL 5
#define PAINT 6
#define DRAW 7
#define LINE 8
#define ERASE 9
#define RECT_H 10
#define RECT_F 11
#define CIRCLE 12
#define OVAL 13
#define POLY_H 14
#define POLY_F 15

#define FLIP_HOR 0
#define FLIP_VER 1
#define INVERSE 2
#define ROTATE 3
#define COPY 4
#define SCALE 5
#define MOVE 6
#define CUT 7
#define PASTE 8

#define TRANSPARENT 7

#define PIX_XOR PIX_SRC^PIX_DST
#define MAX_PTS 3000

#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define MAX(x,y) ((x) > (y) ? (x) : (y))

#define TEXT_HGT real_font->pf_defaultsize.y

#define MAX_POLY 100

#define BRUSH_NO 7

#define CURSOR_SIZE 22
#define PATTERN_SIZE 32
#define PATTERN_NO 40

#define COMMAND_SIZE 48
#define COMMAND_NO 16

#define MAIN_FONT "/usr/lib/fonts/fixedwidthfonts/screen.r.14"

#define MY_pr_destroy(prect) if (prect) pr_destroy(prect);prect=NULL;

#define MAX_FILE_NAME 120

#define SAVE_ALL 0
#define SAVE_CUT_PASTE 1

#define LOAD_ALL 0
#define LOAD_CUT_PASTE 1

#define NILPR ((struct pixrect *)0)

#define PALET_BLOCK 15

#define SCREEN_MAX_X  400
#define SCREEN_MAX_Y  400

extern int BW_mode;
extern int run_box;
extern int paint_brush,line_brush;
extern int brush_radius[7];
extern unsigned char red[256],green[256],blue[256];
extern unsigned char temp_red[256],temp_green[256],temp_blue[256];
extern int image_wid,image_hgt,image_depth;
extern int top_x,top_y,bottom_x,bottom_y;
extern int cur_color,grid_size;
extern int magnify_fac,fat_x,fat_y,fat_source_x,fat_source_y;
extern int mouse_left,mouse_middle;
extern int select_pt_x,select_pt_y;
extern int old_x, old_y;
extern int start_x, start_y;
extern char file_name[MAX_FILE_NAME];
extern struct pixrect *cut_buffer_pr,*undo_pr;
extern struct pixfont *main_font;
extern struct pixrect *brush_temp_pr;
extern struct pixrect *pattern[];
extern struct pixrect *brushes[];
extern struct pr_pos poly_points[];
extern struct pr_pos  ptlist[];

extern struct  pixfont  *real_font;

extern Frame base_frame,fat_frame,color_frame;
extern Frame confirmer;
extern Canvas canvas,fat_canvas,color_canvas;
extern Pixwin *pw,*fat_pw,*color_pw;
extern Scrollbar	vertical_sb, horizontal_sb;
extern Panel       panel,fat_panel,color_panel,pattern_panel,
		brush_panel,region_panel,command_panel, text_panel;
extern Panel_item magnify_cycle, command_choice,height_text,width_text;
extern Panel_item lasso_remove, text_choice;
extern Panel_item file_panel,brush_choice,mono_cycle,save_cycle,
	msg_string,color_button,border_cycle,view_cycle,current_pattern,
		load_cycle,pattern_choice,region_choice,grid_cycle,
		compress_cycle,undo_button,ROP_cycle,yes_button,no_button;

extern struct pixrect			brush1_pr,brush2_pr,brush3_pr,
					brush4_pr,brush5_pr,brush6_pr,
					brush7_pr;

extern struct pixrect			text_center_pr,text_left_pr,
					text_right_pr;

extern struct pixrect			pattern1_pr,pattern2_pr,pattern3_pr,
					pattern4_pr,pattern5_pr,
					pattern6_pr,pattern7_pr,
					pattern8_pr,pattern9_pr,
					pattern10_pr,
	     				pattern11_pr,pattern12_pr,pattern13_pr,
					pattern14_pr,pattern15_pr,
					pattern16_pr,pattern17_pr,
					pattern18_pr,pattern19_pr,
					pattern20_pr,
	     				pattern21_pr,pattern22_pr,pattern23_pr,
					pattern24_pr,pattern25_pr,
					pattern26_pr,pattern27_pr,
					pattern28_pr,pattern29_pr,
					pattern30_pr,
	     				pattern31_pr,pattern32_pr,pattern33_pr,
					pattern34_pr,pattern35_pr,
					pattern36_pr,pattern37_pr,
					pattern38_pr,pattern39_pr,
					pattern40_pr;

extern struct pixrect			command1_pr,command2_pr,command3_pr,
					command4_pr,command5_pr,
					command6_pr,command7_pr,
					command8_pr,command9_pr,
					command10_pr,
	     				command11_pr,
					command12_pr,command13_pr,
					command14_pr,command15_pr,
					command16_pr;

extern struct pixrect			reg_command1_pr,reg_command2_pr,
					reg_command3_pr,reg_command4_pr,
					reg_command5_pr,reg_command6_pr,
					reg_command7_pr,reg_command8_pr,
					reg_command9_pr;

extern char *malloc();
extern quit();
extern color_handle_event();
extern draw_colormap();
extern color_done();
extern fat_mode();
extern fat_handle_event();
extern fat_parms();
extern fat_update();
extern fat_done();
extern handle_event();
extern load_file();
extern save_file();
extern cut_region();
extern copy_region();
extern paste_region();
extern mouse_parms();
extern change_parms();
extern color_mode();
extern undo_screen();
extern poly_addpt();
extern clear_screen();
extern clean_poly();
extern confirm();
extern draw_circle();
extern draw_oval();
extern init_colortable();
extern command_handle();
extern region_handle();
extern struct pixrect *my_mem_create();
extern rotate_region();
extern draw_text();
extern fill_mode();
extern move_region();
extern info_init();
extern pattern_define();
extern viewer();
extern select_pattern();
extern char *expand_file_name();
extern make_new_name();
extern int get_current_ROP();
extern resize_canvas();
extern return_true();
extern return_false();
extern change_brush();
extern move_box();

#ifdef CHANGE_CURSOR
extern Cursor old_cur, draw_cur, erase_cur, ffill_cur, laso_cur,
		 paint_cur, sel_point_cur, text_cur;
extern init_cursors();
extern change_cursor();
#endif


int new_draw_text();
int finsih_text();


