
/*
	strwin.c - strings, lines, and boxes

	$Header: strwin.c,v 1.8 89/08/27 11:56:01 pturner Locked $
*/

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sunwindow/attr.h>
#include <sunwindow/defaults.h>
#include <suntool/frame.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <pixrect/pixrect.h>
#include "defines.h"
#include "objdefs.h"
#include "globals.h"

#define PIX_XOR PIX_SRC^PIX_DST

#define define_select_string_panel2(panel,panel_item,notify_proc,x,y) \
				panel_item=panel_create_item( panel,\
				PANEL_CYCLE,\
				PANEL_LABEL_STRING,"Select string:",\
	 			PANEL_CHOICE_STRINGS,\
			"Str 0", "Str 1", "Str 2", "Str 3", "Str 4", "Str 5",\
			"Str 6", "Str 7", "Str 8", "Str 9", "Str 10", "Str 11",\
			"Str 12", "Str 13", "Str 14", 0, PANEL_NOTIFY_PROC,notify_proc,\
	    		PANEL_ITEM_X,x,\
	    		PANEL_ITEM_Y,y,\
			0 );

void define_strings_popup();

extern Pixfont *winfont;
extern Window main_frame, main_panel;

plotstr pstr[MAXSTR];

Frame strings_frame;
static Panel strings_panel;
static Panel_item string_item;
static Panel_item strings_item;
static Panel_item strings_font_item;
static Panel_item strings_rot_item;
static Panel_item strings_size_item;
Panel_item strings_x_item;
Panel_item strings_y_item;

static Panel_item lines_arrow_item;
static Panel_item lines_pen_item;
static Panel_item boxes_pen_item;

static void do_strings_proc();

void drawgraph();

extern int box_color, line_color, line_arrow;

static void do_boxes_proc()
{
    set_action(0);
    set_action(MAKE_BOX_1ST);
    box_color = (int) panel_get_value(boxes_pen_item) + 1;
}

static void do_lines_proc()
{
    set_action(0);
    set_action(MAKE_LINE_1ST);
    line_color = (int) panel_get_value(lines_pen_item) + 1;
    line_arrow = (int) panel_get_value(lines_arrow_item);
}

static void do_move_proc()
{
    extern Cursor cursor_move;
    extern Canvas canvas;

    set_action(0);
    set_action(MOVE_OBJECT_1ST);
    window_set(canvas, WIN_CURSOR, cursor_move, 0);
}

static void do_delete_object_proc()
{
    extern Cursor cursor_del;
    extern Canvas canvas;

    set_action(0);
    set_action(DEL_OBJECT);
    window_set(canvas, WIN_CURSOR, cursor_del, 0);
}

updatestrings(i)
    int i;
{
    char buf[256];

    panel_set_value(string_item, i);
    panel_set_value(strings_font_item, pstr[i].font);
    sprintf(buf, "%lf", pstr[i].x);
    panel_set_value(strings_item, pstr[i].s);
    panel_set_value(strings_x_item, buf);
    sprintf(buf, "%lf", pstr[i].y);
    panel_set_value(strings_y_item, buf);
    sprintf(buf, "%lf", pstr[i].size);
    panel_set_value(strings_size_item, buf);
    sprintf(buf, "%d", pstr[i].rot);
    panel_set_value(strings_rot_item, buf);
}

static void do_strings_proc(item, event)
    Panel_item item;
    Event *event;
{
    char buf[36];
    int i;

    i = (int) panel_get_value(string_item);
    strcpy(pstr[i].s, panel_get_value(strings_item));
    pstr[i].font = (int) panel_get_value(strings_font_item);
    strcpy(buf, (char *) panel_get_value(strings_rot_item));
    pstr[i].rot = atoi(buf);
    strcpy(buf, (char *) panel_get_value(strings_size_item));
    pstr[i].size = atof(buf);
    strcpy(buf, (char *) panel_get_value(strings_x_item));
    pstr[i].x = atof(buf);
    strcpy(buf, (char *) panel_get_value(strings_y_item));
    pstr[i].y = atof(buf);
    drawgraph();
}

static void strings_item_proc(item, value, event)
    Panel_item item;
    unsigned int value;
    Event *event;
{
    updatestrings(value);
}

static void strings_loc_proc()
{
    set_action(0);
    set_action(STR_LOC);
}

static void strings_done_proc()
{
    window_set(strings_frame, WIN_SHOW, FALSE, 0);
}

void do_strings_popup()
{
    window_set(strings_frame, WIN_SHOW, TRUE, 0);
}

void define_strings_popup()
{

    strings_frame = window_create(main_frame, FRAME,
				  WIN_Y, 50,
				  FRAME_LABEL, "Strings and things",
				  FRAME_SHOW_LABEL, TRUE,
			     WIN_ERROR_MSG, "Couldn't create strings_frame",
				  0);
    strings_panel = window_create(strings_frame, PANEL, 0);

    panel_create_item(strings_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		    panel_button_image(strings_panel, "Define", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(1),
		      PANEL_NOTIFY_PROC, do_strings_proc,
		      0);

    define_select_string_panel2(strings_panel, string_item, strings_item_proc, ATTR_COL(1), ATTR_ROW(2));

    strings_font_item = panel_create_item(strings_panel, PANEL_CYCLE,
					  PANEL_LABEL_STRING, "Font:",
					  PANEL_CHOICE_STRINGS,
					  "Romanc",
					  "Romand",
					  "Romans",
					  "Romant",
					  "Italicc",
					  "Italict",
					  "Scriptc",
					  "Scripts",
					  "Greekc",
					  "Greeks",
					  0,
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(3),
					  0);
    strings_rot_item = panel_create_item(strings_panel, PANEL_TEXT,
					 PANEL_LABEL_STRING, "Rotation: ",
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(4),
					 PANEL_VALUE_DISPLAY_LENGTH, 5, 0);
    strings_size_item = panel_create_item(strings_panel, PANEL_TEXT,
					  PANEL_LABEL_STRING, "Size: ",
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(5),
					  PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    strings_x_item = panel_create_item(strings_panel, PANEL_TEXT,
				       PANEL_LABEL_STRING, "X: ",
				       PANEL_ITEM_X, ATTR_COL(1),
				       PANEL_ITEM_Y, ATTR_ROW(6),
				       PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    strings_y_item = panel_create_item(strings_panel, PANEL_TEXT,
				       PANEL_LABEL_STRING, "Y: ",
				       PANEL_ITEM_X, ATTR_COL(1),
				       PANEL_ITEM_Y, ATTR_ROW(7),
				       PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    strings_item = panel_create_item(strings_panel, PANEL_TEXT,
				     PANEL_LABEL_STRING, "String: ",
				     PANEL_ITEM_X, ATTR_COL(1),
				     PANEL_ITEM_Y, ATTR_ROW(8),
				     PANEL_VALUE_STORED_LENGTH,MAXSTRLEN,
				     PANEL_VALUE_DISPLAY_LENGTH, 60, 0);
    panel_create_item(strings_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(strings_panel, "Define location with mouse", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(9),
		      PANEL_NOTIFY_PROC, strings_loc_proc,
		      0);
    panel_create_item(strings_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(strings_panel, "Line", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(11),
		      PANEL_NOTIFY_PROC, do_lines_proc,
		      0);
    lines_pen_item = panel_create_item(strings_panel, PANEL_CYCLE,
				       PANEL_LABEL_STRING, "Pen:",
				       PANEL_CHOICE_STRINGS,
				       "Pen 1",
				       "Pen 2",
				       "Pen 3",
				       "Pen 4",
				       "Pen 5",
				       "Pen 6",
				       "Pen 7",
				       "Pen 8", 0,
				       PANEL_ITEM_X, ATTR_COL(15),
				       PANEL_ITEM_Y, ATTR_ROW(11),
				       0);
    lines_arrow_item = panel_create_item(strings_panel, PANEL_CYCLE,
					 PANEL_LABEL_STRING, "Arrow:",
					 PANEL_CHOICE_STRINGS,
					 "None",
					 "At start",
					 "At end",
					 "Both ends",
					 0,
					 PANEL_ITEM_X, ATTR_COL(30),
					 PANEL_ITEM_Y, ATTR_ROW(11),
					 0);
    panel_create_item(strings_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(strings_panel, "Box", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(12),
		      PANEL_NOTIFY_PROC, do_boxes_proc,
		      0);
    boxes_pen_item = panel_create_item(strings_panel, PANEL_CYCLE,
				       PANEL_LABEL_STRING, "Pen:",
				       PANEL_CHOICE_STRINGS,
				       "Pen 1",
				       "Pen 2",
				       "Pen 3",
				       "Pen 4",
				       "Pen 5",
				       "Pen 6",
				       "Pen 7",
				       "Pen 8", 0,
				       PANEL_ITEM_X, ATTR_COL(15),
				       PANEL_ITEM_Y, ATTR_ROW(12),
				       0);
    panel_create_item(strings_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(strings_panel, "Move", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(13),
		      PANEL_NOTIFY_PROC, do_move_proc,
		      0);
    panel_create_item(strings_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		    panel_button_image(strings_panel, "Delete", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(14),
		      PANEL_NOTIFY_PROC, do_delete_object_proc,
		      0);
    panel_create_item(strings_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(strings_panel, "Done", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(16),
		      PANEL_NOTIFY_PROC, strings_done_proc,
		      0);
    updatestrings(0);
    window_fit(strings_panel);
    window_fit(strings_frame);
}
