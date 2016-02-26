/* $Id: strwin.c,v 1.9 92/07/19 08:14:00 pturner Exp Locker: pturner $
 *
 * strings, lines, and boxes
 *
 */

#include <stdio.h>
#include <math.h>
#include <sys/types.h>

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/cursor.h>

#include "globals.h"

extern Frame main_frame;

/*
extern Xv_cursor cursor_box, cursor_line, cursor_del, cursor_move, cursor_strloc;
*/

void define_objects_popup();
void define_strings_popup();
void define_lines_popup();
void define_boxes_popup();

Frame strings_frame;
static Panel strings_panel;

Frame lines_frame;
static Panel lines_panel;

Frame boxes_frame;
static Panel boxes_panel;

static Panel_item strings_font_item;
static Panel_item strings_rot_item;
static Panel_item strings_size_item;
static Panel_item strings_loc_item;
static Panel_item strings_pen_item;
static Panel_item strings_linew_item;
static Panel_item strings_just_item;
Panel_item strings_x_item;
Panel_item strings_y_item;

static Panel_item lines_arrow_item;
static Panel_item lines_arrowhead_item;
static Panel_item lines_asize_item;
static Panel_item lines_pen_item;
static Panel_item lines_style_item;
static Panel_item lines_width_item;
static Panel_item lines_loc_item;
static Panel_item boxes_pen_item;
static Panel_item boxes_lines_item;
static Panel_item boxes_linew_item;
static Panel_item boxes_fill_item;
static Panel_item boxes_fillpat_item;
static Panel_item boxes_fillcol_item;
static Panel_item boxes_loc_item;

/* the following are defined in objutils.c */
void do_boxes_proc();
void do_lines_proc();
void do_move_proc();
void do_delete_object_proc();
void strings_loc_proc();
void strings_ang_proc();
void strings_edit_proc();
void do_clear_lines();
void do_clear_boxes();
void do_clear_text();

void define_string_defaults()
{
    int iv;

    if (strings_frame) {
	string_font = (int) xv_get(strings_font_item, PANEL_VALUE);
	string_color = (int) xv_get(strings_pen_item, PANEL_VALUE);
	string_linew = (int) xv_get(strings_linew_item, PANEL_VALUE) + 1;
	iv = (int) xv_get(strings_size_item, PANEL_VALUE);
	string_size = iv / 100.0;
	iv = (int) xv_get(strings_rot_item, PANEL_VALUE);
	string_rot = iv;
	string_loctype = (int) xv_get(strings_loc_item, PANEL_VALUE) ? VIEW : WORLD;
	string_just = (int) xv_get(strings_just_item, PANEL_VALUE);
    }
}

void boxes_def_proc()
{
    box_color = (int) xv_get(boxes_pen_item, PANEL_VALUE);
    box_loctype = (int) xv_get(boxes_loc_item, PANEL_VALUE) ? VIEW : WORLD;
    box_lines = (int) xv_get(boxes_lines_item, PANEL_VALUE) + 1;
    box_linew = (int) xv_get(boxes_linew_item, PANEL_VALUE) + 1;
    switch (xv_get(boxes_fill_item, PANEL_VALUE)) {
    case 0:
	box_fill = NONE;
	break;
    case 1:
	box_fill = COLOR;
	break;
    case 2:
	box_fill = PATTERN;
	break;
    }
    box_fillcolor = (int) xv_get(boxes_fillcol_item, PANEL_VALUE);
    box_fillpat = (int) xv_get(boxes_fillpat_item, PANEL_VALUE) + 1;
}

void lines_def_proc()
{
    int value;

    value = (int) xv_get(lines_asize_item, PANEL_VALUE);
    line_asize = value / 50.0;
    line_color = (int) xv_get(lines_pen_item, PANEL_VALUE);
    line_arrow = (int) xv_get(lines_arrow_item, PANEL_VALUE);
    line_atype = (int) xv_get(lines_arrowhead_item, PANEL_VALUE);
    line_lines = (int) xv_get(lines_style_item, PANEL_VALUE) + 1;
    line_linew = (int) xv_get(lines_width_item, PANEL_VALUE) + 1;
    line_loctype = (int) xv_get(lines_loc_item, PANEL_VALUE) ? VIEW : WORLD;
}

void updatestrings()
{
    int iv;

    if (strings_frame) {
	xv_set(strings_font_item, PANEL_VALUE, string_font, NULL);
	xv_set(strings_pen_item, PANEL_VALUE, string_color, NULL);
	xv_set(strings_linew_item, PANEL_VALUE, string_linew - 1, NULL);
	iv = (int) (100 * string_size);
	xv_set(strings_size_item, PANEL_VALUE, iv, NULL);
	xv_set(strings_rot_item, PANEL_VALUE, string_rot, NULL);
	xv_set(strings_loc_item, PANEL_VALUE, string_loctype == VIEW ? 1 : 0, NULL);
	xv_set(strings_just_item, PANEL_VALUE, string_just, NULL);
    }
}

void update_lines()
{
    int iv;

    if (lines_frame) {
	xv_set(lines_pen_item, PANEL_VALUE, line_color, NULL);
	xv_set(lines_style_item, PANEL_VALUE, line_lines - 1, NULL);
	xv_set(lines_width_item, PANEL_VALUE, line_linew - 1, NULL);
	xv_set(lines_arrow_item, PANEL_VALUE, line_arrow, NULL);
	xv_set(lines_arrowhead_item, PANEL_VALUE, line_atype, NULL);
	iv = (int) (50 * line_asize);
	xv_set(lines_asize_item, PANEL_VALUE, iv, NULL);
	xv_set(lines_loc_item, PANEL_VALUE, line_loctype == VIEW ? 1 : 0, NULL);
    }
}

void update_boxes()
{
    if (boxes_frame) {
	xv_set(boxes_pen_item, PANEL_VALUE, box_color, NULL);
	xv_set(boxes_lines_item, PANEL_VALUE, box_lines - 1, NULL);
	xv_set(boxes_linew_item, PANEL_VALUE, box_linew - 1, NULL);
	switch (box_fill) {
	case NONE:
	    xv_set(boxes_fill_item, PANEL_VALUE, 0, NULL);
	    break;
	case COLOR:
	    xv_set(boxes_fill_item, PANEL_VALUE, 1, NULL);
	    break;
	case PATTERN:
	    xv_set(boxes_fill_item, PANEL_VALUE, 2, NULL);
	    break;
	}
	xv_set(boxes_fillpat_item, PANEL_VALUE, box_fillpat - 1, NULL);
	xv_set(boxes_fillcol_item, PANEL_VALUE, box_fillcolor, NULL);
	xv_set(boxes_loc_item, PANEL_VALUE, box_loctype == VIEW ? 1 : 0, NULL);
    }
}

static void strings_done_proc()
{
    xv_set(strings_frame, WIN_SHOW, FALSE, 0);
}

static void lines_done_proc()
{
    xv_set(lines_frame, WIN_SHOW, FALSE, 0);
}

static void boxes_done_proc()
{
    xv_set(boxes_frame, WIN_SHOW, FALSE, 0);
}

void define_strings_popup()
{
    if (strings_frame) {
	updatestrings();
	xv_set(strings_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    strings_frame = xv_create(main_frame, FRAME,
			      XV_LABEL, "Strings",
			      FRAME_SHOW_LABEL, TRUE,
			      WIN_ERROR_MSG, "Couldn't create strings_frame",
			      NULL);
    strings_panel = xv_create(strings_frame, PANEL,
			      PANEL_LAYOUT, PANEL_VERTICAL,
			      NULL);

    strings_font_item = (Panel_item) xv_create(strings_panel, PANEL_CHOICE_STACK,
					       PANEL_LABEL_STRING, "Font:",
					       PANEL_CHOICE_STRINGS,
				"Times-Roman", "Times-Bold", "Times-Italic",
					    "Times-BoldItalic", "Helvetica",
				      "Helvetica-Bold", "Helvetica-Oblique",
				 "Helvetica-BoldOblique", "Greek", "Symbol",
					       NULL,
				   PANEL_VALUE_X, xv_col(strings_panel, 16),
					       NULL);

    strings_pen_item = (Panel_item) xv_create(strings_panel, PANEL_CHOICE_STACK,
					      PANEL_LABEL_STRING, "Color:",
					      PANEL_CHOICE_NCOLS, 4,
					      PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
					 "10", "11", "12", "13", "14", "15",
					      NULL,
				   PANEL_VALUE_X, xv_col(strings_panel, 16),
					      NULL);

    strings_linew_item = (Panel_item) xv_create(strings_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line width:",
						PANEL_CHOICE_NCOLS, 3,
						PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
						NULL,
				   PANEL_VALUE_X, xv_col(strings_panel, 16),
						NULL);

    strings_just_item = (Panel_item) xv_create(strings_panel, PANEL_CHOICE_STACK,
				       PANEL_LABEL_STRING, "Justification:",
					       PANEL_CHOICE_STRINGS,
					       "Left",
					       "Right",
					       "Centered",
					       NULL,
				   PANEL_VALUE_X, xv_col(strings_panel, 16),
					       NULL);

    strings_loc_item = (Panel_item) xv_create(strings_panel, PANEL_CHOICE_STACK,
					 PANEL_LABEL_STRING, "Position in:",
					      PANEL_CHOICE_STRINGS,
					      "World coordinates",
					      "Viewport coordinates",
					      NULL,
				   PANEL_VALUE_X, xv_col(strings_panel, 16),
					      NULL);

    strings_rot_item = (Panel_item) xv_create(strings_panel, PANEL_SLIDER,
					      PANEL_SLIDER_WIDTH, 160,
					      PANEL_SHOW_VALUE, TRUE,
					      PANEL_SHOW_RANGE, FALSE,
					      PANEL_MIN_VALUE, 0,
					      PANEL_MAX_VALUE, 360,
					    PANEL_LABEL_STRING, "Rotation:",
				   PANEL_VALUE_X, xv_col(strings_panel, 16),
					      NULL);

    strings_size_item = (Panel_item) xv_create(strings_panel, PANEL_SLIDER,
					       PANEL_SLIDER_WIDTH, 100,
					       PANEL_SHOW_VALUE, TRUE,
					       PANEL_SHOW_RANGE, FALSE,
					       PANEL_MIN_VALUE, 0,
					       PANEL_MAX_VALUE, 400,
				      PANEL_LABEL_STRING, "Character size:",
				   PANEL_VALUE_X, xv_col(strings_panel, 16),
					       NULL);
    (void) xv_create(strings_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, strings_done_proc,
		     XV_X, xv_col(strings_panel, 15),
		     XV_Y, xv_row(strings_panel, 8),
		     NULL);
    (void) xv_create(strings_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, define_string_defaults,
		     XV_X, xv_col(strings_panel, 5),
		     XV_Y, xv_row(strings_panel, 8),
		     NULL);
    updatestrings();
    window_fit(strings_panel);
    window_fit(strings_frame);
    xv_set(strings_frame, WIN_SHOW, TRUE, 0);
}

void define_lines_popup()
{
    if (lines_frame) {
	update_lines();
	xv_set(lines_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    lines_frame = xv_create(main_frame, FRAME,
			    XV_LABEL, "Lines",
			    FRAME_SHOW_LABEL, TRUE,
			    WIN_ERROR_MSG, "Couldn't create lines_frame",
			    NULL);
    lines_panel = xv_create(lines_frame, PANEL,
			    PANEL_LAYOUT, PANEL_VERTICAL,
			    NULL);

    lines_pen_item = (Panel_item) xv_create(lines_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Color:",
					    PANEL_CHOICE_NCOLS, 4,
					    PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
					 "10", "11", "12", "13", "14", "15",
					    NULL,
				     PANEL_VALUE_X, xv_col(lines_panel, 20),
					    NULL);

    lines_width_item = (Panel_item) xv_create(lines_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line width:",
					      PANEL_CHOICE_NCOLS, 3,
					      PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					      NULL,
				     PANEL_VALUE_X, xv_col(lines_panel, 20),
					      NULL);

    lines_style_item = (Panel_item) xv_create(lines_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line style:",
					      PANEL_CHOICE_STRINGS,
					      "Solid line",
					      "Dotted line",
					      "Dashed line",
					      "Long Dashed",
					      "Dot-dashed",
					      NULL,
				     PANEL_VALUE_X, xv_col(lines_panel, 20),
					      NULL);
    lines_arrow_item = (Panel_item) xv_create(lines_panel, PANEL_CHOICE_STACK,
					      PANEL_LABEL_STRING, "Arrow:",
					      PANEL_CHOICE_STRINGS,
					      "None",
					      "At start",
					      "At end",
					      "Both ends",
					      NULL,
				     PANEL_VALUE_X, xv_col(lines_panel, 20),
					      NULL);

    lines_arrowhead_item = (Panel_item) xv_create(lines_panel, PANEL_CHOICE_STACK,
				    PANEL_LABEL_STRING, "Arrow head style:",
						  PANEL_CHOICE_STRINGS,
						  "Line",
						  "Filled",
						  "Hollow",
						  NULL,
				     PANEL_VALUE_X, xv_col(lines_panel, 20),
						  NULL);

    lines_loc_item = (Panel_item) xv_create(lines_panel, PANEL_CHOICE_STACK,
					 PANEL_LABEL_STRING, "Position in:",
					    PANEL_CHOICE_STRINGS,
					    "World coordinates",
					    "Viewport coordinates",
					    NULL,
				     PANEL_VALUE_X, xv_col(lines_panel, 20),
					    NULL);

    lines_asize_item = (Panel_item) xv_create(lines_panel, PANEL_SLIDER,
					      PANEL_SLIDER_WIDTH, 100,
					      PANEL_SHOW_VALUE, TRUE,
					      PANEL_SHOW_RANGE, FALSE,
					      PANEL_MIN_VALUE, 0,
					      PANEL_MAX_VALUE, 400,
					  PANEL_LABEL_STRING, "Arrow size:",
				     PANEL_VALUE_X, xv_col(lines_panel, 20),
					      NULL);
    (void) xv_create(lines_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, lines_done_proc,
		     XV_X, xv_col(lines_panel, 15),
		     XV_Y, xv_row(lines_panel, 9),
		     NULL);
    (void) xv_create(lines_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, lines_def_proc,
		     XV_X, xv_col(lines_panel, 5),
		     XV_Y, xv_row(lines_panel, 9),
		     NULL);
    update_lines();
    window_fit(lines_panel);
    window_fit(lines_frame);
    xv_set(lines_frame, WIN_SHOW, TRUE, 0);
}

void define_boxes_popup()
{
    if (boxes_frame) {
	update_boxes();
	xv_set(boxes_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    boxes_frame = xv_create(main_frame, FRAME,
			    XV_LABEL, "Boxes",
			    FRAME_SHOW_LABEL, TRUE,
			    WIN_ERROR_MSG, "Couldn't create boxes_frame",
			    NULL);
    boxes_panel = xv_create(boxes_frame, PANEL,
			    PANEL_LAYOUT, PANEL_VERTICAL,
			    NULL);

    boxes_pen_item = (Panel_item) xv_create(boxes_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Color:",
					    PANEL_CHOICE_NCOLS, 4,
					    PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
					 "10", "11", "12", "13", "14", "15",
					    NULL,
				     PANEL_VALUE_X, xv_col(boxes_panel, 16),
					    NULL);

    boxes_linew_item = (Panel_item) xv_create(boxes_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line width:",
					      PANEL_CHOICE_NCOLS, 3,
					      PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					      NULL,
				     PANEL_VALUE_X, xv_col(boxes_panel, 16),
					      NULL);

    boxes_lines_item = (Panel_item) xv_create(boxes_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line style:",
					      PANEL_CHOICE_STRINGS,
					      "Solid line",
					      "Dotted line",
					      "Dashed line",
					      "Long Dashed",
					      "Dot-dashed",
					      NULL,
				     PANEL_VALUE_X, xv_col(boxes_panel, 16),
					      NULL);
    boxes_fill_item = (Panel_item) xv_create(boxes_panel, PANEL_CHOICE_STACK,
					     PANEL_LABEL_STRING, "Fill:",
					     PANEL_CHOICE_STRINGS,
					     "None",
					     "Color",
					     "Pattern",
					     NULL,
				     PANEL_VALUE_X, xv_col(boxes_panel, 16),
					     NULL);
    boxes_fillpat_item = (Panel_item) xv_create(boxes_panel, PANEL_CHOICE_STACK,
					     PANEL_LABEL_STRING, "Pattern:",
						PANEL_CHOICE_NCOLS, 4,
						PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
					 "10", "11", "12", "13", "14", "15",
						NULL,
				     PANEL_VALUE_X, xv_col(boxes_panel, 16),
						NULL);

    boxes_fillcol_item = (Panel_item) xv_create(boxes_panel, PANEL_CHOICE_STACK,
						PANEL_LABEL_STRING, "Color:",
						PANEL_CHOICE_NCOLS, 4,
						PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
					 "10", "11", "12", "13", "14", "15",
						NULL,
				     PANEL_VALUE_X, xv_col(boxes_panel, 16),
						NULL);

    boxes_loc_item = (Panel_item) xv_create(boxes_panel, PANEL_CHOICE_STACK,
					 PANEL_LABEL_STRING, "Position in:",
					    PANEL_CHOICE_STRINGS,
					    "World coordinates",
					    "Viewport coordinates",
					    NULL,
				     PANEL_VALUE_X, xv_col(boxes_panel, 16),
					    NULL);
    (void) xv_create(boxes_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, boxes_done_proc,
		     XV_X, xv_col(boxes_panel, 15),
		     XV_Y, xv_row(boxes_panel, 9),
		     NULL);
    (void) xv_create(boxes_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, boxes_def_proc,
		     XV_X, xv_col(boxes_panel, 5),
		     XV_Y, xv_row(boxes_panel, 9),
		     NULL);
    update_boxes();
    window_fit(boxes_panel);
    window_fit(boxes_frame);
    xv_set(boxes_frame, WIN_SHOW, TRUE, 0);
}
