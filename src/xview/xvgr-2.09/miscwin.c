/* $Id: miscwin.c,v 1.1 92/08/15 15:55:07 pturner Exp Locker: pturner $
 *
 * frame Panel
 *
 */
#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>

#include "globals.h"

extern Frame main_frame;

static Frame misc_frame;
static Panel misc_panel;

/*
 * Panel item declarations
 */
static Panel_item timestamp_active_item;
static Panel_item timestamp_font_item;
static Panel_item timestamp_size_item;
static Panel_item timestamp_color_item;
static Panel_item timestamp_linew_item;
Panel_item timestamp_x_item;
Panel_item timestamp_y_item;

/*
 * Event and Notify proc declarations
 */
static int misc_Done_notify_proc();
static int misc_define_notify_proc();

void update_misc_items()
{
    int iv;
    if (misc_frame) {
	xv_set(timestamp_active_item, PANEL_VALUE, timestamp.active == ON, NULL);
	xv_set(timestamp_font_item, PANEL_VALUE, timestamp.font, NULL);
	xv_set(timestamp_color_item, PANEL_VALUE, timestamp.color, NULL);
	xv_set(timestamp_linew_item, PANEL_VALUE, timestamp.linew - 1, NULL);
	iv = (int) (100 * timestamp.charsize);
	xv_set(timestamp_size_item, PANEL_VALUE, iv, NULL);
	sprintf(buf, "%lg", timestamp.x);
	xv_set(timestamp_x_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%lg", timestamp.y);
	xv_set(timestamp_y_item, PANEL_VALUE, buf, NULL);
    }
}

static void misc_place_notify_proc()
{
     set_action(0);
    set_action(PLACE_TIMESTAMP);
}

/*
 * Create the page Frame and the page Panel
 */
void create_misc_frame()
{

    if (misc_frame) {
	 update_misc_items();
	xv_set(misc_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    misc_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Misc.",
				   NULL);
    misc_panel = (Panel) xv_create(misc_frame, PANEL,
				   XV_HELP_DATA, "xvgr:misc_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    timestamp_active_item = (Panel_item) xv_create(misc_panel, PANEL_CHOICE_STACK,
				       XV_HELP_DATA, "xvgr:timestamp_onoff",
					  PANEL_LABEL_STRING, "Time stamp:",
						   PANEL_CHOICE_STRINGS,
						   "OFF",
						   "ON",
						   NULL,
				      PANEL_VALUE_X, xv_col(misc_panel, 15),
						   NULL);
    timestamp_font_item = (Panel_item) xv_create(misc_panel, PANEL_CHOICE_STACK,
						 PANEL_LABEL_STRING, "Font:",
						 PANEL_CHOICE_STRINGS,
				"Times-Roman", "Times-Bold", "Times-Italic",
					    "Times-BoldItalic", "Helvetica",
				      "Helvetica-Bold", "Helvetica-Oblique",
				 "Helvetica-BoldOblique", "Greek", "Symbol",
						 NULL,
				      PANEL_VALUE_X, xv_col(misc_panel, 14),
						 NULL);
    timestamp_color_item = (Panel_item) xv_create(misc_panel, PANEL_CHOICE_STACK,
					    XV_HELP_DATA, "xvgr:misc_color",
						  PANEL_CHOICE_NCOLS, 4,
					       PANEL_LABEL_STRING, "Color:",
						  PANEL_CHOICE_STRINGS,
				"0", "1", "2", "3", "4", "5", "6", "7", "8",
				    "9", "10", "11", "12", "13", "14", "15",
						  NULL,
				      PANEL_VALUE_X, xv_col(misc_panel, 15),
						  NULL);
    timestamp_linew_item = (Panel_item) xv_create(misc_panel, PANEL_CHOICE_STACK,
					    XV_HELP_DATA, "xvgr:misc_linew",
					  PANEL_LABEL_STRING, "Line width:",
						  PANEL_CHOICE_NCOLS, 3,
						  PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
						  NULL,
				      PANEL_VALUE_X, xv_col(misc_panel, 15),
						  NULL);

    timestamp_size_item = (Panel_item) xv_create(misc_panel, PANEL_SLIDER,
						 PANEL_SLIDER_WIDTH, 100,
						 PANEL_SHOW_VALUE, TRUE,
						 PANEL_SHOW_RANGE, FALSE,
						 PANEL_MIN_VALUE, 0,
						 PANEL_MAX_VALUE, 400,
					   PANEL_LABEL_STRING, "Char size:",
				      PANEL_VALUE_X, xv_col(misc_panel, 15),
						 NULL);
    timestamp_x_item = (Panel_item) xv_create(misc_panel, PANEL_TEXT,
					   XV_HELP_DATA, "xvgr:timestamp_x",
					      PANEL_LAYOUT, PANEL_HORIZONTAL,
					      PANEL_VALUE_DISPLAY_LENGTH, 10,
					 PANEL_LABEL_STRING, "Timestamp X:",
				      PANEL_VALUE_X, xv_col(misc_panel, 15),
					      NULL);
    timestamp_y_item = (Panel_item) xv_create(misc_panel, PANEL_TEXT,
					   XV_HELP_DATA, "xvgr:timestamp_y",
					      PANEL_LAYOUT, PANEL_HORIZONTAL,
					      PANEL_VALUE_DISPLAY_LENGTH, 10,
					 PANEL_LABEL_STRING, "Timestamp Y:",
				      PANEL_VALUE_X, xv_col(misc_panel, 15),
					      NULL);
    (void) xv_create(misc_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:misc_accept",
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, misc_define_notify_proc,
		     XV_X, xv_col(misc_panel, 1),
		     XV_Y, xv_row(misc_panel, 9),
		     NULL);
    (void) xv_create(misc_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:misc_place",
		     PANEL_LABEL_STRING, "Place timestamp",
		     PANEL_NOTIFY_PROC, misc_place_notify_proc,
		     XV_X, xv_col(misc_panel, 10),
		     XV_Y, xv_row(misc_panel, 9),
		     NULL);
    (void) xv_create(misc_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:misc_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, misc_Done_notify_proc,
		     XV_X, xv_col(misc_panel, 27),
		     XV_Y, xv_row(misc_panel, 9),
		     NULL);
    window_fit(misc_panel);
    window_fit(misc_frame);
    update_misc_items();
    xv_set(misc_frame, WIN_SHOW, TRUE, 0);
}				/* end create_misc_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int misc_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(misc_frame, WIN_SHOW, FALSE, 0);
}

/*ARGSUSED*/
static int misc_define_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int value;
    timestamp.active = (int) xv_get(timestamp_active_item, PANEL_VALUE) ? ON : OFF;
    timestamp.font = (int) xv_get(timestamp_font_item, PANEL_VALUE);
    timestamp.color = (int) xv_get(timestamp_color_item, PANEL_VALUE);
    timestamp.linew = (int) xv_get(timestamp_linew_item, PANEL_VALUE) + 1;
    value = (int) xv_get(timestamp_size_item, PANEL_VALUE);
    timestamp.charsize = value / 100.0;
    timestamp.x = atof((char *) xv_get(timestamp_x_item, PANEL_VALUE));
    timestamp.y = atof((char *) xv_get(timestamp_y_item, PANEL_VALUE));
    drawgraph();
    return XV_OK;
}
