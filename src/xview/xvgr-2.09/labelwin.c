/* $Id: labelwin.c,v 1.8 92/06/28 16:29:04 pturner Exp Locker: pturner $
 *
 * title/subtitle popup
 *
 */

#include <stdio.h>

#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

extern Frame main_frame;

Frame label_frame = (Frame) NULL;
static Panel label_panel;

Frame labelprops_frame = (Frame) NULL;
static Panel labelprops_panel;

/*
 * Panel item declarations
 */
static Panel_item label_title_text_item;
static Panel_item label_subtitle_text_item;
static Panel_item title_color_item;
static Panel_item title_linew_item;
static Panel_item title_font_item;
static Panel_item title_size_item;
static Panel_item stitle_color_item;
static Panel_item stitle_linew_item;
static Panel_item stitle_font_item;
static Panel_item stitle_size_item;

/*
 * Event and Notify proc declarations
 */
static label_Done_notify_proc();
static label_props_notify_proc();
static label_define_notify_proc();
static label_title_notify_proc();
static label_subtitle_notify_proc();

void update_label_proc()
{
    if (label_frame) {
	xv_setstr(label_title_text_item, g[cg].labs.title.s);
	xv_setstr(label_subtitle_text_item, g[cg].labs.stitle.s);
    }
}

/*
 * Create the label Frame and the label Panel
 */
void create_label_frame()
{
    if (label_frame) {
	update_label_proc();
	xv_set(label_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    label_frame = xv_create(main_frame, FRAME,
			    XV_LABEL, "Title/subtitle",
			    FRAME_SHOW_LABEL, TRUE,
			    WIN_ERROR_MSG, "Couldn't create label_frame",
			    WIN_Y, 0,
			    0);
    label_panel = xv_create(label_frame, PANEL,
			    PANEL_LAYOUT, PANEL_VERTICAL,
			    0);

    label_title_text_item = (Panel_item) xv_create(label_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 25,
					       PANEL_LABEL_STRING, "Title:",
				     PANEL_VALUE_X, xv_col(label_panel, 11),
						   NULL);
    label_subtitle_text_item = (Panel_item) xv_create(label_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 25,
					    PANEL_LABEL_STRING, "Subtitle:",
				     PANEL_VALUE_X, xv_col(label_panel, 11),
						      NULL);

    (void) xv_create(label_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, label_define_notify_proc,
		     XV_X, xv_col(label_panel, 1),
		     XV_Y, xv_row(label_panel, 2),
		     NULL);
    (void) xv_create(label_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Props...",
		     PANEL_NOTIFY_PROC, label_props_notify_proc,
		     XV_X, xv_col(label_panel, 11),
		     XV_Y, xv_row(label_panel, 2),
		     NULL);
    (void) xv_create(label_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, label_Done_notify_proc,
		     XV_X, xv_col(label_panel, 22),
		     XV_Y, xv_row(label_panel, 2),
		     NULL);
    update_label_proc();
    window_fit(label_panel);
    window_fit(label_frame);
    xv_set(label_frame, WIN_SHOW, TRUE, 0);
}

static label_Done_notify_proc()
{
    xv_set(label_frame, WIN_SHOW, FALSE, 0);
}

static label_define_notify_proc()
{
    strcpy(g[cg].labs.title.s, (char *) xv_getstr(label_title_text_item));
    strcpy(g[cg].labs.stitle.s, (char *) xv_getstr(label_subtitle_text_item));
    drawgraph();
}

static labelprops_Done_notify_proc()
{
    xv_set(labelprops_frame, WIN_SHOW, FALSE, 0);
}

static labelprops_define_notify_proc()
{
    int value;

    g[cg].labs.title.font = (int) xv_get(title_font_item, PANEL_VALUE);
    g[cg].labs.title.color = (int) xv_get(title_color_item, PANEL_VALUE);
    g[cg].labs.title.linew = (int) xv_get(title_linew_item, PANEL_VALUE) + 1;
    value = (int) xv_get(title_size_item, PANEL_VALUE);
    g[cg].labs.title.charsize = value / 100.0;

    g[cg].labs.stitle.font = (int) xv_get(stitle_font_item, PANEL_VALUE);
    g[cg].labs.stitle.color = (int) xv_get(stitle_color_item, PANEL_VALUE);
    g[cg].labs.stitle.linew = (int) xv_get(stitle_linew_item, PANEL_VALUE) + 1;
    value = (int) xv_get(stitle_size_item, PANEL_VALUE);
    g[cg].labs.stitle.charsize = value / 100.0;
    drawgraph();
}

void update_labelprops_proc()
{
    int iv;

    if (labelprops_frame) {
	xv_set(title_font_item, PANEL_VALUE, g[cg].labs.title.font, NULL);
	xv_set(title_color_item, PANEL_VALUE, g[cg].labs.title.color, NULL);
	xv_set(title_linew_item, PANEL_VALUE, g[cg].labs.title.linew - 1, NULL);
	iv = (int) (100 * g[cg].labs.title.charsize);
	xv_set(title_size_item, PANEL_VALUE, iv, NULL);
	xv_set(stitle_font_item, PANEL_VALUE, g[cg].labs.stitle.font, NULL);
	xv_set(stitle_color_item, PANEL_VALUE, g[cg].labs.stitle.color, NULL);
	xv_set(stitle_linew_item, PANEL_VALUE, g[cg].labs.stitle.linew - 1, NULL);
	iv = (int) (100 * g[cg].labs.stitle.charsize);
	xv_set(stitle_size_item, PANEL_VALUE, iv, NULL);
    }
}

static label_props_notify_proc()
{
    if (labelprops_frame) {
	update_labelprops_proc();
	xv_set(labelprops_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    labelprops_frame = xv_create(main_frame, FRAME,
				 XV_LABEL, "Title/subtitle props",
				 FRAME_SHOW_LABEL, TRUE,
			  WIN_ERROR_MSG, "Couldn't create labelprops_frame",
				 WIN_Y, NULL,
				 NULL);
    labelprops_panel = xv_create(labelprops_frame, PANEL,
				 NULL);
    (void) xv_create(labelprops_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Title props:",
		     XV_X, xv_col(labelprops_panel, 14),
		     XV_Y, xv_row(labelprops_panel, 0),
		     NULL);
    (void) xv_create(labelprops_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Subtitle props:",
		     XV_X, xv_col(labelprops_panel, 46),
		     XV_Y, xv_row(labelprops_panel, 0),
		     NULL);

    title_font_item = (Panel_item) xv_create(labelprops_panel, PANEL_CHOICE_STACK,
					     PANEL_LABEL_STRING, "Font:",
					     PANEL_CHOICE_STRINGS,
				"Times-Roman", "Times-Bold", "Times-Italic",
					     "Times-BoldItalic", "Helvetica",
				      "Helvetica-Bold", "Helvetica-Oblique",
				 "Helvetica-BoldOblique", "Greek", "Symbol",
					     NULL,
				     PANEL_VALUE_X, xv_col(label_panel, 14),
					     XV_Y, xv_row(label_panel, 1),
					     NULL);

    title_color_item = (Panel_item) xv_create(labelprops_panel, PANEL_CHOICE_STACK,
					      PANEL_LABEL_STRING, "Color:",
					      PANEL_CHOICE_NCOLS, 4,
					      PANEL_CHOICE_STRINGS,
				"0", "1", "2", "3", "4", "5", "6", "7", "8",
				    "9", "10", "11", "12", "13", "14", "15",
					      NULL,
				     PANEL_VALUE_X, xv_col(label_panel, 14),
					      XV_Y, xv_row(label_panel, 2),
					      NULL);

    title_linew_item = (Panel_item) xv_create(labelprops_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line width:",
					      PANEL_CHOICE_NCOLS, 4,
					      PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					      NULL,
				     PANEL_VALUE_X, xv_col(label_panel, 14),
					      XV_Y, xv_row(label_panel, 3),
					      NULL);

    stitle_font_item = (Panel_item) xv_create(labelprops_panel, PANEL_CHOICE_STACK,
					      PANEL_LABEL_STRING, "Font:",
					      PANEL_CHOICE_STRINGS,
				"Times-Roman", "Times-Bold", "Times-Italic",
					    "Times-BoldItalic", "Helvetica",
				      "Helvetica-Bold", "Helvetica-Oblique",
				 "Helvetica-BoldOblique", "Greek", "Symbol",
					      NULL,
				     PANEL_VALUE_X, xv_col(label_panel, 46),
					      XV_Y, xv_row(label_panel, 1),
					      NULL);

    stitle_color_item = (Panel_item) xv_create(labelprops_panel, PANEL_CHOICE_STACK,
					       PANEL_LABEL_STRING, "Color:",
					       PANEL_CHOICE_NCOLS, 4,
					       PANEL_CHOICE_STRINGS,
				"0", "1", "2", "3", "4", "5", "6", "7", "8",
				    "9", "10", "11", "12", "13", "14", "15",
					       NULL,
				     PANEL_VALUE_X, xv_col(label_panel, 46),
					       XV_Y, xv_row(label_panel, 2),
					       NULL);

    stitle_linew_item = (Panel_item) xv_create(labelprops_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line width:",
					       PANEL_CHOICE_NCOLS, 3,
					       PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					       NULL,
				     PANEL_VALUE_X, xv_col(label_panel, 46),
					       XV_Y, xv_row(label_panel, 3),
					       NULL);

    title_size_item = (Panel_item) xv_create(labelprops_panel, PANEL_SLIDER,
					     PANEL_SLIDER_WIDTH, 100,
					     PANEL_SHOW_VALUE, TRUE,
					     PANEL_SHOW_RANGE, FALSE,
					     PANEL_MIN_VALUE, 0,
					     PANEL_MAX_VALUE, 400,
					   PANEL_LABEL_STRING, "Char size:",
				     PANEL_VALUE_X, xv_col(label_panel, 14),
					     XV_Y, xv_row(label_panel, 4),
					     NULL);

    stitle_size_item = (Panel_item) xv_create(labelprops_panel, PANEL_SLIDER,
					      PANEL_SLIDER_WIDTH, 100,
					      PANEL_SHOW_VALUE, TRUE,
					      PANEL_SHOW_RANGE, FALSE,
					      PANEL_MIN_VALUE, 0,
					      PANEL_MAX_VALUE, 400,
					   PANEL_LABEL_STRING, "Char size:",
				     PANEL_VALUE_X, xv_col(label_panel, 46),
					      XV_Y, xv_row(label_panel, 4),
					      NULL);

    (void) xv_create(labelprops_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, labelprops_Done_notify_proc,
		     XV_X, xv_col(label_panel, 15),
		     XV_Y, xv_row(label_panel, 5),
		     NULL);
    (void) xv_create(labelprops_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, labelprops_define_notify_proc,
		     XV_X, xv_col(label_panel, 5),
		     XV_Y, xv_row(label_panel, 5),
		     NULL);
    update_labelprops_proc();
    window_fit(labelprops_panel);
    window_fit(labelprops_frame);
    xv_set(labelprops_frame, WIN_SHOW, TRUE, 0);
}
