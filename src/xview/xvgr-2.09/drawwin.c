/* $Id: drawwin.c,v 1.7 92/07/16 05:54:24 pturner Exp Locker: pturner $
 *
 * Set drawing and scrolling options
 *
 */
#include <stdio.h>
#include <math.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

extern Frame main_frame;

Frame draw_frame = (Frame) NULL;
Panel draw_panel;

/*
 * Panel item declarations
 */
Panel_item autoredraw_type_item;
Panel_item autoclear_type_item;
Panel_item cursor_type_item;
Panel_item scrollper_item;
Panel_item linkscroll_item;
Panel_item linkzoom_item;

/*
 * scroll amount declared in graphutils.c TODO - move to globals.h
 */
extern int scrolling_islinked;
extern double scrollper;
extern double shexper;

/* suppress clear in the drivers - TODO move to globals.h */
int overlay = 0;

/* TODO move to globals.h - defined in events.c */
extern int cursortype;

/*
 * Event and Notify proc declarations
 */
static void draw_Done_notify_proc();
static void draw_define_notify_proc();

void update_draw()
{
    int iv;

    if (draw_frame) {
	xv_set(linkscroll_item, PANEL_VALUE, scrolling_islinked == TRUE, NULL);
	xv_set(autoredraw_type_item, PANEL_VALUE, auto_redraw == TRUE, NULL);
	xv_set(autoclear_type_item, PANEL_VALUE, overlay == TRUE, NULL);
	xv_set(cursor_type_item, PANEL_VALUE, cursortype == TRUE, NULL);
	iv = (int) (100 * scrollper);
	xv_set(scrollper_item, PANEL_VALUE, iv, NULL);
    }
}

/*
 * define the draw options
 */
static void define_draw_proc()
{
    int value, otmp = overlay;

    scrolling_islinked = (int) xv_get(linkscroll_item, PANEL_VALUE);
    auto_redraw = (int) xv_get(autoredraw_type_item, PANEL_VALUE);
    overlay = (int) xv_get(autoclear_type_item, PANEL_VALUE);
    cursortype = (int) xv_get(cursor_type_item, PANEL_VALUE);
    value = (int) xv_get(scrollper_item, PANEL_VALUE);
    scrollper = value / 100.0;
    xv_set(draw_frame, WIN_SHOW, FALSE, 0);
    if (otmp != overlay) {
	drawgraph();
    }
}

/*
 * Create the draw Frame and the draw Panel
 */
void create_draw_frame()
{
    if (draw_frame) {
	update_draw();
	xv_set(draw_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    draw_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Draw options",
				   NULL);
    draw_panel = (Panel) xv_create(draw_frame, PANEL,
				   XV_HELP_DATA, "xvgr:draw_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);

    scrollper_item = (Panel_item) xv_create(draw_panel, PANEL_SLIDER,
				    XV_HELP_DATA, "xvgr:draw_scrollpercent",
					    PANEL_SLIDER_WIDTH, 100,
					    PANEL_SHOW_VALUE, TRUE,
					    PANEL_SHOW_RANGE, FALSE,
					    PANEL_MIN_VALUE, 0,
					    PANEL_MAX_VALUE, 400,
					    PANEL_LABEL_STRING, "Scroll %:",
				      PANEL_VALUE_X, xv_col(draw_panel, 10),
					    XV_Y, xv_row(draw_panel, 0),
					    NULL);

    linkscroll_item = (Panel_item) xv_create(draw_panel, PANEL_CHECK_BOX,
				     XV_HELP_DATA, "xvgr:draw_linkedscroll",
					     PANEL_CHOICE_STRINGS,
					     "Linked scrolling",
					     NULL,
				      PANEL_VALUE_X, xv_col(draw_panel, 10),
					     XV_Y, xv_row(draw_panel, 1),
					     NULL);

    linkzoom_item = (Panel_item) xv_create(draw_panel, PANEL_CHECK_BOX,
				       XV_HELP_DATA, "xvgr:draw_linkedzoom",
					   PANEL_CHOICE_STRINGS,
					   "*Linked zoom",
					   NULL,
				      PANEL_VALUE_X, xv_col(draw_panel, 10),
					   XV_Y, xv_row(draw_panel, 2),
					   NULL);
    autoredraw_type_item = (Panel_item) xv_create(draw_panel, PANEL_CHECK_BOX,
				       XV_HELP_DATA, "xvgr:draw_autoredraw",
						  PANEL_CHOICE_STRINGS,
						  "Auto redraw",
						  NULL,
				      PANEL_VALUE_X, xv_col(draw_panel, 10),
						XV_Y, xv_row(draw_panel, 3),
						  NULL);
    autoclear_type_item = (Panel_item) xv_create(draw_panel, PANEL_CHECK_BOX,
					  XV_HELP_DATA, "xvgr:draw_overlay",
						 PANEL_CHOICE_STRINGS,
						 "Overlay",
						 NULL,
				      PANEL_VALUE_X, xv_col(draw_panel, 10),
						 XV_Y, xv_row(draw_panel, 4),
						 NULL);
    cursor_type_item = (Panel_item) xv_create(draw_panel, PANEL_CHECK_BOX,
					  XV_HELP_DATA, "xvgr:draw_cursor",
						 PANEL_CHOICE_STRINGS,
						 "Crosshair cursor",
						 NULL,
				      PANEL_VALUE_X, xv_col(draw_panel, 10),
						 XV_Y, xv_row(draw_panel, 5),
						 NULL);
    (void) xv_create(draw_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:draw_cancel",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, draw_Done_notify_proc,
		     XV_X, xv_col(draw_panel, 15),
		     XV_Y, xv_row(draw_panel, 6),
		     NULL);
    (void) xv_create(draw_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:draw_accept",
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, define_draw_proc,
		     XV_X, xv_col(draw_panel, 5),
		     XV_Y, xv_row(draw_panel, 6),
		     NULL);
    update_draw();
    window_fit(draw_panel);
    window_fit(draw_frame);
    xv_set(draw_frame, WIN_SHOW, TRUE, NULL);
}				/* end create_draw_panel */

static void draw_Done_notify_proc()
{
    xv_set(draw_frame, WIN_SHOW, FALSE, NULL);
}
