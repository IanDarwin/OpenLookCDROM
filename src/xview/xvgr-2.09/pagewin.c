/* $Id: pagewin.c,v 1.6 92/08/15 15:55:05 pturner Exp Locker: pturner $
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

static Frame page_frame;
static Panel page_panel;

/*
 * Panel item declarations
 */
static Panel_item page_layout_item;
static Panel_item page_scrollx_item;
static Panel_item page_scrolly_item;
static Panel_item page_x_item;
static Panel_item page_y_item;

/*
 * Event and Notify proc declarations
 */
static int page_Done_notify_proc();
static int page_define_notify_proc();

void update_page_items()
{
    int iv;
    if (page_frame) {
	xv_set(page_layout_item, PANEL_VALUE, page_layout, NULL);
    }
}

/*
 * Create the page Frame and the page Panel
 */
void create_page_frame()
{

    if (page_frame) {
	 update_page_items();
	xv_set(page_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    page_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Page layout",
				   NULL);
    page_panel = (Panel) xv_create(page_frame, PANEL,
				   XV_HELP_DATA, "xvgr:page_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    page_layout_item = (Panel_item) xv_create(page_panel, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:page_layout",
					 PANEL_LABEL_STRING, "Page layout:",
					      PANEL_CHOICE_STRINGS,
					      "Free",
					      "Landscape (8.5x11)",
					      "Portrait (11x8.5)",
					      "Custom",
					      NULL,
				      PANEL_VALUE_X, xv_col(page_panel, 15),
					      NULL);
    (void) xv_create(page_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:page_accept",
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, page_define_notify_proc,
		     XV_X, xv_col(page_panel, 1),
		     XV_Y, xv_row(page_panel, 2),
		     NULL);
    (void) xv_create(page_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:page_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, page_Done_notify_proc,
		     XV_X, xv_col(page_panel, 12),
		     XV_Y, xv_row(page_panel, 2),
		     NULL);
    window_fit(page_panel);
    window_fit(page_frame);
    update_page_items();
    xv_set(page_frame, WIN_SHOW, TRUE, 0);
}				/* end create_page_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int page_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(page_frame, WIN_SHOW, FALSE, 0);
}

/*ARGSUSED*/
static int page_define_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int value, width, height;
    extern Canvas canvas;
    extern Display *disp;
    static Scrollbar h, v;
    double wx1, wx2, wy1, wy2;
    wx1 = DisplayWidth(disp, DefaultScreen(disp));
    wx2 = DisplayWidthMM(disp, DefaultScreen(disp));
    wy1 = DisplayHeight(disp, DefaultScreen(disp));
    wy2 = DisplayHeightMM(disp, DefaultScreen(disp));
    width  = 25.4 * (wx1 / wx2);
    height = 25.4 * (wy1 / wy2);

    value = (int) xv_get(page_layout_item, PANEL_VALUE);
    if (page_layout != value) {
	page_layout = value;
	switch (page_layout) {
	case 0:
	    xv_set(canvas,
		   CANVAS_AUTO_EXPAND, TRUE,
		   CANVAS_AUTO_SHRINK, TRUE,
		   CANVAS_AUTO_CLEAR, TRUE,
		   NULL);
	    xv_destroy(h);
	    xv_destroy(v);
	    h = (Scrollbar) 0;
	    v = (Scrollbar) 0;
	    window_fit(main_frame);
	    break;
	case 1:
	    xv_set(canvas,
		   CANVAS_WIDTH, (int) (width * 11.5),
		   CANVAS_HEIGHT, (int) (height * 8.5),
		   CANVAS_AUTO_EXPAND, FALSE,
		   CANVAS_AUTO_SHRINK, FALSE,
		   CANVAS_AUTO_CLEAR, TRUE,
		   NULL);
	    if (!h) {
		h = (Scrollbar) xv_create(canvas, SCROLLBAR,
				  SCROLLBAR_DIRECTION, SCROLLBAR_HORIZONTAL,
					  NULL);
		v = (Scrollbar) xv_create(canvas, SCROLLBAR,
				    SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
					  NULL);
	    }
	    window_fit(main_frame);
	    break;
	case 2:
	    xv_set(canvas,
		   CANVAS_WIDTH, (int) (width * 8.5),
		   CANVAS_HEIGHT, (int) (height * 11.5),
		   CANVAS_AUTO_EXPAND, FALSE,
		   CANVAS_AUTO_SHRINK, FALSE,
		   CANVAS_AUTO_CLEAR, TRUE,
		   NULL);
	    if (!h) {
		h = (Scrollbar) xv_create(canvas, SCROLLBAR,
				  SCROLLBAR_DIRECTION, SCROLLBAR_HORIZONTAL,
					  NULL);
		v = (Scrollbar) xv_create(canvas, SCROLLBAR,
				    SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
					  NULL);
	    }
	    window_fit(main_frame);
	    break;
	}
    }
    drawgraph();
    return XV_OK;
}
