/* $Id: framewin.c,v 1.9 92/07/16 05:54:32 pturner Exp Locker: pturner $
 *
 * frame Panel
 *
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

Frame frame_frame = (Frame) 0;
Panel frame_panel;

/*
 * Panel item declarations
 */
Panel_item frame_frameactive_choice_item;
Panel_item frame_framestyle_choice_item;
Panel_item frame_color_choice_item;
Panel_item frame_lines_choice_item;
Panel_item frame_linew_choice_item;
Panel_item frame_fillbg_choice_item;
Panel_item frame_bgcolor_choice_item;

/*
 * Event and Notify proc declarations
 */
static int frame_Done_notify_proc();
static int frame_define_notify_proc();
static void frame_framestyle_notify_proc();
static void frame_pen_notify_proc();
static void frame_line_notify_proc();

void update_frame_items(gno)
{

    if (frame_frame) {
	xv_set(frame_frameactive_choice_item, PANEL_VALUE, g[gno].f.active == OFF, NULL);
	xv_set(frame_framestyle_choice_item, PANEL_VALUE, g[gno].f.type, NULL);
	xv_set(frame_color_choice_item, PANEL_VALUE, g[gno].f.color, NULL);
	xv_set(frame_linew_choice_item, PANEL_VALUE, g[gno].f.linew - 1, NULL);
	xv_set(frame_lines_choice_item, PANEL_VALUE, g[gno].f.lines - 1, NULL);
	xv_set(frame_fillbg_choice_item, PANEL_VALUE, g[gno].f.fillbg == ON, NULL);
	xv_set(frame_bgcolor_choice_item, PANEL_VALUE, g[gno].f.bgcolor, NULL);
    }
}

/*
 * Create the frame Frame and the frame Panel
 */
void create_frame_frame()
{
    extern Frame main_frame;

    if (frame_frame) {
	update_frame_items(cg);
	xv_set(frame_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    frame_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "frame",
				    NULL);
    frame_panel = (Panel) xv_create(frame_frame, PANEL,
				    XV_HELP_DATA, "xvgr:frame_panel",
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    frame_frameactive_choice_item = (Panel_item) xv_create(frame_panel, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:frame_onoff",
					       PANEL_LABEL_STRING, "Frame:",
						       PANEL_CHOICE_STRINGS,
							   "ON",
							   "OFF",
							   NULL,
				     PANEL_VALUE_X, xv_col(frame_panel, 15),
							   NULL);
    frame_framestyle_choice_item = (Panel_item) xv_create(frame_panel, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:frame_style",
					       PANEL_LABEL_STRING, "Style:",
						       PANEL_CHOICE_STRINGS,
							  "Closed",
							  "Half open",
							  NULL,
				     PANEL_VALUE_X, xv_col(frame_panel, 15),
							  NULL);
    frame_color_choice_item = (Panel_item) xv_create(frame_panel, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:frame_color",
						     PANEL_CHOICE_NCOLS, 4,
					       PANEL_LABEL_STRING, "Color:",
						     PANEL_CHOICE_STRINGS,
				"0", "1", "2", "3", "4", "5", "6", "7", "8",
			"9", "10", "11", "12", "13", "14", "15",
						     NULL,
				     PANEL_VALUE_X, xv_col(frame_panel, 15),
						     NULL);
    frame_linew_choice_item = (Panel_item) xv_create(frame_panel, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:frame_linew",
					  PANEL_LABEL_STRING, "Line width:",
						     PANEL_CHOICE_NCOLS, 3,
						     PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
						     NULL,
				     PANEL_VALUE_X, xv_col(frame_panel, 15),
						     NULL);
    frame_lines_choice_item = (Panel_item) xv_create(frame_panel, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:frame_lines",
					  PANEL_LABEL_STRING, "Line style:",
						     PANEL_CHOICE_STRINGS,
						     "Solid line",
						     "Dotted line",
						     "Dashed line",
						     "Long Dashed",
						     "Dot-dashed",
						     NULL,
				     PANEL_VALUE_X, xv_col(frame_panel, 15),
						     NULL);
    frame_fillbg_choice_item = (Panel_item) xv_create(frame_panel, PANEL_TOGGLE,
					  XV_HELP_DATA, "xvgr:frame_fillbg",
						      PANEL_CHOICE_STRINGS,
					      "Fill graph background", NULL,
				     PANEL_VALUE_X, xv_col(frame_panel, 15),
						      NULL);
    frame_bgcolor_choice_item = (Panel_item) xv_create(frame_panel, PANEL_CHOICE_STACK,
					 XV_HELP_DATA, "xvgr:frame_bgcolor",
						       PANEL_CHOICE_NCOLS, 4,
				    PANEL_LABEL_STRING, "Background color:",
						       PANEL_CHOICE_STRINGS,
				"0", "1", "2", "3", "4", "5", "6", "7", "8",
			"9", "10", "11", "12", "13", "14", "15",
						       NULL,
					       XV_X, xv_col(frame_panel, 3),
						       NULL);
    (void) xv_create(frame_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:frame_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, frame_Done_notify_proc,
		     XV_X, xv_col(frame_panel, 12),
		     XV_Y, xv_row(frame_panel, 9),
		     NULL);
    (void) xv_create(frame_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:frame_accept",
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, frame_define_notify_proc,
		     XV_X, xv_col(frame_panel, 1),
		     XV_Y, xv_row(frame_panel, 9),
		     NULL);
    window_fit(frame_panel);
    window_fit(frame_frame);
    update_frame_items(cg);
    xv_set(frame_frame, WIN_SHOW, TRUE, 0);
}				/* end create_frame_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int frame_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(frame_frame, WIN_SHOW, FALSE, 0);
}

/*ARGSUSED*/
static int frame_define_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    g[cg].f.active = (int) xv_get(frame_frameactive_choice_item, PANEL_VALUE) ? OFF : ON;
    g[cg].f.type = (int) xv_get(frame_framestyle_choice_item, PANEL_VALUE);
    g[cg].f.color = (int) xv_get(frame_color_choice_item, PANEL_VALUE);
    g[cg].f.linew = (int) xv_get(frame_linew_choice_item, PANEL_VALUE) + 1;
    g[cg].f.lines = (int) xv_get(frame_lines_choice_item, PANEL_VALUE) + 1;
    g[cg].f.fillbg = (int) xv_get(frame_fillbg_choice_item, PANEL_VALUE) ? ON : OFF;
    g[cg].f.bgcolor = (int) xv_get(frame_bgcolor_choice_item, PANEL_VALUE);
/*
    xv_set(frame_frame, WIN_SHOW, FALSE, 0);
*/
    drawgraph();
    return XV_OK;
}
