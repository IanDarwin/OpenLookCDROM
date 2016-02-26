/* $Id: locatewin.c,v 1.7 91/12/07 09:55:42 pturner Exp Locker: pturner $
 *
 * Locator Panel
 *
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

extern Frame main_frame;

void make_format();

Frame locator_frame = (Frame) 0;
Panel locator_panel;

/*
 * Panel item declarations
 */
Panel_item locator_onoff_item;
Panel_item delta_item;
Panel_item loc_formatx;
Panel_item loc_formaty;
Panel_item loc_precx;
Panel_item loc_precy;
Panel_item locx_item;
Panel_item locy_item;
Panel_item fixedp_item;

/*
 * Event and Notify proc declarations
 */
static int locator_Done_notify_proc();
static int locator_define_notify_proc();
static int locator_reset_notify_proc();

extern int go_locateflag;
int locfx = GENERAL, locfy = GENERAL, locpx = 6, locpy = 6;

void update_locator_items(gno)
{
    if (locator_frame) {
	xv_set(locator_onoff_item, PANEL_VALUE, go_locateflag == FALSE, NULL);
	xv_set(fixedp_item, PANEL_VALUE, g[gno].pointset == TRUE, NULL);
	xv_set(delta_item, PANEL_VALUE, g[gno].pt_type, NULL);
	xv_set(loc_formatx, PANEL_VALUE, get_format_index(g[gno].fx), NULL);
	xv_set(loc_formaty, PANEL_VALUE, get_format_index(g[gno].fy), NULL);
	xv_set(loc_precx, PANEL_VALUE, g[gno].px, NULL);
	xv_set(loc_precy, PANEL_VALUE, g[gno].py, NULL);
	if (g[gno].pointset) {
	    sprintf(buf, "%lf", g[gno].dsx);
	    xv_set(locx_item, PANEL_VALUE, buf, NULL);
	    sprintf(buf, "%lf", g[gno].dsy);
	    xv_set(locy_item, PANEL_VALUE, buf, NULL);
	}
    }
}

/*
 * Create the locator Frame and the locator Panel
 */
void create_locator_frame()
{
    if (locator_frame) {
	update_locator_items(cg);
	xv_set(locator_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    locator_frame = (Frame) xv_create(main_frame, FRAME,
				      FRAME_LABEL, "Locator",
				      NULL);
    locator_panel = (Panel) xv_create(locator_frame, PANEL,
				      PANEL_LAYOUT, PANEL_VERTICAL,
				      NULL);
    locator_onoff_item = xv_create(locator_panel, PANEL_CHOICE,
				   PANEL_LABEL_STRING,
				   "Locator:",
				   PANEL_CHOICE_STRINGS,
				   "ON",
				   "OFF",
				   NULL,
				   PANEL_VALUE_X, xv_col(locator_panel, 23),
				   NULL);
    delta_item = xv_create(locator_panel, PANEL_CYCLE,
			   PANEL_LABEL_STRING,
			   "Locator display type:",
			   PANEL_CHOICE_STRINGS,
			   "[X, Y]",
			   "[DX, DY]",
			   "[DISTANCE]",
			   "[R, Theta]",
			   "[VX, VY]",
			   "[SX, SY]",
			   NULL,
			   PANEL_VALUE_X, xv_col(locator_panel, 23),
			   NULL);
    (void) xv_create(locator_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Format",
		     XV_X, xv_col(locator_panel, 0),
		     NULL);
    loc_formatx = xv_create(locator_panel, PANEL_CYCLE,
			    PANEL_LABEL_STRING,
			    "X:",
			    PANEL_CHOICE_NCOLS, 3,
			    PANEL_CHOICE_STRINGS,
			    "Decimal",
			    "Exponential",
			    "Power (decimal)",
			    "General",
			    "DD-MM-YY",
			    "MM-DD-YY",
			    "MM-YY",
			    "MM-DD",
			    "Month-DD",
			    "DD-Month",
			    "Month (abrev.)",
			    "Month",
			    "Day of week (abrev.)",
			    "Day of week",
			    "Day of year",
			    "HH:MM:SS.s",
			    "MM-DD HH:MM:SS.s",
			    "MM-DD-YY HH:MM:SS.s",
			    "Degrees (lon)",
			    "DD MM' (lon)",
			    "DD MM' SS.s\" (lon)",
			    "MM' SS.s\" (lon)",
			    "Degrees (lat)",
			    "DD MM' (lat)",
			    "DD MM' SS.s\" (lat)",
			    "MM' SS.s\" (lat)",
			    NULL,
			    PANEL_VALUE_X, xv_col(locator_panel, 3),
			    XV_Y, xv_row(locator_panel, 3),
			    NULL);
    loc_formaty = xv_create(locator_panel, PANEL_CYCLE,
			    PANEL_LABEL_STRING,
			    "Y:",
			    PANEL_CHOICE_NCOLS, 3,
			    PANEL_CHOICE_STRINGS,
			    "Decimal",
			    "Exponential",
			    "Power (decimal)",
			    "General",
			    "DD-MM-YY",
			    "MM-DD-YY",
			    "MM-YY",
			    "MM-DD",
			    "Month-DD",
			    "DD-Month",
			    "Month (abrev.)",
			    "Month",
			    "Day of week (abrev.)",
			    "Day of week",
			    "Day of year",
			    "HH:MM:SS.s",
			    "MM-DD HH:MM:SS.s",
			    "MM-DD-YY HH:MM:SS.s",
			    "Degrees (lon)",
			    "DD MM' (lon)",
			    "DD MM' SS.s\" (lon)",
			    "MM' SS.s\" (lon)",
			    "Degrees (lat)",
			    "DD MM' (lat)",
			    "DD MM' SS.s\" (lat)",
			    "MM' SS.s\" (lat)",
			    NULL,
			    PANEL_VALUE_X, xv_col(locator_panel, 28),
			    XV_Y, xv_row(locator_panel, 3),
			    NULL);
    (void) xv_create(locator_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Precision",
		     XV_X, xv_col(locator_panel, 0),
		     XV_Y, xv_row(locator_panel, 4),
		     NULL);
    loc_precx = xv_create(locator_panel, PANEL_CYCLE,
			  PANEL_LABEL_STRING,
			  "X:",
			  PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
			  NULL,
			  PANEL_VALUE_X, xv_col(locator_panel, 3),
			  XV_Y, xv_row(locator_panel, 5),
			  NULL);
    loc_precy = xv_create(locator_panel, PANEL_CYCLE,
			  PANEL_LABEL_STRING,
			  "Y:",
			  PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
			  NULL,
			  PANEL_VALUE_X, xv_col(locator_panel, 28),
			  XV_Y, xv_row(locator_panel, 5),
			  NULL);
    (void) xv_create(locator_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Fixed point at",
		     XV_X, xv_col(locator_panel, 0),
		     XV_Y, xv_row(locator_panel, 6),
		     NULL);
    locx_item = (Panel_item) xv_create(locator_panel, PANEL_TEXT,
				       PANEL_LAYOUT, PANEL_HORIZONTAL,
				       PANEL_VALUE_DISPLAY_LENGTH, 10,
				       PANEL_LABEL_STRING, "X:",
				    PANEL_VALUE_X, xv_col(locator_panel, 3),
				       XV_Y, xv_row(locator_panel, 7),
				       NULL);
    locy_item = (Panel_item) xv_create(locator_panel, PANEL_TEXT,
				       PANEL_LAYOUT, PANEL_HORIZONTAL,
				       PANEL_VALUE_DISPLAY_LENGTH, 10,
				       PANEL_LABEL_STRING, "Y:",
				   PANEL_VALUE_X, xv_col(locator_panel, 28),
				       XV_Y, xv_row(locator_panel, 7),
				       NULL);
    fixedp_item = xv_create(locator_panel, PANEL_CHECK_BOX,
			    PANEL_LABEL_STRING,
			    "Use fixed point:",
			    XV_X, xv_col(locator_panel, 1),
			    XV_Y, xv_row(locator_panel, 8),
			    NULL);

    (void) xv_create(locator_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, locator_Done_notify_proc,
		     XV_X, xv_col(locator_panel, 25),
		     XV_Y, xv_row(locator_panel, 9),
		     NULL);
    (void) xv_create(locator_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, locator_define_notify_proc,
		     XV_X, xv_col(locator_panel, 5),
		     XV_Y, xv_row(locator_panel, 9),
		     NULL);
    (void) xv_create(locator_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Reset",
		     PANEL_NOTIFY_PROC, locator_reset_notify_proc,
		     XV_X, xv_col(locator_panel, 15),
		     XV_Y, xv_row(locator_panel, 9),
		     NULL);
    window_fit(locator_panel);
    window_fit(locator_frame);
    update_locator_items(cg);
    xv_set(locator_frame, WIN_SHOW, TRUE, 0);
}				/* end create_locator_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int locator_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(locator_frame, WIN_SHOW, FALSE, 0);
}

/*ARGSUSED*/
static int locator_define_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    double atof();
    int type;

    go_locateflag = (int) xv_get(locator_onoff_item, PANEL_VALUE) == 0;
    type = g[cg].pt_type = (int) xv_get(delta_item, PANEL_VALUE);
    locfx = g[cg].fx = format_types[(int) xv_get(loc_formatx, PANEL_VALUE)];
    locfy = g[cg].fy = format_types[(int) xv_get(loc_formaty, PANEL_VALUE)];
    locpx = g[cg].px = (int) xv_get(loc_precx, PANEL_VALUE);
    locpy = g[cg].py = (int) xv_get(loc_precy, PANEL_VALUE);
    g[cg].pointset = (int) xv_get(fixedp_item, PANEL_VALUE);
    if (g[cg].pointset) {
	strcpy(buf, (char *) xv_get(locx_item, PANEL_VALUE));
	if (buf[0]) {
	    g[cg].dsx = atof(buf);
	}
	strcpy(buf, (char *) xv_get(locy_item, PANEL_VALUE));
	if (buf[0]) {
	    g[cg].dsy = atof(buf);
	}
    }
    make_format(cg);
/*    xv_set(locator_frame, WIN_SHOW, FALSE, 0);*/
    return XV_OK;
}

/*ARGSUSED*/
static int locator_reset_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    g[cg].dsx = g[cg].dsy = 0.0;/* locator props */
    g[cg].pointset = FALSE;
    g[cg].pt_type = 0;
    g[cg].fx = GENERAL;
    g[cg].fy = GENERAL;
    g[cg].px = 6;
    g[cg].py = 6;
    update_locator_items(cg);
    return XV_OK;
}
