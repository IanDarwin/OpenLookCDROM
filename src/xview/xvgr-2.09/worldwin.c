/* $Id: worldwin.c,v 1.16 92/08/22 19:37:37 pturner Exp Locker: pturner $
 *
 * world, view, arrange, and autoscale popups
 *
 */

#include <stdio.h>
#include <math.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

int snapflag = 0;
double snapval = 0.0;

void update_world();

extern Frame main_frame;

Frame world_frame = (Frame) NULL;
Panel world_panel;

Frame view_frame = (Frame) NULL;
Panel view_panel;

Frame arrange_frame = (Frame) NULL;
Panel arrange_panel;

Frame autos_frame = (Frame) NULL;
Panel autos_panel;

/*
 * Panel item declarations
 */
Panel_item world_xmin_text_item;
Panel_item world_xmax_text_item;
Panel_item world_ymin_text_item;
Panel_item world_ymax_text_item;
Panel_item world_xmajor_text_item;
Panel_item world_xminor_text_item;
Panel_item world_ymajor_text_item;
Panel_item world_yminor_text_item;
Panel_item world_autoset_choice_item;
Panel_item world_autotype_choice_item;
Panel_item world_wmsg_message_item;
Panel_item world_tmsg_message_item;
Panel_item world_applyto_item;

/*
 * for the view popup
 */
Panel_item define_view_xv1;
Panel_item define_view_xv2;
Panel_item define_view_yv1;
Panel_item define_view_yv2;
Panel_item view_applyto_item;
Panel_item view_snap_item;

/*
 * for the arrange popup
 */
Panel_item arrange_rows_item;
Panel_item arrange_cols_item;
Panel_item arrange_vgap_item;
Panel_item arrange_hgap_item;
Panel_item arrange_startx_item;
Panel_item arrange_starty_item;
Panel_item arrange_widthx_item;
Panel_item arrange_widthy_item;
Panel_item arrange_packed_item;
Panel_item arrange_applyto_item;

/*
 * for the autoscale popup
 */
Panel_item autos_on_item;
Panel_item autos_using_item;
Panel_item autos_method_item;
Panel_item autos_nticksx_item;
Panel_item autos_nticksy_item;
Panel_item autos_applyto_item;

/*
 * Event and Notify proc declarations
 */
static int world_Done_notify_proc();
static int world_accept_notify_proc();
static int world_update_notify_proc();
static void world_autotype_notify_proc();
static int world_aautoset_notify_proc();

/*
 * Create the world Frame and the world Panel
 */
void create_world_frame()
{
    if (world_frame) {
	update_world(cg);
	xv_set(world_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    world_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "world",
				    NULL);
    world_panel = (Panel) xv_create(world_frame, PANEL,
				    NULL);
    world_wmsg_message_item = xv_create(world_panel, PANEL_MESSAGE,
				    PANEL_LABEL_STRING, "World coordinates",
					XV_X, xv_col(world_panel, 2),
					NULL);
    world_xmin_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
						PANEL_LABEL_STRING, "Xmin:",
				      PANEL_VALUE_X, xv_col(world_panel, 8),
					       XV_Y, xv_row(world_panel, 1),
						  NULL);
    world_xmax_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
						PANEL_LABEL_STRING, "Xmax:",
				      PANEL_VALUE_X, xv_col(world_panel, 8),
					       XV_Y, xv_row(world_panel, 2),
						  NULL);
    world_ymin_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
						PANEL_LABEL_STRING, "Ymin:",
				      PANEL_VALUE_X, xv_col(world_panel, 8),
					       XV_Y, xv_row(world_panel, 3),
						  NULL);
    world_ymax_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
						PANEL_LABEL_STRING, "Ymax:",
				      PANEL_VALUE_X, xv_col(world_panel, 8),
					       XV_Y, xv_row(world_panel, 4),
						  NULL);
    world_tmsg_message_item = xv_create(world_panel, PANEL_MESSAGE,
					PANEL_LABEL_STRING, "Tick marks",
					XV_X, xv_col(world_panel, 22),
					XV_Y, xv_row(world_panel, 0),
					NULL);
    world_xmajor_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "X-major:",
				     PANEL_VALUE_X, xv_col(world_panel, 29),
					       XV_Y, xv_row(world_panel, 1),
						    NULL);
    world_xminor_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "X-minor:",
				     PANEL_VALUE_X, xv_col(world_panel, 29),
					       XV_Y, xv_row(world_panel, 2),
						    NULL);
    world_ymajor_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "Y-major:",
				     PANEL_VALUE_X, xv_col(world_panel, 29),
					       XV_Y, xv_row(world_panel, 3),
						    NULL);
    world_yminor_text_item = (Panel_item) xv_create(world_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "Y-minor:",
				     PANEL_VALUE_X, xv_col(world_panel, 29),
					       XV_Y, xv_row(world_panel, 4),
						    NULL);
    world_applyto_item = (Panel_item) xv_create(world_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Apply to:",
						PANEL_CHOICE_STRINGS,
						"Current graph",
						"All active graphs",
						"X, all active graphs",
						"Y, all active graphs",
						NULL,
						XV_X, xv_col(world_panel, 0),
						XV_Y, xv_row(world_panel, 5),
						NULL);
    (void) xv_create(world_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, world_accept_notify_proc,
		     XV_X, xv_col(world_panel, 1),
		     XV_Y, xv_row(world_panel, 6),
		     NULL);
    (void) xv_create(world_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Update world/ticks",
		     PANEL_NOTIFY_PROC, world_update_notify_proc,
		     XV_X, xv_col(world_panel, 10),
		     XV_Y, xv_row(world_panel, 6),
		     NULL);
    (void) xv_create(world_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, world_Done_notify_proc,
		     XV_X, xv_col(world_panel, 32),
		     XV_Y, xv_row(world_panel, 6),
		     NULL);
    window_fit(world_panel);
    window_fit(world_frame);
    update_world(cg);
    xv_set(world_frame, WIN_SHOW, TRUE, 0);
}				/* end create_world_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int world_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(world_frame, WIN_SHOW, FALSE, 0);
}

/*ARGSUSED*/
static int world_update_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    update_world(cg);
    return XV_OK;
}

/*
 * update the items in define world/view popup
 */
void update_world(gno)
    int gno;
{
    if (world_frame) {
	sprintf(buf, "%.9g", g[gno].w.xg1);
	xv_set(world_xmin_text_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9g", g[gno].w.xg2);
	xv_set(world_xmax_text_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9g", g[gno].w.yg1);
	xv_set(world_ymin_text_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9g", g[gno].w.yg2);
	xv_set(world_ymax_text_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9g", g[gno].t[0].tmajor);
	xv_set(world_xmajor_text_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9g", g[gno].t[0].tminor);
	xv_set(world_xminor_text_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9g", g[gno].t[1].tmajor);
	xv_set(world_ymajor_text_item, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9g", g[gno].t[1].tminor);
	xv_set(world_yminor_text_item, PANEL_VALUE, buf, NULL);
    }
}

/*
 * define the window coordinates, note that input is scanned for expressions
 */
/* ARGSUSED */
static int world_accept_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    char val[80];
    int i, which, ming, maxg, errpos;
    double x, y, a, b, c, d;
    extern double result;	/* result passed from the expression
				 * interpreter */

    if (which = (int) xv_get(world_applyto_item, PANEL_VALUE)) {
	ming = 0;
	maxg = maxgraph - 1;
    } else {
	ming = cg;
	maxg = cg;
    }
    if (ming == cg && maxg == cg) {
	if (!isactive_graph(cg)) {
	    errwin("Current graph is not active!");
	    return XV_OK;
	}
    }
    for (i = ming; i <= maxg; i++) {
	if (isactive_graph(i)) {
	    x = (g[i].w.xg2 - g[i].w.xg1);	/* DX */
	    y = (g[i].w.yg2 - g[i].w.yg1);	/* DY */
	    a = g[i].w.xg1;	/* XMIN */
	    b = g[i].w.yg1;	/* XMAX */
	    c = g[i].w.xg2;	/* YMIN */
	    d = g[i].w.yg2;	/* YMAX */
	    if (which <= 1 || which == 2) {
		strcpy(val, (char *) xv_get(world_xmin_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return;
		}
		g[i].w.xg1 = result;

		strcpy(val, (char *) xv_get(world_xmax_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return;
		}
		g[i].w.xg2 = result;

		strcpy(val, (char *) xv_get(world_xmajor_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return;
		}
		g[i].t[0].tmajor = result;

		strcpy(val, (char *) xv_get(world_xminor_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return;
		}
		g[i].t[0].tminor = result;
	    }
	    if (which <= 1 || which == 3) {

		strcpy(val, (char *) xv_get(world_ymin_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return;
		}
		g[i].w.yg1 = result;
		strcpy(val, (char *) xv_get(world_ymax_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return;
		}
		g[i].w.yg2 = result;

		strcpy(val, (char *) xv_get(world_ymajor_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return;
		}
		g[i].t[1].tmajor = result;

		strcpy(val, (char *) xv_get(world_yminor_text_item, PANEL_VALUE));
		fixupstr(val);
		scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
		if (errpos) {
		    return XV_OK;
		}
		g[i].t[1].tminor = result;
	    }
	}
    }
    drawgraph();
    return XV_OK;
}

void update_view(gno)
    int gno;
{
    if (view_frame) {
	sprintf(buf, "%.5g", g[gno].v.xv1);
	xv_setstr(define_view_xv1, buf);
	sprintf(buf, "%.5g", g[gno].v.xv2);
	xv_setstr(define_view_xv2, buf);
	sprintf(buf, "%.5g", g[gno].v.yv1);
	xv_setstr(define_view_yv1, buf);
	sprintf(buf, "%.5g", g[gno].v.yv2);
	xv_setstr(define_view_yv2, buf);
    }
}

double fround(x, r)
    double x, r;
{
    x = x * 1.0 / r;
    x = (long) (x + 0.5);
    return x * r;
}

static void define_view_proc()
{
    char val[80];
    double tmpx1, tmpx2;
    double tmpy1, tmpy2;
    double r;
    int i, itmp, ming, maxg, which, ierr = 0;

    snapflag = (int) xv_get(view_snap_item, PANEL_VALUE);
    if (which = (int) xv_get(view_applyto_item, PANEL_VALUE)) {
	ming = 0;
	maxg = maxgraph - 1;
    } else {
	ming = cg;
	maxg = cg;
    }
    if (ming == cg && maxg == cg) {
	if (!isactive_graph(cg)) {
	    errwin("Current graph is not active!");
	    return;
	}
    }
    strcpy(val, (char *) xv_get(define_view_xv1, PANEL_VALUE));
    tmpx1 = atof(val);
    strcpy(val, (char *) xv_get(define_view_xv2, PANEL_VALUE));
    tmpx2 = atof(val);
    strcpy(val, (char *) xv_get(define_view_yv1, PANEL_VALUE));
    tmpy1 = atof(val);
    strcpy(val, (char *) xv_get(define_view_yv2, PANEL_VALUE));
    tmpy2 = atof(val);
    if (!fbounds(tmpx1, 0.0, 1.0, "View xmin")) {
	ierr = 1;
    } else if (!fbounds(tmpx2, tmpx1, 1.0, "View xmax")) {
	ierr = 1;
    } else if (!fbounds(tmpy1, 0.0, 1.0, "View ymin")) {
	ierr = 1;
    } else if (!fbounds(tmpy2, tmpy1, 1.0, "View ymax")) {
	ierr = 1;
    } else if (tmpx2 - tmpx1 <= 0.0) {
	ierr = 1;
    } else if (tmpy2 - tmpy1 <= 0.0) {
	ierr = 1;
    }
    if (ierr) {
	errwin("Viewport not set");
	return;
    }
    switch (snapflag) {
    case 0:			/* no snap */
	break;
    case 1:			/* .1 */
	snapval = r = 0.1;
	break;
    case 2:			/* .05 */
	snapval = r = 0.05;
	break;
    case 3:			/* .01 */
	snapval = r = 0.01;
	break;
    case 4:			/* .005 */
	snapval = r = 0.005;
	break;
    case 5:			/* .001 */
	snapval = r = 0.001;
	break;
    }
    if (snapflag) {
	tmpx1 = fround(tmpx1, r);
	tmpx2 = fround(tmpx2, r);
	tmpy1 = fround(tmpy1, r);
	tmpy2 = fround(tmpy2, r);
    }
    for (i = ming; i <= maxg; i++) {
	if (isactive_graph(i)) {
	    if (which <= 1 || which == 2) {
		g[i].v.xv1 = tmpx1;
		g[i].v.xv2 = tmpx2;
	    }
	    if (which <= 1 || which == 3) {
		g[i].v.yv1 = tmpy1;
		g[i].v.yv2 = tmpy2;
	    }
	}
    }
    if (snapflag) {
	update_view(cg);
    }
    drawgraph();
}

static void do_view_proc()
{
    xv_set(view_frame, WIN_SHOW, FALSE, 0);
    set_action(0);
    set_action(VIEW_1ST);
}

static void snap_proc()
{
}

static void view_Done_notify_proc()
{
    xv_set(view_frame, WIN_SHOW, FALSE, 0);
}

void create_view_frame()
{
    if (view_frame) {
	update_view(cg);
	xv_set(view_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    view_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Viewport",
				   NULL);
    view_panel = (Panel) xv_create(view_frame, PANEL,
				   NULL);
    (void) xv_create(view_panel, PANEL_MESSAGE,
		  PANEL_LABEL_STRING, "Viewport settings (from 0.0 to 1.0)",
		     XV_X, xv_col(view_panel, 0),
		     XV_Y, xv_row(view_panel, 0),
		     NULL);
    define_view_xv1 = (Panel_item) xv_create(view_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "Xmin:",
				      PANEL_VALUE_X, xv_col(view_panel, 10),
					     XV_Y, xv_row(view_panel, 1),
					     NULL);
    define_view_xv2 = (Panel_item) xv_create(view_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "Xmax:",
				      PANEL_VALUE_X, xv_col(view_panel, 10),
					     XV_Y, xv_row(view_panel, 2),
					     NULL);
    define_view_yv1 = (Panel_item) xv_create(view_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "Ymin:",
				      PANEL_VALUE_X, xv_col(view_panel, 10),
					     XV_Y, xv_row(view_panel, 3),
					     NULL);
    define_view_yv2 = (Panel_item) xv_create(view_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					     PANEL_LABEL_STRING, "Ymax:",
				      PANEL_VALUE_X, xv_col(view_panel, 10),
					     XV_Y, xv_row(view_panel, 4),
					     NULL);
    view_snap_item = (Panel_item) xv_create(view_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Snap:",
					    PANEL_CHOICE_STRINGS,
					    "None",
					    ".1",
					    ".05",
					    ".01",
					    ".005",
					    ".001",
					    NULL,
				      PANEL_VALUE_X, xv_col(view_panel, 10),
					    XV_Y, xv_row(view_panel, 5),
					    NULL);
    view_applyto_item = (Panel_item) xv_create(view_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Apply to:",
					       PANEL_CHOICE_STRINGS,
					       "Current graph",
					       "All active graphs",
					       "X, all active graphs",
					       "Y, all active graphs",
					       NULL,
				      PANEL_VALUE_X, xv_col(view_panel, 10),
					       XV_Y, xv_row(view_panel, 6),
					       NULL);
    (void) xv_create(view_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, view_Done_notify_proc,
		     XV_X, xv_col(view_panel, 22),
		     XV_Y, xv_row(view_panel, 7),
		     NULL);
    (void) xv_create(view_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Pick view",
		     PANEL_NOTIFY_PROC, do_view_proc,
		     XV_X, xv_col(view_panel, 10),
		     XV_Y, xv_row(view_panel, 7),
		     NULL);
    (void) xv_create(view_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, define_view_proc,
		     XV_X, xv_col(view_panel, 0),
		     XV_Y, xv_row(view_panel, 7),
		     NULL);
    window_fit(view_panel);
    window_fit(view_frame);
    update_view(cg);
    xv_set(view_frame, WIN_SHOW, TRUE, 0);
}

static void define_arrange_proc()
{
    int i, j, k, gno, nrows, ncols, pack, applyto = 0;
    double vgap, hgap, sx, sy, wx, wy;

    nrows = (int) xv_get(arrange_rows_item, PANEL_VALUE) + 1;
    ncols = (int) xv_get(arrange_cols_item, PANEL_VALUE) + 1;
    pack = (int) xv_get(arrange_packed_item, PANEL_VALUE);
    vgap = atof((char *) xv_get(arrange_vgap_item, PANEL_VALUE));
    hgap = atof((char *) xv_get(arrange_hgap_item, PANEL_VALUE));
    sx = atof((char *) xv_get(arrange_startx_item, PANEL_VALUE));
    sy = atof((char *) xv_get(arrange_starty_item, PANEL_VALUE));
    wx = atof((char *) xv_get(arrange_widthx_item, PANEL_VALUE));
    wy = atof((char *) xv_get(arrange_widthy_item, PANEL_VALUE));
    if (wx < 0.0) {
	errwin("Graph width <= 0.0");
	return;
    }
    if (wy < 0.0) {
	errwin("Graph height <= 0.0");
	return;
    }
    define_arrange(nrows, ncols, pack, vgap, hgap, sx, sy, wx, wy);
}

static void arrange_Done_notify_proc()
{
    xv_set(arrange_frame, WIN_SHOW, FALSE, 0);
}

void create_arrange_frame()
{
    if (arrange_frame) {
	xv_set(arrange_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    arrange_frame = (Frame) xv_create(main_frame, FRAME,
				      FRAME_LABEL, "Arrange graphs",
				      NULL);
    arrange_panel = (Panel) xv_create(arrange_frame, PANEL,
				      PANEL_LAYOUT, PANEL_VERTICAL,
				      NULL);
    arrange_rows_item = (Panel_item) xv_create(arrange_panel, PANEL_CHOICE_STACK,
					       PANEL_LABEL_STRING, "Rows:",
					       PANEL_CHOICE_STRINGS,
			  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       NULL,
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 0),
					       NULL);
    arrange_cols_item = (Panel_item) xv_create(arrange_panel, PANEL_CHOICE_STACK,
					     PANEL_LABEL_STRING, "Columns:",
					       PANEL_CHOICE_STRINGS,
			  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       NULL,
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 1),
					       NULL);
    arrange_packed_item = (Panel_item) xv_create(arrange_panel, PANEL_CHOICE_STACK,
					     PANEL_LABEL_STRING, "Packing:",
						 PANEL_CHOICE_STRINGS,
				   "None", "Horizontal", "Vertical", "Both",
						 NULL,
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 2),
						 NULL);
    arrange_vgap_item = (Panel_item) xv_create(arrange_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					PANEL_LABEL_STRING, "Vertical gap:",
					       PANEL_VALUE, "0.05",
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 3),
					       NULL);
    arrange_hgap_item = (Panel_item) xv_create(arrange_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
				      PANEL_LABEL_STRING, "Horizontal gap:",
					       PANEL_VALUE, "0.05",
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 4),
					       NULL);
    arrange_startx_item = (Panel_item) xv_create(arrange_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					 PANEL_LABEL_STRING, "Start at X =",
						 PANEL_VALUE, "0.1",
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 5),
						 NULL);
    arrange_starty_item = (Panel_item) xv_create(arrange_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					 PANEL_LABEL_STRING, "Start at Y =",
						 PANEL_VALUE, "0.1",
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 6),
						 NULL);
    arrange_widthx_item = (Panel_item) xv_create(arrange_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					 PANEL_LABEL_STRING, "Graph width:",
						 PANEL_VALUE, "0.9",
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 7),
						 NULL);
    arrange_widthy_item = (Panel_item) xv_create(arrange_panel, PANEL_TEXT,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
					PANEL_LABEL_STRING, "Graph height:",
						 PANEL_VALUE, "0.9",
				   PANEL_VALUE_X, xv_col(arrange_panel, 17),
					     XV_Y, xv_row(arrange_panel, 8),
						 NULL);
    (void) xv_create(arrange_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, arrange_Done_notify_proc,
		     XV_X, xv_col(arrange_panel, 15),
		     XV_Y, xv_row(arrange_panel, 9),
		     NULL);
    (void) xv_create(arrange_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, define_arrange_proc,
		     XV_X, xv_col(arrange_panel, 5),
		     XV_Y, xv_row(arrange_panel, 9),
		     NULL);
    window_fit(arrange_panel);
    window_fit(arrange_frame);
    xv_set(arrange_frame, WIN_SHOW, TRUE, 0);
}

static void define_autos_proc()
{
    int i, aon, ameth, antx, anty, au, ap, ming, maxg;

    aon = (int) xv_get(autos_on_item, PANEL_VALUE) - 4;
    au = (int) xv_get(autos_using_item, PANEL_VALUE) - 1;
    ap = (int) xv_get(autos_applyto_item, PANEL_VALUE);
    ameth = (int) xv_get(autos_method_item, PANEL_VALUE);
    antx = (int) xv_get(autos_nticksx_item, PANEL_VALUE);
    anty = (int) xv_get(autos_nticksy_item, PANEL_VALUE);
    define_autos(aon, au, ap, ameth, antx, anty);
}

void update_autos(gno)
    int gno;
{
    if (autos_frame) {
	xv_set(autos_method_item, PANEL_VALUE, g[gno].auto_type == SPEC, NULL);
	xv_set(autos_nticksx_item, PANEL_VALUE, g[gno].t[0].t_num - 2, NULL);
	xv_set(autos_nticksy_item, PANEL_VALUE, g[gno].t[1].t_num - 2, NULL);
    }
}

static void autos_Done_notify_proc()
{
    xv_set(autos_frame, WIN_SHOW, FALSE, 0);
}

void create_autos_frame()
{
    if (autos_frame) {
	update_autos(cg);
	xv_set(autos_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    autos_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "Autoscale",
				    NULL);
    autos_panel = (Panel) xv_create(autos_frame, PANEL,
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    autos_on_item = (Panel_item) xv_create(autos_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Autoscale axis:",
					   PANEL_CHOICE_STRINGS,
					   "None", "All", "All X-axes", "All Y-axes", "X-axis", "Y-axis", "Zero X-axis", "Zero Y-axis", "Alternate X-axis", "Alternate Y-axis",
					   NULL,
				     PANEL_VALUE_X, xv_col(autos_panel, 25),
					   XV_Y, xv_row(autos_panel, 0),
					   NULL);
    autos_using_item = (Panel_item) xv_create(autos_panel, PANEL_CHOICE_STACK,
					PANEL_LABEL_STRING, "Using set(s):",
					      PANEL_CHOICE_NCOLS, 3,
					      PANEL_CHOICE_STRINGS,
		    "All", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
		 "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
		 "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
					      NULL,
				     PANEL_VALUE_X, xv_col(autos_panel, 25),
					      XV_Y, xv_row(autos_panel, 1),
					      NULL);
    autos_method_item = (Panel_item) xv_create(autos_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Autoscale type:",
					       PANEL_CHOICE_STRINGS,
					       "Heckbert",
					       "Fixed",
					       NULL,
				     PANEL_VALUE_X, xv_col(autos_panel, 25),
					       XV_Y, xv_row(autos_panel, 2),
					       NULL);
    autos_nticksx_item = (Panel_item) xv_create(autos_panel, PANEL_CHOICE_STACK,
				PANEL_LABEL_STRING, "Number of ticks in X:",
						PANEL_CHOICE_STRINGS,
		   "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
						NULL,
				     PANEL_VALUE_X, xv_col(autos_panel, 25),
						XV_Y, xv_row(autos_panel, 3),
						NULL);
    autos_nticksy_item = (Panel_item) xv_create(autos_panel, PANEL_CHOICE_STACK,
				PANEL_LABEL_STRING, "Number of ticks in Y:",
						PANEL_CHOICE_STRINGS,
		   "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
						NULL,
				     PANEL_VALUE_X, xv_col(autos_panel, 25),
						XV_Y, xv_row(autos_panel, 4),
						NULL);
    autos_applyto_item = (Panel_item) xv_create(autos_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Apply to:",
						PANEL_CHOICE_STRINGS,
						"Current graph",
						"All active graphs",
						NULL,
				     PANEL_VALUE_X, xv_col(autos_panel, 25),
						XV_Y, xv_row(autos_panel, 5),
						NULL);
    (void) xv_create(autos_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, autos_Done_notify_proc,
		     XV_X, xv_col(autos_panel, 15),
		     XV_Y, xv_row(autos_panel, 6),
		     NULL);
    (void) xv_create(autos_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, define_autos_proc,
		     XV_X, xv_col(autos_panel, 5),
		     XV_Y, xv_row(autos_panel, 6),
		     NULL);
    window_fit(autos_panel);
    window_fit(autos_frame);
    update_autos(cg);
    xv_set(autos_frame, WIN_SHOW, TRUE, 0);
}
