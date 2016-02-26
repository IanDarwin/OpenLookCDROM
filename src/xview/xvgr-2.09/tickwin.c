/* $Id: tickwin.c,v 1.18 92/08/01 08:31:35 pturner Exp Locker: pturner $
 *
 * ticks / tick labels / axes labels
 *
 */

#include <stdio.h>
#include <math.h>

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/cursor.h>

#include "globals.h"

extern Frame main_frame;

Frame ticks_frame = (Frame) 0;
Panel ticks_panel;

static Panel_item editaxis;	/* which axis to edit */
static Panel_item ticks_applyto;	/* override the previous item */
static Panel_item offx;		/* x offset of axis in viewport coords */
static Panel_item offy;		/* y offset of axis in viewport coords */
static Panel_item altmap;	/* alternate mapping for axis */
static Panel_item altmin;	/* alternate mapping for axis */
static Panel_item altmax;	/* alternate mapping for axis */
static Panel_item tonoff;	/* toggle display of axis ticks */
static Panel_item tlonoff;	/* toggle display of tick labels */
static Panel_item axislabel;	/* axis label */
static Panel_item axislabelop;	/* axis label on opposite side */
static Panel_item axislabellayout;	/* axis label layout (perp or
					 * parallel) */
static Panel_item axislabelfont;/* axis label font */
static Panel_item axislabelcharsize;	/* axis label charsize */
static Panel_item axislabelcolor;	/* axis label color */
static Panel_item axislabellinew;	/* axis label linew */
static Panel_item tmajor;	/* major tick spacing */
static Panel_item tminor;	/* minor tick spacing */
static Panel_item tickop;	/* ticks opposite */
static Panel_item ticklop;	/* tick labels opposite */
static Panel_item ticklabel_applyto;	/* apply to several axes */
static Panel_item tlform;	/* format for labels */
static Panel_item tlprec;	/* precision for labels */
static Panel_item tlfont;	/* tick label font */
static Panel_item tlcharsize;	/* tick label charsize */
static Panel_item tlcolor;	/* tick label color */
static Panel_item tllinew;	/* tick label color */
static Panel_item tlvgap;	/* */
static Panel_item tlhgap;	/* */
static Panel_item tlskip;	/* tick marks to skip */
static Panel_item tltype;	/* tick label type (auto or specified) */
static Panel_item ttype;	/* tick mark type (auto or specified) */
static Panel_item tlstarttype;	/* use graph min or starting value */
static Panel_item tlstart;	/* value to start tick labels */
static Panel_item tlstoptype;	/* use graph max or stop value */
static Panel_item tlstop;	/* value to stop tick labels */
static Panel_item tllayout;	/* tick labels perp or horizontal or use the *
				 * angle */
static Panel_item tlangle;	/* angle */
static Panel_item tlstagger;	/* stagger tick labels */
static Panel_item tlsign;	/* sign of tick label (normal, negate, *
				 * absolute) */
static Panel_item tlappend;	/* append string to tick labels */
static Panel_item tlprepend;	/* prepend string to tick labels */
static Panel_item tlspec;	/* tick labels specified */
static Panel_item tick_applyto;	/* apply to several axes */
static Panel_item tnum;		/* number of ticks for autoscaling */
static Panel_item tgrid;	/* major ticks grid */
static Panel_item tgridcol;
static Panel_item tgridlinew;
static Panel_item tgridlines;
static Panel_item tmgrid;	/* minor ticks grid */
static Panel_item tmgridcol;
static Panel_item tmgridlinew;
static Panel_item tmgridlines;
static Panel_item tlen;		/* tick length */
static Panel_item tmlen;
static Panel_item tinout;	/* ticks in out or both */
static Panel_item tspec;	/* tick marks specified */
static Panel_item baronoff;	/* axis bar */
static Panel_item barcolor;
static Panel_item barlinew;
static Panel_item barlines;

#define TPAGESIZE 5
#define NPAGES (MAX_TICK_LABELS/TPAGESIZE)
static int tcurpage = 0;

static Panel_item specticks;	/* special ticks and tick labels */
static Panel_item specticklabels;
static Panel_item nspec;
static Panel_item specloc_item[MAX_TICK_LABELS];
static Panel_item speclabel_item[MAX_TICK_LABELS];

/*
 * Event and Notify proc declarations
 */
static void ticks_Done_notify_proc();
static void ticks_define_notify_proc();
static void do_axis_proc();
static void do_axislabel_proc();
static void do_ticklabels_proc();
static void do_tickmarks_proc();
static void do_axisbar_proc();
static void do_special_proc();
void update_ticks_items();
static void update_axis_items();
static void update_axislabel_items();
static void update_ticklabel_items();
static void update_tickmark_items();
static void update_axisbar_items();
static void update_special_items();
static void load_special();

void update_ticks(gno)
    int gno;
{
    update_ticks_items(gno);
    update_axis_items(gno);
    update_axislabel_items(gno);
    update_ticklabel_items(gno);
    update_tickmark_items(gno);
    update_axisbar_items(gno);
    load_special(gno, curaxis);
    update_special_items(gno);
}

void update_ticks_items(gno)
{
    tickmarks t;

    if (ticks_frame) {
	get_graph_tickmarks(gno, &t, curaxis);
	xv_set(tonoff, PANEL_VALUE, t.t_flag == ON, NULL);
	xv_set(baronoff, PANEL_VALUE, t.t_drawbar == ON, NULL);
	xv_set(tlonoff, PANEL_VALUE, t.tl_flag == ON, NULL);
	xv_set(axislabel, PANEL_VALUE, t.label.s, NULL);
	if (islogx(gno) && (curaxis % 2 == 0)) {
	    t.tmajor = (int) t.tmajor;
	    if (t.tmajor == 0) {
		t.tmajor = 1;
	    }
	    sprintf(buf, "%.0g", t.tmajor);
	} else if (islogy(gno) && (curaxis % 2 == 1)) {
	    t.tmajor = (int) t.tmajor;
	    if (t.tmajor == 0) {
		t.tmajor = 1;
	    }
	    sprintf(buf, "%.0g", t.tmajor);
	} else if (t.tmajor > 0) {
	    sprintf(buf, "%.5g", t.tmajor);
	} else {
	    strcpy(buf, "UNDEFINED");
	}
	xv_set(tmajor, PANEL_VALUE, buf, NULL);
	if (islogx(gno) && (curaxis % 2 == 0)) {
	    t.tminor = (int) t.tminor;
	    if (t.tminor < 0 || t.tminor > 5) {
		t.tminor = 0;
	    }
	    sprintf(buf, "%.0g", t.tminor);
	} else if (islogy(gno) && (curaxis % 2 == 1)) {
	    t.tminor = (int) t.tminor;
	    if (t.tminor < 0 || t.tminor > 5) {
		t.tminor = 0;
	    }
	    sprintf(buf, "%.0g", t.tminor);
	} else if (t.tminor > 0) {
	    sprintf(buf, "%.5g", t.tminor);
	} else {
	    strcpy(buf, "UNDEFINED");
	}
	xv_set(tminor, PANEL_VALUE, buf, NULL);
    }
}

static void set_axis_proc()
{
    void update_ticks();

    curaxis = (int) xv_get(editaxis, PANEL_VALUE);
    update_ticks(cg);
}

/*
 * Create the ticks popup
 */
void create_ticks_frame()
{
    if (ticks_frame) {
	xv_set(editaxis, PANEL_VALUE, curaxis, NULL);
	update_ticks(cg);
	xv_set(ticks_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    ticks_frame = xv_create(main_frame, FRAME,
			    XV_LABEL, "Ticks/tick labels",
			    FRAME_SHOW_LABEL, TRUE,
			    WIN_ERROR_MSG, "Couldn't create ticks_frame",
			    NULL);
    ticks_panel = xv_create(ticks_frame, PANEL,
			    XV_HELP_DATA, "xvgr:ticks_panel",
			    PANEL_LAYOUT, PANEL_VERTICAL,
			    NULL);

    editaxis = (Panel_item) xv_create(ticks_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Edit:",
				      PANEL_NOTIFY_PROC, set_axis_proc,
				      XV_HELP_DATA, "xvgr:ticks_editaxis",
				      PANEL_CHOICE_STRINGS,
				      "X axis",
				      "Y axis",
				      "Zero X axis",
				      "Zero Y axis",
				      "Alternate X axis",
				      "Alternate Y axis",
				      NULL,
				      XV_X, xv_col(ticks_panel, 0),
				      XV_Y, xv_row(ticks_panel, 0),
				      NULL);
    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Axis props...",
		     XV_HELP_DATA, "xvgr:ticks_axisprops",
		     PANEL_NOTIFY_PROC, do_axis_proc,
		     XV_X, xv_col(ticks_panel, 25),
		     XV_Y, xv_row(ticks_panel, 0),
		     NULL);
    axislabel = (Panel_item) xv_create(ticks_panel, PANEL_TEXT,
				       PANEL_LAYOUT, PANEL_HORIZONTAL,
				       PANEL_VALUE_DISPLAY_LENGTH, 25,
				       PANEL_LABEL_STRING, "Axis label",
				       XV_HELP_DATA, "xvgr:ticks_axislabel",
				       XV_X, 5,
				       XV_Y, xv_row(ticks_panel, 2),
				       NULL);
    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Axis label Props...",
		     XV_HELP_DATA, "xvgr:ticks_axislabelprops",
		     PANEL_NOTIFY_PROC, do_axislabel_proc,
		     XV_X, xv_col(ticks_panel, 0),
		     XV_Y, xv_row(ticks_panel, 3),
		     NULL);
    tmajor = (Panel_item) xv_create(ticks_panel, PANEL_TEXT,
				    PANEL_LAYOUT, PANEL_HORIZONTAL,
				    PANEL_VALUE_DISPLAY_LENGTH, 10,
				    PANEL_LABEL_STRING, "Major tick spacing",
				    XV_HELP_DATA, "xvgr:ticks_major",
				    XV_X, xv_col(ticks_panel, 0),
				    XV_Y, xv_row(ticks_panel, 5),
				    NULL);
    tminor = (Panel_item) xv_create(ticks_panel, PANEL_TEXT,
				    PANEL_LAYOUT, PANEL_HORIZONTAL,
				    PANEL_VALUE_DISPLAY_LENGTH, 10,
				    PANEL_LABEL_STRING, "Minor tick spacing",
				    XV_HELP_DATA, "xvgr:ticks_minor",
				    XV_X, xv_col(ticks_panel, 0),
				    XV_Y, xv_row(ticks_panel, 6),
				    NULL);
    tlonoff = (Panel_item) xv_create(ticks_panel, PANEL_CHECK_BOX,
				     PANEL_CHOICE_STRINGS,
				     "Tick labels",
				     NULL,
				     XV_HELP_DATA, "xvgr:ticks_ticklabeltog",
				     XV_X, xv_col(ticks_panel, 0),
				     XV_Y, xv_row(ticks_panel, 8),
				     NULL);
    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Props...",
		     XV_HELP_DATA, "xvgr:ticks_ticklabelprops",
		     PANEL_NOTIFY_PROC, do_ticklabels_proc,
		     XV_X, xv_col(ticks_panel, 15),
		     XV_Y, xv_row(ticks_panel, 8),
		     NULL);
    tonoff = (Panel_item) xv_create(ticks_panel, PANEL_CHECK_BOX,
				    PANEL_CHOICE_STRINGS,
				    "Tick marks",
				    NULL,
				    XV_HELP_DATA, "xvgr:ticks_ticktog",
				    XV_X, xv_col(ticks_panel, 0),
				    XV_Y, xv_row(ticks_panel, 9),
				    NULL);
    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Props...",
		     XV_HELP_DATA, "xvgr:ticks_tickprops",
		     PANEL_NOTIFY_PROC, do_tickmarks_proc,
		     XV_X, xv_col(ticks_panel, 15),
		     XV_Y, xv_row(ticks_panel, 9),
		     NULL);
    baronoff = (Panel_item) xv_create(ticks_panel, PANEL_CHECK_BOX,
				      PANEL_CHOICE_STRINGS,
				      "Axis bar",
				      NULL,
				      XV_HELP_DATA, "xvgr:ticks_bartog",
				      XV_X, xv_col(ticks_panel, 0),
				      XV_Y, xv_row(ticks_panel, 10),
				      NULL);
    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Props...",
		     XV_HELP_DATA, "xvgr:ticks_barprops",
		     PANEL_NOTIFY_PROC, do_axisbar_proc,
		     XV_X, xv_col(ticks_panel, 15),
		     XV_Y, xv_row(ticks_panel, 10),
		     NULL);
    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Special ticks-tick labels...",
		     XV_HELP_DATA, "xvgr:ticks_special",
		     PANEL_NOTIFY_PROC, do_special_proc,
		     XV_X, xv_col(ticks_panel, 0),
		     XV_Y, xv_row(ticks_panel, 11),
		     NULL);

    ticks_applyto = (Panel_item) xv_create(ticks_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Apply to:",
					  XV_HELP_DATA, "xvgr:ticks_applyto",
					  PANEL_CHOICE_STRINGS,
					  "Current axis",
					  "All axes, current graph",
					  "Current axis, all graphs",
					  "All axes, all graphs",
					  NULL,
					  XV_X, xv_col(ticks_panel, 0),
					  XV_Y, xv_row(ticks_panel, 12),
					  NULL);

    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Apply",
		     XV_HELP_DATA, "xvgr:ticks_apply",
		     PANEL_NOTIFY_PROC, ticks_define_notify_proc,
		     XV_X, xv_col(ticks_panel, 5),
		     XV_Y, xv_row(ticks_panel, 13),
		     NULL);
    (void) xv_create(ticks_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:ticks_done",
		     PANEL_NOTIFY_PROC, ticks_Done_notify_proc,
		     XV_X, xv_col(ticks_panel, 15),
		     XV_Y, xv_row(ticks_panel, 13),
		     NULL);
    xv_set(editaxis, PANEL_VALUE, curaxis, NULL);
    update_ticks(cg);
    window_fit(ticks_panel);
    window_fit(ticks_frame);
    xv_set(ticks_frame, WIN_SHOW, TRUE, 0);
}

static void ticks_Done_notify_proc()
{
    xv_set(ticks_frame, WIN_SHOW, FALSE, 0);
}

/*
 * define tick marks
 */
static void ticks_define_notify_proc()
{
    char val[80];
    int i, j;
    int applyto;
    extern double result;
    double x = (g[cg].w.xg2 - g[cg].w.xg1), y = (g[cg].w.yg2 - g[cg].w.yg1),
     a = g[cg].w.xg1, b = g[cg].w.yg1, c = g[cg].w.xg2, d = g[cg].w.yg2;
    int errpos;
    tickmarks t;

    get_graph_tickmarks(cg, &t, curaxis);

    applyto = (int) xv_get(ticks_applyto, PANEL_VALUE);
    strcpy(val, (char *) xv_getstr(tmajor));
    fixupstr(val);
    scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
    if (errpos) {
	return;
    }
    t.tmajor = result;
    if (islogx(cg) && (curaxis % 2 == 0)) {
	t.tmajor = (int) t.tmajor;
    } else if (islogy(cg) && (curaxis % 2 == 1)) {
	t.tmajor = (int) t.tmajor;
    }
    strcpy(val, (char *) xv_getstr(tminor));
    fixupstr(val);
    scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
    if (errpos) {
	return;
    }
    t.tminor = result;
    if (islogx(cg) && (curaxis % 2 == 0)) {
	t.tminor = (int) t.tminor;
	if (t.tminor < 0 || t.tminor > 5) {
	    t.tminor = 0;
	}
    } else if (islogy(cg) && (curaxis % 2 == 1)) {
	t.tminor = (int) t.tminor;
	if (t.tminor < 0 || t.tminor > 5) {
	    t.tminor = 0;
	}
    }
    t.tl_flag = (int) xv_get(tlonoff, PANEL_VALUE) ? ON : OFF;
    t.t_flag = (int) xv_get(tonoff, PANEL_VALUE) ? ON : OFF;
    t.t_drawbar = (int) xv_get(baronoff, PANEL_VALUE) ? ON : OFF;
    strcpy(t.label.s, (char *) xv_getstr(axislabel));

    switch (applyto) {
    case 0:			/* current axis */
	set_graph_tickmarks(cg, &t, curaxis);
	break;
    case 1:			/* all axes, current graph */
	for (i = 0; i < 6; i++) {
	    g[cg].t[i].tl_flag = t.tl_flag;
	    g[cg].t[i].t_flag = t.t_flag;
	    g[cg].t[i].t_drawbar = t.t_drawbar;
	    strcpy(g[cg].t[i].label.s, t.label.s);
	    g[cg].t[i].tmajor = t.tmajor;
	    g[cg].t[i].tminor = t.tminor;
	}
	break;
    case 2:			/* current axis, all graphs */
	for (i = 0; i < maxgraph; i++) {
	    g[i].t[curaxis].tl_flag = t.tl_flag;
	    g[i].t[curaxis].t_flag = t.t_flag;
	    g[i].t[curaxis].t_drawbar = t.t_drawbar;
	    strcpy(g[i].t[curaxis].label.s, t.label.s);
	    g[i].t[curaxis].tmajor = t.tmajor;
	    g[i].t[curaxis].tminor = t.tminor;
	}
	break;
    case 3:			/* all axes, all graphs */
	for (i = 0; i < maxgraph; i++) {
	    for (j = 0; j < 6; j++) {
		g[i].t[i].tl_flag = t.tl_flag;
		g[i].t[i].t_flag = t.t_flag;
		g[i].t[i].t_drawbar = t.t_drawbar;
		strcpy(g[i].t[i].label.s, t.label.s);
		g[i].t[i].tmajor = t.tmajor;
		g[i].t[i].tminor = t.tminor;
	    }
	}
	break;
    }
    drawgraph();
}

static Frame axis_frame;
static Panel axis_panel;
static Panel_item axis_applyto;

static props_Done_notify_proc()
{
    xv_set(axis_frame, WIN_SHOW, FALSE, 0);
}

static void accept_axis_proc(w)
    Panel_item w;
{
    tickmarks t;

    get_graph_tickmarks(cg, &t, curaxis);
    t.alt = (int) xv_get(altmap, PANEL_VALUE) ? ON : OFF;
    t.tmin = atof((char *) xv_getstr(altmin));
    t.tmax = atof((char *) xv_getstr(altmax));
    t.offsx = atof((char *) xv_getstr(offx));
    t.offsy = atof((char *) xv_getstr(offy));
    set_graph_tickmarks(cg, &t, curaxis);
    drawgraph();
}

static void update_axis_items(gno)
    int gno;
{
    tickmarks t;

    if (axis_frame) {
	get_graph_tickmarks(gno, &t, curaxis);
	xv_set(altmap, PANEL_VALUE, t.alt == ON, NULL);
	sprintf(buf, "%.5g", t.tmin);
	xv_set(altmin, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.5g", t.tmax);
	xv_set(altmax, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.5g", t.offsx);
	xv_set(offx, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.5g", t.offsy);
	xv_set(offy, PANEL_VALUE, buf, NULL);
    }
}

static void do_axis_proc()
{

    if (axis_frame) {
	update_axis_items(cg);
	xv_set(axis_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    axis_frame = xv_create(main_frame, FRAME,
			   XV_LABEL, "Axis props",
			   FRAME_SHOW_LABEL, TRUE,
			   WIN_ERROR_MSG, "Couldn't create axis_frame",
			   NULL);
    axis_panel = xv_create(axis_frame, PANEL,
			   XV_HELP_DATA, "xvgr:axisprops_panel",
			   PANEL_LAYOUT, PANEL_VERTICAL,
			   NULL);
    altmap = (Panel_item) xv_create(axis_panel, PANEL_CHECK_BOX,
				    PANEL_CHOICE_STRINGS,
				    "Use alternate map",
				    NULL,
				    XV_HELP_DATA, "xvgr:axisprops_altmap",
				    XV_X, xv_col(axis_panel, 0),
				    XV_Y, xv_row(axis_panel, 0),
				    NULL);
    altmin = (Panel_item) xv_create(axis_panel, PANEL_TEXT,
				    PANEL_VALUE_DISPLAY_LENGTH, 10,
				    PANEL_LABEL_STRING, "Alternate min:",
				    XV_HELP_DATA, "xvgr:axisprops_altmin",
				    PANEL_VALUE_X, xv_col(axis_panel, 17),
				    XV_Y, xv_row(axis_panel, 1),
				    NULL);
    altmax = (Panel_item) xv_create(axis_panel, PANEL_TEXT,
				    PANEL_VALUE_DISPLAY_LENGTH, 10,
				    PANEL_LABEL_STRING, "Alternate max:",
				    XV_HELP_DATA, "xvgr:axisprops_altmax",
				    PANEL_VALUE_X, xv_col(axis_panel, 17),
				    XV_Y, xv_row(axis_panel, 2),
				    NULL);
    (void) xv_create(axis_panel, PANEL_MESSAGE,
		  PANEL_LABEL_STRING, "Axis offset in viewport coordinates",
		     XV_X, xv_col(axis_panel, 0),
		     XV_Y, xv_row(axis_panel, 4),
		     NULL);
    offx = (Panel_item) xv_create(axis_panel, PANEL_TEXT,
				  PANEL_VALUE_DISPLAY_LENGTH, 10,
				  PANEL_LABEL_STRING, "Left or bottom:",
				  XV_HELP_DATA, "xvgr:axisprops_leftorbot",
				  PANEL_VALUE_X, xv_col(axis_panel, 17),
				  XV_Y, xv_row(axis_panel, 5),
				  NULL);
    offy = (Panel_item) xv_create(axis_panel, PANEL_TEXT,
				  PANEL_VALUE_DISPLAY_LENGTH, 10,
				  PANEL_LABEL_STRING, "Right or top:",
				  XV_HELP_DATA, "xvgr:axisprops_rightortop",
				  PANEL_VALUE_X, xv_col(axis_panel, 17),
				  XV_Y, xv_row(axis_panel, 6),
				  NULL);
    (void) xv_create(axis_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:axisprops_cancel",
		     PANEL_NOTIFY_PROC, props_Done_notify_proc,
		     XV_X, xv_col(axis_panel, 15),
		     XV_Y, xv_row(axis_panel, 8),
		     NULL);
    (void) xv_create(axis_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     XV_HELP_DATA, "xvgr:axisprops_accept",
		     PANEL_NOTIFY_PROC, accept_axis_proc,
		     XV_X, xv_col(axis_panel, 5),
		     XV_Y, xv_row(axis_panel, 8),
		     NULL);
    update_axis_items(cg);
    window_fit(axis_panel);
    window_fit(axis_frame);
    xv_set(axis_frame, WIN_SHOW, TRUE, 0);
}

static Frame axislabel_frame;
static Panel axislabel_panel;
static Panel_item axislabel_applyto;

static axislabel_Done_notify_proc()
{
    xv_set(axislabel_frame, WIN_SHOW, FALSE, 0);
}

static void accept_axislabel_proc()
{
    tickmarks t;
    int iv;

    get_graph_tickmarks(cg, &t, curaxis);
    t.label_layout = (int) xv_get(axislabellayout, PANEL_VALUE) ? PERP : PARA;
    t.label.font = (int) xv_get(axislabelfont, PANEL_VALUE);
    t.label.color = (int) xv_get(axislabelcolor, PANEL_VALUE);
    t.label.linew = (int) xv_get(axislabellinew, PANEL_VALUE) + 1;
    iv = (int) xv_get(axislabelcharsize, PANEL_VALUE);
    t.label.charsize = iv / 100.0;
    set_graph_tickmarks(cg, &t, curaxis);
    drawgraph();
}

static void update_axislabel_items(gno)
    int gno;
{
    tickmarks t;
    int iv;

    if (axislabel_frame) {
	get_graph_tickmarks(gno, &t, curaxis);
	xv_set(axislabellayout, PANEL_VALUE, t.label_layout == PERP ? 1 : 0, NULL);
	xv_set(axislabelfont, PANEL_VALUE, t.label.font, NULL);
	xv_set(axislabelcolor, PANEL_VALUE, t.label.color, NULL);
	xv_set(axislabellinew, PANEL_VALUE, t.label.linew - 1, NULL);
	iv = (int) (100 * t.label.charsize);
	xv_set(axislabelcharsize, PANEL_VALUE, iv, NULL);
    }
}

static void do_axislabel_proc()
{

    if (axislabel_frame) {
	update_axislabel_items(cg);
	xv_set(axislabel_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    axislabel_frame = xv_create(main_frame, FRAME,
				XV_LABEL, "Axis label props",
				FRAME_SHOW_LABEL, TRUE,
				WIN_ERROR_MSG, "Couldn't create axis_frame",
				NULL);
    axislabel_panel = xv_create(axislabel_frame, PANEL,
				XV_HELP_DATA, "xvgr:axislabel_panel",
				PANEL_LAYOUT, PANEL_VERTICAL,
				NULL);

    axislabellayout = (Panel_item) xv_create(axislabel_panel, PANEL_CHOICE_STACK,
					 PANEL_LABEL_STRING, "Axis layout:",
				      XV_HELP_DATA, "xvgr:axislabel_layout",
					     PANEL_CHOICE_STRINGS,
					     "Parallel to axis",
					     "Perpendicular to axis",
					     NULL,
				 PANEL_VALUE_X, xv_col(axislabel_panel, 20),
					   XV_Y, xv_row(axislabel_panel, 0),
					     NULL);

    axislabelfont = (Panel_item) xv_create(axislabel_panel, PANEL_CHOICE_STACK,
					   PANEL_LABEL_STRING, "Font:",
					XV_HELP_DATA, "xvgr:axislabel_font",
					   PANEL_CHOICE_STRINGS,
				"Times-Roman", "Times-Bold", "Times-Italic",
					   "Times-BoldItalic", "Helvetica",
				      "Helvetica-Bold", "Helvetica-Oblique",
				 "Helvetica-BoldOblique", "Greek", "Symbol",
					   NULL,
				 PANEL_VALUE_X, xv_col(axislabel_panel, 20),
					   XV_Y, xv_row(axislabel_panel, 1),
					   NULL);

    axislabellinew = (Panel_item) xv_create(axislabel_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Line width:",
				       XV_HELP_DATA, "xvgr:axislabel_linew",
					    PANEL_CHOICE_NCOLS, 3,
					    PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					    NULL,
				 PANEL_VALUE_X, xv_col(axislabel_panel, 20),
					    XV_Y, xv_row(axislabel_panel, 2),
					    NULL);

    axislabelcolor = (Panel_item) xv_create(axislabel_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Color:",
				       XV_HELP_DATA, "xvgr:axislabel_color",
					    PANEL_CHOICE_NCOLS, 4,
					    PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					    "11", "12", "13", "14", "15",
					    NULL,
				 PANEL_VALUE_X, xv_col(axislabel_panel, 20),
					    XV_Y, xv_row(axislabel_panel, 3),
					    NULL);
    axislabelcharsize = (Panel_item) xv_create(axislabel_panel, PANEL_SLIDER,
					       PANEL_SLIDER_WIDTH, 100,
					       PANEL_SHOW_VALUE, TRUE,
					       PANEL_SHOW_RANGE, FALSE,
					       PANEL_MIN_VALUE, 10,
					       PANEL_MAX_VALUE, 400,
				      PANEL_LABEL_STRING, "Character size:",
				    XV_HELP_DATA, "xvgr:axislabel_charsize",
				 PANEL_VALUE_X, xv_col(axislabel_panel, 20),
					   XV_Y, xv_row(axislabel_panel, 4),
					       NULL);

    (void) xv_create(axislabel_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     XV_HELP_DATA, "xvgr:axislabel_accept",
		     PANEL_NOTIFY_PROC, accept_axislabel_proc,
		     XV_X, xv_col(axislabel_panel, 5),
		     XV_Y, xv_row(axislabel_panel, 6),
		     NULL);
    (void) xv_create(axislabel_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:axislabel_cancel",
		     PANEL_NOTIFY_PROC, axislabel_Done_notify_proc,
		     XV_X, xv_col(axislabel_panel, 15),
		     XV_Y, xv_row(axislabel_panel, 6),
		     NULL);
    update_axislabel_items(cg);
    window_fit(axislabel_panel);
    window_fit(axislabel_frame);
    xv_set(axislabel_frame, WIN_SHOW, TRUE, 0);
}

static Panel_item ticklabel_frame;
static Panel_item ticklabel_panel;

static ticklabel_Done_notify_proc()
{
    xv_set(ticklabel_frame, WIN_SHOW, FALSE, 0);
}

static void accept_ticklabel_proc()
{
    tickmarks t;
    int iv;
    int i, j, applyto, gstart, gstop, astart, astop;;
    applyto = xv_get(ticklabel_applyto, PANEL_VALUE);
    switch (applyto) {
    case 0:
	gstart = gstop = cg;
	astart = astop = curaxis;
	break;
    case 1:
	gstart = gstop = cg;
	astart = 0;
	astop = 5;
	break;
    case 2:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = astop = curaxis;
	break;
    case 3:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = 0;
	astop = 5;
	break;
    }
    for (i = gstart; i <= gstop; i++) {
	for (j = astart; j <= astop; j++) {
	    get_graph_tickmarks(i, &t, j);
	    t.tl_font = (int) xv_get(tlfont, PANEL_VALUE);
	    t.tl_color = (int) xv_get(tlcolor, PANEL_VALUE);
	    t.tl_linew = (int) xv_get(tllinew, PANEL_VALUE) + 1;
	    t.tl_skip = (int) xv_get(tlskip, PANEL_VALUE);
	    t.tl_prec = (int) xv_get(tlprec, PANEL_VALUE);
	    t.tl_staggered = (int) xv_get(tlstagger, PANEL_VALUE);
	    t.tl_starttype = (int) xv_get(tlstarttype, PANEL_VALUE) == 0 ? AUTO : SPEC;
	    if (t.tl_starttype == SPEC) {
		t.tl_start = atof((char *) xv_get(tlstart, PANEL_VALUE));
	    }
	    t.tl_stoptype = (int) xv_get(tlstoptype, PANEL_VALUE) == 0 ? AUTO : SPEC;
	    if (t.tl_stoptype == SPEC) {
		t.tl_stop = atof((char *) xv_get(tlstop, PANEL_VALUE));
	    }
	    strcpy(t.tl_appstr, (char *) xv_get(tlappend, PANEL_VALUE));
	    strcpy(t.tl_prestr, (char *) xv_get(tlprepend, PANEL_VALUE));
	    t.tl_format = format_types[(int) xv_get(tlform, PANEL_VALUE)];
	    switch ((int) xv_get(ticklop, PANEL_VALUE)) {
	    case 0:
		if (j % 2) {
		    t.tl_op = LEFT;
		} else {
		    t.tl_op = BOTTOM;
		}
		break;
	    case 1:
		if (j % 2) {
		    t.tl_op = RIGHT;
		} else {
		    t.tl_op = TOP;
		}
		break;
	    case 2:
		t.tl_op = BOTH;
		break;
	    }
	    switch ((int) xv_get(tlsign, PANEL_VALUE)) {
	    case 0:
		t.tl_sign = NORMAL;
		break;
	    case 1:
		t.tl_sign = ABSOLUTE;
		break;
	    case 2:
		t.tl_sign = NEGATE;
		break;
	    }
	    switch ((int) xv_get(tllayout, PANEL_VALUE)) {
	    case 0:
		t.tl_layout = HORIZONTAL;
		break;
	    case 1:
		t.tl_layout = VERTICAL;
		break;
	    case 2:
		t.tl_layout = SPEC;
		t.tl_angle = (int) xv_get(tlangle, PANEL_VALUE);
		break;
	    }
	    iv = (int) xv_get(tlcharsize, PANEL_VALUE);
	    t.tl_charsize = iv / 100.0;
	    set_graph_tickmarks(i, &t, j);
	}
    }
    drawgraph();
}

static void update_ticklabel_items(gno)
    int gno;
{
    tickmarks t;
    int iv;

    if (ticklabel_frame) {
	get_graph_tickmarks(gno, &t, curaxis);
	xv_set(tlfont, PANEL_VALUE, t.tl_font, NULL);
	xv_set(tlcolor, PANEL_VALUE, t.tl_color, NULL);
	xv_set(tllinew, PANEL_VALUE, t.tl_linew - 1, NULL);
	xv_set(tlskip, PANEL_VALUE, t.tl_skip, NULL);
	xv_set(tlstagger, PANEL_VALUE, t.tl_staggered, NULL);
	xv_set(tlstarttype, PANEL_VALUE, t.tl_starttype == SPEC, NULL);
	if (t.tl_starttype == SPEC) {
	    sprintf(buf, "%lf", t.tl_start);
	    xv_set(tlstart, PANEL_VALUE, buf, NULL);
	    sprintf(buf, "%lf", t.tl_stop);
	    xv_set(tlstop, PANEL_VALUE, buf, NULL);
	}
	xv_set(tlstoptype, PANEL_VALUE, t.tl_stoptype == SPEC, NULL);
	if (t.tl_stoptype == SPEC) {
	    sprintf(buf, "%lf", t.tl_stop);
	    xv_set(tlstop, PANEL_VALUE, buf, NULL);
	}
	xv_set(tlappend, PANEL_VALUE, t.tl_appstr, NULL);
	xv_set(tlprepend, PANEL_VALUE, t.tl_prestr, NULL);
	iv = get_format_index(t.tl_format);
	xv_set(tlform, PANEL_VALUE, iv, NULL);
	switch (t.tl_op) {
	case LEFT:
	    xv_set(ticklop, PANEL_VALUE, 0, NULL);
	    break;
	case RIGHT:
	    xv_set(ticklop, PANEL_VALUE, 1, NULL);
	    break;
	case BOTTOM:
	    xv_set(ticklop, PANEL_VALUE, 0, NULL);
	    break;
	case TOP:
	    xv_set(ticklop, PANEL_VALUE, 1, NULL);
	    break;
	case BOTH:
	    xv_set(ticklop, PANEL_VALUE, 2, NULL);
	    break;
	}
	switch (t.tl_sign) {
	case NORMAL:
	    xv_set(tlsign, PANEL_VALUE, 0, NULL);
	    break;
	case ABSOLUTE:
	    xv_set(tlsign, PANEL_VALUE, 1, NULL);
	    break;
	case NEGATE:
	    xv_set(tlsign, PANEL_VALUE, 2, NULL);
	    break;
	}
	xv_set(tlprec, PANEL_VALUE, t.tl_prec, NULL);
	iv = (int) (100 * t.tl_charsize);
	xv_set(tlcharsize, PANEL_VALUE, iv, NULL);
	switch (t.tl_layout) {
	case HORIZONTAL:
	    xv_set(tllayout, PANEL_VALUE, 0, NULL);
	    break;
	case VERTICAL:
	    xv_set(tllayout, PANEL_VALUE, 1, NULL);
	    break;
	case SPEC:
	    xv_set(tllayout, PANEL_VALUE, 2, NULL);
	    break;
	}
	xv_set(tlangle, PANEL_VALUE, (int) (t.tl_angle) % 360, NULL);
    }
}

static void do_ticklabels_proc()
{
    int row = 0;

    if (ticklabel_frame) {
	update_ticklabel_items(cg);
	xv_set(ticklabel_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    ticklabel_frame = xv_create(main_frame, FRAME,
				XV_LABEL, "Tick labels",
				FRAME_SHOW_LABEL, TRUE,
			   WIN_ERROR_MSG, "Couldn't create ticklabel_frame",
				NULL);
    ticklabel_panel = xv_create(ticklabel_frame, PANEL,
				XV_HELP_DATA, "xvgr:ticklabel_panel",
				PANEL_LAYOUT, PANEL_VERTICAL,
				NULL);

    tlfont = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				    PANEL_LABEL_STRING, "Font:",
				    XV_HELP_DATA, "xvgr:ticklabel_font",
				    PANEL_CHOICE_STRINGS,
				"Times-Roman", "Times-Bold", "Times-Italic",
				    "Times-BoldItalic", "Helvetica",
				    "Helvetica-Bold", "Helvetica-Oblique",
				 "Helvetica-BoldOblique", "Greek", "Symbol",
				    NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				    XV_Y, xv_row(ticklabel_panel, row++),
				    NULL);

    tlcolor = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				     PANEL_LABEL_STRING, "Color:",
				     XV_HELP_DATA, "xvgr:ticklabel_color",
				     PANEL_CHOICE_NCOLS, 4,
				     PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				     "11", "12", "13", "14", "15",
				     NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				     XV_Y, xv_row(ticklabel_panel, row++),
				     NULL);

    tllinew = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				     PANEL_LABEL_STRING, "Line width:",
				     XV_HELP_DATA, "xvgr:ticklabel_linew",
				     PANEL_CHOICE_NCOLS, 3,
				     PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
				     NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				     XV_Y, xv_row(ticklabel_panel, row++),
				     NULL);
    tlcharsize = (Panel_item) xv_create(ticklabel_panel, PANEL_SLIDER,
					PANEL_SLIDER_WIDTH, 100,
					PANEL_SHOW_VALUE, TRUE,
					PANEL_SHOW_RANGE, FALSE,
					PANEL_MIN_VALUE, 0,
					PANEL_MAX_VALUE, 400,
					PANEL_LABEL_STRING, "Char size:",
				    XV_HELP_DATA, "xvgr:ticklabel_charsize",
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
					XV_Y, xv_row(ticklabel_panel, row++),
					NULL);

    tlform = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				    PANEL_LABEL_STRING, "Format:",
				    XV_HELP_DATA, "xvgr:ticklabel_format",
				    PANEL_CHOICE_NCOLS, 3,
				    PANEL_CHOICE_STRINGS,
				    "Decimal",
				    "Exponential",
				    "Power",
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
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				    XV_Y, xv_row(ticklabel_panel, row++),
				    NULL);

    tlstagger = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				       PANEL_LABEL_STRING, "Stagger:",
				     XV_HELP_DATA, "xvgr:ticklabel_stagger",
				       PANEL_CHOICE_STRINGS,
				       "0", "1", "2", "3",
				       NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				       XV_Y, xv_row(ticklabel_panel, row++),
				       NULL);

    tlprec = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				    PANEL_LABEL_STRING, "Precision:",
				    XV_HELP_DATA, "xvgr:ticklabel_precision",
				    PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				    NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				    XV_Y, xv_row(ticklabel_panel, row++),
				    NULL);

    tlskip = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				    PANEL_LABEL_STRING, "Skip every:",
				    XV_HELP_DATA, "xvgr:ticklabel_skip",
				    PANEL_CHOICE_STRINGS,
	      "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Specified",
				    NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				    XV_Y, xv_row(ticklabel_panel, row++),
				    NULL);

    tlstarttype = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				     PANEL_LABEL_STRING, "Start labels at:",
				     XV_HELP_DATA, "xvgr:ticklabel_startat",
					 PANEL_CHOICE_STRINGS,
					 "Graph min", "Specified:",
					 NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
					 XV_Y, xv_row(ticklabel_panel, row),
					 NULL);
    tlstart = (Panel_item) xv_create(ticklabel_panel, PANEL_TEXT,
				     XV_HELP_DATA, "xvgr:ticklabel_start",
				     PANEL_VALUE_DISPLAY_LENGTH, 10,
				     XV_X, xv_col(ticklabel_panel, 35),
				     XV_Y, xv_row(ticklabel_panel, row++),
				     NULL);

    tlstoptype = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Stop labels at:",
				      XV_HELP_DATA, "xvgr:ticklabel_stopat",
					PANEL_CHOICE_STRINGS,
					"Graph max", "Specified:",
					NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
					XV_Y, xv_row(ticklabel_panel, row),
					NULL);
    tlstop = (Panel_item) xv_create(ticklabel_panel, PANEL_TEXT,
				    PANEL_VALUE_DISPLAY_LENGTH, 10,
				    XV_HELP_DATA, "xvgr:ticklabel_stop",
				    XV_X, xv_col(ticklabel_panel, 35),
				    XV_Y, xv_row(ticklabel_panel, row++),
				    NULL);

    tllayout = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Layout:",
				      XV_HELP_DATA, "xvgr:ticklabel_layout",
				      PANEL_CHOICE_STRINGS,
				      "Horizontal",
				      "Vertical",
				      "Specified (deg):",
				      NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				      XV_Y, xv_row(ticklabel_panel, row),
				      NULL);
    tlangle = (Panel_item) xv_create(ticklabel_panel, PANEL_SLIDER,
				     XV_HELP_DATA, "xvgr:ticklabel_angle",
				     PANEL_SLIDER_WIDTH, 100,
				     PANEL_SHOW_VALUE, TRUE,
				     PANEL_SHOW_RANGE, FALSE,
				     PANEL_MIN_VALUE, 0,
				     PANEL_MAX_VALUE, 360,
				     XV_X, xv_col(ticklabel_panel, 35),
				     XV_Y, xv_row(ticklabel_panel, row++),
				     NULL);

    ticklop = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				     PANEL_LABEL_STRING, "Draw tick labels:",
				     XV_HELP_DATA, "xvgr:ticklabel_op",
				     PANEL_CHOICE_STRINGS,
			       "Normal side", "Opposite side", "Both sides",
				     NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				     XV_Y, xv_row(ticklabel_panel, row++),
				     NULL);
    tlsign = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
				    PANEL_LABEL_STRING, "Sign:",
				    XV_HELP_DATA, "xvgr:ticklabel_sign",
				    PANEL_CHOICE_STRINGS,
				    "As is", "Absolute value", "Negate",
				    NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				    XV_Y, xv_row(ticklabel_panel, row++),
				    NULL);
    tlprepend = (Panel_item) xv_create(ticklabel_panel, PANEL_TEXT,
				       PANEL_LABEL_STRING, "Prepend string:",
				     XV_HELP_DATA, "xvgr:ticklabel_prepend",
				       PANEL_VALUE_DISPLAY_LENGTH, 10,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				       XV_Y, xv_row(ticklabel_panel, row++),
				       NULL);
    tlappend = (Panel_item) xv_create(ticklabel_panel, PANEL_TEXT,
				      PANEL_LABEL_STRING, "Append string:",
				      XV_HELP_DATA, "xvgr:ticklabel_append",
				      PANEL_VALUE_DISPLAY_LENGTH, 10,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				      XV_Y, xv_row(ticklabel_panel, row++),
				      NULL);
    ticklabel_applyto = (Panel_item) xv_create(ticklabel_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "Apply to:",
				     XV_HELP_DATA, "xvgr:ticklabel_applyto",
					       PANEL_CHOICE_STRINGS,
					       "Current axis",
					       "All axes, current graph",
					       "Current axis, all graphs",
					       "All axes, all graphs",
					       NULL,
				 PANEL_VALUE_X, xv_col(ticklabel_panel, 20),
				       XV_Y, xv_row(ticklabel_panel, row++),
					       NULL);
    (void) xv_create(ticklabel_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:ticklabel_cancel",
		     PANEL_NOTIFY_PROC, ticklabel_Done_notify_proc,
		     XV_X, xv_col(ticklabel_panel, 15),
		     XV_Y, xv_row(ticklabel_panel, row),
		     NULL);
    (void) xv_create(ticklabel_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     XV_HELP_DATA, "xvgr:ticklabel_accept",
		     PANEL_NOTIFY_PROC, accept_ticklabel_proc,
		     XV_X, xv_col(ticklabel_panel, 5),
		     XV_Y, xv_row(ticklabel_panel, row),
		     NULL);

    update_ticklabel_items(cg);
    window_fit(ticklabel_panel);
    window_fit(ticklabel_frame);
    xv_set(ticklabel_frame, WIN_SHOW, TRUE, 0);
}

static Panel_item tickmark_frame;
static Panel_item tickmark_panel;

static tickmark_Done_notify_proc()
{
    xv_set(tickmark_frame, WIN_SHOW, FALSE, 0);
}

static void accept_tickmark_proc()
{
    tickmarks t;
    int iv;
    int i, j, applyto, gstart, gstop, astart, astop;;
    applyto = xv_get(tick_applyto, PANEL_VALUE);
    switch (applyto) {
    case 0:
	gstart = gstop = cg;
	astart = astop = curaxis;
	break;
    case 1:
	gstart = gstop = cg;
	astart = 0;
	astop = 5;
	break;
    case 2:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = astop = curaxis;
	break;
    case 3:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = 0;
	astop = 5;
	break;
    }
    for (i = gstart; i <= gstop; i++) {
	for (j = astart; j <= astop; j++) {
	    get_graph_tickmarks(i, &t, j);
	    switch ((int) xv_get(tinout, PANEL_VALUE)) {
	    case 0:
		t.t_inout = IN;
		break;
	    case 1:
		t.t_inout = OUT;
		break;
	    case 2:
		t.t_inout = BOTH;
		break;
	    }
	    switch ((int) xv_get(tickop, PANEL_VALUE)) {
	    case 0:
		if (j % 2) {
		    t.t_op = LEFT;
		} else {
		    t.t_op = BOTTOM;
		}
		break;
	    case 1:
		if (j % 2) {
		    t.t_op = RIGHT;
		} else {
		    t.t_op = TOP;
		}
		break;
	    case 2:
		t.t_op = BOTH;
		break;
	    }
	    t.t_color = (int) xv_get(tgridcol, PANEL_VALUE);
	    t.t_linew = (int) xv_get(tgridlinew, PANEL_VALUE) + 1;
	    t.t_lines = (int) xv_get(tgridlines, PANEL_VALUE) + 1;
	    t.t_mcolor = (int) xv_get(tmgridcol, PANEL_VALUE);
	    t.t_mlinew = (int) xv_get(tmgridlinew, PANEL_VALUE) + 1;
	    t.t_mlines = (int) xv_get(tmgridlines, PANEL_VALUE) + 1;
	    iv = (int) xv_get(tlen, PANEL_VALUE);
	    t.t_size = iv / 100.0;
	    iv = (int) xv_get(tmlen, PANEL_VALUE);
	    t.t_msize = iv / 100.0;
	    t.t_gridflag = (int) xv_get(tgrid, PANEL_VALUE) ? ON : OFF;
	    t.t_mgridflag = (int) xv_get(tmgrid, PANEL_VALUE) ? ON : OFF;

	    set_graph_tickmarks(i, &t, j);
	}
    }
    drawgraph();
}

static void update_tickmark_items(gno)
    int gno;
{
    tickmarks t;
    int iv;

    if (tickmark_frame) {
	get_graph_tickmarks(gno, &t, curaxis);
	switch (t.t_inout) {
	case IN:
	    xv_set(tinout, PANEL_VALUE, 0, NULL);
	    break;
	case OUT:
	    xv_set(tinout, PANEL_VALUE, 1, NULL);
	    break;
	case BOTH:
	    xv_set(tinout, PANEL_VALUE, 2, NULL);
	    break;
	}
	switch (t.t_op) {
	case LEFT:
	    xv_set(tickop, PANEL_VALUE, 0, NULL);
	    break;
	case RIGHT:
	    xv_set(tickop, PANEL_VALUE, 1, NULL);
	    break;
	case BOTTOM:
	    xv_set(tickop, PANEL_VALUE, 0, NULL);
	    break;
	case TOP:
	    xv_set(tickop, PANEL_VALUE, 1, NULL);
	    break;
	case BOTH:
	    xv_set(tickop, PANEL_VALUE, 2, NULL);
	    break;
	}
	xv_set(tgridcol, PANEL_VALUE, t.t_color, NULL);
	xv_set(tgridlinew, PANEL_VALUE, t.t_linew - 1, NULL);
	xv_set(tgridlines, PANEL_VALUE, t.t_lines - 1, NULL);
	xv_set(tmgridcol, PANEL_VALUE, t.t_mcolor, NULL);
	xv_set(tmgridlinew, PANEL_VALUE, t.t_mlinew - 1, NULL);
	xv_set(tmgridlines, PANEL_VALUE, t.t_mlines - 1, NULL);
	iv = (int) (100 * t.t_size);
	xv_set(tlen, PANEL_VALUE, iv, NULL);
	iv = (int) (100 * t.t_msize);
	xv_set(tmlen, PANEL_VALUE, iv, NULL);
	xv_set(tgrid, PANEL_VALUE, t.t_gridflag == ON, NULL);
	xv_set(tmgrid, PANEL_VALUE, t.t_mgridflag == ON, NULL);
    }
}

static void do_tickmarks_proc()
{
    if (tickmark_frame) {
	update_tickmark_items(cg);
	xv_set(tickmark_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    tickmark_frame = xv_create(main_frame, FRAME,
			       XV_LABEL, "Tick marks",
			       FRAME_SHOW_LABEL, TRUE,
			    WIN_ERROR_MSG, "Couldn't create tickmark_frame",
			       NULL);
    tickmark_panel = xv_create(tickmark_frame, PANEL,
			       XV_HELP_DATA, "xvgr:tick_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);

    tinout = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
				PANEL_LABEL_STRING, "Tick marks direction:",
				    XV_HELP_DATA, "xvgr:tick_dir",
				    PANEL_CHOICE_STRINGS,
				    "In", "Out", "Both",
				    NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 22),
				    XV_Y, xv_row(tickmark_panel, 0),
				    NULL);

    tickop = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
				    PANEL_LABEL_STRING, "Tick marks on:",
				    XV_HELP_DATA, "xvgr:tick_op",
				    PANEL_CHOICE_STRINGS,
			       "Normal side", "Opposite side", "Both sides",
				    NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 22),
				    XV_Y, xv_row(tickmark_panel, 1),
				    NULL);
    tlen = (Panel_item) xv_create(tickmark_panel, PANEL_SLIDER,
				  PANEL_LABEL_STRING, "Major tick length:",
				  XV_HELP_DATA, "xvgr:tick_majorlen",
				  PANEL_SLIDER_WIDTH, 100,
				  PANEL_SHOW_VALUE, TRUE,
				  PANEL_SHOW_RANGE, FALSE,
				  PANEL_MIN_VALUE, 0,
				  PANEL_MAX_VALUE, 400,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 22),
				  XV_Y, xv_row(tickmark_panel, 2),
				  NULL);
    tmlen = (Panel_item) xv_create(tickmark_panel, PANEL_SLIDER,
				   PANEL_LABEL_STRING, "Minor tick length:",
				   XV_HELP_DATA, "xvgr:tick_minorlen",
				   PANEL_SLIDER_WIDTH, 100,
				   PANEL_SHOW_VALUE, TRUE,
				   PANEL_SHOW_RANGE, FALSE,
				   PANEL_MIN_VALUE, 0,
				   PANEL_MAX_VALUE, 400,
				   PANEL_VALUE_X, xv_col(tickmark_panel, 22),
				   XV_Y, xv_row(tickmark_panel, 3),
				   NULL);

    tgrid = (Panel_item) xv_create(tickmark_panel, PANEL_CHECK_BOX,
				   XV_HELP_DATA, "xvgr:tick_majorgrid",
				   PANEL_CHOICE_STRINGS,
				   "Major grid lines",
				   NULL,
				   XV_X, xv_col(tickmark_panel, 0),
				   XV_Y, xv_row(tickmark_panel, 4),
				   NULL);
    tmgrid = (Panel_item) xv_create(tickmark_panel, PANEL_CHECK_BOX,
				    XV_HELP_DATA, "xvgr:tick_minorgrid",
				    PANEL_CHOICE_STRINGS,
				    "Minor grid lines",
				    NULL,
				    XV_X, xv_col(tickmark_panel, 25),
				    XV_Y, xv_row(tickmark_panel, 4),
				    NULL);

    tgridcol = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Color:",
				      XV_HELP_DATA, "xvgr:tick_majorcolor",
				      PANEL_CHOICE_NCOLS, 4,
				      PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				      "11", "12", "13", "14", "15",
				      NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 12),
				      XV_Y, xv_row(tickmark_panel, 5),
				      NULL);

    tgridlinew = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
					PANEL_LABEL_STRING, "Line width:",
					XV_HELP_DATA, "xvgr:tick_majorlinew",
					PANEL_CHOICE_NCOLS, 3,
					PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 12),
					XV_Y, xv_row(tickmark_panel, 6),
					NULL);

    tgridlines = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
					PANEL_LABEL_STRING, "Line style:",
					XV_HELP_DATA, "xvgr:tick_majorlines",
					PANEL_CHOICE_STRINGS,
					"Solid line",
					"Dotted line",
					"Dashed line",
					"Long Dashed",
					"Dot-dashed",
					NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 12),
					XV_Y, xv_row(tickmark_panel, 7),
					NULL);

    tmgridcol = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
				       PANEL_LABEL_STRING, "Color:",
				       XV_HELP_DATA, "xvgr:tick_minorcolor",
				       PANEL_CHOICE_NCOLS, 4,
				       PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				       "11", "12", "13", "14", "15",
				       NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 37),
				       XV_Y, xv_row(tickmark_panel, 5),
				       NULL);

    tmgridlinew = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
					 PANEL_LABEL_STRING, "Line width:",
				       XV_HELP_DATA, "xvgr:tick_minorlinew",
					 PANEL_CHOICE_NCOLS, 3,
					 PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					 NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 37),
					 XV_Y, xv_row(tickmark_panel, 6),
					 NULL);

    tmgridlines = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
					 PANEL_LABEL_STRING, "Line style:",
				       XV_HELP_DATA, "xvgr:tick_minorlines",
					 PANEL_CHOICE_STRINGS,
					 "Solid line",
					 "Dotted line",
					 "Dashed line",
					 "Long Dashed",
					 "Dot-dashed",
					 NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 37),
					 XV_Y, xv_row(tickmark_panel, 7),
					 NULL);
    tick_applyto = (Panel_item) xv_create(tickmark_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Apply to:",
					  XV_HELP_DATA, "xvgr:tick_applyto",
					  PANEL_CHOICE_STRINGS,
					  "Current axis",
					  "All axes, current graph",
					  "Current axis, all graphs",
					  "All axes, all graphs",
					  NULL,
				  PANEL_VALUE_X, xv_col(tickmark_panel, 15),
					  XV_Y, xv_row(tickmark_panel, 8),
					  NULL);
    (void) xv_create(tickmark_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:tick_cancel",
		     PANEL_NOTIFY_PROC, tickmark_Done_notify_proc,
		     XV_X, xv_col(tickmark_panel, 15),
		     XV_Y, xv_row(tickmark_panel, 9),
		     NULL);
    (void) xv_create(tickmark_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     XV_HELP_DATA, "xvgr:tick_accept",
		     PANEL_NOTIFY_PROC, accept_tickmark_proc,
		     XV_X, xv_col(tickmark_panel, 5),
		     XV_Y, xv_row(tickmark_panel, 9),
		     NULL);

    update_tickmark_items(cg);
    window_fit(tickmark_panel);
    window_fit(tickmark_frame);
    xv_set(tickmark_frame, WIN_SHOW, TRUE, 0);
}

static Frame axisbar_frame;
static Panel axisbar_panel;
static Panel_item axisbar_applyto;

static axisbar_Done_notify_proc()
{
    xv_set(axisbar_frame, WIN_SHOW, FALSE, 0);
}

static void accept_axisbar_proc(w)
    Panel_item w;
{
    tickmarks t;
    int i, j, applyto, gstart, gstop, astart, astop;;
    applyto = xv_get(axisbar_applyto, PANEL_VALUE);
    switch (applyto) {
    case 0:
	gstart = gstop = cg;
	astart = astop = curaxis;
	break;
    case 1:
	gstart = gstop = cg;
	astart = 0;
	astop = 5;
	break;
    case 2:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = astop = curaxis;
	break;
    case 3:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = 0;
	astop = 5;
	break;
    }
    for (i = gstart; i <= gstop; i++) {
	for (j = astart; j <= astop; j++) {
	    get_graph_tickmarks(i, &t, j);
	    t.t_drawbarcolor = (int) xv_get(barcolor, PANEL_VALUE);
	    t.t_drawbarlinew = (int) xv_get(barlinew, PANEL_VALUE) + 1;
	    t.t_drawbarlines = (int) xv_get(barlines, PANEL_VALUE) + 1;
	    set_graph_tickmarks(i, &t, j);
	}
    }
    drawgraph();
}

static void update_axisbar_items(gno)
    int gno;
{
    tickmarks t;

    if (axisbar_frame) {
	get_graph_tickmarks(gno, &t, curaxis);
	xv_set(barcolor, PANEL_VALUE, t.t_drawbarcolor, NULL);
	xv_set(barlinew, PANEL_VALUE, t.t_drawbarlinew - 1, NULL);
	xv_set(barlines, PANEL_VALUE, t.t_drawbarlines - 1, NULL);
    }
}

static void do_axisbar_proc()
{
    int i;
    char buf[128];

    if (axisbar_frame) {
	update_axisbar_items(cg);
	xv_set(axisbar_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    axisbar_frame = xv_create(main_frame, FRAME,
			      XV_LABEL, "Axis bar",
			      FRAME_SHOW_LABEL, TRUE,
			      WIN_ERROR_MSG, "Couldn't create axisbar_frame",
			      NULL);
    axisbar_panel = xv_create(axisbar_frame, PANEL,
			      XV_HELP_DATA, "xvgr:axisbar_panel",
			      PANEL_LAYOUT, PANEL_VERTICAL,
			      NULL);

    barcolor = (Panel_item) xv_create(axisbar_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Color:",
				      XV_HELP_DATA, "xvgr:axisbar_color",
				      PANEL_CHOICE_NCOLS, 4,
				      PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				      "11", "12", "13", "14", "15",
				      NULL,
				   PANEL_VALUE_X, xv_col(axisbar_panel, 15),
				      NULL);

    barlinew = (Panel_item) xv_create(axisbar_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Line width:",
				      XV_HELP_DATA, "xvgr:axisbar_linew",
				      PANEL_CHOICE_NCOLS, 3,
				      PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
				      NULL,
				   PANEL_VALUE_X, xv_col(axisbar_panel, 15),
				      NULL);

    barlines = (Panel_item) xv_create(axisbar_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "Line style:",
				      XV_HELP_DATA, "xvgr:axisbar_lines",
				      PANEL_CHOICE_STRINGS,
				      "Solid line",
				      "Dotted line",
				      "Dashed line",
				      "Long Dashed",
				      "Dot-dashed",
				      NULL,
				   PANEL_VALUE_X, xv_col(axisbar_panel, 15),
				      NULL);
    axisbar_applyto = (Panel_item) xv_create(axisbar_panel, PANEL_CHOICE_STACK,
					     PANEL_LABEL_STRING, "Apply to:",
				       XV_HELP_DATA, "xvgr:axisbar_applyto",
					     PANEL_CHOICE_STRINGS,
					     "Current axis",
					     "All axes, current graph",
					     "Current axis, all graphs",
					     "All axes, all graphs",
					     NULL,
				   PANEL_VALUE_X, xv_col(axisbar_panel, 15),
					     NULL);
    (void) xv_create(axisbar_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:axisbar_cancel",
		     PANEL_NOTIFY_PROC, axisbar_Done_notify_proc,
		     XV_X, xv_col(axisbar_panel, 15),
		     XV_Y, xv_row(axisbar_panel, 6),
		     NULL);
    (void) xv_create(axisbar_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     XV_HELP_DATA, "xvgr:axisbar_accept",
		     PANEL_NOTIFY_PROC, accept_axisbar_proc,
		     XV_X, xv_col(axisbar_panel, 5),
		     XV_Y, xv_row(axisbar_panel, 6),
		     NULL);
    update_axisbar_items(cg);
    window_fit(axisbar_panel);
    window_fit(axisbar_frame);
    xv_set(axisbar_frame, WIN_SHOW, TRUE, 0);
}

static Frame special_frame;
static Panel special_panel;
static Panel_item spec_applyto;

static char savelab[MAX_TICK_LABELS][80];
static char saveloc[MAX_TICK_LABELS][30];

static special_Done_notify_proc()
{
    xv_set(special_frame, WIN_SHOW, FALSE, 0);
}

static void accept_special_proc()
{
    tickmarks t;
    int iv;
    int i, j, k, applyto, gstart, gstop, astart, astop;

    applyto = (int) xv_get(spec_applyto, PANEL_VALUE);
    switch (applyto) {
    case 0:
	gstart = gstop = cg;
	astart = astop = curaxis;
	break;
    case 1:
	gstart = gstop = cg;
	astart = 0;
	astop = 5;
	break;
    case 2:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = astop = curaxis;
	break;
    case 3:
	gstart = 0;
	gstop = maxgraph - 1;
	astart = 0;
	astop = 5;
	break;
    }
    for (k = gstart; k <= gstop; k++) {
	for (j = astart; j <= astop; j++) {
	    get_graph_tickmarks(k, &t, j);
	    t.t_type = (int) xv_get(specticks, PANEL_VALUE) ? SPEC : AUTO;
	    t.tl_type = (int) xv_get(specticklabels, PANEL_VALUE) ? SPEC : AUTO;
	    iv = atoi((char *) xv_getstr(nspec));
	    if (iv > MAX_TICK_LABELS) {
		sprintf(buf, "Number of ticks/tick labels exceeds %d", MAX_TICK_LABELS);
		errwin(buf);
		return;
	    }
	    t.t_spec = iv;
	    for (i = 0; i < MAX_TICK_LABELS; i++) {
		if (t.t_type == SPEC) {
		    t.t_specloc[i] = atof((char *) xv_getstr(specloc_item[i]));
		}
		if (t.tl_type == SPEC) {
		    strcpy(t.t_speclab[i].s, (char *) xv_getstr(speclabel_item[i]));
		}
	    }
	    set_graph_tickmarks(k, &t, j);
	}
    }
    drawgraph();
}

static void update_special_items(gno)
    int gno;
{
    tickmarks t;
    int i, j, itmp;

    if (special_frame) {
	get_graph_tickmarks(gno, &t, curaxis);
	xv_set(specticks, PANEL_VALUE, t.t_type == SPEC, NULL);
	xv_set(specticklabels, PANEL_VALUE, t.tl_type == SPEC, NULL);
	j = 0;
	if (tcurpage == 0) {
	    itmp = (NPAGES - 1) * TPAGESIZE;
	} else {
	    itmp = (tcurpage - 1) * TPAGESIZE;
	}
	for (i = tcurpage * TPAGESIZE; i < TPAGESIZE * (tcurpage + 1); i++) {
	    xv_set(specloc_item[itmp], XV_SHOW, FALSE, NULL);
	    xv_set(speclabel_item[itmp++], XV_SHOW, FALSE, NULL);
	    xv_set(specloc_item[i], XV_SHOW, TRUE, NULL);
	    xv_set(speclabel_item[i], XV_SHOW, TRUE, NULL);
	}
    }
}

static void load_special(gno, a)
    int gno, a;
{
    int i;
    char buf[128];
    tickmarks t;

    if (special_frame) {
	get_graph_tickmarks(gno, &t, a);
	sprintf(buf, "%d", t.t_spec);
	xv_set(nspec, PANEL_VALUE, buf, NULL);
	for (i = 0; i < t.t_spec; i++) {
	    sprintf(buf, "%lf", t.t_specloc[i]);
	    xv_setstr(specloc_item[i], buf);
	    xv_setstr(speclabel_item[i], t.t_speclab[i].s);
	}
    }
}

static void page_special_notify_proc()
{
    tcurpage = (tcurpage + 1) % NPAGES;
    update_special_items(cg);
}

static void do_special_proc()
{
    int i;
    char buf[128];

    if (special_frame) {
	update_special_items(cg);
	xv_set(special_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    special_frame = xv_create(main_frame, FRAME,
			      XV_LABEL, "Specified ticks/labels",
			      FRAME_SHOW_LABEL, TRUE,
			      WIN_ERROR_MSG, "Couldn't create special_frame",
			      NULL);
    special_panel = xv_create(special_frame, PANEL,
			      XV_HELP_DATA, "xvgr:specialticks_panel",
			      PANEL_LAYOUT, PANEL_VERTICAL,
			      NULL);

    specticks = (Panel_item) xv_create(special_panel, PANEL_CHECK_BOX,
				 XV_HELP_DATA, "xvgr:specialticks_useticks",
				       PANEL_CHOICE_STRINGS,
				       "Use specified tick locations",
				       NULL,
				       XV_X, xv_col(special_panel, 0),
				       NULL);
    specticklabels = (Panel_item) xv_create(special_panel, PANEL_CHECK_BOX,
			    XV_HELP_DATA, "xvgr:specialticks_useticklabels",
					    PANEL_CHOICE_STRINGS,
					    "Use specified tick labels",
					    NULL,
					    XV_X, xv_col(special_panel, 0),
					    NULL);
    nspec = (Panel_item) xv_create(special_panel, PANEL_TEXT,
	       PANEL_LABEL_STRING, "Number of special ticks/labels to use:",
				   XV_HELP_DATA, "xvgr:specialticks_n",
				   PANEL_VALUE_DISPLAY_LENGTH, 10,
				   XV_X, xv_col(special_panel, 0),
				   NULL);
    (void) xv_create(special_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Page",
		     XV_HELP_DATA, "xvgr:specialticks_page",
		     PANEL_NOTIFY_PROC, page_special_notify_proc,
		     XV_X, xv_col(special_panel, 0),
		     XV_Y, xv_row(special_panel, 3),
		     NULL);
    (void) xv_create(special_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Tick locations:",
		     XV_HELP_DATA, "xvgr:specialticks_tickloc",
		     XV_X, xv_col(special_panel, 0),
		     XV_Y, xv_row(special_panel, 4),
		     NULL);
    (void) xv_create(special_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Tick labels:",
		     XV_HELP_DATA, "xvgr:specialticks_ticklableloc",
		     XV_X, xv_col(special_panel, 15),
		     XV_Y, xv_row(special_panel, 4),
		     NULL);

    for (i = 0; i < MAX_TICK_LABELS; i++) {
	sprintf(buf, "%2d:", i + 1);
	specloc_item[i] = (Panel_item) xv_create(special_panel, PANEL_TEXT,
						 PANEL_LABEL_STRING, buf,
				     XV_HELP_DATA, "xvgr:specialticks_tick",
					     PANEL_VALUE_DISPLAY_LENGTH, 10,
				      XV_SHOW, i < TPAGESIZE ? TRUE : FALSE,
					     XV_X, xv_col(special_panel, 0),
			     XV_Y, xv_row(special_panel, i % TPAGESIZE + 5),
						 NULL);
	speclabel_item[i] = (Panel_item) xv_create(special_panel, PANEL_TEXT,
				    XV_HELP_DATA, "xvgr:specialticks_label",
					     PANEL_VALUE_DISPLAY_LENGTH, 30,
				      XV_SHOW, i < TPAGESIZE ? TRUE : FALSE,
					    XV_X, xv_col(special_panel, 15),
			     XV_Y, xv_row(special_panel, i % TPAGESIZE + 5),
						   NULL);
    }
    spec_applyto = (Panel_item) xv_create(special_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Apply to:",
				  XV_HELP_DATA, "xvgr:specialticks_applyto",
					  PANEL_CHOICE_STRINGS,
					  "Current axis",
					  "All axes, current graph",
					  "Current axis, all graphs",
					  "All axes, all graphs",
					  NULL,
				   PANEL_VALUE_X, xv_col(special_panel, 15),
					  XV_Y, xv_row(special_panel, 12),
					  NULL);

    (void) xv_create(special_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:specialticks_cancel",
		     PANEL_NOTIFY_PROC, special_Done_notify_proc,
		     XV_X, xv_col(special_panel, 15),
		     XV_Y, xv_row(special_panel, 14),
		     NULL);
    (void) xv_create(special_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     XV_HELP_DATA, "xvgr:specialticks_accept",
		     PANEL_NOTIFY_PROC, accept_special_proc,
		     XV_X, xv_col(special_panel, 5),
		     XV_Y, xv_row(special_panel, 14),
		     NULL);
    load_special(cg, curaxis);
    update_special_items(cg);
    window_fit(special_panel);
    window_fit(special_frame);
    xv_set(special_frame, WIN_SHOW, TRUE, 0);
}
