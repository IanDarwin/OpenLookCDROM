/* $Id: graphwin.c,v 1.13 92/08/01 08:31:38 pturner Exp Locker: pturner $
 *
 * graph operations
 *
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include "globals.h"

extern Frame main_frame;	/* from xvgr.c */

#define FRAME_KEY        101
void generic_done_proc();	/* presently in compwin.c */

Frame graph_show_frame = (Frame) NULL;
Frame graph_focus_frame = (Frame) NULL;
Frame graph_type_frame = (Frame) NULL;

static int gtypes[] = {XY, LOGX, LOGY, LOGXY, BAR, STACKEDBAR, HBAR, STACKEDHBAR, XY, XY, XY, XY, XY, XY};

/*
 * Panel item declarations
 */
Panel_item graph_setcur_choice_item;
Panel_item graph_rendsets_choice_item;
Panel_item graph_focus_choice_item;
Panel_item graph_drawfocus_choice_item;
Panel_item graph_activate_choice_item;
Panel_item graph_kill_choice_item;
Panel_item graph_copyfrom_choice_item;
Panel_item graph_copyto_choice_item;
Panel_item graph_swapfrom_choice_item;
Panel_item graph_swapto_choice_item;
Panel_item graph_show_choice_item;
Panel_item graph_prop_choice_item;

/*
 * Event and Notify proc declarations
 */
static void graph_setcur_notify_proc();
static void graph_rendsets_notify_proc();
static void graph_focus_notify_proc();
static void graph_activate_notify_proc();
static void graph_kill_notify_proc();
static void graph_copy_notify_proc();
static void graph_show_notify_proc();

static void update_type_items(gno)
    int gno;
{
    int i;

    if (graph_type_frame) {
	i = 0;
	while (g[gno].type != gtypes[i])
	    i++;
	if (i > 8) {
	    errwin("Graph type not found");
	} else {
	    xv_set(graph_rendsets_choice_item, PANEL_VALUE, i, NULL);
	}
    }
}

static void update_focus_items(gno)
    int gno;
{
    int itest = 0;

    if (graph_focus_frame) {
	xv_set(graph_setcur_choice_item, PANEL_VALUE, gno, NULL);
	if (focus_policy == SET) {
	    itest = 1;
	} else if (focus_policy == CLICK) {
	    itest = 0;
	} else if (focus_policy == FOLLOWS) {
	    itest = 2;
	}
	xv_set(graph_focus_choice_item, PANEL_VALUE, itest, NULL);
	xv_set(graph_drawfocus_choice_item, PANEL_VALUE, draw_focus_flag == ON, NULL);
    }
}

static void update_show_items()
{
    int i, itest = 0;

    if (graph_show_frame) {
	itest = 0;
	for (i = 0; i < MAXGRAPH; i++) {
	    if (g[i].hidden) {
		itest = itest & (~(1 << i));
	    } else {
		itest = itest | (1 << i);
	    }
	}
	xv_set(graph_show_choice_item, PANEL_VALUE, itest, NULL);
    }
}

void update_graph_items()
{
    update_focus_items(cg);
    update_show_items();
    update_type_items(cg);
}

/*
 * Notify and event procs
 */

static void graph_rendsets_notify_proc(item)
    Panel_item item;
{
    int graphtype;

    graphtype = (int) xv_get(graph_rendsets_choice_item, PANEL_VALUE);
    if (g[cg].type != gtypes[graphtype]) {
	g[cg].type = gtypes[graphtype];
	autoscale_graph(cg, -3);
	drawgraph();
    }
}

static void graph_focus_notify_proc(item)
    Panel_item item;
{
    int newcg;

    switch ((int) xv_get(graph_focus_choice_item, PANEL_VALUE)) {
    case 0:
	focus_policy = CLICK;
	break;
    case 1:
	focus_policy = SET;
	break;
    case 2:
	focus_policy = FOLLOWS;
	break;
    }
    draw_focus_flag = (int) xv_get(graph_drawfocus_choice_item, PANEL_VALUE) ? ON : OFF;
    newcg = (int) xv_get(graph_setcur_choice_item, PANEL_VALUE);
    if (newcg != cg) {
	draw_focus(cg);
	cg = newcg;
	defineworld(g[cg].w.xg1, g[cg].w.yg1, g[cg].w.xg2, g[cg].w.yg2, islogx(cg), islogy(cg));
	viewport(g[cg].v.xv1, g[cg].v.yv1, g[cg].v.xv2, g[cg].v.yv2);
	update_all(cg);
	make_format(cg);
	draw_focus(cg);
    }
}

static void graph_copyprops_notify_proc(item)
    Panel_item item;
{
    int prop = (int) xv_get(graph_prop_choice_item, PANEL_VALUE);
    switch (prop) {
	case 0:
	break;
	case 1:
	break;
	case 3:
	break;
    }
    drawgraph();
}

static void graph_activate_notify_proc(item)
    Panel_item item;
{
    set_graph_active((int) xv_get(graph_activate_choice_item, PANEL_VALUE));
    drawgraph();
}

static void graph_kill_notify_proc(item)
    Panel_item item;
{
    int gno = (int) xv_get(graph_kill_choice_item, PANEL_VALUE);

    kill_graph(gno);
    drawgraph();
}

static void graph_copy_notify_proc(item)
    Panel_item item;
{
    int from = (int) xv_get(graph_copyfrom_choice_item, PANEL_VALUE);
    int to = (int) xv_get(graph_copyto_choice_item, PANEL_VALUE);

    if (from == to) {
	errwin("Graph from and graph to are the same");
	return;
    }
    if (!isactive_graph(from)) {
	errwin("Graph from isn't active");
	return;
    }
    if (isactive_graph(to)) {
	if (!yesno("Graph to copy to is active, kill it?", "YES to kill or NO to abort", "YES", "NO")) {
	    return;
	}
    }
    copy_graph(from, to);
    drawgraph();
}

static void graph_swap_notify_proc(item)
    Panel_item item;
{
    int from = (int) xv_get(graph_swapfrom_choice_item, PANEL_VALUE);
    int to = (int) xv_get(graph_swapto_choice_item, PANEL_VALUE);

    if (from == to) {
	errwin("Graph from and graph to are the same");
	return;
    }
    swap_graph(from, to);
    drawgraph();
}

static void graph_show_notify_proc(item)
    Panel_item item;
{
    int i, itest;

    itest = (int) xv_get(graph_show_choice_item, PANEL_VALUE);
    for (i = 0; i < MAXGRAPH; i++) {
	if (itest & (1 << i)) {
	    g[i].hidden = FALSE;
	} else {
	    g[i].hidden = TRUE;
	}
    }
    drawgraph();
}

void create_gactive_frame()
{
    static Frame top;
    static Panel dialog;

    if (top) {
	xv_set(top, WIN_SHOW, TRUE, 0);
	return;
    }
    top = (Frame) xv_create(main_frame, FRAME,
			    FRAME_LABEL, "Activate graphs",
			    NULL);
    dialog = (Panel) xv_create(top, PANEL,
			       XV_HELP_DATA, "xvgr:gactivate_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);
    graph_activate_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE_STACK,
				       XV_HELP_DATA, "xvgr:gactivate_graph",
						PANEL_LABEL_STRING, "Graph",
							PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
							NULL,
						    XV_X, xv_col(dialog, 0),
						    XV_Y, xv_row(dialog, 0),
							NULL);
    xv_create(dialog, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:gactivate_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, graph_activate_notify_proc,
	      XV_X, xv_col(dialog, 5),
	      XV_Y, xv_row(dialog, 1),
	      XV_KEY_DATA, FRAME_KEY, top,
	      NULL);
    (void) xv_create(dialog, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:gactivate_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_X, xv_col(dialog, 15),
		     XV_Y, xv_row(dialog, 1),
		     XV_KEY_DATA, FRAME_KEY, top,
		     NULL);
    window_fit(dialog);
    window_fit(top);
    xv_set(top, WIN_SHOW, TRUE, 0);
}

void create_gcopy_frame()
{
    static Frame top;
    static Panel dialog;

    if (top) {
	xv_set(top, WIN_SHOW, TRUE, 0);
	return;
    }
    top = (Frame) xv_create(main_frame, FRAME,
			    FRAME_LABEL, "Copy graphs",
			    NULL);
    dialog = (Panel) xv_create(top, PANEL,
			       XV_HELP_DATA, "xvgr:gcopy_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);
    graph_copyfrom_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE_STACK,
					    XV_HELP_DATA, "xvgr:gcopy_from",
					   PANEL_LABEL_STRING, "From graph",
							PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
							NULL,
					  PANEL_VALUE_X, xv_col(dialog, 15),
							NULL);
    graph_copyto_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE_STACK,
					      XV_HELP_DATA, "xvgr:gcopy_to",
					     PANEL_LABEL_STRING, "To graph",
						      PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						      NULL,
					  PANEL_VALUE_X, xv_col(dialog, 15),
						      NULL);
    xv_create(dialog, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:gcopy_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, graph_copy_notify_proc,
	      XV_X, xv_col(dialog, 5),
	      XV_Y, xv_row(dialog, 2),
	      XV_KEY_DATA, FRAME_KEY, top,
	      NULL);
    (void) xv_create(dialog, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:gcopy_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_X, xv_col(dialog, 15),
		     XV_Y, xv_row(dialog, 2),
		     XV_KEY_DATA, FRAME_KEY, top,
		     NULL);
    window_fit(dialog);
    window_fit(top);
    xv_set(top, WIN_SHOW, TRUE, 0);
}

void create_gswap_frame()
{
    static Frame top;
    static Panel dialog;

    if (top) {
	xv_set(top, WIN_SHOW, TRUE, 0);
	return;
    }
    top = (Frame) xv_create(main_frame, FRAME,
			    FRAME_LABEL, "Swap graphs",
			    NULL);
    dialog = (Panel) xv_create(top, PANEL,
			       XV_HELP_DATA, "xvgr:gswap_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);
    graph_swapfrom_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:gswap_graph",
					   PANEL_LABEL_STRING, "Swap graph",
							PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
							NULL,
					  PANEL_VALUE_X, xv_col(dialog, 15),
							NULL);
    graph_swapto_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE_STACK,
				       XV_HELP_DATA, "xvgr:gswap_withgraph",
					   PANEL_LABEL_STRING, "With graph",
						      PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						      NULL,
					  PANEL_VALUE_X, xv_col(dialog, 15),
						      NULL);
    xv_create(dialog, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:gswap_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, graph_swap_notify_proc,
	      XV_X, xv_col(dialog, 5),
	      XV_Y, xv_row(dialog, 2),
	      XV_KEY_DATA, FRAME_KEY, top,
	      NULL);
    (void) xv_create(dialog, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_X, xv_col(dialog, 15),
		     XV_Y, xv_row(dialog, 2),
		     XV_KEY_DATA, FRAME_KEY, top,
		     NULL);
    window_fit(dialog);
    window_fit(top);
    xv_set(top, WIN_SHOW, TRUE, 0);
}

void create_gkill_frame()
{
    static Frame top;
    static Panel dialog;

    if (top) {
	xv_set(top, WIN_SHOW, TRUE, 0);
	return;
    }
    top = (Frame) xv_create(main_frame, FRAME,
			    FRAME_LABEL, "Kill graphs",
			    NULL);
    dialog = (Panel) xv_create(top, PANEL,
			       XV_HELP_DATA, "xvgr:gkill_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);
    graph_kill_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:gkill_graph",
						PANEL_LABEL_STRING, "Graph",
						    PANEL_CHOICE_STRINGS,
		    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "All",
						    NULL,
						    XV_X, xv_col(dialog, 0),
						    XV_Y, xv_row(dialog, 0),
						    NULL);
    xv_create(dialog, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, graph_kill_notify_proc,
	      XV_X, xv_col(dialog, 5),
	      XV_Y, xv_row(dialog, 1),
	      XV_KEY_DATA, FRAME_KEY, top,
	      NULL);
    (void) xv_create(dialog, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_X, xv_col(dialog, 15),
		     XV_Y, xv_row(dialog, 1),
		     XV_KEY_DATA, FRAME_KEY, top,
		     NULL);
    window_fit(dialog);
    window_fit(top);
    xv_set(top, WIN_SHOW, TRUE, 0);
}

void create_gfocus_frame()
{
    static Panel dialog;

    if (graph_focus_frame) {
	update_graph_items();
	xv_set(graph_focus_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    graph_focus_frame = (Frame) xv_create(main_frame, FRAME,
					  FRAME_LABEL, "Graph focus",
					  NULL);
    dialog = (Panel) xv_create(graph_focus_frame, PANEL,
			       XV_HELP_DATA, "xvgr:gfocus_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);
    graph_setcur_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE,
					XV_HELP_DATA, "xvgr:gfocus_current",
				PANEL_LABEL_STRING, "Set current graph to:",
						      PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						      NULL,
					  PANEL_VALUE_X, xv_col(dialog, 24),
						      NULL);
    graph_focus_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE,
				    XV_HELP_DATA, "xvgr:gfocus_focusmethod",
				  PANEL_LABEL_STRING, "Graph focus set by:",
						     PANEL_CHOICE_STRINGS,
						     "Button press",
						     "As set",
						     "Follows mouse",
						     NULL,
					  PANEL_VALUE_X, xv_col(dialog, 24),
						     NULL);
    graph_drawfocus_choice_item = (Panel_item) xv_create(dialog, PANEL_CHECK_BOX,
				   XV_HELP_DATA, "xvgr:gfocus_focusdisplay",
			       PANEL_LABEL_STRING, "Display focus markers:",
					  PANEL_VALUE_X, xv_col(dialog, 24),
							 NULL);

    xv_create(dialog, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, graph_focus_notify_proc,
	      XV_X, xv_col(dialog, 5),
	      XV_Y, xv_row(dialog, 4),
	      XV_KEY_DATA, FRAME_KEY, graph_focus_frame,
	      NULL);
    (void) xv_create(dialog, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_X, xv_col(dialog, 15),
		     XV_Y, xv_row(dialog, 4),
		     XV_KEY_DATA, FRAME_KEY, graph_focus_frame,
		     NULL);
    window_fit(dialog);
    window_fit(graph_focus_frame);
    update_graph_items();
    xv_set(graph_focus_frame, WIN_SHOW, TRUE, 0);
}

void create_gshow_frame()
{
    static Panel dialog;

    if (graph_show_frame) {
	update_graph_items();
	xv_set(graph_show_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    graph_show_frame = (Frame) xv_create(main_frame, FRAME,
					 FRAME_LABEL, "Show graphs",
					 NULL);
    dialog = (Panel) xv_create(graph_show_frame, PANEL,
			       XV_HELP_DATA, "xvgr:gshow_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);
    graph_show_choice_item = (Panel_item) xv_create(dialog, PANEL_TOGGLE,
					  XV_HELP_DATA, "xvgr:gshow_toggle",
					   PANEL_LABEL_STRING, "Show graph",
						    PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						    NULL,
						    XV_X, xv_col(dialog, 0),
						    XV_Y, xv_row(dialog, 0),
						    NULL);
    xv_create(dialog, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, graph_show_notify_proc,
	      XV_X, xv_col(dialog, 5),
	      XV_Y, xv_row(dialog, 1),
	      XV_KEY_DATA, FRAME_KEY, graph_show_frame,
	      NULL);
    (void) xv_create(dialog, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_X, xv_col(dialog, 15),
		     XV_Y, xv_row(dialog, 1),
		     XV_KEY_DATA, FRAME_KEY, graph_show_frame,
		     NULL);
    window_fit(dialog);
    window_fit(graph_show_frame);
    update_graph_items();
    xv_set(graph_show_frame, WIN_SHOW, TRUE, 0);
}

void create_gtype_frame()
{
    static Panel dialog;

    if (graph_type_frame) {
	update_graph_items();
	xv_set(graph_type_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    graph_type_frame = (Frame) xv_create(main_frame, FRAME,
					 FRAME_LABEL, "Graph types",
					 NULL);
    dialog = (Panel) xv_create(graph_type_frame, PANEL,
			       XV_HELP_DATA, "xvgr:gtype_panel",
			       PANEL_LAYOUT, PANEL_VERTICAL,
			       NULL);
    graph_rendsets_choice_item = (Panel_item) xv_create(dialog, PANEL_CHOICE_STACK,
					    XV_HELP_DATA, "xvgr:gtype_type",
			    PANEL_LABEL_STRING, "Set current graph type to",
							PANEL_CHOICE_STRINGS,
							"XY graph",
							"Log-linear",
							"Linear-log",
							"Log-log",
							"Bar chart",
							"Stacked bar",
						     "Horizontal bar chart",
						   "Horizontal stacked bar",
/*
					     "*XY graph with fixed scaling",
							"*Polar",
							"*Stacked line",
							"*Pie",
*/
							NULL,
						    XV_X, xv_col(dialog, 0),
						    XV_Y, xv_row(dialog, 0),
							NULL);
    xv_create(dialog, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, graph_rendsets_notify_proc,
	      XV_X, xv_col(dialog, 5),
	      XV_Y, xv_row(dialog, 1),
	      XV_KEY_DATA, FRAME_KEY, graph_type_frame,
	      NULL);
    (void) xv_create(dialog, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_X, xv_col(dialog, 15),
		     XV_Y, xv_row(dialog, 1),
		     XV_KEY_DATA, FRAME_KEY, graph_type_frame,
		     NULL);
    window_fit(dialog);
    window_fit(graph_type_frame);
    update_graph_items();
    xv_set(graph_type_frame, WIN_SHOW, TRUE, 0);
}
