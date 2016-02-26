/* (C) Universitaet Passau 1986-1991 */
#include <xview/xview.h>
#include <xview/panel.h>

#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>
#include <algorithms.h>
#include "bends.h"
#include "graphed/gridder.h"

extern	Frame	base_frame;


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_bends_subframe ()				*/
/*									*/
/************************************************************************/

static	void	create_bends_subframe ();
static		notify_bends_buttons  ();
static	char	*bends_check_preconditions_and_call_bends ();


static	Frame		bends_subframe;
static	Panel		bends_panel;
static	Panel_item	bends_set_button,
			bends_do_button,
			bends_quit_button;
static	Gridder		gridder;

Bends_settings bends_settings = {
	64,	/* vertical grid   */
	GRIDDER_DISTANCE_2_LARGEST_SIZE,
};


static	void	create_bends_subframe()
{
	int	row_count = 0;
	
	bends_subframe = (Frame)xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	if (bends_subframe == (Frame)NULL) {
		bell ();
		return;
	}
	
	bends_panel = (Panel)xv_create(bends_subframe, PANEL, NULL);
	if (bends_panel == (Panel)NULL) {
		bell ();
		bends_subframe = NULL;
		return;
	}
	


	bends_set_button = xv_create(bends_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Set",
		XV_X,			xv_col(bends_panel, 0),
		PANEL_NOTIFY_PROC,	notify_bends_buttons,
/*		PANEL_MENU_CHOICE_STRINGS,	"Set values", 0,	fis: use MENU_STRING instaed*/
		NULL);

	bends_do_button = xv_create(bends_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do",
		XV_X,			xv_col(bends_panel, 10),
		PANEL_NOTIFY_PROC,	notify_bends_buttons,
/*		PANEL_MENU_CHOICE_STRINGS,	"Set values and execute algorithm", 0,	fis: use MENU_STRING instead*/
		NULL);


	bends_quit_button = xv_create(bends_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Quit",
		XV_X,			xv_col(bends_panel, 20),
		PANEL_NOTIFY_PROC,	notify_bends_buttons,
/*		PANEL_MENU_CHOICE_STRINGS,	"Leave this subframe", 0,	fis: use MENU_STRING instead*/
		NULL);


	row_count += 2;
	gridder = create_gridder (bends_panel,
		GRIDDER_HEIGHT,
		"grid",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);


	window_fit(bends_panel);
	window_fit(bends_subframe);
}


static	int	showing_bends_subframe = FALSE;


void		show_bends_subframe ()
{
	if (!showing_bends_subframe) {
		create_bends_subframe();
	}
	
	if (bends_subframe != (Frame)NULL) {
	
		gridder_set (gridder,
			bends_settings.grid_defaults,
			bends_settings.grid);
		
		compute_subwindow_position_at_graph_of_current_selection (
			bends_subframe);
		xv_set(bends_subframe, WIN_SHOW, TRUE, NULL);
		
		showing_bends_subframe = TRUE;
		
	} else {
		showing_bends_subframe = FALSE;
	}
}


static		notify_bends_buttons (item, event)
Panel_item	item;
Event *		event;
{
	extern	char	*bends_layout ();
	
	save_bends_settings ();
	
	if (item == bends_do_button) {
		call_sgraph_proc (bends_check_preconditions_and_call_bends);
	} else if (item == bends_set_button) {
		;
	} else if (item == bends_quit_button) {
		free (gridder);   gridder = (Gridder)NULL;
		xv_destroy_safe(bends_subframe);
		showing_bends_subframe = FALSE;
	}
}

void	save_bends_settings ()
{
	if (showing_bends_subframe) {
		bends_settings.grid   = gridder_get_size (gridder);
		bends_settings.grid_defaults = (int)gridder_get_value (gridder);
	} else {
		if (bends_settings.grid != GRIDDER_DISTANCE_OTHER) {
			bends_settings.grid = recompute_gridder_size (
				NULL,
				bends_settings.grid_defaults,
				GRIDDER_HEIGHT);
		}
	}
}


char	*bends_menu_callback_proc(menu,menu_item)
char	*menu, *menu_item;
{
	Event	*event;
	
	save_bends_settings ();
	
	event = (Event*)menu_get (menu, MENU_FIRST_EVENT);
	if (event_ctrl_is_down (event)) {
		show_bends_subframe ();
	} else {
		call_sgraph_proc (bends_check_preconditions_and_call_bends);
	}
	
	return(0);
}


int	snode_degree (node)
Snode	node;
{
	Sedge	edge;
	int	degree = 0;
	
	for_sourcelist (node, edge) {
		degree ++;
	} end_for_sourcelist (node, edge);
	if (node->graph->directed) for_targetlist (node, edge) {
		degree ++;
	} end_for_targetlist (node, edge);
	
	return degree;
}


int	sgraph_degree (graph)
Sgraph	graph;
{
	Snode	node;
	int	degree = 0, node_deg;
	
	for_all_nodes (graph, node) {
		node_deg = snode_degree (node);
		degree = maximum (degree, node_deg);
	} end_for_all_nodes (graph, node);
		
	return degree;
}


static	char		*bends_check_preconditions_and_call_bends (info)
Sgraph_proc_info	info;
{
	Sgraph	graph;
	Snode	node;
	int	degree, degree_ok;
	
	graph = info->sgraph;
	
	if (graph == empty_graph) {
		error ("empty graph\n");
	} else if (graph->nodes == empty_node || graph->nodes == graph->nodes->suc) {
		;
	} else if (!test_sgraph_connected(graph)) {
		error ("graph is not connected\n");
	} else {
	
		Slist	nodes_with_degree_greater_four;
		int	degree, degree_ok;
		
		
		/* Check for nodes with degree > 4 */
		
		nodes_with_degree_greater_four = empty_slist;
		degree_ok = TRUE;
		for_all_nodes (graph, node) {
			degree = snode_degree (node);
			if (degree > 4) {
				degree_ok = FALSE;
				nodes_with_degree_greater_four = add_to_slist (
					nodes_with_degree_greater_four,
					make_attr(ATTR_DATA, (char *)node));
			}
		} end_for_all_nodes (graph, node);
		
		if (!degree_ok) {
			if (nodes_with_degree_greater_four == nodes_with_degree_greater_four->suc) {
				error ("There is a node with degree > 4\n");
			} else {
				error ("There are nodes with degree > 4\n");
			}
			info->new_selected = SGRAPH_SELECTED_GROUP;
			info->new_selection.group = nodes_with_degree_greater_four;
			return NULL;
		}
		
		
		/* Check for a planar drawing */
		
		if (test_graph_is_drawn_planar (graphed_graph(graph))) {
			call_call_bends (info);
			info->recenter = TRUE;
		} else {
			; /* War wohl nix */
		}
	}
}

