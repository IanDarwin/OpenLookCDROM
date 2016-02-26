/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#undef GRAPHED_DEBUG_MALLOC

/************************************************************************/
/*									*/
/*				util.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	In diesem Modul befinden sich diverse Hilfsprozeduren aus	*/
/*	allen Bereichen.						*/
/*									*/
/************************************************************************/


#include "misc.h"
#include "graph.h"
#include "group.h"
#include "user_header.h"


/************************************************************************/
/*									*/
/*			   Compute degrees				*/
/*									*/
/************************************************************************/


int	node_degree (node)
Node	node;
{
	Edge	edge;
	int	degree = 0;
	
	for_edge_sourcelist (node, edge) {
		degree ++;
	} end_for_edge_sourcelist (node, edge);
	for_edge_targetlist (node, edge) {
		degree ++;
	} end_for_edge_targetlist (node, edge);
	
	return degree;
}


int	graph_degree (graph)
Graph	graph;
{
	Node	node;
	int	degree = 0, node_deg;
	
	for_nodes (graph, node) {
		node_deg = node_degree (node);
		degree = maximum (degree, node_deg);
	} end_for_nodes (graph, node);
		
	return degree;
}


/************************************************************************/
/*									*/
/*		    Test whether a graph is drawn planar		*/
/*									*/
/************************************************************************/


static	node_intersects_node (n1, n2)
Node	n1,n2;
{
	return rect_intersectsrect (&(n1->box), &(n2->box));
}


static	line_at_x (x1,y1, x2,y2, x)
int	x1,y1, x2,y2, x;
{
	float m;
	
	if (x1 == x2) {
		return (y1 + y2) / 2;
	} else {
		m = (float)(y2 -y1) / (float)(x2 - x1);
		return y1 + m * (x - x1);
	}
}


static	line_at_y (x1,y1, x2,y2, y)
int	x1,y1, x2,y2, y;
{
	float m;
	
	if (y1 == y2) {
		return (x1 + x2) / 2;
	} else {
		m = (float)(x2 -x1) / (float)(y2 - y1);
		return x1 + m * (y - y1);
	}
}


#define	is_in_interval(x,start,end) ((x) > (start) && (x) < (end))

static	edge_intersects_node (e,n)
Edge	e;
Node	n;
{
	Edgeline	el;
	Rect		node_box;
	int		el_at_left, el_at_right,
			el_at_top,  el_at_bottom;


	node_box = n->box;

	if (rect_intersectsrect (&(e->box), &node_box)) {
	    for_edgeline (e->line, el)
		if (el->suc != e->line &&
		    !(n == e->source && el == e->line) &&		/* First line segment	*/
		    !(n == e->target && el == e->line->pre->pre)	/* Last line segment	*/
		    /* The first resp. last segment may intersect with the source resp. target	*/
		    /* nodes; this is due to node/edge interface				*/
		    ) {
		    if (rect_intersectsrect (&(el->box), &node_box)) {
			el_at_top    = line_at_y (el->x, el->y, el->suc->x, el->suc->y, rect_top(&node_box));
			el_at_bottom = line_at_y (el->x, el->y, el->suc->x, el->suc->y, rect_bottom(&node_box));
			el_at_left   = line_at_x (el->x, el->y, el->suc->x, el->suc->y, rect_left(&node_box));
			el_at_right  = line_at_x (el->x, el->y, el->suc->x, el->suc->y, rect_right(&node_box));
			if (is_in_interval (el_at_top,    rect_left(&node_box), rect_right(&node_box)) ||
			    is_in_interval (el_at_bottom, rect_left(&node_box), rect_right(&node_box)) ||
			    is_in_interval (el_at_left,   rect_top(&node_box),  rect_bottom(&node_box)) ||
			    is_in_interval (el_at_right,  rect_top(&node_box),  rect_bottom(&node_box))) {
				return TRUE;
		    }
		}
	    } end_for_edgeline (e->line, el);
	}

	return FALSE;
}


static	edge_intersects_edge (e1,e2)
Edge	e1, e2;
{
	Edgeline	el1, el2;
	Rect		e1_box,  e2_box;
	Rect		el1_box, el2_box;
	int		intersects;
	int		x,y;
	
	e1_box = e1->box,
	e2_box = e2->box;
	
	if (rect_intersectsrect (&e1_box, &e2_box)) {
		for_edgeline (e1->line, el1) if (el1->suc != e1->line) {
			for_edgeline (e2->line, el2) if (el2->suc != e2->line) {
			
				/* Check for any intersection */
				
				el1_box = el1->box,
				el2_box = el2->box;
				
				intersects = line_line_intersection (
					el1->x, el1->y,  el1->suc->x, el1->suc->y,
					el2->x, el2->y,  el2->suc->x, el2->suc->y,
					&x, &y);
				if (intersects &&
				    is_in_interval (x, rect_left(&el1_box), rect_right(&el1_box)) &&
				    is_in_interval (x, rect_left(&el2_box), rect_right(&el2_box)) &&
				    is_in_interval (y, rect_top(&el1_box),  rect_bottom(&el1_box)) &&
				    is_in_interval (y, rect_top(&el2_box),  rect_bottom(&el2_box)) ) {
					return TRUE;
				}
		} end_for_edgeline (e2->line, el2);
	   } end_for_edgeline (e1->line, el1);
	}
	
	return FALSE;
}


Picklist	check_graph_is_drawn_planar (g)
Graph		g;
{
	Node	n1, n2;
	Edge	e1, e2;
	
	if (g == empty_graph) {
		return TRUE;
	}
		
	for_nodes (g, n1) {
	    for_nodes (g, n2) {
		if ((n1 != n2) && node_intersects_node(n1,n2)) {
		    return new_picklist (GROUP_PICKED,
			add_to_group (new_group (n1), n2));
		}
	    } end_for_nodes (g, n2);
	} end_for_nodes (g, n1);
	
	for_nodes (g, n1) {
	    for_edge_sourcelist (n1, e1) {
	    
		for_nodes (g, n2) if (n1 != n2) {
		
		    if (edge_intersects_node (e1, n2)) {
			return new_picklist (GROUP_PICKED,
				add_to_group (new_group (e1->source), e1->target));
		    }
		    
		    for_edge_sourcelist (n2, e2) { 
			if ((e1 != e2) && edge_intersects_edge (e1,e2)) {
			    return new_picklist (GROUP_PICKED,
				add_to_group (add_to_group (add_to_group (new_group (
				    e1->source), e1->target), e2->source), e2->target));
			}
		    } end_for_edge_sourcelist (n2, e2);
		    
		} end_for_nodes (g, n2);
		
	    } end_for_edge_sourcelist (n1, e1);
	} end_for_nodes (g, n1);
	
	return empty_picklist;
}


#include "sgraph/std.h"
#include "sgraph/sgraph.h"
#include "sgraph/slist.h"
#include "sgraph/graphed.h"
#include "sgraph/algorithms.h"
#include "graphed_sgraph_interface.h"

int	test_graph_is_drawn_planar (graph)
Graph	graph;
{
	Picklist	bad;
	
	if (graph == empty_graph) {
		message ("No graph selected.\n");
		return TRUE;
	} else {
	
		bad = check_graph_is_drawn_planar (graph);
	
		if (bad == empty_picklist) {
			message ("Planar drawing\n");
			return TRUE;
		} else {
			message ("No planar drawing\n");
			dispatch_user_action (SELECT, bad);
			return FALSE;
		}
	}
	
}

char	*menu_test_graph_is_drawn_planar (menu, menu_item)
char*	menu;		/* The menu from which it is called	*/
char*	menu_item;	/* The menu item from ...		*/
{
	test_graph_is_drawn_planar (get_picked_or_only_existent_graph());
	
	return NULL;
}



/*	Check for any non-straight-line edges	*/

Edge	find_non_straight_line_edge (graph)
Graph	graph;
{
	Node	node;
	Edge	edge;
	
	for_nodes (graph, node) {
		for_edge_sourcelist (node, edge) {
			if (!is_single_edgeline (edge->line)) {
				return edge;
			}
		} end_for_edge_sourcelist (node, edge);
	} end_for_nodes (graph, node);
	
	return empty_edge;
}



int	test_find_non_straight_line_edge (graph)
Graph	graph;
{
	Edge	edge;
	
	if (graph == empty_graph) {
		message ("No graph selected.\n");
		return TRUE;
	} else {
		edge = find_non_straight_line_edge (graph);
	
		if (edge == empty_edge) {
			message ("Only straight line edges\n");
			return FALSE;
		} else {
			message ("Found an edge with bends\n");
			dispatch_user_action (SELECT_EDGE, edge);
			return TRUE;
		}
	}
}

char	*menu_test_find_non_straight_line_edge (menu, menu_item)
char*	menu;		/* The menu from which it is called	*/
char*	menu_item;	/* The menu item from ...		*/
{
	test_find_non_straight_line_edge (get_picked_or_only_existent_graph());
	
	return NULL;
}



/************************************************************************/
/*									*/
/*			    Find a cycle				*/
/*									*/
/************************************************************************/

static	Group	reachable_loop (node, reachable)
Node		node;
Group		reachable;
{
	Edge	edge;
		
	if (contains_group_node (reachable, node) == empty_group) {
		reachable = add_immediately_to_group (reachable, node);
		for_edge_sourcelist (node, edge) {
			reachable = reachable_loop (edge->target, reachable);
		} end_for_edge_sourcelist (node, edge);
	} else {
		;
	}
	
	return reachable;
}


Group	find_nodes_reachable_from_node (node)
Node		node;
{
	Group	reachable = empty_group;
	Edge	edge;
	
	
	for_edge_sourcelist (node, edge) {
		reachable = reachable_loop (edge->target, reachable);
	} end_for_edge_sourcelist (node, edge);
	
	return reachable;
}


Group	find_cycle_in_directed_graph (graph)
Graph	graph;
{
	Group	cycle;
	Node	node;
	
	for_nodes (graph, node) {
		cycle = find_nodes_reachable_from_node (node);
		if (contains_group_node(cycle, node)) {
			return cycle;
		} else {
			free_group (cycle);
		}
	} end_for_nodes (graph, node);
	
	return empty_group;
}


void	test_find_cycle_in_directed_graph (graph)
Graph	graph;
{
	Group	cycle;
	
	if (graph == empty_graph) {
		warning ("No graph selected\n");
	} else if (!graph->directed) {
		error ("Graph is not directed\n");
	} else {
		cycle = find_cycle_in_directed_graph (graph);
		if (cycle != empty_group) {
			message ("Found a cycle\n");
			dispatch_user_action (SELECT_GROUP, cycle);
		} else {
			message ("Found no cycle\n");
		}
	}
}


char	*menu_test_find_cycle_in_directed_graph (menu, menu_item)
char*	menu;		/* The menu from which it is called	*/
char*	menu_item;	/* The menu item from ...		*/
{	
	test_find_cycle_in_directed_graph (get_picked_or_only_existent_graph ());
	
	return NULL;
}


void	remove_all_self_loops_in_graph (graph)
Graph	graph;
{
	Node	node;
	Edge	edge;
	Slist	edges_to_remove, l;
	
	if (graph == empty_graph) {
		warning ("No graph selected\n");
	} else {
	
		dispatch_user_action (UNSELECT);
		
		edges_to_remove = empty_slist;
		for_nodes (graph, node) {
			for_edge_sourcelist (node, edge) {
				if (edge->target == edge->source) {
					edges_to_remove = add_to_slist (edges_to_remove, make_attr (ATTR_DATA, (char *)edge));
				}
			} end_for_edge_sourcelist (node, edge);
		} end_for_nodes (graph, node);
		
		for_slist (edges_to_remove, l) {
			erase_and_delete_edge ((Edge)attr_data(l));
		} end_for_slist (edges_to_remove, l);
		free_slist (edges_to_remove);
		
		dispatch_user_action (SELECT_GRAPH, graph);
	}
}


char	*menu_remove_all_self_loops_in_graph (menu, menu_item)
char*	menu;		/* The menu from which it is called	*/
char*	menu_item;	/* The menu item from ...		*/
{	
	remove_all_self_loops_in_graph (get_picked_or_only_existent_graph ());
	
	return NULL;
}


void	remove_all_multiple_edges_in_graph (graph)
Graph	graph;
{
	Node	node;
	Edge	edge;
	Slist	edges_to_remove, l;
	Group	targets;
	
	if (graph == empty_graph) {
		warning ("No graph selected\n");
	} else {
	
		dispatch_user_action (UNSELECT);
		
		for_nodes (graph, node) {
		
			edges_to_remove = empty_slist;
			targets = empty_group;
			for_edge_sourcelist (node, edge) {
				if (contains_group_node(targets, edge->target)) {
					edges_to_remove = add_to_slist (edges_to_remove, make_attr (ATTR_DATA, (char *)edge));
				} else {
					targets = add_to_group (targets, edge->target);
				}
			} end_for_edge_sourcelist (node, edge);
			for_edge_targetlist (node, edge) {
				if (contains_group_node(targets, edge->source)) {
					edges_to_remove = add_to_slist (edges_to_remove, make_attr (ATTR_DATA, (char *)edge));
				} else {
					targets = add_to_group (targets, edge->source);
				}
			} end_for_edge_targetlist (node, edge);
			
			free_group (targets);
			for_slist (edges_to_remove, l) {
				erase_and_delete_edge ((Edge)attr_data(l));
			} end_for_slist (edges_to_remove, l);
			free_slist (edges_to_remove);
			
		} end_for_nodes (graph, node);
		
		dispatch_user_action (SELECT_GRAPH, graph);
	}
}


char	*menu_remove_all_multiple_edges_in_graph (menu, menu_item)
char*	menu;		/* The menu from which it is called	*/
char*	menu_item;	/* The menu item from ...		*/
{	
	remove_all_multiple_edges_in_graph (get_picked_or_only_existent_graph ());
	
	return NULL;
}




/************************************************************************/
/*									*/
/*			    set on grid points				*/
/*									*/
/************************************************************************/
/* This little procedure sets the selection on grid points :		*/
/* the centers of nodes and bends of edges will be moved to grid	*/
/* points.								*/
/************************************************************************/


char		*set_on_grid_points (menu, menu_item)
Menu		menu;		/* The menu from which it is called	*/
Menu_item	menu_item;	/* The menu item from ...		*/
{
	Picklist	selection;	/* the selected object(s)	*/
	Node		node;
	Edge		edge;
	Group		group;

	Edgeline	el;		/* variables to run loops	*/
	Group		g;		/* -------- " -----------	*/
	int		x,y;		/* various coordinates		*/

#ifdef GRAPHED2
	int		gridwidth = get_gridwidth (wac_buffer);
#else
	extern	int	gridwidth;
#endif

	/* first check whether we are allowed to do something	*/
	if (test_user_interface_locked()) {
		bell ();
		return NULL;
	}

	if (gridwidth == 0) {
		notice_prompt (base_frame, NULL,		/*fisprompt*/
			NOTICE_MESSAGE_STRINGS,	"Only usable with grid.", NULL,
			NOTICE_BUTTON_YES,	"Ok",
			NULL);
		return NULL;
	}
	

	/* First get the selection	*/
	selection = get_picked_object ();
	
	/* dispatch it			*/
	if (selection != empty_picklist) switch (selection->what) {
	
	    case NODE_PICKED :

		node = selection->which.node;

		x = node_x (node);
		y = node_y (node);
		constrain_to_grid (gridwidth, &x, &y);

		node_set (node, NODE_POSITION, x, y, 0);
		break;
	
	    case EDGE_PICKED :

		edge = selection->which.edge;
		
		for_edgeline (edge->line, el) {
		
			x = edgeline_x (el);
			y = edgeline_y (el);
			constrain_to_grid (gridwidth, &x, &y);
			edge_set (edge, ONLY_SET, MOVE, el, x-el->x, y-el->y, 0);
			
		} end_for_edgeline (edge->line, el);
		
		edge_set (edge, RESTORE_IT, 0);
		break;
	
	    case GROUP_PICKED :

		group = selection->which.group;
		
		for_group (group, g) {
		
			node = g->node;
			x = node_x (node);
			y = node_y (node);
			constrain_to_grid (gridwidth, &x, &y);
			node_set (node, ONLY_SET, NODE_POSITION, x, y, 0);
			
			for_edge_sourcelist (node, edge) {
				 if (contains_group_node (group,edge->target)) for_edgeline (edge->line, el) {
					x = edgeline_x (el);
					y = edgeline_y (el);
					constrain_to_grid (gridwidth, &x, &y);
					edge_set (edge, ONLY_SET, MOVE, el, x-el->x, y-el->y, 0);
				} end_for_edgeline (edge->line, el);
			} end_for_edge_sourcelist (node, edge);
			
		} end_for_group (group, g)
		
		group_set (group, RESTORE_IT, 0);
		break;
	
	    default :
		break;
	}
	
	return NULL;
}



/************************************************************************/
/*									*/
/*			    fit node to text				*/
/*									*/
/************************************************************************/


char		*fit_node_to_text (menu, menu_item)
Menu		menu;		/* The menu from which it is called	*/
Menu_item	menu_item;	/* The menu item from ...		*/
{
	Picklist	selection;	/* the selected object(s)	*/
	Node		node;
	Group		group;
	
	Group		g;		   /* -------- " -----------	*/
	int		x,y;		   /* various coordinates	*/
	struct	pr_subregion label_bound;  /* box around the label	*/
	struct	pr_subregion this_line_bound,
		             first_line_bound;


	/* first check whether we are allowed to do something	*/
	if (test_user_interface_locked()) {
		bell ();
		return NULL;
	}


	/* First get the selection	*/
	selection = get_picked_object ();
	
	/* dispatch it			*/
	if (selection != empty_picklist) switch (selection->what) {
	
	    case NODE_PICKED :

		node = selection->which.node;
		if (node->label.text != NULL) {

			int	lines = 0;
			char	**lines_text;
			
			lines_text = split_string (node->label.text);
			label_bound = compute_lines_subregion_size (lines_text,
				node->label.font);
			free_lines (lines_text);
			
			if (node->label.placement != NODELABEL_MIDDLE) {
				node_set (node, NODE_SIZE,
					label_bound.size.x + 2*NODELABEL_GAP,
					label_bound.size.y + 2*NODELABEL_GAP,
					0);
			} else {
				node_set (node, NODE_SIZE,
					label_bound.size.x + 2*2*NODELABEL_GAP,
					label_bound.size.y + 2*2*NODELABEL_GAP,
					0);
			}
		}
		break;
	
	    case GROUP_PICKED :

		group = selection->which.group;
		
		for_group (group, g) {
		
			int	lines = 0;
			char	**lines_text;
			
			node = g->node;
			
			if (node->label.text != NULL) {
			
				node = g->node;
			
				lines_text = split_string (node->label.text);
				label_bound = compute_lines_subregion_size (lines_text,
					node->label.font);
				free_lines (lines_text);
			
				if (node->label.placement != NODELABEL_MIDDLE) {
					node_set (node, ONLY_SET, NODE_SIZE,
						label_bound.size.x + 2*NODELABEL_GAP,
						label_bound.size.y + 2*NODELABEL_GAP,
						0);
				} else {
					node_set (node, ONLY_SET, NODE_SIZE,
						label_bound.size.x + 2*2*NODELABEL_GAP,
						label_bound.size.y + 2*2*NODELABEL_GAP,
						0);
				}
			}
			
		} end_for_group (group, g)
		
		group_set (group, RESTORE_IT, 0);
		break;
	
	    default :
		break;
	}
	
	return NULL;
}



/************************************************************************/
/*									*/
/*				next entry				*/
/*									*/
/************************************************************************/

