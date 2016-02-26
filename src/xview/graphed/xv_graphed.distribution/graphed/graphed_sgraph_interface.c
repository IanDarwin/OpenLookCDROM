/* (C) Universitaet Passau 1986-1991 */
#include "misc.h"
#include "graph.h"
#include "group.h"

#include "user_header.h"

#define INCLUDE_SGRAPH
#ifdef INCLUDE_SGRAPH
#include "sgraph/std.h"
#include "sgraph/sgraph.h"
#include "sgraph/slist.h"
#include "sgraph/graphed.h"
#include "graphed_sgraph_interface.h"


void	compute_sgraph_center ();


/************************************************************************/
/*									*/
/*	Sgraph	copy_graphed_graph_to_sgraph (graph)			*/
/*	Graph	graph;							*/
/*									*/
/*	Copies a GraphEd-graph to an Sgraph.				*/
/*	- labels are reallocated.					*/
/*	- pointers ->sgraph_[node,edge,graph] and ->graphed are		*/
/*	  established in the graphed resp. sgraph structures.		*/
/*	- node coordinates are copied.					*/
/*									*/
/************************************************************************/


Sgraph	copy_graphed_graph_to_sgraph (graph)
Graph	graph;
{
	Node		node;
	Edge		edge;
	Sgraph		sgraph;
	Snode		snode;
	Sedge		sedge;
	Attributes	graph_attr, node_attr, edge_attr;
	Slist		remove_node_list = empty_slist;
	Slist		remove_edge_list = empty_slist;
	Slist		l;
	
	graph_attr.data = NULL;
	node_attr.data  = NULL;
	edge_attr.data  = NULL;
	
	
	/* The strategy :						*/
	/* If there are any non-NULL ->sgraph pointers,	assume that	*/
	/* they are valid pointers to sgraphs/snodes/sedges and REUSE	*/
	/* them. this way, we may leave some attrs IN the		*/
	/* Sgraph-structures and reuse them.				*/
	/* Conversely, we have to check whether any GraphEd-Nodes have	*/
	/* gone away, and still left their Sgraph counterparts.		*/
	
	/* At initialize the graph	*/
	if (graph->sgraph_graph == NULL) {
		sgraph = make_graph (graph_attr);
		sgraph->graphed     = (char *)graph;
		graph->sgraph_graph = (char *)sgraph;
		sgraph->directed    = graph->directed;
	} else {
		sgraph = (Sgraph)graph->sgraph_graph;
	}
	
	
	/* Check for Snodes & Sedges without GraphEd counterparts.	*/
	
	for_all_nodes (sgraph, snode) {
		snode->graphed = NULL;
		for_sourcelist (snode, sedge) {
			sedge->graphed = NULL;
		} end_for_sourcelist (snode, sedge)
	} end_for_all_nodes (sgraph, snode);
	
	for_nodes (graph, node) {
		if (node->sgraph_node == NULL)
			(Snode)(node->sgraph_node) = make_node (graph->sgraph_graph, node_attr);
		((Snode)(node->sgraph_node))->graphed = (char *)node;
		((Snode)(node->sgraph_node))->nr = node->nr;
		((Snode)(node->sgraph_node))->x = node->x;
		((Snode)(node->sgraph_node))->y = node->y;
		set_nodelabel (((Snode)(node->sgraph_node)),
			strsave (node->label.text));
	} end_for_nodes (graph, node);
			
	for_nodes (graph, node) {
		for_edge_sourcelist (node, edge) {
			if (edge->sgraph_edge == NULL)
				edge->sgraph_edge = (char *)make_edge (edge->source->sgraph_node, edge->target->sgraph_node, edge_attr);
			((Sedge)(edge->sgraph_edge))->graphed = (char*)edge;
			if (!sgraph->directed)
				/* Sgraph implementation dependend !	*/
				((Sedge)(edge->sgraph_edge))->tsuc->graphed = (char*)edge;
			set_edgelabel (((Sedge)(edge->sgraph_edge)), strsave (edge->label.text));
		} end_for_edge_sourcelist (node, edge);
	} end_for_nodes (graph, node);
	
	
	for_all_nodes (sgraph, snode) {
		if (snode->graphed == NULL) {
			remove_node_list = add_immediately_to_slist (remove_node_list,
				make_attr (ATTR_DATA, (char *)snode));
		}
	} end_for_all_nodes (sgraph, snode);
	
	if (remove_node_list != empty_slist) {
		for_slist (remove_node_list, l) {
			remove_node (attr_data_of_type (l, Snode));
		} end_for_slist (remove_node_list, l);
		free_slist (remove_node_list);
	}
	
	
	for_all_nodes (sgraph, snode) {
		for_sourcelist (snode, sedge) if (sgraph->directed || sedge < sedge->tsuc) {
			if (sedge->graphed == NULL) {
				remove_edge_list = add_immediately_to_slist (remove_edge_list,
					make_attr (ATTR_DATA, (char *)sedge));
			}
		} end_for_sourcelist (snode, sedge);
	} end_for_all_nodes (sgraph, snode);
	
	if (remove_edge_list != empty_slist) {
		for_slist (remove_edge_list, l) {
			remove_edge (attr_data_of_type (l, Sedge));
		} end_for_slist (remove_edge_list, l);
		free_slist (remove_edge_list);
	}
	
	
	return sgraph;
}



/************************************************************************/
/*									*/
/*	void	free_sgraph (graph)					*/
/*	Sgraph	graph;							*/
/*									*/
/*	Deallocates an sgraph.						*/
/*	- labels of nodes and edges with ->graphed != NULL are free'd.	*/
/*	  This is done because they have been allocated in the above	*/
/*	  copy procedure.						*/
/*	- pointers ->sgraph_[node,edge,graph] in graphed are set to	*/
/*	  NULL.								*/
/*									*/
/************************************************************************/


void	free_sgraph (graph)
Sgraph	graph;
{
	Snode	node;
	Sedge	edge;

	for_all_nodes (graph,node) {
		if (node->label != NULL)
			myfree (node->label);	
		if (node->graphed != NULL)
			((Node)(node->graphed))->sgraph_node = NULL;
		node->graphed = NULL;
		for_sourcelist (node, edge) {
			if (edge->label != NULL)
				myfree (edge->label);
			if (edge->graphed != NULL)
				((Edge)(edge->graphed))->sgraph_edge = NULL;
			edge->graphed = NULL;
		} end_for_sourcelist (node, edge);
		if (graph->directed) for_targetlist (node, edge) {
			if (edge->label != NULL)
				myfree (edge->label);
			if (edge->graphed != NULL)
				((Edge)(edge->graphed))->sgraph_edge = NULL;
			edge->graphed = NULL;
		} end_for_targetlist (node, edge);
	} end_for_all_nodes (graph, node);
	
	
	if (graph->graphed != NULL)
		((Graph)(graph->graphed))->sgraph_graph = NULL;
	graph->graphed = NULL;
	
	remove_graph (graph);
}

/************************************************************************/
/*									*/
/*		Create a GraphEd-Graph from a Sgraph			*/
/*									*/
/************************************************************************/



Graphed_node	create_graphed_graph_from_sgraph (sgraph)
Sgraph		sgraph;
{
	Graph	graphed_graph;
	Snode	n;
	Sedge	e;
	
	
	graphed_graph = create_graph (wac_buffer);
	set_graph_directedness (graphed_graph, sgraph->directed);
	
	sgraph->graphed             = (char *)graphed_graph;
	graphed_graph->sgraph_graph = (char *)sgraph;
	
	for_all_nodes (sgraph, n) {
		(void) create_graphed_node_from_snode (n);
	} end_for_all_nodes (sgraph, n);
	
	for_all_nodes (sgraph, n) {
		for_sourcelist (n, e) if (sgraph->directed || e < e->tsuc) {
			(void) create_graphed_edge_from_sedge (e);
		} end_for_sourcelist (n, e);
/*
		if (sgraph->directed) for_targetlist (n, e) {
			(void) create_graphed_edge_from_sedge (e);
		} end_for_targetlist (n, e);
*/
	} end_for_all_nodes (sgraph, n);
	
	return	(Graphed_graph)graphed_graph;
}


Graphed_node	create_graphed_node_from_snode (snode)
Snode		snode;
{
	Node	graphed_node;
	
	graphed_node = create_node (graphed_graph(snode->graph));
	node_set (graphed_node, ONLY_SET,
		NODE_POSITION, snode->x, snode->y,
		DEFAULT_NODE_ATTRIBUTES,
		0);
	
	snode->graphed            = (char *)graphed_node;
	graphed_node->sgraph_node = (char *)snode;
	
	return	(Graphed_node)graphed_node;
}


Graphed_edge	create_graphed_edge_from_sedge (sedge)
Sedge		sedge;
{
	Edge		graphed_edge;
	Edgeline	line;
	
	graphed_edge = create_edge (
		graphed_node (sedge->snode),
		graphed_node (sedge->tnode));
	
	line = new_edgeline (sedge->snode->x, sedge->snode->y);
	(void)add_to_edgeline (line, sedge->tnode->x, sedge->tnode->y);
	edge_set (graphed_edge, ONLY_SET,
		EDGE_LINE, line,
		DEFAULT_EDGE_ATTRIBUTES,
		0);
		
	sedge->graphed = (char *)graphed_edge;
	if (!sedge->snode->graph->directed) {
		sedge->tsuc->graphed = (char *)graphed_edge;
	}
	graphed_edge->sgraph_edge = (char *)sedge;
	
	return	(Graphed_edge)graphed_edge;
}


Graphed_group	create_graphed_group_from_slist (slist)
Slist		slist;
{
	Group	g = empty_group;
	Slist	l;
	
	for_slist (slist, l) {
		g = add_to_group (g, graphed_node((Snode)attr_data(l)));
	} end_for_slist (slist, l);
	
	return	(Graphed_group)g->suc;
}



Slist	create_slist_from_graphed_group (group)
Group	group;
{
	Group	g = empty_group;
	Slist	l = empty_slist;
	
	for_group (group, g) {
		l = add_to_slist (l,
			make_attr(ATTR_DATA, (char*)(g->node->sgraph_node)));
	} end_for_group (group, g);
	
	return	l->suc;
}



char		*node_get (node, attr)
Node		node;
Set_attribute	attr;
{
	switch (attr) {
	
	    case NODE_X :
		return (char *)(node->x);
		break;
	
	    case NODE_Y :
		return (char *)(node->y);
		break;
		
	    case NODE_WIDTH :
		return (char *)(node_width(node));
		break;
	
	    case NODE_HEIGHT :
		return (char *)(node_height(node));
		break;
	
	    case NODE_NEI :
		return (char *)(node->node_edge_interface);
		break;
	
	    case NODE_NLP :
		return (char *)(node->label.placement);
		break;
	
	    case NODE_LABEL :
		return (char *)(node->label.text);
		break;
	
	    case NODE_FONT :
		return (char *)(get_font_index(node->label.font));
		break;
	
	    case NODE_TYPE :
		return (char *)(get_nodetype_index(node->type));
		break;
	
	    case NODE_LABEL_VISIBILITY :
		return (char *)(node->label.visible);
		break;
	
	    case NODE_COLOR :
#ifdef GRAPHED2
		return (char *)(node->color);
#endif
		break;
	
	}
}



char		*edge_get (edge, attr)
Edge		edge;
Set_attribute	attr;
{
	switch (attr) {
	
	    case EDGE_LINE :
		return (char *)(edge->line);
		break;
	
	    case EDGE_ARROW_LENGTH :
		return (char *)(edge->arrow.length);
		break;
	
	    case EDGE_LABEL :
		return (char *)(edge->label.text);
		break;
	
	    case EDGE_FONT :
		return (char *)(get_font_index(edge->label.font));
		break;
	
	    case EDGE_TYPE :
		return (char *)(get_edgetype_index(edge->type));
		break;
	
	    case EDGE_LABEL_VISIBILITY :
		return (char *)(edge->label.visible);
		break;
	
	    case EDGE_COLOR :
#ifdef GRAPHED2
		return (char *)(edge->color);
#endif
		break;
	
	}
}



void			compute_sgraph_selection (picked, selected, selection)
Picklist		picked;
Sgraph_selected		*selected;
Sgraph_selection	*selection;
{
	if (picked != empty_picklist) switch (picked->what) {
	    case NODE_PICKED :
		*selected        = SGRAPH_SELECTED_SNODE;
		selection->snode = (Snode)(picked->which.node->sgraph_node);
		break;
	    case EDGE_PICKED :
		*selected        = SGRAPH_SELECTED_SEDGE;
		selection->sedge = (Sedge)(picked->which.edge->sgraph_edge);
		break;
	    case GROUP_PICKED :
		*selected  = SGRAPH_SELECTED_GROUP;
		selection->group = create_slist_from_graphed_group (picked->which.group);
		break;
	    default :
		*selected = SGRAPH_SELECTED_NONE;
		break;
		
	} else /* picked is empty */ {
	
		*selected = SGRAPH_SELECTED_NONE;
	
	}
}

/************************************************************************/
/*									*/
/*			Calling Sgraph Procedures			*/
/*									*/
/************************************************************************/
/*									*/
/*	char	*menu_call_sgraph_proc (menu, menu_item, proc)		*/
/*									*/
/*======================================================================*/
/*									*/
/*	char	*call_sgraph_proc (proc)				*/
/*									*/
/************************************************************************/

char		*menu_call_sgraph_proc (menu, menu_item, proc)
Menu		menu;		/* The menu from which it is called	*/
Menu_item	menu_item;	/* The menu item from ...		*/
void		(*proc)();
{
	return	call_sgraph_proc (proc);
}


void			 prepare_sgraph_info (sgraph_info, x_center_before, y_center_before, n_nodes)
struct	sgraph_proc_info *sgraph_info;
int			 *x_center_before;
int			 *y_center_before;
int			 *n_nodes;
{
	Graph		graph;
	Sgraph		sgraph;
	Picklist	picked;
	
	
	picked = get_picked_object();
	if (picked == empty_picklist) {
		graph = get_picked_or_only_existent_graph ();
	} else {
		graph = get_picked_graph ();
	}
	
	if (graph == (Graph)NULL) {
		sgraph = empty_sgraph;
	} else {
		sgraph = copy_graphed_graph_to_sgraph (graph);
	}
	
	sgraph_info->buffer = wac_buffer;

	sgraph_info->sgraph = sgraph;
	compute_sgraph_selection (picked, &(sgraph_info->selected), &(sgraph_info->selection));
		
	sgraph_info->repaint              = FALSE;
	sgraph_info->recompute            = FALSE;
	sgraph_info->no_changes           = FALSE;
	sgraph_info->no_structure_changes = FALSE;
	sgraph_info->save_selection       = FALSE;
	sgraph_info->recenter             = FALSE;
	
	sgraph_info->new_selected         = SGRAPH_SELECTED_SAME;
	sgraph_info->new_selection        = sgraph_info->selection;
	sgraph_info->new_buffer           = wac_buffer;
	sgraph_info->new_sgraph           = empty_sgraph;

	compute_sgraph_center (sgraph_info->sgraph, x_center_before, y_center_before, n_nodes);
}


void	prepare_sgraph_event_proc_info (sgraph_info, uev_info, sgraph_event_proc_info)
struct	sgraph_proc_info *sgraph_info;
UEV_info		 uev_info;
Sgraph_event_proc_info	 sgraph_event_proc_info;
{
	/* At the moment, this procedure does nothing more	*/
	/* than translating the UEV_info into its equivalent	*/
	/* Sgraph_event_proc_info				*/
	
	switch (uev_info->state) {
	    case UEV_START :
		sgraph_event_proc_info->state = SGRAPH_UEV_START;
		break;
	    case UEV_DRAG :
		sgraph_event_proc_info->state = SGRAPH_UEV_DRAG;
		break;
	    case UEV_INTERMEDIATE_STOP :
		sgraph_event_proc_info->state = SGRAPH_UEV_INTERMEDIATE_STOP;
		break;
	    case UEV_FINISH :
		sgraph_event_proc_info->state = SGRAPH_UEV_FINISH;
		break;
	    case UEV_ERROR :
		sgraph_event_proc_info->state = SGRAPH_UEV_ERROR;
		break;
	    default :
		break;
	}
	
	sgraph_event_proc_info->do_default_action = uev_info->do_default_action;

	switch (uev_info->type) {
	
	    case UEV_CLICK :
		sgraph_event_proc_info->type = SGRAPH_UEV_CLICK;
		break;
		
	    case UEV_DOUBLE_CLICK :
		sgraph_event_proc_info->type = SGRAPH_UEV_DOUBLE_CLICK;
		break;
		
	    case UEV_DRAG_NODE :
		sgraph_event_proc_info->type = SGRAPH_UEV_DRAG_NODE;
		sgraph_event_proc_info->details.node.what = uev_info->details.node.what;
		if (uev_info->details.node.node != empty_node) {
			sgraph_event_proc_info->details.node.node = (Snode)uev_info->details.node.node->sgraph_node;
		} else {
			sgraph_event_proc_info->details.node.node = empty_snode;
		}
		sgraph_event_proc_info->details.node.x  = uev_info->details.node.x;
		sgraph_event_proc_info->details.node.y  = uev_info->details.node.y;
		sgraph_event_proc_info->details.node.sx = uev_info->details.node.sx;
		sgraph_event_proc_info->details.node.sy = uev_info->details.node.sy;
		sgraph_event_proc_info->details.node.correction_x = uev_info->details.node.correction_x;
		sgraph_event_proc_info->details.node.correction_y = uev_info->details.node.correction_y;
		break;
		
	    case UEV_DRAG_EDGE :
		sgraph_event_proc_info->type = SGRAPH_UEV_DRAG_EDGE;
		sgraph_event_proc_info->details.edge.what = uev_info->details.edge.what;
		switch (uev_info->details.edge.what) {
		    case NEW_SEDGE :
			if (uev_info->details.edge.which.new_edge.source != empty_node) {
				sgraph_event_proc_info->details.edge.which.new_edge.source =
					(Snode)uev_info->details.edge.which.new_edge.source->sgraph_node;
			} else {
				sgraph_event_proc_info->details.edge.which.new_edge.source = empty_snode;
			}
			sgraph_event_proc_info->details.edge.which.new_edge.el =
				uev_info->details.edge.which.new_edge.el;
			break;
		    case OLD_SEDGE_REAL_POINT :
			if (uev_info->details.edge.which.real_point.edge != empty_edge) {
				sgraph_event_proc_info->details.edge.which.real_point.edge =
					(Sedge)uev_info->details.edge.which.real_point.edge->sgraph_edge;
			} else {
				sgraph_event_proc_info->details.edge.which.real_point.edge = empty_sedge;
			}
			sgraph_event_proc_info->details.edge.which.real_point.el =
				uev_info->details.edge.which.real_point.el;
			break;
		    case OLD_SEDGE_IMAGINARY_POINT :
			if (uev_info->details.edge.which.imaginary_point.edge != empty_edge) {
				sgraph_event_proc_info->details.edge.which.imaginary_point.edge =
					(Sedge)uev_info->details.edge.which.imaginary_point.edge->sgraph_edge;
			} else {
				sgraph_event_proc_info->details.edge.which.imaginary_point.edge = empty_sedge;

			}
			sgraph_event_proc_info->details.edge.which.imaginary_point.el =
				uev_info->details.edge.which.imaginary_point.el;
			break;
		}
		sgraph_event_proc_info->details.edge.x    = uev_info->details.edge.x;
		sgraph_event_proc_info->details.edge.y    = uev_info->details.edge.y;
		break;
		
	    case UEV_DRAG_GROUP	 :
		sgraph_event_proc_info->type = SGRAPH_UEV_DRAG_GROUP;
		sgraph_event_proc_info->details.group.group = sgraph_info->selection.group;
		sgraph_event_proc_info->details.group.x  = uev_info->details.group.x;
		sgraph_event_proc_info->details.group.y  = uev_info->details.group.y;
		sgraph_event_proc_info->details.group.x0 = uev_info->details.group.x0;
		sgraph_event_proc_info->details.group.y0 = uev_info->details.group.y0;
		sgraph_event_proc_info->details.group.correction_x = uev_info->details.group.correction_x;
		sgraph_event_proc_info->details.group.correction_y = uev_info->details.group.correction_y;
		break;
		
	    case UEV_DRAG_BOX :
		sgraph_event_proc_info->type = SGRAPH_UEV_DRAG_BOX;
		sgraph_event_proc_info->details.box.x1 = uev_info->details.box.x1;
		sgraph_event_proc_info->details.box.x1 = uev_info->details.box.x1;
		sgraph_event_proc_info->details.box.x2 = uev_info->details.box.x2;
		sgraph_event_proc_info->details.box.x2 = uev_info->details.box.x2;
		sgraph_event_proc_info->details.box.shift_is_down = uev_info->details.box.shift_is_down;
		break;
		
	    default :
		break;
	}
}


void	postprocess_sgraph_event_proc_info (sgraph_info, uev_info, sgraph_event_proc_info)
struct	sgraph_proc_info *sgraph_info;
UEV_info		 uev_info;
Sgraph_event_proc_info	 sgraph_event_proc_info;
{
	/* Pendant to prepare_sgraph_event_proc_info */
	
	uev_info->do_default_action = sgraph_event_proc_info->do_default_action;
	/* It's my only line !! */
}


void	postprocess_sgraph_info (sgraph_info,
		new_selected, new_selection, new_sgraph, new_buffer, selection_changed)
struct	sgraph_proc_info *sgraph_info;
Sgraph_selected		 *new_selected;
Sgraph_selection	 *new_selection;
Sgraph			 *new_sgraph;
int			 *new_buffer;
int			 *selection_changed;
{

	/* Check the new selection	*/
	if (sgraph_info->new_selected == SGRAPH_SELECTED_SAME) {
		*new_selected  = sgraph_info->selected;
		*new_selection = sgraph_info->selection;
		*new_sgraph    = sgraph_info->sgraph;
		*new_buffer    = sgraph_info->buffer;
		*selection_changed = FALSE;
	} else if (sgraph_info->new_sgraph == empty_sgraph) {
		*new_selected  = sgraph_info->new_selected;
		*new_selection = sgraph_info->new_selection;
		*new_sgraph    = sgraph_info->sgraph;
		*new_buffer    = sgraph_info->buffer;
		*selection_changed = TRUE;
	} else {
		*new_selected  = sgraph_info->new_selected;
		*new_selection = sgraph_info->new_selection;
		if (sgraph_info->new_sgraph != empty_sgraph) {
			*new_sgraph    = sgraph_info->new_sgraph;
			*new_buffer    = sgraph_info->new_buffer;
		} else {
			*new_sgraph    = sgraph_info->sgraph;
			*new_buffer    = sgraph_info->buffer;
		}
		*selection_changed = TRUE;
	}
	
	if (*new_buffer != wac_buffer) {
		if (*new_buffer == -1 || !buffer_is_used(*new_buffer)) {
			*new_buffer = create_buffer ();
		}
	}
	
	if (*new_sgraph != empty_sgraph && (*new_sgraph)->graphed == NULL) {
		(void) create_graphed_graph_from_sgraph (*new_sgraph);
		sgraph_info->no_changes           = TRUE;
		sgraph_info->no_structure_changes = TRUE;
		sgraph_info->recompute            = TRUE;
		sgraph_info->repaint              = TRUE;
		sgraph_info->recenter             = FALSE;
	}
}
	
	
void			 update_sgraph_structure (new_sgraph, sgraph_info, anything_changed, structure_changed)
Sgraph			 new_sgraph;
struct	sgraph_proc_info sgraph_info;
int			 *anything_changed;
int			 *structure_changed;
{
	/* Check for new or deleted nodes & edges	*/
	if (new_sgraph != empty_sgraph && !sgraph_info.no_structure_changes) {
	
		Graph	graph;
		Node	node;
		Edge	edge;
		Snode	snode;
		Sedge	sedge;
		Slist	remove_node_slist = empty_slist;
		Slist	remove_edge_slist = empty_slist;
		Slist	l;
		
		graph = (Graph)(new_sgraph->graphed);
		
		/* Reset the sgraph_...pointers.	*/
		for_nodes (graph, node) {
			node->sgraph_node = NULL;
			for_edge_sourcelist (node, edge) {
				edge->sgraph_edge = NULL;
			} end_for_edge_sourcelist (node, edge);
		} end_for_nodes (graph, node);
		
		/* Check for new nodes	*/
		for_all_nodes (new_sgraph, snode) {
			if (snode->graphed == NULL) {
				(void) create_graphed_node_from_snode (snode);
				*anything_changed  = TRUE;
				*structure_changed = TRUE;
			} else {
				((Node)(snode->graphed))->sgraph_node = (char *)snode;
			}
		} end_for_all_nodes (new_sgraph, snode);
	
		/* Check for new edges	*/
		for_all_nodes (new_sgraph, snode) {
			for_sourcelist (snode, sedge) {
				if (sedge->graphed == NULL) {
					if (new_sgraph->directed) {
						(void) create_graphed_edge_from_sedge (sedge);
					} else if (sedge < sedge->tsuc) {
						(void) create_graphed_edge_from_sedge (sedge);
					}
					*anything_changed  = TRUE;
					*structure_changed = TRUE;
				} else {
					((Edge)(sedge->graphed))->sgraph_edge = (char *)sedge;
				}
			} end_for_sourcelist (snode, sedge);
		} end_for_all_nodes (new_sgraph, snode);
		
		/* remove now all those nodes whose ->sgraph_... pointers are NULL	*/
		for_nodes (graph, node) {
			if (node->sgraph_node == NULL) {
				remove_node_slist = add_immediately_to_slist (remove_node_slist,
					make_attr (ATTR_DATA, (char *)node));
			}
		} end_for_nodes (graph, node);
		
		if (remove_node_slist != empty_slist) {
			*structure_changed = TRUE;
			*anything_changed  = TRUE;
			for_slist (remove_node_slist, l) {
				erase_and_delete_node (attr_data_of_type (l, Node));
			} end_for_slist (remove_node_slist, l);
			free_slist (remove_node_slist);
		}

		/* now, remove the edges	*/
		for_nodes (graph, node) {
			for_edge_sourcelist (node, edge) {
				if (edge->sgraph_edge == NULL) {
					remove_edge_slist = add_immediately_to_slist (remove_edge_slist,
						make_attr (ATTR_DATA, (char *)edge));
					}
			} end_for_edge_sourcelist (node, edge);
		} end_for_nodes (graph, node);
		
		if (remove_edge_slist != empty_slist) {
			*structure_changed = TRUE;
			*anything_changed  = TRUE;
			for_slist (remove_edge_slist, l) {
				erase_and_delete_edge (attr_data_of_type (l, Edge));
			} end_for_slist (remove_edge_slist, l);
			free_slist (remove_edge_slist);
		}
	}
}
			


void			 update_sgraph_coordinates_and_labels (sgraph_info, new_sgraph, anything_changed)
struct	sgraph_proc_info sgraph_info;
Sgraph			 new_sgraph;
int			 *anything_changed;
{
	Snode	snode;
	Sedge	sedge;
	
	/* Check for any changes */
	if (new_sgraph != empty_sgraph && !sgraph_info.no_changes) {
	
		for_all_nodes (new_sgraph, snode) {
	
			if (snode->x != ((Node)(snode->graphed))->x ||
			    snode->y != ((Node)(snode->graphed))->y) {
				node_set ((Node)(snode->graphed), ONLY_SET,
					NODE_POSITION, snode->x, snode->y,
					0);
				*anything_changed = TRUE;
			}
			if ((snode->label != NULL &&
			     ((Node)(snode->graphed))->label.text != NULL &&
			      strcmp (snode->label, ((Node)(snode->graphed))->label.text)) ||
			    (snode->label == NULL && ((Node)(snode->graphed))->label.text != NULL) ||
			    (snode->label != NULL && ((Node)(snode->graphed))->label.text == NULL)) {
				node_set ((Node)(snode->graphed), ONLY_SET,
					NODE_LABEL, snode->label,
					0);
				*anything_changed = TRUE;
			}
		
			for_sourcelist (snode, sedge) if (new_sgraph->directed || sedge < sedge->tsuc) {
				if ((sedge->label != NULL &&
				     ((Edge)(sedge->graphed))->label.text != NULL &&
				      strcmp (sedge->label, ((Edge)(sedge->graphed))->label.text)) ||
				    (sedge->label == NULL && ((Edge)(sedge->graphed))->label.text != NULL) ||
				    (sedge->label != NULL && ((Edge)(sedge->graphed))->label.text == NULL)) {
					edge_set ((Edge)(sedge->graphed), ONLY_SET,
						EDGE_LABEL, sedge->label,
						0);
					*anything_changed = TRUE;
				}
			} end_for_sourcelist (snode, sedge);

		} end_for_all_nodes (new_sgraph, snode);
	}
}
	
	

void			 update_sgraph_position (sgraph_info, new_sgraph, new_buffer,
				anything_changed, x_center_before, y_center_before)
struct	sgraph_proc_info sgraph_info;
Sgraph			 new_sgraph;
int			 new_buffer;
int			 *anything_changed;
int			 x_center_before;
int			 y_center_before;
{
	Snode		snode;
	Sedge		sedge;
	int		x_center_after;
	int		y_center_after;
	int		n_nodes;
	Edgeline	line, el;
	
	if (new_sgraph != empty_sgraph) {
	
		int	move_x, move_y;
		int	buffer_width, buffer_height;
		
		buffer_width =  (int)xv_get(canvases [new_buffer].canvas, CANVAS_WIDTH);
		buffer_height = (int)xv_get(canvases [new_buffer].canvas, CANVAS_HEIGHT);
		
		if (sgraph_info.recenter) {
			compute_sgraph_center (new_sgraph,
				&x_center_after, &y_center_after, &n_nodes);
			move_x = (x_center_before - x_center_after);
			move_y = (y_center_before - y_center_after);
		} else {
			move_x = 0;
			move_y = 0;
		}
		
		if (new_sgraph != empty_sgraph) for_all_nodes (new_sgraph, snode) {
			if (snode->x + move_x > buffer_width) {
				move_x = buffer_width - snode->x;
			}
			if (snode->y + move_y > buffer_height) {
				move_y = buffer_height - snode->y;
			}
		} end_for_all_nodes (new_sgraph, snode);
		
		if (new_sgraph != empty_sgraph) for_all_nodes (new_sgraph, snode) {
			if (snode->x + move_x < 0) {
				move_x = - snode->x;
			}
			if (snode->y + move_y < 0) {
				move_y = - snode->y;
			}
		} end_for_all_nodes (new_sgraph, snode);


		if (move_x != 0 || move_y != 0) {
		
			for_all_nodes (new_sgraph, snode) {

				for_sourcelist (snode, sedge) if (new_sgraph->directed || unique_edge(sedge)) {
				
					Edgeline line = (Edgeline)edge_get (graphed_edge(sedge), EDGE_LINE);
					Edgeline el;
				
					for_edgeline (line, el) {
						set_edgeline_xy (el, edgeline_x(el) + move_x,
								     edgeline_y(el) + move_y);
					} end_for_edgeline (line, el);
		
				} end_for_sourcelist (snode, sedge);
	    
				node_set (graphed_node(snode), ONLY_SET, NODE_POSITION,
					snode->x + move_x,
					snode->y + move_y,
					0);
			
			} end_for_all_nodes (new_sgraph, snode);

			*anything_changed = TRUE;
		}
	}
}



void			 update_sgraph_recompute (sgraph_info, new_sgraph, anything_changed)
struct	sgraph_proc_info sgraph_info;
Sgraph			 new_sgraph;
int			 *anything_changed;
{
	if (new_sgraph != empty_sgraph && (*anything_changed || sgraph_info.recompute)) {
	
		/* do a RESTORE_IT on new_sgraph				*/
		/* we cannot RESTORE_IT only the selected object,		*/
		/* because the Sgraph procedure may (and, in most cases, will)	*/
		/* have changed the whole graph					*/
	
		Group	g;
		
		g = make_group_of_graph (new_sgraph->graphed);
		if (g != empty_group) {
			group_set (g, RESTORE_IT, 0);
			free_group (g);
		}
	}
}
	
	

void			 update_sgraph_selection (sgraph_info, new_sgraph, new_selected, new_selection, selection_changed, structure_changed)
struct	sgraph_proc_info sgraph_info;
Sgraph			 new_sgraph;
Sgraph_selected		 new_selected;
Sgraph_selection	 new_selection;
int			 selection_changed;
int			 structure_changed;
{
	if (new_sgraph != empty_sgraph && !sgraph_info.save_selection &&
	    (structure_changed || selection_changed)) {
	
		/* renew the selections */
		
		int	found = FALSE;
		Slist	l;
		Snode	n;
		Sedge	e;
		Group	g = empty_group;
		
		switch (new_selected) {
		    case SGRAPH_SELECTED_NONE :
			dispatch_user_action (SELECT_GRAPH, (Graph)(new_sgraph->graphed));
		    case SGRAPH_SELECTED_NOTHING :
			break;
			
		    case SGRAPH_SELECTED_SNODE :
			for_all_nodes (new_sgraph, n) {
				if (n == new_selection.snode) {
					found = TRUE;
					break;
				}
			} end_for_all_nodes (new_sgraph, n);
			
			if (found) {
				dispatch_user_action (SELECT_NODE, (Node)(new_selection.snode->graphed));
			} else {
				dispatch_user_action (UNSELECT);
			}
			break;
			
		    case SGRAPH_SELECTED_SEDGE :
			for_all_nodes (new_sgraph, n) {
				for_sourcelist (n, e) {
					if (e == new_selection.sedge) {
						found = TRUE;
						break;
					}
				} end_for_sourcelist (n, e);
				if (found)
					break;
			} end_for_all_nodes (new_sgraph, n);
			
			if (found) {
				dispatch_user_action (SELECT_EDGE, (Edge)(new_selection.sedge->graphed));
			} else {
				dispatch_user_action (UNSELECT);
			}
			break;
			
		    case SGRAPH_SELECTED_GROUP :

			/* Build a Group of all *valid* elements in new_selection.group	*/
			for_slist (new_selection.group, l) {
			
				found = FALSE;
				for_all_nodes (new_sgraph, n) {
					if (attr_data_of_type(l, Snode) == n) {
						found = TRUE;
						break;
					}
				} end_for_all_nodes (new_sgraph, n);
					
				if (found) {
					g = add_to_group (g, (Node)(attr_data_of_type(l, Snode)->graphed));
				}
					
			} end_for_slist (new_selection.group, l);
				
			if (g != empty_group) {
				dispatch_user_action (SELECT_GROUP, g);
			} else {
				dispatch_user_action (UNSELECT);
			}
			break;
		    default :
			break;
		}
	}
}
	


typedef	enum	{
	SIMPLE_SGRAPH_PROC,
	SGRAPH_EVENT_PROC
}
	Sgraph_proc_type;
	
	
char			*general_call_sgraph_proc (type, proc, args1, args2)
Sgraph_proc_type	type;
void			(*proc)();
char			*args1;
char			*args2;

{
	Snode		snode, n;
	Sedge		sedge, e;
	int		buffer;
	int		anything_changed = FALSE;
	int		structure_changed = FALSE;
	int		selection_changed = FALSE;
	
	int	x_center_before = 0, y_center_before = 0;
	int	x_center_after  = 0, y_center_after  = 0;
	int	n_nodes  = 0;

	struct	sgraph_proc_info 	sgraph_info;
	struct	sgraph_event_proc_info	sgraph_event_proc_info;
	
	Sgraph_selected		 new_selected;
	Sgraph_selection	 new_selection;
	Sgraph			 new_sgraph;
	int			 new_buffer;

	if (test_user_interface_locked()) {
		bell ();
		return NULL;
	}

	prepare_sgraph_info (&sgraph_info, &x_center_before, &y_center_before, &n_nodes);
	
	switch (type) {
	    case SIMPLE_SGRAPH_PROC :
		break;
	    case SGRAPH_EVENT_PROC :
		prepare_sgraph_event_proc_info (sgraph_info, (UEV_info)args1, &sgraph_event_proc_info);
		break;
	}
	
	
	/* Now call the Sgraph-proc	*/
	switch (type) {
	    case SIMPLE_SGRAPH_PROC :
		proc (&sgraph_info);
		break;
	    case SGRAPH_EVENT_PROC :
		proc (&sgraph_info, &sgraph_event_proc_info, args2);
		break;
	}
	

	postprocess_sgraph_info (&sgraph_info,
		&new_selected, &new_selection, &new_sgraph, &new_buffer, &selection_changed);
		
	switch (type) {
	    case SIMPLE_SGRAPH_PROC :
		break;
	    case SGRAPH_EVENT_PROC :
		postprocess_sgraph_event_proc_info (sgraph_info, (UEV_info)args1, &sgraph_event_proc_info);
		break;
	}
	
	

	update_sgraph_structure (new_sgraph, sgraph_info,
		&anything_changed, &structure_changed);
	if (structure_changed && !sgraph_info.save_selection)
		dispatch_user_action (UNSELECT);
	update_sgraph_coordinates_and_labels (sgraph_info, new_sgraph,
		&anything_changed);
	update_sgraph_position (sgraph_info, new_sgraph, new_buffer,
		&anything_changed,
		x_center_before, y_center_before);
	update_sgraph_recompute (sgraph_info, new_sgraph,
		&anything_changed);
	update_sgraph_selection (sgraph_info, new_sgraph,
		new_selected, new_selection,
		selection_changed, structure_changed);

	if (sgraph_info.selected == SGRAPH_SELECTED_GROUP)
		free_slist (sgraph_info.selection.group);
	if (sgraph_info.new_selected == SGRAPH_SELECTED_GROUP)
		free_slist (sgraph_info.new_selection.group);
	
	if (sgraph_info.repaint || sgraph_info.recompute || anything_changed) {
		force_repainting();
	}

	return NULL; /* dummy return so that the procedure can be used as menu action proc	*/
}



char	*call_sgraph_proc (proc)
void	(*proc)();
{
	return	general_call_sgraph_proc (SIMPLE_SGRAPH_PROC, proc, NULL);
}



char		*call_sgraph_event_proc (proc, uev_info, event)
void		(*proc)();
UEV_info	uev_info;
Event		*event;
{
	return	general_call_sgraph_proc (SGRAPH_EVENT_PROC, proc, uev_info, event);
}

/************************************************************************/
/*									*/
/*			    Misc. Utilities				*/
/*									*/
/************************************************************************/


int	sgraph_create_new_buffer ()
{
	return create_buffer();
}


int	sgraph_buffer_exists (b)
int	b;
{
	return buffer_is_used(b);
}


int	sgraph_get_buffer (b)
int	b;
{
	if (buffer_is_used(b)) {
		return b;
	} else {
		return create_buffer();
	}
}


void	sgraph_set_working_area_buffer (b)
int	b;
{
	set_working_area_canvas (canvases[b].canvas);
}


Snode	sedge_real_source (sedge)
Sedge	sedge;
{
	return ((Snode)((Edge)(sedge->graphed))->source->sgraph_node);
}


Snode	sedge_real_target (sedge)
Sedge	sedge;
{
	return ((Snode)((Edge)(sedge->graphed))->target->sgraph_node);
}


/*									*/



void	compute_sgraph_center (sgraph, x_center,y_center, n_nodes)
Sgraph	sgraph;
int	*x_center,*y_center;
int	*n_nodes;
{
	Snode	n;
	
	*x_center = 0;
	*y_center = 0;
	*n_nodes  = 0;
	
	if (sgraph != empty_sgraph) {
		for_all_nodes (sgraph, n) {
			*x_center += n->x;
			*y_center += n->y;
			(*n_nodes)++;
		} end_for_all_nodes (sgraph, n);
	}
	if (*n_nodes > 0) {
		*x_center /= *n_nodes;
		*y_center /= *n_nodes;
	} else {
		*x_center = 0;
		*y_center = 0;

	}
}


/************************************************************************/
/*									*/
/*		        Detach Sgraph from GraphEd graphs		*/
/*									*/
/************************************************************************/


void	delete_attatched_snode (node)
Node	node;
{
	if (node->sgraph_node = NULL) {
		remove_node (node->sgraph_node);
		node->sgraph_node = NULL;
	}
}


void	delete_attatched_sedge (edge)
Edge	edge;
{
	if (edge->sgraph_edge) {
		remove_edge (edge->sgraph_edge);
		edge->sgraph_edge = NULL;
	}
}


void	delete_attatched_sgraph (graph)
Graph	graph;
{
	Node	node;
	Edge	edge;
	
	if (graph->sgraph_graph != NULL) {
	
		for_nodes (graph, node) if (node->sgraph_node != NULL) {
			for_edge_sourcelist (node, edge) {
				delete_attatched_sedge (edge);
			} end_for_edge_sourcelist (node, edge);
		} end_for_nodes (graph, node);
	
		for_nodes (graph, node) {
			delete_attatched_snode (node);
		} end_for_nodes (graph, node);
	
		remove_graph (graph->sgraph_graph);
		graph->sgraph_graph = NULL;
	}
}


void	delete_attatched_sgroup (group)
Group	group;
{
	Group	g;
	Node	node;
	Edge	edge;
	
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) {
			if (node->sgraph_node != NULL && contains_group_node (group, edge->target)) {
				delete_attatched_sedge (edge);
			}
		} end_for_edge_sourcelist (g->node, edge);
		for_edge_targetlist (g->node, edge) {
			if (node->sgraph_node != NULL && contains_group_node (group, edge->target)) {
				delete_attatched_sedge (edge);
			}
		} end_for_edge_targetlist (g->node, edge);
	} end_for_group (group, g);
	
	for_group (group, g) {
		delete_attatched_snode (g->node);
	} end_for_group (group, g);
}



#endif INCLUDE_SGRAPH
