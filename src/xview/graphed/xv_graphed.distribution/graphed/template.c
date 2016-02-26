/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				template.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	A template is a snapshot of a node or edge.			*/
/*	There are routines to rebuild a node from its template		*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "group.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/


Node_template	create_node_template (node)
Node		node;
{
	Node_template template;
	
	template.attributes = get_node_attributes (node);
	template.graph_nr   = node->nr;
	template.node_nr    = node->graph->nr;
	
	return	template;
}


Edge_template	create_edge_template (edge)
Edge		edge;
{
	Edge_template template;
	
	template.attributes = get_edge_attributes (edge);
	template.edge_nr    = edge->nr;
	template.source_nr  = edge->source->nr;
	template.target_nr  = edge->target->nr;
	template.graph_nr   = edge->source->graph->nr;
	
	return	template;
}



Node		create_node_from_template (template)
Node_template	template;
{
	int	i, found = FALSE;
	Graph	g, graph;
	Node	node;
	
	for (i=0; i<N_BUFFERS && !found; i++) {
		for_all_graphs (i, g) {
			if (g->nr == template.graph_nr) {
				found = TRUE;
				graph = g;
				break;
			}
		} end_for_all_graphs (i, g)
	}
	
	if (!found) {
		node = empty_node;
	} else {
		node_set (node = get_node_with_number (graph, template.node_nr),
			SET_NODE_ATTRIBUTES(template.attributes),
			0);
	}
	
	return	node;
}


Edge		create_edge_from_template (template)
Edge_template	template;
{
	int	i, found        = FALSE,
		   found_graph  = FALSE;
	Graph	g, graph;
	Node	n, node, source, target;
	Edge	e, edge;
	
	for (i=0; i<N_BUFFERS && !found_graph; i++) {
		for_all_graphs (i, g) {
			if (g->nr == template.graph_nr) {
				found_graph = TRUE;
				graph = g;
				break;
			}
		} end_for_all_graphs (i, g)
	}
	
	if (!found_graph)
		return empty_edge;
	
	if ((source = find_node_with_number (graph, template.source_nr)) == empty_node ||
	    (target = find_node_with_number (graph, template.target_nr)) == empty_node) {
		return empty_edge;
	}
	
	for_edge_sourcelist (source, e) {
		if (e->nr == template.edge_nr) {
			found = TRUE;
			edge = e;
		}
	} end_for_edge_sourcelist (source, e);
	
	if (!found) for_edge_targetlist (target, e) {
		if (e->nr == template.edge_nr) {
			found = TRUE;
			edge = e;
		}
	} end_for_edge_targetlist (target, e);

	if (!found)
		edge = create_edge_with_number (graph, source, target, template.edge_nr);
	
	edge_set (edge,
		SET_EDGE_ATTRIBUTES(template.attributes),
		0);
	
	return edge;
}
