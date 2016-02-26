/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

/************************************************************************/
/*									*/
/*				stdnode.c				*/
/*									*
/************************************************************************/

#include "std.h"
#include "sgraph.h"


Snode		make_node (graph, attrs)
Sgraph		graph;
Attributes	attrs;
{
	return make_node_with_number (graph, attrs, -1);
}



Snode		make_node_with_number (graph, attrs, nr)
Sgraph		graph;
Attributes	attrs;
int		nr;
{
	Snode		node, new_node;
	static	int	highest_number_up_to_now = 1;

	if (nr != -1) {
		/* Look for a node with the given number.	*/
		/* If one exists, return it.			*/
		for_all_nodes (graph, node)
			if (node->nr == nr) return node;
		end_for_all_nodes (graph, node);
	}
	
	/* We have not found a node with nr -- create a new one	*/
	
	new_node = (Snode)mymalloc (sizeof(struct snode));

	/* Insert the new node					*/
	
	if (graph->nodes == empty_node) {
		graph->nodes  = new_node;
		new_node->suc = new_node;
		new_node->pre = new_node;
	} else {
		new_node->pre      = last_node_in_graph  (graph);
		new_node->suc      = first_node_in_graph (graph);
		new_node->pre->suc = new_node;
		new_node->suc->pre = new_node;
	}
	
	new_node->slist = empty_edge;
	new_node->tlist = empty_edge;
	new_node->graph = graph;
	new_node->nr    = iif (nr == -1, ++highest_number_up_to_now, nr);
	new_node->label = nil;
	new_node->attrs = attrs;
	new_node->x     = -1;
	new_node->y     = -1;
#ifdef GRAPHED_POINTERS
	new_node->graphed = NULL;
#endif
#ifdef SGRAGRA_POINTERS
	new_node->embedding = NULL;
#endif

	if (new_node->graph->make_node_proc != NULL) {
		new_node->graph->make_node_proc (new_node);
	}

	if (nr > highest_number_up_to_now)
		highest_number_up_to_now = nr;

	return	new_node;
}


Global	void	remove_node (node)
Snode		node;
{
	Sgraph	graph;
	Sedge	edge;

	while ( (edge = node->slist) != empty_edge)
		remove_edge (edge);
	while ( (edge = node->tlist) != empty_edge)
		remove_edge (edge);

	graph = node->graph;
	if (node->suc == node)
		graph->nodes = empty_node;
	else {
		node->pre->suc = node->suc;
		node->suc->pre = node->pre;
		if (node == graph->nodes)
			graph->nodes = node->suc;
	}
	
	if (node->graph->remove_node_proc != NULL) {
		node->graph->remove_node_proc (node);
	}

	myfree (node);
}


Global	void	set_nodelabel (node, text)
Snode		node;
char		*text;
{
	node->label = text;
}


Global	void	set_nodeattrs (node, attrs)
Snode		node;
Attributes	attrs;
{
	node->attrs = attrs;
}


Global	void	set_node_xy (node, x,y)
Snode		node;
int		x,y;
{
	node->x = x;
	node->y = y;
}
