/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				group.c					*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "group.h"

#include "graphed_subwindows.h"

/************************************************************************/
/*									*/
/*		    GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	Group	new_group           (node)				*/
/*	Group	add_to_group        (group, node)			*/
/*	Group	subtract_from_group (group, node)			*/
/*									*/
/*	Group	add_groups          (g1,g2)				*/
/*	Group	add_groups_disjoint (g1,g2)				*/
/*	Group	subtract_groups     (g1,g2)				*/
/*									*/
/*	Group	free_group (group)					*/
/*	Group	copy_group (group)					*/
/*									*/
/*	int	group_contains_exactly_one_node (group)			*/
/*	Group	contains_group_node             (group, graph)		*/
/*	Group	contains_group_graph            (group, graph)		*/
/*	int	intersects_group_graph          (group, graph)		*/
/*	int	group_intersects_group          (group1, group2)	*/
/*									*/
/*	Rect	compute_rect_around_group (group)			*/
/*									*/
/*	Group	make_group_of_graph (graph)				*/
/*	Group	make_group_of_sourcelist (node)				*/
/*	Group	make_group_of_targetlist (node)				*/
/*	Graph	make_graph_of_graph (buffer, graph)			*/
/*	Group   make_group_of_all()					*/
/*	Group	copy_group_to_graph (group, graph)			*/
/*	int	size_of_group (group)					*/
/*	int	group_nodes_are_all_of_same_graph (group)		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	move_group (group, dx,dy)				*/
/*	void	group_set (group, ...)					*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*		    LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	Group	add_immediately_to_group	(group, node)		*/
/*	Group	subtract_immediately_from_group	(group, g)		*/
/*									*/
/************************************************************************/

static	void	do_move_group                   ();


/************************************************************************/
/*									*/
/*			Datenstruktur Group				*/
/*									*/
/************************************************************************/
/*									*/
/*	'Group' is a set of (pointers to) nodes.			*/
/*									*/
/*	typedef	struct	group	{					*/
/*									*/
/*		Node		node;					*/
/*		struct	group	*pre,suc;				*/
/*									*/
/*	}								*/
/*									*/
/*	A group is a double linked list of pointers to nodes :		*/
/*	node	*the* node						*/
/*	pre,suc	pointer to predecessor / successor			*/
/*									*/
/*======================================================================*/
/*									*/
/*	All procedures workingon group do have their own memory		*/
/*	management.							*/
/*									*/
/************************************************************************/
/*									*/
/*	Group	new_group (node)					*/
/*									*/
/*	Creates a new group with node as the only element.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	Group	add_immediately_to_group (group, node)		*/
/*									*/
/*	Fuegt node in die Gruppe group ein. Falls group == empty_group,	*/
/*	so wird eine komplette neue Gruppe erzeugt.			*/
/*	ACHTUNG : Es wird keine Abfrage dahingehend vorgenommen, ob der	*/
/*	Knoten bereits in der Gruppe enthalten ist.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	add_to_group (group, node)				*/
/*									*/
/*	Wie oben, es wird aber nur dann eingefuegt, wenn der Knoten	*/
/*	nicht schon in der Gruppe enthalten war.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	Group	subtract_immediately_from_group (group, g)	*/
/*									*/
/*	Entfernt das Element g (Gruppenelement !) aus group.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	subtract_from_group (group, node)			*/
/*									*/
/*	Entfernt node aus group (falls vorhanden).			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	add_groups (g1, g2)					*/
/*									*/
/*	Fuegt die Elemente von g2 zu g1 hinzu. ACHTUNG : Seiteneffekt	*/
/*	auf die Elemente von g1 !					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	add_groups_disjoint (g1, g2)				*/
/*									*/
/*	Bildet eine neue Gruppe, die aus der disjunkten Vereinigung von	*/
/*	g1 und g2 besteht.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	subtract_groups (g1, g2)				*/
/*									*/
/*	Entfernt alle in g2 enthaltenen Elemente aus g1. ACHTUNG :	*/
/*	Seiteneffekt auf die Elemente in g1 !				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	free_group (group)					*/
/*									*/
/*	Speicherfreigabe fuer die Elemente in group. Die obigen		*/
/*	Prozeduren erledigen im uebrigen die Speicherverwaltung		*/
/*	selbsttaetig, so dass free_group lediglich am Ende der Arbeit	*/
/*	fuer Aufraeumarbeiten benoetigt wird.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	copy_group (group)					*/
/*									*/
/*	Gibt eine vollstaendige Kopie von group zurueck.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	group_contains_exactly_one_node (group)			*/
/*									*/
/*	Prueft, ob group aus genau einem Knoten besteht.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	contains_group_node (group, node)			*/
/*									*/
/*	contains_group_node bestimmt das Element von group, das node	*/
/*	enthaelt. Falls es kein solches gibt, wird empty_group		*/
/*	zurueckgegeben.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	Rect	compute_rect_around_goup (group)			*/
/*									*/
/*	Berechnet das die Gruppe umfassende Rechteck.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	make_group_of_graph (graph)				*/
/*									*/
/*	Bildet eine Gruppe, die alle Knoten von graph umfasst.		*/
/*									*/
/************************************************************************/



Group	new_group (node)
Node	node;
{
	Group	new;
	
	new = (Group)mymalloc (sizeof (struct group));
	
	new->node = node;
	new->suc  = new;
	new->pre  = new;
	
	return	new;
}


Group	add_immediately_to_group (group, node)
Group		group;
Node		node;
{
	/* Warning : no check about double elements	*/
	
	Group	new;
	
	new = new_group (node);
	if (group != empty_group) {
		new->suc      = group;
		new->pre      = group->pre;
		new->suc->pre = new;
		new->pre->suc = new;
	} else {
		group = new;
	}
	
	return	group;
}


Group	add_to_group (group, node)
Group	group;
Node	node;
{
	Group	new;
	
	if (node == empty_node)
		return group;
	else if (contains_group_node (group, node) == empty_group)
		return add_immediately_to_group (group, node);
	else
		return	group;
}


Group	subtract_immediately_from_group (group, g)
Group		group, g;
{
	if (g == empty_group) {
		; /* nothing to do ... */
	} else if (group->suc == group) {
		/* einelementige Gruppe	*/
		group = empty_group;
	} else if (g == group) {
		/* Anfangselement	*/
		group->suc->pre = group->pre;
		group->pre->suc = group->suc;
		group = group->suc;
	} else {
		g->pre->suc = g->suc;
		g->suc->pre = g->pre;
	}
	myfree (g);
	
	return group;
}


Group	subtract_from_group (group, node)
Group	group;
Node	node;
{
	Group	g;
	
	if (node == empty_node)
		return group;
	else if ((g = contains_group_node (group, node)) != empty_group)
		return subtract_immediately_from_group (group, g);
	else
		return group;
}


Group	add_groups (g1, g2)
Group	g1, g2;
{
	Group	g;
	
	for_group (g2, g) {
		g1 = add_to_group (g1, g->node);
	} end_for_group (g2, g);
	
	return	g1;
}


Group	add_groups_disjoint (g1, g2)
Group	g1, g2;
{
	Group	g;
	Group	disjoint_group;
	
	disjoint_group = empty_group;
	
	for_group (g1, g) {
		if (contains_group_node (g2, g->node) == empty_group)
			disjoint_group = add_immediately_to_group (
				disjoint_group, g->node);
	} end_for_group (g1, g);

	for_group (g2, g) {
		if (contains_group_node (g1, g->node) == empty_group)
			disjoint_group = add_immediately_to_group (
				disjoint_group, g->node);
	} end_for_group (g2, g);
	
	return	disjoint_group;
}


Group	subtract_groups (g1, g2)
Group	g1, g2;
{
	Group	g;
	
	for_group (g2, g) {
		g1 = subtract_from_group (g1, g->node);
	} end_for_group (g2, g);
	
	return g1;
}


void	free_group (group)
Group	group;
{
	Group	suc, g;
	
	g = group;
	if (group != empty_group) do {
		suc = g->suc;
		myfree (g);
		g = suc;
	} while (g != group);
}


Group	copy_group (group)
Group	group;
{
	Group	g, copy;
	
	copy = empty_group;
	
	for_group (group, g) {
		copy = add_to_group (copy, g->node);
	} end_for_group (group, g);
	
	return copy;
}


int	group_contains_exactly_one_node	(group)
Group	group;
{
	if (group != empty_group)
		return group == group->suc;
	else
		return FALSE;
}


Group	contains_group_node (group, node)
Group	group;
Node	node;
{
	Group	g;
	
	if (node != empty_node) for_group (group, g) {
		if (g->node == node) return g;
	} end_for_group (group, g);
	
	return empty_group;
}


int	contains_group_graph (group, graph)
Group	group;
Graph	graph;
{
	Node	node;

	for_nodes (graph, node) {
		if (contains_group_node (group, node) == empty_group)
			return FALSE;
	} end_for_nodes (graph, node);

	return TRUE;
}


int	intersects_group_graph (group, graph)
Group	group;
Graph	graph;
{
	Group	g;

	for_group (group, g) {
		if (g->node->graph == graph)
			return TRUE;
	} end_for_group (group, g);

	return FALSE;
}


int	group_intersects_group (group1, group2)
Group	group1, group2;
{
	Group	g1,g2;

	if (group1 == empty_group || group2 == empty_group)
		return FALSE;
	
	for_group (group1, g1) {
		if (contains_group_node (group2, g1->node) != empty_group)
			return TRUE;
	} end_for_group (group1, g1);
	
	for_group (group2, g2) {
		if (contains_group_node (group1, g2->node) != empty_group)
			return TRUE;
	} end_for_group (group2, g2);

	return FALSE;
}



Rect	compute_rect_around_group (group)
Group	group;
{
	Group	g;
	Edge	edge;
	Rect	rect;
	
	rect = rect_null;
	for_group (group, g) {
		rect = rect_bounding (&rect, &(g->node->box));
		for_edge_sourcelist (g->node, edge) {
			rect = rect_bounding (&rect, &(edge->box));
		} end_for_edge_sourcelist (g->node, edge);
		for_edge_targetlist (g->node, edge) {
			rect = rect_bounding (&rect, &(edge->box));
		} end_for_edge_targetlist (g->node, edge);
	} end_for_group (group, g);
	
	return rect;
}


Group	make_group_of_graph (graph)
Graph	graph;
{
	Node	node;
	Group	group_to_make;
	
	group_to_make = empty_group;
	
	for_nodes (graph, node) {
		group_to_make = add_to_group (group_to_make, node);
	} end_for_nodes (graph, node);
	
	return group_to_make;
}



Group	make_group_of_all ()
{
	Graph	graph;
	Node	node;
	Group	group_to_make;
	
	group_to_make = empty_group;
	
	for_all_graphs (wac_buffer, graph) {
	    for_nodes (graph, node) {
		group_to_make = add_immediately_to_group (group_to_make, node);
	    } end_for_nodes (graph, node);
	} end_for_all_graphs (wac_buffer, graph);

	return group_to_make;
}



Group	make_group_of_sourcelist (node)
Node	node;
{
	Edge	edge;
	Group	group_to_make;
	
	group_to_make = empty_group;
	
	for_edge_sourcelist (node, edge) {
		group_to_make = add_to_group (group_to_make, edge->target);
	} end_for_edge_sourcelist (node, edge);
	
	return group_to_make;
}



Group	make_group_of_targetlist (node)
Node	node;
{
	Edge	edge;
	Group	group_to_make;
	
	group_to_make = empty_group;
	
	for_edge_targetlist (node, edge) {
		group_to_make = add_to_group (group_to_make, edge->source);
	} end_for_edge_targetlist (node, edge);
	
	return group_to_make;
}



Graph	make_graph_of_group (buffer, group)
int	buffer;
Group	group;
{
	Graph	new_graph;
	Node	node;
	Edge	edge;
	Group	g;
	
	
	new_graph = create_graph (buffer);
	
	for_group (group, g) {
		(void) copy_node (new_graph, g->node);
	} end_for_group (group, g);
	
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) {
			if (contains_group_node (g, edge->target))
				(void) copy_edge (new_graph, edge);
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);

	return new_graph;
}


Group	copy_group_to_graph (group, graph)
Group	group;
Graph	graph;
{
	Node	node;
	Edge	edge;
	Group	copied_nodes = empty_group;
	Group	g;
	
	
	for_group (group, g) {
		node = copy_node (graph, g->node);
		copied_nodes = add_to_group (copied_nodes, node);
	} end_for_group (group, g);
	
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) {
			if (contains_group_node (g, edge->target))
				(void) copy_edge (graph, edge);
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);

	return copied_nodes;
}


int	size_of_group (group)
Group	group;
{
	Group	g;
	int	count = 0;
	
	for_group (group, g) {
		count ++;
	} end_for_group (group, g);
	
	return count;
}


int	group_nodes_are_all_of_same_graph (group)
Group	group;
{
	Group	g;
	Graph	graph = empty_graph;
	
	if (group == empty_group)
		return TRUE;
	
	graph = group->node->graph;
	for_group (group, g) {
		if (g->node->graph != graph)
			return FALSE;
	} end_for_group (group, g);
	
	return TRUE;
}
	
/************************************************************************/
/*									*/
/*			    ZEICHENROUTINEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	group_set (group, <Attributes> ...)			*/
/*									*/
/*	Analog node_set oder edge_set.					*/
/*									*/
/*	Syntax :							*/
/*	group_set (group, [ONLY_SET,]					*/
/*		[NODE_POSITION,         x,y,]				*/
/*		[NODE_SIZE,             width, height,]			*/
/*		[NODE_TYPE,             index,]				*/
/*		[EDGE_TYPE,             index,]				*/
/*		[NODE_LABEL,            text,]				*/
/*		[EDGE_LABEL,            text,]				*/
/*		[NODE_FONT,             index,]				*/
/*		[EDGE_FONT,             index,]				*/
/*		[NODE_LABEL_VISIBILITY, nlp,]				*/
/*		[EDGE_LABEL_VISIBILITY, nlp,]				*/
/*		[NODE_NEI,              nei,]				*/
/*		[EDGE_ARROW_LENGTH,     length,]			*/
/*		[EDGE_ARROW_ANGLE,      angle,]				*/
/*		[MOVE,                  dx,dy,]				*/
/*		[RESTORE_IT,]						*/
/*		0);							*/
/*									*/
/*	ONLY_SET dient zum schnellen setzen von Parametern; vor dem	*/
/*	Neuzeichnen muss unbedingt RESTORE_IT aufgerufen werden.	*/
/*	(ONLY_SET setzt nur den Wert ein und loescht/zeichnet nicht und	*/
/*	fuehrt keine adjust_...	Prozeduren durch).			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	do_move_group (group, dx,dy, only_set)			*/
/*									*/
/*	Verschiebe group um (dx,dy); only_set kommt von group_set.	*/
/*									*/
/************************************************************************/



void	group_set (va_alist)
va_dcl
{
	va_list		args;
	
	int		adjust_nlp   = FALSE,	/* Flags whether to	*/
			adjust_edges = FALSE,	/* adjust attributes	*/
			adjust_graph = FALSE,
			adjust_node_label_text_to_draw = FALSE,
			adjust_elp      = FALSE,
			adjust_arrow    = FALSE,
			adjust_edge     = FALSE,	/* adjust edge's box	*/
			adjust_edge_label_text_to_draw = FALSE;

	int		set_nodeposition    = FALSE,
			set_nodesize        = FALSE,
			set_nodetype        = FALSE,
			set_edgetype        = FALSE,
			set_nei             = FALSE,
			set_nodelabel       = FALSE,
			set_edgelabel       = FALSE,
			set_nodefont        = FALSE,
			set_edgefont        = FALSE,
			set_nodelabel_visibility = FALSE,
			set_edgelabel_visibility = FALSE,
			set_nlp             = FALSE,
			set_arrow_length    = FALSE,
			set_arrow_angle     = FALSE,
			set_nodecolor       = FALSE,
			set_edgecolor       = FALSE,
			move                = FALSE;
			
	int		only_set   = FALSE;
	int		restore_it = FALSE;
			
	Set_attribute	attr;
	
	Group		group, g;
	Node		node;
	Edge		edge;
	int		x,y, dx,dy, width, height;
	int		nodetype_index, edgetype_index, nodefont_index,
			edgefont_index, nodelabel_visibility,
			edgelabel_visibility;
	char		*node_label_text, *edge_label_text;
	int		length;
	double		angle;
	int		nodecolor, edgecolor;
	Node_edge_interface	nei;
	Nodelabel_placement	nlp;
	

	va_start (args);
	
	group = va_arg (args, Group);
	attr = va_arg (args, Set_attribute);
	if (attr == ONLY_SET) {
		only_set = TRUE;
		attr = va_arg (args, Set_attribute);
	}
		
	/* Set the flags etc.	*/
	
	while (attr != SET_ATTRIBUTE_END) {
	
		switch (attr) {
	
		    case NODE_POSITION :
			x = va_arg (args, int);
			y = va_arg (args, int);
			adjust_nlp       = TRUE;
			adjust_edges     = TRUE;
			adjust_graph     = TRUE;
			set_nodeposition = TRUE;
			break;
		
		    case NODE_SIZE :
			width = va_arg (args, int);
			height = va_arg (args, int);
			if (width < 1)  width  = 1;
			if (height < 1) height = 1;
			adjust_nlp   = TRUE;
			adjust_edges = TRUE;
			adjust_graph = TRUE;
			adjust_node_label_text_to_draw = TRUE;
			set_nodesize = TRUE;
			break;
	
		    case NODE_TYPE :
			nodetype_index = va_arg (args, int);
			adjust_node_label_text_to_draw = TRUE;
			adjust_nlp                = TRUE;
			adjust_edges              = TRUE;
			set_nodetype              = TRUE;
			break;
	
		    case NODE_NEI :
			nei = va_arg (args, Node_edge_interface);
			adjust_edges = TRUE;
			adjust_graph = TRUE;
			set_nei      = TRUE;
			break;
	
		    case NODE_LABEL :
			node_label_text = va_arg (args,char *);
			adjust_node_label_text_to_draw = TRUE;
			adjust_nlp                     = TRUE;
			set_nodelabel                  = TRUE;
			break;
	
		    case NODE_FONT :
			nodefont_index = va_arg (args, int);
			adjust_node_label_text_to_draw = TRUE;
			adjust_nlp                     = TRUE;
			set_nodefont                   = TRUE;
			break;
	
		    case NODE_LABEL_VISIBILITY :
			nodelabel_visibility     = va_arg (args, int);
			set_nodelabel_visibility = TRUE;
			break;
	
		    case NODE_NLP :
			nlp         = va_arg (args, Nodelabel_placement);
			adjust_nlp  = TRUE;
			set_nlp     = TRUE;
			break;
	
		    case EDGE_TYPE :
			edgetype_index = va_arg (args, int);
			set_edgetype   = TRUE;
			break;
	
		    case EDGE_LABEL :
			edge_label_text = va_arg (args,char *);
			adjust_edge_label_text_to_draw = TRUE;
			adjust_elp                     = TRUE;
			adjust_edge                    = TRUE;
			adjust_graph                   = TRUE;
			set_edgelabel                  = TRUE;
			break; 
	
		    case EDGE_FONT :
			edgefont_index = va_arg (args, int);
			adjust_edge_label_text_to_draw = TRUE;
			adjust_elp                     = TRUE;
			adjust_edge                    = TRUE;
			adjust_graph                   = TRUE;
			set_edgefont                   = TRUE;
			break;
	
		    case EDGE_LABEL_VISIBILITY :
			edgelabel_visibility = va_arg (args, int);
			set_edgelabel_visibility  = TRUE;
			break;
	
		    case EDGE_ARROW_LENGTH :
			length            = va_arg (args, int);
			adjust_arrow      = TRUE;
			adjust_edge       = TRUE;
			adjust_graph      = TRUE;
			set_arrow_length  = TRUE;
			break;
	
		    case EDGE_ARROW_ANGLE :
			angle           = va_arg (args, double);
			adjust_arrow     = TRUE;
			adjust_edge      = TRUE;
			adjust_graph     = TRUE;
			set_arrow_angle  = TRUE;
			break;
	
		    case RESTORE_IT :
			adjust_nlp                     = TRUE;
			adjust_elp                     = TRUE;
			adjust_edges                   = TRUE;
			adjust_graph                   = TRUE;
			adjust_edge_label_text_to_draw = TRUE;
			adjust_node_label_text_to_draw = TRUE;
			break;
	
		    case NODE_COLOR :
			nodecolor      = va_arg (args, int);
			set_nodecolor  = TRUE;
			break;
	
		    case EDGE_COLOR :
			edgecolor      = va_arg (args, int);
			set_edgecolor  = TRUE;
			break;
	
		    case MOVE :
			dx = va_arg (args, int);
			dy = va_arg (args, int);
			move                      = TRUE;
			adjust_graph              = TRUE;
			break;
		
		    default :
			break;
		}
	
		attr = va_arg (args, Set_attribute);
	}
	
	
	/* Unmark && erase	*/
	
	if (!restore_it) {
		for_group (group, g) {
		    node = g->node;
		    set_node_to_be_marked (node);
		    for_edge_sourcelist (node, edge) {
			    set_edge_to_be_marked (edge);
		    } end_for_edge_sourcelist (node, edge);
		    for_edge_targetlist (node, edge) {
			    set_edge_to_be_marked (edge);
		    } end_for_edge_targetlist (node, edge);
		} end_for_group (group, g);
		erase_group (group);
	}
	
	

	/* Let's do It		*/
	
	if (set_nodeposition || set_nodesize  || set_nodetype             ||
	    set_nodelabel    || set_nodefont  || set_nodelabel_visibility ||
	    set_nei          || set_nlp       || set_nodecolor            ||
	    set_edgetype     || set_edgelabel || set_edgelabel_visibility ||
	    set_edgefont     || set_edgecolor || set_arrow_length         ||
	    set_arrow_angle)
	for_group (group, g) {
	
		node = g->node;
		
		if (set_nodeposition)
			node_set (node, ONLY_SET, NODE_POSITION, x,y, 0);
		if (set_nodesize)
			node_set (node, ONLY_SET, NODE_SIZE, width, height, 0);
		if (set_nodetype)	
			node_set (node, ONLY_SET, NODE_TYPE, nodetype_index, 0);
		if (set_nei)	
			node_set (node, ONLY_SET, NODE_NEI, nei, 0);
		if (set_nodelabel)
			node_set (node, ONLY_SET, NODE_LABEL, node_label_text, 0);
		if (set_nodefont)	
			node_set (node, ONLY_SET, NODE_FONT, nodefont_index, 0);
		if (set_nodelabel_visibility)
			node_set (node, ONLY_SET, NODE_LABEL_VISIBILITY, nodelabel_visibility, 0);
		if (set_nlp)
			node_set (node, ONLY_SET, NODE_NLP, nlp, 0);
		if (set_nodecolor)
			node_set (node, ONLY_SET, NODE_COLOR, nodecolor, 0);
		
		if (set_edgetype     || set_edgelabel   || set_edgefont || set_edgelabel_visibility ||
		    set_arrow_length || set_arrow_angle || set_edgecolor) for_edge_sourcelist (node, edge) {
		    
			if (contains_group_node (group, edge->target)) {
				if (set_edgetype)
					edge_set (edge, ONLY_SET, EDGE_TYPE, edgetype_index, 0);
				if (set_edgelabel)
					edge_set (edge, ONLY_SET, EDGE_LABEL, edge_label_text, 0);
				if (set_edgefont)
					edge_set (edge, ONLY_SET, EDGE_FONT, edgefont_index, 0);
				if (set_edgelabel_visibility)
					edge_set (edge, ONLY_SET, EDGE_LABEL_VISIBILITY, edgelabel_visibility, 0);
				if (set_arrow_length)
					edge_set (edge, ONLY_SET, EDGE_ARROW_LENGTH, length, 0);
				if (set_arrow_angle)
					edge_set (edge, ONLY_SET, EDGE_ARROW_ANGLE, angle, 0);
				if (set_edgecolor)
					edge_set (edge, ONLY_SET, EDGE_COLOR, edgecolor, 0);
			}
		} end_for_edge_sourcelist (node, edge);
		
	} end_for_group (group, g);
		
	if (move)
		do_move_group (group, dx,dy, only_set);


	/* Adjust it	*/
	
	if (!only_set && group != empty_group) {
	
		Graph	last_graph = empty_graph;
		/* The last graph we adjusted.		*/
		
		for_group (group, g) {
		
			node = g->node;
		
			if (adjust_node_label_text_to_draw)
				adjust_nodelabel_text_to_draw (node);
			
			if (adjust_nlp)
				adjust_nodelabel_position (node);
			
			if (adjust_arrow || adjust_edge || adjust_elp || adjust_edge_label_text_to_draw) {
				for_edge_sourcelist (node, edge) {
					if (contains_group_node (group, edge->target)) {
						if (adjust_arrow)
							adjust_arrow_to_edge (edge);
						if (adjust_edge_label_text_to_draw)
							adjust_edgelabel_text_to_draw (edge);
						if (adjust_elp)
							adjust_edgelabel_position (edge);
						if (adjust_edge)
							adjust_edge_box (edge);
					}
				} end_for_edge_sourcelist (node, edge);
			}
			
			if (adjust_edges) {
				/* adjust_arrow and adjust_edges should not be set here	*/
				adjust_all_edges (node);
			}

			if (adjust_graph && node->graph != last_graph && last_graph != empty_graph)
				/* May adjust the same graph more than once,	*/
				/* but it is unlikely that the code to prevent	*/
				/* that would make it all faster.		*/
				adjust_graph_box (node->graph);
		
			set_graph_has_changed (node->graph);
	
			last_graph = node->graph;

		} end_for_group (group, g);
		
		if (adjust_graph)	/* The last graph in the group is not adjusted	*/
					/* in the above code				*/
			adjust_graph_box (node->graph);
	}

	
	/* mark again && redraw	*/
	if (!only_set) {
		draw_group (group);
	}
	
	
	va_end (args);
}



/*									*/
/* Since moving groups is very essential, it is treated in a special,	*/
/* *very* fast procedure						*/
/*									*/


static	void	do_move_group (group, dx,dy, only_set)
Group		group;
int		dx,dy;
int		only_set;
{
	Group		g;
	Node		node;
	Edge		edge;
	Edgeline	el;
	
	for_group (group, g) {
	
		node = g->node;
		
		rect_passtoparent (dx,dy, &(node->box));
		node->x += dx;
		node->y += dy;
		
		rect_passtoparent (dx,dy, &(node->label.box));
		node->label.x += dx;
		node->label.y += dy;
		
		node->full_box = rect_bounding (&(node->box), &(node->label.box));
		
		for_edge_sourcelist (node, edge) {
			if (contains_group_node (group, edge->target)) {
				for_edgeline (edge->line, el) {
					el->x += dx; el->y += dy;
					rect_passtoparent (dx,dy, &(el->box));
				} end_for_edgeline (edge->line, el);
				rect_passtoparent (dx,dy, &(edge->label.box));
				edge->label.x += dx; edge->label.y += dy;
				edge->arrow.x0 +=dx; edge->arrow.y0 +=dy;
				edge->arrow.x1 +=dx; edge->arrow.y1 +=dy;
				edge->arrow.x2 +=dx; edge->arrow.y2 +=dy;
				rect_passtoparent (dx,dy, &(edge->arrow.box));
				if (!only_set) {
					adjust_edge_box (edge);
				}
			} else {
				if (node->node_edge_interface == NO_NODE_EDGE_INTERFACE) {
					el = edge->line;
					set_edgeline_xy (el, el->x+dx, el->y+dy);
				}
				if (!only_set) {
					adjust_edge_head (edge);
					adjust_edge_box  (edge);
				}
		   	 }
		} end_for_edge_sourcelist (node, edge);
		
		for_edge_targetlist (node, edge) {
			if (contains_group_node (group, edge->source)) {
				/* Dann ist nichts zu tun, da diese Kante in	*/
				/* der sourcelist von edge->source auftritt.	*/
			} else {
				if (node->node_edge_interface == NO_NODE_EDGE_INTERFACE) {
					el = edge->line->pre;
					set_edgeline_xy (el, el->x+dx, el->y+dy);
				}
				if (!only_set) {
					adjust_edge_tail (edge);
					adjust_edge_box  (edge);
				}
			}
		} end_for_edge_targetlist (node, edge);
		
	} end_for_group (group, g);
}
