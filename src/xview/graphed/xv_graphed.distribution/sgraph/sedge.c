/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

/************************************************************************/
/*									*/
/*				stdedge.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	STANDARDIZED DATA STRUCTURE FOR EDGES IN A GRAPH		*/
/*									*/
/*	Further Information : N.A.					*/
/*									*/
/************************************************************************/

#include "std.h"
#include "sgraph.h"


Local	Sedge	_make_edge   ();
Local	void	_remove_edge ();


Global	Sedge	make_edge (snode, tnode, attrs)
Snode		snode;
Snode		tnode;
Attributes	attrs;
{
	return 	_make_edge (snode, tnode, attrs, FALSE);
}



Local	Sedge	_make_edge (snode, tnode, attrs, is_a_double)
Snode		snode;
Snode		tnode;
Attributes	attrs;
int		is_a_double;
{
	Sedge	new_edge;
	
	new_edge = (Sedge)mymalloc (sizeof(struct sedge));
	new_edge->spre  = empty_edge;
	new_edge->ssuc  = empty_edge;
	new_edge->tpre  = empty_edge;
	new_edge->tsuc  = empty_edge;
	new_edge->snode = snode;
	new_edge->tnode = tnode;
	new_edge->label = nil;
	new_edge->attrs = attrs;
#ifdef GRAPHED_POINTERS
	new_edge->graphed = NULL;
#endif

	if (snode->slist == empty_edge) {
		snode->slist = new_edge;
		new_edge->spre = new_edge;
		new_edge->ssuc = new_edge;
	} else {
		new_edge->spre = snode->slist->spre;
		new_edge->ssuc = snode->slist;
		new_edge->spre->ssuc = new_edge;
		new_edge->ssuc->spre = new_edge;
	}
	
	if (snode->graph->directed) {
	
		/* directed_graph --- use tsuc, ssuc to link all the	*/
		/* nodes having the same target				*/
		
		if (tnode->tlist == empty_edge) {
			tnode->tlist = new_edge;
			new_edge->tpre = new_edge;
			new_edge->tsuc = new_edge;
		} else {
			new_edge->tpre = tnode->tlist->tpre;
			new_edge->tsuc = tnode->tlist;
			new_edge->tpre->tsuc = new_edge;
			new_edge->tsuc->tpre = new_edge;
		}
		
	} else /* undirected */ if (!is_a_double) {
	
		/* undirected_graph --- use tsuc, tpre as links to the	*/
		/* corresponding edge target -> source			*/
		
		Sedge	new_edges_double;

		new_edges_double = _make_edge (tnode, snode,
		                               attrs, TRUE);
		new_edge->tpre         = new_edges_double;
		new_edge->tsuc         = new_edges_double;
		new_edges_double->tpre = new_edge;
		new_edges_double->tsuc = new_edge;
	}
	
        if (new_edge->snode->graph->make_edge_proc != NULL) {
		new_edge->snode->graph->make_edge_proc (new_edge);
	}

	return	new_edge;
}



Global	void	remove_edge (edge)
Sedge		edge;
{
	_remove_edge (edge, FALSE);
}



Local	void	_remove_edge (edge, is_a_double)
Sedge		edge;
int		is_a_double;
{
	if (edge->ssuc == edge)
		edge->snode->slist = empty_edge;
	else {
		edge->spre->ssuc = edge->ssuc;
		edge->ssuc->spre = edge->spre;
		if (edge->snode->slist == edge)
			edge->snode->slist = edge->ssuc;
	}
	
	if (edge->snode->graph->directed) {
		if (edge->tsuc == edge)
			edge->tnode->tlist = empty_edge;
		else {
			edge->tpre->tsuc = edge->tsuc;
			edge->tsuc->tpre = edge->tpre;
			if (edge->tnode->tlist == edge)
				edge->tnode->tlist = edge->tsuc;
		}
	} else /* undirected */ if (!is_a_double) {
		_remove_edge (edge->tsuc, TRUE);
	}
	
        if (edge->snode->graph->remove_edge_proc != NULL) {
		edge->snode->graph->make_edge_proc (edge);
	}

	myfree (edge);
}


Global	void	set_edgelabel (edge, text)
Sedge		edge;
char		*text;
{
	edge->label = text;
	if (!edge->snode->graph->directed)
		edge->tsuc->label = text;
}


Global	void	set_edgeattrs (edge, attrs)
Sedge		edge;
Attributes	attrs;
{
	edge->attrs = attrs;
	if (!edge->snode->graph->directed)
		edge->tsuc->attrs = attrs;
}
