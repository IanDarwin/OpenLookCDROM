/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

/************************************************************************/
/*									*/
/*				stdraph.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	STANDARDIZED DATA STRUCTURE FOR GRAPHS				*/
/*									*/
/*	Further Information : N.A.					*/
/*									*/
/************************************************************************/

#include "std.h"
#include "sgraph.h"


Sgraph		make_graph (attrs)
Attributes	attrs;
{
	extern	char	*mymalloc();
	Sgraph	new_graph;
	
	new_graph = (Sgraph)mymalloc (sizeof(struct sgraph));

	new_graph->nodes    = empty_node;
	new_graph->label    = nil;
	new_graph->directed = false;
	new_graph->attrs    = attrs;
#ifdef GRAPHED_POINTERS
	new_graph->graphed  = NULL;
#endif

        new_graph->make_node_proc    = NULL;
        new_graph->make_edge_proc    = NULL;
        new_graph->remove_node_proc  = NULL;
        new_graph->remove_edge_proc  = NULL;
        new_graph->remove_graph_proc = NULL;

	return new_graph;
}


void	remove_graph (graph)
Sgraph	graph;
{
	while (graph->nodes != empty_node)
		remove_node (graph->nodes);

	if (graph->remove_graph_proc != NULL) {
		graph->remove_graph_proc();
	}

	myfree (graph);
}


Global	void	set_graphlabel (graph, text)
Sgraph		graph;
char		*text;
{
	graph->label = text;
}


Global	void	set_graphattrs (graph, attrs)
Sgraph		graph;
Attributes	attrs;
{
	graph->attrs = attrs;
}


Global	void	print_graph (file, g, print_graph_attributes,
                                      print_node_attributes,
                                      print_edge_attributes)
FILE		*file;
Sgraph		g;
void		(*print_graph_attributes)();
void		(*print_node_attributes)();
void		(*print_edge_attributes)();
{
	Snode	n;
	Sedge	e;
	
	fprintf (file, "GRAPH \"%s\" = %s\n",
	         g->label, iif(g->directed, "DIRECTED", "UNDIRECTED"));
	print_graph_attributes (file, g);
	for_all_nodes (g,n) {
		fprintf (file, "%d ", n->nr);
		print_node_attributes (file, n);
		fprintf (file, "\"%s\" ",  n->label);
		for_sourcelist (n,e) {
			if (g->directed ||
			    (e->snode->nr < e->tnode->nr)) {
				/* print edge				*/
				/* Caution : in undirected graphs, each*/
				/* edge is printed only once !		*/
				fprintf (file,"%d ", e->tnode->nr);
				print_edge_attributes (file, e);
				fprintf (file," \"%s\" ", e->label);
			}
		} end_for_sourcelist (n,e);
		fprintf (file, ";\n");
	} end_for_all_nodes (g,n);
	fprintf (file, "END\n");
}
