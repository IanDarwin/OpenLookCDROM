/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

#ifndef SGRAPH_HEADER
#define SGRAPH_HEADER

#ifndef SGRAPH_STANDALONE
#define GRAPHED_POINTERS
#define SGRAGRA_POINTERS
#endif


typedef struct	sgraph {
	struct	snode	*nodes;
	char		*label;
	int		directed;
	Attributes	attrs;
#ifdef GRAPHED_POINTERS
	char		*graphed;
#endif

	void		(*make_node_proc)();
	void		(*make_edge_proc)();
	void		(*remove_node_proc)();
	void		(*remove_edge_proc)();
	void		(*remove_graph_proc)();
}
	*Sgraph;

#define	first_node_in_graph(g)	((g)->nodes)
#define	last_node_in_graph(g)	((g)->nodes->pre)
#ifndef GRAPHED
#define	empty_graph		((Sgraph)NULL)
#endif
#define	empty_sgraph	((Sgraph)NULL)

extern	Sgraph	make_graph     ();
extern	void	remove_graph   ();
extern	void	set_graphlabel ();
extern	void	print_graph    ();
extern	Sgraph	load_graph     ();



/************************************************************************/
/*									*/
/*				SNODE					*/
/*									*/
/************************************************************************/


typedef	struct	snode {
	struct	snode	*pre,   *suc;
	struct	sedge	*slist, *tlist;
	struct	sgraph	*graph;
	char		*label;
	int		nr;
	int		x,y;
	struct	snode	*iso;
	Attributes	attrs;
#ifdef GRAPHED_POINTERS
	char		*graphed;
#endif
#ifdef SGRAGRA_POINTERS
	char		*embedding;
#endif
}
	*Snode;

#ifndef GRAPHED
#define	empty_node	((Snode)NULL)
#endif
#define	empty_snode	((Snode)NULL)

#define snode_x(n) ((n)->x)
#define snode_y(n) ((n)->y)

#define	for_all_nodes(graph, node) \
	{ if (((node) = (graph)->nodes) != (Snode)NULL) do {
#define	end_for_all_nodes(graph, node) \
	} while (((node) = (node)->suc) != (graph)->nodes); }

extern	Snode	make_node             ();
extern	Snode	make_node_with_number ();
extern	void	remove_node           ();
extern	void	set_nodelabel         ();



/************************************************************************/
/*									*/
/*				SEDGE					*/
/*									*/
/************************************************************************/


typedef	struct	sedge {
	struct	sedge	*spre,  *ssuc,
			*tpre,  *tsuc;
	struct	snode	*snode, *tnode;
	char		*label;
	Attributes	attrs;
#ifdef GRAPHED_POINTERS
	char		*graphed;
#endif
}
	*Sedge;


#ifndef GRAPHED
#define	empty_edge	((Sedge)NULL)
#endif
#define empty_sedge	((Sedge)NULL)

extern	Sedge	make_edge     ();
extern	void	remove_edge   ();
extern	void	set_edgelabel ();

#define	for_sourcelist(node, edge)		\
	{ if (((edge) = (node)->slist) != (Sedge)NULL) do {
#define	end_for_sourcelist(node, edge)	\
	} while (((edge) = (edge)->ssuc) != (node)->slist); }
#define	for_targetlist(node, edge)		\
	{ if (((edge) = (node)->tlist) != (Sedge)NULL) do {
#define	end_for_targetlist(node, edge)	\
	} while (((edge) = (edge)->tsuc) != (node)->tlist); }

#define unique_edge(e) ((e) < (e)->tsuc)

#endif
