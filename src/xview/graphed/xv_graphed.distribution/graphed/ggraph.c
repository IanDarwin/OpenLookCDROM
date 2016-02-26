/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				ggraph.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	In diesem Modul befinden sich die Verwaltungsfunktionen		*/
/*	fuer								*/
/*	- die Datenstruktur graph					*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "user.h"
#include "group.h"

#include "graphed_subwindows.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	init_graphs ()						*/
/*									*/
/*	Graph	create_graph (buffer)					*/
/*	Graph	create_production (buffer)				*/
/*	void	delete_graph (graph)					*/
/*									*/
/*	Graph	copy_graph          (buffer, graph)			*/
/*	Group	copy_graph_to_graph (to_graph, from_graph)		*/
/*									*/
/*	void	set_graph_directedness (graph, directed)		*/
/*	void	erase_and_delete_graph (graph)				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_graph_has_changed    (graph)			*/
/*	void	reset_graph_has_changed  (graph)			*/
/*	void	reset_buffer_has_changed (buffer)			*/
/*	int	graph_has_changed        (graph)			*/
/*	int	any_graph_has_changed    ()				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Rect	compute_rect_around_graph  (graph)			*/
/*	Rect	compute_rect_around_graphs (graph)			*/
/*	int	graph_is_empty             (graph)			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	create_graph_attributes (graph, create_graph_attr_proc,	*/
/*		                                create_node_attr_proc,	*/
/*		                                create_edge_attr_proc)	*/
/*	void	push_graph_attributes   (graph)				*/
/*	void	pop_graph_attributes    (graph, destroy_graph_attr_proc,*/
/*		                                destroy_node_attr_proc,	*/
/*		                                destroy_edge_attr_proc)	*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			LOKALE PROZEDUREN				*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GRAPH VERWALTEN					*/
/*									*/
/*	Die hier implementierten Prozeduren und Funktionen betreffen	*/
/*	nur den gesamten Graphen bzw. den Dateinamen des Graphen.	*/
/*	Fuer Knoten oder Kanten siehe die Module nnode.c und eedge.c.	*/
/*									*/
/************************************************************************/
/*									*/
/*	Die Datenstruktur "Graph" hat folgende Gestalt :		*/
/*									*/
/*	typedef struct graph {						*/
/*									*/
/*		Node		firstnode;				*/
/*		struct graph	*pre, *suc;				*/
/*									*/
/*		char		*label;					*/
/*									*/
/*		Rect		box;					*/
/*		int		changed;				*/
/*		int		change_time;				*/
/*									*/
/*		int		buffer;					*/
/*		int		directed;				*/
/*									*/
/*		Node		iso;					*/
/*									*/
/*		int		is_production;				*/
/*		int		compile_time;				*/
/*		Gragra_prod	gra;					*/
/*									*/
/*		char		*attr;					*/
/*		Attr_stack	attr_stack;				*/
/*	}								*/
/*		*Graph;							*/
/*									*/
/*	firstnode	Zeiger auf den ersten Knoten im Graphen		*/
/*	pre,suc		Zeiger auf den Vorgaenger/Nachfolgergraphen.	*/
/*			Graphen sind in Buffern in doppelt verketteten	*/
/*			Listen organisiert.				*/
/*									*/
/*	label		Label des Graphen.				*/
/*									*/
/*	box		Den Knoten umschreibendes Rechteck. Wird zum	*/
/*			Zeichnen benoetigt.				*/
/*	changed		Flag, ob der Graph veraendert wurde		*/
/*	change_time	Zeit der letzten Aenderung am Graphen.		*/
/*									*/
/*	buffer		Der Buffer, in dem der Graph steht		*/
/*	directed	Flag, ob der Graph gerichtret ist		*/
/*									*/
/*	iso		Hilfzeiger auf isomorphen Knoten (bei		*/
/*			Kopieroperationen				*/
/*									*/
/*	is_production	Flag, ob der Graph eine GraGra ist		*/
/*	compile_time	Letzte Compilation der GraGra			*/
/*	gra		Die Graph-Grammatik				*/
/*									*/
/*	attr		Benutzerdefinierte Attribute			*/
/*	attr_stack	Stack von Benutzerdefinierten Attributen	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Makros :							*/
/*									*/
/*	empty_graph	NULL-Zeiger fuer Graphen			*/
/*									*/
/*	for_all_graphs (buffer, graph)					*/
/*		...							*/
/*	end_for_all_graphs (buffer, graph)				*/
/*									*/
/*	is_left_side_of_production (node)				*/
/*	Ist node eine Linke Seite einer GraGra - Produktion ?		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	init_graphs ()						*/
/*									*/
/*	Tritt nur beim Systemstart in Aktion.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Graph	create_graph (buffer)					*/
/*									*/
/*	Erzeugt einen neuen Graphen innerhalb buffer.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	delete_graph (graph)					*/
/*									*/
/*	Loescht graph aus seinem buffer.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Graph	copy_graph (buffer, graph)				*/
/*									*/
/*	Erstellt in buffer eine Kopie von Graph.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Group	copy_graph_to_graph (to_graph, from_graph)		*/
/*									*/
/*	Fuegt eine Kopie von from_graph in to_graph ein. Rueckgabe	*/
/*	ist eine Group aus den kopierten Knoten.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	 set_graph_directedness (graph, directed)		*/
/*									*/
/*	Setzt das directed-Flag in graph. Sonst tut sich an der		*/
/*	Struktur nichts !						*/
/*	Um einen bestehenden Graphen umzuwandeln, muss explizit		*/
/*	graph_set (graph, RESTORE_IT, 0) aufgerufen werden.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	 set_graph_label (graph, label)				*/
/*									*/
/*	Setzt graph->label. Es wird angenommen, dass label mit mymalloc	*/
/*	angelegt wird; der bisherige Eintrag in graph->label wird	*/
/*	mit myfree freigegeben.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	erase_and_delete_graph (graph)				*/
/*									*/
/*	Loescht den ganzen Graphen, physikalisch und zeichnerisch.	*/
/*									*/
/*======================================================================*/
/*									*/
/*	Graph	create_production (buffer)				*/
/*									*/
/*	Erzeugt eine GraGra - Produktion. Analog zu create_graph.	*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	graph_set (graph, ..., 0)				*/
/*									*/
/*	Pendant zu group_set.						*/
/*									*/
/************************************************************************/



void	init_graphs ()
{
}


Graph	create_graph (buffer)
int	buffer;
{
	Graph		new_graph;
	static	int	highest_nr_up_to_now = 0;
	
	new_graph = (Graph)mymalloc (sizeof(struct graph));
	
	new_graph->label       = NULL;
	new_graph->firstnode   = empty_node;
	new_graph->changed     = FALSE;
	new_graph->change_time = ticks ();
	new_graph->buffer      = buffer;
	new_graph->nr          = ++highest_nr_up_to_now;
	rect_construct(&new_graph->box,0,0,0,0);
	
	new_graph->directed   = TRUE;
	
	new_graph->is_production = FALSE;
	new_graph->compile_time  = 0;
	new_graph->gra.type           = ENCE_1;
	new_graph->gra.gra.nce1.right_side = empty_group;
	new_graph->gra.gra.nce1.left_side  = empty_group;
	new_graph->gra.gra.nce1.embed_in   = empty_embedding;
	new_graph->gra.gra.nce1.embed_out  = empty_embedding;
	
	if (buffers[buffer].graphs == empty_graph) {
		buffers[buffer].graphs = new_graph;
		new_graph->pre = new_graph;
		new_graph->suc = new_graph;
	} else {
		new_graph->suc = buffers[buffer].graphs;
		new_graph->pre = buffers[buffer].graphs->pre;
		new_graph->pre->suc = new_graph;
		new_graph->suc->pre = new_graph;
	}
	
	new_graph->attr         = NULL;
	new_graph->attr_stack   = NULL;
	new_graph->sgraph_graph = NULL;
	
	return	new_graph;
}


void	delete_graph (graph)
Graph	graph;
{
	delete_attatched_sgraph (graph);
	
	while (graph->firstnode != empty_node)  /* solange es Knoten gibt ... */
		delete_node (graph->firstnode); /* loesche sie !              */
	
	
	if (graph->suc == graph) {
		buffers[graph->buffer].graphs = empty_graph;
	} else {
		graph->pre->suc = graph->suc;
		graph->suc->pre = graph->pre;
		if (graph == buffers[graph->buffer].graphs)
			buffers[graph->buffer].graphs = graph->suc;
	}
	
	if (graph->is_production) {
		free_group (graph->gra.gra.nce1.left_side);
		free_group (graph->gra.gra.nce1.right_side);
		free_embedding (graph->gra.gra.nce1.embed_in);
		free_embedding (graph->gra.gra.nce1.embed_out);
	}
	myfree (graph);
}


Graph	copy_graph (buffer, graph)
int	buffer;
Graph	graph;
{
	Graph	new_graph;
	Node	node;
	Edge	edge;
	
	
	new_graph = create_graph (buffer);
	new_graph->box           = graph->box;
	new_graph->directed      = graph->directed;
	new_graph->is_production = graph->is_production;
	new_graph->gra.type      = graph->gra.type;
	/* Embeddings will not be copied - new productions	*/
	/* will have to be compiled				*/
	
	for_nodes (graph, node) {
		(void) copy_node (new_graph, node);
	} end_for_nodes (graph, node);
	
	for_nodes (graph, node)
		for_edge_sourcelist (node, edge)
			(void) copy_edge (new_graph, edge);
		end_for_edge_sourcelist (node, edge)
	end_for_nodes (graph, node);
	
	return new_graph;
}



Group	copy_graph_to_graph (to_graph, from_graph)
Graph	to_graph, from_graph;
{
	Node	node, copy_of_node;
	Edge	edge;
	Group	group = empty_group;	/* in group wird die Menge der	*/
			/* Knoten, die in to_graph eingefuegt wurden, 	*/
			/* abgespeichert.				*/
	

	for_nodes (from_graph, node) {
		copy_of_node = copy_node (to_graph, node);
		group = add_to_group (group, copy_of_node);
	} end_for_nodes (from_graph, node);
	
	for_nodes (from_graph, node)
		for_edge_sourcelist (node, edge)
			(void) copy_edge (to_graph, edge);
		end_for_edge_sourcelist (node, edge)
	end_for_nodes (from_graph, node);
	
	return	group;
}



void	set_graph_directedness (graph, directed)
Graph	graph;
int	directed;
{
	graph->directed = directed;
}



void	set_graph_label (graph, label)
Graph	graph;
char	*label;
{
	if (graph->label != NULL)
		myfree (graph->label);
	graph->label = label;
}



void	erase_and_delete_graph (graph)
Graph	graph;
{
	delete_graph (graph);
	
	redraw_all ();
}



Graph	create_production (buffer)
int	buffer;
{
	Graph	new_production;
	
	new_production = create_graph (buffer);
	new_production->is_production = TRUE;
	
	return	new_production;
}


void		set_gragra_type (graph, type)
Graph		graph;
Gragra_type	type;
{
	graph->gra.type = type;
	set_graph_has_changed (graph);
}


void	graph_set (va_alist)
va_dcl
{
	va_list	args;
	Graph	graph;
	Group	group;
	
	va_start (args);
	graph = va_arg (args, Graph);
	
	group_set (group = make_group_of_graph (graph), &args);
	free_group (group);
	
	va_end (args);
}
/************************************************************************/
/*									*/
/*		AENDERUNGEN AM GRAPHEN NOTIEREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	set_graph_has_changed    (graph)			*/
/*	void	reset_graph_has_changed  (graph)			*/
/*	void	reset_buffer_has_changed (buffer)			*/
/*	int	graph_has_changed        (graph)			*/
/*	int	any_graph_has_changed    ()				*/
/*									*/
/*	set_graph_has_changed setzt graph->changed = TRUE und zeigt	*/
/*	dies im Label des base_frame an.				*/
/*	reset_graph_has_changed setzt graph->changed = FALSE und nimmt	*/
/*	die Angabe im Label des base_frame zurueck.			*/
/*	graph_has_changed gibt den momentanen Status zurueck.		*/
/*									*/
/************************************************************************/


void	set_graph_has_changed (graph)
Graph	graph;
{
	if (graph != NULL) {
	
		graph->change_time = ticks();
		
		if (!graph->changed) {
		
			graph->changed = TRUE;
			buffers[graph->buffer].changed = TRUE;
			set_canvas_frame_label (graph->buffer);
			
			if (graph->is_production) {
				if (graph->gra.gra.nce1.left_side  != empty_group) free_group (graph->gra.gra.nce1.left_side);
				if (graph->gra.gra.nce1.right_side != empty_group) free_group (graph->gra.gra.nce1.right_side);
				if (graph->gra.gra.nce1.embed_in   != empty_embedding) free_embedding (graph->gra.gra.nce1.embed_in);
				if (graph->gra.gra.nce1.embed_out  != empty_embedding) free_embedding (graph->gra.gra.nce1.embed_out);
				graph->gra.gra.nce1.left_side  = empty_group;
				graph->gra.gra.nce1.right_side = empty_group;
				graph->gra.gra.nce1.embed_in   = empty_embedding;
				graph->gra.gra.nce1.embed_out  = empty_embedding;
			}
		}
	}
	/* else ist nichts zu tun	*/
}


int	all_graphs_unchanged ()
{
	int	buffer;
	Graph	g;
	
	for (buffer = N_PASTE_BUFFERS; buffer<N_BUFFERS; buffer++) {
		for_all_graphs (buffer, g) {
			if (g->changed) {
				return FALSE;
				break;
			}
		} end_for_all_graphs (buffer, g);
	}
	
	return TRUE;
}


void	reset_graph_has_changed (graph)
Graph	graph;
{
	int	buffer,
		all_unchanged;
	Graph	g;
	
	if (graph != NULL && graph->changed) {
			
		graph->changed      = FALSE;
		graph->change_time  = 0;
		graph->compile_time = 0;
		
		buffer = graph->buffer;
		if (buffers[buffer].changed) {
			all_unchanged = TRUE;
			for_all_graphs (buffer, g) {
				if (graph->changed) {
					all_unchanged = FALSE;
					break;
				}
			} end_for_all_graphs (buffer, g);
			if (all_unchanged) {
				buffers[buffer].changed = FALSE;
				set_canvas_frame_label (buffer);
			}
		}
	}
	/* else ist nichts zu tun	*/
}


void	reset_buffer_has_changed (buffer)
int	buffer;
{
	if (buffers[buffer].changed) {
	
		Graph	g;
		
		buffers[buffer].changed = FALSE;
		
		/* Set all graphs in buffer unchanged.	*/
		/* We hope, the programmer knows what	*/
		/* he is doing.				*/
		for_all_graphs (buffer, g) {
			g->changed      = FALSE;
			g->change_time  = 0;
			g->compile_time = 0;
		} end_for_all_graphs (buffer, g);
		
		set_canvas_frame_label (buffer);
	}
	/* else ist nichts zu tun	*/
}


int	graph_has_changed (graph)
Graph	graph;
{
	return graph->changed;
}


int	any_graph_has_changed ()
{
	Graph	graph;
	int	i;
	
	for (i=N_PASTE_BUFFERS; i<N_BUFFERS; i++) {
		if (buffers[i].changed) return TRUE;
	}
	
	return FALSE;
}
/************************************************************************/
/*									*/
/*			HILFSFUNKTIONEN					*/
/*									*/
/************************************************************************/
/*									*/
/*	Rect	compute_rect_around_graph (graph)			*/
/*									*/
/*	Berechnet das den Graphen umfassende Rechteck.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Rect	compute_rect_around_graphs (buffer)			*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	graph_is_empty (graph)					*/
/*									*/
/*	Gibt eine Antwort auf die Frage, ob der Graph Knoten enthaelt.	*/
/*									*/
/************************************************************************/



Rect	compute_rect_around_graph (graph)
Graph	graph;
{
	Rect	rect;
	Node	node;
	Edge	edge;
	
	rect = rect_null;
	for_nodes (graph, node) {
		rect = rect_bounding (&rect, &(node->box));
		for_edge_sourcelist (node, edge) {
			rect = rect_bounding (&rect, &(edge->box));
		} end_for_edge_sourcelist (node, edge);
	} end_for_nodes (graph, node);
	
	return rect;
}


Rect	compute_rect_around_graphs (buffer)
int	buffer;
{
	Graph	graph;
	Rect	rect, graph_rect;
	
	rect = rect_null;
	for_all_graphs (buffer, graph) {
		graph_rect = compute_rect_around_graph (graph);
		rect = rect_bounding (&rect, &graph_rect);
	} end_for_all_graphs (buffer, graph);

	return rect;
}


int	graph_is_empty (graph)
Graph	graph;
{
	return	graph->firstnode == empty_node;
}
/************************************************************************/
/*									*/
/*									*/
/*		Algorithmenspezifische Graphsttribute verwalten		*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_graph_attributes (graph,				*/
/*			create_graph_attr_proc,				*/
/*			create_node_attr_proc,				*/
/*			create_edge_attr_proc)				*/
/*									*/
/*	void	push_graph_attributes (graph)				*/
/*									*/
/*	void	pop_graph_attributes (graph,				*/
/*			destroy_graph_attr_proc,			*/
/*			destroy_node_attr_proc,				*/
/*			destroy_edge_attr_proc,				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	char	*create_..._attr_proc (...);				*/
/*	void	destroy_..._attr_proc (...);				*/
/*									*/
/*	... = graph|node|edge						*/
/*									*/
/************************************************************************/



void	create_graph_attributes (graph, create_graph_attr_proc,
	                                create_node_attr_proc,
	                                create_edge_attr_proc)
Graph	graph;
char	*(*create_graph_attr_proc)();
char	*(*create_node_attr_proc)();
char	*(*create_edge_attr_proc)();
{
	Node	node;
	Edge	edge;
	
	graph->attr = create_graph_attr_proc (graph);
	
	for_nodes (graph, node) {
		create_node_attributes (node, create_node_attr_proc);
		for_edge_sourcelist (node, edge) {
			create_edge_attributes (edge, create_edge_attr_proc);
		} end_for_edge_sourcelist (node, edge);
	} end_for_nodes (graph, node);
}


void	push_graph_attributes   (graph)
Graph	graph;
{
	Node		node;
	Edge		edge;
	Attr_stack	new_stack;
	
	new_stack = (Attr_stack) mymalloc (sizeof(struct attr_stack));
	new_stack->attr = graph->attr;
	new_stack->next = iif (graph->attr_stack != (Attr_stack)NULL,
	                       graph->attr_stack->next,
	                       (Attr_stack)NULL);
	
	graph->attr = NULL;
	graph->attr_stack = new_stack;

	for_nodes (graph, node) {
		push_node_attributes (node);
		for_edge_sourcelist (node, edge) {
			push_edge_attributes (edge);
		} end_for_edge_sourcelist (node, edge);
	} end_for_nodes (graph, node);
}


void	pop_graph_attributes    (graph, destroy_graph_attr_proc,
	                                destroy_node_attr_proc,
	                                destroy_edge_attr_proc)
Graph	graph;
void	(*destroy_graph_attr_proc)();
void	(*destroy_node_attr_proc)();
void	(*destroy_edge_attr_proc)();
{
	Node	node;
	Edge	edge;
	
	
	destroy_graph_attr_proc (graph);
	graph->attr = NULL;
	
	if (graph->attr_stack != NULL && graph->attr_stack->attr != NULL) {
		
		Attr_stack	top = graph->attr_stack;
		
		graph->attr      = top->attr;
		graph->attr_stack =top->next;
		
		myfree (top);
	}

	for_nodes (graph, node) {
		pop_node_attributes (node, destroy_node_attr_proc);
		for_edge_sourcelist (node, edge) {
			pop_edge_attributes (edge, destroy_edge_attr_proc);
		} end_for_edge_sourcelist (node, edge);
	} end_for_nodes (graph, node);
}

