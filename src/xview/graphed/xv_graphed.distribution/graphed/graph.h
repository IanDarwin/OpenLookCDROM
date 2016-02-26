/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	GRAPH_HEADER
#define	GRAPH_HEADER
/************************************************************************/
/*									*/
/*				graph.h					*/
/*									*/
/************************************************************************/
/*									*/
/*	DATENSTRUKTUREN, MAKROS UND FUNKTIONEN FUER DEN GRAPHEN		*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*				Attr_stack				*/
/*									*/
/*	Stack von Benutzerattributen. Damit koennen sich Algorithmen	*/
/*	Aufrufen, die jeweils verschiedene Attribute an Knoten		*/
/*	und Kanten anhaengen.						*/
/*									*/
/************************************************************************/

typedef	struct 	attr_stack {

	char			*attr;
	struct	attr_stack	*next;
}
	*Attr_stack;


/************************************************************************/
/*									*/
/*			NODE_LABEL_PLACEMENT (NLP)			*/
/*									*/
/************************************************************************/


typedef	enum {
	NODELABEL_MIDDLE,
	NODELABEL_UPPERLEFT,
	NODELABEL_UPPERRIGHT,
	NODELABEL_LOWERLEFT,
	NODELABEL_LOWERRIGHT,
	
	NUMBER_OF_NODELABEL_PLACEMENTS		/* Dummy		*/
	}
	Nodelabel_placement;

#define is_legal_nodelabel_placement(i) \
	((((int)i)>=0) && (((int)i)<(int)NUMBER_OF_NODELABEL_PLACEMENTS))
	

/************************************************************************/
/*									*/
/*		KNOTEN - UND KANTENMARKIERUNGEN				*/
/*									*/
/************************************************************************/


typedef	struct
{
	char			*text;
	char			**text_to_draw;
	Graphed_font		font;
	int			x,y;		/* Position in pf_text	*/
	Rect			box;		/* Bounding box		*/
	int			visible;
	Nodelabel_placement	placement;

	int			n_lines;        /* # of visible lines	*/
	int			line_height;

}
	Nodelabel;

typedef	struct
{
	char			*text;
	char			**text_to_draw;
	Graphed_font		font;
	int			x,y;		/* Position in pf_text	*/
	Rect			box;		/* Bounding box		*/
	int			visible;

	int			n_lines;        /* # of visible lines	*/
	int			line_height;

}
	Edgelabel;


/************************************************************************/
/*									*/
/*				EDGELINE				*/
/*									*/
/************************************************************************/


typedef	struct	edgeline
{
	coord		x,y;		/* Koordinaten			*/
	struct edgeline	*pre,		/* vorheriges Stueck		*/
			*suc;		/* naehstes   Stueck		*/
	Rect		box;		/* Rechteck, in dem die		*/
					/* Edgeline (mit ->suc) liegt	*/
}
	*Edgeline;

#define edgeline_x(el)   ((el)->x)
#define edgeline_y(el)   ((el)->y)
#define edgeline_pre(el) ((el)->pre)
#define edgeline_suc(el) ((el)->suc)
#define	is_single_edgeline(el) \
	(((el) != (Edgeline)NULL) && ((el)->suc->suc == (el)))
	 
extern	Edgeline	new_edgeline         ();
extern	Edgeline	add_to_edgeline      ();
extern	Edgeline	remove_from_edgeline ();
extern	void		set_edgeline_xy      ();
extern	void		free_edgeline        ();
extern	Edgeline	copy_edgeline        ();

extern	Edgeline	temporary_edgeline;

#define	for_edgeline(el_head,el) \
	{ if (((el) = (el_head)) != (Edgeline)NULL) do {
#define	end_for_edgeline(el_head,el) \
	} while (((el) = (el)->suc) != (el_head)); }

#define	for_edgeline_reverse(el_tail,el) \
	{ if (((el) = (el_tail)) != (Edgeline)NULL) do {
#define	end_for_edgeline_reverse(el_tail,el) \
	} while (((el) = (el)->pre) != (el_tail)); }

/************************************************************************/
/*									*/
/*				ARROW					*/
/*									*/
/************************************************************************/


typedef	struct
{
	int	x0,y0, x1,y1, x2,y2;
	Rect	box;
	int	length;
	float	angle;
}
	Arrow;


/************************************************************************/
/*									*/
/*			NODE_EDGE_INTERFACE (NEI)			*/
/*									*/
/************************************************************************/

typedef	enum {
	NO_NODE_EDGE_INTERFACE,			/* "none"		*/
	TO_BORDER_OF_BOUNDING_BOX,		/* "middle"		*/
	TO_CORNER_OF_BOUNDING_BOX,		/* "corner"		*/
	CLIPPED_TO_MIDDLE_OF_NODE,		/* "clipped"		*/
	SPECIAL_NODE_EDGE_INTERFACE,		/* "special"		*/
	
	NUMBER_OF_NODE_EDGE_INTERFACES		/* Dummy		*/
	}
	Node_edge_interface;

#define is_legal_node_edge_interface(i) \
	((((int)i)>=0) && (((int)i)<(int)NUMBER_OF_NODELABEL_PLACEMENTS))



/************************************************************************/
/*									*/
/*				MARKED					*/
/*									*/
/************************************************************************/

typedef enum {
	NOT_MARKED,		/* Keine Markierung angebracht		*/
	MARKED_WITH_SQUARES,	/* Markierung mit Quadraten/Rechtecken	*/
	MARKED_AT_BOUNDARY,	/* Markierung durch fetten Rand		*/
	
	TO_BE_MARKED_WITH_SQUARES,
	TO_BE_MARKED_AT_BOUNDARY,

	NUMBER_OF_MARKINGS	/* Dummy */
}
	Marked;

#define	is_legal_marking(m) \
	((int)(m) >= (int)NOT_MARKED && (int)(m) < (int)NUMBER_OF_MARKINGS)

#define	is_not_marked(x)	 ((x)->marked == NOT_MARKED)
#define	is_square_marked(x)	 ((x)->marked == MARKED_WITH_SQUARES)
#define	is_boundary_marked(x)	 ((x)->marked == MARKED_AT_BOUNDARY)
#define	is_marked(x)		 (is_square_marked(x) || is_boundary_marked(x))
#define	to_be_square_marked(x)	 ((x)->marked == TO_BE_MARKED_WITH_SQUARES)
#define	to_be_boundary_marked(x) ((x)->marked == TO_BE_MARKED_AT_BOUNDARY)
#define	to_be_marked(x)		 (to_be_square_marked(x) || to_be_boundary_marked(x))


/************************************************************************/
/*									*/
/*				KNOTEN					*/
/*									*/
/************************************************************************/

typedef	struct	node
{
	/*-------   LOGISCHE STRUKTUR  -------				*/
	
	struct	edge	*sourcelist,	/* Adjazenzliste : alle Kanten,	*/
			*targetlist;	/* die Knoten als Source bzw.	*/
					/* Target haben			*/
	struct	node	*pre, *suc;	/* Zeiger auf vorherigen /	*/
					/* naechsten Knoten		*/
	
	struct	graph	*graph;		/* Der Graph, zu dem der Knoten	*/
					/* gehoert			*/
					
	/*-------       ATTRIBUTE      -------				*/
	
	Rect		box;		/* Bounding box			*/
	Rect		full_box;	/* Bounding box Knoten + Label	*/
	Nodetype	type;		/* Typ des Knoten		*/
	Nodetypeimage	image;		/* Bild des Knoten		*/
	Nodelabel	label;		/* Knotenmarkierung		*/
	int		nr;		/* Laufende Nummer		*/
	Marked		marked;		/* Markiert ?			*/
	Node_edge_interface node_edge_interface;
	int		color;		/* Index in der globalen	*/
					/* Farbtabelle			*/
	
	/*-------       SONSTIGES      -------				*/
	
	struct	node	*iso;		/* Kopier - Hilfszeiger		*/
	
	int		x,y;		/* position, just for		*/
					/* convenience			*/
	int		loaded;		/* Flag for the parser		*/
	
	char		*attr;
	Attr_stack	attr_stack;

	char		*sgraph_node;
}
	*Node;

#define	empty_node            ((Node)NULL)

#define node_top(node)        ((int)(node)->box.r_top)
#define node_left(node)       ((int)(node)->box.r_left)
#define node_width(node)      ((int)(node)->box.r_width)
#define node_height(node)     ((int)(node)->box.r_height)
#define node_x(node)          (node->x)
#define node_y(node)          (node->y)
#define node_sourcelist(node) ((node)->sourcelist)
#define node_targetlist(node) ((node)->targetlist)
#define nodelabel_text(node)  ((node)->label.text)
#define nodelabel_font(node)  ((node)->label.font)

extern	Node	create_node              ();
extern	Node	create_node_with_number  ();
extern	void	delete_node              ();
extern	Node	copy_node                ();

extern	void	node_set ();

extern	void	set_node_marked          ();
#define		mark_node(node)   set_node_marked ((node), MARKED_WITH_SQUARES)
#define		unmark_node(node) set_node_marked ((node), NOT_MARKED)
extern	void	set_node_to_be_marked    ();

extern	Node	find_node_with_number    ();
extern	Node	get_node_with_number     ();

extern	int	all_nodes_complete       ();

#define	for_nodes(graph, node) \
	{ if (((node) = graph->firstnode) != empty_node) do {
#define	end_for_nodes(graph, node) \
	} while (((node) = (node)->suc) != graph->firstnode); }


/*	template structure for node attributes	*/

typedef	struct	node_attributes {

	unsigned int		set;
	
	int			type_index, font_index;
	int			label_visibility;
	Node_edge_interface	node_edge_interface;
	Nodelabel_placement	nodelabel_placement;
	int			color;
	int			x,y;		/* not always needed	*/
	int			width, height;	/* not always needed	*/
	char			*label;		/* not always needed	*/
}
	Node_attributes;

extern	Node_attributes	get_node_attributes ();



/************************************************************************/
/*									*/
/*				KANTEN					*/
/*									*/
/************************************************************************/


typedef	struct	edge
{
	/*-------   LOGISCHE STRUKTUR  -------				*/
	
	struct	node	*source,	/* Quell- und Zielknoten	*/
			*target;
	struct	edge	*sourcepre,	/* doppelt verkettete Listen	*/
			*sourcesuc,	/* fuer Source- und Target-	*/
			*targetpre,	/* Adjazenzlisten		*/
			*targetsuc;
			
	/*-------       ATTRIBUTE      -------				*/
	
	int		nr;
	
	Rect		box;
	Marked		marked;		/* Markiert ?			*/
	Edgeline	line;		/* Linienzug der Kante		*/
	Edgetype	type;		/* Kantentyp			*/
	Edgelabel	label;		/* Kantenmarkierung		*/
	
	Arrow		arrow;		/* Pfeil			*/
	
	int		color;		/* Index in der globalen	*/
					/* Farbtabelle			*/
	char		*attr;
	Attr_stack	attr_stack;

	char		*sgraph_edge;
}
	*Edge;

#define	empty_edge           ((Edge)NULL)

#define edge_edgeline(edge)  ((edge)->line)
#define edgelabel_text(edge) ((edge)->label.text)
#define edgelabel_font(edge) ((edge)->label.font)

extern	Edge	create_edge              ();
extern	Edge	create_edge_with_number  ();
extern	void	delete_edge              ();
extern	Edge	copy_edge                ();
extern	Edge	copy_edge_without_line   ();

extern	void	edge_set ();

extern	void	set_edge_marked          ();
#define		mark_edge(edge)   set_edge_marked (edge, MARKED_WITH_SQUARES)
#define		unmark_edge(edge) set_edge_marked (edge, NOT_MARKED)
extern	void	set_edge_to_be_marked    ();

extern	Edge	find_edge_with_number	();
extern	Edge	get_edge_with_number	();

/* Makros zum Traversieren von Kanten	*/

#define	for_edge_sourcelist(node, edge)                                    \
	{ if (((edge) = node_sourcelist(node)) != empty_edge) do {
#define	end_for_edge_sourcelist(node, edge)                                \
	} while (((edge) = (edge)->sourcesuc) != node_sourcelist(node)); }
#define	for_edge_targetlist(node, edge)                                    \
	{ if (((edge) = node_targetlist(node)) != empty_edge) do {
#define	end_for_edge_targetlist(node, edge)                                \
	} while (((edge) = (edge)->targetsuc) != node_targetlist(node)); }


/*	template structure for edge attributes	*/

typedef	struct	edge_attributes {

	unsigned int		set;
	
	int			type_index, font_index;
	int			label_visibility;
	float			arrow_angle;
	int			arrow_length;
	int			color;
	Edgeline		line;	/* not always needed	*/
	char			*label;	/* not always needed	*/
}
	Edge_attributes;

extern	Edge_attributes	get_edge_attributes ();



/************************************************************************/
/*									*/
/*				GROUP					*/
/*									*/
/*	Weiteres siehe group.h						*/
/*									*/
/************************************************************************/



typedef	struct group {

	Node	node;
	struct	group	*pre, *suc;
}
	*Group;
	
#define	empty_group ((Group)NULL)

#define	for_group(group,g)	\
	{ if (((g) = group) != empty_group) do {
#define	end_for_group(group,g)	\
	} while (((g) = (g)->suc) != group); }
	
#define	group_is_empty(group)	((group) == empty_group)


/************************************************************************/
/*									*/
/*		Graphgrammatikspezifische Teile : eigenes Modul		*/
/*									*/
/************************************************************************/


#include "gragra.h"


/************************************************************************/
/*									*/
/*				Graph_state				*/
/*									*/
/*		Aktuelle Defaultwerte der Graphenattribute		*/
/*									*/
/************************************************************************/

#include "state.h"

/************************************************************************/
/*									*/
/*				Graph					*/
/*									*/
/************************************************************************/

typedef	struct	{
	void	(*create_node)();
	void	(*create_edge)();
	int	(*delete_node)();
	int	(*delete_edge)();
	void	(*action_node)();
	void	(*action_edge)();
}
	Graph_notifiers;


typedef struct graph
{
	Node		firstnode;
	struct graph	*pre, *suc;
	
	char		*label;
	
	Rect		box;
	int		changed;
	int		change_time;
	int		buffer;
	
	int		nr;
	
	int		directed;
	
	int		is_production;
	int		compile_time;
	Gragra_prod	gra;
	
	Graph_notifiers	functions;
	
	char		*attr;
	Attr_stack	attr_stack;

	char		*sgraph_graph;
}
	*Graph;

extern	Graph	create_graph			();
extern	void	delete_graph			();
extern	Graph	copy_graph			();
extern	Group	copy_graph_to_graph		();
extern	Graph	create_production		();

extern	void	set_graph_directedness		();
extern	void	set_graph_label			();
extern	void	graph_set			();

extern	Rect	compute_rect_around_graph	();
extern	Rect	compute_rect_around_graphs	();
extern	int	graph_is_empty			();

extern	void	set_graph_has_changed		();
extern	void	reset_graph_has_changed		();
extern	int	graph_has_changed		();


#define	empty_graph ((Graph)NULL)

#define	for_all_graphs(buffer, g) \
	{ if (((g) = (buffers[(buffer)].graphs)) != empty_graph) do {
#define	end_for_all_graphs(buffer, g) \
	} while (((g) = (g)->suc) != (buffers[(buffer)].graphs)); }

#define	is_left_side_of_production(node)	\
	((node)->graph->is_production && (node)->graph->firstnode == (node))


/************************************************************************/
/*									*/
/*				Buffer					*/
/*									*/
/************************************************************************/
/*									*/
/*	Zugehoerige Datei : buffer.c					*/
/*									*/
/************************************************************************/


typedef	struct	buffer	{
	Graph	graphs;
	int	changed;
	int	used;
	char	*filename;
}
	Buffer;

#define	N_PASTE_BUFFERS	1
#define	N_CANVASES	15
#define	N_BUFFERS	(N_PASTE_BUFFERS + N_CANVASES)

extern	Buffer	buffers [N_BUFFERS];
extern	int	wac_buffer;
extern	int	paste_buffer;

#define	buffer_is_used(b)	(buffers[(b)].used == TRUE)
#define	buffer_is_empty(b)	(graphs_of_buffer(b) == empty_graph)
#define	graphs_of_buffer(b)	(buffers[(b)].graphs)

extern	void	init_buffers  ();
extern	int	create_buffer ();
extern	void	delete_buffer ();
extern	void	delete_graphs_in_buffer ();
extern	void	unuse_buffer ();


/****************************************************************/
/*								*/
/*			    Template				*/
/*								*/
/****************************************************************/

typedef struct node_template {

	int		node_nr;
	int		graph_nr;
	Node_attributes	attributes;

}
	Node_template;


typedef struct edge_template {

	int		source_nr, target_nr;
	int		graph_nr;
	int		edge_nr;
	Edge_attributes	attributes;

}
	Edge_template;


/****************************************************************/
/*								*/
/*	Set_attribute						*/
/*								*/
/*	Knoten-, Kanten-, Group- und Graphattribute		*/
/*	fuer die Prozeduren					*/
/*		node_set					*/
/*		edge_set					*/
/*		group_set					*/
/*		graph_set					*/
/*								*/
/****************************************************************/

typedef	enum	{
	
	/* Dummy for end of list	*/
	SET_ATTRIBUTE_END = 0,
	
	/* Node attributes		*/
	NODE_POSITION = 1,
	NODE_SIZE  = NODE_POSITION << 1,
	NODE_TYPE  = NODE_SIZE     << 1,
	NODE_NEI   = NODE_TYPE     << 1,
	NODE_NLP   = NODE_NEI      << 1,
	NODE_LABEL = NODE_NLP      << 1,
	NODE_FONT  = NODE_LABEL    << 1,
	NODE_LABEL_VISIBILITY = NODE_FONT << 1,
	NODE_COLOR = NODE_LABEL_VISIBILITY << 1,
	
	/* Edge attributes		*/
	EDGE_LINE = NODE_COLOR << 1,
	EDGE_TYPE = EDGE_LINE << 1,
	EDGE_ARROW_LENGTH = EDGE_TYPE << 1,
	EDGE_ARROW_ANGLE  = EDGE_ARROW_LENGTH << 1,
	EDGE_LABEL = EDGE_ARROW_ANGLE << 1,
	EDGE_FONT  = EDGE_LABEL << 1,
	EDGE_LABEL_VISIBILITY = EDGE_FONT << 1,
	EDGE_COLOR = EDGE_LABEL_VISIBILITY << 1,
	
	/* Misc	*/
	EDGE_INSERT = EDGE_COLOR + 1,
	EDGE_DELETE = EDGE_INSERT + 1,
	
	MOVE   = EDGE_DELETE + 1,
	RESIZE = MOVE + 1,
	
	/* Specialities			*/
	ONLY_SET   = RESIZE + 1,
	RESTORE_IT = ONLY_SET + 1,
	
	/* Sgraph goodies		*/
	NODE_WIDTH  = RESTORE_IT + 1,
	NODE_HEIGHT = NODE_WIDTH + 1,
	NODE_X      = NODE_HEIGHT + 1,
	NODE_Y      = NODE_X + 1,
	
}
	Set_attribute;


/*	Macros to set values from a Node_attributes/Edge_attributes	*/
/*	structure.							*/
/*	POSITIONS and LABELS are NOT set !				*/

#define	SET_NODE_ATTRIBUTES(attr) \
	NODE_SIZE,		(attr).width, (attr).height,	\
	NODE_FONT,		(attr).font_index,		\
	NODE_TYPE,		(attr).type_index,		\
	NODE_NEI,		(attr).node_edge_interface,	\
	NODE_NLP,		(attr).nodelabel_placement,	\
	NODE_LABEL_VISIBILITY,	(attr).label_visibility,	\
	NODE_COLOR,		(attr).color

#define	SET_EDGE_ATTRIBUTES(attr) \
	EDGE_TYPE,		(attr).type_index,		\
	EDGE_FONT,		(attr).font_index,		\
	EDGE_ARROW_LENGTH,	(attr).arrow_length,		\
	EDGE_ARROW_ANGLE,	(attr).arrow_angle,		\
	EDGE_LABEL_VISIBILITY,	(attr).label_visibility,	\
	EDGE_COLOR,		(attr).color

/* 	Macros to create the default attributes.	*/

#define	DEFAULT_NODE_ATTRIBUTES \
	NODE_SIZE,		current_node_width, current_node_height,	\
	NODE_TYPE,		current_nodetype_index,				\
	NODE_NEI,		current_node_edge_interface,			\
	NODE_NLP,		current_nodelabel_placement,			\
	NODE_LABEL,		NULL,						\
	NODE_LABEL_VISIBILITY,	current_nodelabel_visibility,			\
	NODE_COLOR,		current_nodecolor

#define	DEFAULT_EDGE_ATTRIBUTES \
	EDGE_TYPE,		current_edgetype_index,		\
	EDGE_ARROW_LENGTH,	current_arrow_length,		\
	EDGE_ARROW_ANGLE,	current_arrow_angle,		\
	EDGE_LABEL,		NULL,				\
	EDGE_LABEL_VISIBILITY,	current_edgelabel_visibility,	\
	EDGE_COLOR,		current_edgecolor




/************************************************************************/
/*									*/
/*			USER_DEFINED ATTRIBUTES				*/
/*									*/
/************************************************************************/

extern	void	create_graph_attributes		();
extern	void	push_graph_attributes		();
extern	void	pop_graph_attributes		();

extern	void	create_node_attributes		();
extern	void	push_node_attributes		();
extern	void	pop_node_attributes		();

extern	void	create_edge_attributes		();
extern	void	push_edge_attributes		();
extern	void	pop_edge_attributes		();

#endif
