/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				state.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	In diesem Modul befinden Sicht die Verwaltungsfunktionen	*/
/*	fuer								*/
/*	- die Datenstruktur graph (allgemein) sowie			*/
/*	- fuer die globale Variable graph_state, die die aktuellen	*/
/*	  Defaultwerte fuer die Attribute von Knoten und Kanten haelt	*/
/*	  und								*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "install.h"
#include "graphed_subwindows.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	init_graph_state ()					*/
/*									*/
/*	void	set_current_node_edge_interface  (node_edge_interface)	*/
/*	void	set_current_nodelabel_placement  (nodelabel_placement)	*/
/*	void	set_current_arrowlength          (length)		*/
/*	void	set_current_arrowangle           (angle)		*/
/*	void	set_current_nodesize             (x,y)			*/
/*	void	set_current_edgelabelsize        (x,y)			*/
/*	void	set_current_nodelabel_visibility (visibility)		*/
/*	void	set_current_edgelabel_visibility (visibility)		*/
/*									*/
/*	void	init_graphed_state ()					*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Graph_state	graph_state;					*/
/*	Graphed_state	graphed_state;					*/
/*									*/
/************************************************************************/


Graph_state	graph_state;	/* Aktuelle Defaultwerte der Attribute	*/
				/* von Knoten und Kanten etc.		*/

Graphed_state	graphed_state;	/* Aktuelle GraphEd Defaults		*/


/************************************************************************/
/*									*/
/*			GLOBALE PROZEDUREN				*/
/*									*/
/************************************************************************/


void	init_graph_state   ();
void	init_graphed_state ();



/************************************************************************/
/*									*/
/*			GRAPH_STATE VERWALTEN				*/
/*									*/
/*	Die folgenden Prozeduren manipulieren den in graph_state	*/
/*	festgehaltene aktuellen Zustand der Attribute von Knoten	*/
/*	und Kanten.							*/
/*									*/
/************************************************************************/
/*									*/
/*======================================================================*/
/*									*/
/*	void	init_graph_state ()					*/
/*									*/
/*	Initialisiert graph_state uber die nachfolgenden Prozeduren.	*/
/*	Wie init_graphs, tritt diese Prozedur nur beim Hochfahren des	*/
/*	Grapheneditors in Aktion.					*/
/*	ACHTUNG : Im Normalfall werden diese Attribute beim Hochfahren	*/
/*	aus einer Datei (GRAPHED_INITIALISATION_FILE) eingelesen.	*/
/*	init_graph_state wird hier nur der Vollstaendigkeit wegen	*/
/*	aufgerufen, da Inkonsistenzen auftreten koennen, falls gewisse	*/
/*	Attribute in der Datei nicht vertreten sind (z.B. weil neue	*/
/*	Attribute mit abgespeichert werden, die Datei aber noch aus	*/
/*	einer aelteren Version stammt).					*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_current_node_edge_interface  (node_edge_interface)	*/
/*	void	set_current_nodelabel_placement  (nodelabel_placement)	*/
/*	void	set_current_arrowlength          (length)		*/
/*	void	set_current_arrowangle           (angle)		*/
/*	void	set_current_nodesize             (width, height)	*/
/*	void	set_current_edgelabelsize        (width, height)	*/
/*	void	set_current_nodelabel_visibility (visible)		*/
/*	void	set_current_edgelabel_visibility (visible)		*/
/*	void	set_current_nodecolor            (color)		*/
/*	void	set_current_edgecolor            (color)		*/
/*	void	set_current_directedness         (directed)		*/
/*									*/
/*	void	set_current_gragra_type		(type)			*/
/*	void	set_current_gragra_terminals	(terminals)		*/
/*	void	set_current_gragra_nonterminals	(nonterminals)		*/
/*	void	set_gragra_always_match_empty	(boolean)		*/
/*									*/
/*	Alle diese Prozeduren installieren die gesetzten Parameter	*/
/*	selbstaendig in Menues etc.					*/
/*	Eine Besonderheit weist set_current_edgelabelsize auf : diese	*/
/*	Prozedur aendert direkt alle Kantenlabel, waehrend die anderen	*/
/*	Prozeduren nichts am Graphen aendern.				*/
/*									*/
/************************************************************************/



void	init_graph_state ()
{
	int	i;
	
	graph_state.node = (Node_attributes *)mycalloc (NUMBER_OF_NODE_STYLES, sizeof (Node_attributes));
	graph_state.edge = (Edge_attributes *)mycalloc (NUMBER_OF_EDGE_STYLES, sizeof (Edge_attributes));
	
	set_current_node_edge_interface (TO_BORDER_OF_BOUNDING_BOX);
	set_current_nodelabel_placement (NODELABEL_MIDDLE);
	set_current_nodesize (DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT);
	set_current_edgelabelsize (256, 64);
	set_current_nodetype (0);
	set_current_edgetype (0);
	set_current_nodefont (0);
	set_current_edgefont (0);
	set_current_arrowlength (8);
	set_current_arrowangle  (M_PI_4);
	set_current_nodelabel_visibility (TRUE);
	set_current_edgelabel_visibility (TRUE);
	set_current_edgecolor (3);
	set_current_nodecolor (1);
	set_current_directedness (TRUE);
	
	set_current_gragra_type (ENCE_1);
	graph_state.gragra_terminals    = NULL;
	graph_state.gragra_nonterminals = NULL;
	set_current_gragra_terminals (strsave("[a-z_$]*"));
	set_current_gragra_nonterminals (strsave("[A-Z][a-zA-Z_$]*"));
	set_gragra_always_match_empty (TRUE);
	
	set_always_pretty_print_productions (FALSE);
	set_global_embedding_name           (strsave("embedding"));
	set_embed_match_attributes          (0);
	
	for (i=0; i<NUMBER_OF_NODE_STYLES; i++) {
		if (i != NORMAL_NODE_STYLE)
			graph_state.node[i] = graph_state.node[NORMAL_NODE_STYLE];
	}
	for (i=0; i<NUMBER_OF_EDGE_STYLES; i++) {
		if (i != NORMAL_EDGE_STYLE)
			graph_state.edge[i] = graph_state.edge[NORMAL_EDGE_STYLE];
	}
}



void			set_current_node_edge_interface (node_edge_interface)
Node_edge_interface	node_edge_interface;
{
	graph_state.node[NORMAL_NODE_STYLE].node_edge_interface = node_edge_interface;
	install_node_edge_interface_in_menu (node_edge_interface);
}


void			set_current_nodelabel_placement (nodelabel_placement)
Nodelabel_placement	nodelabel_placement;
{
	graph_state.node[NORMAL_NODE_STYLE].nodelabel_placement = nodelabel_placement;
	install_nodelabel_placement_in_menu (nodelabel_placement);
}


void	set_current_arrowlength (length)
int	length;
{
	graph_state.edge[NORMAL_EDGE_STYLE].arrow_length = length;
	install_arrowlength_in_menu (length);
}


void	set_current_arrowangle (angle)
float	angle;
{
	graph_state.edge[NORMAL_EDGE_STYLE].arrow_angle = angle;
	install_arrowangle_in_menu (angle);
}


void	set_current_nodesize (width, height)
int	width, height;
{
	if (width < 1)  width  = 1;
	if (height < 1) height = 1;
	graph_state.node[NORMAL_NODE_STYLE].width  = width;
	graph_state.node[NORMAL_NODE_STYLE].height = height;
	install_nodesize_in_menu (width, height);
}


void	set_current_edgelabelsize (width, height)
int	width, height;
{
	Node	node;
	Edge	edge;
	Graph	graph;
	
	graph_state.max_edgelabel_width  = width;
	graph_state.max_edgelabel_height = height;
	install_edgelabelsize_in_menu (width, height);
	
	for_all_graphs (wac_buffer, graph)
	    for_nodes (graph, node)
		for_edge_sourcelist (node, edge)
			adjust_edgelabel_text_to_draw (edge);
			adjust_edgelabel_position     (edge);
			adjust_edge_box (edge);
		end_for_edge_sourcelist (node, edge);
	    end_for_nodes (graph, node);
	    adjust_graph_box (graph);
	    set_graph_has_changed (graph);
	end_for_all_graphs (wac_buffer, graph);
	
	redraw_all ();
}


void	set_current_nodelabel_visibility (visible)
int	visible;
{
	graph_state.node[NORMAL_NODE_STYLE].label_visibility = visible;
	install_nodelabel_visibility_in_menu (visible);
}


void	set_current_edgelabel_visibility (visible)
int	visible;
{
	graph_state.edge[NORMAL_EDGE_STYLE].label_visibility = visible;
	install_edgelabel_visibility_in_menu (visible);
}


void	set_current_nodecolor (color)
int	color;
{
	graph_state.node[NORMAL_NODE_STYLE].color = color;
	install_current_nodecolor_in_nodecolor_selection ();
}


void	set_current_edgecolor (color)
int	color;
{
	graph_state.edge[NORMAL_EDGE_STYLE].color = color;
	install_current_edgecolor_in_edgecolor_selection ();
}


void	set_current_nodefont (index)
int	index;
{
	graph_state.node[NORMAL_NODE_STYLE].font_index = index;
	install_current_nodefont ();
}


void	set_current_edgefont (index)
int	index;
{
	graph_state.edge[NORMAL_EDGE_STYLE].font_index = index;
	install_current_edgefont ();
}

void	set_current_nodetype (index)
int	index;
{
	graph_state.node[NORMAL_NODE_STYLE].type_index = index;
	install_current_nodetype ();
}


void	set_current_edgetype (index)
int	index;
{
	graph_state.edge[NORMAL_EDGE_STYLE].type_index = index;
	install_current_edgetype ();
}


void	set_current_directedness (directed)
int	directed;
{
	graph_state.directedness = directed;
	install_directedness_in_menu (directed);
}


void		set_current_gragra_type (type)
Gragra_type	type;
{
	graph_state.gragra_type = type;
	install_gragra_type_in_menu (type);
}


void	set_current_gragra_terminals (terminals)
char	*terminals;
{
	if (graph_state.gragra_terminals != NULL)
		myfree(graph_state.gragra_terminals);
	graph_state.gragra_terminals = terminals;
}


void	set_current_gragra_nonterminals (nonterminals)
char	*nonterminals;
{
	if (graph_state.gragra_nonterminals != NULL)
		myfree(graph_state.gragra_nonterminals);
	graph_state.gragra_nonterminals = nonterminals;
}


void	set_gragra_always_match_empty (boolean)
int	boolean;
{
	graph_state.gragra_always_match_empty = boolean;
}


void	set_always_pretty_print_productions (boolean)
int	boolean;
{
	graph_state.always_pretty_print_productions = boolean;
}


void	set_global_embedding_name (string)
char	*string;
{
	graph_state.global_embedding_name = string;
}


void		set_embed_match_attributes (set)
unsigned	set;
{
	graph_state.embed_match_attributes = set;
}
/************************************************************************/
/*									*/
/*	... und dasselbe nochmal mit get_...				*/
/*									*/
/************************************************************************/


Node_edge_interface	get_current_node_edge_interface ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].node_edge_interface;
}

Nodelabel_placement	get_current_nodelabel_placement ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].nodelabel_placement;
}


float	get_current_arrow_angle ()
{
	return	graph_state.edge[NORMAL_EDGE_STYLE].arrow_angle;
}

int	get_current_arrow_length ()
{
	return	graph_state.edge[NORMAL_EDGE_STYLE].arrow_length;
}


int	get_current_node_width ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].width;
}

int	get_current_node_height ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].height;
}


int	get_current_nodefont_index ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].font_index;
}

int	get_current_edgefont_index ()
{
	return	graph_state.edge[NORMAL_EDGE_STYLE].font_index;
}


int	get_current_nodetype_index ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].type_index;
}

int	get_current_edgetype_index ()
{
	return	graph_state.edge[NORMAL_EDGE_STYLE].type_index;
}


int	get_current_edgelabel_width ()
{
	return	graph_state.max_edgelabel_width;
}

int	get_current_edgelabel_height ()
{
	return	graph_state.max_edgelabel_height;
}


int	get_current_nodelabel_visibility ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].label_visibility;
}

int	get_current_edgelabel_visibility ()
{
	return	graph_state.edge[NORMAL_EDGE_STYLE].label_visibility;
}


int	get_current_nodecolor ()
{
	return	graph_state.node[NORMAL_NODE_STYLE].color;
}

int	get_current_edgecolor ()
{
	return	graph_state.edge[NORMAL_EDGE_STYLE].color;
}

int	get_current_directedness ()
{
	return	graph_state.directedness;
}


Gragra_type	get_current_gragra_type ()
{
	return	graph_state.gragra_type;
}


char	*get_current_gragra_terminals ()
{
	return	graph_state.gragra_terminals;
}

char	*get_current_gragra_nonterminals ()
{
	return	graph_state.gragra_nonterminals;
}


int	get_gragra_always_match_empty ()
{
	return	graph_state.gragra_always_match_empty;
}

/************************************************************************/
/*									*/
/*			Node/Edge Attributes				*/
/*									*/
/************************************************************************/


void		set_node_style (i, attr)
int		i;
Node_attributes	attr;
{	
	graph_state.node[i] = attr;
}


void		set_edge_style (i, attr)
int		i;
Edge_attributes	attr;
{	
	graph_state.edge[i] = attr;
}



Node_attributes	get_node_style (i)
int		i;
{
	return	graph_state.node[i];
}


Edge_attributes	get_edge_style (i)
int		i;
{
	return	graph_state.edge[i];
}


int	get_always_pretty_print_produtions ()
{
	return	graph_state.always_pretty_print_productions;
}


char	*get_global_embedding_name ()
{
	return	graph_state.global_embedding_name;
}


unsigned	get_embed_match_attributes ()
{
	return	graph_state.embed_match_attributes;
}

/************************************************************************/
/*									*/
/*				GRAPHED_STATE				*/
/*									*/
/*									*/
/************************************************************************/

void	init_graphed_state ()
{
	graphed_state.shutdown = FALSE;
	graphed_state.startup  = FALSE;
	graphed_state.loading  = FALSE;
	
	graphed_state.default_working_area_window_width  = DEFAULT_WORKING_AREA_WINDOW_WIDTH;
	graphed_state.default_working_area_window_height = DEFAULT_WORKING_AREA_WINDOW_HEIGHT;
	graphed_state.default_working_area_canvas_width  = DEFAULT_WORKING_AREA_CANVAS_WIDTH;
	graphed_state.default_working_area_canvas_height = DEFAULT_WORKING_AREA_CANVAS_HEIGHT;
	
	graphed_state.colorscreen = FALSE;
}
