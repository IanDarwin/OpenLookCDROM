/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				store.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt Routinen zum Abspeichern des Graphen	*/
/*	sowie der momentanen Konfiguration der Benutzerschnittstelle	*/
/*	auf eine Datei. Das Format ist selbstverstaendlich kompatibel	*/
/*	zu den Grammatiken un scanner.l und parser.y.			*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "store.h"

#include "graphed_subwindows.h"
#include "menu.h"
#include "user.h"
#include "font.h"
#include "type.h"
#include "draw.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	store_graph  (graph, filename)				*/
/*	int	store_graphs (filename)					*/
/*	int	save_state   ()						*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			LOKALE FUNKTIONEN				*/
/*									*/
/************************************************************************/


static	void	write_graphs             ();
static	void	write_graph              ();
static	void	write_plain_graph        ();
static	void	write_graph_internals    ();
static	int	write_edge_sourcelist    ();
static	void	write_node               ();
static	void	write_edge               ();
static	int	write_node_internals     ();
static	int	write_node_attributes    ();
static	int	write_edge_internals     ();
static	int	write_edge_attributes    ();
static	int	write_node_style         ();
static	int	write_edge_style         ();
static	int	write_edgeline_internals ();
static	int	write_graph_state_list   ();
static	int	write_graphed_state_list ();


/************************************************************************/
/*									*/
/*			LOKALE TYPDEKLARATIONEN				*/
/*									*/
/************************************************************************/


typedef struct {

	enum { STORE_ONE_GRAPH, STORE_ALL_GRAPHS } what;
	
	union {
		Graph	graph;		/* Der Rest kommt spaeter	*/
	}
		which;
}
	Store_info;


/************************************************************************/
/*									*/
/*			ABSPEICHERN VON GRAPHEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	store (info, filename)					*/
/*									*/
/*	...								*/
/*	Ist filename == "", so wird auf stdout geschrieben.		*/
/*	Rueckmeldung ist TRUE, falls das Abspeichern erfolgreich	*/
/*	verlaufen ist, sonst FALSE (d.h., die Datei konnte nicht	*/
/*	geoeffnet werden).						*/
/*									*/
/*	store aktiviert bzw. deaktiviert im Menue die Punkte		*/
/*	LOAD_AGAIN und STORE_TO_SAME_FILE, falls das			*/
/*	Abspeichern erfolgreich bzw. erfolglos verlaufen ist.		*/
/*	Im Label des base_frame wird (bei erfolgreichem Speichern)	*/
/*	der neue Filename eingetragen.					*/
/*	Store oeffnet und schliesst die Datei selbststaendig.		*/
/*									*/
/************************************************************************/


int	store_graph (graph, filename)
Graph	graph;
char	*filename;
{
	Store_info	info;
	
	info.what = STORE_ONE_GRAPH;
	info.which.graph = graph;
	
	return store (info, filename);
}


int	store_graphs (filename)
char	*filename;
{
	Store_info	info;
	
	info.what = STORE_ALL_GRAPHS;
	
	return store (info, filename);
}


int		store (info, filename)
Store_info	info;
char		*filename;
{
	FILE	*file;
	Graph	graph;
	
	if (!strcmp(filename, "")) {
		message ("Storing graph to stdout\n");
		file = stdout;
	} else {
		if ((file = fopen (filename, "w")) == (FILE *)NULL) {
			error ("Can't open file %s\n", filename);
			sys_error (errno);
			set_filename ("");
			inactivate_menu_item (LOAD_AGAIN);
			inactivate_menu_item (STORE_TO_SAME_FILE);
			return FALSE;
		};
		message ("Storing graph to file %s\n", filename);
	}
	
	if (info.what == STORE_ONE_GRAPH)
		write_plain_graph (file, info.which.graph);
	else if (info.what == STORE_ALL_GRAPHS)
		write_graphs (file, wac_buffer);
	
	if (file != stdout)
		fclose (file);
	else
		fflush (file);
	
	set_filename (filename);
	activate_menu_item (LOAD_AGAIN);
	activate_menu_item (STORE_TO_SAME_FILE);
	
	if (info.what == STORE_ONE_GRAPH)
		reset_graph_has_changed (info.which.graph);
	else if (info.what == STORE_ALL_GRAPHS) {
		reset_buffer_has_changed (wac_buffer);
	}
	
	return TRUE;
}
/************************************************************************/
/*									*/
/*	GRAPHSTRUKTUR UND -INTERNE INFORMATIONEN AUF DATEI SCHREIBEN	*/
/*									*/
/************************************************************************/
/*									*/
/*	static	void	write_graphs (file, buffer)			*/
/*									*/
/*	Schreibt (mittels der nachfolgenden Prozeduren) alle Graphen	*/
/*	auf die Datei file.						*/
/*	Die Datei muss offen sein; sie wird auch nicht wieder		*/
/*	geschlossen.							*/
/*	Fuer das Ausgabeformat siehe die Grammatiken in scanner.l und	*/
/*	parser.y.							*/
/*	Boole'sche Werte werden ueber das Makro bool_to_int auf die	*/
/*	Datei geschrieben, Winkel in Grad, als int.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	write_graph (file, graph)			*/
/*									*/
/*	Schreibt (mittels der nachfolgenden Prozeduren) einen Graphen	*/
/*	auf file; sonst wie oben.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	write_plain_graph (file, graph)			*/
/*									*/
/*	Schreibt (mittels der nachfolgenden Prozeduren) einen Graphen	*/
/*	mit graph_internals auf die Datei file.				*/
/*	Diese Prozedur ist dazu gedacht, einen einzelnen Graphen auf	*/
/*	eine Datei zu schreiben.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	write_graph_internals (file)			*/
/*									*/
/*	Schreibt Listen der Zeichensaetze, Knoten- und Kantentypen	*/
/*	sowie graph_state und graphed_state auf file.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	int	write_edge_sourcelist (file, edge)		*/
/*	static	void	write_node            (file, node)		*/
/*	static	void	write_edge            (file, edge)		*/
/*									*/
/*	Schreiben die Source-Kantenliste eines Knotens, einen Knoten	*/
/*	oder eine einzelne Kante auf file.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	int	write_node_internals (file, node)		*/
/*	static	int	write_edge_internals (file, edge)		*/
/*	static	int	write_edgeline_internals (file, el)		*/
/*									*/
/*	Alle Attribute ausser Knotennummern und nodelabel.text bzw.	*/
/*	edgelabel.text gelten als "internal" und werden gesondert	*/
/*	gespeichert.							*/
/*	Jede Sequenz von internen Informationen (= ein Aufruf einer	*/
/*	der obigen Prozeduren) wird von BEGIN_GRAPH_INTERNALS_STRING	*/
/*	angefuehrt und von END_GRAPH_INTERNALS_STRING beendet. Dabei	*/
/*	handelt es sich um eine spezielle Form von Kommentaren, die	*/
/*	nicht von anderen Programmen verwendet werden sollte (!).	*/ 
/*									*/
/*======================================================================*/
/*									*/
/*	static	int	write_graph_state_list (file)			*/
/*									*/
/*	Schreibt Komponenten aus graph_state (ggraph.c) file.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	int	write_graphed_state_list (file)			*/
/*									*/
/*	Schreibt Informationen ueber den Zustand von GraphEd auf file.	*/
/*									*/
/************************************************************************/


static	void	write_graphs (file, buffer)
FILE		*file;
int		buffer;
{
	Graph	graph;
	
	fprintf (file, "GRAPHS\n");
	
	write_graph_internals (file);
	
	for_all_graphs (buffer, graph)
		write_graph (file, graph);
	end_for_all_graphs (buffer, graph);
	
	fprintf (file, "END\n");
}



static	void	write_graph (file, graph)
FILE		*file;
Graph		graph;
{
	register Node	node;

	if (graph == empty_graph) {
		fprintf (file, "GRAPH \"\" =\n");
		fprintf (file, "END");
		return;
	}
	
	fprintf (file, iif (graph->is_production, "PROD ", "GRAPH "));
	write_quoted_text (file, graph->label);
	fprintf (file, " = ");
	if (graph->is_production) {
		fprintf (file, " GRAGRA %d", graph->gra.type);
	}
	fprintf (file, " %s\n", iif (graph->directed, "DIRECTED", "UNDIRECTED"));

	for_nodes (graph, node)
		write_node(file, node);
		write_edge_sourcelist (file, node->sourcelist);
		fprintf (file, ";\n");
	end_for_nodes (graph, node);
		
		
	fprintf (file, "END\n");
}


static	void	write_plain_graph (file, graph)
FILE		*file;
Graph		graph;
{
	register Node	node;

	if (graph == empty_graph) {
		fprintf (file, "GRAPH \"\" =\n");
		fprintf (file, "END");
		return;
	}
	
	fprintf (file, iif (graph->is_production, "PROD ", "GRAPH "));
	write_quoted_text (file, graph->label);
	fprintf (file, " = ");
	if (graph->is_production) {
		fprintf (file, "GRAGRA %d", graph->gra.type);
	}
	fprintf (file, " %s\n", iif (graph->directed, "DIRECTED", "UNDIRECTED"));

	write_graph_internals (file);
	
	for_nodes (graph, node)
		write_node(file, node);
		write_edge_sourcelist (file, node->sourcelist);
		fprintf (file, ";\n");
	end_for_nodes (graph, node);
			
	fprintf (file, "END\n");
}


static	void	write_graph_internals (file)
FILE		*file;
{
	fprintf (file, "%s\n", BEGIN_GRAPH_INTERNALS_STRING);
	write_fonts     (file);          fprintf (file, ";\n");
	write_nodetypes (file);          fprintf (file, ";\n");
	write_edgetypes (file);          fprintf (file, ";\n");
	write_graph_state_list (file);   fprintf (file, ";\n");
	write_graphed_state_list (file);
	fprintf (file, "%s\n", END_GRAPH_INTERNALS_STRING);
}


static	 int	write_edge_sourcelist (file, edge)
FILE		*file;
register Edge	edge;
{
	register Edge	firstedge = edge;

	if (edge != empty_edge)
		do {
			write_edge (file, edge);
			edge = edge->sourcesuc;
		} while (edge != firstedge);
}


static	void	write_node (file, node)
FILE		*file;
Node		node;
{
	fprintf (file, "%d ", node->nr);
	write_node_internals (file, node);
	write_quoted_text    (file, node->label.text);
	fprintf (file, "\n");
}


static	void	write_edge (file, edge)
FILE		*file;
Edge		edge;
{
	fprintf (file, " %d ", edge->target->nr);
	write_edge_internals (file, edge);
	write_quoted_text (file, edge->label.text);
	fprintf (file, "\n");
}


static	int	write_node_internals (file, node)
FILE		*file;
Node		node;
{
	fprintf (file, BEGIN_GRAPH_INTERNALS_STRING);
	write_node_attributes (file, get_node_attributes (node));
	fprintf (file, END_GRAPH_INTERNALS_STRING);
}


static	int	write_node_attributes (file, attr)
FILE		*file;
Node_attributes	attr;
{
	fprintf (file, " NS %d %d ", attr.width, attr.height);
	fprintf (file, "NTI %d ",    attr.type_index);
	fprintf (file, "NFI %d ",    attr.font_index);
	fprintf (file, "NLP %d ",    attr.nodelabel_placement);
	fprintf (file, "NEI %d ",    attr.node_edge_interface);
	fprintf (file, "NLV %d ",    attr.label_visibility);
	fprintf (file, "COL %d ",    attr.color);
	fprintf (file, "NP %d %d ",  attr.x, attr.y);
}


static	int	write_edge_internals (file, edge)
FILE		*file;
Edge		edge;
{
	fprintf (file, BEGIN_GRAPH_INTERNALS_STRING);
	write_edge_attributes (file, get_edge_attributes (edge));
	fprintf (file, END_GRAPH_INTERNALS_STRING);
}


static	int	write_edge_attributes (file, attr)
FILE		*file;
Edge_attributes	attr;
{
	fprintf (file, " ETI %d ", attr.type_index);
	fprintf (file, "EFI %d ",  attr.font_index);
	fprintf (file, "ELV %d ",  attr.label_visibility);
	fprintf (file, "AL %d ",   attr.arrow_length);
	fprintf (file, "AA %d ",   rad_to_deg(attr.arrow_angle));
	fprintf (file, "COL %d ",  attr.color);
	if (attr.line != (Edgeline)NULL) {
		fprintf (file, "EL ");
		write_edgeline_internals (file, attr.line);
	}
}


static	int	write_edgeline_internals (file, el_head)
FILE		*file;
Edgeline	el_head;
{
	Edgeline	el;
	
	if ( (el = el_head) != (Edgeline)NULL) do {
		fprintf (file, "%d %d ", (int)el->x, (int)el->y);
		el = el->suc;
	} while (el != el_head);
}


static	int	write_graph_state_list (file)
FILE		*file;
{
	write_node_style (file, "normal", NORMAL_NODE_STYLE);
	write_node_style (file, "gragra left side", LEFT_SIDE_NODE_STYLE);
	write_node_style (file, "gragra embed node",EMBED_NODE_STYLE);
	
	write_edge_style (file, "normal", NORMAL_EDGE_STYLE);
	write_edge_style (file, "gragra embed edge", EMBED_EDGE_STYLE);

	fprintf (file, "DIR %d\n",    bool_to_int(current_directedness));
	fprintf (file, "GRAGRA %d ", current_gragra_type);
	write_quoted_text (file, get_current_gragra_terminals());
	write_quoted_text (file, get_current_gragra_nonterminals());
	fprintf (file, " GEMBED "); write_quoted_text (file, get_global_embedding_name());
	fprintf (file, " EMBMATCH %d\n", (int)get_embed_match_attributes());
	fprintf (file, "ELS %d %d\n", current_edgelabel_width,
	                              current_edgelabel_height);
	fprintf (file, "\n");
}


static	int	write_node_style (file, name, n)
FILE		*file;
char		*name;
int		n;
{
	fprintf (file, "NSTYL ");
	write_quoted_text (file, name);
	write_node_attributes (file, get_node_style(n));
	fprintf (file, ",\n");
}


static	int	write_edge_style (file, name, n)
FILE		*file;
char		*name;
int		n;
{
	fprintf (file, "ESTYL ");
	write_quoted_text (file, name);
	write_edge_attributes (file, get_edge_style(n));
	fprintf (file, ",\n");
}


static	int	write_graphed_state_list (file)
FILE		*file;
{
	int	scroll_offset_x, scroll_offset_y;
	
	get_scroll_offset (wac_buffer, &scroll_offset_x, &scroll_offset_y);
	
	fprintf (file, "WA %d %d\n",
		(int)xv_get(canvases[wac_buffer].canvas, CANVAS_WIDTH),
		(int)xv_get(canvases[wac_buffer].canvas, CANVAS_HEIGHT));
	fprintf (file, "WAWIN %d %d %d %d\n",
		(int)xv_get(canvases[wac_buffer].frame, WIN_X),
		(int)xv_get(canvases[wac_buffer].frame, WIN_Y),
		(int)xv_get(canvases[wac_buffer].frame, XV_WIDTH),
		(int)xv_get(canvases[wac_buffer].frame, XV_HEIGHT));
	fprintf (file, "SC %d %d\n", scroll_offset_x, scroll_offset_y);
	fprintf (file, "GR %d\n",    get_gridwidth (wac_buffer));
}
/************************************************************************/
/*									*/
/*			int	save_state ()				*/
/*									*/
/************************************************************************/
/*									*/
/*	Speichert die momentane Konfiguration der Benutzerschnittstelle	*/
/*	auf die Datei GRAPHED_INITIALISATION_FILE (i.a. ".graphed").	*/
/*	Von dort wird sie beim Hochfahren des Programms geladen.	*/
/*	Zur Konfiguration gehoeren Knoten- und Kantentypen, die		*/
/*	Zeichensaetze, graph_state und graphed_state (siehe oben)	*/
/*	(Alles das wird bei auch bei Graphen mit abgespeichert).	*/
/*									*/
/************************************************************************/



int	save_state ()
{
	FILE	*file;
	
	if ((file = fopen (GRAPHED_INITIALISATION_FILE, "w")) == (FILE *)NULL) {
		error ("Can't open %s for writing\n",
			GRAPHED_INITIALISATION_FILE);
		return FALSE;
	}
	
	fprintf (file, "INIT\n");
	write_fonts (file);
	fprintf (file, ";\n");
	write_nodetypes (file);
	fprintf (file, ";\n");	
	write_edgetypes (file);
	fprintf (file, ";\n");
	write_graph_state_list (file);
	fprintf (file, ";\n");
	write_graphed_state_list (file);
	fclose (file);

	init_print_fileselector ();
	init_type_fileselector ();
	init_font_fileselector ();
	init_file_fileselector ();

	if ((file = fopen (get_possible_fileselector_startup_filename(), "w")) == (FILE *)NULL) {
	
		error ("Can't open %s for writing\n", get_possible_fileselector_startup_filename());
		return FALSE;
		
	} else {
	
		
		write_print_fileselector (file);
		write_load_store_fileselector (file);
		write_type_fileselector (file);
		write_font_fileselector (file);
		
		fclose (file);
	}

}

