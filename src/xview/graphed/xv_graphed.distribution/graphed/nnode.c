/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				nnode.c					*/
/*									*
/************************************************************************/
/*									*/
/*		VERWALTUNGSFUNKTIONEN FUER DIE KNOTEN			*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "draw.h"
#include "adjust.h"
#include "type.h"
#include "font.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Node	create_node (graph)					*/
/*	void	delete_node (node)					*/
/*	Node	copy_node   (graph, node)				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void		node_set (node, ...)				*/
/*	Node_attributes	get_node_attributes (node)			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_node_marked       (node, marked)			*/
/*	void	set_node_to_be_marked (node)				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Node	find_node_with_number (graph, number)			*/
/*	Node	get_node_with_number  (graph, number)			*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	all_nodes_complete    ()				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	create_node_attributes (node, create_node_attr_proc)	*/
/*	void	push_node_attributes  (graph)				*/
/*	void	pop_node_attributes   (graph, destroy_node_attr_proc)	*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			LOKALE FUNKTIONEN				*/
/*									*/
/************************************************************************/


Node	create_node_with_number ();


/************************************************************************/
/*									*/
/*			KNOTENVERWALTUNG				*/
/*									*/
/************************************************************************/
/*									*/
/*	Alle Knoten eines Graphen sind ueber pre und suc in einer	*/
/*	doppelt verketteten Liste abgespeichert. Die Reihenfolge gibt	*/
/*	die Reihenfolge des Erzeugens an; graph.firstnode ist der	*/
/*	"aelteste" Knoten. Loeschen von Knoten aendert diese		*/
/*	Reihenfolge nicht.						*/
/*	Falls KEIN Knoten existiert, so ist graph.firstnode == NULL.	*/
/*									*/
/*======================================================================*/
/*									*/
/*	Datenstruktur Node						*/
/*									*/
/*									*/
/*	typedef	struct	node						*/
/*		{							*/
/*									*/
/*		-------   LOGISCHE STRUKTUR  -------			*/
/*									*/
/*		struct	edge		*sourcelist,			*/
/*					*targetlist;			*/
/*		struct	node		*pre, *suc;			*/
/*									*/
/*		Graph			graph;				*/
/*									*/
/*		-------      ATTRIBUTE       -------			*/
/*									*/
/*		Rect			box;				*/
/*		Nodetype		type;				*/
/*		Nodetypeimage		image;				*/
/*		Nodelabel		label;				*/
/*		int			nr;				*/
/*		Marked			marked;				*/
/*		Node			iso;				*/
/*		Node_edge_interface	node_edge_interface;		*/
/*									*/
/*		}							*/
/*		*Node;							*/
/*									*/
/*									*/
/*	sourcelist	Adjazenzlisten : die Kanten, die den Knoten	*/
/*	targetlist	als source / target haben (naehres siehe Modul	*/
/*			eedge.c)					*/
/*	pre, suc	Zeiger auf Vorgaengerknoten / Nachfolgerknoten	*/
/*			in der Liste der Knoten eines Graphs		*/
/*									*/
/*	graph		Der Graph, zu dem der Knoten gehoert		*/
/*									*/
/*	box 		Boundingbox um den Knoten (inklusive Label)	*/
/*	type 		Typ des Knoten. Details siehe Modul type.c	*/
/*	image 		Bild des Knotens. Knoten gleichen Typs und	*/
/*			gleicher Groesse haben das Bild des Knotentyps	*/
/*			gemeinsam. Genaueres siehe Module type.c und	*/
/*			paint.c.					*/
/*	label 		Siehe Nodelabel					*/
/*	marked		Flag, ob der Knoten markiert ist. Wird in	*/
/*			set_node_marked gesetzt.			*/
/*	nr		laufende Nummer (nur beim Abspeichern /		*/
/*			Wiedereinlesen wichtig)				*/
/*	iso		ist ein Zeiger auf einen isomorphen Knoten;	*/
/*			wird durch Kopieroperationen gesetzt.		*/
/*	node_edge_interface Anbindung von Kanten an den Knoten		*/
/*									*/
/*	Die Knoten sind in einer doppelt verketteten Liste, beginnend	*/
/*	mit graph.firstnode abgespeichert. Die Reihenfolge ist z.Zt.	*/
/*	nicht relevant.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	So sieht ein Knotenlabel aus :					*/
/*									*/
/*	typedef	struct {						*/
/*									*/
/*		char			*text;				*/
/*		char			*text_to_draw;			*/
/*		Graphed_font		font;				*/
/*		int			x,y;				*/
/*		Rect			box;				*/
/*		int			visible;			*/
/*		Nodelabel_placement	placement;			*/
/*									*/
/*	}								*/
/*		Nodelabel;						*/
/*									*/
/*									*/
/*	text		Text						*/
/*	text_to_draw	Dieser Text wird gezeichnet. Hier sind		*/
/*			Tabulatoren durch Leerzeichen ersetzt; text	*/
/*			wird nach dem ersten Zeilenumbruch abgebrochen	*/
/*			(-> remove_control_characters_from_text in	*/
/*			misc.c).					*/
/*	Graphed_font	Zeichenfont fuer Text. Beschreibung siehe	*/
/*			Graphed_font (->font.c).			*/
/*	x,y		Position des Textes, auf die Prozedur pf_text	*/
/*			zurechtgeschnitten				*/
/*	box		Box um den Text des Knotenlabels		*/
/*	visible		Flag, ob der Knotenlabel sichtbar (und		*/
/*			mitzuzeichnen) ist.				*/
/*	placement	Plazierung des Labels innerhalb des Knotens.	*/
/*									*/
/*	DER LABEL UEBERSCHREITET DIE BOX DES KNOTENS NIE, d.h.		*/
/*	node.label.box  <=  node.box !					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Node_edge_interface (Abkuerzung nei) :				*/
/*									*/
/*	typedef	enum	{						*/
/*		NO_NODE_EDGE_INTERFACE,					*/
/*		TO_BORDER_OF_BOUNDING_BOX,				*/
/*		TO_CORNER_OF_BOUNDING_BOX,				*/
/*		CLIPPED_TO_MIDDLE_OF_NODE,				*/
/*		SPECIAL_NODE_EDGE_INTERFACE,				*/
/*		NUMBER_OF_NODE_EDGE_INTERFACES	(Dummy)			*/
/*	}								*/
/*		Node_edge_interface;					*/
/*									*/
/*									*/
/*	Nodelabel_placement (Abkuerzung nlp) :				*/
/*									*/
/*	typedef	enum	{						*/
/*		NODELABEL_MIDDLE,					*/
/*		NODELABEL_UPPERLEFT,					*/
/*		NODELABEL_UPPERRIGHT,					*/
/*		NODELABEL_LOWERLEFT,					*/
/*		NODELABEL_LOWERRIGHT,					*/
/*		NUMBER_OF_NODELABEL_PLACEMENTS	(Dummy)			*/
/*	}								*/
/*		Nodelabel_placement;					*/
/*									*/
/*									*/
/*	Das letzte Element ist jeweils ein Dummy und gibt fuer die fuer	*/
/*	eine Deklaration eines Arrays mit Index vom Typ			*/
/*	Node_edge_interface bzw. Nodelabel_placement die benoetigte	*/
/*	Groesse an.							*/
/*	Details zu diesen Typen siehe auch adjust_nodelabel_placement	*/
/*	und adjust_line_to_node im Modul adjust.c.			*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*		     LOGISCHE STRUKTUR VERWALTEN			*/
/*									*/
/*	In den drei folgenden Prozeduren wird nichts gezeichnet !	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Node	create_node (graph)					*/
/*									*/
/*	Erzeuge einen neuen Knoten vom Typ nodetype_index und fuege	*/
/*	ihn in den Graphen in die Liste der Knoten an "letzter" Stelle	*/
/*	ein.								*/
/*	Alle Attribute sind "undefiniert" bzw. "nicht vorhanden".	*/
/*	Die Knotennummer wird 1 + hoechste bisher vergebene Nummer.	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Node	create_node_with_number (graph, nr)			*/
/*									*/
/*	Wie oben, Knotennummer = n. Falls nr = -1, hoechste bisher	*/
/*	vergebene Nummer + 1.						*/
/*	Die vorige Prozedur wird (natuerlich) ueber			*/
/*	create_node_with_number (graph, -1) abgewickelt.		*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	delete_node (node)					*/
/*									*/
/*	Loesche die zum Knoten gehoerigen Kanten und entferne den	*/
/*	Knoten aus der Liste. node->image, node->type und		*/
/*	node->label.font werden abgemeldet (unuse_...).			*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Node	copy_node (node)					*/
/*									*/
/*	Kopiert den Knoten node; die iso-Felder zeigen auf das		*/
/*	jeweilige Pendant.						*/
/*									*/
/************************************************************************/

static		int	highest_nr = 1;


Node	create_node (graph)
Graph	graph;
{
	return create_node_with_number (graph, -1);
}



Node	create_node_with_number (graph, nr)
Graph	graph;
int	nr;
{
	register	Node	new_node;

	new_node                      = (Node)mymalloc(sizeof(struct node));

	new_node->sourcelist          = empty_edge;
	new_node->targetlist          = empty_edge;
	
	/* In Graphen an letzter Stelle einfuegen	*/
	if (graph->firstnode == empty_node) {
		graph->firstnode = new_node;
		new_node->suc   = new_node;
		new_node->pre   = new_node;
	} else {
		new_node->pre      = graph->firstnode->pre;
		new_node->suc      = graph->firstnode;
		new_node->pre->suc = new_node;
		new_node->suc->pre = new_node;
	}
	
	new_node->graph = graph;
	
	new_node->type                = (Nodetype)NULL;
	new_node->image               = (Nodetypeimage)NULL;
	rect_construct (&new_node->box, -2,-2, 0,0);
	rect_construct (&new_node->full_box, -2,-2, 0,0);
	new_node->x                   = -1;
	new_node->y                   = -1;
	new_node->label.text          = NULL;
	new_node->label.text_to_draw  = (char **)NULL;
	new_node->label.font          = (Graphed_font)NULL;
	new_node->label.x             = 0;
	new_node->label.y             = 0;
	new_node->label.n_lines       = 0;
	new_node->label.line_height   = 0;
	rect_construct (&new_node->label.box, 0, 0, 0, 0);
	new_node->label.visible       = TRUE;
	new_node->label.placement     = current_nodelabel_placement;
	new_node->color               = 0;
	new_node->nr                  = iif (nr == -1, highest_nr++, nr);
	new_node->marked              = NOT_MARKED;
	new_node->node_edge_interface = current_node_edge_interface;
	new_node->loaded              = FALSE;
	new_node->iso                 = empty_node;
	new_node->attr                = NULL;
	new_node->attr_stack          = (Attr_stack)NULL;
	new_node->sgraph_node         = NULL;
	
	if (nr >= highest_nr)
		highest_nr = nr+1;

	return	new_node;
}



void	delete_node (node)
Node	node;
{
	register Graph	graph;
	register Edge	edge;

	delete_attatched_snode (node);

	/* Loesche Sourceliste : solange es Kanten gibt ...		*/
	/*                       loesche sie !				*/
	while ( (edge = node_sourcelist(node)) != empty_edge)
		delete_edge (edge);

	/* Loesche Targetliste : wie oben				*/
	while ( (edge = node_targetlist(node)) != empty_edge)
		delete_edge (edge);

	graph = node->graph;
	if (node->suc == node)
		/* Der einzige Knoten im Graph	*/
		graph->firstnode = empty_node;
	else {
		node->pre->suc = node->suc;
		node->suc->pre = node->pre;
	}
	if (graph->firstnode == node)
		graph->firstnode = node->suc;

	if (node->type != (Nodetype)NULL && !node->type->is_system)
		unuse_nodetypeimage (node->type, node->image);
	unuse_nodetype (node->type);
	unuse_font (node->label.font);
	
	if (node->label.text != NULL) {
		myfree (node->label.text);
	}
	if (node->label.text_to_draw != (char **)NULL) {
		myfree (node->label.text_to_draw);
	}
	myfree (node);
}



Node	copy_node (graph, node)
Graph	graph;
Node	node;
{
	register	Node	new_node;
	
	new_node             = (Node)mymalloc(sizeof(struct node));

	new_node->sourcelist = empty_edge;
	new_node->targetlist = empty_edge;
	
	/* In Graphen an letzter Stelle einfuegen	*/
	if (graph->firstnode == empty_node) {
		graph->firstnode = new_node;
		new_node->suc    = new_node;
		new_node->pre    = new_node;
	} else {
		new_node->pre      = graph->firstnode->pre;
		new_node->suc      = graph->firstnode;
		new_node->pre->suc = new_node;
		new_node->suc->pre = new_node;
	}
	
	new_node->graph = graph;
	
	new_node->type  = use_nodetype (get_nodetype_index (node->type));
	if (!node->type->is_system)
		new_node->image = use_nodetypeimage (node->type,
			node_width(node), node_height(node));
	else
		new_node->image = (Nodetypeimage)NULL;
			
	new_node->box = node->box;
	new_node->full_box = node->full_box;
	new_node->x   = node->x;
	new_node->y   = node->y;
	
	if (node->label.text != NULL) {
	
		int	line, n_lines = 0;
		
		new_node->label.text = strcpy (
			mymalloc(strlen(node->label.text)+1),
			node->label.text);
			
		new_node->label.text_to_draw = (char **)mycalloc(node->label.n_lines+1, sizeof (char *));
		
		for (line = 0; node->label.text_to_draw[line] != NULL; line ++) {
			new_node->label.text_to_draw[line] = strsave (
				node->label.text_to_draw[line]);
		}
		new_node->label.text_to_draw[line] = NULL;
		new_node->label.font = use_font (
			get_font_index(node->label.font));
	} else {
		new_node->label.text         = NULL;
		new_node->label.text_to_draw = (char **)NULL;
		new_node->label.font         = (Graphed_font)NULL;
	}
	new_node->label.x           = node->label.x;
	new_node->label.y           = node->label.y;
	new_node->label.box         = node->label.box;
	new_node->label.visible     = node->label.visible;
	new_node->label.placement   = node->label.placement;
	new_node->label.n_lines     = node->label.n_lines;
	new_node->label.line_height = node->label.line_height;
	
	new_node->nr     = highest_nr++;
	new_node->marked = NOT_MARKED;
	new_node->node_edge_interface = node->node_edge_interface;
	new_node->color  = node->color;
	
	new_node->loaded = node->loaded;
	new_node->iso    = node;
	node->iso        = new_node;
	
	/* Attribute werden NICHT mitkopiert !	*/
	new_node->attr        = NULL;
	new_node->attr_stack  = (Attr_stack)NULL;
	new_node->sgraph_node = NULL;
	
	return	new_node;
}
/************************************************************************/
/*									*/
/*			KNOTENATTRIBUTE VERWALTEN			*/
/*									*/
/*		    Ab jetzt wird wieder gezeichnet !			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	node_set (node, ...)					*/
/*									*/
/*	Setzen von Knotenattributen. Syntax ( [...] = optional ) :	*/
/*									*/
/*	node_set (node, [ONLY_SET,]					*/
/*		  [NODE_POSITION,         x,y,]				*/
/*		  [MOVE,                  dx,dy,]			*/
/*		  [NODE_SIZE,             width, height,]		*/
/*		  [NODE_TYPE,             nodetype_index,]		*/
/*		  [NODE_NEI,              nei,]				*/
/*		  [NODE_NLP,              nlp,]				*/
/*		  [NODE_LABEL,            text,]			*/
/*		  [NODE_FONT,             font_index,]			*/
/*		  [NODE_LABEL_VISIBILITY, visibility,]			*/
/*		  [NODE_COLOR,            color,]			*/
/*		  [RESTORE_IT,]						*/
/*		  0);							*/
/*									*/
/*	ONLY_SET dient zum schnellen setzen von Parametern; vor dem	*/
/*	Neuzeichnen muss unbedingt RESTORE_IT aufgerufen werden.	*/
/*	(ONLY_SET setzt nur den Wert ein und loescht/zeichnet nicht und	*/
/*	fuehrt keine adjust_...	Prozeduren durch).			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_node_marked (node, marked)				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_node_to_be_marked (node)				*/
/*									*/
/*	Knoten wird beim naechsten Neuzeichnen (force_repainting)	*/
/*	markiert.							*/
/*									*/
/************************************************************************/



void	node_set (va_alist)
va_dcl
{
	va_list		args;
	
	int		adjust_nlp   = FALSE,	/* Flags whether to adjust	*/
			adjust_edges = FALSE,	/* attributes etc; a set would	*/
			adjust_graph = FALSE,	/* be the right thing here ...	*/
			adjust_label_text_to_draw = FALSE;	/* but in C ?	*/
	int		redraw_node               = FALSE,
			redraw_only_label         = FALSE,
			redraw_edges              = FALSE;
			
	int		set_position   = FALSE,	/* Flags what to set		*/
			move           = FALSE,
			set_size       = FALSE,
			set_type       = FALSE,
			set_nei        = FALSE,
			set_label      = FALSE,
			set_font       = FALSE,
			set_visibility = FALSE,
			set_nlp        = FALSE,
			set_color      = FALSE;
	
	int		only_set     = FALSE;
	int		restore_node = FALSE;
			
	Set_attribute	attr;
	
	Node		node;
	Edge		edge;
	int		x,y, dx,dy, width, height;
	int		nodetype_index, font_index, visible, nei, nlp, color;
	char		*text;
	
	
	
	
	va_start (args);
	
	node = va_arg (args, Node);
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
			redraw_node  = TRUE;
			redraw_edges = TRUE;
			adjust_nlp   = TRUE;
			adjust_edges = TRUE;
			adjust_graph = TRUE;
			set_position = TRUE;
			break;
		
		    case MOVE :
			dx = va_arg (args, int);
			dy = va_arg (args, int);
			redraw_node  = TRUE;
			redraw_edges = TRUE;
			adjust_nlp   = TRUE;
			adjust_edges = TRUE;
			adjust_graph = TRUE;
			move = TRUE;
			break;
		
		    case NODE_SIZE :
			width = va_arg (args, int);
			height = va_arg (args, int);
			if (width < 1)  width  = 1;
			if (height < 1) height = 1;
			redraw_node  = TRUE;
			redraw_edges = TRUE;
			adjust_nlp   = TRUE;
			adjust_edges = TRUE;
			adjust_graph = TRUE;
			adjust_label_text_to_draw = TRUE;
			set_size     = TRUE;
			break;
	
		    case NODE_TYPE :
			nodetype_index = va_arg (args, int);
			redraw_node  = TRUE;
			redraw_edges = TRUE;
			adjust_label_text_to_draw = TRUE;
			adjust_nlp                = TRUE;
			adjust_edges              = TRUE;
			/* Adjusting the graph seems to be unnecessary	*/
			set_type                  = TRUE;
			break;
	
		    case NODE_NEI :
			nei = va_arg (args, int);
			redraw_edges = TRUE;
			adjust_edges = TRUE;
			/* Adjusting the graph seems to be unnecessary	*/
			set_nei      = TRUE;
			break;
	
		    case NODE_LABEL :
			text = va_arg (args,char *);
			if (text != NULL && strcmp(text, ""))
				redraw_only_label = TRUE;
			else
				/* The label size is 0,	and therefore nothing	*/
				/* gets done - the markings would not be	*/
				/* redrawn in force_repainting. To avoid this,	*/
				/* redraw the whole node. MH 18/6/89		*/
				redraw_node       = TRUE;
			adjust_label_text_to_draw = TRUE;
			adjust_nlp                = TRUE;
			set_label                 = TRUE;
			break;
	
		    case NODE_FONT :
			font_index = va_arg (args, int);
			redraw_node               = TRUE;
			adjust_label_text_to_draw = TRUE;
			adjust_nlp                = TRUE;
			set_font                  = TRUE;
			break;
	
		    case NODE_LABEL_VISIBILITY :
			visible        = va_arg (args, int);
			redraw_node    = TRUE;
			set_visibility = TRUE;
			break;
	
		    case NODE_NLP :
			nlp         = va_arg (args, int);
			redraw_node = TRUE;
			adjust_nlp  = TRUE;
			set_nlp     = TRUE;
			break;
	
		    case NODE_COLOR :
			color       = va_arg (args, int);
			set_color   = TRUE;
			redraw_node = TRUE;
			break;
	
		    case RESTORE_IT :
			redraw_node               = TRUE,
			redraw_edges              = TRUE;
			restore_node              = TRUE;
			adjust_nlp                = TRUE,
			adjust_edges              = TRUE,
			adjust_graph              = TRUE,
			adjust_label_text_to_draw = TRUE;
			break;
		
		    default :
			break;
		}
	
		attr = va_arg (args, Set_attribute);
	}
	
	
	/* Unmark && erase	*/
	if (!restore_node) {
		set_node_to_be_marked (node);
		if (redraw_edges)
			erase_edges_at_node (node);
		if (redraw_node)
			erase_node (node);
		else if (redraw_only_label)
			erase_nodelabel (node);
	}
	
	
	/* Let's do It		*/
	
	if (set_position || move) {
	
		if (set_position) {
			dx = x - node_x (node);
			dy = y - node_y (node);
		} else /* move */ {
			x = dx + node_x (node);
			y = dy + node_y (node);
		}
		
		rect_construct (&(node->box),
	                        x - node_width(node)/2, y - node_height(node)/2,
		                node_width(node),       node_height(node));
		node->full_box = rect_bounding (&(node->box), &(node->label.box));
		
		node->x = x;
		node->y = y;
		
		if (node->node_edge_interface == NO_NODE_EDGE_INTERFACE &&
		    !graphed_state.loading) {
			for_edge_sourcelist (node, edge) {
				set_edgeline_xy (edge->line,
				                 edge->line->x + dx,
				                 edge->line->y + dy);
			} end_for_edge_sourcelist (node, edge);
			for_edge_targetlist (node, edge) {
				set_edgeline_xy (edge->line->pre,
				                 edge->line->pre->x + dx,
				                 edge->line->pre->y + dy);
			} end_for_edge_targetlist (node, edge);
		}
	}
	
	if (set_size) {
		rect_construct (&(node->box),
	                (int)node_x(node) - width/2,
	                (int)node_y(node) - height/2,
	                width, height);
		node->full_box = rect_bounding (&(node->box), &(node->label.box));
		if ((node->type != (Nodetype)NULL) && (!node->type->is_system)) {
			unuse_nodetypeimage (node->type, node->image);
			node->image = use_nodetypeimage (node->type, width, height);
		}
	}
	
	if (set_type) {	
		unuse_nodetypeimage (node->type, node->image);
		unuse_nodetype      (node->type);
		node->type = use_nodetype (nodetype_index);
		if (!node->type->is_system)
			node->image = use_nodetypeimage (node->type, node_width(node), node_height(node));
		else
			/* image is not used; it is always drawn new	*/
			node->image = (Nodetypeimage)NULL;
	}
	
	if (set_nei) {	
		node->node_edge_interface = nei;
	}
			
	if (set_label) {
		if (node->label.text != NULL)
			myfree (node->label.text);
		if (text == NULL) {
			unuse_font (node->label.font);
			node->label.text = NULL;
			node->label.font = (Graphed_font)NULL;
		} else {
			node->label.text = text;
			if (node->label.font == (Graphed_font)NULL)
				node->label.font = use_font (current_nodefont_index);
		}
	}
	
	if (set_font) {	
		unuse_font (node->label.font);
		if (font_index != -1) {
			node->label.font = use_font (font_index);
		}
	}
			
	if (set_visibility) {
		node->label.visible = visible;
	}
			
	if (set_nlp) {
		node->label.placement = nlp;
	}
	if (set_color) {
		node->color = color;
	}
			

	/* Adjust it 		*/
	
	if (!only_set) {
		if (adjust_label_text_to_draw)
			adjust_nodelabel_text_to_draw (node);
		if (adjust_nlp)
			adjust_nodelabel_position (node);
		if (adjust_edges && !only_set)
			adjust_all_edges (node);
		if (adjust_graph && !only_set)
			adjust_graph_box (node->graph);
	}
	
	/* mark again && redraw	*/
	if (!only_set) {
		if (redraw_edges)
			draw_edges_at_node (node);
		if (redraw_node)
			draw_node (node);
		else if (redraw_only_label)
			draw_nodelabel (node);
	}
	
	set_graph_has_changed (node->graph);
	
	va_end (args);
}



Node_attributes	get_node_attributes (node)
Node		node;
{
	Node_attributes attr;
	
	attr.x = node_x (node);
	attr.y = node_y (node);
	attr.width  = node_width  (node);
	attr.height = node_height (node);
	attr.type_index = get_nodetype_index (node->type);
	if (node->label.font != (Graphed_font)NULL)
		attr.font_index = get_font_index (node->label.font);
	else
		attr.font_index = current_nodefont_index;
	attr.nodelabel_placement = node->label.placement;
	attr.node_edge_interface = node->node_edge_interface;
	attr.label_visibility    = node->label.visible;
	attr.color               = node->color;
	attr.label               = strsave (node->label.text);
	
	return	attr;
}



void	set_node_marked (node, marked)
Node	node;
Marked	marked;
{
	switch (marked) {
	     case NOT_MARKED :
		if (is_marked(node))
			do_unmark_node (node);
		node->marked = marked;
		break;
	    case MARKED_WITH_SQUARES :
	    case MARKED_AT_BOUNDARY  :
		if (is_marked(node)) {
			do_unmark_node (node);
			node->marked = marked;
			do_mark_node (node);
		} else if (is_not_marked(node)) {
			node->marked = marked;
			do_mark_node (node);
		} else /* is_not_marked (node)	*/ {
			node->marked = marked;	
			do_mark_node (node);
		}		
		break;
	    case TO_BE_MARKED_WITH_SQUARES :
	    case TO_BE_MARKED_AT_BOUNDARY  :
		if (is_marked(node))
			do_unmark_node (node);
		node->marked = marked;
		break;
	}
}


void	set_node_to_be_marked (node)
Node	node;
{
	switch (node->marked) {
	     case NOT_MARKED :
		break;
	    case MARKED_WITH_SQUARES :
		set_node_marked (node, TO_BE_MARKED_WITH_SQUARES);
		break;
	    case MARKED_AT_BOUNDARY  :
		set_node_marked (node, TO_BE_MARKED_AT_BOUNDARY);
		break;
	    case TO_BE_MARKED_WITH_SQUARES :
	    case TO_BE_MARKED_AT_BOUNDARY  :
		break;
	}
}
/************************************************************************/
/*									*/
/*			NACH KNOTEN SUCHEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Node	find_node_with_number (graph, number)			*/
/*									*/
/*	Sucht den Knoten number; Rueckgabe empty_node, wenn gefunden.	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Node	get_node_with_number (number)				*/
/*									*/
/*	Gibt den Knoten mit Nummer number zurueck. Falls kein solcher	*/
/*	Knoten existiert, wird einer neu erzeugt.			*/
/*									*/
/************************************************************************/


Node	find_node_with_number (graph, number)
Graph	graph;
int	number;
{
	register Node	node  = empty_node;
	
	for_nodes (graph, node) {
		if (node->nr == number) return node;
	} end_for_nodes (graph, node);
		
	return empty_node;
}



Node	get_node_with_number (graph, number)
Graph	graph;
int	number;
{
	Node	node;
	
	if ( (node = find_node_with_number (graph, number)) != empty_node)
		return node;
	else
		return create_node_with_number (graph, number);
}
/************************************************************************/
/*									*/
/*		KNOTE AUF VOLLSTAENDIGKEIT UEBERPRUEFEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	int	all_nodes_complete ()					*/
/*									*/
/*	Da beim Laden von Graphen bei Kanten Knoten im Voraus angelegt	*/
/*	werden muessen (Adjazenzlistenformat), muss am Ende abgeprueft	*/
/*	werden, ob alle Knoten spezifiziert wurden. Ist in der Eingabe	*/
/*	ein Knoten versehentlich nicht spezifiziert worden, so fuehrt	*/
/*	das zu Problemen, da fuer einen solchen Knoten keine Attribute	*/
/*	(insbesondere Typ) gesetzt sind.				*/
/*									*/
/************************************************************************/


int	all_nodes_complete ()
{
	Node	node;
	Graph	graph;
	
	for_all_graphs (wac_buffer, graph)
	    for_nodes (graph, node)
		if (node_x(node) == -1) return FALSE;
	    end_for_nodes (graph, node);
	end_for_all_graphs (wac_buffer, graph)
	
	return TRUE;
}
/************************************************************************/
/*									*/
/*		BENUTZERSPEZIFISCHE ATTRIBUTE VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_node_attributes (node, create_node_attr_proc)	*/
/*	void	push_node_attributes   (node)				*/
/*	void	pop_node_attributes    (node, destroy_node_attr_proc)	*/
/*									*/
/*	char	*create_node_attr_proc (node);				*/
/*	void	destroy_node_attr_proc (node);				*/
/*									*/
/************************************************************************/



void	create_node_attributes (node, create_node_attr_proc)
Node	node;
char	*(*create_node_attr_proc)();
{
	node->attr = create_node_attr_proc (node);
}


void	push_node_attributes (node)
Node	node;
{
	Attr_stack	new_stack;
	
	new_stack = (Attr_stack) mymalloc (sizeof(struct attr_stack));
	new_stack->attr = node->attr;
	new_stack->next = iif (node->attr_stack != (Attr_stack)NULL,
	                       node->attr_stack->next,
	                       (Attr_stack)NULL);
	                         
	node->attr = NULL;
	node->attr_stack = new_stack;
}


void	pop_node_attributes    (node, destroy_node_attr_proc)
Node	node;
void	(*destroy_node_attr_proc)();
{
	destroy_node_attr_proc (node);
	node->attr = NULL;
	
	if (node->attr_stack != NULL && node->attr_stack->attr != NULL) {
		
		Attr_stack	top = node->attr_stack;
		
		node->attr       = top->attr;
		node->attr_stack = top->next;
		
		myfree (top);
	}
}

