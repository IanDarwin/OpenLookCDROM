/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				eedge.c					*/
/*									*
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltungsfunktionen und		*/
/*	-prozeduren fuer Kanten.					*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "adjust.h"
#include "draw.h"
#include "type.h"
#include "font.h"



/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_edge (snode, tnode)				*/
/*	void	delete_edge (edge)					*/
/*	Edge	copy_edge   (graph, edge)				*/
/*	Edge	copy_edge_without_line (graph, edge)			*/
/*									*/
/*	void	edge_set (edge, ...)					*/
/*									*/
/*	void	move_edge     (edge, el, x,y) |				*/
/*	void	extend_edge   (edge, el, x,y) + for compatibility only	*/
/*	void	comprime_edge (edge, el)      |				*/
/*									*/
/*	void	set_edge_marked       (edge, marked)			*/
/*	void	set_edge_to_be_marked (edge)				*/
/*									*/
/*	Edgeline	new_edgeline         (x,y)			*/
/*	Edgeline	add_to_edgeline      (el_tail, x,y)		*/
/*	Edgeline	remove_from_edgeline (el)			*/
/*	void		set_edgeline_xy      (el,      x,y)		*/
/*	void		free_edgeline        (el_head)			*/
/*	Edgeline	copy_edgeline        (el_head)			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	create_edge_attributes (edge, create_edge_attr_proc)	*/
/*	void	push_edge_attributes (edge)				*/
/*	void	pop_edge_attributes  (edge, destroy_edge_attr_proc)	*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*			ZUM AUFBAU EINER KANTE				*/
/*									*/
/************************************************************************/
/*									*/
/*		     LOGISCHE STRUKTUR DES GRAPHEN			*/
/*									*/
/*									*/
/*	Die Kanten werden in Kettrecord - Darstellung abgepeichert.	*/
/*	Jede Kante ist in zwei doppelt verketteten, geschlossenen	*/
/*	Listen [ (sourcepre, sourcesuc) fuer die vom Sourceknoten	*/
/*	ausgehenden, (targetpre, targetsuc) fuer die zum Targetknoten	*/
/*	einlaufenden Kanten ] enthalten. 				*/
/*									*/
/*									*/
/*		sourcesuc		targetsuc			*/
/*		   ^			    |				*/
/*		   +--------+     +---------+				*/
/*			    |     ^					*/
/*		         +-----------+					*/
/*			 |   \   /   |					*/
/*			 |    \ /    |					*/
/*	source <---------+  E D G E  |---------> target			*/
/*			 |    / \    |					*/
/*			 |   /   \   |					*/
/*			 +-----------+					*/
/*			    ^     |					*/
/*		   +--------+     +---------+				*/
/*		   |                        ^				*/
/*		targetpre		sourcepre			*/
/*									*/
/*	mit :	source = edge->source					*/
/*		target = edge->target					*/
/*									*/
/*	Eine Kante besteht aus fuenf Teilen :				*/
/*	- der zentralen Datenstruktur Edge				*/
/*	- dem Kantenzug "Edgeline", dem Kantenzug			*/
/*	- einem Kantentyp, der in type.c verwaltet wird			*/
/*	- einem Label							*/
/*	- einem Pfeil (Arrow) am Zielknoten				*/
/*									*/
/*	Gelegentlich werden Kopf- und Schwanzstueck der Kante als	*/
/*	mit head und tail bezeichnet :					*/
/*	- head = erstes Stueck (1. - 2. Punkt) von edge->line +		*/
/*	         Label der Kante (da dieser in die Mitte der obigen	*/
/*	         Strecke gelegt wird) +					*/
/*	         Pfeil, falls is_single_edgeline (edge->line)		*/
/*	- tail = letztes Stueck (vorletzter - letzter Punkt) von	*/
/*	         edge->line +						*/
/*	         Pfeil +						*/
/*	         Label, falls is_single_edgeline (edge->line).		*/
/*									*/
/*======================================================================*/
/*									*/
/*			TYPDEKLARATION					*/
/*									*/
/*									*/
/*	typedef	struct	edge						*/
/*		{							*/
/*									*/
/*		-------   LOGISCHE STRUKTUR  -------			*/
/*									*/
/*		struct	node	*source,				*/
/*				*target;				*/
/*		struct	edge	*sourcepre,				*/
/*				*sourcesuc,				*/
/*				*targetpre,				*/
/*				*targetsuc;				*/
/*									*/
/*		-------       ATTRIBUTE      -------			*/
/*									*/
/*		Rect		box;					*/
/*		Marked		marked;					*/
/*		Edgeline	line;					*/
/*		Edgetype	type;					*/
/*		Edgelabel	label;					*/
/*		Arrow		arrow;					*/
/*									*/
/*	}								*/
/*		*Edge;							*/
/*									*/
/*									*/
/*	source, target	Ausgangs- und Endknoten der Kante		*/
/*	sourcepre,	+						*/
/*	sourcesuc,	| Doppelt verkettete geschlossene Listen	*/
/*	targetpre,	| Diagramm siehe oben				*/
/*	targetsuc	+						*/
/*									*/
/*	box		Die gesamte (line + label + arrow) Kante	*/
/*			umfassendes Rechteck.				*/
/*	marked		Flag, ob die Kante markiert ist. Wird in	*/
/*			set_edge_marked (wo sonst wohl ?) gesetzt.	*/
/*	line		Liste mit den Punkten, die die Kante bilden	*/
/*			(naeheres siehe Edgeline weiter unten)		*/
/*	type		Kantentyp (siehe Edgetype in type.c)		*/
/*	label		Kantenmarkierung (siehe Edgelabel)		*/
/*	arrow		Pfeil der Kante	(siehe Arrow)			*/
/*									*/
/*======================================================================*/
/*									*/
/*			KANTENLABEL					*/
/*									*/
/*									*/
/*	typedef	struct	{						*/
/*									*/
/*		char		*text;					*/
/*		char		*text_to_draw;				*/
/*		Graphed_font	font;					*/
/*		int		x,y;					*/
/*		Rect		box;					*/
/*		int		visible;				*/
/*									*/
/*	}								*/
/*		Edgelabel;						*/
/*									*/
/*									*/
/*	text		Text						*/
/*	text_to_draw	Dieser Text wird gezeichnet. Hier sind		*/
/*			Tabulatoren durch Leerzeichen ersetzt; text	*/
/*			wird nach dem ersten Zeilenumbuch abgebrochen.	*/
/*			(-> adjust_edgelabel_text_to_draw in adjust.c).	*/
/*	Graphed_font	Zeichenfont fuer Text. Beschreibung siehe	*/
/*			font.c.						*/
/*	x,y		Position des Textes, auf die Prozedur pf_text	*/
/*			zurechtgeschnitten.				*/
/*	box		Box um den Text des Kantenlabels.		*/
/*	visible		Sichtbarkeit des Labels.			*/
/*									*/
/*======================================================================*/
/*									*/
/*			PFEIL AN DER KANTE				*/
/*									*/
/*									*/
/*	typedef	struct {						*/
/*									*/
/*		int		x0,y0, x1,y1, x2,y2;			*/
/*		Rect		box;					*/
/*		int		length;					*/
/*		float		angle;					*/
/*									*/
/*	}								*/
/*		Arrow;							*/
/*									*/
/*									*/
/*	x0, ... ,y2	Linienzug des Pfeiles, (x0,y0) - (x1,y1) -	*/
/*			(x2,y2); Pfeilspitze ist (x1,y1).		*/
/*	box		Box um den Pfeil.				*/
/*	length		Laenge eines der Kantenzuege des Pfeiles	*/
/*	angle		Winkel zwischen Pfeil und Kante			*/
/*									*/
/*		     (x0,y0)						*/
/*			\ l						*/
/*			_\ e						*/
/*		angle  /  \ n						*/
/*		      |    \ gth					*/
/*	--------------------> (x1,y1)					*/
/*	Kante (an Target)  / l						*/
/*			  / e						*/
/*			 / n						*/
/*			/ gth						*/
/*		     (x2,y2)						*/
/*									*/
/*	Jede Kante hat einen Pfeil am Ende mit dem Target-Knoten.	*/
/*									*/
/*======================================================================*/
/*									*/
/*	Naeheres zu den Attributen siehe auch modul adjust.c		*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*		LOGISCHE STRUKTUR DER KANTEN VERWALTEN			*/
/*									*/
/*	     Die beiden folgenden Prozeduren zeichnen nichts !		*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_edge (snode, tnode)				*/
/*									*/
/*	Erzeuge eine neue Kante vom vom Knoten snode zum Knoten tnode	*/
/*	und fuege sie in die entsprechenden Listen (snode->sourcelist,	*/
/*	tnode->targetlist) ein.						*/
/*	Alle Attribute sind "undefiniert" bzw. "nicht vorhanden".	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	delete_edge (edge)					*/
/*									*/
/*	Entferne die Kante aus den source- und targetlisten, melde type	*/
/*	und font ab und loesche dann den Speicherplatz.			*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Edge	copy_edge_without_line (graph, edge)			*/
/*	Edge	copy_edge              (graph, edge)			*/
/*									*/
/*	Kopiert edge in graph (erstere Funktion kopiert edge->line	*/
/*	nicht mit).							*/
/*	Die neue Kante wird zwischen edge->source->iso und		*/
/*	edge->target->iso verlegt.					*/
/*									*/
/************************************************************************/


Edge		create_edge (snode, tnode)
Node		snode;	/* Sourceknoten	*/
Node		tnode;	/* Targetknoten	*/
{
	return create_edge_with_number (snode, tnode, -1);
}

Edge		create_edge_with_number (snode, tnode, nr)
Node		snode;	/* Sourceknoten	*/
Node		tnode;	/* Targetknoten	*/
int		nr;
{
	register	Edge	new_edge;
	static		int	highest_nr_up_to_now = 0;
	
	new_edge                = (Edge)mymalloc (sizeof(struct edge));
	new_edge->line          = (Edgeline)NULL;
	new_edge->marked        = NOT_MARKED;
	new_edge->type          = (Edgetype)NULL;
	rect_construct (&new_edge->label.box, 0, 0, 0, 0);
	rect_construct (&new_edge->box,       0, 0, 0, 0);
	new_edge->label.visible = TRUE;
	new_edge->source        = snode;
	new_edge->target        = tnode;
	new_edge->sourcepre     = new_edge->sourcesuc = empty_edge;
	new_edge->targetpre     = new_edge->targetsuc = empty_edge;
	new_edge->color         = 0;
	new_edge->nr            = iif (nr == -1, highest_nr_up_to_now++, nr);
	
	new_edge->label.text          = NULL;
	new_edge->label.text_to_draw  = (char **)NULL;
	new_edge->label.font          = (Graphed_font)NULL;
	new_edge->label.x             = 0;
	new_edge->label.y             = 0;
	
	new_edge->arrow.length = 0;
	new_edge->arrow.angle  = 0.0;
	
	new_edge->attr        = NULL;
	new_edge->attr_stack  = (Attr_stack)NULL;
	new_edge->sgraph_edge = NULL;
	
	if (snode->sourcelist == empty_edge) {
		snode->sourcelist   = new_edge;
		new_edge->sourcepre = new_edge;
		new_edge->sourcesuc = new_edge;
	} else {
		new_edge->sourcepre = snode->sourcelist->sourcepre;
		new_edge->sourcesuc = snode->sourcelist;
		new_edge->sourcepre->sourcesuc = new_edge;
		new_edge->sourcesuc->sourcepre = new_edge;
	}
	if (tnode->targetlist == empty_edge) {
		tnode->targetlist   = new_edge;
		new_edge->targetpre = new_edge;
		new_edge->targetsuc = new_edge;
	} else {
		new_edge->targetpre = tnode->targetlist->targetpre;
		new_edge->targetsuc = tnode->targetlist;
		new_edge->targetpre->targetsuc = new_edge;
		new_edge->targetsuc->targetpre = new_edge;
	}

	if (nr >= highest_nr_up_to_now)
		highest_nr_up_to_now = nr+1;

	return	new_edge;
}


void	delete_edge (edge)
Edge	edge;
{
	delete_attatched_sedge (edge);

	if (edge->sourcesuc == edge)
		edge->source->sourcelist = empty_edge;
	else {
		edge->sourcepre->sourcesuc = edge->sourcesuc;
		edge->sourcesuc->sourcepre = edge->sourcepre;
	}
	if (edge->source->sourcelist == edge)
		edge->source->sourcelist = edge->sourcesuc;

	if (edge->targetsuc == edge)
		edge->target->targetlist = empty_edge;
	else {
		edge->targetpre->targetsuc = edge->targetsuc;
		edge->targetsuc->targetpre = edge->targetpre;
	}
	if (edge->target->targetlist == edge)
		edge->target->targetlist = edge->targetsuc;
		
	free_edgeline (edge_edgeline(edge));
	edge_edgeline (edge) = (Edgeline)NULL;

	unuse_edgetype (edge->type);
	unuse_font (edge->label.font);
	
	if (edge->label.text != NULL) {
		myfree (edge->label.text);
	}
	if (edge->label.text_to_draw != NULL) {
		myfree (edge->label.text_to_draw);
	}
	myfree (edge);
}


Edge	copy_edge_without_line (graph, edge)
Graph	graph;
Edge	edge;
{
	register	Edge	new_edge;

	new_edge                = (Edge)mymalloc (sizeof(struct edge));
	
	new_edge->line          = (Edgeline)NULL;
	new_edge->marked        = NOT_MARKED;
	new_edge->type          = use_edgetype(get_edgetype_index(edge->type));
	new_edge->box           = edge->box;
	new_edge->label.box     = edge->label.box;
	new_edge->label.visible = edge->label.visible;
	new_edge->source        = edge->source->iso;
	new_edge->target        = edge->target->iso;
	new_edge->sourcepre     = new_edge->sourcesuc = empty_edge;
	new_edge->targetpre     = new_edge->targetsuc = empty_edge;
	new_edge->color         = edge->color;

	if (edge->label.text != NULL) {
		new_edge->label.text = strcpy(
			mymalloc(strlen(edge->label.text)+1),
				edge->label.text);
		new_edge->label.text_to_draw = (char **)mymalloc (sizeof(char *));
		*(new_edge->label.text_to_draw) = strcpy(
			mymalloc(strlen(*(edge->label.text_to_draw))+1),
				*(edge->label.text_to_draw));
		new_edge->label.font = use_font(
			get_font_index(edge->label.font));
	} else {
		new_edge->label.text         = NULL;
		new_edge->label.text_to_draw = (char **)NULL;
		new_edge->label.font         = (Graphed_font)NULL;
	}
	new_edge->label.x = edge->label.x;
	new_edge->label.y = edge->label.y;
	
	new_edge->arrow = edge->arrow;

	/* Attribute werden NICHT mitkopiert !	*/
	new_edge->attr        = NULL;
	new_edge->attr_stack  = (Attr_stack)NULL;
	new_edge->sgraph_edge = NULL;
	
	if (new_edge->source->sourcelist == empty_edge) {
		new_edge->source->sourcelist = new_edge;
		new_edge->sourcepre = new_edge;
		new_edge->sourcesuc = new_edge;
	} else {
		new_edge->sourcepre = new_edge->source->sourcelist->sourcepre;
		new_edge->sourcesuc = new_edge->source->sourcelist;
		new_edge->sourcepre->sourcesuc = new_edge;
		new_edge->sourcesuc->sourcepre = new_edge;
	}
	if (new_edge->target->targetlist == empty_edge) {
		new_edge->target->targetlist = new_edge;
		new_edge->targetpre = new_edge;
		new_edge->targetsuc = new_edge;
	} else {
		new_edge->targetpre = new_edge->target->targetlist->targetpre;
		new_edge->targetsuc = new_edge->target->targetlist;
		new_edge->targetpre->targetsuc = new_edge;
		new_edge->targetsuc->targetpre = new_edge;
	}

	return	new_edge;

}



Edge	copy_edge (graph, edge)
Graph	graph;
Edge	edge;
{
	Edge	new_edge;

	new_edge	= copy_edge_without_line (graph, edge);
	new_edge->line  = copy_edgeline (edge->line);
	
	return	new_edge;
}
/************************************************************************/
/*									*/
/*		       KANTE POSITIONIEREN				*/
/*									*/
/*		Ab jetzt wird wieder mitgezeichnet !			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	edge_set (edge, ...)					*/
/*									*/
/*	edge_set (edge,							*/
/*	          [ONLY_SET],		  Nur setzen, keine adjust_...	*/
/*	          [EDGE_LINE,   line]					*/
/*	          [EDGE_INSERT, el, x,y]  Nach el (x,y) einfuegen	*/
/*	          [EDGE_DELETE, el]       el aus edge->line loeschen	*/
/*	          [MOVE, el, dx,dy]					*/
/*	          [EDGE_TYPE, index]					*/
/*	          [EDGE_FONT, index]					*/
/*	          [EDGE_LABEL, text]					*/
/*	          [EDGE_LABEL_VISIBILITY, visible]			*/
/*	          [EDGE_ARROW_LENGTH, length]				*/
/*	          [EDGE_ARROW_ANGLE, angle]				*/
/*	          [RESTORE_IT]		    Nur nach ONLY_SET		*/
/*									*/
/*	ONLY_SET dient zum schnellen setzen von Parametern; vor dem	*/
/*	Neuzeichnen muss unbedingt RESTORE_IT aufgerufen werden.	*/
/*	(ONLY_SET setzt nur den Wert ein und loescht/zeichnet nicht und	*/
/*	fuehrt keine adjust_...	Prozeduren durch).			*/
/*	EDGE_DELETE kann edge->line loeschen !				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	move_edge (edge, el, x,y)				*/
/*									*/
/*	Bewegt den Punkt el in edge->line auf Position (x,y).		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	extend_edge (edge, el, x,y)				*/
/*									*/
/*	Erweitert edge->line um einen Punkt (x,y) NACH el.		*/
/*	ACHTUNG : el != edge->line->pre (= letzter Punkt).		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	comprime_edge (edge, el)				*/
/*									*/
/*	Entfernt el aus edge->line.					*/
/*									*/
/************************************************************************/


void	edge_set (va_alist)
va_dcl
{
	va_list		args;
	
	int		adjust_elp   = FALSE,
			adjust_graph = FALSE,
			adjust_arrow = FALSE,
			adjust_box   = FALSE,
			adjust_head_and_tail = FALSE,
			adjust_label_text_to_draw = FALSE;
			
	int		redraw_edge   = FALSE,
			redraw_label  = FALSE;
			
	int		set_line         = FALSE,	/* Flags what to set	*/
			set_type         = FALSE,
			set_label        = FALSE,
			set_font         = FALSE,
			set_visibility   = FALSE,
			set_arrow_length = FALSE,
			set_arrow_angle  = FALSE,
			set_color       = FALSE;
	
	int		only_set     = FALSE;
	int		restore_edge = FALSE;
			
	Set_attribute	attr;
	
	Edge		edge;
	Edgeline	el;
	int		x,y, dx,dy;
	int		edgetype_index, font_index, visible, length, color;
	double		angle;
	char		*text;
	
	
	
	
	va_start (args);
	
	edge = va_arg (args, Edge);
	attr = va_arg (args, Set_attribute);
	if (attr == ONLY_SET) {
		only_set = TRUE;
		attr = va_arg (args, Set_attribute);
	}
		
	/* Set the flags etc.	*/
	
	while (attr != SET_ATTRIBUTE_END) {
	
		switch (attr) {
	
		    case EDGE_LINE :
			el = va_arg (args, Edgeline);
			redraw_edge          = TRUE;
			adjust_elp           = TRUE;
			adjust_head_and_tail = TRUE;
			adjust_box           = TRUE;
			adjust_graph         = TRUE,
			set_line             = TRUE;
			break;
		
		    case EDGE_INSERT :
			el = va_arg (args, Edgeline);
			x = va_arg (args, int);
			y = va_arg (args, int);
			el = add_to_edgeline (el, x,y);
			el = edge->line;
			redraw_edge          = TRUE;
			adjust_elp           = TRUE;
			adjust_head_and_tail = TRUE;
			adjust_box           = TRUE;
			adjust_graph         = TRUE,
			set_line             = TRUE;
			break;
		
		    case EDGE_DELETE :
			el = va_arg (args, Edgeline);
			if (el == edge->line) {
				el = remove_from_edgeline (el);
				edge->line = el->suc;
			} else
				el = remove_from_edgeline (el);
			el = edge->line;
			redraw_edge          = TRUE;
			adjust_elp           = TRUE;
			adjust_head_and_tail = TRUE;
			adjust_box           = TRUE;
			adjust_graph         = TRUE,
			set_line             = TRUE;
			break;
		
		    case MOVE :
			el = va_arg (args, Edgeline);
			dx  = va_arg (args, int);
			dy  = va_arg (args, int);
			set_edgeline_xy (el, el->x + dx, el->y + dy);
			redraw_edge          = TRUE;
			adjust_elp           = TRUE;
			adjust_head_and_tail = TRUE;
			adjust_box           = TRUE;
			adjust_graph         = TRUE;
			break;
		
		    case EDGE_TYPE :
			edgetype_index = va_arg (args, int);
			redraw_edge    = TRUE;
			set_type       = TRUE;
			break;
	
		    case EDGE_LABEL :
			text = va_arg (args,char *);
			if (text != NULL && strcmp(text, ""))
				redraw_label = TRUE;
			else
				/* The label size is 0,	and therefore nothing	*/
				/* gets done - the markings would not be	*/
				/* redrawn in force_repainting. To avoid this,	*/
				/* redraw the whole edge. MH 18/6/89		*/
				redraw_edge       = TRUE;
			adjust_label_text_to_draw = TRUE;
			adjust_elp                = TRUE;
			adjust_box                = TRUE;
			adjust_graph              = TRUE,
			set_label                 = TRUE;
			break;
	
		    case EDGE_FONT :
			font_index = va_arg (args, int);
			redraw_label              = TRUE;
			adjust_label_text_to_draw = TRUE;
			adjust_elp                = TRUE;
			adjust_box                = TRUE;
			adjust_graph              = TRUE,
			set_font                  = TRUE;
			break;
	
		    case EDGE_LABEL_VISIBILITY :
			visible        = va_arg (args, int);
			redraw_label   = TRUE;
			set_visibility = TRUE;
			break;
	
		    case EDGE_ARROW_LENGTH :
			length            = va_arg (args, int);
			redraw_edge       = TRUE;
			adjust_arrow      = TRUE;
			adjust_box        = TRUE;
			adjust_graph      = TRUE,
			set_arrow_length  = TRUE;
			break;
	
		    case EDGE_ARROW_ANGLE :
			angle            = va_arg (args, double);
			redraw_edge      = TRUE;
			adjust_arrow     = TRUE;
			adjust_box       = TRUE;
			adjust_graph     = TRUE,
			set_arrow_angle  = TRUE;
			break;
	
		    case EDGE_COLOR :
			color            = va_arg (args, int);
			redraw_edge      = TRUE;
			set_color        = TRUE;
			break;
	
		    case RESTORE_IT :
			redraw_edge               = TRUE,
			adjust_head_and_tail      = TRUE;
			adjust_label_text_to_draw = TRUE;
			adjust_elp                = TRUE,
			adjust_graph              = TRUE,
			adjust_box                = TRUE,
			restore_edge              = TRUE;
			break;
		
		    default :
			break;
		}
	
		attr = va_arg (args, Set_attribute);
	}
	
	
	/* Unmark && erase	*/
	if (!restore_edge) {
		set_edge_to_be_marked (edge);
		if (!only_set) {
			if (redraw_edge)
				erase_edge (edge);
			else if (redraw_label)
				erase_edgelabel (edge);
		}
	}
		
	
	/* Let's do It		*/
	
	if (set_line) {
		edge->line = el;
	}
	
	if (set_type) {	
		unuse_edgetype (edge->type);
		edge->type = use_edgetype (edgetype_index);
	}
	
	if (set_label) {
		if (edge->label.text != NULL)
			myfree (edge->label.text);
		if (text == NULL) {
			unuse_font (edge->label.font);
			edge->label.text = NULL;
			edge->label.font = (Graphed_font)NULL;
		} else {
			edge->label.text = text;
			if (edge->label.font == (Graphed_font)NULL)
				edge->label.font = use_font (current_edgefont_index);
		}
	}
	
	if (set_font) {	
		unuse_font (edge->label.font);
		if (font_index != -1) {
			edge->label.font = use_font (font_index);
		}
	}
			
	if (set_visibility) {
		edge->label.visible = visible;
	}
			
	if (set_arrow_length) {
		edge->arrow.length = length;
	}
			
	if (set_arrow_angle) {
		edge->arrow.angle = angle;
	}
	if (set_color) {
		edge->color = color;
	}
			


	/* Adjust it 		*/
	
	if (!only_set) {
		if (adjust_label_text_to_draw)
			adjust_edgelabel_text_to_draw (edge);
		if (adjust_head_and_tail) {
			if (is_single_edgeline (edge->line)) {
				adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL, edge);
			} else {
				adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
				adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
			}
			adjust_arrow_to_edge (edge);
			adjust_edgelabel_position (edge);
		} else {
			if (adjust_arrow)
				adjust_arrow_to_edge (edge);
			if (adjust_elp)
				adjust_edgelabel_position (edge);
		}
		if (adjust_box)
			adjust_edge_box (edge);
		if (adjust_graph)
			adjust_graph_box (edge->source->graph);
	}
	
	/* redraw	*/
	if (!only_set) {
		if (redraw_edge)
			draw_edge (edge);
		else if (redraw_label)
			draw_edgelabel (edge);
	}
	
	set_graph_has_changed (edge->source->graph);
	
	va_end (args);
}



Edge_attributes	get_edge_attributes (edge)
Edge		edge;
{
	Edge_attributes attr;
	
	attr.line = edge->line;
	attr.type_index = get_edgetype_index (edge->type);
	if (edge->label.font != (Graphed_font)NULL)
		attr.font_index = get_font_index (edge->label.font);
	else
		attr.font_index = current_edgefont_index;
	attr.arrow_length      = edge->arrow.length;
	attr.arrow_angle       = edge->arrow.angle;
	attr.label_visibility  = edge->label.visible;
	attr.color             = edge->color;
	attr.label             = strsave (edge->label.text);
	
	return	attr;
}



void		move_edge (edge, el, x,y)
Edge		edge;
Edgeline	el;
int		x,y;
{
	if (is_marked(edge)) do_unmark_edge (edge);
	
	if (is_single_edgeline (edge->line)) {
	
		erase_edge (edge);
		
		set_edgeline_xy (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL, edge);
		adjust_arrow_to_edge (edge);
		adjust_edgelabel_position (edge);
		adjust_edge_box  (edge); /* needed here for draw_edge	*/
		
		draw_edge (edge);
	
	} else if (el == edge->line) {
	
		erase_edgelabel (edge);
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		set_edgeline_xy (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
		adjust_edgelabel_position (edge);
		adjust_edge_box  (edge);
		
		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_edgelabel (edge);
	
	} else if (el == edge->line->suc && el == edge->line->pre->pre) {
	
		erase_arrow (edge);
		erase_edgelabel (edge);
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		set_edgeline_xy (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
		adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
		adjust_edgelabel_position (edge);
		adjust_arrow_to_edge (edge);
		adjust_edge_box  (edge);
		
		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_single_edgeline (edge->source->graph->buffer, el->pre);
		draw_edgelabel (edge);
		draw_arrow (edge);

	} else if (el == edge->line->suc) {
	
		erase_edgelabel (edge);
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		set_edgeline_xy (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
		adjust_edgelabel_position (edge);
		adjust_edge_box  (edge);
		
		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_single_edgeline (edge->source->graph->buffer, el->pre);
		draw_edgelabel (edge);

	} else if (el == edge->line->pre->pre) {
	
		erase_arrow (edge);
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		set_edgeline_xy (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
		adjust_arrow_to_edge (edge);
		adjust_edge_box  (edge);

		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_single_edgeline (edge->source->graph->buffer, el->pre);
		draw_arrow (edge);
	
	} else if (el == edge->line->pre) {
	
		erase_arrow (edge);
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		
		set_edgeline_xy (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
		adjust_arrow_to_edge (edge);
		adjust_edge_box  (edge);

		draw_single_edgeline (edge->source->graph->buffer, el->pre);
		draw_arrow (edge);
	
	} else {
	
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		set_edgeline_xy (el, x,y);
		adjust_edge_box  (edge);
		
		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_single_edgeline (edge->source->graph->buffer, el->pre);
	
	}
	
	adjust_graph_box (edge->source->graph);
	
	if (is_marked(edge)) do_mark_edge (edge);
	
	set_graph_has_changed (edge->source->graph);
}



void		extend_edge (edge, el, x,y)
Edge		edge;
Edgeline	el;
int		x,y;
{
	/* el != edge->line->pre */
	
	if (is_marked(edge)) do_unmark_edge (edge);

	if (el == edge->line) {
	
		int	extending_single_edgeline = is_single_edgeline(el);
		
		erase_edgelabel (edge);
		erase_single_edgeline (edge->source->graph->buffer, el);
		if (extending_single_edgeline)
			erase_arrow (edge);
		
		(void) add_to_edgeline (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
		if (extending_single_edgeline) {
			adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
			adjust_arrow_to_edge (edge);
		}
		adjust_edgelabel_position (edge);

		if (extending_single_edgeline)
			draw_arrow (edge);
		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_single_edgeline (edge->source->graph->buffer, el->suc);
		draw_edgelabel (edge);
	
	} else if (el == edge->line->pre->pre) {
	
		erase_arrow (edge);
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		(void) add_to_edgeline (el, x,y);
		adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
		adjust_arrow_to_edge (edge);

		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_single_edgeline (edge->source->graph->buffer, el->suc);
		draw_arrow (edge);
	
	} else {
	
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		(void) add_to_edgeline (el, x,y);
		
		draw_single_edgeline (edge->source->graph->buffer, el);
		draw_single_edgeline (edge->source->graph->buffer, el->suc);
	
	}

	adjust_edge_box  (edge);
	adjust_graph_box (edge->source->graph);
	
	if (is_marked(edge)) do_mark_edge (edge);
	
	set_graph_has_changed (edge->source->graph);
}


	
void		comprime_edge (edge, el)
Edge		edge;
Edgeline	el;
{
	if (is_single_edgeline (edge->line)) {
		erase_and_delete_edge (edge);
		return;
	}
	
	if (is_marked(edge)) do_unmark_edge (edge);
	
	if (el == edge->line->suc && el == edge->line->pre->pre) {
	
		erase_edge (edge);
		
		el = remove_from_edgeline (el);	
		edge->line = el;
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL, edge);
		adjust_edgelabel_position (edge);
		adjust_arrow_to_edge (edge);
		adjust_edge_box      (edge);
		
		draw_edge (edge);
	
	} else if (el == edge->line) {
		
		erase_single_edgeline (edge->source->graph->buffer, edge->line->suc);
		erase_single_edgeline (edge->source->graph->buffer, edge->line);
		erase_edgelabel (edge);
		
		el =  remove_from_edgeline (el); /* el = el->pre */
		edge->line = el->suc;
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
		adjust_edgelabel_position (edge);

		draw_edgelabel (edge);
		draw_single_edgeline (edge->source->graph->buffer, edge->line);
	
	} else if (el == edge->line->suc) {
				
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_single_edgeline (edge->source->graph->buffer, el);
		erase_edgelabel (edge);
		
		el =  remove_from_edgeline (el);
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
		adjust_edgelabel_position (edge);

		draw_edgelabel (edge);
		draw_single_edgeline (edge->source->graph->buffer, el);
	
	} else if (el == edge->line->pre->pre) {
	
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_single_edgeline (edge->source->graph->buffer, el);
		erase_arrow (edge);
		
		el =  remove_from_edgeline (el);
		adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
		adjust_arrow_to_edge (edge);

		draw_arrow (edge);
		draw_single_edgeline (edge->source->graph->buffer, el);
	
	} else if (el == edge->line->pre) {
	
		erase_single_edgeline (edge->source->graph->buffer, el->pre->pre);
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_arrow (edge);
		
		(void) remove_from_edgeline (el);
		adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
		adjust_arrow_to_edge (edge);

		draw_arrow (edge);
		draw_single_edgeline (edge->line->pre->pre);
		
	} else {
	
		erase_single_edgeline (edge->source->graph->buffer, el->pre);
		erase_single_edgeline (edge->source->graph->buffer, el);
		
		el = remove_from_edgeline (el);

		draw_single_edgeline (edge->source->graph->buffer, el);
	
	}

	adjust_edge_box  (edge);
	adjust_graph_box (edge->source->graph);

	if (is_marked(edge)) do_mark_edge (edge);
	
	set_graph_has_changed (edge->source->graph);
}
/************************************************************************/
/*									*/
/*		    Markierung der Kante verwalten			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	set_edge_marked (edge, marked)				*/
/*									*/
/*	Gueltig sind NOT_MARKED, MARKED_WITH_SQUARES und		*/
/*	TO_BE_MARKED_WITH_SQUARES.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_edge_to_be_marked (edge)				*/
/*									*/
/*	Die Kante wird beim naechsten Neuzeichnen (force_repainting)	*/
/*	des Graphen markiert werden.					*/
/*									*/
/*									*/
/************************************************************************/




void	set_edge_marked (edge, marked)
Edge	edge;
Marked	marked;
{
	switch (marked) {
	     case NOT_MARKED :
		if (is_marked(edge))
			do_unmark_edge (edge);
		edge->marked = marked;
		break;
	    case MARKED_WITH_SQUARES :
	    case MARKED_AT_BOUNDARY  :
		if (is_marked(edge)) {
			do_unmark_edge (edge);
			edge->marked = marked;
			do_mark_edge (edge);
		} else if (is_not_marked(edge)) {
			edge->marked = marked;
			do_mark_edge (edge);
		} else /* to_be_marked (edge) */ {
			edge->marked = marked;
			do_mark_edge (edge);
		}
		break;
	    case TO_BE_MARKED_WITH_SQUARES :
	    case TO_BE_MARKED_AT_BOUNDARY  :
		if (is_marked(edge))
			do_unmark_edge (edge);
		edge->marked = marked;
		break;
	}
}


void	set_edge_to_be_marked (edge)
Edge	edge;
{
	switch (edge->marked) {
	    case NOT_MARKED :
		break;
	    case MARKED_WITH_SQUARES :
		set_edge_marked (edge, TO_BE_MARKED_WITH_SQUARES);
		break;
	    case MARKED_AT_BOUNDARY  :
		set_edge_marked (edge, TO_BE_MARKED_AT_BOUNDARY);
		break;
	    case TO_BE_MARKED_WITH_SQUARES :
	    case TO_BE_MARKED_AT_BOUNDARY  :
		break;
	}
}


/************************************************************************/
/*									*/
/*			EDGELINE-VERWALTUNG				*/
/*									*/
/*		Hier wird wieder nicht gezeichnet !			*/
/*									*/
/************************************************************************/
/*									*/
/*	Eine Kante besteht aus miteinander verbundenen Punkten. Diese	*/
/*	Punkte (=Edgeline) sind in einer doppelt verketteten,		*/
/*	geschlossenen Liste organisiert :				*/
/*									*/
/*	typedef	struct	edgeline {					*/
/*									*/
/*		coord		x,y;					*/
/*		struct edgeline	*pre, *suc;				*/
/*		Rect		box;					*/
/*									*/
/*	}								*/
/*		*Edgeline;						*/
/*									*/
/*	x,y		Koordinaten des Punktes				*/
/*	pre, suc	Vorgaenger- / Nachfolgerpunkt			*/
/*	box		Rechteck um (edgeline, edgeline->suc)		*/
/*			(Bug : der letzte Punkt enthaelt ein Rechteck,	*/
/*			das zusammen mit dem ersten Punkt gebildet	*/
/*			wird, aber keinen Sinn ergibt)			*/
/*									*/
/*	Eine edgeline besteht normalerweise innnerhalb von Kanten	*/
/*	aus mindestens zwei Punkten.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Ebenfalls global verfuegbar ist das Makro			*/
/*									*/
/*	int	is_single_edgeline (el),				*/
/*									*/
/*	das angibt, ob el == el->suc->suc gilt (d.h. el besteht aus	*/
/*	einem oder zwei Punkten).					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Auf die Punkte einer Edgeline el wird i.a. folgendermassen	*/
/*	zugegriffen :							*/
/*									*/
/*	el      = 1. Punkt						*/
/*	el->suc = 2. Punkt						*/
/*	el->pre->pre = vorletzter Punkt					*/
/*	el->pre      = letzter Punkt					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Grundsaetzlich hat eine Edgeline in edge->line mindestens zwei	*/
/*	Punkte. Ein Punkt ist nur dann moeglich, wenn eine Edgeline	*/
/*	sich noch im Aufbau befindet.					*/
/*									*/
/************************************************************************/
/*									*/
/*	Edgeline	new_edgeline    (x,y)				*/
/*									*/
/*	Erzeuge neue Edgeline am Punkt (x,y)				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Edgeline	add_to_edgeline (el_tail, x,y)			*/
/*									*/
/*	Fuegt nach el_tail einen neuen Punkt (x,y) an und gibt diesen	*/
/*	zurueck.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	Edgeline	remove_from_edgeline (el)			*/
/*									*/
/*	Loescht el aus seiner Edgeline. Rueckgabe ist das Element vor	*/
/*	el, falls es noch eine Edgeline gibt, NULL sonst.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_edgeline_xy (el, x,y)				*/
/*									*/
/*	Setzt die Position der Edgeline el auf (x,y). Dazu wird	sowohl	*/
/*	bei el als auch bei el->pre box neu angepasst.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	free_edgeline (el_head)					*/
/*									*/
/*	Loescht die Edgeline (im Speicher). el_head darf		*/
/*	(Edgeline)NULL sein.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	Edgeline	copy_edgeline (el_head)				*/
/*									*/
/************************************************************************/


#define mymalloc_edgeline() (Edgeline)mymalloc(sizeof(struct edgeline))


Edgeline	new_edgeline (x,y)
int		x,y;
{
	register Edgeline	new_el;

	new_el      = mymalloc_edgeline();
	new_el->x   = (coord)x;
	new_el->y   = (coord)y;
	new_el->suc = new_el;
	new_el->pre = new_el;
	rect_construct (&(new_el->box), 0, 0, 0, 0);

	return new_el;
}


Edgeline	add_to_edgeline (el_tail, x, y)
Edgeline	el_tail;
int		x,y;
{
	register Edgeline	new_el;

	new_el = new_edgeline(x,y);
	if (el_tail != (Edgeline)NULL) {
		new_el->suc      = el_tail->suc;
		new_el->pre      = el_tail;
		new_el->pre->suc = new_el;
		new_el->suc->pre = new_el;
	}
	
	set_edgeline_xy (new_el, x,y);
	
	return new_el;
}


Edgeline	remove_from_edgeline (el)
Edgeline	el;
{
	if (el == (Edgeline)NULL)
		return (Edgeline)NULL;
	else if (el->suc == el) {
		myfree (el);
		return (Edgeline)NULL;
	} else {
		Edgeline	el_pre = el->pre;
		
		el->pre->suc = el->suc;
		el->suc->pre = el->pre;
		set_edgeline_xy (el->suc, el->suc->x, el->suc->y);
		myfree (el);
		
		return el_pre;
	}
}


void		set_edgeline_xy (el, x, y)
Edgeline	el;
int		x,y;
{
	int	x0,y0,
		x2,y2;
	
	edgeline_x (el) = x;
	edgeline_y (el) = y;
	
	/* Justiere die box bei el->pre, el neu				*/
	/* BUGS : die (virtuelle) Rueckkante vom Ende zum Anfang wird	*/
	/* ebenfalls gesetzt. Besteht die Kante nur aus einem Punkt (?)	*/
	/* so macht die Prozedur Mist (aber dann ist sowieso etwas	*/
	/* nicht in Ordnung).						*/
	
	x0 = edgeline_x (el->pre);
	y0 = edgeline_y (el->pre);
	x2 = edgeline_x (el->suc);
	y2 = edgeline_y (el->suc);
	
	rect_construct (&(el->pre->box),
	                iif (x0<x, x0, x), iif (y0<y, y0 ,y),     
	                abs (x0-x) + 1,    abs(y0-y) + 1
	               );
	rect_construct (&(el->box),
	                iif (x2<x, x2, x), iif (y2<y, y2, y),     
	                abs (x2-x) + 1,    abs (y2-y) + 1
	               );

}


void		free_edgeline (el_head)
Edgeline	el_head;
{
	register Edgeline	el = el_head,
				el_next = el;

	if (el != (Edgeline)NULL)
	do {
		el_next = el->suc;
		myfree(el);
		el = el_next;
	} while (el != el_head);
}


Edgeline	copy_edgeline (el_head)
Edgeline	el_head;
{
	Edgeline	el, new_el, new_el_head;
	
	new_el_head = new_el = (Edgeline)NULL;
	for_edgeline (el_head, el) {
		if (new_el_head == (Edgeline)NULL)
			new_el = new_el_head = new_edgeline (
				edgeline_x(el), edgeline_y(el));
		else
			new_el = add_to_edgeline (new_el,
				edgeline_x(el), edgeline_y(el));
	} end_for_edgeline (el_head, el);
	
	return new_el_head;
}
/************************************************************************/
/*									*/
/*		BENUTZERSPEZIFISCHE ATTRIBUTE VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_edge_attributes (edge, create_edge_attr_proc)	*/
/*	void	push_edge_attributes   (edge)				*/
/*	void	pop_edge_attributes    (edge, destroy_edge_attr_proc)	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	char	*create_edge_attr_proc (edge);				*/
/*	void	destroy_edge_attr_proc (edge);				*/
/*									*/
/************************************************************************/



void	create_edge_attributes (edge, create_edge_attr_proc)
Edge	edge;
char	*(*create_edge_attr_proc)();
{
	edge->attr = create_edge_attr_proc (edge);
}


void	push_edge_attributes (edge)
Edge	edge;
{
	Attr_stack	new_stack;
	
	new_stack = (Attr_stack) mymalloc (sizeof(struct attr_stack));
	new_stack->attr = edge->attr;
	new_stack->next = iif (edge->attr_stack != (Attr_stack)NULL,
	                       edge->attr_stack->next,
	                       (Attr_stack)NULL);
	
	edge->attr = NULL;
	edge->attr_stack = new_stack;
}


void	pop_edge_attributes (edge, destroy_edge_attr_proc)
Edge	edge;
void	(*destroy_edge_attr_proc)();
{
	destroy_edge_attr_proc (edge);
	edge->attr = NULL;
	
	if (edge->attr_stack != NULL && edge->attr_stack->attr != NULL) {
		
		Attr_stack	top = edge->attr_stack;
		
		edge->attr       = top->attr;
		edge->attr_stack = top->next;
		
		myfree (top);
	}
}
