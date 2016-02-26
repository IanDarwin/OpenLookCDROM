/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				find.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt Prozeduren, mit denen es moeglich ist,	*/
/*	Objekte, die der Benutzer angeklickt hat, ausfindig zu machen.	*/
/*	Ausserden befindet sich hier die Verwaltung der Datenstruktur	*/
/*	"Picklist".							*/
/*									*/
/************************************************************************/


#include "misc.h"
#include "graph.h"
#include "group.h"
#include "find.h"
#include "paint.h"
#include "graphed_subwindows.h"



/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Picklist	picker (x,y)					*/
/*	Picklist	xpicker (x,y)					*/
/*									*/
/*	Node	node_finder (x,y, which)				*/
/*	Edge	edge_finder (x,y, which)				*/
/*	Node	find_node_containing_point (x,y)			*/
/*									*/
/*	Picked_point_of_edgeline					*/
/*		find_picked_point_of_edgeline (el, x,y)			*/
/*	Picked_point_of_node						*/
/*		find_picked_point_of_node (node, x,y)			*/
/*									*/
/*	Picklist	new_picklist    (what, node/edge/group)		*/
/*	Picklist	add_to_picklist (pl, what, node/edge/group)	*/
/*	void		free_picklist   (pl)				*/
/*									*/
/*	int	picklist_contains_object (picklist, picked_object)	*/
/*	void	mark_picked_object   (pl)				*/
/*	void	unmark_picked_object (pl)				*/
/*	Rect	compute_bounding_rect_of_picklist (pl)			*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	int	pick_gap = PICK_GAP;



/************************************************************************/
/*									*/
/*			LOKALE FUNKTIONEN				*/
/*									*/
/************************************************************************/


int	point_is_near_edgeline ();


/************************************************************************/
/*									*/
/*			NAMENSKONVENTIONEN				*/
/*									*/
/*======================================================================*/
/*									*/
/*	..._finder (x,y, which)		Sucht nach einem Knoten / einer	*/
/*		Kante an der Stelle (x,y). which gibt an, ob erstmalig	*/
/*		hier gesucht werden soll (FIND_FIRST) oder die letzte	*/
/*		Suche hier fortgesetzt werden soll (FIND_NEXT) (der	*/
/*		Graph darf sich natuerlich waehrenddessen nicht		*/
/*		aendern !)						*/
/*	find_picked_point_of_... (..., x,y)	Gibt an, welcher	*/
/*		Punkt einer Kante oder eines Knoten angeklickt ist.	*/
/*	picker (x,y, mode)		ist die zentrale Funktion	*/
/*		dieses Moduls. Sie gibt alle Objekte zurueck, die an	*/
/*		der Stelle (x,y) liegen. Mit mode kann die Suche evtl.	*/
/*		auf Knoten oder Kanten beschraenkt werden.		*/
/*									*/
/************************************************************************/
/*									*/
/*			DIE DATENSTRUKTUR PICKLIST			*/
/*									*/
/*======================================================================*/
/*									*/
/*	typedef	enum {							*/
/*		NODE_PICKED, EDGE_PICKED, GROUP_PICKED			*/
/*	}								*/
/*		What_is_picked;						*/
/*									*/
/*	typedef	union {							*/
/*		Node	node;						*/
/*		Edge	edge;						*/
/*		Group	group;						*/
/*	}								*/
/*		Which_is_picked;					*/
/*									*/
/*	typedef	struct	picklist {					*/
/*		What_is_picked	what;					*/
/*		Which_is_picked	which;					*/
/*		struct picklist	*next;					*/
/*	}								*/
/*		*Picklist;						*/
/*									*/
/*	Eine Picklist ist eine Liste von Elementen, die Knoten oder	*/
/*	Kanten (what & which) sein koennen. Das Ende der Liste wird	*/
/*	durch NULL in next angezeigt.					*/
/*									*/
/************************************************************************/
/*									*/
/*	    ALLGEMEINES ZUR VORGEHENSWEISE BEIM AUFSUCHEN VON		*/
/*			ANGEKLICKTEN OBJEKTEN				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Seien (x,y) die Koordinaten, an denen die Maus beim Klick	*/
/*	steht.								*/
/*									*/
/*	Ein Knoten gilt als angeklickt, wenn der Punkt (x,y) im		*/
/*	Inneren von node->box liegt.					*/
/*									*/
/*	Eine Kante gilt als angeklickt, wenn der Punkt (x,y) in		*/
/*	hoechstens pick_gap Distanz in Richtung der x- oder y-Achse	*/
/*	von einem Punkt von edge->line entfernt liegt.			*/
/*	Das ergibt folgendes Distanzmass :				*/
/*		dist ((x1,y1),(x2,y2))  = min (|x2-x1|,|y2-y1|)		*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*		KNOTEN / KANTE(N) AM PUNKT (X,Y) SUCHEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	Picklist	picker (x,y, mode)				*/
/*									*/
/*	Sucht alle Objekte an Position (x,y). mode hat folgende		*/
/*	Bedeutung :							*/
/*	- PICK_NODE         : Picke nur Knoten auf			*/
/*	- PICK_EDGE         : Picke nur Kanten auf			*/
/*	- PICK_NODE_OR_EDGE : Picke Knoten oder Kanten auf		*/
/*									*/
/*======================================================================*/
/*	Picklist	xpicker (x,y, mode)				*/
/*									*/
/*	Analog picker, aber alle linken Seiten von Produktionen werden	*/
/*	nicht berueckischtigt, es sei denn, es gibt an (x,y) nur linke	*/
/*	Seiten von Produktionen. 					*/
/*	Resultat ist also entweder eine Liste, in der alle linken	*/
/*	Seiten entfernt worden sind, oder aber eine nur aus linken	*/
/*	Seiten bestehende Liste.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Node	node_finder (x,y, which)				*/
/*	Edge	edge_finder (x,y, which)				*/
/*									*/
/*	Mit diesen Funktionen koennen sukzessive Knoten bzw. Kanten am	*/
/*	Punkt(x,y) gesucht werden.					*/
/*	Ist which == FIND_FIRST, so wird eine neue Suche gestartet,	*/
/*	fuer which == FIND_NEXT die letzte fortgesetzt.			*/
/*	ACHTUNG : Der Graph sollte innerhalb einer Suche mit mehreren	*/
/*	Aufrufen von ..._finder nicht veraendert werden, da sich	*/
/*	node_finder und edge_finder die Stelle, an der die letzte Suche	*/
/*	geendet hat, merken !						*/
/*									*/
/*======================================================================*/
/*									*/
/*	Node	find_node_containing_point (x,y)			*/
/*									*/
/*	Sucht einen Knoten, der (x,y) enthaelt und EINDEUTIG ist.	*/
/*	Kann kein bzw. kein eindeutiger Knoten gefunden werden, so	*/
/*	wird empty_node zurueckgegeben.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	int	point_is_near_edgeline (el, point_x,point_y,	*/
/*			                            gap_x, gap_y)	*/
/*									*/
/*	Hilfsfunktion, um festzustellen, ob (point_x,point_y) in 	*/
/*	hoechstens pick_gap Entfernung von einem Punkt von el liegt.	*/
/*	Dazu wird getestet, ob ein Punkt eines Stuecks von el mit x-	*/
/*	(y-) Koordinate point_x (point_y) in y- (x-) Richtung		*/
/*	hoechstens eine Distanz pick_gap zu (x,y) hat oder einer der	*/
/*	beiden Endpunkte in beiden Richtungen nicht weiter als pick_gap	*/
/*	von (point_x,point_y) entfernt liegt. Distanzmass :		*/
/*		dist ((x1,y1),(x2,y2))  = min (|x2-x1|,|y2-y1|)		*/
/*									*/
/************************************************************************/



Picklist	picker (x,y, mode)
int		x,y;
Pick_mode	mode;
{
	Picklist	pl_head = empty_picklist,
			pl      = empty_picklist;
	Node		node = empty_node;
	Edge		edge = empty_edge;
	Find_which	first_or_next;
	Which_is_picked	which;
	

	if (mode == PICK_NODE || mode == PICK_NODE_OR_EDGE) {
		first_or_next = FIND_FIRST;
		do {
			node = node_finder (x,y, first_or_next);
			if (node != empty_node) {
				which.node = node;
				if (pl_head == empty_picklist)
					pl_head = pl = new_picklist (NODE_PICKED, which.node);
				else
					pl = add_to_picklist (pl, NODE_PICKED, which.node);

			}
			first_or_next = FIND_NEXT;
		
		} while (node != empty_node);
	}
	if (mode == PICK_EDGE || mode == PICK_NODE_OR_EDGE) {
		first_or_next = FIND_FIRST;
		do {
			edge = edge_finder (x,y, first_or_next);
			if (edge != empty_edge) {
				which.edge = edge;
				if (pl_head == empty_picklist)
					pl_head = pl = new_picklist (EDGE_PICKED, which.edge);
				else
					pl = add_to_picklist (pl, EDGE_PICKED, which.edge);
			}
			
			first_or_next = FIND_NEXT;
		} while (edge != empty_edge);
	}
	
	return pl_head;
}



Picklist	xpicker (x,y, mode)
int		x,y;
Pick_mode	mode;
{
	Picklist	pl_head = empty_picklist,
			pl      = empty_picklist;
	int		all_picked_objects_are_left_sides;
	
	
	pl_head = picker (x,y, mode);
	
	/* Check wheter all objects in pl_head are left sides	*/
	/* of productions.					*/
	all_picked_objects_are_left_sides = TRUE;
	for (pl = pl_head; pl != empty_picklist; pl = pl->suc)
		if (pl->what != NODE_PICKED  || !is_left_side_of_production (pl->which.node)) {
			all_picked_objects_are_left_sides = FALSE;
			break;
	}
	
	if (!all_picked_objects_are_left_sides && pl_head != empty_picklist) {
	    
	    	/* The list contains more than one element -	*/
	    	/* remove all left sides of productions. The	*/
	    	/* list contains shurely other objects.		*/
	    	
		pl_head = remove_left_side_of_productions_from_picklist (pl_head);
		
	} else {
		/* O.K., leave it as it is. If nonempty, the	*/
		/* list consists either totally of left sides	*/
		/* or of exactly one object, which may also be	*/
		/* a left side.					*/
	}
	
	return pl_head;
}



Node		node_finder (x,y, which)
int		x,y;
Find_which	which;
{
	register Graph	graph;
	register Node	node;
	Graph		found_graph = empty_graph;
	Node		found_node  = empty_node;
	static   Graph	last_found_graph;
	static   Node	last_found_node;
	Graph		graphs = buffers[wac_buffer].graphs;
	
	
	if (graphs == empty_graph) {
		graph = empty_graph;
		node  = empty_node;
	} else if (which == FIND_FIRST)  {
		graph = graphs;
		node  = graph->firstnode;
	} else /* == FIND_NEXT */ {
		if (last_found_graph == empty_graph || last_found_node  == empty_node) {
			graph = empty_graph;
			node  = empty_node;
		} else if (last_found_node->suc == last_found_graph->firstnode) {
			if (last_found_graph->suc == graphs) {
				/* last node && graph	*/
				graph = empty_graph;
				node  = empty_node;
			} else {
				/* next graph		*/
				graph = last_found_graph->suc;
				node  = graph->firstnode;
			}
		} else {
			/* next node, same graph	*/
			graph = last_found_graph;
			node  = last_found_node->suc;
		}
		/* @?&#! if graph has changed since last search	*/
	}
	
	if (graph != empty_graph) do {
		if (rect_includespoint (&(graph->box), x,y) && node != empty_node) do {
			if (rect_includespoint (&(node->box), x,y)) {
				found_graph = graph;
				found_node  = node;
				goto found;
			}
			node = node->suc;
		} while (node != graph->firstnode);
		graph = graph->suc;
		node  = graph->firstnode;
	} while (graph != graphs);
	
found :	/* bad but fast */
	last_found_node  = found_node;
	last_found_graph = found_graph;
	
	return found_node;
}



Edge		edge_finder (x,y, which)
int		x,y;
Find_which	which;
{
	register Graph	graph;		/* Graphen zum Durchlaufen	*/
	register Node	node;		/* Knoten zum Durchlaufen	*/
	register Edge	edge;		/* aus sourcelist von node	*/
	Graph		found_graph     = empty_graph;
	Node		found_node      = empty_node;
	Edge		found_edge      = empty_edge;
	Rect		box;
	static	Graph	last_found_graph = empty_graph;
	static	Node	last_found_node  = empty_node;
	static	Edge	last_found_edge  = empty_edge;
	Graph		graphs;
	
	
	graphs = buffers[wac_buffer].graphs;
	
	if (graphs == empty_graph) {
		graph = empty_graph;
		node  = empty_node;
		edge  = empty_edge;
	} else if (which == FIND_FIRST) {
		graph = graphs;
		node  = graph->firstnode;
		edge  = iif (node != empty_node, node->sourcelist, empty_edge);
	} else /* == FIND_NEXT */ {
		if (last_found_graph == empty_graph ||
		    last_found_node  == empty_node  ||
		    last_found_edge  == empty_edge)  {
			/* Nothing found in last search ! (?)	*/
			graph = empty_graph;
			node  = empty_node;
			edge  = empty_edge;
		} else if (last_found_edge->sourcesuc == last_found_node->sourcelist) {
			/* last edge in sourcelist */
			if (last_found_node->suc == last_found_graph->firstnode) {
				/* last node && last edge */
				if (last_found_graph->suc == graphs) {
					/* last node && last edge && last graph	*/
					graph = empty_graph;
					node  = empty_node;
					edge  = empty_edge;
				} else {
					/* next graph	*/
					graph = last_found_graph->suc;
					node  = graph->firstnode;
					edge  = iif (node != empty_node, node->sourcelist, empty_edge);
				}
			} else {
				/* next node, same graph */
				graph = last_found_graph;
				node  = last_found_node->suc;
				edge  = node->sourcelist;
			}
		} else {
			/* next edge, same graph && node */
			graph = last_found_graph;
			node  = last_found_node;
			edge  = last_found_edge->sourcesuc;
		}
	}
	
	
	if (graph != empty_graph) do {
		if (node != empty_node) do {
			if (edge != empty_edge) do {
				box = edge->box;
				rect_marginadjust (&box, MARKER_SQUARE_SIZE/2);
				if (rect_includespoint (&box, x,y) &&
				    point_is_near_edgeline (edge->line, x,y, (int)pick_gap, (int)pick_gap)) {
					found_graph = graph;
					found_node  = node;
					found_edge  = edge;
					goto found;
				}
				edge = edge->sourcesuc;
			} while (edge != node->sourcelist);
			node = node->suc;
			edge = node->sourcelist;
		} while (node != graph->firstnode);
		graph = graph->suc;
		node  = graph->firstnode;
		edge  = iif (node != empty_node, node->sourcelist, empty_edge);
	} while (graph != graphs);
	
found :
	last_found_graph = found_graph;
	last_found_node  = found_node;
	last_found_edge  = found_edge;
	
	return found_edge;
}



Node	find_node_containing_point (x,y)
int	x,y;
{
	Node	node;
	
	node = node_finder (x,y, FIND_FIRST);
	if (node_finder(x,y, FIND_NEXT) != empty_node)
		return empty_node;
	else
		return node;
}



int			point_is_near_edgeline (el, point_x, point_y, gap_x, gap_y)
register Edgeline	el;
int			point_x, point_y;
int			gap_x,   gap_y;
{
	double		x1, y1, x2, y2, ax, ay,
			fx = (double)point_x,	/* Gleitkomma-Versionen	*/
			fy = (double)point_y;	/* von point_x, point_y	*/
	double		fpick_gap_x = gap_x;
	double		fpick_gap_y = gap_y;
	Edgeline	el_head;
	Rect		box;

	el_head = el;
	if (el != (Edgeline)NULL) {
	
		/* Geradengleichung : x = x1 + ay*(x2-x1), wobei	*/
		/* ay = (fy-y1)/(y2-y1) fuer y1 != y2, -1.0 sonst.	*/
		/* Fuer 0.0 <= ay <= 1.0 liegt der Punkt (x,fy) auf der	*/
		/* Strecke (x1,y1) - (x2,y2).				*/
		/* Analog : y = y1 + ax*(y2-y1);			*/
		/* Die Routine kontrolliert, ob die Strecke (x1,y1) -	*/
		/* (x2,y2)						*/
		/* - einen Punkt (fx,*) hat und ob dieser von (fx,fy)	*/
		/*   in y-Richtung weniger als pick_gap entfernt ist	*/
		/* - einen Punkt (*,fy) hat und ob dieser von (fx,fy)	*/
		/*   in x-Richtung weniger als pick_gap entfernt ist	*/
		/* - oder (fx,fy) von (x1,y1) um weniger als		*/
		/*   (pick_gap,pick_gap) entfernt ist			*/
		/* - oder (fx,fy) von (x2,y2) um weniger als		*/
		/*   (pick_gap,pick_gap) entfernt ist			*/
		/* Ist einer dieser Punkte erfuellt, so liegt		*/
		/* (x1,y1) - (x2,y2) nahe genug an (fx,fy).		*/
		/* Fuer den Spezialfall x1 == x2 (bzw. y1 == y2) wird	*/
		/* die erste (bzw. zweite) Bedingung als nicht erfuellt	*/
		/* angesehen (daher ax == -1.0 bzw. ay == -1.0).	*/

		x1   = el->x;
		y1   = el->y;
		el = el->suc;
		do {
			box = el->pre->box;
			rect_marginadjust (&box, pick_gap);
			if (rect_includespoint(&box, point_x,point_y)) {
				x2 = el->x;
				y2 = el->y;
				ax = iif (x1 != x2, (fx-x1)/(x2-x1), -1.0);
				ay = iif (y1 != y2, (fy-y1)/(y2-y1), -1.0);
				if ( ( (ay >= 0.0) && (ay <= 1.0) && (fabs(fx-(x1+ay*(x2-x1))) <= fpick_gap_x) ) ||
				     ( (ax >= 0.0) && (ax <= 1.0) && (fabs(fy-(y1+ax*(y2-y1))) <= fpick_gap_y) ) ||
				     ( (fabs(fx-x1) <= fpick_gap_x) && (fabs(fy-y1) <= fpick_gap_y) )            ||
				     ( (fabs(fx-x2) <= fpick_gap_x) && (fabs(fy-y2) <= fpick_gap_y) ) )
					return TRUE;
			}
			x1 = el->x;
			y1 = el->y;
			el = el->suc;
		} while (el != el_head);
	}
	return FALSE;
}
/************************************************************************/
/*									*/
/*	WELCHER PUNKT EINES KNOTENS / EINER KANTE IST ANGEKLICKT ?	*/
/*									*/
/************************************************************************/
/*									*/
/*	Picked_point_of_edgeline					*/
/*		find_picked_point_of_edgeline (el, x,y)			*/
/*	Picked_point_of_node						*/
/*		find_picked_point_of_node     (node, x,y)		*/
/*									*/
/*	Diese beiden Funktionen untersuchen genauer, welcher Teil	*/
/*	eines Knotens bzw. einer Kante angeklickt ist.			*/
/*	Sie beziehen sich auf die Markierungsquadrate, die mit		*/
/*	mark_node bzw. mark_edge (siehe mark_picked_object unten oder	*/
/*	das Modul draw.c) angebracht werden.				*/
/*	ACHTUNG : in find_picked_point_of_edgeline wird - entgegen	*/
/*	aelteren C-Konventionen - ein kompletter struct als Resultat	*/
/*	zurueckgegeben !						*/
/*									*/
/*	Deklarationen :							*/
/*									*/
/*	typedef	enum {							*/
/*		NO_NODE_POINT_PICKED,					*/
/*		UPPER_LEFT_POINT_PICKED,	Diese Angaben beziehen	*/
/*		UPPER_RIGHT_POINT_PICKED,	sich auf node->box !	*/
/*		LOWER_LEFT_POINT_PICKED,				*/
/*		LOWER_RIGHT_POINT_PICKED				*/
/*	}								*/
/*		Picked_point_of_node;					*/
/*									*/
/*	typedef struct {						*/
/*		enum {							*/
/*			NO_EDGE_POINT_PICKED,				*/
/*			REAL_POINT_PICKED,				*/
/*			IMAGINARY_POINT_PICKED				*/
/*		} what;							*/
/*		union {							*/
/*			struct {					*/
/*				Edgeline	el;			*/
/*			}						*/
/*				real_point;				*/
/*			struct {					*/
/*				Edgeline	el;			*/
/*				int		x,y;			*/
/*			}						*/
/*				imaginary_point;			*/
/*		} which;						*/
/*	}								*/
/*		Picked_point_of_edgeline;				*/
/*									*/
/*	Erlaeuterung :							*/
/*	- real_point = ein real existierender Eckpunkt, naemlich	*/
/*	  (which.el->x, which.el->y).					*/
/*	- imaginary_point = ein Punkt, der genau in der Mitte zwischen	*/
/*	  which.el und which.e->suc liegt, naemlich (which.x, which.y).	*/
/*	Dieser Unterschied wird auch beim Markieren (->draw.c)		*/
/*	deutlich : "reale" Punkte werden mit schwarzen Quadraten,	*/
/*	"imaginaere" Punkte mit Rechtecken markiert.			*/
/*									*/
/************************************************************************/



Picked_point_of_node	find_picked_point_of_node (node, x,y)
Node	node;
int	x,y;
{
	int    x1 = node_left(node) + MARKER_SQUARE_SIZE/2;
	int    y1 = node_top(node) + MARKER_SQUARE_SIZE/2;
	int    x2 = node_left(node) + node_width(node) - MARKER_SQUARE_SIZE/2;
	int    y2 = node_top(node) + node_height(node) - MARKER_SQUARE_SIZE/2;
	
#	define try(x1,y1, x2,y2) \
	       (abs ((x1) - (x2)) <= MARKER_SQUARE_SIZE/2 && \
	        abs ((y1) - (y2)) <= MARKER_SQUARE_SIZE/2 )
	
	if (try (x1,y1, x,y))
		return UPPER_LEFT_POINT_PICKED;
	else if (try (x2,y1, x,y))
		return UPPER_RIGHT_POINT_PICKED;
	else if (try (x1,y2, x,y))
		return LOWER_LEFT_POINT_PICKED;
	else if (try (x2,y2, x,y))
		return LOWER_RIGHT_POINT_PICKED;
	else
		return NO_NODE_POINT_PICKED;

#undef	try
}



Picked_point_of_edgeline	find_picked_point_of_edgeline (el, x,y)
Edgeline	el;
int		x,y;
{
	Picked_point_of_edgeline	result;
	Edgeline	el_head = el;
#	define try(x1,y1, x2,y2) \
	       (abs ((x1) - (x2)) <= MARKER_SQUARE_SIZE/2 && \
	        abs ((y1) - (y2)) <= MARKER_SQUARE_SIZE/2 )
	
	result.what = NO_EDGE_POINT_PICKED;
	do {
		if (try (el->x, el->y, x,y)) {
			result.what = REAL_POINT_PICKED;
			result.which.real_point.el = el;
			return result;
		}
		if (try ((el->x + el->suc->x)/2, (el->y + el->suc->y)/2, x, y)) {
			result.what = IMAGINARY_POINT_PICKED;
			result.which.imaginary_point.el = el;
			result.which.imaginary_point.x  = (el->x + el->suc->x) / 2;
			result.which.imaginary_point.y  = (el->y + el->suc->y) / 2;
			return result;
		}
		el = el->suc;
	} while ( el != el_head);
	
	return result;

#undef	try
}


/************************************************************************/
/*									*/
/*		DATENSTRUKTUR PICKLIST VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*			Datenstruktur siehe oben			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Picklist	pl;						*/
/*	What_is_picked	what;						*/
/*	Which_is_picked	which;						*/
/*									*/
/*======================================================================*/
/*									*/
/*	Picklist	new_picklist (what, node/edge/group)		*/
/*									*/
/*	Erzeugt ein neues Element mit Eintraegen what, which.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Picklist	add_to_picklist (pl, what, node/edge/group)	*/
/*									*/
/*	Haengt nach pl ein neuen Element mit Eintraegen what, which	*/
/*	an (kein Einfuegen wie add_to_edgeline !).			*/
/*	Rueckgabe ist ein Zeiger auf das neue Element.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	free_picklist (pl)					*/
/*									*/
/*	Loescht die GESAMTE Liste pl.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	picklist_contains_object (picklist, picked_object)	*/
/*									*/
/*	Prueft nach, ob das EINZELOBJEKT picked_object (pl->suc nicht	*/
/*	beachtet !) in picklist enthalten ist.				*/
/*									*/
/************************************************************************/



Picklist	new_picklist (va_alist)
va_dcl
{
	Picklist	new_pl;
	What_is_picked	what;
	Which_is_picked	which;
	va_list		args;

	va_start (args);
	what  = va_arg (args, What_is_picked);
	switch (what) {
	    case NODE_PICKED :
		which.node = va_arg (args, Node);
		break;
	    case EDGE_PICKED :
		which.edge = va_arg (args, Edge);
		break;
	    case GROUP_PICKED :
		which.group = va_arg (args, Group);
		break;
	}
	va_end (args);
	
	new_pl = (Picklist)mymalloc (sizeof(struct picklist));
	new_pl->pre   = empty_picklist;
	new_pl->suc   = empty_picklist;
	new_pl->what  = what;
	new_pl->which = which;
	
	return new_pl;
}


Picklist	add_to_picklist (va_alist)
va_dcl
{
	Picklist	new_pl;
	Picklist	pl;
	What_is_picked	what;
	Which_is_picked	which;
	va_list		args;

	va_start (args);
	pl    = va_arg (args, Picklist);
	what  = va_arg (args, What_is_picked);
	switch (what) {
	    case NODE_PICKED :
		which.node = va_arg (args, Node);
		new_pl = new_picklist (what, which.node);
		break;
	    case EDGE_PICKED :
		which.edge = va_arg (args, Edge);
		new_pl = new_picklist (what, which.edge);
		break;
	    case GROUP_PICKED :
		which.group = va_arg (args, Group);
		new_pl = new_picklist (what, which.group);
		break;
	}
	va_end (args);
		
	if (pl != empty_picklist) {
	
		new_pl->pre = pl;
		new_pl->suc = pl->suc;
		
		if (new_pl->pre != empty_picklist) new_pl->pre->suc = new_pl;
		if (new_pl->suc != empty_picklist) new_pl->suc->pre = new_pl;
	}
	
	return new_pl;
}


Picklist	remove_from_picklist (pl)
Picklist	pl;
{
	if (pl == empty_picklist)
		return empty_picklist;
	else if (pl->suc == pl) {
		myfree (pl);
		return empty_picklist;
	} else {
		Picklist	pl_to_return;
		
		pl_to_return = iif (pl->pre != empty_picklist, pl->pre, pl->suc);
		
		if (pl->pre != empty_picklist) pl->pre->suc = pl->suc;
		if (pl->suc != empty_picklist) pl->suc->pre = pl->pre;
		
		myfree (pl);
		
		return pl_to_return;
	}
}


void		free_picklist (pl)
Picklist	pl;
{
	Picklist	next;
	
	while (pl != empty_picklist) {
		next = pl->suc;
		myfree (pl);
		pl = next;
	}
}



Picklist	picklist_contains_object (picklist, picked_object)
Picklist	picklist;
Picklist	picked_object;
{
	Picklist	pl;
	int		contains = FALSE;
	Picklist	contained_object = empty_picklist;
	
	pl = picklist;
	while (pl != empty_picklist && !contains) {
		if (pl->what == picked_object->what) switch (pl->what) {
		    case NODE_PICKED :
			contains = (pl->which.node == picked_object->which.node);
			contained_object = pl;
			break;
		    case EDGE_PICKED :
			contains = (pl->which.edge == picked_object->which.edge);
			contained_object = pl;
			break;
		    case GROUP_PICKED :
			contains = (pl->which.group == picked_object->which.group);
			contained_object = pl;
			break;
		} else if (pl->what == GROUP_PICKED && picked_object->what == NODE_PICKED) {
			contains = (contains_group_node (pl->which.group, picked_object->which.node) != empty_group);
			contained_object = pl;
		} else if (pl->what == NODE_PICKED && picked_object->what == GROUP_PICKED) {
			contains = (contains_group_node (picked_object->which.group, pl->which.node) != empty_group);
			contained_object = pl;
		} else {
			contains = FALSE;
			contained_object = empty_picklist;
		}

		pl = pl->suc;
	}
	
	return iif (contains, contained_object, empty_picklist);
}



Picklist	remove_left_side_of_productions_from_picklist (picklist)
Picklist	picklist;
{
	Picklist	pl = picklist, pl_suc, pl_head = picklist;
	
	while (pl != empty_picklist) {
	
		pl_suc = pl->suc;
		
		if (pl->what == NODE_PICKED && is_left_side_of_production (pl->which.node)) {
			if (pl_head == pl)
				pl_head = pl_suc;
			pl = remove_from_picklist (pl);
		}
		
		pl = pl_suc;
	}
	
	return pl_head;
}
/************************************************************************/
/*									*/
/*			HILFSPROZEDUREN					*/
/*									*/
/************************************************************************/
/*									*/
/*	void	mark_picked_object   (pl)				*/
/*	void	unmark_picked_object (pl)				*/
/*									*/
/*	Markiert das ERSTE Objekt in pl (pl->suc wird nicht beachtet).	*/
/*									*/
/*======================================================================*/
/*									*/
/*	Rect	compute_bounding_box_of_picklist (pl)			*/
/*									*/
/*	Berechnet das pl umfassende Rechteck.				*/
/*									*/
/************************************************************************/



void		mark_picked_object (pl)
Picklist	pl;
{
	Group	g;
	
	switch (pl->what) {
	    case NODE_PICKED :
		set_node_marked (pl->which.node, MARKED_WITH_SQUARES);
		break;
	    case EDGE_PICKED :
		set_edge_marked (pl->which.edge, MARKED_WITH_SQUARES);
		break;
	    case GROUP_PICKED :
		for_group (pl->which.group, g) {
			set_node_marked (g->node, MARKED_AT_BOUNDARY);
		} end_for_group (pl->which.group, g);
		break;
	}
}


void		unmark_picked_object (pl)
Picklist	pl;
{
	Group	g;
	Group	edited_group;
	Node	edited_node;
	
	/* If the object was edited, re-mark it	*/
	switch (pl->what) {
	    case NODE_PICKED :
		edited_group = get_currently_edited_group ();
		edited_node  = get_currently_edited_node ();
		if (pl->which.node != edited_node &&
		    !contains_group_node (pl->which.node, edited_group))
			set_node_marked (pl->which.node, NOT_MARKED);
		break;
	    case EDGE_PICKED :
		if (pl->which.edge != get_currently_edited_edge())
			set_edge_marked (pl->which.edge, NOT_MARKED);
		break;
	    case GROUP_PICKED :
		edited_group = get_currently_edited_group ();
		edited_node  = get_currently_edited_node ();
		for_group (pl->which.group, g) {
		    if (g->node != edited_node &&
		        !contains_group_node (g->node, edited_group))
			    set_node_marked (g->node, NOT_MARKED);
		} end_for_group (pl->which.group, g);
		break;
	}
}



Rect		compute_bounding_rect_of_picklist (pl)
Picklist	pl;
{
	Rect	rect_around_picklist;
	Group	g;
	
	rect_around_picklist = rect_null;
	while (pl != empty_picklist) {
		switch (pl->what) {
		    case NODE_PICKED :
			rect_around_picklist = rect_bounding (&rect_around_picklist, &(pl->which.node->box));
			break;
		    case EDGE_PICKED :
			rect_around_picklist = rect_bounding (&rect_around_picklist, &(pl->which.edge->box));
			break;
		    case GROUP_PICKED :
			for_group (pl->which.group, g) {
				rect_around_picklist = rect_bounding (&rect_around_picklist, &(g->node->box));
			} end_for_group (pl->which.group, g);
			break;
		}
		pl = pl->suc;
	}
	
	return rect_around_picklist;
}
