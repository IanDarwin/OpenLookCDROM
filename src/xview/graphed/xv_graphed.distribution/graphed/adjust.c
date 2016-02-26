/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				adjust.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt Prozeduren fuer das automatische		*/
/*	Anpassen u.a. von						*/
/*	- Kanten an Knoten						*/
/*	- Label an Knoten und Kanten					*/
/*	- Pfeile an Kanten						*/
/*	- Neujustieren von Knoten nach Formaenderung			*/
/*									*/
/************************************************************************/


#include "misc.h"
#include "graph.h"
#include "draw.h"
#include "adjust.h"
#include "type.h"
#include "group.h"

#include <pixrect/pixrect_hs.h>


/************************************************************************/
/*									*/
/*		GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	adjust_line_to_node (mode, source, x1,y1,		*/
/*		                                   x2,y2, target)	*/
/*	void	adjust_edgeline_to_node    (mode, edge, sourcenode,	*/
/*		                                        targetnode)	*/
/*	void	adjust_to_box_node        (node, x1,y1, x2,y2)		*/
/*	void	adjust_to_elliptical_node (node, x1,y1, x2,y2)		*/
/*	void	adjust_to_diamond_node    (node, x1,y1, x2,y2)		*/
/*									*/
/*	void	adjust_nodelabel_position     (node)			*/
/*	void	adjust_edgelabel_position     (edge)			*/
/*	void	adjust_nodelabel_text_to_draw (node)			*/
/*	void	adjust_edgelabel_text_to_draw (edge)			*/
/*									*/
/*	void	adjust_arrow_to_edge (edge)				*/
/*									*/
/*	void	adjust_edge_box (edge)					*/
/*									*/
/*	void	adjust_edge_head (edge)					*/
/*	void	adjust_edge_tail (edge)					*/
/*	void	adjust_all_edges (node)					*/
/*									*/
/*      void   adjust_boxes_in_graph (graph)                            */
/*      void   adjust_boxes_in_group  (group)				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/

int	box_adjustment_enabled = TRUE;


/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/


static	void	clip_line_out_of_node  ();
static	void	special_adjust_to_node ();


/************************************************************************/
/*									*/
/*			KONVENTIONEN					*/
/*									*/
/*	- Alle globalen Prozeduren in diesem Modul beginnen mit		*/
/*	  adjust_... .							*/
/*	- Keine Prozedur loescht bzw. zeichnet das anzupassende Objekt.	*/
/*	  Das ist vielmehr Aufgabe der aufrufenden Prozedur.		*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		Anpassung von Kanten an Knoten				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Die Art der Anpassung wird durch das Attribut			*/
/*	node_edge_interface im source- und/oder target- Knoten		*/
/*	bestimmt :							*/
/*	NO_NODE_EDGE_INTERFACE : die Endpunkte der Linien werden nicht	*/
/*		veraendert.						*/
/*	TO_BORDER_OF_BOUNDING_BOX : die anzupassenden Endpunkte werden	*/
/*		auf die Mitte der naechstliegenden Seite der		*/
/*		bounding_box des Knotens gesetzt.			*/
/*	TO_CORNER_OF_BOUNDING_BOX : die anzupassenden Endpunkte werden	*/
/*		auf die naechstliegende Ecke der bounding_box des	*/
/*		Knotens gesetzt.					*/
/*	CLIPPED_TO_MIDDLE_OF_NODE : die Linie wird zunaechst auf den	*/
/*		Mittelpunkt des Knotens gefuehrt und dann an der	*/
/*		bounding_box abgeschnitten.				*/
/*	SPECIAL_NODE_EDGE_INTERFACE : hier wird zwischen system-	*/
/*		definierten Knotentypen (is_system == TRUE) und		*/
/*		benutzerdefinierten Typen (ueber Pixelmuster)		*/
/*		unterschieden :						*/
/*		- bei benutzerdefinierten Typen wird analog zu		*/
/*		CLIPPED_TO_MIDDLE_OF_NODE verfahren, jedoch nicht schon	*/
/*		an der bounding_box, sondern erst am ersten gesetzten	*/
/*		Pixel abgeschnitten;					*/
/*		- bei systemdefinierten Typen wird die im Typ		*/
/*		angegebene Prozedur (adjust_func) aufgerufen (die	*/
/*		dasselbe wie oben bewirkt, nur schneller ist).		*/
/*									*/
/************************************************************************/
/*									*/
/*	void	adjust_line_to_node (mode, source, x1,y1,		*/
/*                                                 x2,y2, target)	*/
/*									*/
/*	Passt die Linie (x1,y1) - (x2,y2) an source und/oder target	*/
/*	an, je nachdem mode gesetzt ist :				*/
/*	ADJUST_EDGELINE_HEAD : Anpassung an source			*/
/*	ADJUST_EDGELINE_TAIL : Anpassung an tail			*/
/*	ADJUST_EDGELINE_HEAD_AND_TAIL : Anpassung an source UND tail.	*/
/*									*/
/*	WICHTIG : Soll die Linie an beide Knoten angepasst werden, muss	*/
/*	unbedingt mit ADJUST_EDGELINE_HEAD_AND_TAIL angepasst werden,	*/
/*	da sonst Inkonsistenzen auftreten koennen (Reihenfolge !).	*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	adjust_edgeline_to_node (mode, edge, sourcenode,	*/
/*		                                     targetnode)	*/
/*									*/
/*	Passt eine Kante an ihre Anfangs- und/oder Endknoten an.	*/
/*	Falls edge == NULL, so wird die temporary_edgeline		*/
/*	(->repaint.c) an sourcenode und/oder targetnode angepasst,	*/
/*	ansonsten wird edge an edge->source bzw. edge->target		*/
/*	angepasst.							*/
/*	Mode gibt an, an welche(n) Knoten angepasst wird :		*/
/*	ADJUST_EDGELINE_HEAD :          Anpassung an source		*/
/*	ADJUST_EDGELINE_TAIL :          Anpassung an target		*/
/*	ADJUST_EDGELINE_HEAD_AND_TAIL : Anpassung an source UND tail.	*/
/*	Diese Prozedur verwendet (natuerlich) adjust_line_to_node.	*/
/*									*/
/*	WICHTIG : Soll die Kante an beide Knoten angepasst werden, muss	*/
/*	unbedingt mit ADJUST_EDGELINE_HEAD_AND_TAIL angepasst werden,	*/
/*	da sonst Inkonsistenzen auftreten koennen (Reihenfolge !).	*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	clip_line_out_of_node (node, x1,y1, x2,y2)	*/
/*									*/
/*	Klippt die Linie (x1,y1) - (x2,y2) an der bounding_box von	*/
/*	node, so dass sie ganz ausserhalb zu liegen kommt. Liegt die	*/
/*	Linie urspruenglich ganz innerhalb des Knotens, so schrumpft	*/
/*	sie zu (x2,y2) - (x2,y2) zusammen.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Prozeduren fuer SPECIAL_NODE_EDGE_INTERFACE :			*/
/*									*/
/*	void	adjust_to_box_node        (node, x1,y1, x2,y2)		*/
/*	void	adjust_to_elliptical_node (node, x1,y1, x2,y2)		*/
/*	void	adjust_to_diamond_node    (node, x1,y1, x2,y2)		*/
/*									*/
/*	Prozeduren fuer die Anpassung an systemdefinierte Knotentypen;	*/
/*	Verwendung in nodetype->adjust_func (daher global).		*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	void	special_adjust_to_node (node, x1,y1, x2,y2)	*/
/*									*/
/*	Diese Prozedur fuehrt SPECIAL_NODE_EDGE_INTERFACE bei benutzer-	*/
/*	definierten Knotentypen aus : Die Linie wird vom Schnitt-	*/
/*	punkt mit der bounding_box bis zur Knotenmitte mittels eines	*/
/*	modifizierten Bresenham-Algorithmus verfolgt und der erste	*/
/*	Pixel, den die Kante trifft, gesucht. "Treffen" kann hier auch	*/
/*	eine Position unterhalb bzw. oberhalb bedeuten, da es sonst	*/
/*	moeglich ist, dass sich die Linie "durchschlaengelt".		*/
/*									*/
/************************************************************************/



void		adjust_line_to_node (mode, source, x1,y1, x2,y2, target)
Nei_adjust_mode	mode;
Node		source,   target;
int		*x1, *y1, *x2, *y2;
{
	int	p1_x[4], p1_y[4],	/* Cluster von Punkten		*/
		p2_x[4], p2_y[4];
	int	min_i,min_j;
	int	x,y;

	/* in p1_x, ..., p2_y werden fuer TO_CORNER_OF_BOUNDING_BOX und	*/
	/* TO_BORDER_OF_BOUNDING_BOX die Ecken bzw. Seitenmittelpunkte	*/
	/* eingegeben							*/
	
	if ( (mode == ADJUST_EDGELINE_HEAD) || (mode == ADJUST_EDGELINE_HEAD_AND_TAIL) ) {
	
		switch (source->node_edge_interface) {
		
		    case TO_CORNER_OF_BOUNDING_BOX :
			p1_x[0] = p1_x[3] = node_left(source);
			p1_x[1] = p1_x[2] = node_left(source)  + node_width(source);
			p1_y[0] = p1_y[1] = node_top(source);
			p1_y[2] = p1_y[3] = node_top(source) + node_height(source);
			break;
		
		    case TO_BORDER_OF_BOUNDING_BOX :
			p1_x[3] =           node_left(source);
			p1_x[0] = p1_x[2] = node_left(source)  + node_width(source)/2;
			p1_x[1] =           node_left (source) + node_width(source);
			p1_y[0] =           node_top(source);
			p1_y[1] = p1_y[3] = node_top(source) + node_height(source)/2;
			p1_y[2] =           node_top(source) + node_height(source);
			break;
		}
	}
	
	if ( (mode == ADJUST_EDGELINE_TAIL) || (mode == ADJUST_EDGELINE_HEAD_AND_TAIL) ) {
	
		switch (target->node_edge_interface) {
		
		    case TO_CORNER_OF_BOUNDING_BOX :
			p2_x[0] = p2_x[3] = node_left(target);
			p2_x[1] = p2_x[2] = node_left(target)  + node_width(target);
			p2_y[0] = p2_y[1] = node_top(target);
			p2_y[2] = p2_y[3] = node_top(target) + node_height(target);
			break;

		    case TO_BORDER_OF_BOUNDING_BOX :
			p2_x[3] =           node_left(target);
			p2_x[0] = p2_x[2] = node_left(target) + node_width(target)/2;
			p2_x[1] =           node_left(target) + node_width(target);
			p2_y[0] =           node_top(target);
			p2_y[1] = p2_y[3] = node_top(target) + node_height(target)/2;
			p2_y[2] =           node_top(target) + node_height(target);
			break;
		}
	}

	
	switch (mode) {
	
	    case ADJUST_EDGELINE_HEAD :

		switch (source->node_edge_interface) {
		
		    case TO_CORNER_OF_BOUNDING_BOX :
		    case TO_BORDER_OF_BOUNDING_BOX :
			p2_x[0] = *x2;
			p2_y[0] = *y2;
			find_min_distance_between_pointclusters (p1_x,p1_y,4, p2_x,p2_y,1, &min_i,&min_j);
			*x1 = p1_x[min_i];
			*y1 = p1_y[min_i];
			break;
		
		    case CLIPPED_TO_MIDDLE_OF_NODE :
			clip_line_out_of_node (source, x1,y1, *x2,*y2);
			break;
			
		    case SPECIAL_NODE_EDGE_INTERFACE :
			if (source->type->is_system)
				(source->type->adjust_func) (source, x1,y1, *x2,*y2);
			else
				special_adjust_to_node (source, x1,y1, *x2,*y2);

			break;
			
		    default : 
			break;
		}
		break;
		
		
	    case ADJUST_EDGELINE_TAIL :
	    
	    	switch (target->node_edge_interface) {
		
		    case TO_CORNER_OF_BOUNDING_BOX :
		    case TO_BORDER_OF_BOUNDING_BOX :
			p1_x[0] = *x1;
			p1_y[0] = *y1;
			find_min_distance_between_pointclusters (p1_x,p1_y,1, p2_x,p2_y,4, &min_i,&min_j);
			*x2 = p2_x[min_j];
			*y2 = p2_y[min_j];
			break;
	
		    case CLIPPED_TO_MIDDLE_OF_NODE :
			clip_line_out_of_node (target, x2,y2, *x1,*y1);
			break;
			
		    case SPECIAL_NODE_EDGE_INTERFACE :
			if (target->type->is_system)
				(target->type->adjust_func) (target, x2,y2, *x1,*y1);
			else
				special_adjust_to_node (target, x2,y2, *x1,*y1);
			break;
			
		    default :
			break;
		}
		break;

			
	    case ADJUST_EDGELINE_HEAD_AND_TAIL :
		
		if (source->node_edge_interface == target->node_edge_interface) {
		
			switch (source->node_edge_interface) {
			
			    case TO_BORDER_OF_BOUNDING_BOX :
			    case TO_CORNER_OF_BOUNDING_BOX :
			
				find_min_distance_between_pointclusters (
					p1_x,p1_y, 4,  p2_x,p2_y, 4,  &min_i,&min_j);
				*x1 = p1_x[min_i];  *y1 = p1_y[min_i];
				*x2 = p2_x[min_j];  *y2 = p2_y[min_j];
				break;
			
			    case CLIPPED_TO_MIDDLE_OF_NODE :
				clip_line_out_of_node (source, x1,y1, node_x(target),node_y(target));
				clip_line_out_of_node (target, x2,y2, node_x(source),node_y(source));
				break;
			
			    case SPECIAL_NODE_EDGE_INTERFACE :
				x = node_x (target); y = node_y (target);
				adjust_line_to_node (ADJUST_EDGELINE_HEAD, source, x1,y1, &x,&y, target);
				x = node_x (source); y = node_y (source);
				adjust_line_to_node (ADJUST_EDGELINE_TAIL, source, &x,&y, x2,y2, target);
				break;
			
			    default :
				break;
			}

		} else {
		
			if ((int)source->node_edge_interface < (int)target->node_edge_interface   ) {
				adjust_line_to_node (ADJUST_EDGELINE_HEAD, source, x1,y1, x2,y2, target);
				adjust_line_to_node (ADJUST_EDGELINE_TAIL, source, x1,y1, x2,y2, target);
			} else {
				adjust_line_to_node (ADJUST_EDGELINE_TAIL, source, x1,y1, x2,y2, target);
				adjust_line_to_node (ADJUST_EDGELINE_HEAD, source, x1,y1, x2,y2, target);
			}
		}
		
		break;

	}

}
			
			
			
void		adjust_edgeline_to_node (mode, edge, sourcenode, targetnode)
Nei_adjust_mode	mode;
Edge		edge;
Node		sourcenode, targetnode;
{
	int		x1,y1, x2,y2;
	Edgeline	el;
	Node		source, target;
	
	if (edge == empty_edge) {
		el = temporary_edgeline;
		source = sourcenode;
		target = targetnode;
	} else {
		el = edge->line;
		source = edge->source;
		target = edge->target;
	}
	switch (mode) {
	    case ADJUST_EDGELINE_HEAD          :
	    case ADJUST_EDGELINE_HEAD_AND_TAIL :
		x1 = el->x;
		y1 = el->y;
		x2 = el->suc->x;
		y2 = el->suc->y;
		break;
	    case ADJUST_EDGELINE_TAIL :
		x1 = el->pre->pre->x;
		y1 = el->pre->pre->y;
		x2 = el->pre->x;
		y2 = el->pre->y;
		break;
	}
	
	adjust_line_to_node (mode, source, &x1,&y1, &x2,&y2, target);
	
	switch (mode) {
	    case ADJUST_EDGELINE_HEAD          :
	    case ADJUST_EDGELINE_HEAD_AND_TAIL :
		set_edgeline_xy (el,      x1,y1);
		set_edgeline_xy (el->suc, x2,y2);
		break;
	    case ADJUST_EDGELINE_TAIL :
		set_edgeline_xy (el->pre->pre, x1,y1);
		set_edgeline_xy (el->pre,      x2,y2);
		break;
	}
}


static	void	clip_line_out_of_node (node, x1,y1, x2,y2)
Node		node;
int		*x1,*y1, x2,y2;
{
	register int	x = node_x (node),
			y = node_y (node),
			dx  = x2 - x,
			dy  = y2 - y,
			adx = abs(dx),
			ady = abs(dy),
			width  = node_width  (node),
			height = node_height (node);
	
	if ( (adx > width/2) || (ady > height/2) ) {
		/* (x2,y2) ausserhalb des Knotens	*/
		if (adx * height >= ady * width ) {
			if (dx >0) {
				*x1 = x + (width - width/2);
				*y1 = y + ((width -width/2) * dy) / dx;
			} else {
				*x1 = x - width/2;
				*y1 = y - (width/2 * dy) / dx;
			}
		} else {
			if (dy >0) {
				*x1 = x + ((height - height/2) * dx) / dy;
				*y1 = y + (height - height/2);
			} else {
				*x1 = x - (height/2 * dx) / dy;
				*y1 = y - height/2;
			}
		}
	} else {
		/* (x2,y2) innerhalb des Knotens	*/
		*x1 = x2;
		*y1 = y2;
	}
}




void	adjust_to_box_node (node, x1,y1, x2,y2)
Node	node;
int	*x1,*y1, x2,y2;
{
	clip_line_out_of_node (node, x1,y1, x2,y2);
}


void	adjust_to_elliptical_node (node, x1,y1, x2,y2)
Node	node;
int	*x1,*y1, x2,y2;
{
	double	x,y, m, a,b, xx,yy,aa,bb;
	
	a = (double)node_width(node)  / 2.0;	/* Halbachsen der	*/
	b = (double)node_height(node) / 2.0;	/* Ellipse		*/
	x = x2 - node_x(node);	/* (x2,y2) im Koordinatensystem	*/
	y = y2 - node_y(node);	/* des Knotens			*/
	aa = a*a; bb = b*b;
	xx = x*x; yy = y*y;
	
	if ( xx/aa + yy/bb < 1 ) {
		x = 0;
		y = 0;
	} else if (x != 0) {
		m = y/x;
		if (x>0)
			x =   a*b * sqrt ( 1 / (bb + aa * m*m) );
		else
			x = - a*b * sqrt ( 1 / (bb + aa * m*m) );
		y = m * x;
	} else /* x == 0 */ {
		x = 0;
		y = iif (y2 >= node_y(node), b, -b);
	}
	
	*x1 = node_x(node) + x;
	*y1 = node_y(node) + y;
}


/* Subprocedure needed for adjust_to_diamond_node	*/

int	line_line_intersection (x1,y1, x2,y2, x3,y3, x4,y4, x,y)
int	x1,y1, x2,y2,	/* first line				*/
	x3,y3, x4,y4;	/* second line				*/
int	*x, *y;		/* *the* return point if result is TRUE	*/
{
	/* Note : this procedure does NOT check whether the	*/
	/* intersection point lies in [x1,x2]*[x3,x4] and	*/
	/* [y1,y2]*[y3,y4].					*/
	
	double	dx12 = x2 - x1,
		dy12 = y2 - y1,
		dx34 = x4 - x3,
		dy34 = y4 - y3;
		
	if (dx12 == 0 && dx34 == 0) {
		if (x1 == x3) {
			*x = x1; *y = y1; return TRUE;
		} else {
			return FALSE;
		}
	} else if (dx12 == 0) /* dx34 != 0 */ {
		*y = y3 + dy34/dx34 * (x1 - x3);
		*x = x1;
		return TRUE;
	} else if (dx34 == 0) /* dx12 != 0 */ {
		*y = y1 + dy12/dx12 * (x3 - x1);
		*x = x3;
		return TRUE;
	} else /* dx12 !=0 && dx34 != 0 */ {
		double	m12 = dy12 / dx12,	/* Slopes	*/
			m34 = dy34 / dx34;
		*x = (y3-y1 - (m34*x3 - m12*x1)) / (m12 - m34);
		*y = y1 + (*x-x1)*m12;
		return TRUE;
	}	
}



void	adjust_to_diamond_node (node, x1,y1, x2,y2)
Node	node;
int	*x1,*y1, x2,y2;
{
	int	dx, dy;
	int	n_x = node_x (node),
		n_y = node_y (node),
		n_width  = node_width  (node),
		n_height = node_height (node),
		n_left = node_left (node),
		n_top  = node_top  (node);
	int	x[4], y[4];			/* The diamond's endpoints	*/
	int	intersect_x, intersect_y;	/* The intersection point	*/
	int	intersects = FALSE;		/* Do they really intersect ?	*/
	
	
	x[0] = n_left;			y[0] = n_top + n_height/2 -1;
	x[1] = n_left + n_width/2 -1;	y[1] = n_top;
	x[2] = n_left + n_width   -1;	y[2] = n_top + n_height/2;
	x[3] = n_left + n_width/2;	y[3] = n_top + n_height   -1;

	dx = x2 - n_x;	/* Ignore any value in *x1 and *x2	*/
	dy = y2 - n_y;
	
	if (dx < 0) {
		if (dy < 0) {
			/* IV.  Quadrant	*/
			intersects = line_line_intersection (x[0],y[0], x[1],y[1], n_x,n_y, x2,y2,
			                                     &intersect_x,&intersect_y);
		} else /* dy >= 0 */ {
			/* III. Quadrant	*/
			intersects = line_line_intersection (x[3],y[3], x[0],y[0], n_x,n_y, x2,y2,
			                                     &intersect_x,&intersect_y);
		}
	} else /* dx >= 0 */ {
		if (dy < 0) {
			/* I.   Quadrant	*/
			intersects = line_line_intersection (x[1],y[1], x[2],y[2], n_x,n_y, x2,y2,
			                                     &intersect_x,&intersect_y);
		} else /* dy >= 0 */ {
			/* II.   Quadrant	*/
			intersects = line_line_intersection (x[2],y[2], x[3],y[3], n_x,n_y, x2,y2,
			                                     &intersect_x,&intersect_y);
		}
	}
	
	if (intersects) {
		*x1 = intersect_x;
		*y1 = intersect_y;
	} else {
		*x1 = n_x;
		*y1 = n_y;
	}
}


static	void	special_adjust_to_node (node, x1,y1, x2,y2)
Node		node;
int		*x1,*y1, x2,y2;
{
	/* Die Prozedur folgt einem modifizierten Bresenhamalgorithmus.	*/
	/* Die Gerade durch (x1,y1) - (x2,y2) wird von der bounding_box	*/
	/* des Knotens aus bis zum Mittelpunkt verfolgt. Der erste	*/
	/* dabei beruehrte Punkt [(x,y), (x,y-1), (x,y+1)] wird als	*/
	/* neuer Endpunkt (x1,y1) ausgegeben; falls nichts gefunden	*/
	/* wurde, der Knotenmittelpunkt.				*/
	/* DIESEM VERFAHERN LIEGT EINE PROZEDUR AUS DER SOURCE ZUM	*/
	/* SUNTOOLS - BEISPIELPROGRAMM "ICONEDIT" ZUGRUNDE.		*/
	
	register int	p,	/* Schneller Zwischenspeicher fuer das	*/
				/* Ergebnis von pr_get			*/
			d,	/* Entscheidungsgroesse, ob dep		*/
				/* gleich bleibt oder sich aendert	*/
			ind,	/* Abhaengige Variable			*/
			dep,	/* Unabhaengige Variable		*/
			incr_only_ind,    /* Inkrement fuer d, je	*/
			incr_ind_and_dep, /* nachdem dep geaendert wird	*/
			                  /* oder nur ind		*/
			ind_end,	/* Letzter Wert fuer ind	*/
					/* ( = Knotenmitte)		*/
			incr_ind,	/* Inkrement fuer ind		*/
			incr_dep,	/* Inkrement fuer dep		*/
			x_is_ind,	/* Ist x die unabhaengige	*/
					/* Variable			*/
			check_d_smaller_zero;	/* Soll d auf <0 oder	*/
						/* auf >0 geprueft	*/
						/* werden ?		*/
						
	int		ind1,ind2,	/* Startpunkt			*/
			dep1,dep2,	/* Endpunkt			*/
			d_ind, d_dep;	/* |Endpunkt - Startpunkt|	*/

	clip_line_out_of_node (node, x1,y1, x2,y2);
	*x1 -= node_left (node);
	*y1 -= node_top  (node);
	
	x2 = node_width  (node) / 2;
	y2 = node_height (node) / 2;
	if (abs(x2 - *x1) > abs(y2 - *y1)) {
		x_is_ind = TRUE;
		ind1 = *x1; dep1 = *y1;
		ind2 = x2;  dep2 = y2;
	} else {
		x_is_ind = FALSE;
		ind1 = *y1; dep1 = *x1;
		ind2 = y2;  dep2 = x2;
	}
	
	d_ind = ind2 - ind1;
	d_dep = dep2 - dep1;
	incr_only_ind    = iif (d_ind > 0, 2*d_dep, - 2*d_dep);
	incr_ind_and_dep = incr_only_ind - iif (d_dep > 0, 2*d_ind, -2*d_ind);
	d = (incr_only_ind + incr_ind_and_dep) / 2;
	ind = ind1;
	dep = dep1;
	ind_end = ind2;
	incr_ind = iif (d_ind >= 0, 1, -1);
	incr_dep = iif (d_dep >= 0, 1, -1);
	check_d_smaller_zero = (d_ind > 0  && d_dep > 0) ||
	                       (d_ind < 0  && d_dep < 0) ||
	                       (d_dep == 0 && d_ind < 0);
	
	while (ind != ind_end) {
		if (x_is_ind) {
			if ( (p = pr_get (node->image->pr, ind, dep)) != PIX_ERR && p != 0) {
				goto found;
			} else if ( (p = pr_get (node->image->pr, ind, dep-1)) != PIX_ERR && p != 0) {
				goto found;
			} else if ( (p = pr_get (node->image->pr, ind, dep+1)) != PIX_ERR && p != 0) {
				goto found;
			}
		} else {
			if ( (p = pr_get (node->image->pr, dep, ind)) != PIX_ERR && p != 0) {
				goto found;
			} else if ( (p = pr_get (node->image->pr, dep-1, ind)) != PIX_ERR && p != 0) {
				goto found;
			} else if ( (p = pr_get (node->image->pr, dep+1, ind)) != PIX_ERR && p != 0) {
				goto found;
			}
		}
		if ( iif (check_d_smaller_zero, d<0, d>0) ) {
			d += incr_only_ind;
		} else {
			d += incr_ind_and_dep;
			dep += incr_dep;
		}
		ind += incr_ind;
	}
	
found :	/* Bad but fast */
	if (x_is_ind) {
		*x1 = node_left (node) + ind;
		*y1 = node_top  (node) + dep;
	} else {
		*x1 = node_left (node) + dep;
		*y1 = node_top  (node) + ind;
	}
}
/************************************************************************/
/*									*/
/*		LABEL AN KNOTEN UND KANTEN ANPASSEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	Zum Anpassen von Labeln sind zwei Arten von Prozeduren		*/
/*	vorhandem :							*/
/*		- adjust_..._text_to_draw passt den Text an (Feld	*/
/*		text_to_draw in Nodelabel bzw. Edgelabel). Dazu werden	*/
/*		nicht wiedergebbare Zeichen aus dem Text entfernt und	*/
/*		gleichzeitig der Text auf die erste Zeile gekuerzt.	*/
/*		Ausserdem darf der Text die vorgegebene Maximalgroesse	*/
/*		nicht ueberschreiten.					*/
/*		- adjust_..._position passt nur die Position des Labels	*/
/*		an, nicht aber den Text.				*/
/*	Wird also der Text des Labels geaendert, muessen beide		*/
/*	Prozeduren aufgerufen werden, aendert sich nur die Position,	*/
/*	so genuegt adjust_...position.					*/
/*									*/
/*	Am Knotenrand werden jeweils NODELABEL_GAP Pixel Platz		*/
/*	gelassen; damit kann der Label auch dann in die Ecke		*/
/*	gezeichnet werden, wenn der Knoten eine Umrahmung (z.B.		*/
/*	Rechteckknoten) hat.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	adjust_nodelabel_position (node)			*/
/*									*/
/*	Passt den Knotenlabel entsprechend node->label.placement an.	*/
/*	Knotenlabel liegen grundsaetzlich innerhalb der Bounding-box	*/
/*	des Knotens.							*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	adjust_edgelabel_position (edge)			*/
/*									*/
/*	Justiert die Position des Kantenlabels neu. Der Kantenlabel	*/
/*	wird in die Mitte des ersten Teilstuecks von edge->line		*/
/*	gesetzt.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	adjust_nodelabel_text_to_draw (node)			*/
/*									*/
/*	Maximalgroesse fuer den Text ist die bounding_box des Knotens.	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	adjust_edgelabel_text_to_draw (edge)			*/
/*									*/
/*	Maximalgroesse fuer den Text ist (current_edgelabel_size_x *	*/
/*	current_edgelabelsize_y).					*/
/*									*/
/************************************************************************/



struct	pr_subregion	compute_lines_subregion_size (lines, font)
char			**lines;
Graphed_font		font;
{
	struct	pr_subregion label_bound, this_line_bound;
	int	line;
	int	label_bound_max_x, label_bound_max_y;
	
	if (lines == NULL)
		return;
	
	/* Zuerst berechnen wir in r eine neue Boundingbox fuer		*/
	/* den Label ...						*/
	label_bound_max_x = 0;
	label_bound_max_y = 0;
	for (line=0; lines[line] != NULL; line++) {
		
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_textbound instead, remember to extern it
#endif
pf_textbound (&this_line_bound,
			strlen(lines[line]),
			font->font,
			lines[line]);
		label_bound_max_x = maximum (label_bound_max_x, this_line_bound.size.x);
		label_bound_max_y = maximum (label_bound_max_y, this_line_bound.size.y);
	}
	label_bound.size.x = label_bound_max_x;
	label_bound.size.y = line * label_bound_max_y;
	
	return label_bound;
}


void	adjust_nodelabel_position (node)
Node	node;
{
	struct	pr_subregion label_bound, first_line_bound;
	Rect	r;
	
	if (node->label.text_to_draw == NULL)
		return;
	
	label_bound = compute_lines_subregion_size (node->label.text_to_draw,
		node->label.font);
		
	if (node->label.text_to_draw [0] != NULL) {
		
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_textbound instead, remember to extern it
#endif
pf_textbound (&first_line_bound,
			strlen(node->label.text_to_draw[0]),
			node->label.font->font,
			node->label.text_to_draw[0]);
	} else {
		first_line_bound.pos.x = 0;
		first_line_bound.pos.y = 0;
		first_line_bound.size.x = 0;
		first_line_bound.size.y = 0;
	}
	
	
	/* ... dann passen wir diese box in den Knoten ein ...		*/
	
	switch (node->label.placement) {
	    case NODELABEL_MIDDLE :
		rect_construct (&r, node_left(node) + (node_width(node))/2  - label_bound.size.x /2,
		                    node_top(node)  + (node_height(node))/2 - label_bound.size.y /2,
		                    label_bound.size.x,
		                    label_bound.size.y);
		break;
	    case NODELABEL_UPPERLEFT :
		rect_construct (&r, node_left(node) + NODELABEL_GAP,
		                    node_top(node)  + NODELABEL_GAP,
		                    label_bound.size.x,
		                    label_bound.size.y);
		break;
	    case NODELABEL_UPPERRIGHT :
		rect_construct (&r, node_left(node) + node_width(node) - NODELABEL_GAP - label_bound.size.x,
		                    node_top(node) + NODELABEL_GAP,
		                    label_bound.size.x,
		                    label_bound.size.y);
		break;
	    case NODELABEL_LOWERLEFT :
		rect_construct (&r, node_left(node) + NODELABEL_GAP,
		                    node_top(node) + node_height(node) - NODELABEL_GAP - label_bound.size.y,
		                    label_bound.size.x,
		                    label_bound.size.y);
		break;
	    case NODELABEL_LOWERRIGHT :
		rect_construct (&r, node_left(node) + node_width(node) - NODELABEL_GAP - label_bound.size.x,
		                    node_top(node) + node_height(node) - NODELABEL_GAP - label_bound.size.y,
		                    label_bound.size.x,
		                    label_bound.size.y);
		break;
	}
	
	/* ... und schon setzen wir noch die neue Boundingbox ein	*/
	
	node->label.box = r;	
	node->label.x = rect_left(&(node->label.box)) + first_line_bound.pos.x;
	node->label.y = rect_top (&(node->label.box)) - first_line_bound.pos.y;
	
	node->full_box = rect_bounding (&(node->box), &(node->label.box));
}



void	adjust_edgelabel_position (edge)
Edge	edge;
{
	struct		pr_subregion label_bound;
	Edgeline	el;
	int		n, i;
	
	if (edge->label.text_to_draw == NULL)
		return;
	
	
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_textbound instead, remember to extern it
#endif
pf_textbound (&label_bound,
	              strlen(*(edge->label.text_to_draw)),
	              edge->label.font->font,
	              *(edge->label.text_to_draw));
	              
	/* determine the middle part of edge->line	*/
	
	if (!is_single_edgeline(edge->line)) {
		n = 0;
		for_edgeline (edge->line, el) {
			n ++;
		} end_for_edgeline (edge->line, el);
	
		el = edge->line;
		for (i=0; i<n/2-1; i++) {
			el = el->suc;
		}
	} else {
		el = edge->line;
	}
					
	rect_construct (&edge->label.box,
		(el->x + el->suc->x) / 2 - label_bound.size.x / 2,
		(el->y + el->suc->y) / 2 - label_bound.size.y / 2,
		minimum (label_bound.size.x, current_edgelabel_width),
		minimum (label_bound.size.y, current_edgelabel_height));
	
	edge->label.x = rect_left(&(edge->label.box)) + label_bound.pos.x;
	edge->label.y = rect_top (&(edge->label.box)) - label_bound.pos.y;
}



void	adjust_nodelabel_text_to_draw (node)
Node	node;
{
	int	line, n_lines;
	char	**text;
	
	int	label_is_small_enough, length, i;
	int	line_max_height   = 0;
	struct	pr_subregion	label_bound;	/* "Groesse" des Labels	*/


	if (node->label.text == NULL) {
		node->label.box = rect_null;
		if (node->label.text_to_draw != NULL) {
			myfree (node->label.text_to_draw);
		}
		node->label.text_to_draw = NULL;
		return;
	} else {
		if (node->label.text_to_draw != NULL) {
			myfree (node->label.text_to_draw);
		}
	}
	
	text = (char **)split_string (node->label.text);
	
	n_lines = 0;
	line_max_height = 0;
	for (line=0; text[line] != NULL; line++) {
		
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_textbound instead, remember to extern it
#endif
pf_textbound (&label_bound, strlen(text[line]),
			node->label.font->font,
			text[line]);
		line_max_height = maximum (line_max_height, label_bound.size.y);
		n_lines ++;
	}
	
	for (line=0; text[line] != NULL; line++) {
		
		if ((line+1) * line_max_height > node_height (node)) {
		
			myfree (text[line]);
			text[line] = NULL;
			n_lines --;
		
		} else {
		
			length = strlen (text[line]);
			
			label_is_small_enough = TRUE;
			for (i=1; i<=length && label_is_small_enough; i++) {
				
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_textbound instead, remember to extern it
#endif
pf_textbound (&label_bound, i,
					node->label.font->font,
					text[line]);
				label_is_small_enough = (label_bound.size.x <= node_width(node));
				if (!label_is_small_enough) {
					i --;
					break;
				}
			}
			(text[line])[i] = '\0';
		}
	}
	
	node->label.n_lines      = n_lines;
	node->label.line_height  = line_max_height;
	node->label.text_to_draw = text;
}



void	adjust_edgelabel_text_to_draw (edge)
Edge	edge;
{
	int	label_is_small_enough = TRUE,
		i, length;
	char	*text;
	struct	pr_subregion	label_bound;	/* "Groesse" des Label	*/
	
	if (edge->label.text == NULL) {
		edge->label.box = rect_null;
		if (edge->label.text_to_draw != NULL) {
			myfree (edge->label.text_to_draw);
		}
		edge->label.text_to_draw = NULL;
		return;
	} else {
		if (edge->label.text_to_draw != NULL) {
			myfree (edge->label.text_to_draw);
		}
	}
	
	text = remove_control_chars_from_string (edge->label.text);
	length = strlen (text);
	
	for (i=1; i<=length && label_is_small_enough; i++) {
		
#ifdef XVIEW_COMMENT
     XView CONVERSION - Use pf_textbound instead, remember to extern it
#endif
pf_textbound (&label_bound, i,
		              edge->label.font->font,
		              text);
		label_is_small_enough =
			(label_bound.size.x <= current_edgelabel_width) &&
			(label_bound.size.y <= current_edgelabel_height);
	}
	if (!label_is_small_enough)
		/* Dann ist das letzte Zeichen ungueltig */
		i--;
	
	edge->label.text_to_draw = (char **)mymalloc(sizeof(char *));
	*(edge->label.text_to_draw) = mymalloc(i);
	strncpy (*(edge->label.text_to_draw), text, i-1);
	(*(edge->label.text_to_draw)) [i-1] = '\0';
	myfree (text);
}



/************************************************************************/
/*									*/
/*			PFEIL AN KANTE ANPASSEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	adjust_arrow_to_edge (edge)				*/
/*									*/
/*	Justiert den Pfeil an edge entsprechend edge->arrow.length und	*/
/*	edge->arrow.angle.						*/
/*									*/
/************************************************************************/



void	adjust_arrow_to_edge (edge)
Edge	edge;
{
	Arrow		arrow;
	Edgeline	el;	/* An diese Kante kommt der Pfeil	*/
	double		m;	/* Steigung der Kante			*/
	int		box_x,box_y,		/* Box um den		*/
			box_width,box_height;	/* zukuenftigen Pfeil	*/
	
	if (edge->line == (Edgeline)NULL) 
		return;
	
	
	arrow.length = edge->arrow.length;
	arrow.angle  = edge->arrow.angle;
	el = edge->line->pre->pre;
	
	/* Als erstes setzen wir die Spitze des Pfeiles ...		*/
	arrow.x1 = el->suc->x;
	arrow.y1 = el->suc->y;
	
	if (!edge->source->graph->directed || edge->arrow.length == 0) {
		/* ... wenn der Graph ungerichted ist, geht es schnell	*/
		box_x = arrow.x1;
		box_y = arrow.y1;
		box_width  = 0;
		box_height = 0;
	} else {
		/* ... wenn er aber gerichted ist ...				*/
		/* ... dann rechnen wir m (das ist die Steigung der Kante) aus	*/
		if ( !(el->x == el->suc->x && el->y == el->suc->y))
			m = atan2 ((double)(el->suc->y - el->y),
			           (double)(el->suc->x - el->x));
		else
			m = 0;
	
		/* ... und schon koennen wir den Pfeil berechnen !		*/
		arrow.x0 = arrow.x1 - (int)((double)arrow.length * cos(m + (double)arrow.angle));
		arrow.y0 = arrow.y1 - (int)((double)arrow.length * sin(m + (double)arrow.angle));
		arrow.x2 = arrow.x1 - (int)((double)arrow.length * cos(m - (double)arrow.angle));
		arrow.y2 = arrow.y1 - (int)((double)arrow.length * sin(m - (double)arrow.angle));
	
		box_x = minimum (minimum (arrow.x0, arrow.x1), arrow.x2);
		box_y = minimum (minimum (arrow.y0, arrow.y1), arrow.y2);
		box_width  = maximum (maximum (arrow.x0, arrow.x1), arrow.x2) - box_x + 1;
		box_height = maximum (maximum (arrow.y0, arrow.y1), arrow.y2) - box_y + 1;
	}
	
	rect_construct (&(arrow.box), box_x,box_y, box_width,box_height);
	
	edge->arrow = arrow;
}



/************************************************************************/
/*									*/
/*		BOX UM DIE KANTE NEU ANPASSEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	adjust_edge_box (edge)					*/
/*									*/
/*	Passt edge->box (bounding_box um edge->line, edge->label und	*/
/*	edge->arrow) neu an.						*/
/*									*/
/************************************************************************/


void	adjust_edge_box (edge)
Edge	edge;
{
	Edgeline	el;
	Rect		r;
	
	if (!box_adjustment_enabled)
		return;
		
	r = rect_bounding (&(edge->arrow.box), &(edge->label.box));
	if ((el = edge->line) != (Edgeline)NULL) do {
		r = rect_bounding (&r, &(el->box));
		el = el->suc;
	} while (el->suc != edge->line);
	
	edge->box = r;
}



/************************************************************************/
/*									*/
/*			KANTE INSGESAMT NEU ANPASSEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	adjust_edge_head (edge)					*/
/*	void	adjust_edge_tail (edge)					*/
/*									*/
/*	Diese Prozeduren sollten aufgerufen werden, falls der erste	*/
/*	bzw. zweite Punkt von edge->line geaendert worden ist.		*/
/*	Sie erledigen alles, was in diesem Fall notwendig ist.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	adjust_all_edges (node)					*/
/*									*/
/*	Justiert alle Kanten an node (adjust_edge_head in		*/
/*	node->sourcelist, adjust_edge_tail in node->targetlist) neu.	*/
/*									*/
/************************************************************************/



void	adjust_edge_head (edge)
Edge	edge;
{

	if (is_single_edgeline(edge->line)) {
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL, edge);
		adjust_arrow_to_edge (edge);
	} else {
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD, edge);
	}
	adjust_edgelabel_position (edge);
}


void	adjust_edge_tail (edge)
Edge	edge;
{
	if (is_single_edgeline(edge->line)) {
		adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL, edge);
	} else {
		adjust_edgeline_to_node (ADJUST_EDGELINE_TAIL, edge);
	}
	adjust_edgelabel_position (edge);
	adjust_arrow_to_edge (edge);
}


void	adjust_all_edges (node)
Node	node;
{
	Edge	edge;
	
	for_edge_sourcelist (node, edge)
		adjust_edge_head (edge);
		adjust_edge_box (edge);
	end_for_edge_sourcelist (node, edge);
	for_edge_targetlist (node, edge)
		adjust_edge_tail (edge);
		adjust_edge_box (edge);
	end_for_edge_targetlist (node, edge);
}



/************************************************************************/
/*									*/
/*			RECHTECK UM GRAPHEN NEU ANPASSEN		*/
/*									*/
/*									*/
/************************************************************************/



void	adjust_graph_box (graph)
Graph	graph;
{
	if (box_adjustment_enabled)
		graph->box = compute_rect_around_graph (graph);
}



/************************************************************************/
/*									*/
/*	RECHTECKE UM OBJEKTE BERECHNEN ERLAUBEN / VERBIETEN		*/
/*									*/
/*									*/
/************************************************************************/



void	adjust_boxes_in_graph (graph)
Graph	graph;
{
	Node	node;
	Edge	edge;

	for_nodes (graph, node) {
	
		for_edge_sourcelist (node, edge) {
			adjust_edge_box (edge);
		} end_for_edge_sourcelist (node, edge);
		
	} end_for_nodes (graph, node);
	
	adjust_graph_box (graph);
}
