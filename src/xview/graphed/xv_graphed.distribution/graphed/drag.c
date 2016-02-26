/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				drag	.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Procedures for dragging nodes, edges, group and group-boxes	*/
/*									*/
/************************************************************************/

#include "user_header.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Evp_result	drag_node_proc               ()			*/
/*	Evp_result	drag_edge_proc     	     ()			*/
/*	Evp_result	drag_group_box_proc          ()			*/
/*	Evp_result	drag_group_proc     	     ()			*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GRUPPEN UMRAHMEN UND ZIEHEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	The following procedures are used as subprocedures to		*/
/*	select_mode_event_proc. They process events in their own way,	*/
/*	but are not full event procedures standing on their own.	*/
/*									*/
/*======================================================================*/
/*									*/
/*	Evp_result	drag_node_proc (window, event, info)		*/
/*	Drag_node_info	*info;						*/
/*									*/
/*	"Ziehen" von Knoten mit der Maus. Diese Prozedur umfasst	*/
/*	sowohl Verschieben als auch Vergroessern und Verkleinern von	*/
/*	Knoten und wird sowohl von node_mode_event_proc als auch von	*/
/*	der spaeter beschriebenen select_event_proc verwendet.		*/
/*									*/
/*	info hat folgende Gestalt :					*/
/*									*/
/*	typedef	struct	{						*/
/*		enum {							*/
/*			MOVE_NODE,					*/
/*			SCALE_NODE_MIDDLE,				*/
/*			SCALE_NODE_UPPER_LEFT,				*/
/*			SCALE_NODE_UPPER_RIGHT,				*/
/*			SCALE_NODE_LOWER_LEFT,				*/
/*			SCALE_NODE_LOWER_RIGHT				*/
/*		}							*/
/*			what;						*/
/*									*/
/*		Node	node;		Der zu bearbeitende Knoten	*/
/*					NULL : wird noch erzeugt	*/
/*					(node_mode_event_proc)		*/
/*									*/
/*		int	x,y, sx,sy;	Plazierung und Groesse		*/
/*	}								*/
/*		Drag_node_info;						*/
/*									*/
/*									*/
/*	Plazierung und Groesse in info koennen von drag_node_proc	*/
/*	geaendert werden, wenn "constrain" aktiv ist oder das Format	*/
/*	des Knotens geaendert wird. Diese Aenderungen werden an die	*/
/*	aufrufende Prozedur uebergeben.					*/
/*									*/
/*	Erlaeuterung zu 'what' :					*/
/*	Dieses Attribut gibt an, was mit 'node' geschehen soll :	*/
/*	MOVE_NODE		verschieben				*/
/*	SCALE_NODE_MIDDLE	Groesse einstellen, so dass der Cursor	*/
/*				in einer der Ecken des Knotens liegt.	*/
/*				Alle Ecken veraendern ihre Lage.	*/
/*	SCALE_NODE_UPPER_LEFT	| Groesse so einstellen, dass der	*/
/*	SCALE_NODE_UPPER_RIGHT	| Cursor in der angegebenen Ecke	*/
/*	SCALE_NODE_LOWER_LEFT	| (oben links usw.) zu liegen kommt,	*/
/*	SCALE_NODE_LOWER_RIGHT	| waehrend die GEGEBUEBERLIEGENDE	*/
/*				  Ecke ihre Lage nicht veraendert.	*/
/*									*/
/************************************************************************/
/*									*/
/*	Evp_result		drag_edge_proc (window, event, info)	*/
/*	Drag_edge_info		*info;					*/
/*									*/
/*	"Ziehen" von Kanten mit der Maus.				*/
/*	drag_edge_proc wird sowohl von make_edge_event_proc als auch	*/
/*	in der spaeter beschriebenen select_event_proc verwendet.	*/
/*									*/
/*	info hat folgende Gestalt :					*/
/*									*/
/*	typedef	struct	{						*/
/*		enum {							*/
/*			NEW_EDGE,					*/
/*			OLD_EDGE_REAL_POINT,				*/
/*			OLD_EDGE_IMAGINARY_POINT,			*/
/*		}							*/
/*			what;						*/
/*		union {							*/
/*			struct {					*/
/*				Node		source;			*/
/*				Edgeline	el;			*/
/*			}						*/
/*				new_edge;				*/
/*			struct {					*/
/*				Edge		edge;			*/
/*				Edgeline	el;			*/
/*			}						*/
/*				real_point;				*/
/*			struct {					*/
/*				Edge		edge;			*/
/*				Edgeline	el;			*/
/*			}						*/
/*				imaginary_point;			*/
/*		}							*/
/*			which;						*/
/*		int	x,y;						*/
/*	}								*/
/*		Drag_edge_info;						*/
/*									*/
/*	Erlaeuterungen :						*/
/*	- "what" gibt an, um welchen Typ von Punkt (Kante) es sich	*/
/*	  handelt :							*/
/*		= NEW_EDGE : Neue Kante wird gezeichnet (d.h. Aufruf	*/
/*		  von make_edge_event_proc aus)				*/
/*		= OLD_EDGE_REAL_POINT : Ein Punkt einer Kante soll	*/
/*		  verschoben werden.					*/
/*		= OLD_EDGE_IMAGINARY_POINT : Ein neuer (imaginaerer)	*/
/*		  Punkt soll in eine bestehende Kante eingefuegt	*/
/*		  werden. Seine Lage wird zunaechst mit dieser Prozedur	*/
/*		  fixiert.						*/
/*	- "which" gibt - in Abhaengigkeit von what - das gewuenschte	*/
/*	  Objekt an (variant-record nennt man so was in Pascal) :	*/
/*		= new_edge gibt den letzten Punkt der (noch unfertigen)	*/
/*		  Kante und den Quellknoten an				*/
/*		= real_point gibt den zu verschiebenden Punkt an	*/
/*		= imaginary_point gibt den vor dem neuen Punkt		*/
/*		  liegenden Kanteneckpunkt an				*/
/*									*/
/*	- x,y	sind die Koordinaten, an die gezogen wurde		*/
/*									*/
/*======================================================================*/
/*									*/
/*	Evp_result	drag_group_box (window, event, info)		*/
/*									*/
/*	Manages dragging a box for group selection.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	Evp_result	drag_group_proc (window, event, info)		*/
/*									*/
/*	Manages dragging a group.					*/
/*									*/
/************************************************************************/





Evp_result	drag_node_proc (window, event, info)
Xv_Window	window;
Event		*event;
Drag_node_info	*info;
{
	static int	xe,ye, sxe, sye;
	static int	dragging;
	static Nodetype		virtual_nodetype      = (Nodetype)NULL;
	static Nodetypeimage	virtual_nodetypeimage = (Nodetypeimage)NULL;
	static Nodetypeimage	image = (Nodetypeimage)NULL;
	
	/* Erlaeuterungen :						*/
	/*								*/
	/* xe,ye, sxe, sye sind die Koordinaten, die der Knoten beim	*/
	/*	letzten Durchlauf durch diese Prozedur hatte. Sie	*/
	/*	werden gebraucht, um den "virtuellen" Knoten, mit	*/
	/*	dem Position und Groesse angezeigt werden, wieder	*/
	/*	zu loeschen.						*/
	/*								*/
	/* dragging : Um die Position und die Groesse des	*/
	/*	zukuenftigen Knotens anzuzeigen, wird ein "virtueller"	*/
	/*	Knoten (mit image) angezeigt.				*/
	/*								*/
	/* image : das Bild, mit dem der `virtuelle' Knoten gezeichnet	*/
	/*	wird. Beim Ziehen des Knotens ueber die Arbeitsflaeche	*/
	/*	ist image == NULL, so dass (siehe draw_virtual_node in	*/
	/*	draw.c) nur ein Rechteck gezeichnet wird. Bleibt die	*/
	/*	Maus stehen (LOC_STILL), so wird image kurzfristig	*/
	/*	auf virtual_nodetypeimage gesetzt (beim ziehen lohnt	*/
	/*	sich das nicht).					*/
	
	int		x,y, sx,sy;	/* aktuelle Position / Groesse		*/
	Node		node;		/* DER Knoten				*/
	Evp_result	result = EVP_CONSUMED;
	
	node = info->node;
	
	
	/* Vorverarbeitung : Position und Groesse (x,y, sx,sy)		*/
	/* festlegen							*/
	
	if (info->what == MOVE_NODE) {
	
		if (node == empty_node) { /* Knoten wird frisch erzeugt	*/
			if (last.graph != empty_graph && last.graph->is_production && last.graph->firstnode == empty_node) {
				/* The node will be the left side in a production */
				sx = (get_node_style(LEFT_SIDE_NODE_STYLE)).width;
				sy = (get_node_style(LEFT_SIDE_NODE_STYLE)).height;
			} else {
				sx = current_node_width;
				sy = current_node_height;
			}
		} else {	/* "alter" Knoten	*/
			sx = node_width  (node);
			sy = node_height (node);
		}
		
		if (node != empty_node) {
			if (!dragging) {
				/* We are starting to move a node and have to compute the corrections.	*/
				/* corrections are needed  because the user does not always click at	*/
				/* the nodes center							*/
				info->correction_x = event_x(event) - node_x (node);
				info->correction_y = event_y(event) - node_y (node);
			}
			event_set_x (event, event_x(event) - info->correction_x);
			event_set_y (event, event_y(event) - info->correction_y);
			if (constrain_is_active || shift_is_down)
				constrain_event (event, node_x(node), node_y(node));
		} else /* node == empty_node */ if ((constrain_is_active || shift_is_down) && get_gridwidth (wac_buffer) > 0)
			constrain_event (event, 0,0); /* 0 = Dummy */
		x = event_x (event);
		y = event_y (event);

	} else if (info->what == SCALE_NODE_MIDDLE) {
	
		/* Knoten punktsymmetrisch zum Ursprung skalieren	*/
		
		x  = node_x (node);
		y  = node_y (node);
		if (constrain_is_active || shift_is_down)
			constrain_event (event, x,y);
		sx = 2 * abs (event_x (event) - node_x (node));
		sy = 2 * abs (event_y (event) - node_y (node));
		if (shift_is_down || constrain_is_active) {
			/* Spezielle Form von "constrain" :	*/
			/* behalte x- bzw. y-Dimension bei	*/
			if (sx == 0) sx = node_width(node);
			if (sy == 0) sy = node_height(node);
		}
		
	} else {
	
		/* Knoten an einer Ecke verziehen			*/
		
		int	x0,y0;	/* "Fixunkte" beim Verziehen		*/
		
		/* Finde Fixpunkte heraus */
		switch (info->what) {
		    case SCALE_NODE_UPPER_LEFT :
			x0 = node_left (node) + node_width (node);
			y0 = node_top (node) + node_height (node);
			break;
		    case SCALE_NODE_UPPER_RIGHT :
			x0 = node_left (node);
			y0 = node_top (node) + node_height (node);
			break;
		    case SCALE_NODE_LOWER_LEFT :
			x0 = node_left (node) + node_width (node);
			y0 = node_top (node);
			break;
		    case SCALE_NODE_LOWER_RIGHT :
			x0 = node_left (node);
			y0 = node_top (node);
			break;
		}
		
		/* Berechne Position / Groesse; fuer "constrain" ist	*/
		/* eine Spezialbehandlung noetig (s.u.)			*/
		if (constrain_is_active || shift_is_down)
			constrain_event (event, x0,y0);
		sx = abs (event_x (event) - x0);
		sy = abs (event_y (event) - y0);
		if (constrain_is_active || shift_is_down) {
			/* Bei waagrechten (senkrechten) Bewegungen	*/
			/* ist sy (sx) jetzt 0. sy (sx) wird in diesem	*/
			/* Fall auf die Knotenhoehe (Knotenbreite)	*/
			/* gesetzt, so dass sich "constrain" so		*/
			/* auswirkt, dass nur Verziehen in waagrechter	*/
			/* (senkrechter) Richtung moeglich ist, die	*/
			/* Hoehe (Breite) also erhalten bleibt.		*/
			/* Bei Bewegungen unter 45 Grad bleiben die	*/
			/* Proportionen des Knoten erhalten.		*/
			if (sx == 0) {	/* waagrechte Bewegung	*/
				sx = node_width(node);
				x = node_x(node);
			} else		/* 45 Grad		*/
				x = minimum (event_x(event), x0) + sx/2;
			if (sy == 0) {	/* senkrechte Bewegung	*/
				sy = node_height(node);
				y = node_y(node);
			} else		/* 45 Grad		*/
				y = minimum (event_y(event), y0) + sy/2;
		} else {
			x = minimum (event_x(event), x0) + sx/2;
			y = minimum (event_y(event), y0) + sy/2;
		}
	}
	
	/* Position und Groesse in info zurueckgeben	*/
	
	info->x = x;
	info->y = y;
	info->sx = sx;
	info->sy = sy;
	
	
	/* Jetzt werden die Event's verarbeitet		*/
	
	switch (event_id (event)) {
	
	
	    case MS_LEFT   :
	    case MS_MIDDLE :
	
		if (event_is_down(event) && !dragging) {
			
			/* Neuer Knoten zu verschieben. Details stehen	*/
			/* in info.					*/
			
			if (info->what == MOVE_NODE) {
				/* Knoten bewegen	*/
				if (node == empty_node) {
					/* neu zu erzeugender Knoten,	*/
					/* existiert daher noch nicht	*/
					virtual_nodetype = use_nodetype (current_nodetype_index);
					virtual_nodetypeimage = use_nodetypeimage (virtual_nodetype, sx, sy);
				} else {
					/* "alter", bereits bestehender Knoten	*/
					event_set_x (event, node_x (node));
					event_set_y (event, node_y (node));
					virtual_nodetype = use_nodetype (get_nodetype_index (node->type));
					virtual_nodetypeimage = use_nodetypeimage (virtual_nodetype,
						node_width (node), node_height (node));
				}
			} else {
				/* Knotendimensionen veraendern	*/
				virtual_nodetype      = use_nodetype (get_nodetype_index (node->type));
				virtual_nodetypeimage = (Nodetypeimage)NULL;
				/* Zeichnen von Knoten in variabler Groesse ist zu langsam	*/
			}
			
			/* inserted MH 11/10/91 */
			call_user_event_func (UEV_DRAG_NODE, UEV_START, event, info);
			
			if (info->do_default_action) {
				/*
				image = (Nodetypeimage)NULL;
				*/
				image = virtual_nodetypeimage;
				draw_virtual_node (x,y, sx,sy, image);
			}
			dragging = TRUE;
			result = EVP_CONSUMED;
			
		} else if (event_is_up(event) && dragging) {
			
			/* Benutzer ist fertig --- Operation beenden	*/
			
			if (info->do_default_action) {
				erase_virtual_node (xe,ye, sxe,sye, image);
			}
			unuse_nodetypeimage (virtual_nodetype,
			                     virtual_nodetypeimage);
			unuse_nodetype      (virtual_nodetype);
			dragging = FALSE;
			result = EVP_FINISHED;
			
		} else if (event_is_up(event) && !dragging) {
		
			/* Benutzer hatte Fenster verlassen (?!?)	*/
			
			result = EVP_ERROR;
			
		} else /* event_is_down(event) && dragging */ {
		
			/* Fehler --- woher auch immer			*/
			
			bell ();
			if (info->do_default_action) {
				erase_virtual_node (xe,ye, sxe,sye, image);
			}
			unuse_nodetypeimage (virtual_nodetype,
			                     virtual_nodetypeimage);
			unuse_nodetype      (virtual_nodetype);
			dragging = FALSE;
			result = EVP_ERROR;
			
		}
		break;
	
	
	    case LOC_DRAG :
	    		
		if (dragging) {
		
			if (info->do_default_action) {
				erase_virtual_node (xe,ye, sxe,sye, image);
			}
			
			call_user_event_func (UEV_DRAG_NODE, UEV_DRAG, event, info);
			
			if (info->do_default_action) {
				/*
				image = (Nodetypeimage)NULL;
				*/
				draw_virtual_node  (x,y, sx,sy, image);
			}
			
			result = EVP_CONSUMED;
		} else {
			result = EVP_ERROR;
		}
		break;
	
#if FALSE
	    case LOC_STILL :
	    
		/* Die Maus hat angehalten. Jetzt wird			*/
		/* virtual_nodetypeimage gezeichnet. Beim Verschieben	*/
		/* ist dadurch der Knoten sichtbar.			*/
		
		if (dragging) {
		
			if (info->do_default_action) {
				erase_virtual_node (xe,ye, sxe,sye, image);
			}
			
			call_user_event_func (UEV_DRAG_NODE, UEV_DRAG, event, info);
			
			if (info->do_default_action) {
				image = virtual_nodetypeimage;
				draw_virtual_node  (x,y, sx,sy, image);
			}
			
			result = EVP_CONSUMED;
		} else {
			result = EVP_ERROR;
		}
		break;
#endif

/* MH conversion
	    case SCROLL_ENTER :
	    case LOC_RGNEXIT  :
*/

	    case LOC_WINEXIT  :
	    
		/* Cursor verlaesst das Arbeitsfenster			*/
		
		if (dragging) {
		
			/* Knotenziehen abbrechen - wer weiss, was	*/
			/* ausserhalb des Fensters passiert		*/
			bell ();
			if (info->do_default_action) {
				erase_virtual_node (xe, ye, sxe,sye, image);
			}
			unuse_nodetypeimage (virtual_nodetype,
			                     virtual_nodetypeimage);
			unuse_nodetype      (virtual_nodetype);
			dragging = FALSE;
			info->x = xe;
			info->y = ye;
			result = EVP_FINISHED;

		} else {
			result = EVP_ERROR;
		}
		break;
	
/* MH conversion
	    case SCROLL_EXIT  :
	    case LOC_RGNENTER :
*/
	    case LOC_WINENTER :
		result = EVP_ERROR;
		break;
	
	
	    default :
		bell ();
		result = EVP_ERROR;
		break;
	
	}
		
	if (dragging) {
		/* xe,ye, sxe,sye auffrischen	*/
		xe  = x;
		ye  = y;
		sxe = sx;
		sye = sy;
	}
		
	switch (result) {
	    case EVP_FINISHED :
		call_user_event_func (UEV_DRAG_NODE, UEV_FINISH, event, info);
		break;
	    case EVP_ERROR :
		call_user_event_func (UEV_DRAG_NODE, UEV_ERROR, event, info);
		break;
		
	}

	return result;
}



Evp_result	drag_edge_proc (window, event, info)
Xv_Window		window;
Event		*event;
Drag_edge_info	*info;
{
	static	int	x_old, y_old;
	static	int	dragging = FALSE;
	int		x,y; 
	static	int	x_pre,y_pre, x_suc,y_suc;
	static	int	drag_pre,    drag_suc;
	static	int	adjust_head, adjust_tail, adjust_head_and_tail;
	static	int	is_first_point_of_line, is_last_point_of_line;
	static	Node	source, target;
	
	/* Erlaeuterungen :						*/
	/*								*/
	/* x_old, y_old : die Punktkoordinaten beim letzten Durchlauf	*/
	/*	durch diese Prozedur.					*/
	/*								*/
	/* dragging : Um den zukuenftigen Kantenverlauf	*/
	/*	anzuzeigen, wird eine "virtuelle" Kante gezeichnet.	*/
	/*								*/
	/* x_pre,y_pre, x_suc,y_suc : die Koordinaten des vorher-	*/
	/*	gehenden und des nachfolgenden Punktes (sofern vor-	*/
	/*	handen).						*/
	/*								*/
	/* drag_pre, drag_suc : soll ein vor bzw. das nach dem Punkt	*/
	/*	liegendes Kantenstueck (graphisch) mitgezogen werden ?	*/
	/*								*/
	/* adjust_head, adjust_tail, adjust_head_and_tail : sollen der	*/
	/*	Kopf bzw. der Schwanz der Kante bzw. beide justiert	*/
	/*	werden ?						*/
	
	Evp_result	result = EVP_CONSUMED;
	
	
	/* Vorverarbeitung	*/
	if (constrain_is_active || shift_is_down) switch (info->what) {
	    case NEW_EDGE :
		constrain_event (event, x_pre, y_pre);
		break;
	    case OLD_EDGE_REAL_POINT :
		constrain_event (event, (int)info->which.real_point.el->x,
		                        (int)info->which.real_point.el->y);
		break;
	    case OLD_EDGE_IMAGINARY_POINT :
		constrain_event (event, (x_pre + x_suc)/2, (y_pre + y_suc)/2);
		break;
	}
	x = event_x (event);
	y = event_y (event);
	
	info->x = x;
	info->y = y;

	
	switch (event_id (event)) {
	
	    case MS_LEFT   :
	    case MS_MIDDLE :
	
		if (event_is_down(event) && !dragging) {
		
			Edge		edge;
			Edgeline	el;
			
			/* Neuer Durchlauf			*/
			
			/* Zuerst lokale Variablen besetzen	*/
			
			switch (info->what) {
			
			    case NEW_EDGE :
				el = info->which.new_edge.el;
				x_pre = el->x;
				y_pre = el->y;
				source = info->which.new_edge.source;
				target = empty_node;
				drag_pre = TRUE;
				drag_suc = FALSE;
				adjust_head = (el == el->suc);
				adjust_tail = FALSE;
				adjust_head_and_tail = FALSE;
				is_first_point_of_line = FALSE;
				is_last_point_of_line  = TRUE;
				break;
			
			    case OLD_EDGE_REAL_POINT :
				el   = info->which.real_point.el;
				edge = info->which.real_point.edge;
				x_pre = el->pre->x;
				y_pre = el->pre->y;
				x_suc = el->suc->x;
				y_suc = el->suc->y;
				source = edge->source;
				target = edge->target;
				drag_pre = (el != edge->line);
				drag_suc = (el != edge->line->pre);
				adjust_head = (el == edge->line->suc) || (el == edge->line);
				adjust_tail = (el == edge->line->pre) || (el == edge->line->pre->pre);
				adjust_head_and_tail = is_single_edgeline (el);
				is_first_point_of_line = (el == edge->line);
				is_last_point_of_line  = (el == edge->line->pre);
				break;

			    case OLD_EDGE_IMAGINARY_POINT :
				el   = info->which.imaginary_point.el;
				edge = info->which.imaginary_point.edge;
				x_pre = el->x;
				y_pre = el->y;
				x_suc = el->suc->x;
				y_suc = el->suc->y;
				source = edge->source;
				target = edge->target;
				drag_pre = TRUE;
				drag_suc = TRUE;
				adjust_head = (el == edge->line);
				adjust_tail = (el == edge->line->pre->pre);
				adjust_head_and_tail = FALSE;
				is_first_point_of_line = FALSE;
				is_last_point_of_line  = FALSE;
				break;
			}
			
			/* Kante an Knote justieren	*/
			
			if (adjust_head_and_tail) {
				if (is_first_point_of_line) {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL,
						source, &x,&y,
						&x_suc,&y_suc, target);
				} else {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL,
						source, &x_pre,&y_pre,
						&x,&y, target);
				}
			} else {
			    if (adjust_head) {
				if (is_first_point_of_line) {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD,
						source, &x,&y,
						&x_suc,&y_suc, empty_node);
				} else {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD,
						source, &x_pre,&y_pre,
						&x,&y, empty_node);
				}
			    }
			    if (adjust_tail) {
				if (is_last_point_of_line) {
					adjust_line_to_node (ADJUST_EDGELINE_TAIL,
						empty_node, &x_pre,&y_pre,
						&x,&y, target);
				} else {
					adjust_line_to_node (ADJUST_EDGELINE_TAIL,
						empty_node, &x,&y,
						&x_suc,&y_suc, target);
				}
			    }
			}
			
			info->x = x;
			info->y = y;
			
			call_user_event_func (UEV_DRAG_EDGE, UEV_START, event, info);
			
			/* Kantenverlauf zeichnen	*/
			
			if (info->do_default_action) {
				if (drag_pre)
					draw_virtual_line (x_pre,y_pre, x,y);
				if (drag_suc)
					draw_virtual_line (x,y, x_suc,y_suc);
			}
				
			dragging = TRUE;
			x_old = x;
			y_old = y;
		
			result = EVP_CONSUMED;
			
		} else if (event_is_up(event) && dragging) {
		
			/* Fertig	*/
			
			if (info->do_default_action) {
				if (drag_pre)
					erase_virtual_line (x_pre,y_pre, x_old,y_old);
				if (drag_suc)
					erase_virtual_line (x_suc,y_suc, x_old,y_old);
			}
			dragging = FALSE;
			result = EVP_FINISHED;
		
		} else if (event_is_up (event) && !dragging) {
		
			result = EVP_ERROR;
		
		} else /* event_is_down (event) && dragging */ {
		
			bell ();
			result = EVP_ERROR;
		
		}
		break;
	
	    case LOC_DRAG :
	    
		if (dragging) {
		
			/* Kantenverlauf loeschen	*/
			
			if (drag_pre)
				erase_virtual_line (x_pre,y_pre, x_old,y_old);
			if (drag_suc)
				erase_virtual_line (x_old,y_old, x_suc,y_suc);
			
			call_user_event_func (UEV_DRAG_EDGE, UEV_DRAG, event, info);
			
			/* Justieren	*/
			
			if (adjust_head_and_tail) {
				if (is_first_point_of_line) {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL,
						source, &x,&y,
						&x_suc,&y_suc, target);
				} else {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD_AND_TAIL,
						source, &x_pre,&y_pre,
						&x,&y, target);
				}
			} else {
			    if (adjust_head) {
				if (is_first_point_of_line) {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD,
						source, &x,&y,
						&x_suc,&y_suc, empty_node);
				} else {
					adjust_line_to_node (ADJUST_EDGELINE_HEAD,
						source, &x_pre,&y_pre,
						&x,&y, empty_node);
				}
			    }
			    if (adjust_tail) {
				if (is_last_point_of_line) {
					adjust_line_to_node (ADJUST_EDGELINE_TAIL,
						empty_node , &x_pre,&y_pre,
						&x,&y, target);
				} else {
					adjust_line_to_node (ADJUST_EDGELINE_TAIL,
						empty_node, &x,&y,
						&x_suc,&y_suc, target);
				}
			    }
			}
			
			/* Kantenverlauf zeichnen	*/
			
			if (info->do_default_action) {
				if (drag_pre)
					draw_virtual_line (x_pre,y_pre, x,y);
				if (drag_suc)
					draw_virtual_line (x,y, x_suc,y_suc);
			}
			
			x_old = x;
			y_old = y;
			result = EVP_CONSUMED;
			
		} else {
			result = EVP_ERROR;
		}
		break;
	
#if FALSE
	    case LOC_STILL :
	
		break;
#endif

	    case KEY_LEFT (10) :
		
		/* Loeschen	*/
		
		if (dragging) {
			if (info->do_default_action) {
				if (drag_pre)
					erase_virtual_line (x_pre,y_pre, x_old,y_old);
				if (drag_suc)
					erase_virtual_line (x_old,y_old, x_suc,y_suc);
			}
			dragging = FALSE;
			info->x = x_old;
			info->y = y_old;
			result = EVP_FINISHED;
		} else {
			result = EVP_ERROR;
		}
		break;

/* Conversion MH
	    case SCROLL_ENTER :
	    case LOC_RGNEXIT  :
*/
	    case LOC_WINEXIT  :
	    
	    	/* Fenster verlassen - ist ein Fehler	*/
	    	
		if (dragging) {
			bell ();
			if (info->do_default_action) {
				if (drag_pre)
					erase_virtual_line (x_pre,y_pre, x_old,y_old);
				if (drag_suc)
					erase_virtual_line (x_old,y_old, x_suc,y_suc);
			}
			dragging = FALSE;
			info->x = x_old;
			info->y = y_old;
			result = EVP_FINISHED;
		} else {
			result = EVP_ERROR;
		}
		break;
/* Conversion MH
	    case SCROLL_EXIT  :
	    case LOC_RGNENTER :
*/

	    case LOC_WINENTER :
		result = EVP_ERROR;
		break;
			
	    default :
		bell ();
		result = EVP_ERROR;
		break;
	
	}
	
	switch (result) {
	    case EVP_FINISHED :
		call_user_event_func (UEV_DRAG_EDGE, UEV_FINISH, event, info);
		break;
	    case EVP_ERROR :
		call_user_event_func (UEV_DRAG_EDGE, UEV_ERROR, event, info);
		break;
		
	}

	return	result;
}
Evp_result		drag_group_box_proc (window, event, info)
Xv_Window		window;
Event			*event;
Drag_group_box_info	*info;
{
	static	int	xe1,ye1, xe2,ye2;	/* e wie erase		*/
	static	int	dragging = FALSE;

	/* xe1,ye1, xe2,ye2 sind die Koordinaten, die die Box beim	*/
	/*	letzten Durchlauf durch diese Prozedur hatte. Sie	*/
	/*	werden gebraucht, um die "virtuelle" Gruppe, mit	*/
	/*	die Position angezeigt wird, wieder zu loeschen.	*/
	
	int		x1,y1, x2,y2;			/* aktuelle Position	*/
	Evp_result	result = EVP_CONSUMED;
	
	/* Bestimme Position			*/
	
	x1 = info->x1;
	y1 = info->y1;
	x2 = event_x (event);
	y2 = event_y (event);
	
	
	/* Position in info zurueckgeben	*/
	
	info->x2 = x2;
	info->y2 = y2;
	
	/* Jetzt werden die Event's verarbeitet		*/
	
	switch (event_id (event)) {
	
	    case MS_LEFT   :
	    case MS_MIDDLE :
	
		if (event_is_down(event) && !dragging) {
			
			draw_virtual_group_box (x1,y1, x2,y2);
			dragging = TRUE;
			result = EVP_CONSUMED;
			
		} else if (event_is_up(event) && dragging) {
			
			/* Benutzer ist fertig --- Operation beenden	*/
			
			erase_virtual_group_box (xe1,ye1, xe2,ye2);
			dragging = FALSE;
			result = EVP_FINISHED;
			
		} else if (event_is_up(event) && !dragging) {
		
			/* Benutzer hatte Fenster verlassen (?!?)	*/
			
			result = EVP_ERROR;
			
		} else /* event_is_down(event) && dragging */ {
		
			/* Fehler --- woher auch immer			*/
			
			bell ();
			dragging = FALSE;
			result = EVP_ERROR;
			
		}
		break;
	
	
	    case LOC_DRAG :
	    		
		if (dragging) {
			erase_virtual_group_box (xe1,ye1, xe2,ye2);
			draw_virtual_group_box  (x1,y1,   x2,y2);
		} else
			result = EVP_ERROR;
		break;
	
/* Conversion MH
	    case SCROLL_ENTER :
	    case LOC_RGNEXIT  :
*/
	    case LOC_WINEXIT  :
	    
		/* Cursor verlaesst das Arbeitsfenster			*/
		
		if (dragging) {
			/* Gruppenziehen abbrechen - wer weiss, was	*/
			/* ausserhalb des Fensters passiert		*/
			bell ();
			erase_virtual_group_box (xe1,ye1, xe2,ye2);
			dragging = FALSE;
		}
		result = EVP_ERROR;
		break;
	
/* Conversion MH
	    case SCROLL_EXIT  :
	    case LOC_RGNENTER :
*/

	    case LOC_WINENTER :
		result = EVP_ERROR;
		break;
	
	
	    default :
		bell ();
		result = EVP_ERROR;
		break;
	
	}
		
	if (dragging) {
		/* xe1,ye1, xe2,ye2 auffrischen	*/
		xe1 = x1;
		ye1 = y1;
		xe2 = x2;
		ye2 = y2;
	}
		
	return result;
}


Evp_result	drag_group_proc (window, event, info)
Xv_Window	window;
Event		*event;
Drag_group_info	*info;
{
	static	int	xe,ye;			/* e wie erase		*/
	static	int	dragging = FALSE;

	/* xe,ye sind die Koordinaten, die die Gruppe beim		*/
	/*	letzten Durchlauf durch diese Prozedur hatte. Sie	*/
	/*	werden gebraucht, um die "virtuelle" Gruppe, mit	*/
	/*	die Position angezeigt wird, wieder zu loeschen.	*/
	
	int		x,y;	/* aktuelle Position	*/
	Group		group;	/* DIE Gruppe		*/
	Evp_result	result = EVP_CONSUMED;
	
	
	group = info->group;
	
	/* Bestimme Position			*/
	
	event_set_x (event, event_x (event) - info->correction_x);
	event_set_y (event, event_y (event) - info->correction_y);
	if (constrain_is_active || shift_is_down)
		constrain_event (event, info->x0, info->y0);
	x = event_x (event);
	y = event_y (event);
	
	/* Position in info zurueckgeben	*/
	
	info->x = x;
	info->y = y;
	
	/* Jetzt werden die Event's verarbeitet		*/
	
	switch (event_id (event)) {
	
	    case MS_MIDDLE :
	
		if (event_is_down(event) && !dragging) {
						
			call_user_event_func (UEV_DRAG_GROUP, UEV_START, event, info);
			if (info->do_default_action) {
				draw_virtual_group (group, x - info->x0, y - info->y0);
			}
			dragging = TRUE;
			result = EVP_CONSUMED;

		} else if (event_is_up(event) && dragging) {
			
			/* Benutzer ist fertig --- Operation beenden	*/
			
			if (info->do_default_action) {
				erase_virtual_group (group, xe - info->x0, ye - info->y0);
			}
			dragging = FALSE;
			result = EVP_FINISHED;
			
		} else if (event_is_up(event) && !dragging) {
		
			/* Benutzer hatte Fenster verlassen (?!?)	*/
			
			result = EVP_ERROR;
			
		} else /* event_is_down(event) && dragging */ {
		
			/* Fehler --- woher auch immer			*/
			
			bell ();
			dragging = FALSE;
			result = EVP_ERROR;
			
		}
		break;
	
	
	    case LOC_DRAG :

		if (dragging) {		
			if (info->do_default_action) {
				erase_virtual_group (group, xe - info->x0, ye - info->y0);
			}
			
			call_user_event_func (UEV_DRAG_GROUP, UEV_DRAG, event, info);
			x = info->x;
			y = info->y;
			
			if (info->do_default_action) {
				draw_virtual_group  (group, x  - info->x0, y  - info->y0);
			}
			result = EVP_CONSUMED;
		} else {
			result = EVP_ERROR;
		}

		break;
	
/* MH conversion
	    case SCROLL_ENTER :
	    case LOC_RGNEXIT  :
*/	    
	    case LOC_WINEXIT  :
		/* Cursor verlaesst das Arbeitsfenster			*/
		
		if (dragging) {
			/* Gruppenziehen abbrechen - wer weiss, was	*/
			/* ausserhalb des Fensters passiert		*/
			
			bell ();
			if (info->do_default_action) {
				erase_virtual_group (group, xe - info->x0, ye - info->y0);
			}
			dragging = FALSE;
			info->x = xe;
			info->y = ye;
			result = EVP_FINISHED;
		} else {
			result = EVP_ERROR;
		}
		break;
	
/* MH Conversion
	    case SCROLL_EXIT  :
	    case LOC_RGNENTER :
*/

	    case LOC_WINENTER :
		result = EVP_ERROR;
		break;
	
	
	    default :
		bell ();
		result = EVP_ERROR;
		break;
	
	}
		
	/* xe,ye auffrischen	*/
	xe = x;
	ye = y;
	
	switch (result) {
	    case EVP_FINISHED :
		call_user_event_func (UEV_DRAG_GROUP, UEV_FINISH, event, info);
		break;
	    case EVP_ERROR :
		call_user_event_func (UEV_DRAG_GROUP, UEV_ERROR, event, info);
		break;
		
	}
	
	return result;
}
