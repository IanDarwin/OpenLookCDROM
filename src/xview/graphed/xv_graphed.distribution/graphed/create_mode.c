/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				create_mode.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Node-Modus des Benutzerinterfaces der working_area		*/
/*									*/
/************************************************************************/

#include "user_header.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Evp_result	create_mode_event_proc  ()			*/
/*	Evp_result	drag_node_proc          ()			*/
/*	Evp_result	drag_edge_proc          ()			*/
/*									*/
/************************************************************************/

int	making_node = FALSE;
int	making_edge = FALSE;

/************************************************************************/
/*									*/
/*			KNOTEN BEARBEITEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Evp_result	create_mode_event_proc (window, event, message)	*/
/*									*/
/*	Event-Proc fuer den 'create'-Modus, in dem Knoten und Kanten	*/
/*	eingefuegt werden koennen.					*/
/*									*/
/*	Diese Prozedur verwendet die 'temporary_edgeline', um die	*/
/*	halbfertige Kante mitzeichnen zu lassen.			*/
/*									*/
/************************************************************************/



Evp_result		create_mode_event_proc (window, event, message)
Xv_Window		window;
Event			*event;
Message_to_event_proc	message;
{
	int			x,y;
	
	/* For nodes ...	*/
	static	Drag_node_info	drag_node_info = {MOVE_NODE, empty_node, 0,0, 0,0, 0,0};
	Node			node = empty_node;
	
	/* For edges ...	*/
	static Edgeline		el = (Edgeline)NULL, el_head = (Edgeline)NULL;
	static Node		source = empty_node, target = empty_node;
	static Drag_edge_info	drag_edge_info;
	Edge			edge = empty_edge;


	/* Zuerst : message abfragen	*/
	
	if (message == EVP_STARTUP) {
	
		/* Initialize for edges ...	*/
		source        = empty_node;
		target        = empty_node;
		making_edge   = FALSE;
		making_node   = FALSE;
		
		drag_node_info.what = MOVE_NODE;
		drag_node_info.node = empty_node;
				
		return EVP_OK;

	}
	
	if (message == EVP_SHUTDOWN) {
		if (making_node || making_edge ||
		    (last.graph != empty_graph && last.graph->is_production && graph_is_empty (last.graph)))
		    /* This second condition means that you try to abort the creation of the left side	*/
			
			return EVP_VETO;
		else
			return EVP_OK;
	}
	
	/* Jetzt : EVP_CONSUME						*/
	/* Ziehen von Knoten wird ueber drag_node_proc erledigt -	*/
	/* node_mode_event_proc erzeugt lediglich am Ende den Knoten.	*/
	
	x = event_x (event);
	y = event_y (event);

	/* A little preprocessing allows the user to label the last	*/
	/* created node directly, without entering select mode. It is	*/
	/* also possible to delete it, e.g. if the user made a mistake,	*/
	/* somehow.							*/
	
	if (!making_node && !making_edge) {
	
		if (event_id (event) == KEY_LEFT(10)) {
		
			if (something_picked) {
				dispatch_user_action (DELETE_SELECTION);
				return EVP_CONSUMED;
			} else {
				bell ();
				return EVP_ERROR;
			}
			
		}
	}
	
	
	/* Now the real create mode begins : creating nodes and edges.	*/
	
	switch (event_id(event)) {

	    case MS_LEFT :
		
		if (making_edge) {
		
			bell ();
			return EVP_ERROR;
			
		} else if (/* !making_edge && */ !making_node) {
			
			Picklist	pl;
			
			pl = xpicker (x,y, PICK_NODE_OR_EDGE);
			
			if (pl != empty_picklist) {
			
				if (pl->suc != empty_picklist) {
				
					free_picklist (pl);
					dispatch_user_action (SELECT_MODE);
					return EVP_CONSUMED;
					
				} else if (!(pl->what == NODE_PICKED && is_left_side_of_production(pl->which.node))) { 
					dispatch_user_action (SELECT_MODE);
					dispatch_user_action (SELECT, pl);
					return EVP_CONSUMED;
					
				}
				/* else you have clicked at the left side of a production	*/
				/* do NOT select it because we sant to create a node here	*/
			}
				
			/* Prepare making a node */
					
			/* First, check wether there is a graph ...	*/
					
			if (last.graph == empty_graph) {
				if (buffers[wac_buffer].graphs == empty_graph) {
					dispatch_user_action (CREATE_GRAPH);
				} else if (buffers[wac_buffer].graphs->suc == buffers[wac_buffer].graphs) {
					set_last_graph (buffers[wac_buffer].graphs);
				} else { notice_prompt (base_frame, NULL,	/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"Please give a graph to which the node shall belong.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
					dispatch_user_action (SELECT_MODE);
					return EVP_ERROR;
				}
			}
					
			dispatch_user_action (UNSELECT);
			drag_node_info.what = MOVE_NODE;
			drag_node_info.node = empty_node;
		}
		
		switch (drag_node_proc(window, event, &drag_node_info)) {
		
		    case EVP_FINISHED :
		    
			/* Benutzer ist mit dem Platzieren fertig - jetzt wird	*/
			/* der Knoten erzeugt					*/
			/* In drag_node_info.x, drag_node_info.x steht die	*/
			/* gewuenschte Position					*/
			
			dispatch_user_action (CREATE_NODE, last.graph, drag_node_info.x, drag_node_info.y);
		
			if (last.graph->is_production && is_left_side_of_production (last.node)) {
		
				/* The first node in a production is treated in	*/
				/* a special way : type is box, size is four	*/
				/* times the default, and the label is in the	*/
				/* upper right corner				*/
			
				node_set (last.node,
					SET_NODE_ATTRIBUTES (get_node_style (LEFT_SIDE_NODE_STYLE)),
					0);
			
			}		
			making_node = FALSE;
			break;
	
		    case EVP_ERROR :
			making_node = FALSE;
			return EVP_ERROR;
			break;
	
		    case EVP_CONSUMED :
			making_node = TRUE;
			break;
		
		    default :
			break;
		}

		return EVP_CONSUMED;
		
		break;
	
	
	    case MS_MIDDLE :
	    
	    	if (making_node) {
	    	
	    		bell ();
	    		return EVP_ERROR;
	    		
	    	}		
	    		    		
		if (event_is_down(event) && !making_edge) {
			
			Picklist	pl;
			
			/* Beginne eine neue Kante			*/
			/* Code analog zu oben; wird aber gebraucht,	*/
			/* um Kanten aus nur zwei Punkten eingeben zu	*/
			/* koennen					*/
			
			/* Suche zuerst Quellknoten	*/
			
			pl = xpicker (x,y, PICK_NODE);
			if (pl == empty_picklist || pl->what != NODE_PICKED || pl->suc != empty_picklist) {
				source = empty_node;
			} else {
				source = pl->which.node;
			}
			free_picklist (pl);
			
			if (source == empty_node)
				if (last.node == empty_node) {
					bell ();
					return EVP_ERROR;
				} else
					source = last.node;
			last.node = source;
			
			if (constrain_is_active || shift_is_down) {
				constrain_event (event, x,y);
				x = event_x (event);
				y = event_y (event);
			}
			
			dispatch_user_action (UNSELECT);
			
			/* Variablen vorbesetzen	*/
			temporary_edgeline = el = el_head = new_edgeline (x,y);
			adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD,
				empty_edge, source, empty_node);
			making_edge = TRUE;
			drag_edge_info.what = NEW_EDGE;
			drag_edge_info.which.new_edge.source = source;
			drag_edge_info.which.new_edge.el     = el;
			
			return drag_edge_proc (window, event, &drag_edge_info);
			
		} else if (event_is_down(event) && making_edge) {
		
			/* Neuer Punkt in der temporary_edgeline	*/
			return drag_edge_proc (window, event, &drag_edge_info);
						
		} else if (event_is_up(event) && making_edge) {
		
			/* Kante ist zu Ende !	*/
			
			if (drag_edge_proc (window, event, &drag_edge_info) == EVP_FINISHED) {
				
				/* Suche evtl. Zielknoten	*/
				
				Picklist	pl;
				
				pl = xpicker (x,y, PICK_NODE);
				if (pl == empty_picklist || pl->what != NODE_PICKED || pl->suc != empty_picklist) {
					target = empty_node;
				} else {
					target = pl->which.node;
				}
				free_picklist (pl);
				
				/*
				if (target == empty_node ||
				    (is_left_side_of_production(target) &&
				     (rect_includesrect(&(target->box), &(source->box)))) ) { 
				*/
				if (target == empty_node || (is_left_side_of_production(target)) ) { 
				
					/* Erzeuge neuen Punkt in der temporary_edgeline	*/
					
					x = drag_edge_info.x;
					y = drag_edge_info.y;
					el = add_to_edgeline (el, x,y);
					if (is_single_edgeline (el))
						/* Am Kantenanfanfang muss justiert werden	*/
						adjust_edgeline_to_node (ADJUST_EDGELINE_HEAD,
							empty_edge, source, empty_node);
					draw_single_edgeline (wac_buffer, el->pre);
					drag_edge_info.which.new_edge.el = el;
					return EVP_CONSUMED;

				} else if (source->graph != target->graph) {
					
					bell ();
					
					if (target != empty_node && source->graph != target->graph)
						notice_prompt (base_frame, NULL,		/*fisprompt*/
							NOTICE_MESSAGE_STRINGS,	"Source and Target belong to different Graphs.", NULL,
							NOTICE_BUTTON_YES,	"Ok",
							NULL);

					if (el_head->suc == el_head) {
						
						/* Up to now, the edge constisted of only one point.	*/
						/* Thus, it is easier for the user if we delete it, so	*/
						/* that he can easily select a new source.		*/
						
						erase_edgelines (wac_buffer, el_head);
						free_edgeline   (el_head);
						el_head = el       = (Edgeline)NULL;
						temporary_edgeline = (Edgeline)NULL;
						making_edge = FALSE;
						return EVP_ERROR;
					}
					
					return EVP_CONSUMED;
					
				}
				
			
				x = event_x (event);	/* constrain_event wird schon	*/
				y = event_y (event);	/* in drag_edge_proc aufgerufen	*/
				el = add_to_edgeline (el,x,y);
				
				/* Jetzt die neue Kante anlegen		*/
				/* adjust wird automatisch gemacht !	*/
				
				dispatch_user_action (CREATE_EDGE, source,target, el_head);

				/* Variablen zuruecksetzen	*/
				making_edge  = FALSE;
				last.node    = target;	/* merken fuer naechste Kante	*/
				el = el_head = (Edgeline)NULL;
				temporary_edgeline = (Edgeline)NULL;
				
				return EVP_FINISHED;
				
			} else
			
				return EVP_ERROR;
				
		} else /* event_is_up(event) && !making_edge	*/ {
			
			/*  Fehler - woher auch immer	*/
			
			return EVP_ERROR;
		}	
			
		break;
	
		
	    case LOC_DRAG :
	    
		if (making_edge && el != (Edgeline)NULL)
			return drag_edge_proc (window, event, &drag_edge_info);
		else if (making_node)
			return drag_node_proc (window, event, &drag_node_info);
		else
			return EVP_ERROR;
		break;
		
#if FALSE
	    case LOC_STILL :
		if (making_node)
			return drag_node_proc (window, event, &drag_node_info);
		break;
#endif


	    case KEY_LEFT (10) :
	    
		/* Loesche letzten Punkt	*/
		
		if (making_edge) {
		
			drag_edge_proc (window, event, &drag_edge_info);
			erase_single_edgeline (wac_buffer, el->pre);
			
			if (is_single_edgeline(el) || shift_is_down) {
			
				/* Loesche alles, da der letzte Punkt	*/
				/* sonst so leicht uebersehen wird.	*/
				erase_edgelines (wac_buffer, temporary_edgeline);
				free_edgeline (el);
				el_head = el = (Edgeline)NULL;
				temporary_edgeline = (Edgeline)NULL;
				making_edge = FALSE;
				
			} else {
			
				el = remove_from_edgeline (el);
				
			}
			
			if (el == (Edgeline)NULL) { /* Alles weg */
				making_edge = FALSE;
				temporary_edgeline = el_head = (Edgeline)NULL;
			} else {
				drag_edge_info.which.new_edge.el = el;
			}
		
		} else {
			/* Fehler	*/
			bell ();
			return EVP_ERROR;
		}
		break;


/* MH Conversion
	    case SCROLL_ENTER :
	    case LOC_RGNEXIT  :
*/
	    case LOC_WINEXIT  :
		/* Fenster verlassen	*/
		if (making_node)
			return drag_node_proc (window, event, &drag_node_info);
		else if (making_edge)
			return drag_edge_proc (window, event, &drag_edge_info);
		else
			return EVP_ERROR;
		break;

/* MH Conversion	
	    case SCROLL_EXIT  :
	    case LOC_RGNENTER :
*/
	    case LOC_WINENTER :
		return EVP_ERROR;
		break;
	
	    default :
		bell ();
		return EVP_ERROR;
		break;
	}
	
	return EVP_CONSUMED;
}
