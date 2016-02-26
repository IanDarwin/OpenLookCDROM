/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				select_mode.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Select-Modus des Benutzerinterfaces der working_area		*/
/*									*/
/************************************************************************/

#include "user_header.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Evp_result	select_mode_event_proc ()			*/
/*									*/
/*	char		*mini_textedit (text, input_character)		*/
/*									*/
/************************************************************************/


int	draging_node = FALSE;	/* Flag, ob gerade ein Knoten	*/
int	draging_edge = FALSE;	/* oder eine Kante gezogen wird	*/
	
int	draging_group = FALSE;		/* ... oder eine Gruppe	*/
int	draging_group_box = FALSE;	/* ... oder eine Box	*/
				/* zum Auswaehlen einer Gruppe	*/

static Evp_result	drag_group_box_proc        ();
static Evp_result	drag_group_proc            ();
static Group		make_group_of_nodes_in_box ();

/************************************************************************/
/*									*/
/*			SELECT - MODUS					*/
/*									*/
/************************************************************************/
/*									*/
/*	Evp_result	select_mode_event_proc (window, event, message)	*/
/*									*/
/*	Event-Prozedur fuer den 'Select-Modus'.				*/
/*									*/
/************************************************************************/



Evp_result		select_mode_event_proc (window, event, message)
Xv_Window			window;
Event			*event;
Message_to_event_proc	message;
{
	
	static Drag_edge_info		drag_edge_info;
	static Drag_node_info		drag_node_info;
	
	static Drag_group_info		drag_group_info;
	static Drag_group_box_info	drag_group_box_info;
	
	int		x,y;
	Evp_result	subevp_result;	/* Rueckgabe, wenn		*/
					/* drag_node_proc oder		*/
					/* drag_edge_proc aufgerufen	*/
					/* wurde.			*/
					
	Node		picked_node_of_group = empty_node;
					/* Wenn eine Gruppe angeklickt	*/
					/* wurde, gibt diese Variable	*/
					/* den betreffenden Knoten an.	*/
	
	Picklist	new_pl_head,
			new_picked_object;
	
	Picked_point_of_node		picked_node_point;
	Picked_point_of_edgeline	picked_el_point;
	
	Pick_mode	pick_mode;	/* Knoten / Kanten / alles	*/
					/* fuer Prozedur picker		*/
	
	/* messgaes verarbeiten	*/
	
	if (message == EVP_STARTUP) {
			
		draging_node      = FALSE;
		draging_edge      = FALSE;
		draging_group     = FALSE;
		draging_group_box = FALSE;	
		
		activate_menu_item (PUT_SELECTION);
		activate_menu_item (PUT_WHOLE_GRAPH);
		activate_menu_item (GET_SELECTION);
		activate_menu_item (GET_AS_GRAPH);
		activate_menu_item (SELECT_GRAPH_OF_SELECTION);
		activate_menu_item (SELECT_ALL);
		
		return EVP_OK;
	}
	
	if (message == EVP_SHUTDOWN) {
		if (draging_node || draging_edge || draging_group || draging_group_box)
			return EVP_VETO;
		else {
			inactivate_menu_item (PUT_SELECTION);
			inactivate_menu_item (PUT_WHOLE_GRAPH);
			inactivate_menu_item (GET_SELECTION);
			inactivate_menu_item (GET_AS_GRAPH);
			inactivate_menu_item (SELECT_GRAPH_OF_SELECTION);
			inactivate_menu_item (SELECT_ALL);
			return EVP_OK;
		}
	}
	
	
	/* Es bleibt uebrig : EVP_CONSUME	*/
	
	x = event_x(event);
	y = event_y(event);
	
	
	/* Wird ein Knoten oder eine Kante verschoben (bzw. ein Knoten	*/
	/* vergroessert oder verkleinert), so wickelt das		*/
	/* select_mode_event_proc ueber drag_node_proc oder		*/
	/* drag_edge_proc ab.						*/
	/* Die beiden folgenden Abschnitte erledigen diese Prozedur-	*/
	/* aufrufe und fragen das Ergebnis ab. WICHTIG : die Prozedur	*/
	/* wird sofort danach verlassen, der nachfolgende Code kommt	*/
	/* also nur ins Spiel, wenn nichts Verschoben (bzw.  		*/
	/* vergroessert oder verkleinert) wird.				*/
		
	if (draging_node) {
	
		subevp_result = drag_node_proc (window, event, &drag_node_info);
		
		switch (subevp_result) {
		
		    case EVP_FINISHED :
			
			if (event_id (event) != DEL && drag_node_info.what == MOVE_NODE) {
				/* Knoten umplazieren	*/
				if (drag_node_info.do_default_action) {
					dispatch_user_action (MOVE_SELECTION,
						drag_node_info.x - node_x(drag_node_info.node),
						drag_node_info.y - node_y(drag_node_info.node));
				}
			} else if (event_id (event) != DEL && drag_node_info.what == SCALE_NODE_MIDDLE) {
				/* Knoten punktsymmetrisch zum Mittelpunkt vergroessern / verkleinern	*/
				if (drag_node_info.do_default_action) {
					dispatch_user_action (RESIZE_SELECTION,
						(float)drag_node_info.sx / (float)node_width (drag_node_info.node),
						(float)drag_node_info.sy / (float)node_height(drag_node_info.node));
				}
			} else if (event_id (event) != DEL) {
				/* Knoten an einer Ecke vergroessern / verkleinern			*/
				if (drag_node_info.do_default_action) {
					dispatch_user_action (MOVE_SELECTION,
						drag_node_info.x - node_x(drag_node_info.node),
						drag_node_info.y - node_y(drag_node_info.node));
					dispatch_user_action (RESIZE_SELECTION,
						(float)drag_node_info.sx / (float)node_width (drag_node_info.node),
						(float)drag_node_info.sy / (float)node_height(drag_node_info.node));
				}
			}

			draging_node = FALSE;
			break;
		
		
		    case EVP_CONSUMED :
			break;
		
		    case EVP_ERROR :
			break;
			
		    default :
			break;
		}
		
		return subevp_result;
	}
	
	
	if (draging_edge) {
	
		subevp_result = drag_edge_proc (window, event, &drag_edge_info);
		
		switch (subevp_result) {
		
		    case EVP_FINISHED :
		
			if (event_id (event)!= DEL) switch (drag_edge_info.what) {
			
			    case OLD_EDGE_REAL_POINT :
				/* Punkt verschieben	*/
				if (drag_edge_info.do_default_action) {
					dispatch_user_action (MOVE_EDGE,
						drag_edge_info.which.real_point.el,    
						drag_edge_info.x,
						drag_edge_info.y);
				}
				draging_edge = FALSE;
				break;
			
			    case OLD_EDGE_IMAGINARY_POINT :
				/* neuen Punkt einfuegen	*/
				if (drag_edge_info.do_default_action) {
					dispatch_user_action (EXTEND_EDGE,
						drag_edge_info.which.imaginary_point.el,    
						drag_edge_info.x,
						drag_edge_info.y);
				}
				draging_edge = FALSE;
				break;
			}
			break;
		
		    case EVP_CONSUMED :
			break;
		
		    case EVP_ERROR :
			draging_edge = FALSE;
			break;
		
		    default :
			/* Go on */
			break;
		}
		
		return subevp_result;
	}
	
	
	if (draging_group_box) {
		
		Group	group;
		
		subevp_result = drag_group_box_proc (window, event, &drag_group_box_info);
		
		switch (subevp_result) {
		
		    case EVP_FINISHED :
			group = make_group_of_nodes_in_box (drag_group_box_info);
			if (group != empty_group) {
				if (shift_is_down)
					dispatch_user_action (EXTEND_SELECTION_WITH_GROUP, group);
				else
					dispatch_user_action (SELECT_GROUP, group);
			} else
				dispatch_user_action (UNSELECT);
			draging_group_box = FALSE;
			break;
			
		    case EVP_ERROR :
			draging_group_box = FALSE;
			break;
			
		    case EVP_CONSUMED :
			break;
			
		    default :
			break;
		}
		
		return subevp_result;
	}
	
	
	if (draging_group) {
	
		int	dx,dy;
		
		subevp_result = drag_group_proc (window, event, &drag_group_info);
		
		switch (subevp_result) {
		
		    case EVP_FINISHED :
			
			if (drag_group_info.do_default_action) {
				dx = drag_group_info.x - drag_group_info.x0;
				dy = drag_group_info.y - drag_group_info.y0;
				dispatch_user_action (MOVE_SELECTION, dx,dy);
			}
			free_group (drag_group_info.group);
			draging_group = FALSE;
			break;
		
		    case EVP_ERROR :
			draging_group = FALSE;
			break;
			
		    case EVP_CONSUMED :
			break;
			
		    default :
			break;
		}
		
		return subevp_result;
	}
	
	
	/*			*/
	/*  Event-Verarbeitung	*/
	/*			*/
	
	/* Im folgenden werden nur `down'-event's beachtet	*/
	if (event_is_up(event))
		return EVP_CONSUMED;
	
	/* Wenn der Benutzer ein Zeichen eingibt, fuer den	*/
	/* immediate_label_character = TRUE gilt, so wird es an	*/
	/* den Label von picked_objekt angehaengt (BACKSPACE	*/
	/* loescht)						*/
	
	switch (event_id(event)) {
	
	    case MS_LEFT   :
	
		/* neues Objekt anklicken	*/
		
		if (shift_is_down) /* meta may be down */
			pick_mode = PICK_NODE;
		else if (meta_is_down && ctrl_is_down)
			pick_mode = PICK_EDGE;
		else
			pick_mode = PICK_NODE_OR_EDGE;
		
		new_pl_head = xpicker (x,y, pick_mode);
			
		if (get_currently_edited_group () != empty_group) {
		
			/* Too complicated : the user could do a lot of	*/
			/* terrible things like modifying the group	*/
			bell ();
			free_picklist (new_pl_head);
			return EVP_ERROR;
			
		} else if (new_pl_head != (Picklist)NULL &&
		           (!shift_is_down || (shift_is_down && meta_is_down) || (meta_is_down && ctrl_is_down)) ) {
		
			/* irgendwas liegt unter dem cursor, keine group-manipulation */
			
			if (something_picked  &&
			    double_click      &&
			    picklist_contains_object (new_pl_head, picked_object) != (Picklist)NULL) {
			
				/* double click : editing	*/
				
				Double_click_info	info;
				
				info.do_default_action = TRUE;
				
				call_user_event_func (UEV_DOUBLE_CLICK, UEV_START, event, &info);
				if (info.do_default_action) {
					dispatch_user_action (EDIT_SELECTION);
				}
								
			} else {
			
				Click_info	info;
				
				info.do_default_action = TRUE;
				
				call_user_event_func (UEV_CLICK, UEV_START, event, &info);
				if (info.do_default_action) {
					dispatch_user_action (SELECT, dispatch_picklist (new_pl_head, TRUE));
				}
			}
			
		} else if (new_pl_head == (Picklist)NULL) {
		
			/* Waehle Gruppe von Knoten mittels box aus	*/
			/* This is done with drag_group_box_proc	*/
			
			drag_group_box_info.x1 = x;
			drag_group_box_info.y1 = y;
			drag_group_box_info.x2 = x;
			drag_group_box_info.y2 = y;
			drag_group_box_info.shift_is_down = shift_is_down;
			drag_group_box_info.do_default_action = TRUE;
			draging_group_box = TRUE;
			
			subevp_result = drag_group_box_proc (window, event, &drag_group_box_info);
			
			if (subevp_result == EVP_ERROR) {
				draging_group_box = FALSE;
				return EVP_ERROR;
			} else
				return subevp_result;
		
		} else if (new_pl_head != empty_picklist && shift_is_down) {
			
			/* Extend selection with a node	*/
			
			Group		group;
			Click_info	info;
			
			new_picked_object = dispatch_picklist (new_pl_head, TRUE);
			if (new_picked_object == (Picklist)NULL ||
			    new_picked_object->what != NODE_PICKED) {
				free_picklist (new_pl_head);
				return EVP_ERROR;
			}
			
			/* Jetzt gilt :					*/
			/* new_picked_object != (Picklist)NULL    &&	*/
			/* new_picked_object->what == NODE_PICKED	*/
			
			/* Berechne die zu bildende Gruppe		*/
			
			info.do_default_action = TRUE;
			call_user_event_func (UEV_CLICK, UEV_START, event, &info);
			if (info.do_default_action) {
				dispatch_user_action (EXTEND_SELECTION, new_picked_object);
			}
			free_picklist (new_pl_head);
			
		} else {
		
			/* Sinnlos danebengeklickt	*/
			
			if (meta_is_down) {
				bell ();
			}
			
		}
		break;
		
		
	    case MS_MIDDLE :
	
		/* Objekte verschieben / vergroessern / verkleinern	*/
		
		/* Zunaechst nachschauen, was unter dem Cursor liegt	*/
		
	    	new_pl_head = xpicker (x,y, PICK_NODE_OR_EDGE);
	    	
	    	
	    	/* new_pl_head untersuchen	*/
	    	
	    	if (get_currently_edited_group () != empty_group) {
		
			/* Too complicated : the user could do a lot of	*/
			/* terrible things like modifying the group	*/
			bell ();
			free_picklist (new_pl_head);
			return EVP_ERROR;
			
		} else if (new_pl_head == (Picklist)NULL) {
		
	    		dispatch_user_action (UNSELECT);	/* war wol nix	*/
	    		
		} else if (picklist_is_single (new_pl_head)) {
		
	   		/* genau ein Element angeklickt	*/
	   		if (something_picked                    &&
	   		    picked_object->what == GROUP_PICKED &&
	   		    new_pl_head->what   == NODE_PICKED  &&
	   		    contains_group_node (picked_object->which.group, new_pl_head->which.node) != empty_group){
	   		    	/* Der angeklickte Knoten ist in der aktuellen Gruppe enthalten, also	*/
	   		    	/* uebernehme die ganze Gruppe						*/
	   			picked_node_of_group = new_pl_head->which.node;
	   			free_picklist (new_pl_head);
	   		} else {
	   			/* neues Objekt auswaehlen	*/
	   			dispatch_user_action (SELECT, new_pl_head);
	   			picked_node_of_group = empty_node;
	   		}
	   		
		} else {
		
			/* mehrere Elemente angeklickt	*/
			if (something_picked) {
			
				Picklist	pl;
				
				if ((pl = picklist_contains_object (new_pl_head, picked_object)) == (Picklist)NULL) {
				
					/* das bisher ausgewaehlte Objekt ist nicht unter den	*/
					/* angeklickten - Pech gehabt.				*/
					bell ();
					dispatch_user_action (UNSELECT);
					free_picklist (new_pl_head);
					
				} else {
					/* OK, das bisher ausgewaehlte Objekt ist unter den	*/
					/* angeklickten - uebernehme es				*/
					
					/* Wenn eine group angeklickt ist, such einen Knoten,	*/
					/* der als picked_node_of_group gesetzt werdem kann	*/
					/* Wichtig fuer Verschieben von Gruppen MH 26/10/89	*/
					
	   				picked_node_of_group = empty_node;
					if (picked_object->what == GROUP_PICKED) {
						for (pl = new_pl_head; pl != empty_picklist; pl = pl->suc) {
							if (pl->what == NODE_PICKED &&
							    contains_group_node (picked_object->which.group, pl->which.node)) {
								picked_node_of_group = pl->which.node;
								break;
							}
						}
					}

					free_picklist (new_pl_head);
	    			}
	    			
	    		} else {
	    			/* something_picked = FALSE => Objekt kann nicht eindeutig	*/
	    			/* identifiziert werden - Pech gehabt				*/
	    			bell ();
	    			free_picklist (new_pl_head);
			}
		}
		
		/* Verarbeite jetzt picked_object	*/
		
		if (something_picked && picked_object->what == NODE_PICKED) {
		
			/* Vorbereitungen treffen fuer drag_node_proc	*/
			
			draging_node = TRUE;
			drag_node_info.node = picked_object->which.node;
			picked_node_point   = find_picked_point_of_node (picked_object->which.node, x,y);
			if (ctrl_is_down) switch (picked_node_point) {
			    case NO_NODE_POINT_PICKED :
				drag_node_info.what = SCALE_NODE_MIDDLE;
				break;
			    case UPPER_LEFT_POINT_PICKED :
				drag_node_info.what = SCALE_NODE_UPPER_LEFT;
				break;
			    case UPPER_RIGHT_POINT_PICKED :
				drag_node_info.what = SCALE_NODE_UPPER_RIGHT;
				break;
			    case LOWER_LEFT_POINT_PICKED :
				drag_node_info.what = SCALE_NODE_LOWER_LEFT;
				break;
			    case LOWER_RIGHT_POINT_PICKED :
				drag_node_info.what = SCALE_NODE_LOWER_RIGHT;
				break;
			} else
				drag_node_info.what = MOVE_NODE;
			
			/* drag_node_proc aufrufen	*/
			
			drag_node_info.do_default_action = TRUE;
			subevp_result = drag_node_proc (window, event, &drag_node_info);
			
			if (subevp_result == EVP_ERROR) {
				draging_edge = FALSE;
				return EVP_ERROR;
			} else {
				return subevp_result;
			}
			break;
			
		} else if (something_picked && picked_object->what == EDGE_PICKED) {
			
			/* Vorbereitungen treffen fuer drag_edge_proc	*/
			
			drag_edge_info.do_default_action = TRUE;
			
			picked_el_point = find_picked_point_of_edgeline (
				picked_object->which.edge->line, x,y);
			
			switch (picked_el_point.what) {
			
			    case NO_EDGE_POINT_PICKED   :
				break;
			
			    case REAL_POINT_PICKED :
				
				/* Bestehenden Punkt verschieben	*/
				
				drag_edge_info.what = OLD_EDGE_REAL_POINT;
				drag_edge_info.which.real_point.edge = picked_object->which.edge;
				drag_edge_info.which.real_point.el   = picked_el_point.which.real_point.el;
				draging_edge = TRUE;
				
				subevp_result = drag_edge_proc (window, event, &drag_edge_info);
				
				if (subevp_result == EVP_ERROR) {
					draging_edge = FALSE;
					return EVP_ERROR;
				} else {
					return subevp_result;
				}
				break;
			
			    case IMAGINARY_POINT_PICKED :
				
				/* Neuen Punkt einfuegen	*/
				
				drag_edge_info.what = OLD_EDGE_IMAGINARY_POINT;
				drag_edge_info.which.imaginary_point.edge = picked_object->which.edge;
				drag_edge_info.which.imaginary_point.el   = picked_el_point.which.imaginary_point.el;
				draging_edge = TRUE;
				
				subevp_result = drag_edge_proc (window, event, &drag_edge_info);
				
				if (subevp_result == EVP_ERROR) {
					draging_edge = FALSE;
					return EVP_ERROR;
				} else {
					return subevp_result;
				}
				break;
			}
			
		} else if (something_picked && picked_object->what == GROUP_PICKED) {
		
			/* Moving a group. If there is no particular node of a group picked,	*/
			/* then try to get one if the group consists of only one node -		*/
			/* else abort.								*/
			
			if (picked_node_of_group == empty_node)
				if (size_of_group (picked_object->which.group) == 1)
					picked_node_of_group = picked_object->which.group->node;
				else {
					bell ();
					return EVP_ERROR;
				}
			
			drag_group_info.group = copy_group(picked_object->which.group);
			drag_group_info.x  = x;
			drag_group_info.y  = y;
			drag_group_info.x0 = node_x(picked_node_of_group);
			drag_group_info.y0 = node_y(picked_node_of_group);
			drag_group_info.correction_x = x - node_x (picked_node_of_group);
			drag_group_info.correction_y = y - node_y (picked_node_of_group);
			drag_group_info.do_default_action = TRUE;
			draging_group = TRUE;
		
			subevp_result = drag_group_proc (window, event, &drag_group_info);
				
			if (subevp_result == EVP_ERROR) {
				draging_group = FALSE;
				return EVP_ERROR;
			} else {
				return subevp_result;
			}
			
		} else {
		
			return EVP_ERROR;
			
		}
		break;
	
	    case KEY_LEFT (10) :
	
		/* Loeschen	*/
		
		if (something_picked) {
		
			Edge		edge;
			Edgeline	el;
			
			switch (picked_object->what) {
			
			    case GROUP_PICKED :
			    case NODE_PICKED :
				dispatch_user_action (DELETE_SELECTION);
				break;
				
			    case EDGE_PICKED :
				edge = picked_object->which.edge;
				picked_el_point = find_picked_point_of_edgeline (edge->line, x,y);
				if (shift_is_down && picked_el_point.what == REAL_POINT_PICKED) {
					dispatch_user_action (
						COMPRIME_EDGE, picked_el_point.which.real_point.el);
				} else if (shift_is_down && picked_el_point.what != REAL_POINT_PICKED) {
					bell ();
				} else {
					/* shift => loesche ganze Kante	*/
					dispatch_user_action (DELETE_SELECTION);
				}
				break;
			}
		}
		break;


	    case KEY_LEFT (6) :
			
		/* PUT	*/
		if (!shift_is_down)
			dispatch_user_action (PUT_SELECTION);
		else
			dispatch_user_action (PUT_WHOLE_GRAPH);
		break;
	
	
	    case KEY_LEFT (8) :
			
		/* GET	*/
		
		if (!shift_is_down)
			dispatch_user_action (GET_SELECTION);
		else
			dispatch_user_action (GET_AS_GRAPH);
		break;
	
	
	    case LOC_DRAG  :
		break;
	

	    case LOC_WINEXIT  :
	    case LOC_WINENTER :

/* MH conversion
	    case SCROLL_ENTER :
	    case LOC_RGNEXIT  :
	    case SCROLL_EXIT  :
	    case LOC_RGNENTER :
*/
		break;
	
	    default :
		bell ();
		return EVP_ERROR;
  
	}
	
	
	return EVP_CONSUMED;
}


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
/*	static	Evp_result	drag_group_box (window, event, info)	*/
/*									*/
/*	Manages dragging a box for group selection.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	Evp_result	drag_group_proc (window, event, info)	*/
/*									*/
/*	Manages dragging a group.					*/
/*									*/
/************************************************************************/


static Evp_result	drag_group_box_proc (window, event, info)
Xv_Window			window;
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
			erase_virtual_group_box (xe1,ye1, xe2,ye2);
			dragging = FALSE;
		}
		result = EVP_ERROR;
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
		/* xe1,ye1, xe2,ye2 auffrischen	*/
		xe1 = x1;
		ye1 = y1;
		xe2 = x2;
		ye2 = y2;
	}
		
	return result;
}


static Evp_result	drag_group_proc (window, event, info)
Xv_Window		window;
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
/************************************************************************/
/*									*/
/*			    HILFSPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static Group		make_group_of_nodes_in_box (info)	*/
/*	Drag_group_box_info	info;					*/
/*									*/
/*	Erzeugt eine Group, die alle Knoten enthaelt, die sich in dem	*/
/*	Rechteck aus info befinden.					*/
/*	Generell werden alle Knoten, die das Rechteck beruehrt, in die	*/
/*	Group aufgenommen; Ausnahme sind linke Seiten von Produktionen,	*/
/*	die ganz im Rechteck liegen muessen.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	char	*mini_textedit (text, input_character)			*/
/*									*/
/*	(Einzeiliger) Mini- Texteditor, um Label an Knoten direkt	*/
/*	eingeben zu koennen. Der zurueckgegebene Text wird mit		*/
/*	mymalloc angelegt.						*/
/*									*/
/************************************************************************/



static Group		make_group_of_nodes_in_box (info)
Drag_group_box_info	info;
{
	Graph	graph;	/* Laufvariablen		*/
	Node	node;
	Group	group;	/* die zu erzeugende Gruppe	*/
	Rect	box;	/* das zu betrachtende Rechteck	*/
	
	int	x1 = info.x1, y1 = info.y1,
		x2 = info.x2, y2 = info.y2;
	
	rect_construct (&box,
		minimum (x1,x2), minimum (y1,y2),
		abs (x1-x2),     abs (y1-y2));
	group = empty_group;
	
	for_all_graphs (wac_buffer, graph)
	     for_nodes (graph, node)
		if ((!is_left_side_of_production(node) && rect_intersectsrect (&box, &(node->box))) ||
		    (is_left_side_of_production(node)  && rect_includesrect   (&box, &(node->box))) )
		    group = add_to_group (group, node);
	     end_for_nodes (graph, node)
	end_for_all_graphs (wac_buffer, graph);
	
	return group;
}



char	*mini_textedit (text, input_string)
char	*text;
char	*input_string;
{
	char	*new_text;
	char	*c, s[2];
	
	
	s[1] = '\0';	/* s is a string of exactly one character	*/
	
	if (text != NULL) {
		new_text = mymalloc (strlen (text) + strlen (input_string) + 1);
		strcpy (new_text, text);
	} else {
		new_text = mymalloc (strlen (input_string) + 1);
		strcpy (new_text, "");
	}
	
	for (c = input_string; *c != '\0'; c++) {
	
		s[0] = *c;
		
		if (s[0] == DEL) {
			if (strlen(new_text) > 0)
				new_text[strlen(new_text)-1] = '\0';
		} else {
			strcat (new_text, s);
		}
	}
	
	return	new_text;
}
