/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				user.c					*/
/*									*/
/************************************************************************/
/*									*/
/*		Benutzerinterface der working_area			*/
/*									*/
/************************************************************************/

#include "user_header.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	working_area_event_proc ()				*/
/*									*/
/*	int	node_is_picked (node)					*/
/*	int	edge_is_picked (edge)					*/
/*	int	graph_is_picked (graph)					*/
/*									*/
/*	Node	get_picked_node ()					*/
/*	Edge	get_picked_edge ()					*/
/*	Graph	get_picked_graph ()					*/
/*									*/
/*	Node	get_last_node ()					*/
/*	Edge	get_last_edge ()					*/
/*	Graph	get_last_graph ()					*/
/*									*/
/*	void	init_user_interface ()					*/
/*									*/
/*	Rect	compute_rect_around_selection (selection)		*/
/*	Rect	compute_rect_around_current_selection ()		*/
/*									*/
/*======================================================================*/
/*									*/
/*	LOKAL INNERHALB DER MODULE DER BENUTZERSCHNITTSTELLE		*/
/*									*/
/*	void		constrain_event ()				*/
/*									*/
/*	int		remove_user_event_proc  ()			*/
/*	int		set_user_event_proc     (proc)			*/
/*	Evp_result	default_user_event_proc ()			*/
/*									*/
/*	Picklist	dispatch_picklist ()				*/
/*	void		pick              ()				*/
/*	void		unpick            ()				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*		(INNERHALB DER BENUZTERSCHNITTSTELLE)			*/
/*									*/
/************************************************************************/


Evp_result	(*user_event_proc)();

/*	Statusanzeigen fuer die event_proc's				*/

int	inside_scrollbar    = FALSE; 	/* Cursor der Scrollbar ?	*/
int	ms_left_down        = FALSE;
int	ms_middle_down      = FALSE;
int	ms_right_down       = FALSE;
int	shift_is_down       = FALSE;
int	ctrl_is_down        = FALSE;
int	meta_is_down        = FALSE;
int	last_event_id       = LOC_MOVE;
int	constrain_is_active = FALSE;	/* constrain =			*/
					/* Bewegungseinschraenkung	*/
int	double_click        = FALSE;

Node_or_edge	group_labelling_operation_goes_to = NODE;

/* 	Multi-Click Schranken						*/

int	multi_click_space;
int	multi_click_timeout;


/*	Momentan angeklickte Objekte					*/

Picklist	pl_head          = empty_picklist; /* Listenkopf	*/
Picklist	picked_object    = empty_picklist; /* Objekt		*/
int		something_picked = FALSE;

/*	pl_head ist der Kopf einer Liste mit all den Elementen, die	*/
/*		an der Anklickposition stehen.				*/ 
/*	picked_object ist das momentan angeklickte Objekt.		*/
/*		picked_object ist immer ein Zeiger auf ein Element aus	*/
/*		der Liste pl_head.					*/
/*	something_picked gibt an, ob ueberhaupt etwas angeklickt wurde.	*/
/*		pl_head und picked_object sind nur gueltig, falls	*/
/*		something_picked == TRUE.				*/


/*	last	ist eine Variable, die den zuletzt verwendeten Knoten,	*/
/*		die zuletzt verwendete Kante und den zuletzt		*/
/*		verwendeten Graphen enthaelt. "verwendet" bedeuted	*/
/*		entweder erzeugt oder angesprochen			*/

Last_worked_object	last;

/*	current_production ist die vom Benutzer momentan zur Anwendung 	*/
/*	bestimmte Graphgrammatikproduktion.				*/

Graph			current_production;


int	current_event_x;	/* Globals for those routines like	*/
int	current_event_y;	/* dispatch_user_action which do not	*/
int	current_event_id;	/* recieve the real event		*/
/* Also needed because the menue overwrites the original event		*/

Menu_called_from menu_called_from;


/************************************************************************/
/*									*/
/*			WORKING_AREA_EVENT_PROC				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	working_area_event_proc (window, event)			*/
/*									*/
/*	Hier ist die Haupt-Event-Prozedur der working_area.		*/
/*	Ihre Aufgabe besteht darin, die oben deklarierten		*/
/*	Satusanzeigen (ms_..._down, inside_scrollbar usw.) zu setzen	*/
/*	und einige wenige Events zu verwalten. Das Gros der Arbeit	*/
/*	wird an eine ueber user_event_proc angesprochene Prozedur	*/
/*	uebergeben (dazu siehe die folgenden Abschnitte).		*/
/*									*/
/*	Selbst verarbeitet werden hier folgende Events :		*/
/*	MS_RIGHT :	Menue aufklappen				*/
/*	    .								*/
/*	    .								*/
/*	    .								*/
/*	   usw.								*/
/*									*/
/*	Ignoriert werden :						*/
/*	LOC_MOVE, SCROLL_REQUEST, WIN_RESIZE, WIN_REPAINT, KBD_USE,	*/
/*	KBD_DONE.							*/
/*									*/
/*	Alle uebrigen Events werden, sofern sich der Cursor nicht	*/
/*	innerhalb einer Scrollbar befindet, an die user_event_proc	*/
/*	uebergeben.							*/
/*									*/
/************************************************************************/


void	working_area_event_proc (xv_window, event, arg)
Xv_Window	xv_window; /* MH */
Event		*event;
Notify_arg	arg;
{
	static	struct	timeval	last_click_event_time = {0,0};
	static	int		last_click_event_x,
				last_click_event_y;
	struct	timeval		this_event_time;
	int			illegal_event = FALSE;
	
	/* Added MH for Conversion			*/
	/* Determine the Canvas window to live like 	*/
	/* it was in SunView				*/
	/* Xview had the Canvas in the plave of		*/
	/* xv_window					*/

	Canvas	window;	/* Orig. Parameter	*/
	int	i;

	for (i=0; i<N_BUFFERS; i++) if (buffers[i].used) {
		if ((Xv_Window)(canvases[i].pixwin) == xv_window) {
			window = canvases[i].canvas;
			break;
		}
	}

	/* fprintf (stderr, "%d\n", event->ie_code); */

	/* Vorverarbeitung der Events	*/
	
	event_set_action (event, ACTION_NULL_EVENT);
	if (event_is_up (event) && !event_is_button(event))
		return;

	switch (event_id(event)) {
	    case WIN_RESIZE     :	/* werden nicht bearbeitet	*/
	    case WIN_REPAINT    :
	    case KBD_USE        :
	    case KBD_DONE       :
	    case SHIFT_LEFT     :
            case SHIFT_RIGHT    :
	    case SHIFT_LEFTCTRL :
            case SHIFT_RIGHTCTRL:
            case SHIFT_META     :
	    case SHIFT_ALT      :

		return;
		break;
	}
	
	if (test_user_interface_locked()) {
		force_repainting();
		return;
	}
	
	if ((Canvas)window != working_area_canvas &&
	    ((event_is_button(event)) ||
	     (event_is_ascii(event))  ||
	     (event_is_meta(event))   ||
	     (event_is_key_left(event))  ||
	     (event_is_key_top(event))   ||
	     (event_is_key_right(event)))
	    ) {
	
		/* Switch to a new canvas - reset it all	*/
		
		clean_buffer (wac_buffer);		

		set_working_area_canvas ((Canvas)window);
		
		last_click_event_time.tv_sec = 0;
		last_click_event_time.tv_usec = 0;
		
		if ( (buffers[wac_buffer].graphs != empty_graph) &&
		     (buffers[wac_buffer].graphs == buffers[wac_buffer].graphs->suc) )
			last.graph = buffers[wac_buffer].graphs;
		else
			last.graph = empty_graph;
		last.node = empty_node;
		last.edge = empty_edge;
	}
	
	current_event_x  = event_x(event);
	current_event_y  = event_y(event);
	current_event_id = event_id(event);
	
	switch (event_id(event)) {
	
	    case MS_LEFT :
		if (!ms_middle_down && !ms_right_down) {
			ms_left_down   = event_is_down(event);
			ms_middle_down = FALSE;
			ms_right_down  = FALSE;
		} else {
			illegal_event = TRUE;
			bell ();
		}
		break;
	    case MS_MIDDLE :
		if (!ms_left_down && !ms_right_down) {
			ms_left_down   = FALSE;
			ms_middle_down = event_is_down(event);
			ms_right_down  = FALSE;
		} else {
			illegal_event = TRUE;
			bell ();
		}
		break;
	    case MS_RIGHT :
		if (!ms_left_down && !ms_middle_down) {
			ms_left_down   = FALSE;
			ms_middle_down = FALSE;
			ms_right_down  = event_is_down(event);
		} else {
			illegal_event = TRUE;
			bell ();
		}
		break;

/* Commented out MH conversion
	    case SCROLL_ENTER :
		inside_scrollbar = TRUE;
		break;
	    case SCROLL_EXIT  :
		inside_scrollbar = FALSE;
		break;
*/
	    default :
		break;
	}
	
	this_event_time = event_time(event);
	double_click = ((ms_left_down || ms_middle_down || ms_right_down)           &&
			(this_event_time.tv_sec  - last_click_event_time.tv_sec) *1000 +
	                (this_event_time.tv_usec - last_click_event_time.tv_usec)/1000 <=
	                multi_click_timeout)                                        &&
	                (abs(event_x(event) - last_click_event_x) <= multi_click_space)   &&
	                (abs(event_y(event) - last_click_event_y) <= multi_click_space);
	shift_is_down = event_shift_is_down(event);
	ctrl_is_down  = event_ctrl_is_down(event);
	meta_is_down  = event_meta_is_down(event);
	
	
	menu_called_from = MENU_CALLED_FROM_CANVAS;
	
	if (!illegal_event &&
	    (event_is_ascii(event) && ctrl_is_down && event_id(event) != CTRL_M) ||
	    (event_is_meta (event)))
	switch (event_id (event)) {
	
	    case CTRL_A :
		if (shift_is_down)
			dispatch_user_action (SELECT_ALL);
		else
			dispatch_user_action (APPLY_CURRENT_PRODUCTION);
		break;
	
	    case META_A :
	    case 'a' :    /* META_ no longer valid in XView	*/
		dispatch_user_action (SELECT_ALL);
		break;
	
	    case CTRL_B :
		dispatch_user_action (CREATE_BUFFER);
		break;
	
	    case CTRL_C :
		dispatch_user_action (CENTER_SELECTION);
		break;
	
	    case CTRL_D :
		dispatch_user_action (CREATE_DIRECTED_GRAPH);
		break;
	
	    case CTRL_U :
		dispatch_user_action (CREATE_UNDIRECTED_GRAPH);
		break;
	
	    case CTRL_E :
		if (shift_is_down)
			dispatch_user_action (EDIT_GRAPH_OF_SELECTION);
		else
			dispatch_user_action (EDIT_SELECTION);
		break;
		
	    case META_E :
	    case 'e'    :
		dispatch_user_action (EDIT_GRAPH_OF_SELECTION);
		break;
		
	    case CTRL_G :
		dispatch_user_action (SELECT_GRAPH_OF_SELECTION);
		break;
		
	    case CTRL_L :
		dispatch_user_action (LOAD_AGAIN);
		break;
		
	    case META_L :
	    case 'l'    :
		dispatch_user_action (LOAD_BY_SUBFRAME);
		break;
		
	    case CTRL_N :
		dispatch_user_action (CREATE_GRAPH);
		break;
		
	    case CTRL_P :
		dispatch_user_action (CREATE_PRODUCTION);
		break;
	
	    case META_P :
	    case 'p'    :
		dispatch_user_action (CREATE_EMBEDDING);
		break;
	
	    case CTRL_Q :
		dispatch_user_action (SET_CURRENT_PRODUCTION);
		break;
	
	    case CTRL_R :
		dispatch_user_action (REVERSE_EDGE);
		break;
		
	    case META_R :
	    case 'r' :
		dispatch_user_action (SWAP_SELECTED_GRAPH_DIRECTEDNESS);
		break;
	
	    case CTRL_S :
		dispatch_user_action (STORE_TO_SAME_FILE);
		break;
			
	    case META_S :
	    case 's'    :
		dispatch_user_action (STORE_BY_SUBFRAME);
		break;
			
	    case CTRL_V :
		dispatch_user_action (SPLIT_SELECTION);
		break;
			
	    case META_V :
	    case 'v'    :
		dispatch_user_action (MERGE_SELECTION);
		break;
			
	    case CTRL_W :
		dispatch_user_action (CREATE_BUFFER);
		break;
			
	    case '.' :	/* = ctrl-'>'	*/
		dispatch_user_action (EXPAND_SELECTION);
		break;
		
	    case ',' :	/* = ctrl-'<'	*/
		dispatch_user_action (SHRINK_SELECTION);
		break;
	
	    default :
		break;
	
	} else if (!illegal_event && event_is_ascii (event) &&
		    (isprint (event_id(event))   ||
		     event_id (event) == '\t'    ||
		     event_id (event) == CTRL_M  ||
		     event_id(event) == DEL)) {
		
			char	c[2];
		
			c[0] = (char)event_id(event); c[1] = '\0';
			
			if (something_picked) {
				dispatch_user_action (LABEL_SELECTION, c);
			} else {
				bell ();
			}

	} else if (!illegal_event) switch (event_id(event)) {
	
	    case MS_RIGHT :
		/* Zeige Menue	*/
		if (event_is_down(event) &&
		    !draging_node  && !draging_edge &&
		    !draging_group && !draging_group_box &&
		    !making_node && !making_edge) {
			menu_show (main_menu,
				xv_window,
				event,
				0);
		}
		/* menu_show konsumiert MS_RIGHT beim loslassen	*/
		ms_right_down = FALSE;
		break;
		    
	    case KEY_RIGHT (1) :
		dispatch_user_action (SET_GRID_16_16);
		break;
	    case KEY_RIGHT (2) :
		dispatch_user_action (SET_GRID_32_32);
		break;
	    case KEY_RIGHT (3) :
		dispatch_user_action (SET_GRID_OFF);
		break;
	
	    case KEY_TOP (1) :
		if (user_event_proc == create_mode_event_proc)
			dispatch_user_action (SELECT_MODE);
		else if (user_event_proc == select_mode_event_proc)
			dispatch_user_action (CREATE_MODE);
		break;
				
	    case KEY_TOP (2)     :
	    	dispatch_user_action (TOGGLE_CONSTRAINED);
	    	break;
	    	
	    case KEY_TOP (3)     :
	    	dispatch_user_action (TOGGLE_GROUP_LABELLING_OPERATION);
	    	break;
	    	
	    case KEY_LEFT (1) :
	    case KEY_LEFT (3) :
	    case KEY_LEFT (5) :
	    case KEY_LEFT (7) :
	    case KEY_LEFT (9) :
		break;
	
	    case KEY_LEFT (6) :
		if (meta_is_down) {
			dispatch_user_action (PUT_WHOLE_GRAPH);
		} else {
			dispatch_user_action (PUT_SELECTION);
		}
		break;
	
	    case KEY_LEFT (8) :
		if (meta_is_down) {
			dispatch_user_action (GET_AS_GRAPH);
		} else {
			dispatch_user_action (GET_SELECTION);
		}
		break;

	    case ESC :
		dispatch_user_action (COMPILE_ALL_PRODUCTIONS);
		break;

/* Commented out MH conversion
	    case SCROLL_ENTER :
	    case SCROLL_EXIT  :
		(void)user_event_proc (window, event, EVP_CONSUME);
		break;
*/
	    case LOC_MOVE       :	/* Diese Event's werden nicht	*/
	    				/* an user_event_proc weiter-	*/

/* Commented out MH conversion
	    case SCROLL_REQUEST 
*/
		break;
	
	
	    default :
		if (!inside_scrollbar)
			(void)user_event_proc (window, event, EVP_CONSUME);
		break;
	}
	
	if (!illegal_event) {
		if (ms_left_down || ms_middle_down || ms_right_down) {
			last_click_event_time = this_event_time;
			last_click_event_x    = event_x (event);
			last_click_event_y    = event_y (event);
		}
		last_event_id   = event_id (event);
	}
	
	
	/* Evtl. geaenderte Teile des Graphen neuzeichnen		*/
	/* Diese Prozedur braucht an keiner anderen Stelle in user.c	*/
	/* aufgerufen zu werden						*/
	
	force_repainting ();
	
	menu_called_from = MENU_CALLED_FROM_NOWHERE;

}
/************************************************************************/
/*									*/
/*			EVENT_PROC(EDUREN) VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	Fuer die Benuzterschnittstelle gibt es von GraphEd aus eigene	*/
/*	Event-Verwaltungsprozeduren, die von working_area_event_proc	*/
/*	aus aufgerufen werden. Zu einen Zeitpunkt ist immer genau eine	*/
/*	aktiv, naehmlich die, die mit user_event_proc angesprochen	*/
/*	wird.								*/
/*	Bis jetzt gibt es drei solcher Prozeduren :			*/
/*	- put_node_event_proc  (`Node-Modus' zum Zeichnen von Knoten)	*/
/*	- make_edge_event_proc (`Edge-Modus' zum Erstellen von Kanten)	*/
/*	- select_event_proc    (`Select-Modus')				*/
/*									*/
/*	Zum Setzen un Loeschen solcher Prozeduren gibt es		*/
/*	remove_user_event_proc und set_user_event_proc.			*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	int	remove_user_event_proc ()			*/
/*									*/
/*	Entfernt user_event_proc, falls sie das erlaubt (EVP_OK).	*/
/*	An ihrer Stelle wird default_user_event_proc eingesetzt.	*/
/*	Rueckgabe TRUE / FALSE, je nachdem loechsen der Prozedur	*/
/*	erfolgreich war oder nicht.					*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	int	set_user_event_proc (proc)			*/
/*									*/
/*	Setzt proc als user_event_proc ein. Falls Das Einsetzen		*/
/*	erfolgreich war, Rueckgabe TRUE, sonst FALSE (und Einsetzen	*/
/*	von default_user_event_proc als Ersatz).			*/
/*									*/
/*	WICHTIG : Die alte user_event_proc muss vorher mit der		*/
/*	Prozedur remove_user_event_proc (erfolgreich) entfernt		*/
/*	worden sein.							*/
/*									*/
/************************************************************************/



int	remove_user_event_proc ()
{
	if (user_event_proc ((Xv_Window)NULL,(Event *)NULL,EVP_SHUTDOWN)
	    == EVP_OK) {
		user_event_proc = default_user_event_proc;
		return TRUE;
	} else
		return FALSE;
}



int	set_user_event_proc (proc)
Evp_result	(*proc)();
{
	if (proc (working_area_canvas, (Event *)NULL, EVP_STARTUP) == EVP_OK) {
		user_event_proc = proc;
		return TRUE;
	} else
		return FALSE;
}
/************************************************************************/
/*									*/
/*	USER - EVENT-PROCS IM ALLGEMEINEN UND DEFAULT_USER_EVENT_PROC	*/
/*				BESONDEREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Der Kopf einer User-event-proc hat folgenden Aufbau :		*/
/*									*/
/*	Evp_result	proc (window, event, message)			*/
/*	Xv_Window	window;						*/
/*	Event		*event;						*/
/*	Message_to_event_proc	message;				*/
/*									*/
/*	wobei Message_to_event_proc und Evp_result folgende Gestalt	*/
/*	haben :								*/
/*									*/
/*	typedef	enum {							*/
/*		EVP_STARTUP,	Hochfahren				*/
/*		EVP_SHUTDOWN,	Prozedur verlassen			*/
/*		EVP_CONSUME	event verarbeiten			*/
/*		}							*/
/*		Message_to_event_proc;					*/
/*									*/
/*	Bei EVP_STARTUP und EVP_SHUTDOWN ist der Parameter event	*/
/*	bedeutungslos; ist Hoch- bzw. Herunterfahren nicht moeglich,	*/
/*	so wird EVP_VETO (s.u.) zurueckgegeben.				*/
/*									*/
/*	typedef	enum {							*/
/*		EVP_CONSUMED,	Event problemlos abgearbeitet		*/
/*		EVP_FINISHED,	consumed + Aktion beendet		*/
/*				(z.B. Kante fertig)			*/
/*		EVP_ERROR,	Fehler aufgetreten			*/
/*									*/
/*		EVP_OK,		Startup / Shutdown o.k.			*/
/*		EVP_VETO	Startup / Shutdown fehlgeschlagen	*/
/*		}							*/
/*		Evp_result;						*/
/*									*/
/*	Die ersten drei beziehen sich auf EVP_CONSUME, die beiden	*/
/*	letzten auf EVP_STARTUP und EVP_SHUTDOWN.			*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	Evp_result	default_user_event_proc (window,	*/
/*				event, message)				*/
/*									*/
/*	Dummy - Prozedur. Weist alle Events zurueck (EVP_ERROR) ausser	*/
/*	LOC_WINENTER, LOC_RGNENTER, LOC_WINEXIT, LOC_RGNEXIT,		*/
/*	SCROLL_ENTER, SCROLL_EXIT und SCROLL_REQUEST.			*/
/*									*/
/************************************************************************/



Evp_result	default_user_event_proc (window, event, message)
Xv_Window		window;
Event			*event;
Message_to_event_proc	message;
{
	if (message == EVP_STARTUP || message == EVP_SHUTDOWN)
		return EVP_OK;
	
	/* Es bleibt uebrig : EVP_CONSUME */
	
	switch (event_id(event)) {
	
	    case LOC_WINENTER   :
	    case LOC_WINEXIT    :
/* Commented out MH conversion
	    case LOC_RGNENTER   :
	    case LOC_RGNEXIT    :
	    case SCROLL_ENTER   :
	    case SCROLL_EXIT    :
	    case SCROLL_REQUEST :
		break;
*/
	    default :
		bell ();
		return EVP_ERROR;
		break;
	}
	
	return EVP_CONSUMED;
}
/************************************************************************/
/*									*/
/*			HILFSPROZEDUREN					*/
/*									*/
/************************************************************************/
/*									*/
/*	Picklist	dispatch_picklist (pl, complain_if_empty)	*/
/*									*/
/*	Geht die Liste pl durch und klaert Mehrdeutigkeit evtl.		*/
/*	interaktiv. Algorithmus :					*/
/*	- leere Liste gilt als Fehler, falls complain_if_empty		*/
/*	  gesetzt ist.							*/
/*	- wenn pl nur ein Element enthaelt - o.k.			*/
/*	- ist pl mehrdeutig ( > 1 Element), so wird interaktiv (ueber	*/
/*	  xprompt) abgefragt, welches Objekt nun gemeint war, falls	*/
/*	  die CTRL-Taste gedrueckt ist.					*/
/*	Im Fehlerfall wird ein Klingelzeichen als Warnsignal		*/
/*	ausgegeben.							*/
/*									*/
/*	Rueckgabe : das letztendlich ausgewaehlte Objekt oder aber	*/
/*	empty_picklist.							*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	int	node_is_picked (node)					*/
/*	int	edge_is_picked (edge)					*/
/*	int	graph_is_picked (graph)					*/
/*									*/
/*	Ueberpruefen, ob das momentan angeklickte Objekt		*/
/*	(picked_object) mit node bzw. edge bzw. graph uebereinstimmt.	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Node	get_picked_node  ()					*/
/*	Edge	get_picked_edge  ()					*/
/*	Graph	get_picked_graph ()					*/
/*									*/
/*	Liefern den gerade angeklickten Knoten bzw. Graph bzw. die	*/
/*	gerade angeklickte Kante.					*/
/*	Diese Prozeduren sind fuer den Export (in andere Module)	*/
/*	bestimmt.							*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Node	get_last_node  ()					*/
/*	Edge	get_last_edge  ()					*/
/*	Graph	get_last_graph ()					*/
/*									*/
/*	void	set_last_node  (node)					*/
/*	void	set_last_edge  (edge)					*/
/*	void	set_last_graph (graph)					*/
/*									*/
/*	Liefern oder setzen den zuletzt verwendeten Knoten bzw. Graph	*/
/*	bzw. die gerade angeklickte Kante.				*/
/*	Diese Prozeduren sind fuer den Export (in andere Module)	*/
/*	bestimmt.							*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	unpick ()						*/
/*									*/
/*	picked_object wird unmarkiert, pl_head freigegeben und		*/
/*	something_picked auf FALSE gesetzt.				*/
/*									*/ 
/************************************************************************/



Picklist	dispatch_picklist (pl, complain_if_empty)
Picklist	pl;
int		complain_if_empty;
{	
	if (pl == empty_picklist && complain_if_empty) {
		bell ();
	} else if (pl->suc == empty_picklist) {
		mark_picked_object (pl);
	} else {
	
		if (ctrl_is_down) {
		
			/* Select "by hand" - the computer has done its	*/
			/* best and is at the end of its latin resp. C.	*/
			
			Rect	rect_around_picklist;
			int	x,y;
			
			/* Koordinaten fuer Abfragebox ermitteln	*/
			
			rect_around_picklist = compute_bounding_rect_of_picklist (pl);
			x = rect_right  (&rect_around_picklist)     + 10;
			y = rect_top    (&rect_around_picklist) +
			    rect_height (&rect_around_picklist) / 2 + 10;
			translate_wac_to_base_frame_space (&x,&y);
			
			/* pl der Reihe nach abfragen (mittels xprompt)	*/
			
			while (pl != empty_picklist) {
				mark_picked_object (pl);
				switch (notice_prompt (base_frame, NULL,	/*fisprompt*/
					NOTICE_FOCUS_XY,	x, y,
					NOTICE_MESSAGE_STRINGS,	"Take this object ?", NULL,
					NOTICE_BUTTON,		"Yes",		PROMPT_ACCEPT,
					NOTICE_BUTTON,		"No",		PROMPT_REFUSE,
					NOTICE_BUTTON,		"Cancel",	PROMPT_CANCEL,
					NULL)) {
				    case PROMPT_ACCEPT :
					return pl;
					break;
				    case PROMPT_REFUSE :
					unmark_picked_object (pl);
					pl = pl->suc;
					break;
				    case PROMPT_CANCEL :
					unmark_picked_object (pl);
					free_picklist (pl);
					pl = empty_picklist;
					break;
				}
			}
		} else {
			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Please make an unambigous selection.",
							"Use 'meta+ctrl', to select only edges,",
							"'shift+ctrl' to select only nodes or",
							"'ctrl' to examine all objects at this place.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
			free_picklist (pl);
			pl = empty_picklist;
		}
	}
	
	return pl;
}



int	node_is_picked (node)
Node	node;
{
	return something_picked &&
	       ((picked_object->what == NODE_PICKED  && picked_object->which.node == node) ||
	        (picked_object->what == GROUP_PICKED && contains_group_node (picked_object->which.group, node)));
}


int	edge_is_picked (edge)
Edge	edge;
{
	return something_picked                   &&
	       picked_object->what == EDGE_PICKED &&
	       picked_object->which.edge == edge;
}


int	graph_is_picked (graph)
Graph	graph;
{
	return something_picked &&
	       ((picked_object->what == NODE_PICKED &&
	         picked_object->which.node->graph == graph) ||
	        (picked_object->what == EDGE_PICKED &&
	         picked_object->which.edge->source->graph == graph) ||
	        (picked_object->what == GROUP_PICKED &&
	         group_nodes_are_all_of_same_graph(picked_object->which.group) &&
	         picked_object->which.group->node->graph == graph));
}



Picklist	get_picked_object ()
{
	if (something_picked)
		return picked_object;
	else
		return empty_picklist;
}



Node	get_picked_node  ()
{
	if (something_picked && picked_object->what == NODE_PICKED)
		return picked_object->which.node;
	else
		return empty_node;
}


Edge	get_picked_edge  ()
{
	if (something_picked && picked_object->what == EDGE_PICKED)
		return picked_object->which.edge;
	else
		return empty_edge;
}


Graph	get_picked_graph  ()
{
	if (something_picked) switch (picked_object->what) {
	    case NODE_PICKED :
		return picked_object->which.node->graph;
	    case EDGE_PICKED :
		return picked_object->which.edge->source->graph;
	    case GROUP_PICKED :
		if (group_nodes_are_all_of_same_graph(picked_object->which.group))
			return picked_object->which.group->node->graph;
		else
			return empty_graph;
	} else
		return empty_graph;
}


Graph	get_picked_or_only_existent_graph ()
{
	Graph	graph;
	
	graph = get_picked_graph ();
	
	if (graph == empty_graph) {
		if (buffers[wac_buffer].graphs != empty_graph &&
		    buffers[wac_buffer].graphs->suc == buffers[wac_buffer].graphs)
			graph = buffers[wac_buffer].graphs;
	}
	
	return graph;
}


Node	get_last_node ()
{
	return last.node;
}


Edge	get_last_edge ()
{
	return last.edge;
}


Graph	get_last_graph ()
{
	return last.graph;
}



void	set_last_node (node)
Node	node;
{
	last.node = node;
}


void	set_last_edge (edge)
Edge	edge;
{
	last.edge = edge;
}


void	set_last_graph (graph)
Graph	graph;
{
	last.graph = graph;
}



void		pick (object)
Picklist	object;
{	
	picked_object = object;
	something_picked = (picked_object != empty_picklist);
	
	if (something_picked){
		switch (picked_object->what) {
		    case NODE_PICKED:
			last.node  = picked_object->which.node;
			last.graph = last.node->graph;
			break;
		    case EDGE_PICKED:
			last.edge  = picked_object->which.edge;
			last.graph = last.edge->source->graph;
			break;
		}
		mark_picked_object (picked_object);
	}
}


void	unpick ()
{
	if (something_picked) {
		unmark_picked_object (picked_object);
		if (picked_object->what == GROUP_PICKED)
			free_group (picked_object->which.group);
		free_picklist (pl_head);
		something_picked = FALSE;
	}
}
/************************************************************************/
/*									*/
/*			HILFSPROZEDUREN					*/
/*									*/
/************************************************************************/
/*									*/
/*	void	constrain_event (event, x,y)				*/
/*									*/
/*	Implementiert die Bewegungseinschraenkung des Cursors.		*/
/*	Dazu werden die Koordinaten IN EVENT entsprechend abgeaendert	*/
/*									*/
/*	Es gibt zwei Varianten :					*/
/*	- Ist ein Gitter vorhanden (gridwidth > 0, aus draw.c), so	*/
/*	  sind nur Gitterpunkte zulaessig. Die Parameter x und y haben	*/
/*	  in diesem Fall keine Bedeutung.				*/
/*	- sonst kann event nur Koordinaten annehmen, deren		*/
/*	  Verbindungslinie dem Punkt (x,y) horizontal, vertikal oder	*/
/*	  schraeg unter 45 Grad verlaeuft. Die Arbeit uebernimmt hier	*/
/*	  die Prozedur constrain_8 (-> misc.c)				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	constrain_length_event (event, x,y)			*/
/*									*/
/*	Wie oben, aber der im Gitterfall soll die Distanz zu (x,y)	*/
/*	in Gittereinheiten sein.					*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	init_user_interface ()					*/
/*									*/
/*	Faehrt die Benutzerschnittstelle hoch.				*/
/*									*/
/************************************************************************/


void	constrain_event (event, x,y)
Event	*event;
int	x,y;
{
	int	evx, evy;
	int	gridwidth = get_gridwidth (wac_buffer);
	
	evx = event_x (event);
	evy = event_y (event);
	
	if (gridwidth > 0) {
		constrain_to_grid (gridwidth, &evx, &evy);
	} else
		constrain_8 (x,y, &evx, &evy);

	event_set_x (event, evx);
	event_set_y (event, evy);
}



void	constrain_length_event (event, x,y)
Event	*event;
int	x,y;
{
	int	evx, evy, tmp_x, tmp_y;
	int	gridwidth = get_gridwidth (wac_buffer);
	
	evx = event_x (event);
	evy = event_y (event);
	
	if (gridwidth > 0) {
		tmp_x = x - evx;
		tmp_y = y - evy;
		constrain_to_grid_in_one_dimension (gridwidth, &tmp_x);
		constrain_to_grid_in_one_dimension (gridwidth, &tmp_y);
		evx = x + tmp_x;
		evy = y + tmp_y;
	} else {
		constrain_8 (x,y, &evx, &evy);
	}

	event_set_x (event, evx);
	event_set_y (event, evy);
}



void	init_user_interface ()
{
	user_event_proc = default_user_event_proc;
	
	inside_scrollbar    = FALSE;
	ms_left_down        = FALSE;
	ms_middle_down      = FALSE;
	ms_right_down       = FALSE;
	shift_is_down       = FALSE;
	ctrl_is_down        = FALSE;
	last_event_id       = LOC_MOVE;
	constrain_is_active = FALSE;

	pl_head          = empty_picklist;
	picked_object    = empty_picklist;
	something_picked = FALSE;
	dispatch_user_action (RESET_CONSTRAINED);
	dispatch_user_action (GROUP_LABELLING_OPERATION_GOES_TO_NODE);

	last.node          = empty_node;
	last.edge          = empty_edge;
	last.graph         = empty_graph;
	current_production = empty_graph;
	
	multi_click_space   = DEFAULT_MULTI_CLICK_SPACE;
	multi_click_timeout = DEFAULT_MULTI_CLICK_TIMEOUT;

	menu_called_from = MENU_CALLED_FROM_NOWHERE;

	show_grid (0);
}
/************************************************************************/
/*									*/
/*		Benutzer - interface sperren / freigeben		*/
/*									*/
/************************************************************************/
/*									*/
/*	void	lock_user_interface   ()				*/
/*	void	unlock_user_interface ()				*/
/*									*/
/*	Sperrt das Benutzer-interface. Damit kann ein "Semaphor"	*/
/*	aufgebaut werden, mit dem temporaer aufgebaute Subwindows den	*/
/*	Rest der Benutzeroberflaeche sperren koennen. 			*/
/*	Alle direkt ueber die working_area aufgerufenen Prozeduren	*/
/*	(insbesondere benutzerdefinierte Kommandos) sollten diese	*/
/*	Sperre beachten !						*/
/*									*/
/*	int	text_user_interface_locked ()				*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	int	user_interface_locked;				*/
/*									*/
/************************************************************************/


static	int	user_interface_locked = FALSE;


void	lock_user_interface ()
{
	user_interface_locked = TRUE;
}


void	unlock_user_interface ()
{
	user_interface_locked = FALSE;
}


int	test_user_interface_locked ()
{
	return user_interface_locked;
}


/************************************************************************/
/*									*/
/*	int	user_interface_check_destroy_buffer ()			*/
/*									*/
/************************************************************************/


int	user_interface_check_destroy_buffer (buffer)
int	buffer;
{
	Graph	graph;
	Node	node;
	Edge	edge;
	Group	group, g;
	int	i, used_buffers = 0;
	
	
	if (graphed_state.shutdown)
		return TRUE;
	
	for (i=N_PASTE_BUFFERS; i<N_BUFFERS; i++)
		if (buffers[i].used)
			used_buffers ++;
	
	graph = get_currently_edited_graph ();
	node  = get_currently_edited_node  ();
	edge  = get_currently_edited_edge  ();
	group = get_currently_edited_group ();
	
	if (used_buffers == 1)
		return FALSE;
	else if (graph != empty_graph && graph->buffer == buffer)
		return FALSE;
	else if (node != empty_node && node->graph->buffer == buffer)
		return FALSE;
	else if (edge != empty_edge && edge->source->graph->buffer == buffer)
		return FALSE;
	else if (group != empty_group) {
		for_group (group, g) {
			if (g->node->graph->buffer == buffer)
				return FALSE;
		} end_for_group (group, g);
	}
	
	if (buffer ==  wac_buffer) {
		dispatch_user_action (UNSELECT);
		/* set wac_buffer to any used buffer, just to avoid	*/
		/* total inconsistency					*/
		for (i=N_PASTE_BUFFERS; i<N_BUFFERS;i++)
			if (buffers[i].used && i != buffer) {
				wac_buffer = i;
				break;
			}
	}
	/* No veto - fix pointers	*/
	
	if (current_production != empty_graph && current_production->buffer == buffer)
		current_production = empty_graph;
	if (last.graph != empty_graph && last.graph->buffer == buffer)
		last.graph = empty_graph;
	if (last.node != empty_node && last.node->graph->buffer == buffer)
		last.node = empty_node;
	if (last.edge != empty_edge && last.edge->source->graph->buffer == buffer)
		last.edge = empty_edge;
		
	return TRUE;
}



/************************************************************************/
/*									*/
/*	Rect	compute_rect_around_selection (selection)		*/
/*	Rect	compute_rect_around_current_selection ()		*/
/*									*/
/************************************************************************/


Rect		compute_rect_around_selection (selection)
Picklist	selection;
{
	if (selection != empty_picklist) switch (selection->what) {
	    case NODE_PICKED :
		return selection->which.node->box;
		break;
	    case EDGE_PICKED :
		return selection->which.edge->box;
		break;
	    case GROUP_PICKED :
		return compute_rect_around_group (selection->which.group);
		break;
	} else {
		Rect	r;
		rect_construct (&r, 0,0,0,0);
		return r;
	}
}


Rect	compute_rect_around_current_selection ()
{
	return compute_rect_around_selection (get_picked_object ());
}


Rect	compute_rect_around_graph_of_current_selection ()
{
	Graph	g;
	Rect	r;
	
	g = get_picked_or_only_existent_graph();
	
	if (g != empty_graph) {
		return compute_rect_around_graph (get_picked_or_only_existent_graph ());
	} else {
		rect_construct (&r, 0,0,0,0);
		return r;
	}
}


void	compute_subwindow_position_at_graph_of_current_selection (window)

Xv_Window	window;
{
	int	x,y;
	Rect	rect;
	Graph	g;
	extern	void	calculate_subwindow_position();
	
	g = get_picked_or_only_existent_graph();
	
	
	if (g == empty_graph) {
		x = (screenwidth  - (int)xv_get(window, XV_WIDTH))  / 2;
		y = (screenheight - (int)xv_get(window, XV_HEIGHT)) / 2;
	} else {
		rect = compute_rect_around_graph (g);
		calculate_subwindow_position (
			(int)xv_get(window, XV_WIDTH),
			(int)xv_get(window, XV_HEIGHT),
			rect.r_left + (rect.r_width)/2,
			rect.r_top  + (rect.r_height)/2,
			rect.r_width,
			rect.r_height,
			&x, &y);
	}
	
	xv_set(window, WIN_X, x, WIN_Y, y, NULL);
}


void	compute_subwindow_position_at_current_selection (window)

Xv_Window	window;
{
	int	x,y;
	Rect	rect;
	
	extern	void	calculate_subwindow_position();
	
	if (!something_picked) {
		x = (screenwidth  - (int)xv_get(window, XV_WIDTH))  / 2;
		y = (screenheight - (int)xv_get(window, XV_HEIGHT)) / 2;
	} else {
		rect = compute_rect_around_current_selection();
		calculate_subwindow_position (
			(int)xv_get(window, XV_WIDTH),
			(int)xv_get(window, XV_HEIGHT),
			rect.r_left + (rect.r_width)/2,
			rect.r_top  + (rect.r_height)/2,
			rect.r_width,
			rect.r_height,
			&x, &y);
	}
	
	xv_set(window, WIN_X, x, WIN_Y, y, NULL);
}

