/* (C) Universitaet Passau 1986-1991 */

/*************************************************************************
**									**
**	MODULNAME:	termgraph_main.c				**
**									**
**************************************************************************
**									**
**	ERSTELLUNG:	Rudolf Seisenberger, 	1990			**
**									**
**	AUFGABEN:	Die Datei enthaelt die Prozeduren, die an der	**
**			Schnittstelle zwischen TermGraph und GraphEd,	**
**			dem Grapheneditor, gebraucht werden.		**
**			Dazu gehoeren auch die Anschlussprozeduren, die	**
**			ueber den Menuepunkt 'user' aus GraphEd auf-	**
**			gerufen werden.					**
**									**
*************************************************************************/

/*************************************************************************
**									**
**				FUNKTIONEN				**
**									**
**************************************************************************
**									**
**	void 	termgraph_window_proc(), petrinet_turn_proc(),		**
**		call_petrinet_turn_proc(),				**
**		term_to_graph_creation_proc(), 			 	**
**		main_several_token_proc(), termgraph_zoom_proc(), 	**
**		termgraph_scroll_proc(); termgraph_free_tree ();	**
**	int 	main_proc_token(), termgraph_fit_font();		**
**		term_main();						**
**		insert_edge_in_Sgraph_list();				**
**	Snode 	insert_node_in_Sgraph_list();				**
**	char    *termgraph_menu_callback_proc_1(),			**
**		*termgraph_menu_callback_proc_2(), 			**
**									**
*************************************************************************/

/*************************************************************************
**									**
**				GLOBALE VARIABLE			**
**									**
**************************************************************************
**									**
**	In diesem Modul sind alle globalen Variablen, die nur durch	**
**	TermGraph benutzt werden, definiert. Fuer andere Module	sind 	**
**	sie also extern erreichbar.					**
**									**
*************************************************************************/

#include "termgraph_decl.h"
#include "termgraph_main.h"

extern	working_area_canvas; /* GraphEd Variable */

extern	char 	termglob_agent_name[MAXSTRING],
		termglob_agent_stdin[MAXSTRING],
		termglob_agent_dir[MAXSTRING];
extern 	Xv_Window 	termgraph_frame;
extern 	int 	termgraph_only_defaults;
extern 	int 	termgraph_initial_x;
extern 	int 	termgraph_initial_y;

extern 	char	*fill_buffer();
extern 	char	*flatten();
extern	pnode	*termgraph_parsetree();
extern	pnode	*createpnode();
extern	int 	show_tree();
extern	int 	show_right();
extern	int	sequential_order();
extern	int	search_for_successor();
extern	char 	*call_name();
extern	char 	*structur();
extern	int 	termgraph_draw_petrinet();
extern	int	termgraph_squares();
extern  int 	sequential_places_insert_activity();
extern  int	brace_correct ();
extern	int 	set_flag_to_target ();

char 	termgraph_default_agent_name[MAXNAME];
int	termgraph_basic_length = DEFAULT_ELEMENT_LENGTH;
int	ELEMENT_LENGTH = DEFAULT_ELEMENT_LENGTH;
int	CORX = DEFAULT_CORX;
int	CORY = DEFAULT_CORY;
int	termgraph_window_exists = 0;
int	termgraph_nodes_sum;  /* node counter starts with -1 */
float 	termgraph_zoom_value = 0.9;
int 	termgraph_edgeline_type = 0;
FILE 	*termglob_graph_fp;

void 	smarties_proc(), termgraph_window_proc(), term_to_graph_creation_proc(),
	main_several_token_proc(), termgraph_zoom_proc(), termgraph_scroll_proc();
int 	main_proc_token(), termgraph_fit_font();
void 	termgraph_free_tree ();	
int 	term_main();
int   	insert_edge_in_Sgraph_list();
Snode 	insert_node_in_Sgraph_list();

Sgraph_proc_info  termglob_info;


void termgraph_window_proc(info)
Sgraph_proc_info	info;
{
	Attributes pattrs;
	Sgraph   sgraph;
	Snode		pnod;
	static int	installed = 0;
	
	termglob_info = info;
	
	if (termgraph_window_exists == 1) {
		if ( (int)xv_get(termgraph_frame, WIN_SHOW) == TRUE ) {
			message ("TermGraph%% TermGraph window is just installed!\n");
			bell();
		} else {
			xv_set (termgraph_frame, WIN_SHOW, TRUE, 0);
			message ("TermGraph%% Show again window for TermGraph.\n");
		}
	} else {
		if (0 == termgraph_window_main(info)) {
			termgraph_window_exists = 0;
			message ("*** Cannot install TermGraph! ***\n");
		} else {
			termgraph_window_exists = 1;
			message ("TermGraph%% Created window for TermGraph.\n");
		}
	}
	
} /* termgraph_window_proc */

void petrinet_turn_proc (pgraph, start_from, deltax, deltay, corx, cory) 
/*** berechnet die Koordinaten der Knoten und der Polylinien neu fuer eine ***/
/*** um 90 Grad gedrehte Darstellung des Graphen pgraph			   ***/

Sgraph	pgraph;
char	start_from;
int	deltax, deltay, corx, cory;
{
	Snode 	 pnod;
	Sedge	 pedge;
	Edgeline el, edgeline;
	int 	 do_it, i, move_x, move_y , sgnx, sgny, value;
	
	switch (start_from)
	{
	case 'N':	do_it = 0;
			break;
	
	case 'W':	move_x = corx - cory + deltay ;
			sgnx = 1;
			move_y = cory + corx;
			sgny = -1;
			do_it = 1;
			break;
	
	case 'E':	move_x = corx + cory;
			sgnx = -1;
			move_y = cory + corx;
			sgny = -1;
			do_it = 1;
			break;

	case 'S':	move_x = 0;
			sgnx = 1;
			move_y = cory - deltay + cory;
			sgny = -1;
			do_it = 1;
			break;
			
	default:	do_it = 0;
			break;
	}
	
	if (do_it == 1) {
	    for_all_nodes(pgraph, pnod) {
		for_sourcelist(pnod, pedge)
			edgeline = (Edgeline)edge_get(graphed_edge(pedge), EDGE_LINE);
			for_edgeline(edgeline, el)
				if (start_from == 'S') {
					set_edgeline_xy(el,
						move_x + sgnx * edgeline_x(el),
						move_y + sgny * edgeline_y(el));
				} else {
					value = edgeline_x(el);
					set_edgeline_xy( el,
						move_x + sgnx * edgeline_y(el),
						move_y + sgny * value );
				}
			end_for_edgeline(edgeline, el);
			edge_set(graphed_edge(pedge),
				EDGE_LINE,		edgeline,
				0);
						
		end_for_sourcelist(pnod, pedge);
		
		/*** Speziell fuer die Verwendung von Vierteldrehungen: ***/
		/*** Der Knotenrahmen wird auch gedreht, ausser...      ***/
		
		if ((pnod->slist != empty_sedge) && (pnod->tlist != empty_sedge)) {
		/*** ... bei dem Knoten, der den Eingabestring enthaelt ***/
		
			node_set(graphed_node(pnod), 
				NODE_SIZE, (int)node_get(graphed_node(pnod), NODE_HEIGHT),
					   (int)node_get(graphed_node(pnod), NODE_WIDTH),
				0);
		}
				
		if (start_from=='S') {
			pnod->x = move_x + sgnx * pnod->x;
			pnod->y = move_y + sgny * pnod->y;
		} else {
			value = pnod->x;
			pnod->x = move_x + sgnx * pnod->y;
			pnod->y = move_y + sgny * value;
		}

	    } end_for_all_nodes(pgraph, pnod);
	    force_repainting();
	}
		
} /* petrinet_turn_proc */

void call_petrinet_turn_proc (info)
/*** ruft petrinet_turn_proc fuer eine Vierteldrehung nach links auf ***/

Sgraph_proc_info	info;
{
	Snode 	 pnod;
	Sedge	 pedge;
	Edgeline el, edgeline;
	int 	 counter, i;

	if (info != NULL){
		i = 0; 
	  	for_all_nodes(info->sgraph, pnod) {
			i = MAX( i, CORY - pnod->y);		
	  	} end_for_all_nodes(info->sgraph, pnod);
		petrinet_turn_proc (info->sgraph, 'W', 0, i, CORX, CORY);
		/* 'W' dreht den Graphen um 90 Grad nach links */
	}
} /* call_petrinet_turn_proc */


void term_to_graph_creation_proc(info)
/*** baut einen evtl. bestehenden Graphen (info->sgraph) ab und ruft term_main ***/
/*** auf, um einen neuen Graphen zu erzeugen				       ***/

Sgraph_proc_info	info;
{
	Attributes pattrs;
	Sgraph   pgraph;
	Snode 	 pnod, 
		 old_node; 	/* GraphEd fuehrt auf einem leeren sgraph-Graphen i.a. */
		 		/* kein Benutzerprogramm wie termgraph aus. Daher wird */
		 		/* zur Ueberbrueckung ein Knoten in sgraph bewahrt.    */
	Sedge	 pedge;	
	Slist	 nodes = empty_slist;
	Edgeline el, edgeline;
	int 	 counter, i;
	char	 count_string[2 * INT_LENGTH];
	char	 current_graph_name[SESSION_RUN_COUNTER +INT_LENGTH];
	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)
		&& (info->sgraph->nodes->suc != NULL)) {
		
		pnod = info->sgraph->nodes;
		while (info->sgraph->nodes->suc != pnod) {
			for_sourcelist (info->sgraph->nodes->suc, pedge) {
			} end_for_sourcelist (info->sgraph->nodes->suc, pedge);
			remove_node (info->sgraph->nodes->suc);
		}
		old_node = pnod;
	} else {
		old_node = NULL;
	}
	
	if ((info != NULL) && (info->sgraph != NULL)) {
	  i=2000;
	  for_all_nodes(info->sgraph, pnod) {
		if (pnod->x >= i) {
			while (pnod->slist != empty_sedge) remove_edge(pnod->slist);
			while (pnod->tlist != empty_sedge) remove_edge(pnod->tlist);
			pnod->x = 3950;
		}
	  } end_for_all_nodes(info->sgraph, pnod);	
	}
	
	/* Verschieben des alten Graphen in x-Richtung  nach rechts um i Pixel */
	if ((info != NULL) && (info->sgraph != NULL)) { /*then*/
	i=2000;
	for_all_nodes(info->sgraph, pnod) {
		for_sourcelist(pnod, pedge)
			edgeline = (Edgeline)edge_get(graphed_edge(pedge), EDGE_LINE);
			for_edgeline(edgeline, el)
				if (edgeline_x(el) < i) {
					set_edgeline_xy (el,
						edgeline_x(el) + i,
						edgeline_y(el) );
				}
			end_for_edgeline(edgeline, el);
			edge_set(graphed_edge(pedge),
				EDGE_LINE,		edgeline,
				0);
		end_for_sourcelist(pnod, pedge);
		
		if (pnod->x < i) 
			pnod->x += i;
	} end_for_all_nodes(info->sgraph, pnod);	
	}  /*then*/	
	

	/*** der TERMGRAPH-Algorithmus: ***/
	
	term_main(info, 1, (char **)NULL);
	
	
	if (old_node != NULL) {
		remove_node (old_node);
	}

	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SNODE;
	info->new_selection.snode = NULL;
	info->recenter = TRUE;

} /* term_to_graph_creation_proc */

char *termgraph_menu_callback_proc_1 (menu, menu_item)
char *menu, *menu_item;
{
	return (call_sgraph_proc (termgraph_window_proc));
}

char *termgraph_menu_callback_proc_2 (menu, menu_item)
char *menu, *menu_item;
{
	return (call_sgraph_proc (term_to_graph_creation_proc));
}

/*extern int malloc_debug();
extern int malloc_verify();*/


/**********************************************/

int main_proc_token(info)
/*** die Tokenuebergaenge werden hier veranlasst ***/

Sgraph_proc_info	info;
{
	int a, b, switchable, any;
	char str[MAXNAME];
	Snode nod;
	Sedge ed, fed;
	
		message ("\nRunning: ");
		for_all_nodes (info->sgraph, nod){
		    if ((int)node_get(graphed_node(nod), NODE_TYPE) == 1 /*=PLACE*/) {
			if (nod->label == NULL) {
				node_set( graphed_node(nod), 
					NODE_COLOR, 	0,
					NODE_FONT, 	0,
					0 );
			}	
			else {
				node_set( graphed_node(nod), 
					NODE_COLOR, 	atoi(nod->label),
					NODE_FONT, 	0,
					0 );
			}
		    }
		}end_for_all_nodes (info->sgraph, nod);
		
		any = FALSE;	
		for_all_nodes (info->sgraph, nod){
		    if ((int)node_get(graphed_node(nod), NODE_TYPE) == 1 /*=PLACE*/) {
			if ( (int)node_get(graphed_node(nod), NODE_COLOR) > 0) {
			   for_sourcelist (nod, ed){
			     if ( ((char *)edge_get(graphed_edge(ed), EDGE_LABEL) != NULL)
			     && (strcmp((char *)edge_get(graphed_edge(ed), EDGE_LABEL), "0") != 0) ) {
			     /* als Kantenlabel blockiert "0" die Kante (Kapazitaet=0) fuer Token */
				switchable = TRUE;
				for_targetlist (ed->tnode, fed){
				   if ( (int)node_get(graphed_node(fed->snode), NODE_COLOR ) <= 0) {
			              switchable = FALSE;
			           }
			        }end_for_targetlist (ed->tnode, fed);
			        
			  	if (switchable == TRUE) {
			  	   any = TRUE;
			           for_targetlist (ed->tnode, fed){
			              if (( (int)node_get(graphed_node(fed->snode), NODE_HEIGHT) == P_SEMA_NSY )) {
			              /* ein Semaphorknoten (P_SEMA_NSY!) gibt beim Schalten immer alle Token ab!! */
			              	   node_set( graphed_node(fed->snode),
			               		NODE_COLOR, 0,
			                 	0);
			              } else {
			                   node_set( graphed_node(fed->snode),
			               		NODE_COLOR, -1 +(int)node_get(graphed_node(fed->snode), NODE_COLOR),
			                 	0);
			              }
			           }end_for_targetlist (ed->tnode, fed);
			           
			           if ((ed->tnode->label[0] != '@') && (ed->tnode->label[0] != '<')) {
					message ("%s;", ed->tnode->label);
				   } else {
					   /*message ("%s;", THETA_HELP_NAME);*/ /*aktivierbar*/
				   }
				   
			           for_sourcelist(ed->tnode, fed){ 
			              node_set( graphed_node(fed->tnode),
			                 NODE_FONT, 1 +(int)node_get(graphed_node(fed->tnode), NODE_FONT),
			                 0);
			           }end_for_sourcelist(ed->tnode, fed);
			        }
			     }/*if*/
			   }end_for_sourcelist (nod, ed);
			}
		    }
		}end_for_all_nodes (info->sgraph, nod);
		
		for_all_nodes (info->sgraph, nod){
		    if ((int)node_get(graphed_node(nod), NODE_TYPE) == 1 /*=PLACE*/) {
			a = (int)node_get(graphed_node(nod), NODE_COLOR);
			b = (int)node_get(graphed_node(nod), NODE_FONT);
			if ((a+b) > 0) {
			   itoa (a+b, str);
			   set_nodelabel (nod, strsave (str));
			} else {
			   set_nodelabel (nod, strsave("0"));
			}
			node_set( graphed_node(nod), 
					ONLY_SET,
					NODE_COLOR, 		(a+b)<=0 ? termgraph_place_color :
						(termgraph_place_color == 1)? 3/*blue*/: 1/*red*/,
					NODE_FONT, 		P_NFI,
					NODE_LABEL_VISIBILITY,	TRUE,
					0 );
		    }
		}end_for_all_nodes (info->sgraph, nod);
		
		if (any == FALSE) {
			message ("%s\nTermGraph%% No Transition is alive!\n", AGENT_SKIP_NAME);
			bell();
		}
	force_repainting();
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SNODE;
	info->new_selection.snode = NULL;
	info->recompute = TRUE;
	
	return any;
} /* main_proc_token */

void main_several_token_proc(info)
/*** maximal 25 Durchgaenge von Token-Schalten werden hintereinander ausgefuehrt ***/

Sgraph_proc_info	info;
{
	int any, i;
	
	for (i = 0, any = 1; i < 25 /*times*/; i++) {
		if (any == 1) {
			any = main_proc_token(info);
			sleep(1);
		} else {
			break;
		}

	}
	message ("\n");
} /* main_several_token_proc */

void termgraph_zoom_proc(info)
/*** termgraph-eigene "Zoom"-Funktion; Vorsicht: bei extrem starker Vergroesserung ***/
/*** kann die Darstellbarkeit der integer-Koordinaten nicht grwaehrleistst werden  ***/
/*** (Problem taucht aber i.a. nie auf). Eigene GraphEd-Zoom-Proc. soll folgen!	   ***/

Sgraph_proc_info	info;
{
	Snode 	 pnod;
	Sedge	 pedge;
	Edgeline el, edgeline;
	int 	 ix, iy, zfont, zwidth, zheight;
	float 	 zoom;	
	int 	 scrollx, scrolly;
	
	zoom = termgraph_zoom_value;
	if ((info != NULL) && (info->selected == SGRAPH_SELECTED_SNODE)) {
		scrollx = ix = info->selection.snode->x;
		scrolly = iy = info->selection.snode->y;
	} else {
		ix = info->sgraph->nodes->x;
		iy = info->sgraph->nodes->y;
		get_scroll_offset(info->buffer, &scrollx, &scrolly);
		scrollx += (int)xv_get (working_area_canvas, XV_WIDTH) / 2;
		scrolly += (int)xv_get (working_area_canvas, XV_HEIGHT) / 4;
	}

	
	if (info != NULL){ /*then*/
	    for_all_nodes(info->sgraph, pnod) {
		pnod->x = scrollx + (int)( (pnod->x - ix) * zoom );
		pnod->y = scrolly + (int)( (pnod->y - iy) * zoom );
		
		zwidth = MAX((int)(zoom * (int)(node_get(graphed_node(pnod),NODE_WIDTH))),1);
		zheight = MAX((int)(zoom * (int)(node_get(graphed_node(pnod),NODE_HEIGHT))),1);
		zfont = termgraph_fit_font (zheight);
		node_set (graphed_node(pnod),
			ONLY_SET,
			NODE_SIZE, 	zwidth,
				   	zheight,
			NODE_FONT,	zfont,
			0);
			
		for_sourcelist(pnod, pedge) {
			edgeline = (Edgeline)edge_get(graphed_edge(pedge), EDGE_LINE);
			for_edgeline(edgeline, el) {
				set_edgeline_xy (el,
					scrollx + (int)( (edgeline_x(el) - ix) * zoom ),
					scrolly + (int)( (edgeline_y(el) - iy) * zoom ));
			} end_for_edgeline(edgeline, el);

			edge_set(graphed_edge(pedge),
				ONLY_SET,
				EDGE_LINE,	edgeline,
				EDGE_ARROW_LENGTH,	
					MAX((int)(zoom * (int)(edge_get(graphed_edge(pedge),
					EDGE_ARROW_LENGTH))), 1),
				EDGE_FONT,	zfont,
				0);
		} end_for_sourcelist(pnod, pedge);
	    } end_for_all_nodes(info->sgraph, pnod);	
	}  /*then*/
	
	ix += 500;
	iy += 500;
	get_scroll_offset(info->buffer, &ix, &iy);
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SNODE;
	info->new_selection.snode = NULL;
	info->recompute = TRUE;

} /* termgraph_zoom_proc */

int termgraph_fit_font(h) 
int h;
/* Diese Funktion ist mit Vorsicht zu verwenden: f legt den font_index fuer graphed */
/* fest, ohne jede Ueberpruefung, ob dafuer auch ein font definiert ist!! */
{
	int f;
	
	     if (h >  30) f = 11;
	else if (h >= 30) f = 10;
	else if (h >= 24) f =  8;
	else if (h >= 22) f =  6;
	else if (h >= 20) f =  4;
	else if (h >= 18) f =  2;
	else if (h >= 16) f =  0;
	else 		  f =  0;
	
	return f;
} /* termgraph_fit_font */
		
void termgraph_scroll_proc(info)
Sgraph_proc_info	info;
{
	Snode 	 pnod;
	Sedge	 pedge;
	Edgeline el, edgeline;
	int 	 scrollx, scrolly, ix, iy;
	
	get_scroll_offset(info->buffer, &scrollx, &scrolly);
	
	scrollx += (int)xv_get (working_area_canvas, XV_WIDTH) / 2;
	scrolly += (int)xv_get (working_area_canvas, XV_HEIGHT) / 2;
	
	if ((info != NULL) && (info->sgraph != NULL) && (info->selected == SGRAPH_SELECTED_SNODE)) {
		ix = info->selection.snode->x - scrollx;
		iy = info->selection.snode->y - scrolly;
	} else {
		ix = VALa(info->sgraph->nodes, 0, x) - scrollx;
		iy = VALa(info->sgraph->nodes, 0, y) - scrolly;
	}
	
	if ((info != NULL) && (info->sgraph != NULL)) { 
	    for_all_nodes(info->sgraph, pnod) {
		pnod->x -= ix;
		pnod->y -= iy;
			
		for_sourcelist(pnod, pedge) {
			edgeline = (Edgeline)edge_get(graphed_edge(pedge), EDGE_LINE);
			for_edgeline(edgeline, el) {
				set_edgeline_xy (el,
					edgeline_x(el) - ix,
					edgeline_y(el) - iy);
			} end_for_edgeline(edgeline, el);
			
		} end_for_sourcelist(pnod, pedge);
	    } end_for_all_nodes(info->sgraph, pnod);
	    	
	    info->new_sgraph = info->sgraph;
	    info->new_selected = SGRAPH_SELECTED_SAME;
	    /*info->recompute = TRUE;*/
	}

} /* termgraph_scroll_proc */

int term_main(iinfo, argc, argv)
Sgraph_proc_info	iinfo;
int				argc;
char				*argv[];

{
	int			i, status, number;
	char			*input_name;
 	char			agent_string[MAXBUF], 
 				term[MAXBUF],
 				flat_string[MAXBUF];
 	pnode			*init;
	Sgraph			pgraph;
	Sgraph_proc_info	info;

			
	pgraph = VALa(iinfo, NULL, sgraph);
	info = iinfo;
	status = 1;
	
	if (termgraph_default_agent_name[0] == '\0') {
		strcpy (termgraph_default_agent_name, DEFAULT_AGENT_FILE);
	}
	
	input_name = (termglob_agent_name[0] == '\0' ? call_name(argc, argv) :
				 	termglob_agent_name);
	i = 0;

	strcpy(term, "*** No specified agent structure! ***");
	
	if (termglob_agent_stdin[0] != '\0') {
		strcpy(agent_string, termglob_agent_stdin);
		termglob_agent_stdin[0] = '\0';
	} else {
		if ((info != NULL) && (info->selected == SGRAPH_SELECTED_SNODE)) {
			if (info->selection.snode->label != NULL) {
				strcpy(agent_string, info->selection.snode->label);
			}
		} else  {
			strcpy (agent_string, fill_buffer(pgraph, input_name, NULL));
			if (agent_string[0] == '\0') {
				message("TermGraph%% *** Loaded data from \"%s\" incorrect! ***\n",input_name);
				strcpy(term, "*** ERROR: Cannot load from file ***");
			}
		}
	} 
	
	init = createpnode();
	init->left = init->right = NULL; 
	
	strcpy(flat_string, flatten(agent_string));
	
	if (flat_string[0] == '\0') {
		init->right = NULL;
	} else {
		if (brace_correct(flat_string) != 0) {
			strcpy (flat_string, " ");
			strcpy (term, "*** SYNTAX ERROR: Check bracket set! ***");
		} else {
			strcpy(term, agent_string);
		}
		
		init->right = termgraph_parsetree(flat_string, 0, strlen(flat_string)-1);
		init->activ = 1;
		strcpy(init->name, "<init>");
		init->left = NULL;
		init->right->pred = init;
		init->succ = NULL/*oder dummy'#'*/;
		init->pred = NULL;
		init->Snod = NULL;
		init->right->lbranch = 0; /* 0 = rechter Nachfolger */
		if ( (status = sequential_order(init->right)) < 0 ) {
			message("TermGraph%% *** Identificator cannot be a label! ***\n");
			strcpy(term, "*** Identificator cannot be a label! ***");
		} else {
			if (search_for_successor(init->right, init) < 0) {
				return -1;
			}
			set_flag_to_target (init);
			number = 1;
			termgraph_squares (init, &number);
			/*sequential_nodes_get_their_left_successors_coordinates(init->right);*/
		}
	}
	
	(number)++;
		
	if (termgraph_only_defaults == FALSE) {
		CORX = termgraph_initial_x;
		CORY = termgraph_initial_y;
	}
	if (status >= 0) {
		termgraph_draw_petrinet(pgraph, 
		termglob_graph_fp, init, &number, argc, argv, term);
	};	
	termgraph_free_tree (init);
	
	return 0;
} /*termgraph_main*/

void termgraph_free_tree (p)
pnode *p;
{
	if (p == NULL) {
		return;
	} 
	
	if ((p->right == NULL) && (p->left == NULL)) {
		/*free (p->name);*/
		free (p);
	} else {
		if (p->left != NULL) {
			termgraph_free_tree (p->left);
		}
		if (p->right != NULL) {
			termgraph_free_tree (p->right);
		}
	}
	return;
} /* termgraph_free_tree */




Snode insert_node_in_Sgraph_list(pgraph, p,
	number,nsx,nsy,nti,nfi,nlp,nei,nlv,col,npx,npy,name_in)
Sgraph	pgraph;
pnode	*p;
int     number,nsx,nsy,nti,nfi,nlp,nei,nlv,col,npx,npy;
char    *name_in;
{ 
	Snode 	pnod;
	char	step_char, name[MAXBUF];
			
	for_all_nodes(pgraph, pnod) {
		if ( (ABS(pnod->x - npx) < 2) && (ABS(pnod->y - npy) < 2) ) {
			/* die Unschaerfe "<2" wird erlaubt um Rundungsfehler zu unterdruecken */
			/* gleiche Knotenposition muss zu Identitaet der Knoten fuehren */
			if ((p != NULL) /* && (p->Snod == NULL)*/ /*7.11.90*/)
				p->Snod = pnod;
			return pnod;
		};
	} end_for_all_nodes(pgraph, pnod);
	
	if (termgraph_only_defaults == FALSE) {
		if ((nsx == P_NSX) || (nsx == T_NSX)) {
			if (nsx == P_NSX) {
				nsx = termgraph_placex_length;
				nsy = termgraph_placey_length;
			} else {
				nsx = termgraph_transx_length;
				nsy = termgraph_transy_length;
			}
		};
		nfi = (nti == P_NTI) ? 
			((nfi==P_NFI)? termgraph_fit_font (termgraph_placey_length):
				      termgraph_place_font) :
			((nfi==T_NFI)? termgraph_fit_font (termgraph_transy_length):
				      termgraph_transition_font);
		nlv = (nti == P_NTI) ? termgraph_place_nlv : termgraph_transition_nlv;
		col = (nti == P_NTI) ? termgraph_place_color : 
				(col == 15/*black for agent nodes!*/)||(col == 1/*red*/)? col:
				termgraph_transition_color;
	};
		
	pnod = make_node (pgraph, make_attr(ATTR_DATA, (char *)pnod));
	pnod->x = npx;
	pnod->y = npy;
	pnod->graphed = (char *)malloc(sizeof(name));
	sprintf (name, "%s", (name_in[0] == '\0')? " ": name_in);
	name[MAXBUF] = '\0'; 
	set_nodelabel (pnod, strsave (name));
	node_set(create_graphed_node_from_snode(pnod), 
		NODE_X,				/*ABS(npx)*/ npx, 
		NODE_Y,				/*ABS(npy)*/ npy,
		NODE_SIZE,			nsx, 	nsy,
		NODE_TYPE,			nti,
		NODE_FONT,			nfi,
		NODE_NLP,			nlp,
		NODE_NEI, 			nei,
		NODE_LABEL_VISIBILITY,		nlv,
		NODE_COLOR,			col,	
		0);
			
	if (p != NULL) {
		p->Snod = pnod; /* Snod-Ersteintrag */
	};
	
	termgraph_nodes_sum++; /* ein Knoten mehr */
	
	force_repainting();
	return pnod;
} /* insert_node_in_Sgraph_list */
	
int insert_edge_in_Sgraph_list( snode, tnode, number1, number2,
	col,eti,efi,elv, al,aa,el,np,name_in)
Snode		snode, tnode;
int 		number1, number2;	
int     	col,eti,efi,elv, al;
float		aa;
Edgeline	el;
char  		*np, *name_in;
{ 
	Sedge 	pedge;
	char	step_char, name[MAXBUF];
	int	exists;
	
	if (snode!=NULL && tnode!=NULL) {
		exists = FALSE;
		for_sourcelist (snode, pedge) {
			if (pedge->tnode == tnode) {
				exists = TRUE;
			}
		} end_for_sourcelist (snode, pedge);
		
		if (exists == FALSE) {
			pedge = make_edge (snode, tnode, make_attr(ATTR_DATA, (char *)pedge));
			sprintf (name, "%s", TG_EDGE_LABEL);
			set_edgelabel (pedge, strsave (name));
			
			if (termgraph_only_defaults == FALSE) {
				col = termgraph_edge_color;
				efi = termgraph_edge_font;
				elv = termgraph_edge_elv;
				al = (al == P_AL) ? termgraph_arrow_length :
						3*termgraph_arrow_length/2;
				aa = (aa == P_AA) ? (double)termgraph_arrow_angle :
						(double)(2*termgraph_arrow_angle/3);
			}
			
			edge_set(create_graphed_edge_from_sedge(pedge),
				EDGE_TYPE,			eti,
				EDGE_ARROW_LENGTH,		al,
				EDGE_ARROW_ANGLE,		(double)aa,
				EDGE_LINE,			el,
				EDGE_FONT,			efi,
				EDGE_LABEL_VISIBILITY,		elv,
				EDGE_COLOR,			col,
				0);		    
			force_repainting();
		}
	} else {
		message ("TermGraph%% *** WARNING: Can't draw edge! ***\n");
	}
	return 0;
} /* insert_edge_in_Sgraph_list */	

