/* Copyright Universitaet Passau 1990 */
/* GraphEd Source, 1986-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				graph_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des graph_subframe zum	*/
/*	Editieren von Knoten						*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "group.h"

#include "graphed_subwindows.h"


/************************************************************************/
/*									*/
/*		GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_graph_subframe ()				*/
/*	void	show_graph_subframe   (graph)				*/
/*									*/
/*	Graph	get_currently_edited_graph ()				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/

		
static	void	hide_graph_subframe           ();
static		notify_graph_subframe_buttons ();
static		notify_graph_subframe_cycles  ();



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Frame		graph_subframe;
static	Panel		graph_subframe_panel;

static	Panel_item	graph_subframe_quit_button;
static	Panel_item      graph_subframe_directedness_cycle;
static	Panel_item      graph_subframe_production_cycle;
static	Panel_item	graph_subframe_label_text;
static	Panel_item	graph_subframe_gragra_type;

static	int		showing_graph_subframe  = FALSE;


/************************************************************************/
/*									*/
/*		NODE_SUBFRAME AUFBAUEN UND VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_graph_subframe (graph)				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	show_graph_subframe (graph)				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	void	hide_graph_subframe ()				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Graph	get_currently_edited_graph ()				*/
/*									*/
/************************************************************************/


void	create_graph_subframe (graph)
Graph	graph;
{
	int	i, row_count = 0;

	graph_subframe = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_LABEL,		"graph_sf.c",
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		FRAME_DONE_PROC,	hide_graph_subframe,
		WIN_CLIENT_DATA,	empty_graph,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	graph_subframe_panel = (Panel)xv_get(graph_subframe, FRAME_CMD_PANEL);

	row_count ++;
	graph_subframe_directedness_cycle = xv_create(graph_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(graph_subframe_panel, 0),
		XV_Y,			xv_row(graph_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list ("directed", TRUE, "undirected", FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_graph_subframe_cycles,
		NULL);

	graph_subframe_production_cycle = xv_create(graph_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(graph_subframe_panel, 22),
		XV_Y,			xv_row(graph_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list ("production", TRUE, "normal graph", FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_graph_subframe_cycles,
		NULL);

	if (graph->is_production) {
		row_count += 1;
		graph_subframe_gragra_type = xv_create(graph_subframe_panel, PANEL_CYCLE,
			ATTR_LIST,		gragra_type_strings_for_cycle,
			XV_X,			xv_col(graph_subframe_panel, 22),
			XV_Y,			xv_row(graph_subframe_panel, row_count),
			PANEL_LABEL_BOLD,	TRUE,
			PANEL_CHOICES_BOLD,	TRUE,
			PANEL_NOTIFY_PROC,	notify_graph_subframe_cycles,
			NULL);
	}

	row_count ++;
	graph_subframe_label_text = xv_create(graph_subframe_panel, PANEL_TEXT,
		XV_X,				xv_col(graph_subframe_panel, 0),
		XV_Y,				xv_row(graph_subframe_panel, row_count),
		PANEL_LABEL_STRING,		"Name : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			"",
		PANEL_VALUE_DISPLAY_LENGTH,	DEFAULT_PANEL_VALUE_DISPLAY_LENGTH,
		PANEL_VALUE_STORED_LENGTH,	FILENAMESIZE,
		NULL);

	window_fit(graph_subframe_panel);
	window_fit(graph_subframe);
}


void	show_graph_subframe (graph)
Graph	graph;
{
	lock_user_interface ();
	
	if (showing_graph_subframe) hide_graph_subframe ();
	
	create_graph_subframe (graph);
	
	xv_set(graph_subframe_directedness_cycle,	PANEL_VALUE, graph->directed, NULL);
	xv_set(graph_subframe_production_cycle,		PANEL_VALUE, graph->is_production, NULL);
	xv_set(graph_subframe_label_text,		PANEL_VALUE, graph->label, NULL);
	if (graph->is_production)
		xv_set(graph_subframe_gragra_type,	PANEL_VALUE, gragra_type_to_int (graph->gra.type), NULL);

	xv_set(graph_subframe,
		WIN_CLIENT_DATA, graph,
		WIN_X,		screenwidth  / 2 - (int)xv_get(graph_subframe, XV_WIDTH) / 2,
		WIN_Y,		screenheight / 2 - (int)xv_get(graph_subframe, XV_HEIGHT) / 2,
		XV_SHOW,	TRUE,
		NULL);
		
	showing_graph_subframe  = TRUE;
}


static	void	hide_graph_subframe (frame)
Frame		frame;
{
	showing_graph_subframe = FALSE;
	
	set_graph_label ((Graph)xv_get(graph_subframe, WIN_CLIENT_DATA),
		strsave ((char *)xv_get(graph_subframe_label_text, PANEL_VALUE)));

	xv_destroy_safe(graph_subframe);
	
	unlock_user_interface ();
}



Graph	get_currently_edited_graph ()
{
	if (showing_graph_subframe) {
		return (Graph)xv_get(graph_subframe, WIN_CLIENT_DATA);
	}
}
/************************************************************************/
/*									*/
/*			NOTIFYPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static	notify_graph_subframe_buttons (item, event)		*/
/*	static	notify_graph_subframe_cycles  (item, value, event)	*/
/*									*/
/*	Wichtig : diese Prozeduren rufen am Ende force_repainting auf.	*/
/*									*/
/************************************************************************/


static		notify_graph_subframe_buttons (item, event)
Panel_item	item;
Event		*event;
{

	Node	graph = (Node)xv_get(graph_subframe, WIN_CLIENT_DATA);
	
	if (item == graph_subframe) {
		hide_graph_subframe (graph_subframe);
	}
	
	force_repainting();
}


static		notify_graph_subframe_cycles (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{

	Graph	graph = (Graph)xv_get(graph_subframe, WIN_CLIENT_DATA);
	Group	group_of_graph;
	
	if (item == graph_subframe_directedness_cycle) {
	
		graph->directed = value;
/*		graph_set (graph, RESTORE_IT, 0); */

		group_of_graph = make_group_of_graph (graph);
		group_set (group_of_graph, RESTORE_IT, 0);
		free_group (group_of_graph);
		
	} else if (item == graph_subframe_production_cycle) {
	
		/* No changes permitted !	*/
		xv_set(graph_subframe_production_cycle, PANEL_VALUE, graph->is_production, NULL);
	
	} else if (item == graph_subframe_gragra_type) {
	
		set_gragra_type (graph, int_to_gragra_type (value));
	
	}
		
	force_repainting();
}
