/* Copyright Universitaet Passau 1990 */
/* GraphEd Source, 1986-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				gragra_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des gragra_subframe zum	*/
/*	Editieren von Knoten						*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"
#include "graphed_svi.h"


/************************************************************************/
/*									*/
/*		GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_gragra_subframe ()				*/
/*	void	show_gragra_subframe   ()				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/

		
static	void	hide_gragra_subframe           ();
static		notify_gragra_subframe_buttons ();
static		notify_gragra_subframe_cycles  ();
static	void	set_left_side_style            ();
static	void	set_embed_node_style           ();
static	void	set_embed_edge_style           ();


/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Frame		gragra_subframe;
static	Panel		gragra_subframe_panel;

static	Panel_item	gragra_subframe_quit_button;

static	Panel_item	gragra_subframe_always_match_empty_cycle;
static	Panel_item	gragra_subframe_nodetypes_significant;
static	Panel_item	gragra_subframe_edgetypes_significant;
static	Panel_item	gragra_subframe_nodecolors_significant;
static	Panel_item	gragra_subframe_edgecolors_significant;

static	Panel_item      gragra_subframe_type_cycle;
static	Panel_item	gragra_subframe_terminals_text;
static	Panel_item	gragra_subframe_nonterminals_text;
static	Panel_item	gragra_subframe_left_side_style_button;
static	Panel_item	gragra_subframe_embed_node_style_button;
static	Panel_item	gragra_subframe_embed_edge_style_button;

static	int		showing_gragra_subframe  = FALSE;

static	Node_defaults_subframe_info	left_side_style;
static	Node_defaults_subframe_info	embed_node_style;
static	Edge_defaults_subframe_info	embed_edge_style;


/************************************************************************/
/*									*/
/*		NODE_SUBFRAME AUFBAUEN UND VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_gragra_subframe ()				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	show_gragra_subframe ()					*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	void	hide_gragra_subframe ()				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Graph	get_currently_edited_gragra ()				*/
/*									*/
/************************************************************************/


void	create_gragra_subframe ()
{
	int	i, row_count = 0;
	Menu	gragra_subframe_menu;
	Icon 	icon;

	gragra_subframe = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_LABEL,		"gragra_sf.c",
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		FRAME_DONE_PROC,	hide_gragra_subframe,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);
		
	icon = (Icon)xv_create(gragra_subframe, ICON,
		ICON_IMAGE,	gragra_icon_svi,
		NULL);

	xv_set(gragra_subframe, FRAME_ICON, icon, NULL);
		
	gragra_subframe_panel = (Panel)xv_get(gragra_subframe, FRAME_CMD_PANEL);

	row_count ++;
	gragra_subframe_type_cycle = xv_create(gragra_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		gragra_type_strings_for_cycle,
		XV_X,			xv_col(gragra_subframe_panel, 0),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"Type :",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_gragra_subframe_cycles,
		NULL);

	row_count ++;
	gragra_subframe_always_match_empty_cycle = xv_create(gragra_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(gragra_subframe_panel, 0),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"empty labels in productions are ",
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("wildcards",TRUE, "empty labels",FALSE), 0,
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		NULL);


	row_count ++;
	gragra_subframe_nodetypes_significant = xv_create(gragra_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(gragra_subframe_panel, 0),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"nodetypes are ",
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("significant",TRUE, "not significant",FALSE), 0,
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		NULL);

	row_count ++;
	gragra_subframe_nodecolors_significant = xv_create(gragra_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(gragra_subframe_panel, 0),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"nodecolors are ",
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("significant",TRUE, "not significant",FALSE), 0,
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		NULL);

	row_count ++;
	gragra_subframe_edgetypes_significant = xv_create(gragra_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(gragra_subframe_panel, 0),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"edgetypes are ",
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("significant",TRUE, "not significant",FALSE), 0,
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		NULL);

	row_count ++;
	gragra_subframe_edgecolors_significant = xv_create(gragra_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(gragra_subframe_panel, 0),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"edgecolors are ",
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("significant",TRUE, "not significant",FALSE), 0,
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		NULL);

	row_count ++;
	gragra_subframe_terminals_text = xv_create(gragra_subframe_panel, PANEL_TEXT,
		XV_X,				xv_col(gragra_subframe_panel, 0),
		XV_Y,				xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,		"Terminals    : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			"",
		PANEL_VALUE_DISPLAY_LENGTH,	DEFAULT_PANEL_VALUE_DISPLAY_LENGTH,
		PANEL_VALUE_STORED_LENGTH,	FILENAMESIZE,
		NULL);

	row_count ++;
	gragra_subframe_nonterminals_text = xv_create(gragra_subframe_panel, PANEL_TEXT,
		XV_X,				xv_col(gragra_subframe_panel, 0),
		XV_Y,				xv_row(gragra_subframe_panel, row_count),
		PANEL_LABEL_STRING,		"Nonterminals : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			"",
		PANEL_VALUE_DISPLAY_LENGTH,	DEFAULT_PANEL_VALUE_DISPLAY_LENGTH,
		PANEL_VALUE_STORED_LENGTH,	FILENAMESIZE,
		NULL);

	row_count ++;
	gragra_subframe_left_side_style_button = xv_create(gragra_subframe_panel, PANEL_BUTTON,
		XV_X,			xv_col(gragra_subframe_panel, 0),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_gragra_subframe_buttons,
		PANEL_LABEL_STRING,     "left side",
		NULL);
	
	gragra_subframe_embed_node_style_button = xv_create(gragra_subframe_panel, PANEL_BUTTON,
		XV_X,			xv_col(gragra_subframe_panel, 20),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_gragra_subframe_buttons,
		PANEL_LABEL_STRING,     "embed nodes",
		NULL);
	
	gragra_subframe_embed_edge_style_button = xv_create(gragra_subframe_panel, PANEL_BUTTON,
		XV_X,			xv_col(gragra_subframe_panel, 40),
		XV_Y,			xv_row(gragra_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_gragra_subframe_buttons,
		PANEL_LABEL_STRING,     "embed edges",
		NULL);

	window_fit(gragra_subframe_panel);	
	window_fit(gragra_subframe);
}


void	show_gragra_subframe ()
{
	create_gragra_subframe ();
	
	lock_user_interface ();
	
	xv_set(gragra_subframe_type_cycle,			PANEL_VALUE, gragra_type_to_int(current_gragra_type), NULL);
	xv_set(gragra_subframe_always_match_empty_cycle,	PANEL_VALUE, get_gragra_always_match_empty (), NULL);
	xv_set(gragra_subframe_terminals_text,			PANEL_VALUE, current_gragra_terminals, NULL);
	xv_set(gragra_subframe_nonterminals_text,		PANEL_VALUE, current_gragra_nonterminals, NULL);
	xv_set(gragra_subframe_nodetypes_significant,		PANEL_VALUE, iif (get_embed_match_attributes() & NODE_TYPE, TRUE, FALSE), NULL);
	xv_set(gragra_subframe_edgetypes_significant,		PANEL_VALUE, iif (get_embed_match_attributes() & EDGE_TYPE, TRUE, FALSE), NULL);
	xv_set(gragra_subframe_nodecolors_significant,		PANEL_VALUE, iif (get_embed_match_attributes() & NODE_COLOR, TRUE, FALSE), NULL);
	xv_set(gragra_subframe_edgecolors_significant,		PANEL_VALUE, iif (get_embed_match_attributes() & EDGE_COLOR, TRUE, FALSE), NULL);
		
	if (current_gragra_type == ENCE_1) {
		xv_set(gragra_subframe_edgetypes_significant,  XV_SHOW, TRUE, NULL);
		xv_set(gragra_subframe_edgecolors_significant, XV_SHOW, TRUE, NULL);
	} else {
		xv_set(gragra_subframe_edgetypes_significant,  XV_SHOW, FALSE, NULL);
		xv_set(gragra_subframe_edgecolors_significant, XV_SHOW, FALSE, NULL);
	}
	
	xv_set(gragra_subframe,
		WIN_X,		screenwidth  / 2 - (int)xv_get(gragra_subframe, XV_WIDTH) / 2,
		WIN_Y,		screenheight / 2 - (int)xv_get(gragra_subframe, XV_HEIGHT) / 2,
		XV_SHOW,	TRUE,
		NULL);
		
	showing_gragra_subframe  = TRUE;
}


static	void	hide_gragra_subframe (frame)
Frame		frame;
{
	unsigned embed_match_attribute = get_embed_match_attributes();
	
	showing_gragra_subframe = FALSE;
	
	set_current_gragra_type
		(int_to_gragra_type ((int)xv_get(gragra_subframe_type_cycle, PANEL_VALUE)));
	set_gragra_always_match_empty
		(int_to_bool((int)xv_get(gragra_subframe_always_match_empty_cycle, PANEL_VALUE)));
	set_current_gragra_terminals (
		strsave ((char *)xv_get(gragra_subframe_terminals_text, PANEL_VALUE)));
	set_current_gragra_nonterminals (
		strsave ((char *)xv_get(gragra_subframe_nonterminals_text, PANEL_VALUE)));
		
	if (int_to_bool((int)xv_get(gragra_subframe_nodetypes_significant, PANEL_VALUE))) {
		embed_match_attribute |= NODE_TYPE;
	} else {
		embed_match_attribute &= ~NODE_TYPE;
	}
	if (int_to_bool((int)xv_get(gragra_subframe_edgetypes_significant, PANEL_VALUE))) {
		embed_match_attribute |= EDGE_TYPE;
	} else {
		embed_match_attribute &= ~EDGE_TYPE;
	}
	if (int_to_bool((int)xv_get(gragra_subframe_nodecolors_significant, PANEL_VALUE))) {
		embed_match_attribute |= NODE_COLOR;
	} else {
		embed_match_attribute &= ~NODE_COLOR;
	}
	if (int_to_bool((int)xv_get(gragra_subframe_edgecolors_significant, PANEL_VALUE))) {
		embed_match_attribute |= EDGE_COLOR;
	} else {
		embed_match_attribute &= ~EDGE_COLOR;
	}
	set_embed_match_attributes (embed_match_attribute);
	
	xv_destroy_safe(gragra_subframe);

	unlock_user_interface ();
}
/************************************************************************/
/*									*/
/*			NOTIFYPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static	notify_gragra_subframe_buttons (item, event)		*/
/*	static	notify_gragra_subframe_cycles  (item, value, event)	*/
/*									*/
/*	Wichtig : diese Prozeduren rufen am Ende force_repainting auf.	*/
/*									*/
/************************************************************************/


static		notify_gragra_subframe_buttons (item, event)
Panel_item	item;
Event		*event;
{
	if (item == gragra_subframe) {
		hide_gragra_subframe ();
	} else if (item == gragra_subframe_left_side_style_button) {
		left_side_style.attr = get_node_style (LEFT_SIDE_NODE_STYLE);
		show_node_defaults_subframe (&left_side_style, set_left_side_style);
	} else if (item == gragra_subframe_embed_node_style_button) {
		embed_node_style.attr = get_node_style (EMBED_NODE_STYLE);
		show_node_defaults_subframe (&embed_node_style, set_embed_node_style);
	} else if (item == gragra_subframe_embed_edge_style_button) {
		embed_edge_style.attr = get_edge_style (EMBED_EDGE_STYLE);
		show_edge_defaults_subframe (&embed_edge_style, set_embed_edge_style);
	}
	
	force_repainting();
}


static		notify_gragra_subframe_cycles (item, value, event)
Panel_item	item;
Event		*event;
int		value;
{
	if (int_to_gragra_type ((int)xv_get(gragra_subframe_type_cycle, PANEL_VALUE)) == ENCE_1) {
		xv_set(gragra_subframe_edgetypes_significant,  XV_SHOW, TRUE, NULL);
		xv_set(gragra_subframe_edgecolors_significant, XV_SHOW, TRUE, NULL);
	} else {
		xv_set(gragra_subframe_edgetypes_significant,  XV_SHOW, FALSE, NULL);
		xv_set(gragra_subframe_edgecolors_significant, XV_SHOW, FALSE, NULL);
	}
	

}


static	void			set_left_side_style (info)
Node_defaults_subframe_info	*info;
{
	set_node_style (LEFT_SIDE_NODE_STYLE, info->attr);
}


static	void			set_embed_node_style (info)
Node_defaults_subframe_info	*info;
{
	set_node_style (EMBED_NODE_STYLE, info->attr);
}

static	void			set_embed_edge_style (info)
Edge_defaults_subframe_info	*info;
{
	set_edge_style (EMBED_EDGE_STYLE, info->attr);
}
