/* Copyright Universitaet Passau 1990 */
/* GraphEd Source, 1986-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				node_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des node_subframe zum	*/
/*	Editieren von Knoten						*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"
#include "graphed_svi.h"

#include "font.h"
#include "type.h"


/************************************************************************/
/*									*/
/*		GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_node_subframe ()					*/
/*	void	show_node_subframe   (node)				*/
/*									*/
/*	void	install_fontlist_in_node_subframe            (list)	*/
/*	void	install_nodetypelist_in_node_subframe        (list)	*/
/*	void	update_nodelabel_visibility_in_node_subframe ()		*/
/*	void	install_current_nodefont_in_node_subframe    ()		*/
/*									*/
/*	Node	get_currently_edited_node ()				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/

		
static	void	hide_node_subframe           ();
static		notify_node_subframe_buttons ();
static		notify_node_subframe_cycles  ();



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Frame		node_subframe;
static	Panel		node_subframe_panel;

static	Panel_item	node_subframe_quit_button;
static	Panel_item	node_subframe_clear_nodelabel_button;
static	Panel_item      node_subframe_scale_cycle;
static	Panel_item	node_subframe_mark_cycle;
static	Panel_item	node_subframe_label_visibility_cycle;
static	Panel_item	node_subframe_set_nei_cycle;
static	Panel_item	node_subframe_set_nlp_cycle;
static	Panel_item	node_subframe_nodefont_selection;
static	Panel_item	node_subframe_nodetype_selection;
static	Panel_item	node_subframe_nodecolor_selection;

static	int		showing_node_subframe  = FALSE;


/************************************************************************/
/*									*/
/*		NODE_SUBFRAME AUFBAUEN UND VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_node_subframe ()					*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	show_node_subframe (node)				*/
/*									*/
/*	Setzt die Anzeigen in node_subframe auf den Stand von node und	*/
/*	und XV_SHOW auf TRUE.						*/
/*	Falls node_subframe bereits auf dem Bildschirm steht, wird als	*/
/*	erstes hide_node_subframe ausgefuehrt.				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	void	hide_node_subframe ()				*/
/*									*/
/*	Laesst den Knotensubframe bis zum naechsten Gebrauch wieder	*/
/*	verschwinden (node_subframe wird aus Zeitgruenden nicht		*/
/*	physikalisch geloescht).					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Edge	get_currently_edited_node ()				*/
/*									*/
/*	Der Knoten, der momentan mit node_subframe editiert wird.	*/
/*	Falls gerade keiner editiert wird, empty_node.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	install_fontlist_in_node_subframe     (list)		*/
/*	void	install_nodetypelist_in_node_subframe (list)		*/
/*									*/
/*	void	update_nodelabel_visibility_in_node_subframe ()		*/
/*	Installiert node->label.visibility in node_subframe neu, wobei	*/
/*	node = get_currently_edited_node() (natuerlich nur, wenn dieser	*/
/*	existiert !).							*/
/*									*/
/*	void	install_current_nodefont_in_node_subframe    ()		*/
/*									*/
/************************************************************************/


void	create_node_subframe ()
{
	Menu	node_subframe_menu;
	int	i, row_count = 0;

	node_subframe = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_LABEL,		"Node Editor",
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		FRAME_DONE_PROC,	hide_node_subframe,
		WIN_CLIENT_DATA,	empty_node,
		NULL);

	node_subframe_panel = (Panel)xv_get(node_subframe, FRAME_CMD_PANEL);
	
	node_subframe_clear_nodelabel_button = xv_create(node_subframe_panel, PANEL_BUTTON,
		XV_X,			xv_col(node_subframe_panel, 2),
		XV_Y,			xv_row(node_subframe_panel, row_count),
		PANEL_LABEL_STRING,     "Clear Label",
		PANEL_NOTIFY_PROC,	notify_node_subframe_buttons,
		NULL);
	
	row_count++;
	node_subframe_scale_cycle = xv_create(node_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		scaling_strings_for_cycle,
		XV_X,			xv_col(node_subframe_panel, 2),
		XV_Y,			xv_row(node_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"nodesize",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_node_subframe_cycles,
		NULL);

	row_count++;
	node_subframe_label_visibility_cycle = xv_create(node_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(node_subframe_panel, 2),
		XV_Y,			xv_row(node_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"label      ",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list ("visible", TRUE, "invisible", FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_node_subframe_cycles,
		NULL);

	node_subframe_mark_cycle = xv_create(node_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(node_subframe_panel, 24),
		XV_Y,			xv_row(node_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"node ",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list ("marked",TRUE, "unmarked",FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_node_subframe_cycles,
		NULL);

	row_count++;
	node_subframe_set_nei_cycle = xv_create(node_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		nei_images_for_cycle,
		XV_X,			xv_col(node_subframe_panel, 2),
		XV_Y,			xv_row(node_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"node/edge interface",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_node_subframe_cycles,
		NULL);

	node_subframe_set_nlp_cycle = xv_create(node_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		nlp_images_for_cycle,
		XV_X,			xv_col(node_subframe_panel, 24),
		XV_Y,			xv_row(node_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"nodelabel placement",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_node_subframe_cycles,
		NULL);
	
	row_count++;
	node_subframe_nodetype_selection = xv_create(node_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(node_subframe_panel, 2),
		XV_Y,			xv_row(node_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"nodetype",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICE_IMAGES,	white_icon_svi, 0, /* Dummy */
		PANEL_NOTIFY_PROC,	notify_node_subframe_cycles,
		NULL);
	
	node_subframe_nodefont_selection = xv_create(node_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(node_subframe_panel, 24),
		XV_Y,			xv_row(node_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"label font",
		PANEL_LABEL_BOLD,	TRUE,
/*fis_label
		PANEL_LABEL_Y,		(int)xv_get(node_subframe_nodetype_selection, PANEL_LABEL_Y),
		PANEL_VALUE_Y,		xv_row(node_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT*2,
*/
		PANEL_NOTIFY_PROC,	notify_node_subframe_cycles,
		NULL);
		
	row_count++;
/* Commented out MH conversion
	node_subframe_nodecolor_selection = create_graphed_color_selection_item(
		node_subframe_panel,
		xv_col(node_subframe_panel, 11),
		xv_row(node_subframe_panel, row_count) + 2*DEFAULT_ICON_HEIGHT,
		notify_node_subframe_cycles);
*/	
	window_fit(node_subframe_panel);
	window_fit(node_subframe);
}


void	show_node_subframe (node)
Node	node;
{
	int		x = node_left(node) + node_width(node) + 10,
			y = node_top(node) + node_height(node) + 10;
	int		node_subframe_height, node_subframe_width;
	Graphed_font	font;


	if (showing_node_subframe) hide_node_subframe (node_subframe);
	
	translate_wac_to_base_frame_space (&x,&y);
	node_subframe_height = (int)xv_get(node_subframe, XV_HEIGHT);
	node_subframe_width  = (int)xv_get(node_subframe, XV_WIDTH);
	
	if (node_width(node) == node_height(node)) switch (node_width (node)) {
		case  16 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_16_16, NULL);	break;
		case  32 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_32_32, NULL);	break;
		case  64 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_64_64, NULL);	break;
		case  96 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_96_96, NULL);	break;
		case 128 : xv_set(node_subframe_scale_cycle, PANEL_VALUE ,SCALE_128_128, NULL);	break;
		case 192 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_192_192, NULL);	break;
		case 256 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_256_256, NULL);	break;
		case 384 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_384_384, NULL);	break;
		case 512 : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_512_512, NULL);	break;
		default  : xv_set(node_subframe_scale_cycle, PANEL_VALUE, SCALE_IDENTITY, NULL);	break;
	} else
	xv_set(node_subframe_scale_cycle, 		PANEL_VALUE, SCALE_IDENTITY, NULL);		/*!vergl*/
	xv_set(node_subframe_label_visibility_cycle,	PANEL_VALUE, bool_to_int(node->label.visible), NULL);
	xv_set(node_subframe_mark_cycle,		PANEL_VALUE, iif(is_marked(node),TRUE,FALSE), NULL);
	xv_set(node_subframe_set_nei_cycle,		PANEL_VALUE, node->node_edge_interface, NULL);
	xv_set(node_subframe_set_nlp_cycle,		PANEL_VALUE, node->label.placement, NULL);
	xv_set(node_subframe_nodetype_selection,	PANEL_VALUE, get_nodetype_index (node->type), NULL);
	xv_set(node_subframe_nodefont_selection,	PANEL_VALUE, 
		iif (nodelabel_font(node) != (Graphed_font)NULL, get_font_index (nodelabel_font(node)), current_nodefont_index ) , NULL);
/* Commented ouy MH conversion
	xv_set(node_subframe_nodecolor_selection,	PANEL_VALUE, node->color, NULL);
*/
	xv_set(node_subframe, WIN_CLIENT_DATA, node,
		WIN_X,		maximum (minimum (x, screenwidth  - node_subframe_width),  0),
		WIN_Y,		maximum (minimum (y, screenheight - node_subframe_height), 0),
		XV_SHOW,	TRUE,
		NULL);
		
	showing_node_subframe  = TRUE;
}


static	void	hide_node_subframe (frame)
Frame		frame;	/* unused - for FRAME_DONE_PROC only */
{
	Node	node      = (Node)xv_get(node_subframe, WIN_CLIENT_DATA);
	
	showing_node_subframe = FALSE;
	
	if (!node_is_picked(node))
		unmark_node (node);
		
	xv_set(node_subframe, XV_SHOW, FALSE, NULL);
	xv_set(node_subframe, WIN_CLIENT_DATA, empty_node, NULL);
}



Node	get_currently_edited_node ()
{
	return (Node)xv_get(node_subframe, WIN_CLIENT_DATA);
}



void	install_fontlist_in_node_subframe (list)
char	*list;
{
	xv_set(node_subframe_nodefont_selection, ATTR_LIST, list, NULL);
}


void	install_nodetypelist_in_node_subframe (list)
char	*list;
{
	xv_set(node_subframe_nodetype_selection, ATTR_LIST, list, NULL);
}


void	update_nodelabel_visibility_in_node_subframe ()
{
	Node	node = (Node)xv_get(node_subframe, WIN_CLIENT_DATA);
	
	if (node != empty_node)
		xv_set(node_subframe_label_visibility_cycle, PANEL_VALUE, bool_to_int(node->label.visible), NULL);
}


void	install_current_nodefont_in_node_subframe ()
{
	Node		node = (Node)xv_get(node_subframe,WIN_CLIENT_DATA);
	Graphed_font	font;
	
	if (node != empty_node && node->label.font == (Graphed_font)NULL) {
		xv_set(node_subframe_nodefont_selection, PANEL_VALUE, current_nodefont_index, NULL);
	}
}


void	install_current_nodecolor_in_node_subframe ()
{
	Node	node = (Node)xv_get(node_subframe,WIN_CLIENT_DATA);
	
	if (node != empty_node) {
		xv_set(node_subframe_nodecolor_selection, PANEL_VALUE, current_nodecolor, NULL);
	}
}


/************************************************************************/
/*									*/
/*			NOTIFYPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static	notify_node_subframe_buttons (item, event)		*/
/*	static	notify_node_subframe_cycles  (item, value, event)	*/
/*									*/
/*	Wichtig : diese Prozeduren rufen am Ende force_repainting auf.	*/
/*									*/
/************************************************************************/


static		notify_node_subframe_buttons (item, event)
Panel_item	item;
Event		*event;
{
	Node	node = (Node)xv_get(node_subframe, WIN_CLIENT_DATA);
	
	if (item == node_subframe) {
		hide_node_subframe ();
	} else if (item == node_subframe_clear_nodelabel_button) {
		node_set (node, NODE_LABEL, NULL, 0);
		install_current_nodefont_in_node_subframe ();
	}
	
	force_repainting();
}


static		notify_node_subframe_cycles (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	Node	node = (Node)xv_get(node_subframe, WIN_CLIENT_DATA);
	
	if (item == node_subframe_mark_cycle) {
		set_node_marked (node, iif(value,MARKED_WITH_SQUARES,NOT_MARKED));
	} else if (item == node_subframe_label_visibility_cycle) {
		node_set (node, NODE_LABEL_VISIBILITY, int_to_bool(value), 0);
	} else if (item == node_subframe_set_nei_cycle) {
		node_set (node, NODE_NEI, value, 0);
	} else if (item == node_subframe_set_nlp_cycle) {
		node_set (node, NODE_NLP, value, 0);
	} else if (item == node_subframe_nodetype_selection) {
		node_set (node, NODE_TYPE, value, 0);
	} else if (item == node_subframe_nodefont_selection) {
		node_set (node, NODE_FONT, value, 0);
	} else if (item == node_subframe_nodecolor_selection) {
		node_set (node, NODE_COLOR, value, 0);
	} else if (item == node_subframe_scale_cycle) {
		int	sx = node_width (node),
			sy = node_height (node);
		scale (xv_get(node_subframe_scale_cycle, PANEL_VALUE), &sx, &sy);
		node_set (node, NODE_SIZE, sx,sy, 0);
	}
	
	force_repainting();
}
