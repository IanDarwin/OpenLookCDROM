/* Copyright Universitaet Passau 1990 */
/* GraphEd Source, 1986-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*			node_defaults_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des subframe zum		*/
/*		Einstellen von Default-Knotenattributen			*/
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
/*	void	create_node_defaults_subframe ()			*/
/*	void	show_node_defaults_subframe   (info, complete_proc)	*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/

		
static	void	hide_node_defaults_subframe           ();
static		notify_node_defaults_subframe_buttons ();
static		notify_node_defaults_subframe_cycles  ();



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Frame		node_defaults_subframe;
static	Panel		node_defaults_subframe_panel;

static	Panel_item	node_defaults_subframe_quit_button;
static	Panel_item      node_defaults_subframe_type_cycle;
static	Panel_item      node_defaults_subframe_font_cycle;
static	Panel_item      node_defaults_subframe_scale_cycle;
static	Panel_item	node_defaults_subframe_label_visibility_cycle;
static	Panel_item	node_defaults_subframe_set_nei_cycle;
static	Panel_item	node_defaults_subframe_set_nlp_cycle;
static	Panel_item	node_defaults_subframe_nodecolor_selection;

static	Node_defaults_subframe_info	*info;
static	void				(*completion_proc)();
static	int				showing_node_defaults_subframe = FALSE;

/************************************************************************/
/*									*/
/*		NODE_SUBFRAME AUFBAUEN UND VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_node_defaults_subframe ()			*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	show_node_defaults_subframe (info, completion_proc)	*/
/*									*/
/*	Setzt die Anzeigen in node_defaults_subframe auf den aktuellen	*/
/*	Stand von node und XV_SHOW auf TRUE.				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	void	hide_node_defaults_subframe (node)		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	install_current_nodesize_in_node_defaults_subframe ()	*/
/*	void	install_current_nlp_in_node_defaults_subframe      ()	*/
/*	void	install_current_nei_in_node_defaults_subframe      ()	*/
/*	void	install_current_nlv_in_node_defaults_subframe      ()	*/
/*									*/
/************************************************************************/


void	create_node_defaults_subframe ()
{
	Menu	node_defaults_subframe_menu;
	int	i, row_count = 0;

	node_defaults_subframe = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_LABEL,		"Node Defaults",
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		FRAME_DONE_PROC,	hide_node_defaults_subframe,
		NULL);
	
	node_defaults_subframe_panel = (Panel)xv_get(node_defaults_subframe, FRAME_CMD_PANEL);

	row_count ++;
	node_defaults_subframe_label_visibility_cycle = xv_create(node_defaults_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(node_defaults_subframe_panel, 0),
		XV_Y,			xv_row(node_defaults_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"label",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list ("visible",TRUE, "invisible",FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_node_defaults_subframe_cycles,
		NULL);

	node_defaults_subframe_scale_cycle = xv_create(node_defaults_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		scaling_strings_for_cycle,
		XV_X,			xv_col(node_defaults_subframe_panel, 22),
		XV_Y,			xv_row(node_defaults_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"nodesize",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_node_defaults_subframe_cycles,
		NULL);

	row_count ++;
	node_defaults_subframe_set_nei_cycle = xv_create(node_defaults_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		nei_images_for_cycle,
		XV_X,			xv_col(node_defaults_subframe_panel, 0),
		XV_Y,			xv_row(node_defaults_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"node/edge interface",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_node_defaults_subframe_cycles,
		NULL);

	node_defaults_subframe_set_nlp_cycle = xv_create(node_defaults_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		nlp_images_for_cycle,
		XV_X,			xv_col(node_defaults_subframe_panel, 22),
		XV_Y,			xv_row(node_defaults_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"nodelabel placement",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_node_defaults_subframe_cycles,
		NULL);
	
	row_count ++;
	node_defaults_subframe_type_cycle = xv_create(node_defaults_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(node_defaults_subframe_panel, 0),
		XV_Y,			xv_row(node_defaults_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"   nodetype",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICE_IMAGES,	white_icon_svi, 0, /* Dummy */
		PANEL_NOTIFY_PROC,	notify_node_defaults_subframe_cycles,
		NULL);
	
	node_defaults_subframe_font_cycle = xv_create(node_defaults_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(node_defaults_subframe_panel, 22),
		XV_Y,			xv_row(node_defaults_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"label font",
		PANEL_LABEL_BOLD,	TRUE,
/*fis_label
		PANEL_LABEL_Y,		(int)xv_get(node_defaults_subframe_type_cycle, PANEL_LABEL_Y),
		PANEL_VALUE_Y,		xv_row(node_defaults_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT*2,
*/
		PANEL_NOTIFY_PROC,	notify_node_defaults_subframe_cycles,
		NULL);

/* MH commented out Conversion	
	row_count ++;
	node_defaults_subframe_nodecolor_selection = create_graphed_color_selection_item(
		node_defaults_subframe_panel,
		xv_col(node_defaults_subframe_panel, 11),
		xv_row(node_defaults_subframe_panel, row_count) + 2*DEFAULT_ICON_HEIGHT,
		notify_node_defaults_subframe_cycles);
*/

	window_fit (node_defaults_subframe_panel);
	window_fit (node_defaults_subframe);

	showing_node_defaults_subframe = FALSE;
}


void				show_node_defaults_subframe (caller_info, caller_completion_proc)
Node_defaults_subframe_info	*caller_info;
void				(*caller_completion_proc)();
{
	int	x, y;
	int	node_defaults_subframe_height, node_defaults_subframe_width;

	if (showing_node_defaults_subframe) hide_node_defaults_subframe ();
	
	info            = caller_info;
	completion_proc = caller_completion_proc;
	
	node_defaults_subframe_height = (int)xv_get(node_defaults_subframe, XV_HEIGHT);
	node_defaults_subframe_width  = (int)xv_get(node_defaults_subframe, XV_WIDTH);
	x = screenwidth/2  - node_defaults_subframe_width/2;
	y = screenheight/2 - node_defaults_subframe_height/2;
		
	xv_set(node_defaults_subframe_set_nei_cycle,		PANEL_VALUE, info->attr.node_edge_interface, NULL);
	xv_set(node_defaults_subframe_set_nlp_cycle,		PANEL_VALUE, info->attr.nodelabel_placement, NULL);
	xv_set(node_defaults_subframe_label_visibility_cycle,	PANEL_VALUE, info->attr.label_visibility, NULL);
	xv_set(node_defaults_subframe_type_cycle,		PANEL_VALUE, info->attr.type_index, NULL);
	xv_set(node_defaults_subframe_font_cycle,		PANEL_VALUE, info->attr.font_index, NULL);
	xv_set(node_defaults_subframe_scale_cycle,		PANEL_VALUE, 
		iif (info->attr.width == info->attr.height, size_to_scale (info->attr.width), SCALE_IDENTITY), NULL);
/*	xv_set(node_defaults_subframe_nodecolor_selection,	PANEL_VALUE, info->attr.color, NULL); */
	xv_set(node_defaults_subframe,
		WIN_X,		maximum (minimum (x, screenwidth  - node_defaults_subframe_width),  0),
		WIN_Y,		maximum (minimum (y, screenheight - node_defaults_subframe_height), 0),
		XV_SHOW,	TRUE,
		NULL);
		
	info->showing                  = TRUE;
	showing_node_defaults_subframe = TRUE;
}


static	void	hide_node_defaults_subframe (frame)
Frame		frame;
{
	showing_node_defaults_subframe = FALSE;
	info->showing                  = FALSE;
			
	info->attr.node_edge_interface = (Node_edge_interface)xv_get(node_defaults_subframe_set_nei_cycle, PANEL_VALUE);
	info->attr.nodelabel_placement = (Nodelabel_placement)xv_get(node_defaults_subframe_set_nlp_cycle, PANEL_VALUE);
	info->attr.label_visibility    = int_to_bool ((int)xv_get(node_defaults_subframe_label_visibility_cycle, PANEL_VALUE));
	scale (xv_get(node_defaults_subframe_scale_cycle, PANEL_VALUE), &(info->attr.width), &(info->attr.height));
	info->attr.type_index          = (int)xv_get(node_defaults_subframe_type_cycle, PANEL_VALUE);
	info->attr.font_index          = (int)xv_get(node_defaults_subframe_font_cycle, PANEL_VALUE);
/*	info->attr.color               = (int)xv_get(node_defaults_subframe_nodecolor_selection, PANEL_VALUE); */
	xv_set(node_defaults_subframe, XV_SHOW, FALSE, NULL);
	
	(*completion_proc)(info);
}


void	install_fontlist_in_node_defaults_subframe (list)
char	*list;
{
	xv_set(node_defaults_subframe_font_cycle, ATTR_LIST, list, NULL);
	xv_set(node_defaults_subframe_font_cycle, PANEL_VALUE, current_nodefont_index, NULL);
}


void	install_nodetypelist_in_node_defaults_subframe (list)
char	*list;
{
	xv_set(node_defaults_subframe_type_cycle, ATTR_LIST, list, NULL);
	xv_set(node_defaults_subframe_font_cycle, PANEL_VALUE, current_nodetype_index, NULL);
}
/************************************************************************/
/*									*/
/*			NOTIFYPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static notify_node_defaults_subframe_buttons(item, event)	*/
/*	static notify_node_defaults_subframe_cycles (item, value, event)*/
/*									*/
/************************************************************************/


static		notify_node_defaults_subframe_buttons (item, event)
Panel_item	item;
Event		*event;
{

	if (item == node_defaults_subframe) {
		hide_node_defaults_subframe (node_defaults_subframe);
	}
}


static		notify_node_defaults_subframe_cycles (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
}
