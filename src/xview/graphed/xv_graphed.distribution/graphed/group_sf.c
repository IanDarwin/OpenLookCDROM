/* Copyright Universitaet Passau 1990 */
/* GraphEd Source, 1986-1990 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				group_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt die Verwaltung des group_subframe zum	*/
/*	Editieren von Knoten						*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"
#include "graphed_svi.h"

#include "font.h"
#include "type.h"
#include "group.h"


/************************************************************************/
/*									*/
/*		GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_group_subframe ()				*/
/*	void	show_group_subframe   (group)				*/
/*									*/
/*	void	install_fontlist_in_group_subframe            (list)	*/
/*	void	install_nodetypelist_in_group_subframe        (list)	*/
/*	void	install_edgetypelist_in_group_subframe        (list)	*/
/*									*/
/*	Group	get_currently_edited_group ()				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/

		
static	void		hide_group_subframe           ();
static			notify_group_subframe_buttons ();
static			notify_group_subframe_cycles  ();
static			notify_group_subframe_toggles ();
static	Panel_setting	notify_arrowlength_selection  ();
static	Panel_setting	notify_arrowangle_selection   ();



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	Frame		group_subframe;
static	Panel		group_subframe_panel;

static	Panel_item	group_subframe_quit_button;

static	Panel_item	group_subframe_clear_nodelabel_button;
static	Panel_item      group_subframe_scale_cycle;
static	Panel_item	group_subframe_nodelabel_visibility_cycle;
static	Panel_item	group_subframe_set_nei_cycle;
static	Panel_item	group_subframe_set_nlp_cycle;
static	Panel_item	group_subframe_nodefont_selection;
static	Panel_item	group_subframe_nodetype_selection;
static	Panel_item	group_subframe_nodecolor_selection;

static	Panel_item	group_subframe_clear_edgelabel_button;
static	Panel_item	group_subframe_arrowlength_selection;
static	Panel_item	group_subframe_arrowangle_selection;
static	Panel_item	group_subframe_edgelabel_visibility_cycle;
static	Panel_item	group_subframe_edgefont_selection;
static	Panel_item	group_subframe_edgetype_selection;
static	Panel_item	group_subframe_edgecolor_selection;

static	Panel_item      group_subframe_scale_onoff;
static	Panel_item	group_subframe_nodelabel_visibility_onoff;
static	Panel_item	group_subframe_set_nei_onoff;
static	Panel_item	group_subframe_set_nlp_onoff;
static	Panel_item	group_subframe_nodefont_selection_onoff;
static	Panel_item	group_subframe_nodetype_selection_onoff;
static	Panel_item	group_subframe_nodecolor_selection_onoff;

static	Panel_item	group_subframe_arrowlength_selection_onoff;
static	Panel_item	group_subframe_arrowangle_selection_onoff;
static	Panel_item	group_subframe_edgelabel_visibility_onoff;
static	Panel_item	group_subframe_edgefont_selection_onoff;
static	Panel_item	group_subframe_edgetype_selection_onoff;
static	Panel_item	group_subframe_edgecolor_selection_onoff;


static	int	scale_onoff;
static	int	nodelabel_visibility_onoff;
static	int	set_nei_onoff;
static	int	set_nlp_onoff;
static	int	nodefont_selection_onoff;
static	int	nodetype_selection_onoff;
static	int	nodecolor_selection_onoff;

static	int	arrowlength_selection_onoff;
static	int	arrowangle_selection_onoff;
static	int	edgelabel_visibility_onoff;
static	int	edgefont_selection_onoff;
static	int	edgetype_selection_onoff;
static	int	edgecolor_selection_onoff;

static	int	showing_group_subframe  = FALSE;

static	char	arrowangle_selection_notify_string  [] = {CTRL_A, CTRL_B, CTRL_C, CR, '\0'};
static	char	arrowlength_selection_notify_string [] = {CTRL_A, CTRL_B, CTRL_C, CTRL_D, CR, '\0'};



/************************************************************************/
/*									*/
/*		NODE_SUBFRAME AUFBAUEN UND VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_group_subframe ()				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	show_group_subframe (group)				*/
/*									*/
/*	Setzt die Anzeigen in group_subframe auf den Stand von group	*/
/*	und XV_SHOW auf TRUE.						*/
/*	Falls group_subframe bereits auf dem Bildschirm steht, wird als	*/
/*	erstes hide_group_subframe ausgefuehrt.				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	void	hide_group_subframe ()				*/
/*									*/
/*	Laesst den Subframe bis zum naechsten Gebrauch wieder		*/
/*	verschwinden (group_subframe wird aus Zeitgruenden nicht	*/
/*	physikalisch geloescht).					*/
/*									*/
/*======================================================================*/
/*									*/
/*	Edge	get_currently_edited_group ()				*/
/*									*/
/*	Die Gruppe, die momentan mit group_subframe editiert wird.	*/
/*	Falls gerade keine editiert wird, empty_group.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	install_fontlist_in_group_subframe     (list)		*/
/*	void	install_nodetypelist_in_group_subframe (list)		*/
/*	void	install_edgetypelist_in_group_subframe (list)		*/
/*									*/
/************************************************************************/


void	create_group_subframe ()
{
	Menu	group_subframe_menu;
	int	i, row_count = 0;

	group_subframe = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_LABEL,		"Group Editor",
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		FRAME_DONE_PROC,	hide_group_subframe,
		WIN_CLIENT_DATA,	empty_group,
		NULL);

	/* Die folgenden Konstanten sind die Spalten in die die 	*/
	/* Panel_item's eingetragen werden.				*/
	
#	define COL_11 1
#	define COL_12 4
#	define COL_21 26
#	define COL_22 29
#	define COL_31 51
#	define COL_32 54
#	define COL_41 75
#	define COL_42 78

	group_subframe_panel = (Panel)xv_get(group_subframe, FRAME_CMD_PANEL);

	group_subframe_clear_nodelabel_button = xv_create(group_subframe_panel, PANEL_BUTTON,
		XV_X,			xv_col(group_subframe_panel, COL_12),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_buttons,
		PANEL_LABEL_STRING,     "Clear Nodelabel",
		NULL);

 	row_count++;
	group_subframe_scale_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_11),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_scale_cycle = xv_create(group_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		scaling_strings_for_cycle,
		XV_X,			xv_col(group_subframe_panel, COL_12),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"nodesize",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);

	group_subframe_nodelabel_visibility_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_21),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_nodelabel_visibility_cycle = xv_create(group_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(group_subframe_panel, COL_22),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"label      ",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("visible",TRUE, "invisible",FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);

	row_count++;
	group_subframe_set_nei_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_11),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_set_nei_cycle = xv_create(group_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		nei_images_for_cycle,
		XV_X,			xv_col(group_subframe_panel, COL_12),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"node/edge interface",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);

	group_subframe_set_nlp_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_21),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_set_nlp_cycle = xv_create(group_subframe_panel, PANEL_CYCLE,
		ATTR_LIST,		nlp_images_for_cycle,
		XV_X,			xv_col(group_subframe_panel, COL_22),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"nodelabel placement",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);	

	row_count++;
	group_subframe_nodetype_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_11),
		XV_Y,			xv_row(group_subframe_panel, row_count)+ DEFAULT_ICON_HEIGHT,
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_nodetype_selection = xv_create(group_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(group_subframe_panel, COL_12),
		XV_Y,			xv_row(group_subframe_panel, row_count)+ DEFAULT_ICON_HEIGHT,
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"nodetype",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICE_IMAGES,	white_icon_svi, NULL, /* Dummy */
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);
	
	group_subframe_nodefont_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_21),
		XV_Y,			xv_row(group_subframe_panel, row_count)+ DEFAULT_ICON_HEIGHT,
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_nodefont_selection = xv_create(group_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(group_subframe_panel, COL_22),
		XV_Y,			xv_row(group_subframe_panel, row_count)+ DEFAULT_ICON_HEIGHT,
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"nodelabel font",
		PANEL_LABEL_BOLD,	TRUE,
/*fis_label
		PANEL_LABEL_Y,		(int)xv_get(group_subframe_nodetype_selection, PANEL_LABEL_Y),
		PANEL_VALUE_Y,		xv_row(group_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT*2,
*/
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);
	
	row_count++;
	group_subframe_nodecolor_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_11),
		XV_Y,			xv_row(group_subframe_panel, row_count)+ 2*DEFAULT_ICON_HEIGHT,
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

/* Commented out MH
	group_subframe_nodecolor_selection = create_graphed_color_selection_item(
		group_subframe_panel,
		xv_col(group_subframe_panel, COL_12),
		xv_row(group_subframe_panel, row_count) + 2*DEFAULT_ICON_HEIGHT,
		notify_group_subframe_cycles);
*/

	/****************************************************************/
	/* Now the second part, the edge attributes are created		*/
	/****************************************************************/
	
	row_count = 0;
	group_subframe_clear_edgelabel_button = xv_create(group_subframe_panel, PANEL_BUTTON,
		XV_X,			xv_col(group_subframe_panel, COL_32),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_buttons,
		PANEL_LABEL_STRING,     "Clear Edgelabel",
		NULL);

	row_count++;
	group_subframe_edgelabel_visibility_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_31),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_edgelabel_visibility_cycle = xv_create(group_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(group_subframe_panel, COL_32),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_LABEL_STRING,	"label      ",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	boolean_ordered_list("visible",TRUE, "invisible",FALSE), 0,
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);

	row_count++;
	group_subframe_arrowlength_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_31),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_arrowlength_selection = xv_create(group_subframe_panel, PANEL_TEXT,
		XV_X,				xv_col(group_subframe_panel, COL_32),
		XV_Y,				xv_row(group_subframe_panel, row_count),
		PANEL_LABEL_STRING,		"arrow length",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			"",
		PANEL_VALUE_STORED_LENGTH,	5,
		PANEL_VALUE_DISPLAY_LENGTH,	5,
		PANEL_NOTIFY_STRING,		arrowlength_selection_notify_string,
		PANEL_NOTIFY_LEVEL,		PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC,		notify_arrowlength_selection,
		NULL);
	
	group_subframe_arrowangle_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_41),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_arrowangle_selection = xv_create(group_subframe_panel, PANEL_TEXT,
		PANEL_LABEL_STRING,		"angle",
		PANEL_LABEL_BOLD,		TRUE,
		XV_X,				xv_col(group_subframe_panel, COL_42),
		XV_Y,				xv_row(group_subframe_panel, row_count),
		PANEL_VALUE,			"",
		PANEL_VALUE_STORED_LENGTH,	5,
		PANEL_VALUE_DISPLAY_LENGTH,	5,
		PANEL_NOTIFY_STRING,		arrowangle_selection_notify_string,
		PANEL_NOTIFY_LEVEL,		PANEL_SPECIFIED,
		PANEL_NOTIFY_PROC,		notify_arrowangle_selection,
		NULL);

	row_count++;
	group_subframe_edgetype_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_31),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_edgetype_selection = xv_create(group_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(group_subframe_panel, COL_32),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"edgetype",
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_CHOICE_IMAGES,	white_icon_svi, NULL,
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);
	
	group_subframe_edgefont_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_41),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

	group_subframe_edgefont_selection = xv_create(group_subframe_panel, PANEL_CYCLE,
		XV_X,			xv_col(group_subframe_panel, COL_42),
		XV_Y,			xv_row(group_subframe_panel, row_count),
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_LABEL_STRING,	"edgelabel font",
		PANEL_LABEL_BOLD,	TRUE,
/*fis_label
		PANEL_LABEL_Y,		(int)xv_get(group_subframe_edgetype_selection, PANEL_LABEL_Y),
		PANEL_VALUE_Y,		xv_row(group_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
*/
		PANEL_NOTIFY_PROC,	notify_group_subframe_cycles,
		NULL);
	
	row_count++;
	group_subframe_edgecolor_selection_onoff = xv_create(group_subframe_panel, PANEL_CHECK_BOX,
		XV_X,			xv_col(group_subframe_panel, COL_31),
		XV_Y,			xv_row(group_subframe_panel, row_count) + DEFAULT_ICON_HEIGHT,
		PANEL_NOTIFY_PROC,	notify_group_subframe_toggles,
		NULL);

/* Commented out MH
	group_subframe_edgecolor_selection = create_graphed_color_selection_item(
		group_subframe_panel,
		xv_col(group_subframe_panel, COL_32),
		xv_row(group_subframe_panel, row_count) + 2*DEFAULT_ICON_HEIGHT,
		notify_group_subframe_cycles);
*/

	window_fit(group_subframe_panel);
	window_fit(group_subframe);
}


void	show_group_subframe (group)
Group	group;
{
	int		x, y;
	Rect		group_rect;
	int		group_subframe_height, group_subframe_width;
	
	int		all_equal, is_first;
	Group		g;
	Edge		edge;
	
	int		visible;	/* Variables to determine uniqueness	*/
	Edgetype	type;		/* of edge attributes			*/
	Graphed_font	font;
	int		arrowlength;
	double		arrowangle;
	int		color;
	
	if (showing_group_subframe) hide_group_subframe ();
	
	group_rect = compute_rect_around_group (group);
	x = rect_right (&group_rect) + 10;
	y = rect_top   (&group_rect);
	
	translate_wac_to_base_frame_space (&x,&y);
	group_subframe_height = (int)xv_get(group_subframe, XV_HEIGHT);
	group_subframe_width  = (int)xv_get(group_subframe, XV_WIDTH);
	

	all_equal = TRUE;
	for_group (group, g) {
		if ( (node_width(g->node)  != node_width(group->node)) ||
		     (node_height(g->node) != node_height(group->node)) ) {
			all_equal = FALSE;
			break;
		}
	} end_for_group (group, g);
	if (all_equal) {
		scale_onoff = TRUE;
		xv_set(group_subframe_scale_onoff, PANEL_VALUE, 1, NULL);
		if (node_width(group->node) == node_height(group->node)) switch (node_width(group->node)) {
		     case 16 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_16_16, NULL);    break;
		     case 32 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_32_32, NULL);    break;
		     case 64 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_64_64, NULL);    break;
		     case 96 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_96_96, NULL);    break;
		     case 128 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_128_128, NULL);  break;
		     case 192 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_192_192, NULL);  break;
		     case 256 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_256_256, NULL);  break;
		     case 384 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_384_384, NULL);  break;
		     case 512 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_512_512, NULL);  break;
		     default  : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_IDENTITY, NULL); break;
		} else {
		    xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_IDENTITY, NULL);
		}
	} else {
		scale_onoff = FALSE;
		xv_set(group_subframe_scale_onoff, PANEL_VALUE, 0, NULL);
		if (current_node_width == current_node_height) switch (current_node_width) {
		     case 16 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_16_16, NULL);    break;
		     case 32 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_32_32, NULL);    break;
		     case 64 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_64_64, NULL);    break;
		     case 96 :  xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_96_96, NULL);    break;
		     case 128 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_128_128, NULL);  break;
		     case 192 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_192_192, NULL);  break;
		     case 256 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_256_256, NULL);  break;
		     case 384 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_384_384, NULL);  break;
		     case 512 : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_512_512, NULL);  break;
		     default  : xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_IDENTITY, NULL); break;
		} else {
		    xv_set(group_subframe_scale_cycle, PANEL_VALUE, SCALE_IDENTITY, NULL);
		}
	}

	all_equal = TRUE;
	for_group (group, g) {
		if ((g->node->label.visible != group->node->label.visible)) {
			all_equal = FALSE;
			break;
		}
	} end_for_group (group, g);
	if (all_equal) {
		nodelabel_visibility_onoff  = TRUE;
		xv_set(group_subframe_nodelabel_visibility_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_nodelabel_visibility_cycle, PANEL_VALUE,	bool_to_int(group->node->label.visible), NULL);
	} else {
		nodelabel_visibility_onoff  = FALSE;
		xv_set(group_subframe_nodelabel_visibility_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_nodelabel_visibility_cycle, PANEL_VALUE,	bool_to_int(current_nodelabel_visibility), NULL);
	}

	all_equal = TRUE;
	for_group (group, g) {
		if ((g->node->label.placement != group->node->label.placement)) {
			all_equal = FALSE;
			break;
		}
	} end_for_group (group, g);
	if (all_equal) {
		set_nlp_onoff  = TRUE;
		xv_set(group_subframe_set_nlp_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_set_nlp_cycle, PANEL_VALUE, group->node->label.placement, NULL);
	} else {
		set_nlp_onoff  = FALSE;
		xv_set(group_subframe_set_nlp_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_set_nlp_cycle, PANEL_VALUE, current_nodelabel_placement, NULL);
	}

	all_equal = TRUE;
	for_group (group, g) {
		if ((g->node->node_edge_interface != group->node->node_edge_interface)) {
			all_equal = FALSE;
			break;
		}
	} end_for_group (group, g);
	if (all_equal) {
		set_nei_onoff  = TRUE;
		xv_set(group_subframe_set_nei_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_set_nei_cycle, PANEL_VALUE, group->node->node_edge_interface, NULL);
	} else {
		set_nei_onoff  = FALSE;
		xv_set(group_subframe_set_nei_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_set_nei_cycle, PANEL_VALUE, current_node_edge_interface, NULL);
	}
	
	all_equal = TRUE;
	for_group (group, g) {
		if ((g->node->type != group->node->type)) {
			all_equal = FALSE;
			break;
		}
	} end_for_group (group, g);
	if (all_equal) {
		nodetype_selection_onoff  = TRUE;
		xv_set(group_subframe_nodetype_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_nodetype_selection, PANEL_VALUE, get_nodetype_index (group->node->type), NULL);
	} else {
		nodetype_selection_onoff  = FALSE;
		xv_set(group_subframe_nodetype_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_nodetype_selection, PANEL_VALUE, current_nodetype_index, NULL);
	}
	
	all_equal = TRUE;
	for_group (group, g) {
		if ((g->node->label.font != group->node->label.font)) {
			all_equal = FALSE;
			break;
		}
	} end_for_group (group, g);
	if (all_equal) {
		nodefont_selection_onoff  = TRUE;
		xv_set(group_subframe_nodefont_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_nodefont_selection, PANEL_VALUE, get_font_index (group->node->label.font), NULL);
	} else {
		nodefont_selection_onoff  = FALSE;
		xv_set(group_subframe_nodefont_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_nodefont_selection, PANEL_VALUE, current_nodefont_index, NULL);
	}
	
	all_equal = TRUE;
	for_group (group, g) {
		if ((g->node->color != group->node->color)) {
			all_equal = FALSE;
			break;
		}
	} end_for_group (group, g);
	if (all_equal) {
		nodecolor_selection_onoff  = TRUE;
/* Commented out MH conversion
		xv_set(group_subframe_nodecolor_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_nodecolor_selection, PANEL_VALUE, group->node->color, NULL);
*/
	} else {
		nodecolor_selection_onoff  = FALSE;
/* Commented out MH conversion
		xv_set(group_subframe_nodecolor_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_nodecolor_selection, PANEL_VALUE, current_nodecolor, NULL);
*/
	}
	
	
	
	all_equal = TRUE;
	is_first  = TRUE;
	visible = current_edgelabel_visibility;
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) if (contains_group_node (group, edge->target)) {
			if (is_first) {
				visible = edge->label.visible;
				is_first = FALSE;
			} else if (edge->label.visible != visible) {
				all_equal = FALSE;
				break;
			}
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);
	if (all_equal) {
		edgelabel_visibility_onoff  = TRUE;
		xv_set(group_subframe_edgelabel_visibility_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_edgelabel_visibility_cycle, PANEL_VALUE, bool_to_int(visible), NULL);
	} else {
		edgelabel_visibility_onoff  = FALSE;
		xv_set(group_subframe_edgelabel_visibility_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_edgelabel_visibility_cycle, PANEL_VALUE,	bool_to_int(current_edgelabel_visibility), NULL);
	}

	
	all_equal = TRUE;
	is_first  = TRUE;
	type = get_current_edgetype ();
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) if (contains_group_node (group, edge->target)) {
			if (is_first) {
				type = edge->type;
				is_first = FALSE;
			} else if (edge->type != type) {
				all_equal = FALSE;
				break;
			}
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);
	if (all_equal) {
		edgetype_selection_onoff  = TRUE;
		xv_set(group_subframe_edgetype_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_edgetype_selection, PANEL_VALUE, get_edgetype_index (type), NULL);
	} else {
		edgetype_selection_onoff  = FALSE;
		xv_set(group_subframe_edgetype_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_edgetype_selection, PANEL_VALUE, current_edgetype_index, NULL);
	}
	
	all_equal = TRUE;
	is_first  = TRUE;
	font = get_current_edgefont();
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) if (contains_group_node (group, edge->target)) {
			if (is_first) {
				font = edge->label.font;
				is_first = FALSE;
			} else if (edge->label.font != font) {
				all_equal = FALSE;
				break;
			}
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);
	if (all_equal) {
		edgefont_selection_onoff  = TRUE;
		xv_set(group_subframe_edgefont_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_edgefont_selection, PANEL_VALUE, get_font_index (font), NULL);
	} else {
		edgefont_selection_onoff  = FALSE;
		xv_set(group_subframe_edgefont_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_edgefont_selection, PANEL_VALUE, current_edgefont_index, NULL);
	}
	
	all_equal = TRUE;
	is_first  = TRUE;
	arrowlength = current_arrow_length;
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) if (contains_group_node (group, edge->target)) {
			if (is_first) {
				arrowlength = edge->arrow.length;
				is_first = FALSE;
			} else if (edge->arrow.length != arrowlength) {
				all_equal = FALSE;
				break;
			}
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);
	if (all_equal) {
		arrowlength_selection_onoff  = TRUE;
		xv_set(group_subframe_arrowlength_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_arrowlength_selection, PANEL_VALUE, int_to_ascii(arrowlength), NULL);
	} else {
		arrowlength_selection_onoff  = FALSE;
		xv_set(group_subframe_arrowlength_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_arrowlength_selection, PANEL_VALUE, int_to_ascii(current_arrow_length), NULL);
	}
	
	all_equal = TRUE;
	is_first  = TRUE;
	arrowangle = current_arrow_angle;
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) if (contains_group_node (group, edge->target)) {
			if (is_first) {
				arrowangle = edge->arrow.angle;
				is_first = FALSE;
			} else if (edge->arrow.angle != arrowangle) {
				all_equal = FALSE;
				break;
			}
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);
	if (all_equal) {
		arrowangle_selection_onoff  = TRUE;
		xv_set(group_subframe_arrowangle_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_arrowangle_selection, PANEL_VALUE, int_to_ascii ((int)rad_to_deg(arrowangle)), NULL);
	} else {
		arrowangle_selection_onoff  = FALSE;
		xv_set(group_subframe_arrowangle_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_arrowangle_selection, PANEL_VALUE, int_to_ascii ((int)rad_to_deg(current_arrow_angle)), NULL);
	}
	
	all_equal = TRUE;
	is_first  = TRUE;
	color = current_edgecolor;
	for_group (group, g) {
		for_edge_sourcelist (g->node, edge) if (contains_group_node (group, edge->target)) {
			if (is_first) {
				color = edge->color;
				is_first = FALSE;
			} else if (edge->color != color) {
				all_equal = FALSE;
				break;
			}
		} end_for_edge_sourcelist (g->node, edge);
	} end_for_group (group, g);
	if (all_equal) {
		edgecolor_selection_onoff  = TRUE;
/* Commented out MH
		xv_set(group_subframe_edgecolor_selection_onoff, PANEL_VALUE, 1, NULL);
		xv_set(group_subframe_edgecolor_selection, PANEL_VALUE, color, NULL);
*/
	} else {
		edgecolor_selection_onoff  = FALSE;
/* Commented out MH
		xv_set(group_subframe_edgecolor_selection_onoff, PANEL_VALUE, 0, NULL);
		xv_set(group_subframe_edgecolor_selection, PANEL_VALUE, current_edgecolor, NULL);
*/
	}


	xv_set(group_subframe,
		WIN_CLIENT_DATA, group,
		WIN_X,           maximum (minimum (x, screenwidth  - group_subframe_width),  0),
		WIN_Y,           maximum (minimum (y, screenheight - group_subframe_height), 0),
		XV_SHOW,	TRUE,
		NULL);
	
	showing_group_subframe  = TRUE;
}


static	void	hide_group_subframe (frame)
Frame		frame;
{
	showing_group_subframe = FALSE;

	xv_set(group_subframe, XV_SHOW, FALSE, NULL);
	xv_set(group_subframe, WIN_CLIENT_DATA, empty_group, NULL);
}



Group	get_currently_edited_group ()
{
	return (Group)xv_get(group_subframe, WIN_CLIENT_DATA);
}



void	install_fontlist_in_group_subframe (list)
char	*list;
{
	xv_set(group_subframe_nodefont_selection, ATTR_LIST, list, NULL);
	xv_set(group_subframe_edgefont_selection, ATTR_LIST, list, NULL);
}


void	install_nodetypelist_in_group_subframe (list)
char	*list;
{
	xv_set(group_subframe_nodetype_selection, ATTR_LIST, list, NULL);
}


void	install_edgetypelist_in_group_subframe (list)
char	*list;
{
	xv_set(group_subframe_edgetype_selection, ATTR_LIST, list, NULL); 
}
/************************************************************************/
/*									*/
/*			NOTIFYPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static	notify_group_subframe_buttons (item, event)		*/
/*	static	notify_group_subframe_cycles  (item, value, event)	*/
/*									*/
/*	Wichtig : diese Prozeduren rufen am Ende force_repainting auf.	*/
/*									*/
/************************************************************************/


static		notify_group_subframe_buttons (item, event)
Panel_item	item;
Event		*event;
{
	Group	group = (Group)xv_get(group_subframe, WIN_CLIENT_DATA);
	Group	g;
	Edge	edge;

	if (item == group_subframe) {
		hide_group_subframe (group_subframe);
	} else if (item == group_subframe_clear_nodelabel_button) {
		group_set (group, NODE_LABEL, NULL, 0);
	} else if (item == group_subframe_clear_edgelabel_button) {
		group_set (group, EDGE_LABEL, NULL, 0);
	}

	force_repainting();
}


static		notify_group_subframe_cycles (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	Group	group = (Group)xv_get(group_subframe, WIN_CLIENT_DATA);
	Group	g;
	Edge	edge;
	
	
	if (item == group_subframe_nodelabel_visibility_cycle) {
		group_set (group, NODE_LABEL_VISIBILITY, int_to_bool(value), 0);
		xv_set(group_subframe_nodelabel_visibility_onoff, PANEL_VALUE, 1, NULL);
		nodelabel_visibility_onoff = TRUE;
	} else if (item == group_subframe_set_nei_cycle) {
		group_set (group, NODE_NEI, value, 0);
		xv_set(group_subframe_set_nei_onoff, PANEL_VALUE, 1, NULL);
		set_nei_onoff = TRUE;
	} else if (item == group_subframe_set_nlp_cycle) {
		group_set (group, NODE_NLP, value, 0);
		xv_set(group_subframe_set_nlp_onoff, PANEL_VALUE, 1, NULL);
		set_nlp_onoff = TRUE;
	} else if (item == group_subframe_nodetype_selection) {
		group_set (group, NODE_TYPE, value, 0);
		xv_set(group_subframe_nodetype_selection_onoff, PANEL_VALUE, 1, NULL);
		nodetype_selection_onoff = TRUE;
	} else if (item == group_subframe_nodefont_selection) {
		group_set (group, NODE_FONT, value, 0);
		xv_set(group_subframe_nodefont_selection_onoff, PANEL_VALUE, 1, NULL);
		nodefont_selection_onoff = TRUE;
	} else if (item == group_subframe_scale_cycle) {
		for_group (group, g) {
			int	sx = node_width  (g->node),
				sy = node_height (g->node);
			scale (xv_get(group_subframe_scale_cycle, PANEL_VALUE), &sx, &sy);
			node_set (g->node, ONLY_SET, NODE_SIZE, sx,sy, 0);
		} end_for_group (group, g);
		group_set (group, RESTORE_IT, 0);
		xv_set(group_subframe_scale_onoff, PANEL_VALUE, 1, NULL);
		scale_onoff = TRUE;
	} else if (item == group_subframe_nodecolor_selection) {
		group_set (group, NODE_COLOR, value, 0);
/* Commented out MH Conversion
		xv_set(group_subframe_nodecolor_selection_onoff, PANEL_VALUE, 1, NULL);
*/
		nodecolor_selection_onoff = TRUE;
	} else if (item == group_subframe_edgelabel_visibility_cycle) {
		group_set (group, EDGE_LABEL_VISIBILITY, int_to_bool(value), 0);
		xv_set(group_subframe_edgelabel_visibility_onoff, PANEL_VALUE, 1, NULL);
		edgelabel_visibility_onoff = TRUE;
	} else if (item == group_subframe_edgetype_selection) {
		group_set (group, EDGE_TYPE, value, 0);
		xv_set(group_subframe_edgetype_selection_onoff, PANEL_VALUE, 1, NULL);
		edgetype_selection_onoff = TRUE;
	} else if (item == group_subframe_edgefont_selection) {
		group_set (group, EDGE_FONT, value, 0);
		xv_set(group_subframe_edgefont_selection_onoff, PANEL_VALUE, 1, NULL);
		edgefont_selection_onoff = TRUE;
	} else if (item == group_subframe_edgecolor_selection) {
		group_set (group, EDGE_COLOR, value, 0);
/* Commented out MH Conversion
		xv_set(group_subframe_edgecolor_selection_onoff, PANEL_VALUE, 1, NULL);
*/
		edgecolor_selection_onoff = TRUE;
	}
	
	force_repainting();
}



static		notify_group_subframe_toggles (item, value, event)
Panel_item	item;
int		value;
Event		*event;
{

#	define	call_corresponding_cycle(c) \
	notify_group_subframe_cycles ((c), xv_get((c), PANEL_VALUE), (Event *)NULL);

	if (item == group_subframe_nodelabel_visibility_onoff) {
		call_corresponding_cycle (group_subframe_nodelabel_visibility_cycle);
	} else if (item == group_subframe_set_nei_onoff) {
		call_corresponding_cycle (group_subframe_set_nei_cycle);
	} else if (item == group_subframe_set_nlp_onoff) {
		call_corresponding_cycle (group_subframe_set_nlp_cycle);					
	} else if (item == group_subframe_nodetype_selection_onoff) {
		call_corresponding_cycle (group_subframe_nodetype_selection);
	} else if (item == group_subframe_nodefont_selection_onoff) {
		call_corresponding_cycle (group_subframe_nodefont_selection);
	} else if (item == group_subframe_scale_onoff) {
		call_corresponding_cycle (group_subframe_scale_cycle);
	} else if (item == group_subframe_nodecolor_selection_onoff) {
/* Commented out MH Conversion
		call_corresponding_cycle (group_subframe_nodecolor_selection);
*/
	} else if (item == group_subframe_edgelabel_visibility_onoff) {
		call_corresponding_cycle (group_subframe_edgelabel_visibility_cycle);
	} else if (item == group_subframe_edgetype_selection_onoff) {
		call_corresponding_cycle (group_subframe_edgetype_selection);
	} else if (item == group_subframe_edgefont_selection_onoff) {
		call_corresponding_cycle (group_subframe_edgefont_selection);
	} else if (item == group_subframe_arrowangle_selection_onoff) {
		notify_arrowangle_selection (group_subframe_arrowangle_selection,  (Event *)NULL);
	} else if (item == group_subframe_arrowlength_selection_onoff) {
		notify_arrowlength_selection (group_subframe_arrowlength_selection, (Event *)NULL);
	}else if (item == group_subframe_edgecolor_selection_onoff) {
/* Commented out MH Conversion
		notify_arrowlength_selection (group_subframe_edgecolor_selection, (Event *)NULL);
*/
	}

#	undef call_corresponding_cycle
}


static	Panel_setting	notify_arrowlength_selection (item, event)
Panel_item		item;
Event			*event;
{
	Group	group = (Group)xv_get(group_subframe, WIN_CLIENT_DATA);
	Group	g;
	int	length;
	Edge	edge;
	
	if (event != (Event *)NULL) switch (event_id (event)) {
		case CTRL_A :	length = 0; 	break;
		case CTRL_B : 	length = 8; 	break;
		case CTRL_C :	length = 12;	break;
		case CTRL_D :	length = 16;	break;
		case CR :	length = atoi ((char *)xv_get(group_subframe_arrowlength_selection, PANEL_VALUE));
				break;
		default :	return PANEL_NONE;
				break;
	} else {
		length = atoi ((char *)xv_get(group_subframe_arrowlength_selection, PANEL_VALUE));

	}
	
	xv_set(group_subframe_arrowlength_selection, PANEL_VALUE, int_to_ascii(length), NULL);
	xv_set(group_subframe_arrowlength_selection_onoff, PANEL_VALUE, 1, NULL);

	group_set (group, EDGE_ARROW_LENGTH, length, 0);
	
	force_repainting ();
	return PANEL_NONE;
}


static	Panel_setting	notify_arrowangle_selection (item, event)
Panel_item		item;
Event			*event;
{
	Group	group = (Group)xv_get(group_subframe, WIN_CLIENT_DATA);
	double	angle;
	Group	g;
	Edge	edge;
	
	if (event != (Event *)NULL) switch (event_id (event)) {
		case CTRL_A :	angle = deg_to_rad(30);	break;
		case CTRL_B :	angle = deg_to_rad(45);	break;
		case CTRL_C :	angle = deg_to_rad(60);	break;
		case CR :	angle  = deg_to_rad (atoi ((char *)xv_get(group_subframe_arrowangle_selection, PANEL_VALUE)));
				break;
		default :	return PANEL_NONE;
				break;
	} else
		angle  = deg_to_rad (atoi ((char *)xv_get(group_subframe_arrowangle_selection, PANEL_VALUE)));
	
	xv_set(group_subframe_arrowangle_selection, PANEL_VALUE, int_to_ascii(rad_to_deg(angle)), NULL);
	xv_set(group_subframe_arrowangle_selection_onoff, PANEL_VALUE, 1, NULL);
	 
	group_set (group, EDGE_ARROW_ANGLE, angle, 0);
	
	force_repainting ();
	return PANEL_NONE;
}
