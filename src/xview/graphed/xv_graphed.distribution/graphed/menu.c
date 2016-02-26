/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				menu.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul uebernimmt die Verwaltung des main_menu, des	*/
/*	Menues der working_area.					*/
/*									*/
/************************************************************************/


#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"
#include "graphed_mpr.h"
#include "menu.h"
#include "user.h"


/* Trennstrich im Menue nach Mac-Art	*/

#define MENU_OLD_SEPARATOR(string)			\
	MENU_ITEM,					\
		MENU_STRING,		string,		\
		MENU_CLIENT_DATA,	NO_ACTION,	\
		NULL


/* Commented MH conversion
#define	MENU_SEPARATOR(string)					\
	MENU_ITEM,						\
		MENU_IMAGE, menu_create_separator ((string)),	\
		MENU_CLIENT_DATA, NO_ACTION,			\
		XV_MARGIN,      0,				\
		0
*/

#define MENU_SEPARATOR(string) \
	MENU_ITEM,			           \
		MENU_STRING,		string,    \
		MENU_CLIENT_DATA,	NO_ACTION, \
		0


#define MENU_EMPTY_SEPARATOR				\
	MENU_ITEM,					\
		MENU_STRING,		"",		\
		MENU_CLIENT_DATA,	NO_ACTION,	\
		MENU_INACTIVE,		TRUE,		\
		NULL

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_working_area_menu ()				*/
/*									*/
/*	void	install_node_edge_interface_in_menu (nei)		*/
/*	void	install_nodelabel_placement_in_menu (nlp)		*/
/*	void	install_nodesize_in_menu      (x,y)			*/
/*	void	install_edgelabelsize_in_menu (x,y)			*/
/*	void	install_arrowlength_in_menu  (length)			*/
/*	void	install_arrowangle_in_menu   (angle)			*/
/*	void	install_nodelabel_visibility_in_menu (visible)		*/
/*	void	install_edgelabel_visibility_in_menu (visible)		*/
/*	void	install_grid_in_menu (width)				*/
/*									*/
/*	Pixrect	*menu_create_separator (string)				*/
/*									*/
/*	void	set_menu_selection ()					*/
/*									*/
/*	void	activate_menu_item   (action)				*/
/*	void	inactivate_menu_item (action)				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/


Menu		main_menu;

		
/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


	Menu	create_submenu;
static	Menu		graph_submenu;
static	Menu		production_submenu;
static	Menu		embedding_submenu;
static	Menu		node_defaults_submenu;
static	Menu			node_edge_interface_menu;
static	Menu			nodelabel_placement_menu;
static	Menu			nodelabel_visibility_menu;
static	Menu			nodesize_scaling_menu;
static	Menu		edge_defaults_submenu;
static	Menu			edgelabelsize_menu;
static	Menu			edgelabel_visibility_menu;
static	Menu			arrowlength_menu;
static	Menu			arrowangle_menu;
	Menu	edit_submenu;
	Menu	gragra_submenu;
	Menu	file_submenu;
	Menu	misc_submenu;
	Menu	tools_submenu;
	Menu	goodies_submenu;
	Menu	layout_submenu;
	Menu	user_submenu;
	Menu	about_submenu;

typedef	struct	{
	User_action	main_action;
	User_action	node_edge_interface;
	User_action	nodelabel_placement;
	User_action	nodelabel_visibility;
	User_action	all_nodelabel_visibility;
	User_action	nodesize_scaling;
	User_action	edgelabelsize_scaling;
	User_action	edgelabel_visibility;
	User_action	all_edgelabel_visibility;
	User_action	arrowlength;
	User_action	arrowangle;
	User_action	gragra_type;
	User_action	grid;
	}
	Selected;

Selected	selected;      /* Siehe set_menu_selection unten	*/


/************************************************************************/
/*									*/
/*			LOKALE FUNKTIONEN				*/
/*									*/
/************************************************************************/


static	void	mark_working_area_menu   ();
static	void	unmark_working_area_menu ();
static	void	set_menu_default_item    ();


/************************************************************************/
/*									*/
/*			MAIN_MENU AUFBAUEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_working_area_menu ()				*/
/*									*/
/*	Baut main_menu auf.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	Zum Aufbau :							*/
/*									*/
/*	Ein menu_item hat folgende Gestalt :				*/
/*									*/
/*	User_action	client_data;					*/
/*									*/
/*	MENU_ITEM,							*/
/*		MENU_IMAGE / MENU_STRING, ...,				*/
/*		...							*/
/*		MENU_CLIENT_DATA, client_data, (Auch bei Pullright !)	*/
/*	0,								*/
/*									*/
/*	Dieses Item wird im folgenden (von aussen) immer ueber		*/
/*	client_data angesprochen; bei Auswahl eines Items durch den	*/
/*	Benutzer gibt client_data an, welche Aktion mit diesem Item	*/
/*	assoziiert ist.							*/
/*	Das Attribut MENU_BOXED wird verwendet, um ein Item zu		*/
/*	markieren. (Out of Date)					*/
/*									*/
/************************************************************************/


static	void	graphed_menu_action_proc (menu, menu_item)
Menu		menu;
Menu_item	menu_item;
{
	menu_called_from = MENU_CALLED_FROM_CANVAS;

	dispatch_user_action(
		(User_action)menu_get (menu_item, MENU_CLIENT_DATA));

	menu_called_from = MENU_CALLED_FROM_NOWHERE;
}


void	create_working_area_menu ()
{
	node_edge_interface_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_IMAGE,       nei_images [(int)NO_NODE_EDGE_INTERFACE],
			MENU_CLIENT_DATA, NEI_NO_NODE_EDGE_INTERFACE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_IMAGE,       nei_images [(int)TO_BORDER_OF_BOUNDING_BOX],
			MENU_CLIENT_DATA, NEI_TO_BORDER_OF_BOUNDING_BOX,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_IMAGE,       nei_images [(int)TO_CORNER_OF_BOUNDING_BOX],
			MENU_CLIENT_DATA, NEI_TO_CORNER_OF_BOUNDING_BOX,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_IMAGE,       nei_images [(int)CLIPPED_TO_MIDDLE_OF_NODE],
			MENU_CLIENT_DATA, NEI_CLIPPED_TO_MIDDLE_OF_NODE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_IMAGE,       nei_images [(int)SPECIAL_NODE_EDGE_INTERFACE],
			MENU_CLIENT_DATA, NEI_SPECIAL,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	nodelabel_placement_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_IMAGE,       nlp_images [(int)NODELABEL_MIDDLE],
			MENU_CLIENT_DATA, NLP_MIDDLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_IMAGE,       nlp_images [(int)NODELABEL_UPPERLEFT],
			MENU_CLIENT_DATA, NLP_UPPERLEFT,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM, 
			MENU_IMAGE,       nlp_images [(int)NODELABEL_UPPERRIGHT],
			MENU_CLIENT_DATA, NLP_UPPERRIGHT,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_IMAGE,       nlp_images [(int)NODELABEL_LOWERLEFT],
			MENU_CLIENT_DATA, NLP_LOWERLEFT,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_IMAGE,       nlp_images [(int)NODELABEL_LOWERRIGHT],
			MENU_CLIENT_DATA, NLP_LOWERRIGHT,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	nodelabel_visibility_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "on",
			MENU_CLIENT_DATA, SET_NODELABEL_VISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "off",
			MENU_CLIENT_DATA, SET_NODELABEL_INVISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,      "all nodelabel on",
			MENU_CLIENT_DATA, SET_ALL_NODELABEL_VISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "all nodelabel off",
			MENU_CLIENT_DATA, SET_ALL_NODELABEL_INVISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	nodesize_scaling_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      scaling_strings [(int)SCALE_16_16],
			MENU_CLIENT_DATA, SCALE_NODESIZE_16_16,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      scaling_strings [(int)SCALE_32_32],
			MENU_CLIENT_DATA, SCALE_NODESIZE_32_32,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      scaling_strings [(int)SCALE_64_64],
			MENU_CLIENT_DATA, SCALE_NODESIZE_64_64,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      scaling_strings [(int)SCALE_128_128],
			MENU_CLIENT_DATA, SCALE_NODESIZE_128_128,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "as selected",
			MENU_CLIENT_DATA, SCALE_NODESIZE_AS_SELECTED,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	edgelabelsize_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "64 x 64",
			MENU_CLIENT_DATA, SCALE_EDGELABELSIZE_64_64,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "256 x 64",
			MENU_CLIENT_DATA, SCALE_EDGELABELSIZE_256_64,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "unconstrained",
			MENU_CLIENT_DATA, SCALE_EDGELABELSIZE_UNCONSTRAINED,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	edgelabel_visibility_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "on",
			MENU_CLIENT_DATA, SET_EDGELABEL_VISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "off",
			MENU_CLIENT_DATA, SET_EDGELABEL_INVISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,      "all edgelabel on",
			MENU_CLIENT_DATA, SET_ALL_EDGELABEL_VISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "all egdelabel off",
			MENU_CLIENT_DATA, SET_ALL_EDGELABEL_INVISIBLE,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	arrowlength_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "0",
			MENU_CLIENT_DATA, SCALE_ARROWLENGTH_0,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "8",
			MENU_CLIENT_DATA, SCALE_ARROWLENGTH_8,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "12",
			MENU_CLIENT_DATA, SCALE_ARROWLENGTH_12,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "16",
			MENU_CLIENT_DATA, SCALE_ARROWLENGTH_16,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "as selected",
			MENU_CLIENT_DATA, SCALE_ARROWLENGTH_AS_SELECTED,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	arrowangle_menu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "30",
			MENU_CLIENT_DATA, SCALE_ARROWANGLE_30,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "45",
			MENU_CLIENT_DATA, SCALE_ARROWANGLE_45,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "60",
			MENU_CLIENT_DATA, SCALE_ARROWANGLE_60,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "as selected",
			MENU_CLIENT_DATA, SCALE_ARROWANGLE_AS_SELECTED,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	graph_submenu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "directed    [C-D]",
			MENU_CLIENT_DATA, CREATE_DIRECTED_GRAPH,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "undirected  [C-U]",
			MENU_CLIENT_DATA, CREATE_UNDIRECTED_GRAPH,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	production_submenu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "directed",
			MENU_CLIENT_DATA, CREATE_DIRECTED_PRODUCTION,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "undirected",
			MENU_CLIENT_DATA, CREATE_UNDIRECTED_PRODUCTION,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	embedding_submenu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "directed",
			MENU_CLIENT_DATA, CREATE_DIRECTED_EMBEDDING,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,      "undirected",
			MENU_CLIENT_DATA, CREATE_UNDIRECTED_EMBEDDING,
			MENU_ACTION_PROC, graphed_menu_action_proc,
			NULL,
		NULL);

	node_defaults_submenu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "...",
			MENU_CLIENT_DATA, NODE_DEFAULTS,
			MENU_ACTION_PROC, graphed_menu_action_proc,
		NULL,
		MENU_PULLRIGHT_ITEM,
			"node/edge-interface",
			node_edge_interface_menu,
		MENU_PULLRIGHT_ITEM,
			"label placement    ",
			nodelabel_placement_menu,
		MENU_PULLRIGHT_ITEM,
			"label visibility   ",
			nodelabel_visibility_menu,
		MENU_PULLRIGHT_ITEM,
			"nodesize           ",
			nodesize_scaling_menu,
		NULL);

	edge_defaults_submenu = xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING,      "...",
			MENU_CLIENT_DATA, EDGE_DEFAULTS,
			MENU_ACTION_PROC, graphed_menu_action_proc,
		NULL,
		MENU_PULLRIGHT_ITEM,
			"label size         ",
			edgelabelsize_menu,
		MENU_PULLRIGHT_ITEM,
			"label visibility   ",
			edgelabel_visibility_menu,
		MENU_PULLRIGHT_ITEM,
			"arrow length       ",
			arrowlength_menu,
		MENU_PULLRIGHT_ITEM,
			"arrow angle        ",
			arrowangle_menu,
		NULL);


	create_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:create_submenu",
		MENU_TITLE_ITEM,		"Create",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Create",
		MENU_ITEM,
			MENU_STRING,		"Create Mode",
			MENU_CLIENT_DATA,	CREATE_MODE,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Graph",
			MENU_CLIENT_DATA,	CREATE_GRAPH,
			MENU_PULLRIGHT,		graph_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Production",
			MENU_CLIENT_DATA,	CREATE_PRODUCTION,
			MENU_PULLRIGHT,		production_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Embedding",
			MENU_CLIENT_DATA,	CREATE_EMBEDDING,
			MENU_PULLRIGHT,		embedding_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Window",
			MENU_CLIENT_DATA,	CREATE_BUFFER,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Node Defaults",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		node_defaults_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Edge Defaults",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		edge_defaults_submenu,
			NULL,
		NULL);

	edit_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:edit_submenu",
		MENU_TITLE_ITEM,		"Edit",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Edit",
		MENU_ITEM,
			MENU_STRING,		"Edit Mode",
			MENU_CLIENT_DATA,	SELECT_MODE,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Select Graph",
			MENU_CLIENT_DATA,	SELECT_GRAPH_OF_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Select all",
			MENU_CLIENT_DATA,	SELECT_ALL,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM, 
			MENU_STRING,		"Edit Selection",
			MENU_CLIENT_DATA,	EDIT_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Edit Graph",
			MENU_CLIENT_DATA,	EDIT_GRAPH_OF_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Delete",
			MENU_CLIENT_DATA,	DELETE_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Delete all",
			MENU_CLIENT_DATA,	DELETE_ALL,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Put",
			MENU_CLIENT_DATA,	PUT_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Get",
			MENU_CLIENT_DATA,	GET_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Split",
			MENU_CLIENT_DATA,	SPLIT_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Merge",
			MENU_CLIENT_DATA,	MERGE_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Reverse Edge",
			MENU_CLIENT_DATA,	REVERSE_EDGE,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Un <-> Directed",
			MENU_CLIENT_DATA,	SWAP_SELECTED_GRAPH_DIRECTEDNESS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Zoom in",
			MENU_CLIENT_DATA,	EXPAND_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Zoom out",
			MENU_CLIENT_DATA,	SHRINK_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Center",
			MENU_CLIENT_DATA,	CENTER_SELECTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Group Labels Node",
			MENU_CLIENT_DATA,	TOGGLE_GROUP_LABELLING_OPERATION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		NULL);

	gragra_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:gragra_submenu",
		MENU_TITLE_ITEM,		"Gragra",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Gragra",
		MENU_ITEM,
			MENU_STRING,		"Defaults",
			MENU_CLIENT_DATA,	EDIT_GRAGRA,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Compile",
			MENU_CLIENT_DATA,	COMPILE_ALL_PRODUCTIONS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Set current",
			MENU_CLIENT_DATA,	SET_CURRENT_PRODUCTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Apply current",
			MENU_CLIENT_DATA,	APPLY_CURRENT_PRODUCTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Pretty Print current",
			MENU_CLIENT_DATA,	PRETTY_PRINT_CURRENT_PRODUCTION,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Pretty Print all",
			MENU_CLIENT_DATA,	PRETTY_PRINT_ALL_PRODUCTIONS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"1-ENCE",
			MENU_CLIENT_DATA,	SET_GRAGRA_TYPE_ENCE_1,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"1-NCE",
			MENU_CLIENT_DATA,	SET_GRAGRA_TYPE_NCE_1,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"NLC",
			MENU_CLIENT_DATA,	SET_GRAGRA_TYPE_NLC,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"BNLC",
			MENU_CLIENT_DATA,	SET_GRAGRA_TYPE_BNLC,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		NULL);

	file_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:file_submenu",
		MENU_TITLE_ITEM,		"File",
		MENU_GEN_PIN_WINDOW,		base_frame,	"File",
		MENU_ITEM,
			MENU_STRING,		"Store",
			MENU_CLIENT_DATA,	STORE_BY_SUBFRAME,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"To same File",
			MENU_CLIENT_DATA,	STORE_TO_SAME_FILE,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Load",
			MENU_CLIENT_DATA,	LOAD_BY_SUBFRAME,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"This File again",
			MENU_CLIENT_DATA,	LOAD_AGAIN,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		NULL);

	misc_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:misc_submenu",
		MENU_TITLE_ITEM,		"Misc",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Misc",
		MENU_ITEM,
			MENU_STRING,		"Redraw all",
			MENU_CLIENT_DATA,	REDRAW_ALL,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Constrained",
			MENU_CLIENT_DATA,	TOGGLE_CONSTRAINED,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Grid  8 x 8",
			MENU_CLIENT_DATA,	SET_GRID_8_8,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Grid  16 x 16",
			MENU_CLIENT_DATA,	SET_GRID_16_16,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Grid  32 x 32",
			MENU_CLIENT_DATA,	SET_GRID_32_32,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Grid  64 x 64",
			MENU_CLIENT_DATA,	SET_GRID_64_64,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Grid 128 x 128",
			MENU_CLIENT_DATA,	SET_GRID_128_128,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Grid off",
			MENU_CLIENT_DATA,	SET_GRID_OFF,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Save State",
			MENU_CLIENT_DATA,	SAVE_STATE,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Print",
			MENU_CLIENT_DATA,	PRINT,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Edit Nodetypes",
			MENU_CLIENT_DATA,	EDIT_NODETYPES,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Edit Edgetypes",
			MENU_CLIENT_DATA,	EDIT_EDGETYPES,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Edit Nodefonts",
			MENU_CLIENT_DATA,	EDIT_NODEFONTS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Edit Edgefonts",
			MENU_CLIENT_DATA,	EDIT_EDGEFONTS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Expand Working Area",
			MENU_CLIENT_DATA,	EXPAND_WORKING_AREA,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Shrink Working Area",
			MENU_CLIENT_DATA,	SHRINK_WORKING_AREA,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_EMPTY_SEPARATOR,
		MENU_ITEM,
			MENU_STRING,		"Selection Statistics",
			MENU_CLIENT_DATA,	SELECTION_STATISTICS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Buffer Statistics",
			MENU_CLIENT_DATA,	BUFFER_STATISTICS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Full Statistics",
			MENU_CLIENT_DATA,	ALL_STATISTICS,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		NULL);

	tools_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:tools_submenu",
		MENU_TITLE_ITEM,		"Tools",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Tools",
		NULL);
		
	layout_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:layout_submenu",
		MENU_TITLE_ITEM,		"Layout",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Layout",
		NULL);

	goodies_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:goodies_submenu",
		MENU_TITLE_ITEM,		"Goodies",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Goodies",
		NULL);

	user_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:user_submenu",
		MENU_TITLE_ITEM,		"User",
		MENU_GEN_PIN_WINDOW,		base_frame,	"User",
		NULL);

	about_submenu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:about_submenu",
		MENU_TITLE_ITEM,		"Help",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Help",
		MENU_ITEM,
			MENU_STRING,		"Help & Info",
			MENU_CLIENT_DATA,	ABOUT_GRAPHED,
			MENU_ACTION_PROC,	graphed_menu_action_proc,
			NULL,
		NULL);


	main_menu = (Menu)xv_create(NULL, MENU,
		XV_HELP_DATA,			"graphed:main_menu",
		MENU_TITLE_ITEM,		"Main",
		MENU_GEN_PIN_WINDOW,		base_frame,	"Main",
		MENU_ITEM,
			MENU_STRING,		"Create",
			MENU_CLIENT_DATA,	CREATE_MODE,
			MENU_PULLRIGHT,		create_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Edit",
			MENU_CLIENT_DATA,	SELECT_MODE,
			MENU_PULLRIGHT,		edit_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Gragra",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		gragra_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"File",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		file_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Misc",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		misc_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Tools",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		tools_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Layout",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		layout_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"Goodies",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		goodies_submenu,
			NULL,
		MENU_ITEM,
			MENU_STRING,		"User",
			MENU_CLIENT_DATA,	NO_ACTION,
			MENU_PULLRIGHT,		user_submenu,
			NULL,
		NULL);


	/* Defaults fuer die Submenues einstellen :			*/
	/* Waehlt der Benutzer nur das Item, von dem aus ein Submenue	*/
	/* aus aufgeklappt wird, an, so wird das MENU_DEFAULT_ITEM AUS	*/
	/* DEM SUBMENUE zurueckgegeben. Dieses Item wird hier fuer die	*/
	/* Menues gesetzt, in denen es nicht von current_... (wie z.B.	*/
	/* bei nodelabel_placement) abhaengt; in den anderen Faellen	*/
	/* wird es in set_menue_selection (s.u.) eingestellt.		*/
	
	set_menu_default_item (create_submenu,      CREATE_MODE);
	set_menu_default_item (edit_submenu,        SELECT_MODE);
	set_menu_default_item (gragra_submenu,      EDIT_GRAGRA);
	set_menu_default_item (file_submenu,        STORE_BY_SUBFRAME);
	set_menu_default_item (misc_submenu,        REDRAW_ALL);
	
	set_menu_default_item (graph_submenu,       CREATE_DIRECTED_GRAPH);
	set_menu_default_item (production_submenu,  CREATE_DIRECTED_PRODUCTION);
	set_menu_default_item (embedding_submenu,   CREATE_DIRECTED_EMBEDDING);
	set_menu_default_item (about_submenu,       ABOUT_GRAPHED);

	
	/* Deaktiviere zu Beginn nicht ausfuehrbare Aktionen		*/
		
	inactivate_menu_item (LOAD_AGAIN);
	inactivate_menu_item (STORE_TO_SAME_FILE);
	inactivate_menu_item (EDIT_SELECTION);
	inactivate_menu_item (DELETE_SELECTION);
	inactivate_menu_item (EXPAND_SELECTION);
	inactivate_menu_item (SHRINK_SELECTION);
	inactivate_menu_item (EXPAND_WORKING_AREA);
	inactivate_menu_item (SHRINK_WORKING_AREA);
	inactivate_menu_item (PUT_SELECTION);
	inactivate_menu_item (GET_SELECTION);
	
}
/************************************************************************/
/*									*/
/*			MENUEAUSWAHL VERWALTEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Die folgenden Prozeduren setzen eine Auswahl im Menue ein :	*/
/*									*/
/*	void	install_node_edge_interface_in_menu (nei)		*/
/*	void	install_nodelabel_placement_in_menu (nlp)		*/
/*	void	install_nodesize_in_menu      (x,y)			*/
/*	void	install_edgelabelsize_in_menu (x,y)			*/
/*	void	install_arrowlength_in_menu (length)			*/
/*	void	install_arrowangle_in_menu  (angle)			*/
/*	void	install_nodelabel_visibility_in_menu (visible)		*/
/*	void	install_edgelabel_visibility_in_menu (visible)		*/
/*	void	install_grid_in_menu (width)				*/
/*									*/
/*	Der Umweg ueber diese Prozeduren ist aus folgenden Gruenden	*/
/*	sinnvoll :							*/
/*	- Modularisierung						*/
/*	- Items im Menue werden ueber eine Konstante vom Typ		*/
/*	  User_action angesprochen. Deshalb ist i.a. eine "Umrechnung"	*/
/*	  erforderlich.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	void		set_menu_selection (action)			*/
/*									*/
/*	Markiert den zu action gehoerenden Eintrag; der bisher		*/
/*	markierte Eintrag in dieser Kategorie (-> selected) wird	*/
/*	geloescht.							*/
/*	Diese Prozedur wird auch von den obigen install_... -		*/
/*	Prozeduren verwendet.						*/
/*									*/
/*	selected hat folgende Gestalt :					*/
/*									*/
/*	typedef	struct	{						*/
/*		User_action	main_action;				*/
/*		User_action	node_edge_interface;			*/
/*		User_action	nodelabel_placement;			*/
/*		User_action	nodelabel_visibility;			*/
/*		User_action	all_nodelabel_visibility;		*/
/*		User_action	nodesize_scaling;			*/
/*		User_action	edgelabelsize_scaling;			*/
/*		User_action	edgelabel_visibility;			*/
/*		User_action	all_edgelabel_visibility;		*/
/*		User_action	arrowlength;				*/
/*		User_action	arrowangle;				*/
/*		User_action	gragra_type;				*/
/*		User_action	grid;					*/
/*	}								*/
/*		Selected;						*/
/*									*/
/*	Selected	selected;					*/
/*									*/
/*	Dabei ist jedes Feld eine Kategorie, in der immer nur ein	*/
/*	Punkt markiert werden kann (i.a. gleichbedeutend mit		*/
/*	Submenues).							*/
/*									*/
/************************************************************************/


void			install_node_edge_interface_in_menu (nei)
Node_edge_interface	nei;
{
	switch (nei) {
	    case NO_NODE_EDGE_INTERFACE :
		set_menu_selection (NEI_NO_NODE_EDGE_INTERFACE);
		break;
	    case TO_BORDER_OF_BOUNDING_BOX :
		set_menu_selection (NEI_TO_BORDER_OF_BOUNDING_BOX);
		break;
	    case TO_CORNER_OF_BOUNDING_BOX :
		set_menu_selection (NEI_TO_CORNER_OF_BOUNDING_BOX);
		break;
	    case CLIPPED_TO_MIDDLE_OF_NODE :
		set_menu_selection (NEI_CLIPPED_TO_MIDDLE_OF_NODE);
		break;
	    case SPECIAL_NODE_EDGE_INTERFACE :
		set_menu_selection (NEI_SPECIAL);
		break;
	}
}



void			install_nodelabel_placement_in_menu (nlp)
Nodelabel_placement	nlp;
{
	switch (nlp) {
	    case NODELABEL_MIDDLE :
		set_menu_selection (NLP_MIDDLE);
		break;
	    case NODELABEL_UPPERLEFT :
		set_menu_selection (NLP_UPPERLEFT);
		break;
	    case NODELABEL_UPPERRIGHT :
		set_menu_selection (NLP_UPPERRIGHT);
		break;
	    case NODELABEL_LOWERLEFT :
		set_menu_selection (NLP_LOWERLEFT);
		break;
	    case NODELABEL_LOWERRIGHT :
		set_menu_selection (NLP_LOWERRIGHT);
		break;
	}
}


void	install_nodesize_in_menu (x,y)
int	x,y;
{
	if (x == y) switch (x) {
	     case 16 :
		set_menu_selection (SCALE_NODESIZE_16_16);
		break;
	     case 32 :
		set_menu_selection (SCALE_NODESIZE_32_32);
		break;
	     case 64 :
		set_menu_selection (SCALE_NODESIZE_64_64);
		break;
	     case 128 :
		set_menu_selection (SCALE_NODESIZE_128_128);
		break;
	     default :
		set_menu_selection (SCALE_NODESIZE_AS_SELECTED);
		break;
	} else
		set_menu_selection (SCALE_NODESIZE_AS_SELECTED);
}


void	install_edgelabelsize_in_menu (width,height)
int	width, height;
{
	if (width == 64 && height == 64)
		set_menu_selection (SCALE_EDGELABELSIZE_64_64);
	else if (width == 256 && height == 64)
		set_menu_selection (SCALE_EDGELABELSIZE_256_64);
	else if (width == MAXINT && height == MAXINT)
		set_menu_selection (SCALE_EDGELABELSIZE_UNCONSTRAINED);
}


void	install_arrowlength_in_menu (length)
int	length;
{
	if (length == 8)
		set_menu_selection (SCALE_ARROWLENGTH_8);
	else if (length == 12)
		set_menu_selection (SCALE_ARROWLENGTH_12);
	else if (length == 16)
		set_menu_selection (SCALE_ARROWLENGTH_16);
	else
		set_menu_selection (SCALE_ARROWLENGTH_AS_SELECTED);
}


void	install_arrowangle_in_menu (angle)
float	angle;
{
	if (angle == deg_to_rad (30))
		set_menu_selection (SCALE_ARROWANGLE_30);
	else if (angle == deg_to_rad (45))
		set_menu_selection (SCALE_ARROWANGLE_45);
	else if (angle == deg_to_rad (60))
		set_menu_selection (SCALE_ARROWANGLE_60);
	else
		set_menu_selection (SCALE_ARROWANGLE_AS_SELECTED);
}


void	install_nodelabel_visibility_in_menu (visible)
int	visible;
{
	if (visible)
		set_menu_selection (SET_NODELABEL_VISIBLE);
	else
		set_menu_selection (SET_NODELABEL_INVISIBLE);
}


void	install_edgelabel_visibility_in_menu (visible)
int	visible;
{
	if (visible)
		set_menu_selection (SET_EDGELABEL_VISIBLE);
	else
		set_menu_selection (SET_EDGELABEL_INVISIBLE);
}


void	install_grid_in_menu (width)
int	width;
{
	if (width == 0)
		set_menu_selection (SET_GRID_OFF);
	else if (width == 8)
		set_menu_selection (SET_GRID_8_8);
	else if (width == 16)
		set_menu_selection (SET_GRID_16_16);
	else if (width == 32)
		set_menu_selection (SET_GRID_32_32);
	else if (width == 64)
		set_menu_selection (SET_GRID_64_64);
	else if (width == 128)
		set_menu_selection (SET_GRID_128_128);
}


void	install_constrained_in_menu (constrain)
int	constrain;
{
	if (constrain)
		set_menu_string (TOGGLE_CONSTRAINED, "unconstrained  [F2]");
	else
		set_menu_string (TOGGLE_CONSTRAINED, "constrained    [F2]");
}



void		install_group_labelling_operation_in_menu (goes_to)
Node_or_edge	goes_to;
{
	if (goes_to == NODE)
		set_menu_string (TOGGLE_GROUP_LABELLING_OPERATION,
		                 "group labels edge [F3]");
	else
		set_menu_string (TOGGLE_GROUP_LABELLING_OPERATION,
		                 "group labels node [F3]");
}


void	install_directedness_in_menu (directed)
int	directed;
{
	if (directed) {
		set_menu_selection (CREATE_DIRECTED_GRAPH);
		set_menu_selection (CREATE_DIRECTED_PRODUCTION);
		set_menu_selection (CREATE_DIRECTED_EMBEDDING);
	} else {
		set_menu_selection (CREATE_UNDIRECTED_GRAPH);
		set_menu_selection (CREATE_UNDIRECTED_PRODUCTION);
		set_menu_selection (CREATE_UNDIRECTED_EMBEDDING);
	}
}



void		install_gragra_type_in_menu (gragra_type)
Gragra_type	gragra_type;
{
	switch (gragra_type) {
	    case NCE_1 :
		set_menu_selection (SET_GRAGRA_TYPE_NCE_1);
		break;
	    case ENCE_1 :
		set_menu_selection (SET_GRAGRA_TYPE_ENCE_1);
		break;
	    case NLC :
		set_menu_selection (SET_GRAGRA_TYPE_NLC);
		break;
	    case BNLC :
		set_menu_selection (SET_GRAGRA_TYPE_BNLC);
		break;
	}
}




void		set_menu_selection (action)
User_action	action;
{
	switch (action) {

	    case CREATE_MODE        :
	    case SELECT_MODE      :
		unmark_working_area_menu (selected.main_action);
		selected.main_action = action;
		mark_working_area_menu   (selected.main_action);
		break;

	    case CREATE_DIRECTED_GRAPH :
	    case CREATE_UNDIRECTED_GRAPH :
		set_menu_default_item (graph_submenu, action);
		break;
	    
	    case CREATE_DIRECTED_PRODUCTION :
	    case CREATE_UNDIRECTED_PRODUCTION :
		set_menu_default_item (production_submenu, action);
		break;
	    
	    case CREATE_DIRECTED_EMBEDDING :
	    case CREATE_UNDIRECTED_EMBEDDING :
		set_menu_default_item (embedding_submenu, action);
		break;
	    
	    case NEI_NO_NODE_EDGE_INTERFACE    :
	    case NEI_TO_BORDER_OF_BOUNDING_BOX :
	    case NEI_TO_CORNER_OF_BOUNDING_BOX :
	    case NEI_CLIPPED_TO_MIDDLE_OF_NODE :
	    case NEI_SPECIAL                   :
		unmark_working_area_menu (selected.node_edge_interface);
		selected.node_edge_interface = action;
		mark_working_area_menu   (selected.node_edge_interface);
		set_menu_default_item (node_edge_interface_menu, action);
		break;
	
	    case NLP_MIDDLE     :
	    case NLP_UPPERLEFT  :
	    case NLP_UPPERRIGHT :
	    case NLP_LOWERLEFT  :
	    case NLP_LOWERRIGHT :
	    	unmark_working_area_menu (selected.nodelabel_placement);
		selected.nodelabel_placement = action;
		mark_working_area_menu   (selected.nodelabel_placement);
		set_menu_default_item (nodelabel_placement_menu, action);
		break;
	
	    case SET_NODELABEL_VISIBLE   :
	    case SET_NODELABEL_INVISIBLE :
		unmark_working_area_menu (selected.nodelabel_visibility);
		selected.nodelabel_visibility = action;
		mark_working_area_menu   (selected.nodelabel_visibility);
		set_menu_default_item (nodelabel_visibility_menu, action);
		break;
	
	    case SCALE_NODESIZE_16_16       :
	    case SCALE_NODESIZE_32_32       :
	    case SCALE_NODESIZE_64_64       :
	    case SCALE_NODESIZE_128_128     :
	    case SCALE_NODESIZE_AS_SELECTED :
	    	unmark_working_area_menu (selected.nodesize_scaling);
		selected.nodesize_scaling = action;
		mark_working_area_menu   (selected.nodesize_scaling);
		set_menu_default_item (nodesize_scaling_menu, action);
		break;
	
	    case SET_EDGELABEL_VISIBLE   :
	    case SET_EDGELABEL_INVISIBLE :
		unmark_working_area_menu (selected.edgelabel_visibility);
		selected.edgelabel_visibility = action;
		mark_working_area_menu   (selected.edgelabel_visibility);
		set_menu_default_item (edgelabel_visibility_menu, action);
		break;
	
	    case SCALE_EDGELABELSIZE_64_64         :
	    case SCALE_EDGELABELSIZE_256_64        :
	    case SCALE_EDGELABELSIZE_UNCONSTRAINED :
	    	unmark_working_area_menu (selected.edgelabelsize_scaling);
		selected.edgelabelsize_scaling = action;
		mark_working_area_menu   (selected.edgelabelsize_scaling);
		set_menu_default_item (edgelabelsize_menu, action);
		break;
	
	    case SCALE_ARROWLENGTH_8           :
	    case SCALE_ARROWLENGTH_12          :
	    case SCALE_ARROWLENGTH_16          :
	    case SCALE_ARROWLENGTH_AS_SELECTED :
		unmark_working_area_menu (selected.arrowlength);
		selected.arrowlength = action;
		mark_working_area_menu   (selected.arrowlength);
		set_menu_default_item (arrowlength_menu, action);
		break;
	
	    case SCALE_ARROWANGLE_30          :
	    case SCALE_ARROWANGLE_45          :
	    case SCALE_ARROWANGLE_60          :
	    case SCALE_ARROWANGLE_AS_SELECTED :
		unmark_working_area_menu (selected.arrowangle);
		selected.arrowangle = action;
		mark_working_area_menu   (selected.arrowangle);
		set_menu_default_item (arrowangle_menu, action);
		break;
	
	    case SET_GRAGRA_TYPE_NCE_1       :
	    case SET_GRAGRA_TYPE_ENCE_1      :
	    case SET_GRAGRA_TYPE_NLC         :
	    case SET_GRAGRA_TYPE_BNLC        :
		unmark_working_area_menu (selected.gragra_type);
		selected.gragra_type = action;
		mark_working_area_menu   (selected.gragra_type);
		break;
	
	    case SET_GRID_8_8                :
	    case SET_GRID_16_16              :
	    case SET_GRID_32_32              :
	    case SET_GRID_64_64              :
	    case SET_GRID_128_128            :
	    case SET_GRID_OFF                :
		unmark_working_area_menu (selected.grid);
		selected.grid = action;
		mark_working_area_menu   (selected.grid);
		break;
	
	    case DELETE_ALL                  : /* werden alle nicht markiert */
	    case EDIT_SELECTION              :
	    case CREATE_BUFFER               :
	    case EDIT_GRAPH_OF_SELECTION     :
	    case EDIT_GRAGRA                 :
	    case DELETE_SELECTION            :
	    case PUT_SELECTION               :
	    case GET_SELECTION               :
	    case SET_ALL_NODELABEL_VISIBLE   :
	    case SET_ALL_NODELABEL_INVISIBLE :
	    case SET_ALL_EDGELABEL_VISIBLE   :
	    case SET_ALL_EDGELABEL_INVISIBLE :
	    case REDRAW_ALL                  :
	    case SAVE_STATE                  :
	    case EXPAND_WORKING_AREA         :
	    case SHRINK_WORKING_AREA         :
	    case CREATE_PRODUCTION           :
	    case COMPILE_ALL_PRODUCTIONS     :
	    case SET_CURRENT_PRODUCTION      :
	    case APPLY_CURRENT_PRODUCTION    :
	    case TOGGLE_CONSTRAINED          :
	    case TOGGLE_GROUP_LABELLING_OPERATION :
	    default :
		break;
	}
}
/************************************************************************/
/*									*/
/*			HILFSFUNKTIONEN					*/
/*									*/
/************************************************************************/
/*									*/
/*	Allgemeines :							*/
/*									*/
/*	Die folgenden Prozeduren suchen in Menues nach einen Item mit	*/
/*	MENU_CLIENT_DATA = action. Ist action mehrfach vorhanden (z.B.	*/
/*	in einem Pullright - Item und in einem Item des Submenues), so	*/
/*	wird die erste gefundene markiert !				*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	mark_working_area_menu     (action)		*/
/*	static	void	unmark_working_area_menu   (action)		*/
/*									*/
/*	Markiert / Unmarkiert den mit action verbundenen Eintrag im	*/
/*	main_menu bzw. einer seiner Submenus.				*/
/*	Markiert wird mit dem Attribut MENUE_BOXED.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	activate_menu_item   (action)				*/
/*	void	inactivate_menu_item (action)				*/
/*									*/
/*	Aktiviert / deaktiviert das mit action verbundene Item mittels	*/
/*	MENUE_INACTIVE. Ein inaktives Item kann (und soll) vom Benutzer	*/
/*	nicht angewaehlt werden !					*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	set_menu_default_item (menu, action)		*/
/*									*/
/*	Setze MENU_DEFAULT_ITEM im angegebenen Menue.			*/
/*	(Waehlt der Beutzer ein Pullright - Item an, ohne das		*/
/*	dazugehoerige Submenue aufzuklappen, so wirkt das wie Auswahl	*/
/*	von MENU_DEFAULT_ITEM.)						*/
/*	Im Gegensatz zu den beiden obigen Prozeduren wirkt sich diese	*/
/*	auf ein bestimmtes Menue aus !					*/
/*									*/
/************************************************************************/



static	void	mark_working_area_menu (action)
User_action	action;
{
	Menu_item	menu_item;
	
	if (action != NO_ACTION) {
		menu_item = menu_find (main_menu, MENU_CLIENT_DATA, action, 0);
		if (menu_item != (Menu_item)NULL)
			menu_set (menu_item, 0);
/* UMARBEITUNG noetig MH conversion */
	}
}



static	void	unmark_working_area_menu (action)
User_action	action;
{
	Menu_item	menu_item;

	if (action != NO_ACTION) {
		menu_item = menu_find (main_menu, MENU_CLIENT_DATA, action, 0);
		if (menu_item != (Menu_item)NULL)
			menu_set (menu_item, 0);
	}
}



void		activate_menu_item (action)
User_action	action;
{
	Menu_item	menu_item;
	
	if (action != NO_ACTION) {
		menu_item = menu_find (main_menu, MENU_CLIENT_DATA, action, 0);
		if (menu_item != (Menu_item)NULL)
			menu_set (menu_item, MENU_INACTIVE, FALSE, 0);
	}
}


void		inactivate_menu_item (action)
User_action	action;
{
	Menu_item	menu_item;
	
	if (action != NO_ACTION) {
		menu_item = menu_find (main_menu, MENU_CLIENT_DATA, action, 0);
		if (menu_item != (Menu_item)NULL)
			menu_set (menu_item, MENU_INACTIVE, TRUE, 0);
	}
}



static	void	set_menu_default_item (menu, action)
Menu		menu;
User_action	action;
{
	Menu_item	menu_item;
	
	if (action != NO_ACTION) {
		menu_item = menu_find (menu, MENU_CLIENT_DATA, action, 0);
		if (menu_item != (Menu_item)NULL)
			menu_set (menu, MENU_DEFAULT_ITEM, menu_item, 0);
	}
}


void		set_menu_string (action, string)
User_action	action;
char		*string;
{
	Menu_item	menu_item;
	
	if (action != NO_ACTION) {
		menu_item = menu_find (main_menu, MENU_CLIENT_DATA, action, 0);
		if (menu_item != (Menu_item)NULL)
			menu_set (menu_item, MENU_STRING, string, 0);
	}
}


Pixrect	*menu_create_separator (string)
char	*string;
{
/* Added MH conversion */
	extern	struct	pr_size	pf_textwidth ();
	struct	pr_size	size;
	int	len;
	Pixrect	*pr;
	Pixfont	*font;
	
	font = (Pixfont *)menu_get (main_menu, XV_FONT);

#ifdef XVIEW_COMMENT
     XView CONVERSION - Compatibility attr, use XV_FONT instead  Sect 3.1
#endif
	
	/* Determine the length of string in Pixels */
	size = pf_textwidth (strlen (string), font, string);
	size.x += (int)menu_get (main_menu, XV_RIGHT_MARGIN);
	size.x += (int)menu_get (main_menu, XV_LEFT_MARGIN);
	size.x += 2*(int)menu_get (main_menu, XV_MARGIN);
	size.x += 16; /* guess pullright */
	
	/* Now create a PixRect of the appropriate size */
	pr = mem_create (size.x, size.y, 1);
	
	/* Draw a line */
	pr_vector (pr,  0, size.y/2,  size.x, size.y/2,  PIX_SET, 1);
	
	return pr;
}


/************************************************************************/
/*									*/
/*		Benutzerspezifische Menues Anfuegen			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	add_to_extra_menu (string, proc)			*/
/*	void	add_to_user_menu  (string, proc)			*/
/*									*/
/*	These procedures are used to add additional procedures to	*/
/*	the menue. These procedures are called as menu action procs;	*/
/*	their parameters are therefore a menu and a menu item.		*/
/*	The extra menu is reserved for common utilities, whereas user	*/
/*	is for the user's own algorithms.				*/
/*									*/
/************************************************************************/


void	add_entry_to_menu (menu, string, proc)
Menu	menu;
char	*string;
char 	(*proc)();
{
	if (string[0] == '-') {
		menu_set (menu,
			MENU_SEPARATOR (string),
		0);
	} else {
		menu_set (menu,
			MENU_APPEND_ITEM,
			menu_create_item (
				MENU_STRING,      string,
				MENU_CLIENT_DATA, NO_ACTION,
				MENU_ACTION_PROC, proc,
				0),
			0);
	}
}


void	add_menu_to_menu (menu, string, add_menu)
Menu	menu;
char	*string;
Menu	add_menu;
{
	menu_set (menu, MENU_APPEND_ITEM,
		menu_create_item(
			MENU_STRING,      string,
			MENU_CLIENT_DATA, NO_ACTION,
			MENU_PULLRIGHT,   add_menu,
			0),
	0);

}


/*				*/
/*	Add item to menu	*/
/*				*/

add_to_tools_menu (string, proc)
char	*string;
char 	(*proc)();
{
	add_entry_to_menu (tools_submenu, string, proc);
}


void	add_to_extra_menu (string, proc)
char	*string;
char 	(*proc)();
{
	add_to_tools_menu (string, proc);
}


void	add_to_layout_menu (string, proc)
char	*string;
char 	(*proc)();
{
	add_entry_to_menu (layout_submenu, string, proc);
}


void	add_to_goodies_menu (string, proc)
char	*string;
char 	(*proc)();
{
	add_entry_to_menu (goodies_submenu, string, proc);
}


void	add_to_user_menu (string, proc)
char	*string;
char 	(*proc)();
{
	add_entry_to_menu (user_submenu, string, proc);
}


/*				*/
/*	Add menu to menu	*/
/*				*/


void	add_menu_to_tools_menu (string, menu)
char	*string;
Menu	menu;
{
	add_menu_to_menu (tools_submenu, string, menu);
}


void	add_menu_to_layout_menu (string, menu)
char	*string;
Menu	menu;
{
	add_menu_to_menu (layout_submenu, string, menu);
}


void	add_menu_to_goodies_menu (string, menu)
char	*string;
Menu	menu;
{
	add_menu_to_menu (goodies_submenu, string, menu);
}


void	add_menu_to_user_menu (string, menu)
char	*string;
Menu	menu;
{
	add_menu_to_menu (user_submenu, string, menu);
}
