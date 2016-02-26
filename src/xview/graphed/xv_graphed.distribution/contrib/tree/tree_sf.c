/* (C) Universitaet Passau 1986-1991 */
#include <xview/xview.h>
#include <xview/panel.h>

#include <std.h>
#include <sgraph.h>
#include <algorithms.h>
#include "tree.h"
#include "graphed/gridder.h"

extern	Frame	base_frame;
extern	char	*int_to_ascii ();


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_tree_subframe ()				*/
/*									*/
/************************************************************************/

static	void	create_tree_subframe ();
static		notify_tree_buttons  ();

static	Frame		tree_subframe;
static	Panel		tree_panel;
static	Panel_item	tree_set_button,
			tree_do_button,
			tree_quit_button;
static	Gridder		vertical_gridder,
			subtreeseparation_gridder,
			siblingseparation_gridder;

Tree_settings tree_settings = {
	64,	/* vertical distance   */
	64,	/* siblingseparation distance */
	64,	/* subtreeseparation distance */
	GRIDDER_DISTANCE_1_DEFAULT_SIZE,
	GRIDDER_DISTANCE_1_DEFAULT_SIZE,
	GRIDDER_DISTANCE_15_DEFAULT_SIZE
};


static	void	create_tree_subframe()
{
	int	row_count = 0;
	
	tree_subframe = (Frame)xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	if (tree_subframe == (Frame)NULL) {
		bell ();
		return;
	}
	
	tree_panel = (Panel)xv_create(tree_subframe, PANEL, NULL);

	if (tree_panel == (Panel)NULL) {
		bell ();
		tree_subframe = NULL;
		return;
	}
	


	tree_set_button = xv_create(tree_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Set",
		XV_X,              	xv_col(tree_panel, 0),
		PANEL_NOTIFY_PROC,	notify_tree_buttons,
/*		PANEL_MENU_CHOICE_STRINGS, "Set values", 0,	fis: use MENU_STRING instead*/
		NULL);

	tree_do_button = xv_create(tree_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do",
		XV_X,              	xv_col(tree_panel, 10),
		PANEL_NOTIFY_PROC,	notify_tree_buttons,
/*		PANEL_MENU_CHOICE_STRINGS, "Set values and execute algorithm", 0,	fis: use MENU_STRING instead*/
		NULL);

	tree_quit_button = xv_create(tree_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Quit",
		XV_X,              	xv_col(tree_panel, 20),
/*		PANEL_MENU_CHOICE_STRINGS, "Leave this subframe", 0,	fis: use MENU_STRING instead*/
		PANEL_NOTIFY_PROC,	notify_tree_buttons,
		NULL);


	row_count += 2;
	vertical_gridder = create_gridder (tree_panel,
		GRIDDER_HEIGHT,
		"vertical separation",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);
		
	row_count += 2;
	siblingseparation_gridder = create_gridder (tree_panel,
		GRIDDER_WIDTH,
		"sibling separation",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);
		
	row_count += 2;
	subtreeseparation_gridder = create_gridder (tree_panel,
		GRIDDER_WIDTH,
		"subtree separation",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);
		

	window_fit(tree_panel);
	window_fit(tree_subframe);
}


static	int	showing_tree_subframe = FALSE;


void		show_tree_subframe ()
{
	if (!showing_tree_subframe) {
		create_tree_subframe();
	}
	
	if (tree_subframe != (Frame)NULL) {
	
		gridder_set (vertical_gridder,
			tree_settings.size_defaults_y,
			tree_settings.vertical_separation);
		gridder_set (siblingseparation_gridder,
			tree_settings.size_defaults_x_sibling,
			tree_settings.siblingseparation);
		gridder_set (subtreeseparation_gridder,
			tree_settings.size_defaults_x_subtree,
			tree_settings.subtreeseparation);
		
		compute_subwindow_position_at_graph_of_current_selection (
			tree_subframe);
		xv_set (tree_subframe, WIN_SHOW, TRUE, 0);
		
		showing_tree_subframe = TRUE;
		
	} else {
		showing_tree_subframe = FALSE;
	}
}


static		notify_tree_buttons (item, event)
Panel_item	item;
Event *		event;
{
	save_tree_settings ();
	
	if (item == tree_do_button) {
		call_sgraph_proc (tree_layout_walker);
	} else if (item == tree_set_button) {
		;
	} else if (item == tree_quit_button) {
		free (siblingseparation_gridder); siblingseparation_gridder = (Gridder)NULL;
		free (subtreeseparation_gridder); subtreeseparation_gridder = (Gridder)NULL;
		free (vertical_gridder);          vertical_gridder = (Gridder)NULL;
		xv_destroy_safe(tree_subframe);
		showing_tree_subframe = FALSE;
	}
}

void	save_tree_settings ()
{
	if (showing_tree_subframe) {
		tree_settings.vertical_separation   = gridder_get_size (vertical_gridder);
		tree_settings.siblingseparation = gridder_get_size (siblingseparation_gridder);
		tree_settings.subtreeseparation = gridder_get_size (subtreeseparation_gridder);
		tree_settings.size_defaults_y = (int)gridder_get_value (vertical_gridder);
		tree_settings.size_defaults_x_sibling = (int)gridder_get_value (siblingseparation_gridder);
		tree_settings.size_defaults_x_subtree = (int)gridder_get_value (subtreeseparation_gridder);
	} else {
		if (tree_settings.size_defaults_y != GRIDDER_DISTANCE_OTHER) {
			tree_settings.vertical_separation = recompute_gridder_size (
				NULL,
				tree_settings.size_defaults_y,
				GRIDDER_HEIGHT);
		}
		if (tree_settings.size_defaults_x_sibling != GRIDDER_DISTANCE_OTHER) {
			tree_settings.siblingseparation = recompute_gridder_size (
				NULL,
				tree_settings.size_defaults_x_sibling,
				GRIDDER_WIDTH);
		}
		if (tree_settings.size_defaults_x_subtree != GRIDDER_DISTANCE_OTHER) {
			tree_settings.subtreeseparation = recompute_gridder_size (
				NULL,
				tree_settings.size_defaults_x_subtree,
				GRIDDER_WIDTH);
		}
	}
}


char *tree_layout_walker_menu_callback_proc (menu, menu_item)
char *menu, *menu_item;
{ 
	Event	*event;
	
	save_tree_settings ();
	
	event = (Event*)menu_get (menu, MENU_FIRST_EVENT);
	if (event_ctrl_is_down (event)) {
		show_tree_subframe ();
	} else {
		call_sgraph_proc (tree_layout_walker);
	}
	
	return(0);
}
