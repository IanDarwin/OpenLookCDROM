/* (C) Universitaet Passau 1986-1991 */
#include <xview/xview.h>
#include <xview/panel.h>

#include <std.h>
#include <sgraph.h>
#include <algorithms.h>
#include "def.h"
#include "graphed/gridder.h"

extern	Frame	base_frame;
extern	char	*int_to_ascii ();


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_sugiyama_subframe ()				*/
/*									*/
/************************************************************************/

static	void	create_sugiyama_subframe ();
static		notify_sugiyama_buttons  ();

static	Frame		sugiyama_subframe;
static	Panel		sugiyama_panel;
static	Panel_item	sugiyama_set_button,
			sugiyama_do_button,
			sugiyama_quit_button,
			sugiyama_it1_pi,
			sugiyama_it2_pi;
static	Gridder		vertical_gridder,
			horizontal_gridder;

Sugiyama_settings sugiyama_settings = {
	64,	/* vertical distance   */
	64,	/* horizontal distance */
	10,	/* it1 */
	3,	/* it2 */
	GRIDDER_DISTANCE_2_LARGEST_SIZE,
	GRIDDER_DISTANCE_2_LARGEST_SIZE
};


static	void	create_sugiyama_subframe()
{
	int	row_count = 0;
	
	sugiyama_subframe = (Frame)xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	if (sugiyama_subframe == (Frame)NULL) {
		bell ();
		return;
	}
	
	sugiyama_panel = (Panel)xv_create(sugiyama_subframe, PANEL, NULL);
	if (sugiyama_panel == (Panel)NULL) {
		bell ();
		sugiyama_subframe = NULL;
		return;
	}
	


	sugiyama_set_button = xv_create(sugiyama_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Set",
		XV_X,			xv_col(sugiyama_panel, 0),
		PANEL_NOTIFY_PROC,	notify_sugiyama_buttons,
/*		PANEL_MENU_CHOICE_STRINGS,	"Set values", 0,	fis: use MENU_STRING instead */
		NULL);

	sugiyama_do_button = xv_create(sugiyama_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do",
		XV_X,			xv_col(sugiyama_panel, 10),
		PANEL_NOTIFY_PROC,	notify_sugiyama_buttons,
/*		PANEL_MENU_CHOICE_STRINGS,	"Set values and execute algorithm", 0,	fis: use MENU_STRING instead */
		NULL);


	sugiyama_quit_button = xv_create(sugiyama_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Quit",
		XV_X,			xv_col(sugiyama_panel, 20),
		PANEL_NOTIFY_PROC,	notify_sugiyama_buttons,
/*		PANEL_MENU_CHOICE_STRINGS,	"Leave this subframe", 0,	fis: use MENU_STRING instead */
		NULL);


	row_count += 2;
	vertical_gridder = create_gridder (sugiyama_panel,
		GRIDDER_HEIGHT,
		"vertical distance",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);
		
	row_count += 2;
	horizontal_gridder = create_gridder (sugiyama_panel,
		GRIDDER_WIDTH,
		"horizontal distance",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);



	row_count += 2;
	sugiyama_it1_pi = xv_create(sugiyama_panel, PANEL_TEXT,
		XV_X,				xv_col(sugiyama_panel, 0),
		XV_Y,				xv_row(sugiyama_panel, row_count),
		PANEL_LABEL_STRING,		"iterations 1 : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(sugiyama_settings.it1),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

	row_count += 1;
	sugiyama_it2_pi = xv_create(sugiyama_panel, PANEL_TEXT,
		XV_X,				xv_col(sugiyama_panel, 0),
		XV_Y,				xv_row(sugiyama_panel, row_count),
		PANEL_LABEL_STRING,		"iterations 2 : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(sugiyama_settings.it2),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);


	window_fit(sugiyama_panel);
	window_fit(sugiyama_subframe);
}


static	int	showing_sugiyama_subframe = FALSE;


void		show_sugiyama_subframe ()
{
	if (!showing_sugiyama_subframe) {
		create_sugiyama_subframe();
	}
	
	if (sugiyama_subframe != (Frame)NULL) {
	
		xv_set(sugiyama_it1_pi, PANEL_VALUE, int_to_ascii(sugiyama_settings.it1));
		xv_set(sugiyama_it2_pi, PANEL_VALUE, int_to_ascii(sugiyama_settings.it2));
		gridder_set (vertical_gridder,
			sugiyama_settings.size_defaults_y,
			sugiyama_settings.vertical_distance);
		gridder_set (horizontal_gridder,
			sugiyama_settings.size_defaults_x,
			sugiyama_settings.horizontal_distance);
		
		compute_subwindow_position_at_graph_of_current_selection (
			sugiyama_subframe);
		xv_set (sugiyama_subframe, WIN_SHOW, TRUE, NULL);
		
		showing_sugiyama_subframe = TRUE;
		
	} else {
		showing_sugiyama_subframe = FALSE;
	}
}


static		notify_sugiyama_buttons (item, event)
Panel_item	item;
Event *		event;
{
	extern	char	*sugiyama_layout ();
	
	save_sugiyama_settings ();
	
	if (item == sugiyama_do_button) {
		call_sgraph_proc (sugiyama_layout);
	} else if (item == sugiyama_set_button) {
		;
	} else if (item == sugiyama_quit_button) {
		free (horizontal_gridder); horizontal_gridder = (Gridder)NULL;
		free (vertical_gridder);   vertical_gridder = (Gridder)NULL;
		xv_destroy_safe(sugiyama_subframe);
		showing_sugiyama_subframe = FALSE;
	}
}

void	save_sugiyama_settings ()
{
	if (showing_sugiyama_subframe) {
		sugiyama_settings.vertical_distance   = gridder_get_size (vertical_gridder);
		sugiyama_settings.horizontal_distance = gridder_get_size (horizontal_gridder);
		sugiyama_settings.size_defaults_y = (int)gridder_get_value (vertical_gridder);
		sugiyama_settings.size_defaults_x = (int)gridder_get_value (horizontal_gridder);
		sugiyama_settings.it1 = atoi (xv_get(sugiyama_it1_pi, PANEL_VALUE));
		sugiyama_settings.it2 = atoi (xv_get(sugiyama_it2_pi, PANEL_VALUE));
	} else {
		if (sugiyama_settings.size_defaults_y != GRIDDER_DISTANCE_OTHER) {
			sugiyama_settings.vertical_distance = recompute_gridder_size (
				NULL,
				sugiyama_settings.size_defaults_y,
				GRIDDER_HEIGHT);
		}
		if (sugiyama_settings.size_defaults_y != GRIDDER_DISTANCE_OTHER) {
			sugiyama_settings.horizontal_distance = recompute_gridder_size (
				NULL,
				sugiyama_settings.size_defaults_x,
				GRIDDER_WIDTH);
		}
	}
	if (sugiyama_settings.vertical_distance == 0)
		sugiyama_settings.vertical_distance = get_current_node_height ();
	if (sugiyama_settings.horizontal_distance == 0)
		sugiyama_settings.horizontal_distance = get_current_node_width ();

}
