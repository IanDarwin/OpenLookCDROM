/* (C) Universitaet Passau 1986-1991 */
#include <xview/xview.h>
#include <xview/panel.h>

#include <std.h>
#include <sgraph.h>
#include <algorithms.h>
#include "graphed/gridder.h"
#include "DrawConvex.h"

extern	Frame	base_frame;
extern	void	DrawGraphConvexEditable();
extern	void	DrawGraphConvexStructur();

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_convex_draw_subframe ()				*/
/*									*/
/************************************************************************/

static	void	create_convex_draw_subframe ();
static		notify_convex_draw_buttons  ();

static	Frame		convex_draw_subframe;
static	Panel		convex_draw_panel;
static	Panel_item	convex_draw_set_button,
			convex_draw_do_button,
			convex_draw_quit_button,
			convex_draw_editable_toggle;
static	Gridder		gridder;

Convex_draw_settings convex_draw_settings = {
	64,	/* grid   */
	GRIDDER_DISTANCE_2_LARGEST_SIZE,
	TRUE,	/* editable */
};


static	void	create_convex_draw_subframe()
{
	int	row_count = 0;
	
	convex_draw_subframe = (Frame)xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	if (convex_draw_subframe == (Frame)NULL) {
		bell ();
		return;
	}
	
	convex_draw_panel = (Panel)xv_create(convex_draw_subframe, PANEL, NULL);
	if (convex_draw_panel == (Panel)NULL) {
		bell ();
		convex_draw_subframe = NULL;
		return;
	}
	


	convex_draw_set_button = xv_create(convex_draw_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Set",
		XV_X,			xv_col(convex_draw_panel, 0),
		PANEL_NOTIFY_PROC,	notify_convex_draw_buttons,
/*		PANEL_MENU_CHOICE_STRINGS, "Set values", 0,	fis: use MENU_STRING instead*/
		NULL);

	convex_draw_do_button = xv_create(convex_draw_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do",
		XV_X,			xv_col(convex_draw_panel, 10),
		PANEL_NOTIFY_PROC,	notify_convex_draw_buttons,
/*		PANEL_MENU_CHOICE_STRINGS, "Set values and execute algorithm", 0,	fis: use MENU_STRING instead*/
		NULL);


	convex_draw_quit_button = xv_create(convex_draw_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Quit",
		XV_X,			xv_col(convex_draw_panel, 20),
		PANEL_NOTIFY_PROC,	notify_convex_draw_buttons,
/*		PANEL_MENU_CHOICE_STRINGS, "Leave this subframe", 0,	fis: use MENU_STRING instead*/
		NULL);


	row_count += 2;
	convex_draw_editable_toggle = xv_create(convex_draw_panel, PANEL_TOGGLE,
		PANEL_CHOICE_STRINGS,	"preserve node sizes", NULL,
		XV_X,			xv_col(convex_draw_panel, 00),
		XV_Y,			xv_row(convex_draw_panel, row_count),
		NULL);

	row_count += 2;
	gridder = create_gridder (convex_draw_panel,
		GRIDDER_HEIGHT,
		"grid",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);


	window_fit(convex_draw_panel);
	window_fit(convex_draw_subframe);
}


static	int	showing_convex_draw_subframe = FALSE;


void		show_convex_draw_subframe ()
{
	if (!showing_convex_draw_subframe) {
		create_convex_draw_subframe();
	}
	
	if (convex_draw_subframe != (Frame)NULL) {
	
		gridder_set (gridder,
			convex_draw_settings.grid_defaults,
			convex_draw_settings.grid);
		xv_set(convex_draw_editable_toggle, PANEL_VALUE, (unsigned int)convex_draw_settings.editable, NULL);
			
		compute_subwindow_position_at_graph_of_current_selection (
			convex_draw_subframe);
		xv_set(convex_draw_subframe, WIN_SHOW, TRUE, NULL);
		
		showing_convex_draw_subframe = TRUE;
		
	} else {
		showing_convex_draw_subframe = FALSE;
	}
}


static		notify_convex_draw_buttons (item, event)
Panel_item	item;
Event *		event;
{
	extern	char	*convex_draw_layout ();
	
	save_convex_draw_settings ();
	
	if (item == convex_draw_do_button) {
		if (convex_draw_settings.editable) {
			call_sgraph_proc (DrawGraphConvexEditable);
		} else {
			call_sgraph_proc (DrawGraphConvexStructur);
		}
	} else if (item == convex_draw_set_button) {
		;
	} else if (item == convex_draw_quit_button) {
		free (gridder);   gridder = (Gridder)NULL;
		xv_destroy_safe(convex_draw_subframe);
		showing_convex_draw_subframe = FALSE;
	}
}

void	save_convex_draw_settings ()
{
	if (showing_convex_draw_subframe) {
		convex_draw_settings.grid   = gridder_get_size (gridder);
		convex_draw_settings.grid_defaults = (int)gridder_get_value (gridder);
		convex_draw_settings.editable = (int)xv_get(convex_draw_editable_toggle, PANEL_VALUE);
	} else {
		if (convex_draw_settings.grid_defaults != GRIDDER_DISTANCE_OTHER) {
			convex_draw_settings.grid = recompute_gridder_size (
				NULL,
				convex_draw_settings.grid_defaults,
				GRIDDER_HEIGHT);
		}
	}
}


char	*convex_draw_menu_callback_proc(menu,menu_item)
char	*menu, *menu_item;
{
	Event	*event;
	
	save_convex_draw_settings ();
	
	event = (Event*)menu_get (menu, MENU_FIRST_EVENT);
	if (event_ctrl_is_down (event)) {
		show_convex_draw_subframe ();
	} else {
		if (convex_draw_settings.editable) {
			call_sgraph_proc (DrawGraphConvexEditable);
		} else {
			call_sgraph_proc (DrawGraphConvexStructur);
		}
	}
	
	return(0);
}

