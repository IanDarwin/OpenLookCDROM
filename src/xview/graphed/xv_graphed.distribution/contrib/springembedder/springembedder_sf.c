/* (C) Universitaet Passau 1986-1991 */
#include <xview/xview.h>
#include <xview/panel.h>

#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>
#include <math.h>
#include "springembedder.h"
#include "graphed/gridder.h"

extern	Frame	base_frame;
extern	char	*int_to_ascii ();


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_springembedder_subframe ()				*/
/*									*/
/************************************************************************/

static	void	create_springembedder_subframe	();
static	void	save_springembedder_settings	();
static		notify_springembedder_buttons ();
static		notify_springembedder_cycles ();

static	Frame		springembedder_subframe;
static	Panel		springembedder_panel;
static	Panel_item	springembedder_set_button,
			springembedder_do_button,
			springembedder_animation_button,
			springembedder_quit_button,
			springembedder_max_force_pi,
			springembedder_period_pi,
			springembedder_max_iter_pi,
			springembedder_anim_interv_pi;
			
static	Gridder		springembedder_opt_distance_gridder;
static	Gridder_choices	springembedder_opt_distance_options = GRIDDER_DISTANCE_2_LARGEST_SIZE;


static	void	create_springembedder_subframe()
{
	int	row_count = 0;
	
	springembedder_subframe = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_LABEL,		"sprimgembedder_sf.c",
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		FRAME_DONE_PROC,	notify_springembedder_buttons,
		NULL);

	springembedder_panel = (Panel)xv_get(springembedder_subframe, FRAME_CMD_PANEL);


	springembedder_set_button = xv_create(springembedder_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Set",
		XV_X,			xv_col(springembedder_panel, 0),
		PANEL_NOTIFY_PROC,	notify_springembedder_buttons,
		NULL);

	springembedder_do_button = xv_create(springembedder_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do",
		XV_X,			xv_col(springembedder_panel, 12),
		PANEL_NOTIFY_PROC,	notify_springembedder_buttons,
		NULL);


	springembedder_animation_button = xv_create(springembedder_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Animation",
		XV_X,			xv_col(springembedder_panel, 24),
		PANEL_NOTIFY_PROC,	notify_springembedder_buttons,
		NULL);


	row_count += 1;
	springembedder_max_force_pi = xv_create(springembedder_panel, PANEL_TEXT,
		XV_X,				xv_col(springembedder_panel, 0),
		XV_Y,				xv_row(springembedder_panel, row_count),
		PANEL_LABEL_STRING,		"Max Force : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			float_to_ascii(springembedder_max_force),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

		
	row_count += 1;
	springembedder_period_pi = xv_create(springembedder_panel, PANEL_TEXT,
		XV_X,				xv_col(springembedder_panel, 0),
		XV_Y,				xv_row(springembedder_panel, row_count),
		PANEL_LABEL_STRING,		"Period : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			float_to_ascii(springembedder_period),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

		
	row_count += 1;
	springembedder_max_iter_pi = xv_create(springembedder_panel, PANEL_TEXT,
		XV_X,				xv_col(springembedder_panel, 0),
		XV_Y,				xv_row(springembedder_panel, row_count),
		PANEL_LABEL_STRING,		"Maximal Interations : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(springembedder_max_iterations),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

		
	row_count += 1;
	springembedder_anim_interv_pi = xv_create(springembedder_panel, PANEL_TEXT,
		XV_X,				xv_col(springembedder_panel, 0),
		XV_Y,				xv_row(springembedder_panel, row_count),
		PANEL_LABEL_STRING,		"Animation Intervals : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(springembedder_animation_intervals),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

	row_count +=1;
	springembedder_opt_distance_gridder = create_gridder(
		springembedder_panel,
		GRIDDER_MAX_OF_BOTH,
		"optimal distance",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);


	window_fit(springembedder_panel);
	window_fit(springembedder_subframe);
}


static	int	showing_springembedder_subframe = FALSE;


void		show_springembedder_subframe ()
{
	if (!showing_springembedder_subframe) {
		create_springembedder_subframe();
		showing_springembedder_subframe = TRUE;
	}
	
	compute_subwindow_position_at_graph_of_current_selection (
		springembedder_subframe);
	gridder_set (springembedder_opt_distance_gridder,
		springembedder_opt_distance_options,
		springembedder_opt_distance);
	
	xv_set(springembedder_subframe, WIN_SHOW, TRUE, NULL);
}


static		notify_springembedder_buttons (item, event)
Panel_item	item;
Event *		event;
{
	save_springembedder_settings ();
	
	if (item == springembedder_do_button) {
		call_sgraph_proc (fast_nature);
	} else if (item == springembedder_animation_button) {
		call_sgraph_proc (nature);
	} else if (item == springembedder_set_button) {
		;
	} else if (item == springembedder_subframe) {
		free (springembedder_opt_distance_gridder);
		springembedder_opt_distance_gridder = NULL;
		xv_destroy_safe(springembedder_subframe);
		showing_springembedder_subframe = FALSE;
	}
}

static	void	save_springembedder_settings ()
{
	if (showing_springembedder_subframe) {
		springembedder_max_force            = atof (xv_get(springembedder_max_force_pi, PANEL_VALUE));
		springembedder_opt_distance         = (float)gridder_get_size(springembedder_opt_distance_gridder);
		springembedder_period               = atof (xv_get(springembedder_period_pi, PANEL_VALUE));
		springembedder_max_iterations       = atoi (xv_get(springembedder_max_iter_pi, PANEL_VALUE));
		springembedder_animation_intervals  = atoi (xv_get(springembedder_anim_interv_pi, PANEL_VALUE));
		springembedder_opt_distance_options = gridder_get_value(springembedder_opt_distance_gridder);
	} else {
		if (springembedder_opt_distance_options != GRIDDER_DISTANCE_OTHER) {
			springembedder_opt_distance = recompute_gridder_size (
				NULL,
				springembedder_opt_distance_options,
				GRIDDER_MAX_OF_BOTH);
		}
	}
}


/*	Callback procedures	*/


char *nature_menu_callback_proc ( menu, menu_item )
     char *menu, *menu_item;
{	
	save_springembedder_settings ();
	
	if (event_ctrl_is_down ((Event*)menu_get (menu, MENU_FIRST_EVENT))) {
		show_springembedder_subframe ();
	} else if (event_meta_is_down ((Event*)menu_get (menu, MENU_FIRST_EVENT))) {
		return call_sgraph_proc (fast_nature);
	} else {
		return call_sgraph_proc (nature);

	}
}
