/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************************

	File :  window.c
	Date :  29.05.90

	

*****************************************************************************/


/*	includes	*****************************************************/
#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>
#include "listen_macros.h" /* fuer basic.h ? */
#include "basic.h"

#include <xview/xview.h>
#include <xview/panel.h>
#include "graphed/gridder.h"

extern	void	woods_draw();

extern	Frame		base_frame;
static	Xv_Window	flags_frame,panel;
static	Panel_item	option_item;
static	Gridder		vertical_gridder,
			horizontal_gridder;
static	int		WindowOpen = FALSE;

Woods_settings woods_settings = {
	TRUE,	/* LargeFace		*/
	TRUE,	/* Horizontal		*/
	GRIDDER_DISTANCE_2_LARGEST_SIZE,	/* size defaults x	*/
	GRIDDER_DISTANCE_2_LARGEST_SIZE,	/* size defaults y	*/
	64,	/* vertical distance	*/
	64	/* horizontal distance	*/
};

void	save_woods_settings ();


done_proc()
{
	save_woods_settings ();
	call_sgraph_proc(woods_draw);
}

set_proc()
{
	save_woods_settings ();
}

quit_proc()
{
	save_woods_settings ();
	free (vertical_gridder);
	free (horizontal_gridder);
	
	xv_destroy_safe(flags_frame);
	WindowOpen = FALSE;
}

void hallihallo(item, value, event)
Panel_item	item;
int		value;
Event		*event;
{
	printf("Meine kleine notifiy-proc");
}

void show_woods_subframe()
{
	int	row_count = 0;
	
	
	if(WindowOpen)
		return;
		
	flags_frame = (Frame)xv_create(base_frame, FRAME_CMD,
		FRAME_CMD_PUSHPIN_IN,	TRUE,
		XV_LABEL,		"Woods Drawing",
		FRAME_SHOW_LABEL,	TRUE,
		FRAME_DONE_PROC,	quit_proc,
		NULL);

	panel = (Panel)xv_get(flags_frame, FRAME_CMD_PANEL);

	(void)xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_X,		xv_col(panel, 0),
		PANEL_LABEL_Y,		xv_row(panel, row_count),	
		PANEL_LABEL_STRING,	"Set",
		PANEL_NOTIFY_PROC, 	done_proc,
		NULL);

	(void)xv_create(panel, PANEL_BUTTON,
		PANEL_LABEL_X,		xv_col(panel, 16),
		PANEL_LABEL_Y,		xv_row(panel, row_count),	
		PANEL_LABEL_STRING,	"Do",
		PANEL_NOTIFY_PROC, 	done_proc,
		NULL);

	row_count += 1;
	option_item = xv_create(panel, PANEL_CHECK_BOX,
		PANEL_LABEL_X,		xv_col(panel, 0),
		PANEL_LABEL_Y,		xv_row(panel, row_count),	
		PANEL_LAYOUT,		PANEL_VERTICAL,
		PANEL_CHOICE_STRINGS,	"largest face on the outer border",
					"Horizontal Edges",
					NULL,
		PANEL_VALUE,		woods_settings.LargeFace | woods_settings.Horizontal<<1,
		NULL);
#define toggle_bit_on(value,bit)  ((value)&(1<<(bit)))

	row_count += 2;
	vertical_gridder = create_gridder (panel,
		GRIDDER_HEIGHT,
		"vertical distance",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);
		
	row_count += 2;
	horizontal_gridder = create_gridder (panel,
		GRIDDER_WIDTH,
		"horizontal distance",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);
		
	gridder_set (vertical_gridder,
		woods_settings.size_defaults_y,
		woods_settings.vertical_distance);
	gridder_set (horizontal_gridder,
		woods_settings.size_defaults_x,
		woods_settings.horizontal_distance);

	window_fit(panel);
	window_fit(flags_frame);

	compute_subwindow_position_at_graph_of_current_selection (flags_frame);

	xv_set(flags_frame, WIN_SHOW, TRUE, NULL);
	WindowOpen = TRUE;
}

void	save_woods_settings ()
{
	if (WindowOpen) {
		woods_settings.vertical_distance   = gridder_get_size (vertical_gridder);
		woods_settings.horizontal_distance = gridder_get_size (horizontal_gridder);
		woods_settings.size_defaults_y = (int)gridder_get_value (vertical_gridder);
		woods_settings.size_defaults_x = (int)gridder_get_value (horizontal_gridder);
		woods_settings.LargeFace =
			toggle_bit_on((unsigned int)xv_get(option_item, PANEL_VALUE), 0);
		woods_settings.Horizontal =
			toggle_bit_on((unsigned int)xv_get(option_item, PANEL_VALUE), 1);
	
	} else {
		if (woods_settings.size_defaults_y != GRIDDER_DISTANCE_OTHER) {
			woods_settings.vertical_distance = recompute_gridder_size (
				NULL,
				woods_settings.size_defaults_y,
				GRIDDER_HEIGHT);
		}
		if (woods_settings.size_defaults_x != GRIDDER_DISTANCE_OTHER) {
			woods_settings.horizontal_distance = recompute_gridder_size (
				NULL,
				woods_settings.size_defaults_x,
				GRIDDER_WIDTH);
		}
	}
}

