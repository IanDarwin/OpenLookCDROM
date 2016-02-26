/* (C) Universitaet Passau 1986-1991 */
#include <xview/xview.h>
#include <xview/panel.h>

#include <std.h>
#include <sgraph.h>

extern	Frame	base_frame;
extern	char	*int_to_ascii ();


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_rt_subframe (node_or_edge)				*/
/*									*/
/************************************************************************/

static	void	create_rt_subframe	();
static	void	save_rt_settings	();
static		notify_rt_buttons ();

static	Frame		rt_subframe;
static	Panel		rt_panel;
static	Panel_item	rt_set_button,
			rt_do_button,
			rt_quit_button,
			rt_text1,
			rt_text2,
			rt_text3,
			rt_text4,
			rt_text5;

int	saved_rt_minsep              = 2,
	saved_rt_correction_of_tree  = 0,
	saved_rt_feasible_difference = 3,
	saved_rt_lower_equal         = 0,
	saved_rt_pixel_per_unit      = 3;



static	void	create_rt_subframe()
{
	int	row_count = 0;
	
	rt_subframe = xv_create(base_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	rt_panel = xv_create(rt_subframe, PANEL,
		NULL);


	rt_set_button = xv_create(rt_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Set",
		XV_X,			xv_col(rt_panel, 0),              
		PANEL_NOTIFY_PROC,	notify_rt_buttons,
		NULL);

	rt_do_button = xv_create(rt_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Do",
		XV_X,			xv_col(rt_panel, 10),              
		PANEL_NOTIFY_PROC,	notify_rt_buttons,
		NULL);


	rt_quit_button = xv_create(rt_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Quit",
		XV_X,			xv_col(rt_panel, 20),              
		PANEL_NOTIFY_PROC,	notify_rt_buttons,
/*		PANEL_MENU_CHOICE_STRINGS,	"Leave this subframe", 0,	fis: use MENU_STRING instead*/
		NULL);


	row_count += 2;
	rt_text1 = xv_create(rt_panel, PANEL_TEXT,
		XV_X,				xv_col(rt_panel, 0),
		XV_Y,				xv_row(rt_panel, row_count),
		PANEL_LABEL_STRING,		"minsep : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(saved_rt_minsep),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);


	row_count += 1;
	rt_text2 = xv_create(rt_panel, PANEL_TEXT,
		XV_X,				xv_col(rt_panel, 0),
		XV_Y,				xv_row(rt_panel, row_count),
		PANEL_LABEL_STRING,		"correction of tree : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(saved_rt_correction_of_tree),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

		
	row_count += 1;
	rt_text3 = xv_create(rt_panel, PANEL_TEXT,
		XV_X,				xv_col(rt_panel, 0),
		XV_Y,				xv_row(rt_panel, row_count),
		PANEL_LABEL_STRING,		"maximum feasible difference : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(saved_rt_feasible_difference),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

		
	row_count += 1;
	rt_text4 = xv_create(rt_panel, PANEL_TEXT,
		XV_X,				xv_col(rt_panel, 0),
		XV_Y,				xv_row(rt_panel, row_count),
		PANEL_LABEL_STRING,		"lower equal : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(saved_rt_lower_equal),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);

		
	row_count += 1;
	rt_text5 = xv_create(rt_panel, PANEL_TEXT,
		XV_X,				xv_col(rt_panel, 0),
		XV_Y,				xv_row(rt_panel, row_count),
		PANEL_LABEL_STRING,		"Pixel per unit : ",
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(saved_rt_pixel_per_unit),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		NULL);


	window_fit(rt_panel);
	window_fit(rt_subframe);
}


static	int	showing_rt_subframe = FALSE;


void		show_rt_subframe ()
{
	if (!showing_rt_subframe) {
		create_rt_subframe();
		showing_rt_subframe = TRUE;
	}
	
	xv_set(rt_subframe, WIN_SHOW, TRUE, NULL);
}


static		notify_rt_buttons (item, event)
Panel_item	item;
Event *		event;
{
	save_rt_settings ();
	
	if (item == rt_do_button) {
	} else if (item == rt_quit_button) {
		xv_destroy_safe(rt_subframe);
		showing_rt_subframe = FALSE;
	}
}


static	void	save_rt_settings ()
{
	saved_rt_minsep              = atoi (xv_get(rt_text1, PANEL_VALUE));
	saved_rt_correction_of_tree  = atoi (xv_get(rt_text2, PANEL_VALUE));
	saved_rt_feasible_difference = atoi (xv_get(rt_text3, PANEL_VALUE));
	saved_rt_lower_equal         = atoi (xv_get(rt_text4, PANEL_VALUE));
	saved_rt_pixel_per_unit      = atoi (xv_get(rt_text5, PANEL_VALUE));
}
