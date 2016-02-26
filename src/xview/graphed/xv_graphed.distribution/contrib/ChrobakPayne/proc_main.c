/* (C) Universitaet Passau 1986-1991 */
/********************************************************************
 **                                                                **
 **     In dieser Datei wird die Benutzerschnittstelle aufge-      **
 **     baut.                                                      **
 **     Die Hauptprozeduren fuer die einzelnen Menuepunkte sind    **
 **     hier enthalten.                                            **
 **                                                                **
 ********************************************************************/



#include "decl.h"
#include "graphed/gridder.h"



/********************************************************************
 **                                                                **
 **     Das Benutzerfenster:                                       **
 **                                                                **
 ********************************************************************/
 
extern	Frame	base_frame;

static	Frame		cp_frame;
static	Panel		cp_panel;
static	Panel_item	quit_button,
			run_chrobak_payne_button,
			chrobak_payne_cond_stretching_toggle,
			run_assila_button,
			assila_cond_stretching_toggle, 
			compression_button,
			compression_iterations_toggle,
			stepwise_compression_button,
			stepwise_compression_toggle,
			compression_animation_button,
			compression_remove_added_edges_button,
			compression_hide_show_added__edges_button;
static	Gridder		gridder;
static	int		showing_cp_window = FALSE;

Chrobak_payne_settings chrobak_payne_settings = {
	64,				 /* Grid */
	GRIDDER_DISTANCE_2_LARGEST_SIZE
};


static	void	main_chrobak_payne			();
static	void	main_chrobak_payne_cond_stretching	();
static	void	main_asslia				();
static	void	main_asslia_cond_stretching		();
static	void	main_compression_y			();
static	void	main_compression_x			();
static	void	main_zoom				();
static	void	main_compression			();
static	void	main_compression_without_iterations	();
static	void	main_animation				();
static	void	main_hide_show_added_edges		();
static	void	main_remove_added_edges			();
static	void	main_compression_stepwise		();

static	void	save_chrobak_payne_settings ();

static	void	cp_notify_buttons ();
static	void	cp_notify_toggles ();

Sgraph sgraph;


int cp_window ()

{
	int	row_count = 0;
	
	cp_frame = (Frame)xv_create(base_frame, FRAME,
		XV_LABEL,		"Compressions on grid:",
		FRAME_SHOW_LABEL,	TRUE,
		WIN_SHOW,		TRUE,
		FRAME_NO_CONFIRM,	TRUE,
		NULL);

	if (cp_frame == (Frame)NULL)
		return FALSE;

	cp_panel = (Panel)xv_create(cp_frame, PANEL,
		WIN_SHOW,		TRUE,
		NULL);
	
	if (cp_panel == (Frame)NULL)
		return FALSE;

#define	COL1 0
#define COL2 25

	quit_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, 40),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"quit",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL);

	row_count += 2;
	run_chrobak_payne_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, COL1),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"Chrobak-Payne",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL);

	chrobak_payne_cond_stretching_toggle = xv_create(cp_panel, PANEL_TOGGLE,
		XV_X,			xv_col(cp_panel, COL2),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_CHOICE_STRINGS,	"conditional stretching", NULL,
		NULL);

	row_count += 2;
	run_assila_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, COL1),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"Nejia Assila Idea",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL);

	assila_cond_stretching_toggle = xv_create(cp_panel, PANEL_TOGGLE,
		XV_X,			xv_col(cp_panel, COL2),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_CHOICE_STRINGS,	"conditional stretching", NULL,
		NULL);

	row_count += 2;
	compression_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, COL1),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"compression",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL);

	compression_iterations_toggle = xv_create(cp_panel, PANEL_TOGGLE,
		XV_X,			xv_col(cp_panel, COL2),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_CHOICE_STRINGS,	"iterations", NULL,
		NULL);

	row_count += 2;
	stepwise_compression_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, COL1),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"compression stepwise",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL); 

	stepwise_compression_toggle = xv_create(cp_panel, PANEL_TOGGLE,
		XV_X,			xv_col(cp_panel, COL2),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_CHOICE_STRINGS,	"x", "y", NULL,
		NULL);

	row_count += 2;
	compression_animation_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, COL1),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"animation",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL);

	row_count += 2;
	compression_hide_show_added__edges_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, COL1),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"hide/show added edges",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL);

	compression_remove_added_edges_button = xv_create(cp_panel, PANEL_BUTTON,
		XV_X,			xv_col(cp_panel, COL2),
		XV_Y,			xv_row(cp_panel, row_count),
		PANEL_LABEL_STRING,	"remove added edges",
		PANEL_NOTIFY_PROC,	cp_notify_buttons,
		NULL);

	row_count += 2;
	gridder = create_gridder (cp_panel,
		GRIDDER_HEIGHT,
		"grid",
		GRIDDER_DISTANCE_2_DEFAULT_SIZE,
		64,
		row_count);

	gridder_set (gridder,
		chrobak_payne_settings.grid_defaults,
		chrobak_payne_settings.grid);


	window_fit (cp_panel);
	window_fit (cp_frame);

	compute_subwindow_position_at_graph_of_current_selection (
		cp_frame);

	showing_cp_window = TRUE;
	return TRUE;

} /* cp_window */



/* Notifiers rewitten by MH 6/10/91 */

static	int	chrobak_payne_prechecks_result;

static	void		chrobak_payne_do_prechecks (info)
Sgraph_proc_info	info;
{
	Snode	node;
	int	n = 0;
	
	if (info->sgraph == NULL) {
		chrobak_payne_prechecks_result = TRUE;
	} else if (test_sgraph_biconnected (info->sgraph) == FALSE) {
		warning ("graph is not biconnected\n");
		chrobak_payne_prechecks_result = FALSE;
	} else {
	
		(void) test_graph_is_drawn_planar (graphed_graph (info->sgraph));
		(void) test_find_non_straight_line_edge (graphed_graph(info->sgraph));
		
		for_all_nodes (info->sgraph, node) {
			n++;
		} end_for_all_nodes (info->sgraph, node);
		
		if (n > 3) {
			chrobak_payne_prechecks_result = TRUE; /* Give it a try */
		} else {
			error ("The graph must contain at least three nodes\n");
			chrobak_payne_prechecks_result = FALSE;
		}
	}
}


static	void	cp_notify_buttons (item, event)
Panel_item item;
Event *event;
{

#define	toggle_bit_on(value,bit)	(((unsigned int)value) & (1 << (bit)))
#define	toggle_bit_off(value,bit)	(!(toggle_bit_on(value,bit)))

	save_chrobak_payne_settings ();

	if (item == run_chrobak_payne_button                  ||
	    item == run_assila_button                         ||
	    item == compression_button                        ||
	    item == stepwise_compression_button               ||
	    item == compression_hide_show_added__edges_button ||
	    item == compression_remove_added_edges_button) {
	
		call_sgraph_proc (chrobak_payne_do_prechecks);
		if (chrobak_payne_prechecks_result == FALSE) {
			error ("Cannot do Chrobak-Payne algorithm\n");
			return;
		}
	
	}
	if (item == run_chrobak_payne_button) {
		if (toggle_bit_off (xv_get(chrobak_payne_cond_stretching_toggle, PANEL_VALUE), 1)) {
			call_sgraph_proc (main_chrobak_payne);
		} else {
			call_sgraph_proc (main_chrobak_payne_cond_stretching);
		}
	} else if (item == run_assila_button) {
		if (toggle_bit_off (xv_get(assila_cond_stretching_toggle, PANEL_VALUE), 1)) {
			call_sgraph_proc (main_asslia);
		} else {
			call_sgraph_proc (main_asslia_cond_stretching);
		}
	} else if (item == compression_button) {
		if (toggle_bit_on (xv_get(compression_iterations_toggle, PANEL_VALUE), 1)) {
			call_sgraph_proc (main_compression);
		} else {
			call_sgraph_proc (main_compression_without_iterations);
		}
	} else if (item == stepwise_compression_button) {
		if (toggle_bit_on (xv_get(stepwise_compression_toggle, PANEL_VALUE), 1)) {
			call_sgraph_proc (main_compression_x); /* x-direction */
		} else if (toggle_bit_on (xv_get(stepwise_compression_toggle, PANEL_VALUE), 2)) {
			call_sgraph_proc (main_compression_y); /* y-direction */
		} else {
			call_sgraph_proc (main_compression_stepwise); /* none or both */
		}
	} else if (item == compression_animation_button) {
		call_sgraph_proc (main_animation);
	} else if (item == quit_button) {
		free (gridder); gridder = (Gridder)NULL;
		xv_destroy_safe(cp_frame);
		showing_cp_window = FALSE;
	} else if (item == compression_hide_show_added__edges_button) {
		call_sgraph_proc (main_hide_show_added_edges);
	} else if (item == compression_remove_added_edges_button) {
		call_sgraph_proc (main_remove_added_edges);
	}
}



char *cp_window_callback_proc (menu, menu_item)
char *menu, *menu_item;
{
	if (showing_cp_window) {
		;
	} else if (cp_window () == FALSE) {
		warning ("No more windows available\n");
		bell ();
	}
	
	return NULL;
}

static	void	save_chrobak_payne_settings ()
{
	if (showing_cp_window) {
		chrobak_payne_settings.grid = gridder_get_size (gridder);
		chrobak_payne_settings.grid_defaults = (int)gridder_get_value (gridder);
	} else {
		if (chrobak_payne_settings.grid_defaults != GRIDDER_DISTANCE_OTHER) {
			chrobak_payne_settings.grid = recompute_gridder_size (
				NULL,
				chrobak_payne_settings.grid_defaults,
				GRIDDER_HEIGHT);
		}
	}
}



/********************************************************************
 **                                                                **
 **     Die Hauptprozeduren:                                       **
 **                                                                **
 ********************************************************************/



static void main_chrobak_payne (info)
			/* Menuepunkt "Chrobak-Payne". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
	
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	chrobak_payne_algorithm (1);
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};

	if (plan == FALSE) {
		return;
	};
		
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_chrobak_payne */




static void main_chrobak_payne_cond_stretching (info)
			/* Erster Menuepunkt "with conditional stretching". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	chrobak_payne_algorithm (2);
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;

} /* main_chrobak_payne_cond_stretching */




static void main_asslia (info)
			/* Menuepunkt "Nejia Assila Idea". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	chrobak_payne_algorithm (3);
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;

} /* main_asslia */




static void main_asslia_cond_stretching (info)
			/* Zweiter Menuepunkt "with conditional stretching". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	
    if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	

	chrobak_payne_algorithm (4);
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_asslia_cond_stretching */




static void main_compression_y (info)
			/* Menuepunkt "in y-direction". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	int change = TRUE;
	no_graph = FALSE;

	
    if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	no_graph = TRUE;  /* Kom. */

	do {
		store_last_graph ();
		compress (sgraph, 1);
	} while (test_changes () == TRUE);
	
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_compression_y */




static void main_compression_x (info)
			/* Menuepunkt "in x-direction". */
Sgraph_proc_info	info;
{
	Sgraph	sgraph = info->sgraph;
	int i = 1;
	int change = TRUE;
	no_graph = FALSE;

	
	
    if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	no_graph = TRUE;  /* Kom. */
		
	change_coordinates ();
	
	do {
		store_last_graph ();
		compress (sgraph, 1);
	} while (test_changes () == TRUE);
	
	change_coordinates ();
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_compression_x */




static void main_zoom (info)
			/* Menuepunkt "zoom, double size". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	
    if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		zoom (sgraph);
	}
	else {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_zoom */




static void main_compression (info)
			/* Menuepunkt "compression with iterations". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	int chx = TRUE, chy = TRUE;
	int change = TRUE, counter;
	no_graph = FALSE;

	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
	
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	no_graph = TRUE;  /* Kom. */
		
	do {
		counter = 0;
		
		do {
			store_last_graph ();
			compress (sgraph, 0);
		} while (chy = (test_changes () == TRUE));
		
		change_coordinates ();
		
		do {
			counter++;
			store_last_graph ();
			compress (sgraph, 0);
		} while (chx = (test_changes () == TRUE));
		
		change_coordinates ();
	} while (counter > 1);
	
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_compression */




static void main_compression_without_iterations (info)
			/* Menuepunkt "compression without iterations". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	int chx = TRUE, chy = TRUE;
	int change = TRUE, counter;
	no_graph = FALSE;

	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	no_graph = TRUE;   /* Kom. */
		
	do {
		store_last_graph ();
		compress (sgraph, 0);
		chy = (test_changes ());
		change_coordinates ();
		store_last_graph ();
		compress (sgraph, 0);
		chx = (test_changes ());
		change_coordinates ();
	} while ((chy == TRUE) || (chx == TRUE));
	
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_compression_without_iterations */




static void main_animation (info)
			/* Menuepunkt "animation". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	int chx = TRUE, chy = TRUE;
	int change = TRUE, counter;
	no_graph = FALSE;

	
		
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
	
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	no_graph = TRUE;   /* Kom. */
	
	do {
		counter = 0;
		
		do {
			store_last_graph ();
			compress (sgraph, 1);
		} while (chy = (test_changes () == TRUE));
		
		change_coordinates ();
		
		do {
			counter++;
			store_last_graph ();
			compress (sgraph, 1);
		} while (chx = (test_changes () == TRUE));
		
		change_coordinates ();
	} while (counter > 1);
	
	message ("Compresssion completed.\n");
	
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_animation */




static void main_hide_show_added_edges (info)
			/* Menuepunkt "hide/show added edges". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		color_switch (sgraph);
	}
	else {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	info->repaint  = TRUE;
	
} /* main_hide_show_added_edges */




static void main_remove_added_edges (info)
			/* Menuepunkt "hide/show added edges". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		color_remove (sgraph);
	}
	else {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_hide_show_added_edges */




static void main_compression_stepwise (info)
			/* Menuepunkt "compression stepwise". */
Sgraph_proc_info	info;

{
	Sgraph	sgraph = info->sgraph;
	int chx = TRUE, chy = TRUE;
	int change = TRUE, counter;
	no_graph = FALSE;

	
	if ((info != NULL) && (info->sgraph != NULL) && (info->sgraph->nodes != NULL)) {
		take_graph (sgraph);
	}
	else {
		return;
	};
		
	if (plan == FALSE) {
		return;
	};
	
	triangulation (sgraph);
			
	if (plan == FALSE) {
		return;
	};
	
	no_graph = TRUE;   /* Kom. */
		
	do {
		store_last_graph ();
		compress (sgraph, 2);
		chy = test_changes ();
		change_coordinates ();
		store_last_graph ();
		compress (sgraph, 2);
		chx = test_changes ();
		change_coordinates ();
	} while ((chy == TRUE) || (chx == TRUE));
	
	make_the_graph (sgraph);

	if (plan == FALSE) {
		return;
	};
	
	info->new_sgraph = info->sgraph;
	info->new_selected = SGRAPH_SELECTED_SAME;
	info->recenter = TRUE;
	
} /* main_compression_stepwise */
