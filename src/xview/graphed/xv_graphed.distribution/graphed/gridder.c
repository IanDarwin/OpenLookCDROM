/* (C) Universitaet Passau 1986-1991 */
#include "user_header.h"
#include "gridder.h"

extern	Frame	base_frame;
extern	char	*int_to_ascii ();


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Gridder		create_gridder (panel, kind, label,		*/
/*			                value, size, row_count)		*/
/*	Gridder_choices	gridder_get_value (gridder)			*/
/*	int		gridder_get_size (gridder)			*/
/*	void		gridder_set      (gridder, value, size)		*/
/*									*/
/************************************************************************/

Gridder		create_gridder    ();
Gridder_choices	gridder_get_value ();
int		gridder_get_size  ();
void		gridder_set       ();

static		notify_gridder_cycles ();



static	char	*gridder_choice_strings[] = {
	"none", 
	"1 * default / grid size", 
	"1 * largest node", 
	"1.5 * default / grid size", 
	"1.5 * largest node", 
	"2 * default / grid size", 
	"2 * largest node", 
	"3 * default / grid size", 
	"3 * largest node", 
	"other", 
};



Gridder			create_gridder (panel, kind, label,
			                value, size, row_count)
Panel			panel;
Gridder_width_or_height	kind;
char			*label;
Gridder_choices		value;
int			size;
int			row_count;
{
	Panel_item	gridder_cycle, gridder_text;
	Gridder		gridder;
		
	gridder_cycle = xv_create(panel, PANEL_CYCLE,
		XV_X,			xv_col(panel, 0),
		XV_Y,			xv_row(panel, row_count),
		PANEL_LABEL_STRING,	label,
		PANEL_LABEL_BOLD,	TRUE,
		PANEL_VALUE,		value,
		PANEL_CHOICES_BOLD,	TRUE,
		PANEL_CHOICE_STRINGS,	gridder_choice_strings[GRIDDER_DISTANCE_NONE],
					gridder_choice_strings[GRIDDER_DISTANCE_1_DEFAULT_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_1_LARGEST_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_15_DEFAULT_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_15_LARGEST_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_2_DEFAULT_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_2_LARGEST_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_3_DEFAULT_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_3_LARGEST_SIZE],
					gridder_choice_strings[GRIDDER_DISTANCE_OTHER],
					NULL,
		PANEL_NOTIFY_PROC,	notify_gridder_cycles,
		NULL);
		
	row_count += 1;
	gridder_text = xv_create(panel, PANEL_TEXT,
		XV_X,				xv_col(panel, 0),
		XV_Y,				xv_row(panel, row_count),
		PANEL_LABEL_STRING,		label,
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_VALUE,			int_to_ascii(size),
		PANEL_VALUE_STORED_LENGTH,	10,
		PANEL_VALUE_DISPLAY_LENGTH,	10,
		XV_SHOW,			FALSE,
		NULL);
		
	
	switch (value) {
	    case GRIDDER_DISTANCE_NONE :
	    case GRIDDER_DISTANCE_1_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_1_LARGEST_SIZE :
	    case GRIDDER_DISTANCE_15_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_15_LARGEST_SIZE :
	    case GRIDDER_DISTANCE_2_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_2_LARGEST_SIZE :
	    case GRIDDER_DISTANCE_3_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_3_LARGEST_SIZE :
		xv_set(gridder_text, XV_SHOW, FALSE, NULL);
		break;
	    case GRIDDER_DISTANCE_OTHER :
		xv_set(gridder_text, XV_SHOW, TRUE, NULL);
		break;
	}

	gridder	= (Gridder)mymalloc (sizeof(struct gridder));
	gridder->text  = gridder_text;
	gridder->cycle = gridder_cycle;
	gridder->kind  = kind;
	
	xv_set(gridder_text,  PANEL_CLIENT_DATA, gridder, NULL);
	xv_set(gridder_cycle, PANEL_CLIENT_DATA, gridder, NULL);
	
	return	gridder;
}


static		notify_gridder_cycles (item, value, event)
Panel_item	item;
int		value;
Event *		event;
{
	Gridder	gridder = (Gridder)xv_get(item, PANEL_CLIENT_DATA);
	
	gridder_set (gridder, value, atoi(xv_get(gridder->text, PANEL_VALUE)));
}


void		gridder_set (gridder, value, size)
Gridder		gridder;
Gridder_choices	value;
int		size;
{
	xv_set(gridder->cycle, PANEL_VALUE, value, NULL);
	
	switch (value) {
	    case GRIDDER_DISTANCE_NONE :
	    case GRIDDER_DISTANCE_1_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_1_LARGEST_SIZE :
	    case GRIDDER_DISTANCE_15_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_15_LARGEST_SIZE :
	    case GRIDDER_DISTANCE_2_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_2_LARGEST_SIZE :
	    case GRIDDER_DISTANCE_3_DEFAULT_SIZE :
	    case GRIDDER_DISTANCE_3_LARGEST_SIZE :
		xv_set(gridder->text,
			XV_SHOW,	FALSE,
			PANEL_VALUE,	int_to_ascii (recompute_gridder_size (gridder)),
			NULL);
		break;
	    case GRIDDER_DISTANCE_OTHER :
		xv_set(gridder->text,
			XV_SHOW,	TRUE,
			PANEL_VALUE,	int_to_ascii (size),
			NULL);
		break;
	}
}


int			recompute_gridder_size (gridder, choice, kind)
Gridder			gridder;
Gridder_choices		choice;
Gridder_width_or_height	kind;
{
	int	largest_node_width  = 0;
	int	largest_node_height = 0;

	if (gridder != NULL) {
		choice = (Gridder_choices)xv_get(gridder->cycle, PANEL_VALUE);
		kind   = gridder->kind;
	}
	
	switch (choice) {
	    case GRIDDER_DISTANCE_NONE :
		largest_node_width  = 0;
		largest_node_height = 0;
		break;
	    case GRIDDER_DISTANCE_1_DEFAULT_SIZE :
		if (get_gridwidth (wac_buffer) == 0) {
			largest_node_width  = 1 * get_current_node_width();
			largest_node_height = 1 * get_current_node_height();
		} else {
			largest_node_width  = 1 * get_gridwidth (wac_buffer);
			largest_node_height = 1 * get_gridwidth (wac_buffer);
		}
		break;
	    case GRIDDER_DISTANCE_1_LARGEST_SIZE :
		compute_largest_node_sizes_in_graph (
			get_picked_or_only_existent_graph(),
			&largest_node_width,
                        &largest_node_height);
		largest_node_width  = largest_node_width * 1;
		largest_node_height = largest_node_height * 1;
		break;
	    case GRIDDER_DISTANCE_15_DEFAULT_SIZE :
		if (get_gridwidth (wac_buffer) == 0) {
			largest_node_width  = 1.5 * get_current_node_width();
			largest_node_height = 1.5 * get_current_node_height();
		} else {
			largest_node_width  = 1.5 * get_gridwidth (wac_buffer);
			largest_node_height = 1.5 * get_gridwidth (wac_buffer);
		}
		break;
	    case GRIDDER_DISTANCE_15_LARGEST_SIZE :
		compute_largest_node_sizes_in_graph (
			get_picked_or_only_existent_graph(),
			&largest_node_width,
                        &largest_node_height);
		largest_node_width  = largest_node_width * 1.5;
		largest_node_height = largest_node_height * 1.5;
		break;
	    case GRIDDER_DISTANCE_2_DEFAULT_SIZE :
		if (get_gridwidth (wac_buffer) == 0) {
			largest_node_width  = 2 * get_current_node_width();
			largest_node_height = 2 * get_current_node_height();
		} else {
			largest_node_width  = 2 * get_gridwidth (wac_buffer);
			largest_node_height = 2 * get_gridwidth (wac_buffer);
		}
		break;
	    case GRIDDER_DISTANCE_2_LARGEST_SIZE :
		compute_largest_node_sizes_in_graph (
			get_picked_or_only_existent_graph(),
			&largest_node_width,
                        &largest_node_height);
		largest_node_width  = largest_node_width * 2;
		largest_node_height = largest_node_height * 2;
		break;
	    case GRIDDER_DISTANCE_3_DEFAULT_SIZE :
		if (get_gridwidth (wac_buffer) == 0) {
			largest_node_width  = 3 * get_current_node_width();
			largest_node_height = 3 * get_current_node_height();
		} else {
			largest_node_width  = 3 * get_gridwidth (wac_buffer);
			largest_node_height = 3 * get_gridwidth (wac_buffer);
		}
		break;
	    case GRIDDER_DISTANCE_3_LARGEST_SIZE :
		compute_largest_node_sizes_in_graph (
			get_picked_or_only_existent_graph(),
			&largest_node_width,
                        &largest_node_height);
		largest_node_width  = largest_node_width * 3;
		largest_node_height = largest_node_height * 3;
		break;
	    case GRIDDER_DISTANCE_OTHER :
		if (gridder != (Gridder)NULL) {
			largest_node_width  = atoi(xv_get(gridder->text, PANEL_VALUE));
			largest_node_height = atoi(xv_get(gridder->text, PANEL_VALUE));
		} else {
			compute_largest_node_sizes_in_graph (
				get_picked_or_only_existent_graph(),
				&largest_node_width,
				&largest_node_height);
			largest_node_width  = largest_node_width * 2;
			largest_node_height = largest_node_height * 2;
		}
		break;
	}
	
	switch (kind) {
	    case GRIDDER_WIDTH :
		return largest_node_width;
		break;
	    case GRIDDER_HEIGHT :
		return largest_node_height;
		break;
	    case GRIDDER_MAX_OF_BOTH :
		return maximum (largest_node_width, largest_node_height);
		break;
	    case GRIDDER_MIN_OF_BOTH :
		return minimum (largest_node_width, largest_node_height);
		break;
	}
}


int	gridder_get_size (gridder)
Gridder	gridder;
{
	return recompute_gridder_size (gridder);
}


Gridder_choices	gridder_get_value (gridder)
Gridder		gridder;
{
	return (Gridder_choices)xv_get(gridder->cycle, PANEL_VALUE);
}



int	compute_largest_node_sizes_in_selection (largest_node_width,
                                                 largest_node_height)
int	*largest_node_width, *largest_node_height;
{
	Picklist	selection;
	Group		g;
	
	*largest_node_width  = 0;
	*largest_node_height = 0;
	selection = get_picked_object ();
	
	if (selection != empty_picklist) switch (selection->what) {
	    case NODE_PICKED :
		*largest_node_width  = node_width(selection->which.node);
		*largest_node_height = node_height(selection->which.node);
		break;
	    case EDGE_PICKED :
		*largest_node_width  = get_current_node_width();
		*largest_node_height = get_current_node_height();
		break;
	    case GROUP_PICKED :
		for_group (selection->which.group, g) {
			*largest_node_width  = maximum (*largest_node_width,
			                                 node_width (g->node));
			*largest_node_height = maximum (*largest_node_height,
			                                 node_height (g->node));
		} end_for_group (selection->which.group, g);
		break;
	}
}


int	compute_largest_node_sizes_in_graph (graph,largest_node_width,
                                                   largest_node_height)
Graph	graph;
int	*largest_node_width, *largest_node_height;
{
	Node	n;
	
	*largest_node_width = 0;
	*largest_node_height = 0;
	
	if (graph != empty_graph) for_nodes (graph, n) {
		*largest_node_width  = maximum (
			*largest_node_width, node_width (n));
		*largest_node_height = maximum (
			*largest_node_height, node_height (n));
	} end_for_nodes (graph, n);
}
