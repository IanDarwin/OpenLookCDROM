/* (C) Universitaet Passau 1986-1991 */
typedef	enum {
	GRIDDER_DISTANCE_NONE,
	GRIDDER_DISTANCE_1_DEFAULT_SIZE,
	GRIDDER_DISTANCE_1_LARGEST_SIZE,
	GRIDDER_DISTANCE_15_DEFAULT_SIZE,
	GRIDDER_DISTANCE_15_LARGEST_SIZE,
	GRIDDER_DISTANCE_2_DEFAULT_SIZE,
	GRIDDER_DISTANCE_2_LARGEST_SIZE,
	GRIDDER_DISTANCE_3_DEFAULT_SIZE,
	GRIDDER_DISTANCE_3_LARGEST_SIZE,
	GRIDDER_DISTANCE_OTHER,
	NUMBER_OF_GRIDDER_DISTANCES
}
	Gridder_choices;
	
typedef	enum {
	GRIDDER_WIDTH, GRIDDER_HEIGHT, GRIDDER_MAX_OF_BOTH, GRIDDER_MIN_OF_BOTH
}
	Gridder_width_or_height;


typedef	struct	gridder	{
	Panel_item		cycle, text;
	Gridder_width_or_height	kind;
}
	*Gridder;


Gridder		create_gridder    ();
Gridder_choices	gridder_get_value ();
int		gridder_get_size  ();
void		gridder_set       ();

