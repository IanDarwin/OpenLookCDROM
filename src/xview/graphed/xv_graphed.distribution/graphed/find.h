/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	FIND_HEADER
#define	FIND_HEADER

#define PICK_GAP (minimum(DEFAULT_CURSOR_WIDTH, DEFAULT_CURSOR_HEIGHT) / 2)

typedef	enum {
	FIND_FIRST,
        FIND_NEXT
}
        Find_which;

extern	Node	node_finder                ();
extern	Edge	edge_finder                ();
extern	Node	find_node_containing_point ();
extern	Edge	find_edge_near_point       ();

extern	int	pick_gap_x;
extern	int	pick_gap_y;


typedef	enum {
	NODE_PICKED,
	EDGE_PICKED,
	GROUP_PICKED
}
	What_is_picked;


typedef	struct {
	Node	node;
	Edge	edge;
	Group	group;
}
	Which_is_picked;


typedef	struct	picklist {
	What_is_picked	what;
	Which_is_picked	which;
	struct picklist	*pre,
	                *suc;
}
	*Picklist;


#define	picklist_is_single(pl) \
	(((pl) != (Picklist)NULL) && ((pl)->suc == (Picklist)NULL))
#define empty_picklist ((Picklist)NULL)


typedef	enum {
	PICK_NODE,
	PICK_EDGE,
	PICK_NODE_OR_EDGE
}
	Pick_mode;


typedef	struct {
	enum {
		NO_EDGE_POINT_PICKED,
		REAL_POINT_PICKED,
		IMAGINARY_POINT_PICKED
	} what;
	union {
		struct {
			Edgeline	el;
		}
			real_point;
		struct {
			Edgeline	el;
			int		x,y;
		}
			imaginary_point;
	} which;
}
	Picked_point_of_edgeline;

typedef	enum {
		NO_NODE_POINT_PICKED,
		UPPER_LEFT_POINT_PICKED,
		UPPER_RIGHT_POINT_PICKED,
		LOWER_LEFT_POINT_PICKED,
		LOWER_RIGHT_POINT_PICKED
	}
	Picked_point_of_node;


extern	Picklist	new_picklist    ();
extern	Picklist	add_to_picklist ();
extern	void		free_picklist   ();
extern	Picklist	remov         ();
extern	Picklist	remove_left_side_of_productions_from_picklist ();

extern	Picked_point_of_edgeline	find_picked_point_of_edgeline ();
extern	Picked_point_of_node		find_picked_point_of_node     ();

extern	Rect		compute_bounding_rect_of_picklist ();
extern	Picklist	picklist_contains_object ();
extern	void		mark_picked_object       ();
extern	void		unmark_picked_object     ();

extern	Picklist	xpicker ();

#endif
