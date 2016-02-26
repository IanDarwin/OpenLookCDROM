/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1991 by Michael Himsolt */

#ifndef SGRAPH_INTERFACE_H
#define SGRAPH_INTERFACE_H

extern	char		*menu_call_sgraph_proc ();
extern	char		*call_sgraph_proc ();
extern	char		*call_sgraph_event_proc ();


typedef	enum	{
	SGRAPH_SELECTED_NONE,
	SGRAPH_SELECTED_NOTHING,
	SGRAPH_SELECTED_SNODE,
	SGRAPH_SELECTED_SEDGE,
	SGRAPH_SELECTED_GROUP,
	
	SGRAPH_SELECTED_SAME
}
	Sgraph_selected;


typedef	union {
	Snode	snode;
	Sedge	sedge;
	Slist	group;
}
	Sgraph_selection;


typedef struct sgraph_proc_info {
	
	Sgraph_selected  selected;
	Sgraph           sgraph;
	Sgraph_selection selection;
	int              buffer;
	
	Sgraph_selected  new_selected;
	Sgraph           new_sgraph;
	Sgraph_selection new_selection;
	int              new_buffer;
	
	int	repaint,
		recompute,
		no_changes,
		no_structure_changes,
		save_selection;
	
	int	recenter;
}
	*Sgraph_proc_info;



/************** Extended Application Interface **************************/


typedef	struct	{

	enum {
		NEW_SEDGE,
		OLD_SEDGE_REAL_POINT,
		OLD_SEDGE_IMAGINARY_POINT,
	}
		what;
		
	union {
		struct {
			Snode		source;
			Edgeline	el;
		}
			new_edge;
		struct {
			Sedge		edge;
			Edgeline	el;
		}
			real_point;
		struct {
			Sedge		edge;
			Edgeline	el;
		}
			imaginary_point;
	}
		which;
		
	int	x,y;
}
	Sgraph_drag_edge_info;


typedef	struct	{
	enum {
		MOVE_SNODE,
		SCALE_SNODE_MIDDLE,
		SCALE_SNODE_UPPER_LEFT,
		SCALE_SNODE_UPPER_RIGHT,
		SCALE_SNODE_LOWER_LEFT,
		SCALE_SNODE_LOWER_RIGHT
	}
		what;
		
	Snode	node;
	
	int	x,y, sx,sy;	/* Platzierung und Groesse	*/
	
	int	correction_x,	/* Korrekturen, da Klickpunkt	*/
		correction_y;	/* nicht immer der Knoten-	*/
				/* mittelpunkt ist.		*/
}
	Sgraph_drag_node_info;



typedef	struct	{
	Slist	group;	/* DIE Gruppe			*/
	int	x ,y,	/* Position von group->node	*/
		x0,y0;	/* Ursprungsposition		*/
	int	correction_x,	/* siehe Drag_node_info	*/
		correction_y;
}
	Sgraph_drag_group_info;


typedef	struct	{
	int	x1,y1,	/* Linke obere Ecke		*/
		x2,y2;	/* Rechte untere Ecke		*/
	int	shift_is_down;
}
	Sgraph_drag_box_info;


typedef	struct	{
	int              x,y;		/* Rechte untere Ecke	*/
	Sgraph_selected  what;
	Sgraph_selection which;
}
	Sgraph_click_info;


typedef	enum	{
	SGRAPH_UEV_START,
	SGRAPH_UEV_DRAG,
	SGRAPH_UEV_INTERMEDIATE_STOP,
	SGRAPH_UEV_FINISH,
	SGRAPH_UEV_ERROR
}
	Sgraph_uev_state;

typedef	enum {
	SGRAPH_UEV_CLICK,
	SGRAPH_UEV_DOUBLE_CLICK,
	SGRAPH_UEV_DRAG_NODE,
	SGRAPH_UEV_DRAG_EDGE,
	SGRAPH_UEV_DRAG_GROUP,
	SGRAPH_UEV_DRAG_BOX,
	NUMBER_OF_SGRAPH_UEV_FUNCTIONS
}
	Sgraph_uev_type;

typedef	struct	sgraph_event_proc_info {
	
	Sgraph_uev_state	state;
	Sgraph_uev_type		type;

	union {
		Sgraph_click_info	click;
		Sgraph_drag_node_info	node;
		Sgraph_drag_edge_info	edge;
		Sgraph_drag_group_info	group;
		Sgraph_drag_box_info	box;
	}
		details;
	
	int	do_default_action;
}
	*Sgraph_event_proc_info;


typedef	struct	sgraph_command_proc_info {
	
	User_action	action;
	
	union {
		Sgraph	sgraph;
		Snode	snode;
		Sedge	sedge;
	}
		data;
	
}
	*Sgraph_command_proc_info;



#endif
