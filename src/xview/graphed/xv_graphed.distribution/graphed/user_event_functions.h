/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source (C) Universitaet Passau 1986-1991 */

typedef	enum	{
	UEV_CONSUMED,
/*	UEV_NOT_CONSUMED, */
	UEV_VETO
}
	User_event_functions_result;	
	

typedef	enum	{
	UEV_CLICK,
	UEV_DOUBLE_CLICK,
	UEV_DRAG_NODE,
	UEV_DRAG_EDGE,
	UEV_DRAG_GROUP,
	UEV_DRAG_BOX,
	NUMBER_OF_UEV_FUNCTIONS
}
	User_event_functions_type;	


typedef	enum	{
		UEV_START,
		UEV_DRAG,
		UEV_INTERMEDIATE_STOP,
		UEV_FINISH,
		UEV_ERROR
}
	User_event_functions_state;	


typedef	User_event_functions_result (*User_event_function)();
	
typedef	struct	user_event_functions {
	User_event_function func [NUMBER_OF_UEV_FUNCTIONS];
}
	User_event_functions;




typedef	struct	uev_info {
	User_event_functions_state	state;
	User_event_functions_type	type;
	
	union {
		Drag_node_info		node;
		Drag_edge_info		edge;
		Drag_group_info		group;
		Drag_group_box_info	box;
	}
		details;
	
	int	do_default_action;
}
	*UEV_info;


extern	User_event_functions		user_event_functions;

extern	void				init_user_event_functions ();
extern	void				set_user_event_func       ();
extern	User_event_functions_result	call_user_event_func      ();

