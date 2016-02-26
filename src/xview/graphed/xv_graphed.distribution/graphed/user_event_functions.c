/* (C) Universitaet Passau 1986-1991 */
#include "misc.h"
#include "graph.h"
#include "group.h"

#include "user_header.h"

#define INCLUDE_SGRAPH
#ifdef INCLUDE_SGRAPH
#include "sgraph/std.h"
#include "sgraph/sgraph.h"
#include "sgraph/slist.h"
#include "sgraph/graphed.h"
#include "graphed_sgraph_interface.h"



User_event_functions	user_event_functions;


static	User_event_functions_result	user_event_null_proc (info, event)
UEV_info info;
Event    *event;
{
	return UEV_CONSUMED;
}


void				set_user_event_func (type, func)
User_event_functions_type	type;
User_event_functions_result	(*func)();
{
  user_event_functions.func[type] = func;
}


void				remove_user_event_func (type, func)
User_event_functions_type	type;
User_event_functions_result	(*func)();
{
  user_event_functions.func[type] = user_event_null_proc;
}


User_event_functions_result	call_user_event_func (type, state, event, args)
User_event_functions_type	type;
User_event_functions_state	state;
Event				*event;
char				*args;
{
	struct	uev_info		info;
	User_event_functions_result	returnvalue;
	
	
	info.state = state;
	info.type  = type;

	if (user_event_functions.func[type] == NULL) {
		return	UEV_CONSUMED;
	}
	
	switch (type) {
	    case UEV_CLICK :
	    case UEV_DOUBLE_CLICK :
		break;
	    case UEV_DRAG_NODE :
		info.details.node = *((Drag_node_info*)args);
		info.do_default_action = ((Drag_node_info*)args)->do_default_action;
		break;
	    case UEV_DRAG_EDGE :
		info.details.edge = *((Drag_edge_info*)args);
		info.do_default_action = ((Drag_edge_info*)args)->do_default_action;
		break;
	    case UEV_DRAG_GROUP :
		info.details.group = *((Drag_group_info*)args);
		info.do_default_action = ((Drag_group_info*)args)->do_default_action;
		break;
	    case UEV_DRAG_BOX :
		info.details.box = *((Drag_group_box_info*)args);
		info.do_default_action = ((Drag_group_box_info*)args)->do_default_action;
		break;
	}

	returnvalue = (user_event_functions.func[type]) (&info, event);
	
	switch (type) {
	    case UEV_CLICK :
	    case UEV_DOUBLE_CLICK :
		break;
	    case UEV_DRAG_NODE :
		((Drag_node_info*)args)->do_default_action = info.do_default_action;
		break;
	    case UEV_DRAG_EDGE :
		((Drag_edge_info*)args)->do_default_action = info.do_default_action;
		break;
	    case UEV_DRAG_GROUP :
		((Drag_group_info*)args)->do_default_action = info.do_default_action;
		break;
	    case UEV_DRAG_BOX :
		((Drag_group_box_info*)args)->do_default_action = info.do_default_action;
		break;
	}

	return	returnvalue;
}


static	void		sgraph_event_null_proc (info, uev_info, event)
Sgraph_proc_info	info;
Sgraph_event_proc_info	uev_info;
Event			*event;
{
}

User_event_functions_result	default_uev_click_func (info, event)
UEV_info			info;
Event				*event;
{
/*	message ("A click\n"); */
}


User_event_functions_result	default_uev_double_click_func (info, event)
UEV_info			info;
Event				*event;
{
/*	message ("A double click\n"); */
}


User_event_functions_result	default_uev_drag_node_func (info, event)
UEV_info			info;
Event				*event;
{
/*
	printf ("UEV_DRAG_NODE\t");
	switch (info->state) {
	    case UEV_START :
		printf ("UEV_START\n");
		break;
	    case UEV_DRAG :
		printf ("UEV_DRAG\n");
		break;
	    case UEV_INTERMEDIATE_STOP :
		printf ("UEV_INTERMEDIATE_STOP\n");
		break;
	    case UEV_FINISH :
		printf ("UEV_FINISH\n");
		break;
	    case UEV_ERROR :
		printf ("UEV_ERROR\n");
		break;
	}
*/	
    
	
	if (info->details.node.node != empty_node) {
		node_set (info->details.node.node,
			NODE_POSITION,
				info->details.node.x,
				info->details.node.y,
			NODE_SIZE,
				info->details.node.sx,
				info->details.node.sy,
			0);
		info->do_default_action = FALSE;
	} else {
		info->do_default_action = TRUE;
	}
    
	call_sgraph_event_proc (sgraph_event_null_proc, info, event);
	
	return UEV_CONSUMED;
}


User_event_functions_result	default_uev_drag_edge_func (info, event)
UEV_info			info;
Event				*event;
{
	static	int	last_x, last_y;
	
/*
	printf ("UEV_DRAG_EDGE\t");
    
	switch (info->state) {
	    case UEV_START :
		printf ("UEV_START\n");
		break;
	    case UEV_DRAG :
		printf ("UEV_DRAG\n");
		break;
	    case UEV_INTERMEDIATE_STOP :
		printf ("UEV_INTERMEDIATE_STOP\n");
		break;
	    case UEV_FINISH :
		printf ("UEV_FINISH\n");
		break;
	    case UEV_ERROR :
		printf ("UEV_ERROR\n");
		break;
	}
*/
	switch (info->details.edge.what) {
	
	    case OLD_EDGE_REAL_POINT :
	
		switch (info->state) {
		    case UEV_START :
			last_x = info->details.edge.x;
			last_y = info->details.edge.y;
			dispatch_user_action (UNSELECT);
			break;
		    case UEV_DRAG :
			edge_set (info->details.edge.which.real_point.edge, MOVE,
				info->details.edge.which.real_point.el,
				info->details.edge.x - last_x,
				info->details.edge.y - last_y,
				0);
			break;
		    case UEV_FINISH :
			edge_set (info->details.edge.which.real_point.edge, MOVE,
				info->details.edge.which.real_point.el,
				info->details.edge.x - last_x,
				info->details.edge.y - last_y,
				0);
			dispatch_user_action (SELECT_EDGE,
				info->details.edge.which.real_point.edge);
			break;
		    case UEV_INTERMEDIATE_STOP :
		    case UEV_ERROR :
			break;
		}
		
		last_x = info->details.edge.x;
		last_y = info->details.edge.y;
		
		info->do_default_action = FALSE;
		break;
		
	    case OLD_EDGE_IMAGINARY_POINT :
	
		switch (info->state) {
		    case UEV_START :
			last_x = info->details.edge.x;
			last_y = info->details.edge.y;
			dispatch_user_action (UNSELECT);
			(void)add_to_edgeline (
				info->details.edge.which.imaginary_point.el,
				info->details.edge.x,
				info->details.edge.y),
			edge_set (info->details.edge.which.imaginary_point.edge,
				EDGE_LINE, info->details.edge.which.imaginary_point.edge->line,
				0);
			break;
		    case UEV_DRAG :
			edge_set (info->details.edge.which.imaginary_point.edge, MOVE,
				info->details.edge.which.imaginary_point.el->suc,
				info->details.edge.x - last_x,
				info->details.edge.y - last_y,
				0);
			break;
		    case UEV_FINISH :
			edge_set (info->details.edge.which.imaginary_point.edge, MOVE,
				info->details.edge.which.imaginary_point.el->suc,
				info->details.edge.x - last_x,
				info->details.edge.y - last_y,
				0);
			dispatch_user_action (SELECT_EDGE,
				info->details.edge.which.imaginary_point.edge);
			break;
		    case UEV_INTERMEDIATE_STOP :
		    case UEV_ERROR :
			break;
		}
		
		last_x = info->details.edge.x;
		last_y = info->details.edge.y;
		
		info->do_default_action = FALSE;
		break;
		
	    default :
		info->do_default_action = TRUE;
	}


	call_sgraph_event_proc (sgraph_event_null_proc, info, event);	
    
	return UEV_CONSUMED;
}


User_event_functions_result	default_uev_drag_group_func (info, event)
UEV_info			info;
Event				*event;
{
	static	int	last_x, last_y;
	static	int	do_default_action = FALSE;
	
/*
	printf ("UEV_DRAG_GROUP");
    
	switch (info->state) {
	    case UEV_START :
		printf ("UEV_START\n");
		break;
	   case UEV_DRAG :
		printf ("UEV_DRAG\n");
		break;
	    case UEV_INTERMEDIATE_STOP :
		printf ("UEV_INTERMEDIATE_STOP\n");
		break;
	    case UEV_FINISH :
		printf ("UEV_FINISH\n");
		break;
	    case UEV_ERROR :
		printf ("UEV_ERROR\n");
		break;
	}
*/

	if (info->details.group.group != empty_group &&
	    (!do_default_action || info->state == UEV_START)) {
		switch (info->state) {
		    case UEV_START :
			if (size_of_group(info->details.group.group) < DRAG_GROUP_TRESHHOLD) {
				last_x = info->details.group.x;
				last_y = info->details.group.y;
				dispatch_user_action (UNSELECT);
				do_default_action = FALSE;
			} else {
				do_default_action = TRUE;
			}
			break;
		    case UEV_DRAG :
		    case UEV_INTERMEDIATE_STOP :
			group_set (info->details.group.group, MOVE,
				info->details.group.x - last_x,
				info->details.group.y - last_y,
				0);
			break;
		    case UEV_FINISH :
			group_set (info->details.group.group, MOVE,
				info->details.group.x - last_x,
				info->details.group.y - last_y,
				0);
			dispatch_user_action (SELECT_GROUP,
				copy_group (info->details.group.group));
			break;
		    case UEV_ERROR :
			break;
		}
		
		last_x = info->details.group.x;
		last_y = info->details.group.y;
		
		info->do_default_action = do_default_action;
		
	} else {
		info->do_default_action = TRUE;
	}
	
	call_sgraph_event_proc (sgraph_event_null_proc, info, event);
	
	return UEV_CONSUMED;
}


User_event_functions_result	default_uev_drag_box_func (info, event)
UEV_info			info;
Event				*event;
{
}

void	init_user_event_functions ()
{
    set_user_event_func (UEV_CLICK,        default_uev_click_func);
    set_user_event_func (UEV_DOUBLE_CLICK, default_uev_double_click_func);
    set_user_event_func (UEV_DRAG_NODE,    default_uev_drag_node_func);
    set_user_event_func (UEV_DRAG_EDGE,    default_uev_drag_edge_func);
    set_user_event_func (UEV_DRAG_GROUP,   default_uev_drag_group_func);
    set_user_event_func (UEV_DRAG_BOX,     default_uev_drag_box_func);
    
}
#endif INCLUDE_SGRAPH
