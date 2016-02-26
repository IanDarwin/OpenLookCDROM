/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	USER_HEADER_HEADER
#define	USER_HEADER_HEADER

#include "misc.h"
#include "graph.h"
#include "group.h"
#include "load.h"
#include "store.h"
#include "draw.h"
#include "adjust.h"
#include "find.h"
#include "type.h"


#include <xview/scrollbar.h> /* SCROLL_ENTER etc. */

#include "graphed_subwindows.h"
#include "graphed_mpr.h"
#include "menu.h"
#include "user.h"
#include <ctype.h>


typedef	enum {
	EVP_STARTUP,
	EVP_SHUTDOWN,
	EVP_CONSUME
	}
	Message_to_event_proc;

typedef	enum {
	EVP_CONSUMED,
	EVP_FINISHED,	/* Consumed + action is finished		*/
	EVP_ERROR,
	EVP_OK,		/* Initialisation / shutdown o.k.		*/
	EVP_VETO	/* Initialisation / shutdown failed		*/
	}
	Evp_result;


typedef	struct	{

	enum {
		NEW_EDGE,
		OLD_EDGE_REAL_POINT,
		OLD_EDGE_IMAGINARY_POINT,
	}
		what;
		
	union {
		struct {
			Node		source;
			Edgeline	el;
		}
			new_edge;
		struct {
			Edge		edge;
			Edgeline	el;
		}
			real_point;
		struct {
			Edge		edge;
			Edgeline	el;
		}
			imaginary_point;
	}
		which;
		
	int	x,y;

	int	do_default_action;
}
	Drag_edge_info;


typedef	struct	{
	enum {
		MOVE_NODE,
		SCALE_NODE_MIDDLE,
		SCALE_NODE_UPPER_LEFT,
		SCALE_NODE_UPPER_RIGHT,
		SCALE_NODE_LOWER_LEFT,
		SCALE_NODE_LOWER_RIGHT
	}
		what;
		
	Node	node;
	
	int	x,y, sx,sy;	/* Platzierung und Groesse	*/
	
	int	correction_x,	/* Korrekturen, da Klickpunkt	*/
		correction_y;	/* nicht immer der Knoten-	*/
				/* mittelpunkt ist.		*/

	int	do_default_action;
}
	Drag_node_info;



typedef	struct	{
	Group	group;	/* DIE Gruppe			*/
	int	x ,y,	/* Position von group->node	*/
		x0,y0;	/* Ursprungsposition		*/
	int	correction_x,	/* siehe Drag_node_info	*/
		correction_y;
	int	do_default_action;
}
	Drag_group_info;


typedef	struct	{
	int	x1,y1,	/* Linke obere Ecke		*/
		x2,y2;	/* Rechte untere Ecke		*/
	int	shift_is_down;
	int	do_default_action;
}
	Drag_group_box_info;


typedef	struct	{
        int     do_default_action;
}
	Click_info;

typedef	struct	{
        int     do_default_action;
}
	Double_click_info;

extern	Evp_result	default_user_event_proc ();
extern	Evp_result	create_mode_event_proc  ();
extern	Evp_result	select_mode_event_proc  ();

extern	Evp_result	drag_node_proc          ();
extern	Evp_result	drag_edge_proc          ();
extern	Evp_result	drag_group_proc         ();
extern	Evp_result	drag_group_box_proc     ();

extern	Picklist	dispatch_picklist       ();
extern	void		pick                    ();
extern	void		unpick                  ();

extern	void		constrain_event         ();

extern	char		*mini_textedit		();

extern	Evp_result	(*user_event_proc)();


extern	int		inside_scrollbar;
extern	int		ms_left_down;
extern	int		ms_middle_down;
extern	int		ms_right_down;
extern	int		shift_is_down;
extern	int		ctrl_is_down;
extern	int		meta_is_down;
extern	int		last_event_id;
extern	int		constrain_is_active;
extern	int		double_click;
extern	Node_or_edge	group_labelling_operation_goes_to;

extern	int	draging_node;	/* Flag, ob gerade ein Knoten	*/
extern	int	draging_edge;	/* oder eine Kante gezogen wird	*/
	
extern	int	draging_group;		/* ... oder eine Gruppe	*/
extern	int	draging_group_box;	/* ... oder eine Box	*/

extern	int	making_node;
extern	int	making_edge;

extern	int		multi_click_space;
extern	int		multi_click_timeout;

extern	Picklist	pl_head;
extern	Picklist	picked_object;
extern	int		something_picked;
extern	Picklist	get_picked_object ();


extern	int	left_side_of_production_nodetype_index;
extern	int	left_side_of_prodution_size_enlargement;
extern	Nodelabel_placement	left_side_of_production_nlp;



typedef	struct	{
	Node	node;
	Edge	edge;
	Graph	graph;
}
	Last_worked_object;

extern	Last_worked_object	last;
extern	Graph			current_production;


extern	int	current_event_x;
extern	int	current_event_y;
extern	int	current_event_id;


#include "user_event_functions.h"

#endif
