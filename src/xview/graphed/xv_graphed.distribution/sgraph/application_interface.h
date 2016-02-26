/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

#include <xview/rect.h>

#ifndef GRAPHED_HEADER
#define GRAPHED_HEADER

/* define virtual access to some of GraphEd's data structures	*/

typedef	char*		Graphed_graph;
typedef	char*		Graphed_node;
typedef	char*		Graphed_edge;
typedef	char*		Graphed_group;

extern	Graphed_graph	graphed_graph ();
extern	Graphed_node	graphed_node  ();
extern	Graphed_edge	graphed_edge  ();

extern	void		graph_set ();
extern	void		node_set  ();
extern	void		edge_set  ();
extern	void		group_set ();

extern	char		*node_get  ();
extern	char		*edge_get  ();

extern	Graphed_graph	create_graphed_graph_from_sgraph ();
extern	Graphed_node	create_graphed_node_from_snode   ();
extern	Graphed_edge	create_graphed_edge_from_sedge   ();
extern	Graphed_group	create_graphed_group_from_slist  ();

extern	void		add_to_user_menu ();
extern	char		*menu_call_sgraph_proc ();
extern	char		*call_sgraph_proc ();

typedef	enum	{
	SGRAPH_SELECTED_NONE,
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

/************************** Sgragra-interface **************************/
#ifdef SGRAGRA_HEADER

typedef enum
{
	SGG_UNDEFINED,
	SGG_NO_PRODUCTION,
	SGG_CURRENT_PROD_WINDOW,
	SGG_ACTIVE_WINDOW,
	SGG_ALL_WINDOWS
}
	Sgragra_create_mode;



typedef	struct	sgragra_proc_info
{
	Sgragra_create_mode	create_mode;
	Sgragra			sgragra;
	Sprod			current_production;
	
}
	*Sgragra_proc_info;


extern	Sgragra_proc_info	init_sgragra_from_graphed_gragra();
extern	void			exit_sgragra_from_graphed_gragra();
	

#endif 
/****************** End ********* Sgragra-interface ******************/


extern	int		sgraph_create_new_buffer ();
extern	int		sgraph_get_buffer ();
extern	int		sgraph_buffer_exists ();
extern	void		sgraph_set_working_area_buffer ();
extern	int		wac_buffer;

extern	void		free_group ();

extern	int		find_nodetype ();
extern	int		find_edgetype ();
extern	int		find_font ();

#ifndef	USER_HEADER
#include "dispatch_commands.h"
extern	char	*dispatch_user_action ();
#endif


#ifndef GRAPH_HEADER

typedef	enum {
	NODELABEL_MIDDLE,
	NODELABEL_UPPERLEFT,
	NODELABEL_UPPERRIGHT,
	NODELABEL_LOWERLEFT,
	NODELABEL_LOWERRIGHT,
	
	NUMBER_OF_NODELABEL_PLACEMENTS		/* Dummy		*/
	}
	Nodelabel_placement;


typedef	enum {
	NO_NODE_EDGE_INTERFACE,			/* "none"		*/
	TO_BORDER_OF_BOUNDING_BOX,		/* "middle"		*/
	TO_CORNER_OF_BOUNDING_BOX,		/* "corner"		*/
	CLIPPED_TO_MIDDLE_OF_NODE,		/* "clipped"		*/
	SPECIAL_NODE_EDGE_INTERFACE,		/* "special"		*/
	
	NUMBER_OF_NODE_EDGE_INTERFACES		/* Dummy		*/
	}
	Node_edge_interface;


typedef	enum	{
	
	/* Dummy for end of list	*/
	SET_ATTRIBUTE_END = 0,
	
	/* Node attributes		*/
	NODE_POSITION = 1,
	NODE_SIZE  = NODE_POSITION << 1,
	NODE_TYPE  = NODE_SIZE     << 1,
	NODE_NEI   = NODE_TYPE     << 1,
	NODE_NLP   = NODE_NEI      << 1,
	NODE_LABEL = NODE_NLP      << 1,
	NODE_FONT  = NODE_LABEL    << 1,
	NODE_LABEL_VISIBILITY = NODE_FONT << 1,
	NODE_COLOR = NODE_LABEL_VISIBILITY << 1,
	
	/* Edge attributes		*/
	EDGE_LINE = NODE_COLOR << 1,
	EDGE_TYPE = EDGE_LINE << 1,
	EDGE_ARROW_LENGTH = EDGE_TYPE << 1,
	EDGE_ARROW_ANGLE  = EDGE_ARROW_LENGTH << 1,
	EDGE_LABEL = EDGE_ARROW_ANGLE << 1,
	EDGE_FONT  = EDGE_LABEL << 1,
	EDGE_LABEL_VISIBILITY = EDGE_FONT << 1,
	EDGE_COLOR = EDGE_LABEL_VISIBILITY << 1,
	
	/* Misc	*/
	EDGE_INSERT = EDGE_COLOR + 1,
	EDGE_DELETE = EDGE_INSERT + 1,
	
	MOVE   = EDGE_DELETE + 1,
	RESIZE = MOVE + 1,
	
	/* Specialities			*/
	ONLY_SET   = RESIZE + 1,
	RESTORE_IT = ONLY_SET + 1,
	
	NODE_WIDTH  = RESTORE_IT + 1,
	NODE_HEIGHT = NODE_WIDTH + 1,
	NODE_X      = NODE_HEIGHT + 1,
	NODE_Y      = NODE_X + 1,
	
}
	Set_attribute;


extern void force_repainting ();
extern void lock_user_interface ();
extern void unlock_user_interface ();
extern int test_user_interface_locked ();


typedef	struct	edgeline
{
	coord		x,y;		/* Koordinaten			*/
	struct edgeline	*pre,		/* vorheriges Stueck		*/
			*suc;		/* naehstes   Stueck		*/
	Rect		box;		/* Rechteck, in dem die		*/
					/* Edgeline (mit ->suc) liegt	*/
}
	*Edgeline;

#define edgeline_x(el)   ((el)->x)
#define edgeline_y(el)   ((el)->y)
#define edgeline_pre(el) ((el)->pre)
#define edgeline_suc(el) ((el)->suc)
#define	is_single_edgeline(el) \
	(((el) != (Edgeline)NULL) && ((el)->suc->suc == (el)))
	 
extern	Edgeline	new_edgeline         ();
extern	Edgeline	add_to_edgeline      ();
extern	Edgeline	remove_from_edgeline ();
extern	void		set_edgeline_xy      ();
extern	void		free_edgeline        ();
extern	Edgeline	copy_edgeline        ();

#define	for_edgeline(el_head,el) \
	{ if (((el) = (el_head)) != (Edgeline)NULL) do {
#define	end_for_edgeline(el_head,el) \
	} while (((el) = (el)->suc) != (el_head)); }

#endif

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
		Sgraph_drag_node_info	node;
		Sgraph_drag_edge_info	edge;
		Sgraph_drag_group_info	group;
		Sgraph_drag_box_info	box;
	}
		details;
}
	*Sgraph_event_proc_info;



#endif
