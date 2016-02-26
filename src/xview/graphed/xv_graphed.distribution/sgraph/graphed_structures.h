/* (C) Universitaet Passau 1986-1991 */
/* Sgraph Source, 1988-1990 by Michael Himsolt */

#ifndef GRAPHED_STRUCTURE_H
#define GRAPHED_STRUCTURE_H

#include <xview/rect.h>

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
extern	void		add_to_extra_menu ();
extern	void		add_to_layout_menu ();
extern	void		add_to_tools_menu ();

extern	void		add_menu_to_user_menu ();
extern	void		add_menu_to_extra_menu ();
extern	void		add_menu_to_layout_menu ();
extern	void		add_menu_to_tools_menu ();

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


/*	template structure for node attributes	*/

typedef	struct	node_attributes {

	unsigned int		set;
	
	int			type_index, font_index;
	int			label_visibility;
	Node_edge_interface	node_edge_interface;
	Nodelabel_placement	nodelabel_placement;
	int			color;
	int			x,y;		/* not always needed	*/
	int			width, height;	/* not always needed	*/
	char			*label;		/* not always needed	*/
}
	Node_attributes;

extern	Node_attributes	get_node_attributes ();



/*	template structure for edge attributes	*/

typedef	struct	edge_attributes {

	unsigned int		set;
	
	int			type_index, font_index;
	int			label_visibility;
	float			arrow_angle;
	int			arrow_length;
	int			color;
	Edgeline		line;	/* not always needed	*/
	char			*label;	/* not always needed	*/
}
	Edge_attributes;

extern	Edge_attributes	get_edge_attributes ();


/*	Macros to set values from a Node_attributes/Edge_attributes	*/
/*	structure.							*/
/*	POSITIONS and LABELS are NOT set !				*/

#define	SET_NODE_ATTRIBUTES(attr) \
	NODE_SIZE,		(attr).width, (attr).height,	\
	NODE_FONT,		(attr).font_index,		\
	NODE_TYPE,		(attr).type_index,		\
	NODE_NEI,		(attr).node_edge_interface,	\
	NODE_NLP,		(attr).nodelabel_placement,	\
	NODE_LABEL_VISIBILITY,	(attr).label_visibility,	\
	NODE_COLOR,		(attr).color

#define	SET_EDGE_ATTRIBUTES(attr) \
	EDGE_TYPE,		(attr).type_index,		\
	EDGE_FONT,		(attr).font_index,		\
	EDGE_ARROW_LENGTH,	(attr).arrow_length,		\
	EDGE_ARROW_ANGLE,	(attr).arrow_angle,		\
	EDGE_LABEL_VISIBILITY,	(attr).label_visibility,	\
	EDGE_COLOR,		(attr).color


typedef	enum	{
	UEV_CONSUMED,
	UEV_VETO
}
	User_event_functions_result;

#endif GRAPHED_HEADER


extern void force_repainting ();
extern void lock_user_interface ();
extern void unlock_user_interface ();
extern int test_user_interface_locked ();

extern	void	set_user_event_func();
extern	void	remove_user_event_func();

#endif
