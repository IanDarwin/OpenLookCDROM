/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				dispatch.c				*/
/*									*/
/************************************************************************/
/*									*/
/*			Benutzeraktionen ausfuehren			*/
/*									*/
/************************************************************************/

#include "user_header.h"
#include "print.h"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void		dispatch_user_action (action)			*/
/*	void		clean_buffer         (buffer)			*/
/*									*/
/************************************************************************/

void	clean_buffer ();


/************************************************************************/
/*									*/
/*			LOKALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static	void	nothing_edited ()				*/
/*									*/
/************************************************************************/

static	int	nothing_edited ();
static	void	set_node_defaults ();
static	void	set_edge_defaults ();


/************************************************************************/
/*									*/
/*		ALLGEMEIN BENUTZEREINGABEN AUSFUEHREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	static	int	nothing_edited ()				*/
/*									*/
/*	Prueft, ob im Moment nichts (kein Knoten, keine Kante, keine	*/
/*	Group editiert wird.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	void		dispatch_user_action (...)			*/
/*									*/
/*	Die folgende Prozedur ist der Dispatcher fuer Benutzeraktionen.	*/
/*									*/
/************************************************************************/
/*
 *	Actions :
 *
 *	Graph			graph
 *	Node			node, source, target
 *	Edge			edge
 *	Group			group
 *	Picklist		pl
 *	int			x,y, dx,dy
 *	char			character
 *	Node_edge_interface	nei
 *	Nodelabel_placement	nlp
 *	int			visibility
 *	int			length
 *	double			angle
 *	int			width
 *	int			index
 *
 *
 *	CREATE_MODE			switch to create mode
 *	SELECT_MODE			switch to edit mode
 *
 *	NODE_DEFAULTS			pop up node defaults subframe
 *	EDGE_DEFAULTS			pop up edge defaults subframe
 *
 *	_CREATE_GRAPH			create new graph, for internal purposes only
 *	CREATE_GRAPH			create new graph
 *	CREATE_DIRECTED_GRAPH		create new directed graph
 *	CREATE_UNDIRECTED_GRAPH		create new undirected graph
 *	CREATE_PRODUCTION		create new production graph
 *
 *	CREATE_NODE, graph, x,y		create new node in graph at (x,y)
 *	CREATE_EDGE, source, target,	create new edge from source to
 *	             line		target with line
 *	
 *	SPLIT_SELECTION			split selected edge at a bend
 *	MERGE_SELECTION			merge selected nodes into one node
 *
 *	REVERSE_EDGE
 *	CHANGE_GRAPH_DIRECTEDNESS
 *
 *	CREATE_BUFFER			create a new buffer
 *	
 *	SELECT       pl			select
 *	UNSELECT			unselect selection
 *	SELECT_NODE  node
 *	SELECT_EDGE  edge
 *	SELECT_GROUP group
 *	SELECT_GRAPH_OF_SELECTION	select the graph to whom the selection belongs
 *	SELECT_ALL
 *
 *	EXTEND_SELECTION pl		add pl to selection (exclusive or with selected group)
 *	EXTEND_SELECTION_WITH_NODE node
 *	EXTEND_SELECTION_WITH_EDGE edge	(no action)
 *	EXTEND_SELECTION_WITH_GROUP group
 *	EXTEND_SELECTION_WITH_GRAPH graph
 *
 *	LABEL_SELECTION character	add character to selection; <del character> deletes.
 *
 *	EDIT_SELECTION			show the corresponding editor subframe
 *	EDIT_GRAPH_OF_SELECTION		edit the selection's graph
 *	EDIT_GRAGRA			show the gragra editor subframe
 *
 *	MOVE_SELECTION	dx,dy
 *	RESIZE_SELECTION sx,sy		scale with factors (sx,sy)
 *	SHRINK_SELECTION		resize by (0.5,0.5)
 *	EXPAND_SELECTION		resize by (2.0,2.0)
 *
 *	EXTEND_EDGE   el, x,y		insert a bend after el at (x,y)
 *	MOVE_EDGE     el, x,y		move el to x,y
 *	COMPRIME_EDGE el		remove bend el from its edgeline
 *
 *	PUT_SELECTION			put operation
 *	PUT_WHOLE_GRAPH			put the selection's graph
 *	GET_SELECTION			get operation (select inserted objects)
 *	GET_AS_GRAPH			create a new graph, then perform get operation into the this graph
 *
 *	DELETE_SELECTION		delete the selection
 *	DELETE_ALL			delete the selection
 *
 *	COMPILE_PRODUCTION		graph grammmar compiler, compiles all productions until error occurs
 *	COMPILE_ALL_PRODUCTIONS
 *	SET_CURRENT_PRODUCTION		
 *	APPLY_PRODUCTION
 *	APPLY_CURRENT_PRODUCTION
 *	PRETTY_PRINT_PRODUCTION
 *	PRETTY_PRINT_CURRENT_PRODUCTION
 *	PRETTY_PRINT_ALL_PRODUCTIONS
 *
 *	CENTER_SELECTION
 *
 *	SELECTION_STATISTICS
 *	BUFFER_STATISTICS
 *	ALL_STATISTICS
 *
 *	ABOUT_GRAPHED
 *	
 *	LOAD_BY_SUBFRAME		show subframe for loading
 *	LOAD_AGAIN			load last file again
 *	STORE_BY_SUBFRAME		show subframe for storing
 *	STORE_TO_SAME_FILE		store to same file as last time
 *
 *	SET_NEI nei			set current node edge interface
 *	NEI_TO_BORDER_OF_BOUNDING_BOX
 *	NEI_TO_CORNER_OF_BOUNDING_BOX
 *	NEI_CLIPPED_TO_MIDDLE_OF_NODE
 *	NEI_SPECIAL
 *	NEI_NO_NODE_EDGE_INTERFACE
 *
 *	SET_NLP nlp			set current nodelabel placement
 *	NLP_MIDDLE
 *	NLP_UPPERLEFT
 *	NLP_UPPERRIGHT
 *	NLP_LOWERLEFT
 *	NLP_LOWERRIGHT
 *
 *	SCALE_NODESIZE x y		set current nodesize
 *	SCALE_NODESIZE_16_16
 *	SCALE_NODESIZE_32_32
 *	SCALE_NODESIZE_64_64
 *	SCALE_NODESIZE_128_128
 *	SCALE_NODESIZE_AS_SELECTED
 *
 *	SET_NODELABEL_VISIBILITY visibility
 *	SET_NODELABEL_VISIBLE
 *	SET_NODELABEL_INVISIBLE
 *
 *	SET_ALL_NODELABEL_VISIBILITY visibility
 *	SET_ALL_NODELABEL_VISIBLE
 *	SET_ALL_NODELABEL_INVISIBLE
 *
 *	SET_EDGELABEL_VISIBILITY visibility
 *	SET_EDGELABEL_VISIBLE
 *	SET_EDGELABEL_INVISIBLE
 *
 *	SET_ALL_EDGELABEL_VISIBILITY visibility
 *	SET_ALL_EDGELABEL_VISIBLE
 *	SET_ALL_EDGELABEL_INVISIBLE
 *
 *	SCALE_EDGELABELSIZE x, y
 *	SCALE_EDGELABELSIZE_64_64
 *	SCALE_EDGELABELSIZE_256_64
 *	SCALE_EDGELABELSIZE_UNCONSTRAINED
 *
 *	SCALE_ARROWLENGTH length
 *	SCALE_ARROWLENGTH_0
 *	SCALE_ARROWLENGTH_8
 *	SCALE_ARROWLENGTH_12
 *	SCALE_ARROWLENGTH_16
 *	SCALE_ARROWLENGTH_AS_SELECTED
 *
 *	SCALE_ARROWANGLE angle
 *	SCALE_ARROWANGLE_30
 *	SCALE_ARROWANGLE_45
 *	SCALE_ARROWANGLE_60
 *
 *	SET_GRAGRA_TYPE
 *	SET_GRAGRA_TYPE_NCE_1
 *	SET_GRAGRA_TYPE_NLC
 *	SET_GRAGRA_TYPE_BNLC
 *	SET_GRAGRA_TYPE_ENCE_1
 *		
 *
 *	SET_GRID width
 *	SET_GRID_8_8
 *	SET_GRID_16_16
 *	SET_GRID_32_32
 *	SET_GRID_64_64
 *	SET_GRID_128_128
 *	SET_GRID_OFF
 *
 *	PRINT				show the printer subframe
 *	REDRAW_ALL
 *	SAVE_STATE			save to .graphed
 *	EXPAND_WORKING_AREA		double working area size
 *	SHRINK_WORKING_AREA		half working area size
 *
 *	SET_CONSTRAINED
 *	RESET_CONSTRAINED
 *	TOGGLE_CONSTRAINED
 *
 *	GROUP_LABELLING_OPERATION_GOES_TO_NODE
 *	GROUP_LABELLING_OPERATION_GOES_TO_EDGE
 *	TOGGLE_GROUP_LABELLING_OPERATION
 *
 *	SET_NODEFONT index
 *	SET_EDGEFONT index
 *	SET_NODETYPE index
 *	SET_EDGETYPE index
 *
 *	NO_ACTION
 */



char	*dispatch_user_action (va_alist)
va_dcl
{
	va_list		args;
	User_action	action;
	
	char		filename [FILENAMESIZE];
	int		buffer;
	Graph		graph;
	Group		group;
	Node		node;
	Edge		edge;
	Picklist	pl;
	int		x,y;
	int		put_whole_graph = FALSE,
			get_as_graph    = FALSE;
	static	Node_defaults_subframe_info	node_defaults_info;
	static	Edge_defaults_subframe_info	edge_defaults_info;
	
	char		*dispatcher_result = NULL;
	
	static	last_wac_buffer = -1;
	
	if (wac_buffer != last_wac_buffer) {
		last_wac_buffer = wac_buffer;
	}
	
	
	
	va_start (args);

	action = va_arg (args, User_action);


	switch (action) {
	
	    case CREATE_MODE :
		if (nothing_edited() && remove_user_event_proc()) {
			set_menu_selection (CREATE_MODE);
			set_user_event_proc (create_mode_event_proc);
			set_base_frame_label (FRAME_LABEL_MODE_STRING, "Create");
				
			if (graphs_of_buffer(wac_buffer) == empty_graph) {
				dispatch_user_action (_CREATE_GRAPH, wac_buffer);
			}
			
		} else
			bell ();
		break;
	
		
	    case SELECT_MODE :
		if (nothing_edited() && remove_user_event_proc()) {
			set_menu_selection (SELECT_MODE);
			set_user_event_proc (select_mode_event_proc);
			set_base_frame_label (FRAME_LABEL_MODE_STRING, "Edit");
			
		} else
			bell ();
		break;


	    case NODE_DEFAULTS :
		node_defaults_info.attr = get_node_style (NORMAL_NODE_STYLE);
		show_node_defaults_subframe (&node_defaults_info, set_node_defaults);
		break;
	
	    case EDGE_DEFAULTS :
		edge_defaults_info.attr = get_edge_style (NORMAL_EDGE_STYLE);
		show_edge_defaults_subframe (&edge_defaults_info, set_edge_defaults);
		break;
	
	    case CREATE_DIRECTED_GRAPH :
		set_current_directedness (TRUE);
		dispatch_user_action (CREATE_GRAPH);
		break;
		
	    case CREATE_UNDIRECTED_GRAPH :
		set_current_directedness (FALSE);
		dispatch_user_action (CREATE_GRAPH);
		break;
		
	    case _CREATE_GRAPH :
		{
			int	buffer = va_arg (args, int);
			
			last.graph = create_graph (buffer);
			set_graph_directedness (last.graph, current_directedness);
			last.node = empty_node;
			last.edge = empty_edge;
			dispatcher_result = (char*)graph;
		}
		break;
		
	
	    case CREATE_GRAPH :
		if (nothing_edited () && remove_user_event_proc) {
		
			int x,y, buffer;
			
			if (menu_called_from == MENU_CALLED_FROM_CANVAS) {
				buffer = wac_buffer;
			} else {
				buffer = (int)dispatch_user_action (CREATE_BUFFER);
			}
			
			if (buffer != -1) {
			
				dispatch_user_action (UNSELECT);
				dispatcher_result = dispatch_user_action (_CREATE_GRAPH, buffer);
				dispatch_user_action (CREATE_MODE);

			} else {
				dispatcher_result = (char*)(-1);
			}
			
		} else {
			last.graph = empty_graph;
			last.node = empty_node;
			last.edge = empty_edge;

			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Please switch to create mode.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
			dispatcher_result = NULL;
		}
		break;
		
	
	    case CREATE_EMBEDDING :
		dispatcher_result = (char*)dispatch_user_action (CREATE_PRODUCTION);
		if (((Graph)dispatcher_result) != empty_graph && ((Graph)dispatcher_result)->firstnode != empty_node) {
			node_set (((Graph)dispatcher_result)->firstnode,
				NODE_LABEL, strsave (get_global_embedding_name()), 0);
		}
		break;
	
	    case CREATE_DIRECTED_EMBEDDING:
		set_current_directedness (TRUE);
		dispatcher_result = (char*)dispatch_user_action (CREATE_EMBEDDING);
		break;
		
	    case CREATE_UNDIRECTED_EMBEDDING :
		set_current_directedness (FALSE);
		dispatcher_result = (char*)dispatch_user_action (CREATE_EMBEDDING);
		break;
		
	
	    case CREATE_PRODUCTION :
			
		if (nothing_edited() && remove_user_event_proc ()) {
		
			int x,y, buffer;
			Graph	prod;
			
			if (menu_called_from == MENU_CALLED_FROM_CANVAS) {
				buffer = wac_buffer;
			} else {
				buffer = (int)dispatch_user_action (CREATE_BUFFER);
			}
			
			if (buffer != -1) {
			
				dispatch_user_action (UNSELECT);
				last.graph = create_production (buffer);
				set_graph_directedness (last.graph, current_directedness);
				set_gragra_type        (last.graph, get_current_gragra_type());
				last.node = empty_node;
				last.edge = empty_edge;
			
				dispatch_user_action (CREATE_MODE);
				
				if (buffer == wac_buffer) {
					dispatch_user_action (CREATE_NODE, last.graph, current_event_x, current_event_y);
				} else {
					get_buffer_center (buffer, &x, &y);
					dispatch_user_action (CREATE_NODE, last.graph, x, y);
				}
			
				node_set (last.node,
					SET_NODE_ATTRIBUTES (get_node_style (LEFT_SIDE_NODE_STYLE)),
					0);
				
				dispatcher_result = (char *)(last.node->graph);
				
			} else {
				dispatcher_result = (char*)(-1);
			}
			
		} else {
		
			last.graph = empty_graph;
			last.node  = empty_node;
			last.edge  = empty_edge;

			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Please switch to create mode.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
		}
		break;
		
	    case CREATE_DIRECTED_PRODUCTION:
		set_current_directedness (TRUE);
		dispatcher_result = (char *)dispatch_user_action (CREATE_PRODUCTION);
		break;
		
	    case CREATE_UNDIRECTED_PRODUCTION :
		set_current_directedness (FALSE);
		dispatcher_result = (char *)dispatch_user_action (CREATE_PRODUCTION);
		break;
		
		
	    case CREATE_NODE :
		
		graph = va_arg (args, Graph);
		x     = va_arg (args, int);
		y     = va_arg (args, int);
		
		dispatch_user_action (UNSELECT);
			
		node_set (node = create_node (graph),
			NODE_POSITION, x,y,
			DEFAULT_NODE_ATTRIBUTES,
			0);
		
		last.node  = node;
		last.graph = node->graph;
		
		dispatch_user_action (SELECT_NODE, node);
		dispatcher_result = (char *)node;
		break;
		
		
	    case CREATE_EDGE :
		{
			Node		source = va_arg (args, Node);
			Node		target = va_arg (args, Node);
			Edgeline	line   = va_arg (args, Edgeline);
			
			dispatch_user_action (UNSELECT);
			
			edge_set (edge = create_edge (source, target),
				EDGE_LINE, line,
				DEFAULT_EDGE_ATTRIBUTES,
				0);
			
			last.graph = edge->source->graph;	
			last.edge  = edge;
			last.node  = target;
		
			dispatch_user_action (SELECT_EDGE, edge);
			dispatcher_result = (char *)edge;
		}
		break;


	    case CREATE_BUFFER :
		{
			int new_buffer = create_buffer();
			
			dispatcher_result = (char *)new_buffer;
		
			if (new_buffer == -1) {
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"Cannot create more buffers.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
			} else {
				/* OK */
			}
		}
		break;
		
		
	    case SELECT :
		unpick ();
		pick (pl_head = va_arg (args, Picklist));
		activate_menu_item (EDIT_SELECTION);
		activate_menu_item (DELETE_SELECTION);
		activate_menu_item (EXPAND_SELECTION);
		activate_menu_item (SHRINK_SELECTION);
		break;
		
	    case UNSELECT :
		unpick ();
		inactivate_menu_item (EDIT_SELECTION);
		inactivate_menu_item (DELETE_SELECTION);
		inactivate_menu_item (EXPAND_SELECTION);
		inactivate_menu_item (SHRINK_SELECTION);
		break;
		
	    case SELECT_NODE :
		if ((node  = va_arg (args, Node)) != empty_node)
			dispatch_user_action (SELECT, new_picklist (NODE_PICKED, node));
		break;
		
	    case SELECT_EDGE :
		if ((edge = va_arg (args, Edge)) != empty_edge)
			dispatch_user_action(SELECT, new_picklist (EDGE_PICKED, edge));
		break;
		
	    case SELECT_GROUP :
		if ((group  = va_arg (args, Group)) != empty_group)
			dispatch_user_action (SELECT, new_picklist (GROUP_PICKED, group));
		break;
		
	    case SELECT_GRAPH :
		if ((graph  = va_arg (args, Graph)) != empty_graph)
			dispatch_user_action (SELECT_GROUP, make_group_of_graph (graph));
		break;
			
	    case SELECT_GRAPH_OF_SELECTION :
		if (something_picked) switch (picked_object->what) {
		    case GROUP_PICKED :
			if (group_nodes_are_all_of_same_graph (picked_object->which.group)) {
				graph = picked_object->which.group->node->graph;
			} else {
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"The group belongs to several graphs.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
				graph = empty_graph;
			}
			break;
		    case NODE_PICKED :
			graph = picked_object->which.node->graph;
			break;
		    case EDGE_PICKED :
			graph = picked_object->which.edge->source->graph;
			break;
		} else {
			graph = get_picked_or_only_existent_graph ();
		}
		if (graph != empty_graph) {
			if (!shift_is_down)
				dispatch_user_action (SELECT_GRAPH, graph);
			else
				dispatch_user_action (EXTEND_SELECTION_WITH_GRAPH, graph);
		}

		break;
			
	    case SELECT_ALL :
		dispatch_user_action (SELECT_GROUP, make_group_of_all());
		break;
	
	
	    case EXTEND_SELECTION :
		pl = va_arg (args, Picklist);
		switch (pl->what) {
		    case NODE_PICKED :
			dispatch_user_action (EXTEND_SELECTION_WITH_NODE, pl->which.node);
			break;
		    case EDGE_PICKED :
			dispatch_user_action (EXTEND_SELECTION_WITH_EDGE, pl->which.edge);
			break;
		    case GROUP_PICKED :
			dispatch_user_action (EXTEND_SELECTION_WITH_GROUP, pl->which.group);
			break;
		}
		break;
			
			
	    case EXTEND_SELECTION_WITH_NODE :
		
		node = va_arg (args, Node);
		if (something_picked) switch (picked_object->what) {
		
		    case NODE_PICKED :
			group = add_to_group (new_group (picked_object->which.node), node);
			dispatch_user_action (SELECT_GROUP, group);
			break;
				
		    case EDGE_PICKED :
			dispatch_user_action (SELECT_GROUP, new_group(node));
			break;
			
		    case GROUP_PICKED :
			if (contains_group_node (picked_object->which.group, node) != empty_group) {
	   	    		group = subtract_from_group (
	   	    			copy_group (picked_object->which.group),
					node);
				unmark_node (node);
	   	    	} else {
				group = add_to_group (copy_group (picked_object->which.group), node);
			}
			
			dispatch_user_action (SELECT_GROUP, group);
			break;
				
		} else /* !something_picked */ {
			
			dispatch_user_action (SELECT_GROUP, new_group(node));
				
		}
		break;
		
	    case EXTEND_SELECTION_WITH_EDGE :
		break;
		
	    case EXTEND_SELECTION_WITH_GROUP :
		group = va_arg (args, Group);
		if (something_picked) switch (picked_object->what) {
		    case NODE_PICKED :
			group = add_to_group (group, picked_object->which.node);
			break;
		    case EDGE_PICKED :
			break;
		    case GROUP_PICKED :
			group = add_groups_disjoint (group, copy_group (picked_object->which.group));
			break;
		}
		dispatch_user_action (SELECT_GROUP, group);
		break;
		
	    case EXTEND_SELECTION_WITH_GRAPH :
		graph = va_arg (args, Graph);
		dispatch_user_action (EXTEND_SELECTION_WITH_GROUP, make_group_of_graph (graph));
		break;
		
				
	    case LABEL_SELECTION :
		if (something_picked) {
			
			char	*string = va_arg (args, char *);
			Group	g;
			
			switch (picked_object->what) {
			    case NODE_PICKED :
				node = picked_object->which.node;
				node_set (node, NODE_LABEL,
					mini_textedit (node->label.text, string), 0);
				break;
			    case EDGE_PICKED :
				edge = picked_object->which.edge;
				edge_set (edge, EDGE_LABEL,
					mini_textedit (edge->label.text, string), 0);
				break;
			    case GROUP_PICKED :
				group = picked_object->which.group;
				if (group_labelling_operation_goes_to == NODE) {
				    for_group (group, g) {
					node_set (g->node, NODE_LABEL,
					    mini_textedit (g->node->label.text, string), 0);
				    } end_for_group (group, g);
				} else {
				    for_group (group, g) {
				        for_edge_sourcelist (g->node, edge) {
					    if (contains_group_node (group, edge->target) != empty_group)
					        edge_set (edge, EDGE_LABEL,
						    mini_textedit (edge->label.text, string), 0);
				       } end_for_edge_sourcelist (g->node, edge);
				   } end_for_group (group, g);
				}
				break;
			}
		}
		break;


	    case EDIT_SELECTION :
		if (something_picked) switch (picked_object->what) {
		    case NODE_PICKED :
			show_node_subframe (picked_object->which.node);
			break;
		    case EDGE_PICKED :
			show_edge_subframe (picked_object->which.edge);
			break;
		    case GROUP_PICKED :
			show_group_subframe (picked_object->which.group);
			break;
		}
		break;


	    case EDIT_GRAPH_OF_SELECTION :
		if ((graph = get_picked_or_only_existent_graph()) != empty_graph) {
			show_graph_subframe (graph);
		} else {
			bell ();
		}
		break;


	    case EDIT_GRAGRA :
		show_gragra_subframe ();
		break;


	    case MOVE_SELECTION :
		{
			int	dx;
			int	dy;
			
			dx = va_arg (args, int);
			dy = va_arg (args, int);
			
			if (something_picked) switch (picked_object->what) {
			    case NODE_PICKED :
				node_set (picked_object->which.node, MOVE, dx,dy, 0);
				break;
			    case EDGE_PICKED :
				break;
			    case GROUP_PICKED :
				group_set (picked_object->which.group, MOVE, dx,dy, 0);
				break;
			}
		}
		break;
		
		
	    case RESIZE_SELECTION :
		{
			int	center_of_group_x = 0,
				center_of_group_y = 0,
				size = 0;

			double	sx;
			double	sy;

			Node	node;
			Edge	edge;
			Group	g;
			Edgeline el;			


			sx = va_arg (args, double);
			sy = va_arg (args, double);

			
			if (something_picked) switch (picked_object->what) {
		
			    case NODE_PICKED :
				node = picked_object->which.node;
				node_set (node, NODE_SIZE,
					(int) (sx * (double)node_width(node)),
					(int) (sy * (double)node_height(node)), 0);
				break;

			    case EDGE_PICKED :
				break;

			    case GROUP_PICKED :
				group = picked_object->which.group;
				for_group (group,g) {
					center_of_group_x += node_x(g->node);				
					center_of_group_y += node_y(g->node);				
					size ++;
				} end_for_group (group,g);
				center_of_group_x /= size;
				center_of_group_y /= size;

#define SCALE_X(x) \
	(center_of_group_x +  (int)((double)((x) - center_of_group_x) * sx) )
#define SCALE_Y(y) \
	(center_of_group_y +  (int)((double)((y) - center_of_group_y) * sy) )

				for_group (group,g) {

					node_set (g->node, ONLY_SET,
						NODE_POSITION,
							SCALE_X (node_x(g->node)),
							SCALE_Y (node_y(g->node)),
						NODE_SIZE,
							(int) (sx * (double)node_width (g->node)),
							(int) (sy * (double)node_height(g->node)),
						0);

					for_edge_sourcelist (g->node, edge) {
						if (contains_group_node (group, edge->target)) {
							for_edgeline (edge->line, el) {
								set_edgeline_xy (el, SCALE_X(el->x), SCALE_Y(el->y));
							} end_for_edgeline (edge->line, el);
							edge_set (edge, ONLY_SET, EDGE_LINE, el, 0);
						}
					} end_for_edge_sourcelist (g->node, edge);
				} end_for_group (group,g);
				group_set (group, RESTORE_IT, 0);
				break;
			}
		}
		break;
		
		
	    case SHRINK_SELECTION :
		dispatch_user_action (RESIZE_SELECTION, 0.5, 0.5);
		break;
		
	    case EXPAND_SELECTION :
	    	dispatch_user_action (RESIZE_SELECTION, 2.0, 2.0);
		break;
	
	
	    case SPLIT_EDGE :
		{
			Edge		edge = va_arg (args, Edge),
					source_edge, target_edge;
			Edgeline	el = va_arg (args, Edgeline),
					line,
					el_source = (Edgeline)NULL,
					el_target = (Edgeline)NULL;
			Node		node;
			
				
			if (is_single_edgeline (el)) {
				node = (Node)dispatch_user_action (CREATE_NODE,
					edge->source->graph,
					(int)((el->x + el->suc->x)/2),
					(int)((el->y + el->suc->y)/2));
			} else {
				node = (Node)dispatch_user_action (CREATE_NODE,
					edge->source->graph, el->x, el->y);
			}
			
			/* Compute the new edgelines */
			
			if (is_single_edgeline (el)) {
			
				el_source = new_edgeline (edge->line->x, edge->line->y);
				(void)add_to_edgeline (el_source,
					(int)((edge->line->x + edge->line->suc->x)/2),
					(int)((edge->line->y + edge->line->suc->y)/2));
				
				el_target = new_edgeline (
					(int)((edge->line->pre->x + edge->line->x)/2),
					(int)((edge->line->pre->y + edge->line->y)/2));
				(void)add_to_edgeline (el_target,
					edge->line->pre->x, edge->line->pre->y);
				
			} else {
			
				if (el != edge->line) {
					for_edgeline (edge->line, line) {
						el_source = add_to_edgeline (el_source, line->x, line->y);
						if (line == el)
							break;
					} end_for_edgeline (edge->line, line);
					el_source = el_source->suc;
				} else {
					el_source = new_edgeline (edge->line->x, edge->line->y);
					(void)add_to_edgeline (el_source,
						edge->line->suc->x, edge->line->suc->y);
				}
				
				if (el != edge->line->pre) {
					for_edgeline (el, line) {
						el_target = add_to_edgeline (el_target, line->x,line->y);
						if (line == edge->line->pre)
							break;
					} end_for_edgeline (el, line);
					el_target = el_target->suc;
				} else {
					el_target = new_edgeline (edge->line->pre->x, edge->line->pre->y);
					(void)add_to_edgeline (el_source,
						edge->line->x, edge->line->y);
				}
			}
			
			/* create the new edges */
			
			source_edge = (Edge)dispatch_user_action (CREATE_EDGE,
				edge->source, node, el_source);
			target_edge = (Edge)dispatch_user_action (CREATE_EDGE,
				node, edge->target, el_target);
			edge_set (source_edge,
				SET_EDGE_ATTRIBUTES(get_edge_attributes(edge)),
				EDGE_LABEL, strsave(edge->label.text),
				0);
			edge_set (target_edge,
				SET_EDGE_ATTRIBUTES(get_edge_attributes(edge)),
				EDGE_LABEL, strsave(edge->label.text),
				0);
			erase_and_delete_edge (edge);
			
			dispatch_user_action (SELECT_NODE, node);
			
			dispatcher_result = (char*)node;
		}
		break;
		
		
	    case SPLIT_SELECTION :
		{
			Picked_point_of_edgeline	picked_el_point;
			Node				new_node;
			Group				g, new_group;
			Edge				e;
			
			if (something_picked) switch (picked_object->what) {
			
			    case NODE_PICKED :
				node = picked_object->which.node;
				
				new_node = (Node)dispatch_user_action (CREATE_NODE,
					node->graph,
					node_x(node) + node_width(node),
					node_y(node));
				node_set (new_node, NODE_LABEL, strsave (node->label.text), 0);
				
				for_edge_sourcelist (node, edge) {
					edge->source->iso = new_node;
					edge->target->iso = edge->target;
					copy_edge (node->graph, edge);
				} end_for_edge_sourcelist (node, edge);
				for_edge_targetlist (node, edge) {
					edge->source->iso = edge->source;
					edge->target->iso = new_node;
					copy_edge (node->graph, edge);
				} end_for_edge_targetlist (node, edge);
				
				for_edge_sourcelist (new_node, edge) {
					edge_set (edge, RESTORE_IT, 0);
				} end_for_edge_sourcelist (new_node, edge);
				for_edge_targetlist (new_node, edge) {
					edge_set (edge, RESTORE_IT, 0);
				} end_for_edge_targetlist (new_node, edge);
				
				dispatch_user_action (SELECT_NODE, new_node);
				dispatcher_result = (char *)new_node;
				break;
				
			    case GROUP_PICKED :
				group = picked_object->which.group;
				new_group = empty_group;
				
				/* First Run : Create the nodes and connecting edges to the rest graph */
				
				for_group (group, g) {
				
					node_set (new_node = copy_node (g->node->graph, g->node),
						ONLY_SET,
						NODE_POSITION,
							node_x(g->node) + node_width(g->node),
							node_y(g->node),
						0);
					new_group = add_to_group (new_group, new_node);
					
					for_edge_sourcelist (g->node, edge) {
						if (contains_group_node (group, edge->target) == empty_group &&
						    contains_group_node (new_group, edge->target) == empty_group) {
							edge->source->iso = new_node;
							edge->target->iso = edge->target;
							edge_set (copy_edge (g->node->graph, edge),
								ONLY_SET,
								0);
						}
					} end_for_edge_sourcelist (g->node, edge);
					for_edge_targetlist (g->node, edge) {
						if (contains_group_node (group, edge->source) == empty_group &&
						    contains_group_node (new_group, edge->source) == empty_group) {
							edge->source->iso = edge->source;
							edge->target->iso = new_node;
							edge_set (copy_edge (g->node->graph, edge),
								ONLY_SET,
								0);
						}
					} end_for_edge_targetlist (g->node, edge);
					
				} end_for_group (group, g);
				
				/* Second Run : Create the edges between the new nodes */
				
				for_group (group, g) {

					for_edge_sourcelist (g->node, edge) {
						if (contains_group_node (group, edge->target) != empty_group) {
							edge->source->iso = g->node->iso;
							edge->target->iso = edge->target->iso;
							edge_set (copy_edge (g->node->graph, edge),
								ONLY_SET,
								0);
						}
					} end_for_edge_sourcelist (g->node, edge);
					
				} end_for_group (group, g);
				
				group_set (new_group, RESTORE_IT, 0);
				dispatch_user_action (SELECT_GROUP, new_group);
				dispatcher_result = NULL;
				break;
				
			    case EDGE_PICKED :
				edge = picked_object->which.edge;
				picked_el_point = find_picked_point_of_edgeline (edge->line,
					current_event_x,
					current_event_y);
				if (picked_el_point.what == REAL_POINT_PICKED) {
					dispatcher_result = dispatch_user_action (
						SPLIT_EDGE, edge, picked_el_point.which.real_point.el);
				} else {
				
					Edgeline el;
					int	 i,n;
					
					/* Bestimme Mitte der edgeline */
					
					if (!is_single_edgeline(edge->line)) {
						n = 0;
						for_edgeline (edge->line, el) {
							n ++;
						} end_for_edgeline (edge->line, el);
					
						el = edge->line;
						for (i=0; i<n/2; i++) {
							el = el->suc;
						}
					} else {
						el = edge->line;
					}
					
					dispatcher_result = dispatch_user_action (
						SPLIT_EDGE, edge, el);
				}
				break;
			
			} else {
				bell ();
			}
		}
		break;
		
		
	    case MERGE_SELECTION :
		{
			Node		node;
			Edge		e;
			Group		picked, g;
			int		center_x, center_y, n;
			
			if (something_picked) switch (picked_object->what) {
			
			    case NODE_PICKED :
			    case EDGE_PICKED :
				dispatcher_result = NULL;
				break;
				
			    case GROUP_PICKED :
				
				picked = picked_object->which.group;
				
				/* compute the center of the selected group */
				center_x = 0;
				center_y = 0;
				n = 0;
				for_group (picked, g) {
					center_x += g->node->x;
					center_y += g->node->y;
					n++;
				} end_for_group (picked, g);
				center_x /= n;
				center_y /= n;
				
				/* Create a new node */
				node_set (node = create_node (picked->node->graph),
					NODE_POSITION, center_x, center_y,
					DEFAULT_NODE_ATTRIBUTES,
					0);
				
				/* Clean ->source->iso, ->target->iso */
				for_group (picked, g) {
					for_edge_sourcelist (g->node, e) {
						e->target->iso = empty_node;
					} end_for_edge_sourcelist (g->node, e);
					for_edge_targetlist (g->node, e) {
						e->source->iso = empty_node;
					} end_for_edge_targetlist (g->node, e);
				} end_for_group (picked, g);
				
				/* Copy the edges */
				for_group (picked, g) {
					for_edge_sourcelist (g->node, e) {
						if (contains_group_node (picked, e->target) == empty_group &&
						    e->target->iso == empty_node) {
							e->source->iso = node;
							e->target->iso = e->target;
							copy_edge (node->graph, e);
						}
					} end_for_edge_sourcelist (g->node, e);
					for_edge_targetlist (g->node, e) {
						if (contains_group_node (picked, e->source) == empty_group &&
						    e->source->iso == empty_node) {
							e->source->iso = e->source;
							e->target->iso = node;
							copy_edge (node->graph, e);
						}
					} end_for_edge_targetlist (g->node, e);
				} end_for_group (picked, g);
				
				/* erase the old group */
				dispatch_user_action (UNSELECT);
				erase_and_delete_group (picked);
				
				/* Restore the graphics */
				node_set (node, RESTORE_IT, 0);
				for_edge_sourcelist (node, e) {
					edge_set (e, RESTORE_IT, 0);
				} end_for_edge_sourcelist (node, e);
				for_edge_targetlist (node, e) {
					edge_set (e, RESTORE_IT, 0);
				} end_for_edge_targetlist (node, e);
				
				dispatch_user_action (SELECT_NODE, node);

				dispatcher_result = (char *)node; 
				break;
			}
		}
		break;
	
	
	    case REVERSE_EDGE :
		if (nothing_edited () && something_picked && picked_object->what == EDGE_PICKED) {
		
			Edge		original_edge = picked_object->which.edge;
			Edge		new_edge;
			Edgeline	reverse_el, el;
			
			original_edge->source->iso = original_edge->target;
			original_edge->target->iso = original_edge->source;
		
			reverse_el = (Edgeline)NULL;
			for_edgeline_reverse (original_edge->line->pre, el) {
				reverse_el = add_to_edgeline (reverse_el, el->x, el->y);
			} end_for_edgeline_reverse (original_edge->line->pre, el);
		
			edge_set (new_edge = copy_edge_without_line (
					original_edge->source->graph,
					original_edge),
				EDGE_LINE, reverse_el->suc,
				0);
		
			dispatch_user_action (UNSELECT);
			erase_and_delete_edge (original_edge);
			dispatch_user_action (SELECT_EDGE, new_edge);
			
			dispatcher_result = (char *)edge;
			
		} else {
			bell ();
		}
		break;
		

	    case SWAP_SELECTED_GRAPH_DIRECTEDNESS :
		if (nothing_edited ()) {
		
			Graph	graph;
			Group	group_of_graph;
			
			graph = get_picked_or_only_existent_graph ();
			
			if (graph != empty_graph) {
				delete_attatched_sgraph (graph);
				graph->directed = !graph->directed;

				group_of_graph = make_group_of_graph (graph);
				group_set (group_of_graph, RESTORE_IT, 0);
				free_group (group_of_graph);
			
				dispatcher_result = (char *)graph;
			} else {
				error ("no graph selected\n");
				dispatcher_result = (char *)empty_graph;
			}
			
		} else {
			bell ();
		}
		break;
		

	    case SET_GRAPH_DIRECTEDNESS :
		if (nothing_edited ()) {
		
			Graph	graph = va_arg (args, Graph);
			int	value = va_arg (args, int);
			Group	group_of_graph;
			
			graph = get_picked_or_only_existent_graph ();
			
			if (graph != empty_graph) {
				delete_attatched_sgraph (graph);
				graph->directed = value;

				group_of_graph = make_group_of_graph (graph);
				group_set (group_of_graph, RESTORE_IT, 0);
				free_group (group_of_graph);
			
				dispatcher_result = (char *)graph;
			} else {
				error ("no graph selected\n");
				dispatcher_result = (char *)empty_graph;
			}
			
		} else {
			bell ();
		}
		break;
		

	    case EXTEND_EDGE :
		if (something_picked && picked_object->what == EDGE_PICKED) {
			Edgeline	el = va_arg (args, Edgeline);
			int		x = va_arg (args, int);
			int		y = va_arg (args, int);
			
			edge_set (picked_object->which.edge, EDGE_INSERT, el, x,y, 0);
		}
		break;
		
	    case MOVE_EDGE :
		if (something_picked && picked_object->what == EDGE_PICKED) {
			Edgeline	el = va_arg (args, Edgeline);
			int		x = va_arg (args, int);
			int		y = va_arg (args, int);
			
			edge_set (picked_object->which.edge, MOVE, el, x-el->x, y-el->y, 0);
		}
		break;
		
	    case COMPRIME_EDGE :
		if (something_picked && picked_object->what == EDGE_PICKED) {
			Edgeline	el = va_arg (args, Edgeline);
			
			if (is_single_edgeline(el))
				/* Delete the whole edge	*/
				dispatch_user_action (DELETE_SELECTION);
			else
				edge_set (picked_object->which.edge, EDGE_DELETE, el, 0);
		}
		break;
		
		
	    case PUT_WHOLE_GRAPH :
		put_whole_graph = TRUE;
		/* Falls through, old assembler trick	*/
		
	    case PUT_SELECTION :
		
		if (something_picked) {
			
			/* Loesche Paste-Buffer und erzeuge eine Kopie des	*/
			/* picked_object im Paste-Buffer. Dieser Code stellt	*/
			/* sicher, dass dieser Buffer (z.Zt.) nur ein Element	*/
			/* enthaelt.						*/
			
			delete_graphs_in_buffer (paste_buffer);
			
			switch (picked_object->what) {
			    case NODE_PICKED :
				if (!put_whole_graph) {
					(void) make_graph_of_group (paste_buffer,
					                            new_group (picked_object->which.node));
				} else {
					(void) copy_graph (paste_buffer, picked_object->which.node->graph);
				}
				break;
			    case GROUP_PICKED :
				if (!put_whole_graph) {
					(void) make_graph_of_group (paste_buffer, picked_object->which.group);
				} else if (group_nodes_are_all_of_same_graph (picked_object->which.group)) {
					(void) copy_graph (paste_buffer, picked_object->which.group->node->graph);
				} else {
					bell ();
				}
				break;
			    default :
				bell ();
				break;
			}
		
		} else {
			
			bell ();
		}
		break;


	    case GET_AS_GRAPH :
		get_as_graph = TRUE;
		/* Falls through	*/
		
	    case GET_SELECTION :

/*
		if (user_event_proc == create_mode_event_proc) {
			if (!remove_user_event_proc()) {
				notice_prompt (base_frame, NULL,		fisprompt
					NOTICE_MESSAGE_STRINGS,	"Please switch to edit mode.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
				break;
			}
			dispatch_user_action (SELECT_MODE);
		}
*/
		
		if (buffers[paste_buffer].graphs != empty_graph) {
			
			Graph	graph = buffers[paste_buffer].graphs;
			Group	group_of_copy;
			Rect	rect;
			int	x,y, dx,dy;
			
			dispatch_user_action (UNSELECT);
			
			rect = compute_rect_around_graph (graph);
			/* graph->box has not been set in paste_buffer	*/
			dx   = current_event_x - (rect_left(&rect) + rect_width (&rect)/2);
			dy   = current_event_y - (rect_top (&rect) + rect_height(&rect)/2);

			/* Fuege den Graphen in last.graph ein oder erzeuge einen	*/
			/* neuen Graphen.						*/
			if (last.graph != empty_graph && !get_as_graph)
				group_of_copy = copy_graph_to_graph (last.graph, graph);
			else {
				group_of_copy = make_group_of_graph(last.graph = copy_graph (wac_buffer, graph));
				last.node = empty_node;
				last.edge = empty_edge;
			}
			
			/* verschiebe den Graphen an die aktuelle Position, so dass	*/
			/* der Cursor in die Mitte des Graphen kommt.			*/
			group_set (group_of_copy, MOVE, dx,dy, RESTORE_IT, 0);
			
			dispatch_user_action (SELECT_GROUP, group_of_copy);
				
			
		} else {
			
			bell ();
		
		}
		break;


	    case DELETE_SELECTION:
	
		if (something_picked) {
		
			Edge	edited_edge, edge;
			Node	edited_node, node;
			Group	edited_group, group, copy_of_group, g;
			int	illegally_contains_left_side;
			
			/* Delete the selected Object. We must check	*/
			/* the following conditions :			*/
			/* - an edited node or edge may not be deleted,	*/
			/*   expecially an edge may not be in the	*/
			/*   adjacency lists of a node that is deleted.	*/
			/* - the left side of a production may not be	*/
			/*   deleted, since that could cause serious	*/
			/*   consistency problems. The only exception	*/
			/*   is the situation when the whole graph	*/
			/*   is deleted.				*/
			
			
			edited_node   = get_currently_edited_node  ();
			edited_edge   = get_currently_edited_edge  ();
			edited_group  = get_currently_edited_group ();
			
			switch (picked_object->what) {
			
			    case GROUP_PICKED :
			
				/* Loesche Gruppe		*/
								
				group = picked_object->which.group;
				
				/* At first, look for a left side of a production.	*/
				/* If group contains the left side but not all other	*/
				/* nodes of a graph, illegally_contains_left_side will	*/
				/* become TRUE. In this case, deletion is rejected.	*/
				
				illegally_contains_left_side = FALSE;
				for_group (group, g) {
					if (is_left_side_of_production (g->node) && !contains_group_graph (group, g->node->graph))
						illegally_contains_left_side = TRUE;
				} end_for_group (group, g);
				
				if (illegally_contains_left_side) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"This group contains a left side of a production.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else if (contains_group_node (group, edited_node) != empty_group) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"You are editing a node of this group.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else if (edited_edge != empty_edge &&
				           (contains_group_node (group, edited_edge->source) != empty_group ||
				            contains_group_node (group, edited_edge->target) != empty_group) ) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"You are editing an edge which belongs to a node of this group.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else if (group_intersects_group (edited_group, group)) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"You are editing a part of the group you want to delete.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else {
				
					if (last.graph != empty_graph && contains_group_graph (group, last.graph)) {
						last.graph = empty_graph;
						last.node  = empty_node;
						last.edge  = empty_edge;
					}
					if (last.node != empty_node && contains_group_node (group, last.node) != empty_group)
						last.node = empty_node;
					if ((last.edge != empty_edge) &&
					     ((contains_group_node (group, last.edge->source) != empty_group) ||
					      (contains_group_node (group, last.edge->target) != empty_group))) {
						last.edge = empty_edge;
					}
					
					copy_of_group = copy_group (group);	/* necessary mecause group is freed in the following UNSELECT	*/
					dispatch_user_action (UNSELECT);
					erase_and_delete_group (copy_of_group);
				}
					
				break;					
					
			    case NODE_PICKED :
				
				/* Loesche Knoten		*/
				
				node = picked_object->which.node;
				
				if (node == edited_node) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"You are editing this node.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else if (edited_edge != empty_edge && (edited_edge->source == node || edited_edge->target == node)) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"You are editing a edge which belongs to this node.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else if (edited_group != empty_group && contains_group_node (edited_group, node) != empty_group) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"You are editing a group of nodes to which the one you want to delete belongs.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else if (is_left_side_of_production (node) && !(node->suc == node)) {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"This node is the left side of a production with nonempty right side.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
				} else {
					
					if (node == last.node)
						last.node = iif (node == node->pre, empty_node, node->pre);
					if (last.edge != empty_edge &&
					    (last.edge->source == node || last.edge->target == node))
						last.edge = empty_edge;
					
					dispatch_user_action (UNSELECT);
					erase_and_delete_node (node);
				}
				break;
				
			    case EDGE_PICKED :
				
				/* Loesche GESAMTE Kante	*/
				
				edge = picked_object->which.edge;
				
				if (edge != edited_edge) {
				
					if (edge == last.edge)
						last.edge = iif (edge == edge->sourcepre,
							iif (edge == edge->targetpre, empty_edge, edge->targetpre),
							edge->sourcepre);
					dispatch_user_action (UNSELECT);
					erase_and_delete_edge (edge);
					
				} else {
					bell ();
				}
				break;
			}
			
			
			/* We must now look for empty graphs, because they are	*/
			/* no more accessable for the user. At best, remove	*/
			/* these zombies.					*/
			
			clean_buffer (wac_buffer);
			
		}
		break;


	    case DELETE_ALL:
		if (nothing_edited () &&
		    /* user_event_proc abmelden, da sonst "dangling pointer"	*/
		    /* entstehen koennen !					*/
		    remove_user_event_proc()) {
			if (NOTICE_YES == notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Delete all these pretty things.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NOTICE_BUTTON_NO,	"Cancel",
				NULL)) {
				dispatch_user_action (UNSELECT);
				last.node  = empty_node;
				last.edge  = empty_edge;
				last.graph = empty_graph;
				current_production = empty_graph;
				delete_graphs_in_buffer (wac_buffer);
				scroll_working_area_to_middle ();
			}
		} else {
			bell ();
		}
		dispatch_user_action (CREATE_MODE);
		break;


	    case COMPILE_PRODUCTION :
		
		graph = va_arg (args, Graph);
		if (user_event_proc == create_mode_event_proc) {
			if (!remove_user_event_proc()) {
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"Please switch to edit mode.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
				break;
			}
			dispatch_user_action (SELECT_MODE);
		}

		if (graph->is_production && graph->change_time >= graph->compile_time) {
			if (!compile_production (graph)) {
				error ("%s\n", compile_production_error_message);
				dispatch_user_action (UNSELECT);
				dispatch_user_action (SELECT, compile_production_error_list);
				break;
			} else {
				if (graph->label == NULL || !strcmp (graph->label, "")) {
					message ("Compilation successful\n");
				} else {
					message ("Compilation of %s successful\n", graph->label);
				}
			}
		}
		break;


	    case COMPILE_ALL_PRODUCTIONS :
		if (user_event_proc == create_mode_event_proc) {
			if (!remove_user_event_proc()) {
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"Please switch to edit mode.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
				break;
			}
			dispatch_user_action (SELECT_MODE);
		}

		{
			int	no_errors = TRUE;
			
			for (buffer=N_PASTE_BUFFERS; buffer<N_BUFFERS && no_errors; buffer++) {
				for_all_graphs (buffer, graph){
					dispatch_user_action (COMPILE_PRODUCTION, graph);
					if (graph->compile_time < 0) {
						no_errors = FALSE;
						break;
					}
				} end_for_all_graphs (buffer, graph);
			}
		}
		break;


	    case SET_CURRENT_PRODUCTION :
		if (user_event_proc == select_mode_event_proc) {
		
			Graph	prod = empty_graph;
			
			if (something_picked) switch (picked_object->what) {
			    case NODE_PICKED :
				prod = picked_object->which.node->graph;
				break;
			    case EDGE_PICKED :
				prod = picked_object->which.edge->source->graph;
				break;
			    case GROUP_PICKED :
			    	if (group_nodes_are_all_of_same_graph (picked_object->which.group)) {
					prod = picked_object->which.group->node->graph;
				} else {
					notice_prompt (base_frame, NULL,		/*fisprompt*/
						NOTICE_MESSAGE_STRINGS,	"Please give a graph.", NULL,
						NOTICE_BUTTON_YES,	"Ok",
						NULL);
					prod = empty_graph;
				}
				break;
			}
			if (something_picked && prod != empty_graph && prod->is_production) {
				current_production = prod;
			} else {
				current_production = empty_graph;
				bell ();
			}
		} else {
			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Can only be done in edit mode.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
			current_production = empty_graph;
		}
		break;	
	
	    case APPLY_CURRENT_PRODUCTION :
			
		if (current_production != empty_graph && something_picked && picked_object->what == NODE_PICKED) {
		
			int	current_production_is_compiled = TRUE;
			
			dispatch_user_action (COMPILE_ALL_PRODUCTIONS);
			
			if (compile_production_error_message != NULL) {
			
				/* nothing */
			
			} else if (picked_object->which.node->graph == current_production) {
				
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"You can't apply a production on itself.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
				
			} else if (current_production->directed != picked_object->which.node->graph->directed) {
					
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"Production and node have different directeness.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
					
			} else if (embed_match_node (picked_object->which.node, current_production->gra.gra.nce1.left_side->node)) {
				
				Group	derived_right_side;
				Node	node_to_apply;
					
				node_to_apply = picked_object->which.node;
				dispatch_user_action (UNSELECT);
				if (last.node != empty_node && last.node == node_to_apply)
					last.node = empty_node;
				if (last.edge != empty_edge &&
				    (last.edge->source == node_to_apply || last.edge->target == node_to_apply) )
					last.edge = empty_edge;
					
				derived_right_side = apply_production (current_production, node_to_apply);
				
				dispatch_user_action (SELECT_GROUP, derived_right_side);
					
			} else {
					
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"Node does not match left side of rule.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
			}
				
		} else {
			
			if (current_production == empty_graph)
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"No current production.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
			else if (!(something_picked && picked_object->what == NODE_PICKED))
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"No node picked.", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);				
		}
		break;
		
		
	    case PRETTY_PRINT_PRODUCTION :
		graph = va_arg (args, Graph);
		if (graph->is_production ) {
			if (graph->compile_time < graph->change_time)
				dispatch_user_action (COMPILE_PRODUCTION, graph);
			if (graph->compile_time > 0) {
				pretty_print_production (graph);
				/* The graph has now changed. Since the gragra information	*/
				/* is still valid, reset the compile_time.			*/
				graph->compile_time = ticks ();
			}
		}
		break;


	    case PRETTY_PRINT_ALL_PRODUCTIONS :
		{
			int	no_errors = TRUE;
			
			for (buffer=N_PASTE_BUFFERS; buffer<N_BUFFERS && no_errors; buffer++) {
				for_all_graphs (buffer, graph) if (graph->is_production) {
					dispatch_user_action (PRETTY_PRINT_PRODUCTION, graph);
					if (graph->compile_time < 0) {
						no_errors = FALSE;
						break;
					}
				} end_for_all_graphs (buffer, graph);
			}
		}
		break;


	    case PRETTY_PRINT_CURRENT_PRODUCTION :
		if (current_production != empty_graph) {
			dispatch_user_action (PRETTY_PRINT_PRODUCTION, current_production);
		} else {
			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"No current production.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
		}
		break;


	    case CENTER_SELECTION :
		
		if (something_picked) {
			
			Group	 g;
			Edgeline el;
			int	 x = 0, y = 0, n_el = 0, n_nodes = 0;
			
			switch (picked_object->what) {
			    case NODE_PICKED :
				node = picked_object->which.node;
				center_buffer_around (node->graph->buffer, node->x, node->y);
				break;
			    case EDGE_PICKED :
				edge = picked_object->which.edge;
				for_edgeline (edge->line, el) {
					n_el ++;
					x += el->x;
					y += el->y;
				} end_for_edgeline (edge->line, el);
				center_buffer_around (edge->source->graph->buffer, x/n_el, y/n_el);
				break;
			    case GROUP_PICKED :
				group = picked_object->which.group;
				for_group (group, g) {
				    	n_nodes ++;
				    	x += g->node->x;
				    	y += g->node->y;
				} end_for_group (group, g);
				center_buffer_around (g->node->graph->buffer, x/n_nodes, y/n_nodes);

				break;
			}
		}
		break;


	    case SELECTION_STATISTICS :
		
		if (something_picked) {
			
			Group	g;
			int 	n_nodes = 0, n_edges = 0;
			
			switch (picked_object->what) {
			    case NODE_PICKED :
				message ("1 node\n");
				break;
			    case EDGE_PICKED :
				message ("1 edge\n");
				break;
			    case GROUP_PICKED :
				group = picked_object->which.group;
				for_group (group, g) {
				    n_nodes ++;
				    for_edge_sourcelist (g->node, edge) {
					if (contains_group_node (group, edge->target)) n_edges ++;
				    } end_for_edge_sourcelist (g->node, edge);
				} end_for_group (group, g);
				message ("%d nodes, %d edges\n", n_nodes, n_edges);
				break;
			}
			
		} else {
		
			message ("nothing selected\n");
		
		}
		break;


	    case BUFFER_STATISTICS :
		{
			Graph	graph;
			int 	i, n_nodes = 0, n_edges = 0, n_graphs = 0;
			
			for_all_graphs (wac_buffer, graph) {
				for_nodes (graph, node) {
					n_nodes ++;
					for_edge_sourcelist (node, edge) {
						n_edges++;
					} end_for_edge_sourcelist (node, edge);
				} end_for_nodes (graph, node);
				n_graphs ++;
				message ("graph %d : %d nodes, %d edges\n", n_graphs, n_nodes, n_edges);
			} end_for_all_graphs (wac_buffer, graph);
		}
		break;


	    case ALL_STATISTICS :
		{
			int 	buffer, n_nodes = 0, n_edges = 0, n_graphs = 0;
			
			for (buffer=N_PASTE_BUFFERS; buffer<N_BUFFERS; buffer++) if (buffers[buffer].graphs != empty_graph) {
				message ("buffer %d / ", buffer);
				for_all_graphs (wac_buffer, graph) {
					for_nodes (graph, node) {
						n_nodes ++;
						for_edge_sourcelist (node, edge) {
							n_edges++;
						} end_for_edge_sourcelist (node, edge);
					} end_for_nodes (graph, node);
					n_graphs ++;
					message ("graph %d : %d nodes, %d edges\n", n_graphs, n_nodes, n_edges);
				} end_for_all_graphs (wac_buffer, graph);
			}
		}
		break;


	    case LOAD_BY_SUBFRAME :
		if (test_user_interface_locked() || nothing_edited() == FALSE) {
			bell ();
		} else if (!buffers[wac_buffer].changed || (menu_called_from != MENU_CALLED_FROM_CANVAS) || NOTICE_YES == notice_prompt(base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Discard all changes ?", NULL,
				NOTICE_BUTTON_YES,	"Discard",
				NOTICE_BUTTON_NO,	"Cancel",
				NULL)) {
		
			int	buffer = wac_buffer;
			
			if (menu_called_from != MENU_CALLED_FROM_CANVAS) {
				buffer = (int)dispatch_user_action (CREATE_BUFFER);
				if (buffer != -1) {
					set_working_area_canvas (canvases[buffer].canvas);
				}
			}
			if (buffer != -1) {
				dispatch_user_action (UNSELECT);
				last.node  = empty_node;
				last.edge  = empty_edge;
				last.graph = empty_graph;
				current_production = empty_graph;
			
				dispatch_user_action (SELECT_MODE);
				show_file_selection_subframe (LOAD);
			}
		}
		break;

	    case LOAD_AGAIN :
		if (menu_called_from != MENU_CALLED_FROM_CANVAS) {
			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"This command can only be called from a window menu.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
		} else if (test_user_interface_locked() || nothing_edited() == FALSE) {
			bell ();
		} else if (get_filename() == NULL || !strcmp(get_filename(),"")) {
			dispatch_user_action (LOAD_BY_SUBFRAME);
		} else {
			if (any_graph_has_changed() == FALSE || NOTICE_YES == notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"Discard all changes ?", NULL,
				NOTICE_BUTTON_YES,	"Discard",
				NOTICE_BUTTON_NO,	"Cancel",
				NULL)) {
		    
				dispatch_user_action (UNSELECT);
				last.node  = empty_node;
				last.edge  = empty_edge;
				last.graph = empty_graph;
				current_production = empty_graph;
			
				load (wac_buffer, buffers[wac_buffer].filename);
			
				dispatch_user_action (SELECT_MODE);
			} else {
				bell ();
			}
		}
		break;
		
		
	    case STORE_BY_SUBFRAME :
		if (test_user_interface_locked()) {
			/* skip */;
		} else if (menu_called_from != MENU_CALLED_FROM_CANVAS) {
			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"This command can only called from a window menu.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
		} else {
			clean_buffer (wac_buffer);
			show_file_selection_subframe (STORE);
		}
		break;

	    case STORE_TO_SAME_FILE :
		if (menu_called_from != MENU_CALLED_FROM_CANVAS) {
			notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"This command can only called from a window menu.", NULL,
				NOTICE_BUTTON_YES,	"Ok",
				NULL);
		} else if (get_filename() == NULL || !strcmp(get_filename(),"")) {
			dispatch_user_action (STORE_BY_SUBFRAME);
		} else {		
			clean_buffer (wac_buffer);
			if (buffers[wac_buffer].graphs != empty_graph) {
				if (buffers[wac_buffer].graphs->suc != buffers[wac_buffer].graphs) {
					store_graphs (get_filename());
				} else {
					store_graph (buffers[wac_buffer].graphs, get_filename());
				}
			} else {
				notice_prompt (base_frame, NULL,		/*fisprompt*/
					NOTICE_MESSAGE_STRINGS,	"No graph to store !", NULL,
					NOTICE_BUTTON_YES,	"Ok",
					NULL);
			}
		}
		break;

	
	    case SET_NEI :
		{
			Node_edge_interface	nei;
			
			nei = va_arg(args, Node_edge_interface);
			set_current_node_edge_interface (nei);
			dispatch_user_action (CREATE_MODE);
		}
		break;
		
	    case NEI_TO_BORDER_OF_BOUNDING_BOX :
		dispatch_user_action (SET_NEI, TO_BORDER_OF_BOUNDING_BOX);
		break;
	    case NEI_TO_CORNER_OF_BOUNDING_BOX :
		dispatch_user_action (SET_NEI, TO_CORNER_OF_BOUNDING_BOX);
		break;
	    case NEI_CLIPPED_TO_MIDDLE_OF_NODE :
		dispatch_user_action (SET_NEI, CLIPPED_TO_MIDDLE_OF_NODE);
		break;
	    case NEI_SPECIAL :
		dispatch_user_action (SET_NEI, SPECIAL_NODE_EDGE_INTERFACE);
		break;
	    case NEI_NO_NODE_EDGE_INTERFACE :
		dispatch_user_action (SET_NEI, NO_NODE_EDGE_INTERFACE);
		break;

			
	    case SET_NLP :
		{
			Nodelabel_placement	nlp;
			
			nlp = va_arg(args, Nodelabel_placement);
			set_current_nodelabel_placement (nlp);
			dispatch_user_action (CREATE_MODE);
		}
		break;
		
	    case NLP_MIDDLE :
		dispatch_user_action (SET_NLP, NODELABEL_MIDDLE);
		break;
	    case NLP_UPPERLEFT :
		dispatch_user_action (SET_NLP, NODELABEL_UPPERLEFT);
		break;
	    case NLP_UPPERRIGHT :
		dispatch_user_action (SET_NLP, NODELABEL_UPPERRIGHT);
		break;
	    case NLP_LOWERLEFT :
		dispatch_user_action (SET_NLP, NODELABEL_LOWERLEFT);
		break;
	    case NLP_LOWERRIGHT :
		dispatch_user_action (SET_NLP, NODELABEL_LOWERRIGHT);
		break;
	
	
	    case SCALE_NODESIZE :
		x = va_arg (args, int);
		y = va_arg (args, int);
		set_current_nodesize (x,y);
		dispatch_user_action (CREATE_MODE);
		break;
	
	    case SCALE_NODESIZE_16_16 :
		dispatch_user_action (SCALE_NODESIZE, 16,16);
		break;
	    case SCALE_NODESIZE_32_32 :
		dispatch_user_action (SCALE_NODESIZE, 32,32);
		break;
	    case SCALE_NODESIZE_64_64 :
		dispatch_user_action (SCALE_NODESIZE, 64,64);
		break;
	    case SCALE_NODESIZE_128_128 :
		dispatch_user_action (SCALE_NODESIZE, 128,128);
		break;
	    case SCALE_NODESIZE_AS_SELECTED :
		if (something_picked && picked_object->what == NODE_PICKED)
			dispatch_user_action (SCALE_NODESIZE,
				node_width  (picked_object->which.node),
				node_height (picked_object->which.node));
		else
			bell ();
		break;
	
	
	    case SET_NODELABEL_VISIBILITY :
		{
			int	visibility;
			
			visibility = va_arg (args, int);
			set_current_nodelabel_visibility (visibility);
			dispatch_user_action (CREATE_MODE);
		}
		break;
	
	    case SET_NODELABEL_VISIBLE :
		dispatch_user_action (SET_NODELABEL_VISIBILITY, TRUE);
		break;
	    case SET_NODELABEL_INVISIBLE :
		dispatch_user_action (SET_NODELABEL_VISIBILITY, FALSE);
		break;
	
	    case SET_ALL_NODELABEL_VISIBILITY :
		{
			int	visibility;
			
			visibility = va_arg (args, int);

			for_all_graphs (wac_buffer, graph)
			    for_nodes (graph, node)
				node_set (node, NODE_LABEL_VISIBILITY, visibility, 0);
			    end_for_nodes (graph, node);
			end_for_all_graphs (wac_buffer, graph);
			set_current_nodelabel_visibility (TRUE);
		}
		break;

	    case SET_ALL_NODELABEL_VISIBLE :
		dispatch_user_action (SET_ALL_NODELABEL_VISIBILITY, TRUE);
		break;
		
	    case SET_ALL_NODELABEL_INVISIBLE :
		dispatch_user_action (SET_ALL_NODELABEL_VISIBILITY, FALSE);
		break;
	
	
	    case SET_EDGELABEL_VISIBILITY :
		{
			int	visibility;
			
			visibility = va_arg (args, int);
			set_current_edgelabel_visibility (visibility);
			dispatch_user_action (CREATE_MODE);
		}
		break;
	
	    case SET_EDGELABEL_VISIBLE :
		dispatch_user_action (SET_EDGELABEL_VISIBILITY, TRUE);
		break;
	    case SET_EDGELABEL_INVISIBLE :
		dispatch_user_action (SET_EDGELABEL_VISIBILITY, FALSE);
		break;
	
	    case SET_ALL_EDGELABEL_VISIBILITY :
		{
			int	visibility;
			
			visibility = va_arg (args, int);
			for_all_graphs (wac_buffer, graph)
			    for_nodes (graph, node)
				for_edge_targetlist (node, edge)
					edge_set (edge, EDGE_LABEL_VISIBILITY, visibility, 0);
				end_for_edge_sourcelist (node, edge);
			    end_for_nodes (graph, node);
    			end_for_all_graphs (wac_buffer, graph);
			set_current_edgelabel_visibility (TRUE);
		}
		break;

	    case SET_ALL_EDGELABEL_VISIBLE :
		dispatch_user_action (SET_ALL_EDGELABEL_VISIBILITY, TRUE);
		break;
	    case SET_ALL_EDGELABEL_INVISIBLE :
		dispatch_user_action (SET_ALL_EDGELABEL_VISIBILITY, FALSE);
		break;
		
		
	    case SCALE_EDGELABELSIZE :
		x = va_arg (args, int);
		y = va_arg (args, int);
		set_current_edgelabelsize (x,y);
		dispatch_user_action (CREATE_MODE);
		break;
		
	    case SCALE_EDGELABELSIZE_64_64 :
		dispatch_user_action (SCALE_EDGELABELSIZE, 64,64);
		break;
	    case SCALE_EDGELABELSIZE_256_64 :
		dispatch_user_action (SCALE_EDGELABELSIZE, 256,64);
		break;
	    case SCALE_EDGELABELSIZE_UNCONSTRAINED :
		dispatch_user_action (SCALE_EDGELABELSIZE, MAXINT, MAXINT);
		break;
		
		
	    case SCALE_ARROWLENGTH :
		set_current_arrowlength (va_arg (args, int));
		dispatch_user_action (CREATE_MODE);
		break;
	
	    case SCALE_ARROWLENGTH_0 :
		dispatch_user_action (SCALE_ARROWLENGTH, 0);
		break;
	    case SCALE_ARROWLENGTH_8 :
		dispatch_user_action (SCALE_ARROWLENGTH, 8);
		break;
	    case SCALE_ARROWLENGTH_12 :
		dispatch_user_action (SCALE_ARROWLENGTH, 12);
		break;
	    case SCALE_ARROWLENGTH_16 :
		dispatch_user_action (SCALE_ARROWLENGTH, 16);
		break;
	    case SCALE_ARROWLENGTH_AS_SELECTED :
		if (something_picked && picked_object->what == EDGE_PICKED)
			dispatch_user_action (SCALE_ARROWLENGTH, picked_object->which.edge->arrow.length);
		else
			bell ();
		break;
	
	
	    case SCALE_ARROWANGLE :
		set_current_arrowangle (va_arg (args, double));
		dispatch_user_action (CREATE_MODE);
		break;
		
	    case SCALE_ARROWANGLE_30 :
		dispatch_user_action (SCALE_ARROWANGLE, (double)deg_to_rad (30));
		break;
	    case SCALE_ARROWANGLE_45 :
		dispatch_user_action (SCALE_ARROWANGLE, (double)deg_to_rad (45));
		break;
	    case SCALE_ARROWANGLE_60 :
		dispatch_user_action (SCALE_ARROWANGLE, (double)deg_to_rad (60));
		break;
	    case SCALE_ARROWANGLE_AS_SELECTED :
		if (something_picked && picked_object->what == EDGE_PICKED)
			dispatch_user_action (SCALE_ARROWANGLE, picked_object->which.edge->arrow.angle);
		else
			bell ();
		break;
	
	
	    case SET_GRAGRA_TYPE :
		set_current_gragra_type (va_arg (args, Gragra_type));
		break;
		
	    case SET_GRAGRA_TYPE_NCE_1 :
		dispatch_user_action (SET_GRAGRA_TYPE, NCE_1);
		break;
	    case SET_GRAGRA_TYPE_ENCE_1 :
		dispatch_user_action (SET_GRAGRA_TYPE, ENCE_1);
		break;
	    case SET_GRAGRA_TYPE_NLC :
		dispatch_user_action (SET_GRAGRA_TYPE, NLC);
		break;
	    case SET_GRAGRA_TYPE_BNLC :
		dispatch_user_action (SET_GRAGRA_TYPE, BNLC);
		break;
	
	
	    case SET_GRID :
		x = va_arg (args, int);
		show_grid (x);
		break;
	
	    case SET_GRID_8_8 :
		dispatch_user_action (SET_GRID, 8);
		break;
	    case SET_GRID_16_16 :
		dispatch_user_action (SET_GRID, 16);
		break;
	    case SET_GRID_32_32 :
		dispatch_user_action (SET_GRID, 32);
		break;
	    case SET_GRID_64_64 :
		dispatch_user_action (SET_GRID, 64);
		break;
	    case SET_GRID_128_128 :
		dispatch_user_action (SET_GRID, 128);
		break;
	    case SET_GRID_OFF :
		dispatch_user_action (SET_GRID, 0);
		break;

	
	    case PRINT :
		show_print_subframe ();
		break;
		
	
	    case REDRAW_ALL :
		redraw_all ();
		break;


	    case SAVE_STATE :
		save_state ();
		break;
	
	
	    case EXPAND_WORKING_AREA :
		set_working_area_size (
			(int)xv_get(canvases[buffer].canvas, CANVAS_WIDTH)  * 2,
			(int)xv_get(canvases[buffer].canvas, CANVAS_HEIGHT) * 2);
		break;
	
	    case SHRINK_WORKING_AREA :
		set_working_area_size (
			(int)xv_get(canvases[buffer].canvas, CANVAS_WIDTH)  / 2,
			(int)xv_get(canvases[buffer].canvas, CANVAS_HEIGHT) / 2);
		break;


	    case SET_CONSTRAINED :
		constrain_is_active = TRUE;
		install_constrained_in_menu (TRUE);
		set_base_frame_label (FRAME_LABEL_CONSTRAINED, TRUE);
		break;

	    case RESET_CONSTRAINED :
		constrain_is_active = FALSE;
		install_constrained_in_menu (FALSE);
		set_base_frame_label (FRAME_LABEL_CONSTRAINED, FALSE);
		break;

	    case TOGGLE_CONSTRAINED :
		if (constrain_is_active)
			dispatch_user_action (RESET_CONSTRAINED);
		else
			dispatch_user_action (SET_CONSTRAINED);
		break;


	    case GROUP_LABELLING_OPERATION_GOES_TO_NODE :
		group_labelling_operation_goes_to = NODE;
		install_group_labelling_operation_in_menu (NODE);
		set_base_frame_label (FRAME_LABEL_GROUP_LABELLING_OPERATION, NODE);
		break;

	    case GROUP_LABELLING_OPERATION_GOES_TO_EDGE :
		group_labelling_operation_goes_to = EDGE;
		install_group_labelling_operation_in_menu (EDGE);
		set_base_frame_label (FRAME_LABEL_GROUP_LABELLING_OPERATION, EDGE);
		break;

	    case TOGGLE_GROUP_LABELLING_OPERATION :
		if (group_labelling_operation_goes_to == NODE)
			dispatch_user_action (GROUP_LABELLING_OPERATION_GOES_TO_EDGE);
		else
			dispatch_user_action (GROUP_LABELLING_OPERATION_GOES_TO_NODE);
		break;


	    case SET_NODEFONT :
		set_current_nodefont (va_arg (args, int));
		break;
		
	    case SET_EDGEFONT :
		set_current_edgefont (va_arg (args, int));
		break;
		
	    case SET_NODETYPE :
		set_current_nodetype (va_arg (args, int));
		break;
		
	    case SET_EDGETYPE :
		set_current_edgetype (va_arg (args, int));
		break;
		
	    case SET_NODECOLOR :
		set_current_nodecolor (va_arg (args, int));
		break;
		
	    case SET_EDGECOLOR :
		set_current_edgecolor (va_arg (args, int));
		break;
	
	
	    case EDIT_NODETYPES :
		if (test_user_interface_locked ())
			break;
		else
			show_type_edit_subframe (NODE);
		break;
		
	    case EDIT_EDGETYPES :
		if (test_user_interface_locked ())
			break;
		else
			show_type_edit_subframe (EDGE);
		break;
	
	
	    case EDIT_NODEFONTS :
		if (test_user_interface_locked ())
			break;
		else
			show_font_edit_subframe (NODE);
		break;
		
	    case EDIT_EDGEFONTS :
		if (test_user_interface_locked ())
			break;
		else
			show_font_edit_subframe (EDGE);
		break;
	
	
	    case ABOUT_GRAPHED :
		show_about_subframe ();
		break;
	
	
	    case NO_ACTION :		
	    default : /* NO_ACTION */
		break;
	}
	
	va_end (args);
	
	return dispatcher_result;
}



static	int	nothing_edited ()
{
	return	get_currently_edited_node()  == empty_node &&
		get_currently_edited_edge()  == empty_edge &&
		get_currently_edited_group() == empty_group;
}



void	clean_buffer (buffer)
int	buffer;
{
	int	number_of_graphs_in_buffer;	/* (original) number of	*/
						/* graphs in the buffer	*/
	int	i;	/* counts the number of graphs processes so far	*/
	Graph	graph, next_graph;
	
	number_of_graphs_in_buffer = 0;
	for_all_graphs (buffer, graph) {
		number_of_graphs_in_buffer ++;
	} end_for_all_graphs (buffer, graph);
	
	graph = buffers[wac_buffer].graphs;
	for (i=0; i < number_of_graphs_in_buffer; i++) {
	
		next_graph = graph->suc;
				
		if (graph->firstnode == empty_node) {
			erase_and_delete_graph (graph);
			if (last.graph == graph) {
				if (buffers[wac_buffer].graphs      != empty_graph &&
				    buffers[wac_buffer].graphs->suc == buffers[wac_buffer].graphs)
					last.graph = buffers[wac_buffer].graphs;
				else
					last.graph = empty_graph;
				last.node = empty_node;
				last.edge = empty_edge;
			}
			if (current_production == graph)
				current_production = empty_graph;
		}
		graph = next_graph;
	}

}



static	void			set_node_defaults (info)
Node_defaults_subframe_info	*info;
{
	set_current_node_edge_interface (info->attr.node_edge_interface);
	set_current_nodelabel_placement (info->attr.nodelabel_placement);
	set_current_nodelabel_visibility (info->attr.label_visibility);
	set_current_nodesize (info->attr.width, info->attr.height);
	set_current_nodetype (info->attr.type_index);
	set_current_nodefont (info->attr.font_index);
	set_current_nodecolor (info->attr.color);
}



static	void			set_edge_defaults (info)
Edge_defaults_subframe_info	*info;
{
	set_current_edgelabel_visibility (info->attr.label_visibility);
	set_current_arrowlength (info->attr.arrow_length);
	set_current_arrowangle  (info->attr.arrow_angle);
	set_current_edgetype (info->attr.type_index);
	set_current_edgefont (info->attr.font_index);
	set_current_edgecolor (info->attr.color);
}
