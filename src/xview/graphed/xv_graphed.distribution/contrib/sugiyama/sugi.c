/* (C) Universitaet Passau 1986-1991 */
/*********************************************************************************/
/*                                                                               */
/*                            M  A  I  N  .  C                                   */
/*                                                                               */
/*********************************************************************************/


#include "def.h"

#include "sgraph/slist.h"
#include "sgraph/graphed.h"
#include "sgraph/algorithms.h"

#include <xview/xview.h>



/* Global int maxlevel = 0; */
Global int maxlevel = 1;	/* Set to 1 to correct possible but in */
				/* starting, MH 12/10/91               */
Global int nodes_of_level[SIZE];


Global bool	sugiyama (g, horizontal_distance, vertical_distance)
Sgraph		g;
int		horizontal_distance,	/* horizontaler Abstand zwischen je zwei Knoten	*/ 
		vertical_distance;	/* vertikaler Abstand zwischen je zwei Knoten	*/ 

{
	bool c;
	
	maxlevel = 2; /* Just a little workaround ... MH 12/10/91 */
	
	if (g != empty_graph)
	{
		c = find_cycles(g);
		if (c)
		{		
			error ("There are cycles in the graph\n");
			error ("No hierarchical layout possible\n");
			return FALSE;
		}
		else
		{
			int	i;
			int	max_nodes_of_level;

			prepare(g);
			make_hierarchy(g);

			if (maxlevel > SIZE) {
				error ("Graph is too large\n");
				return FALSE;
			}

			add_dummies(g);
			init_positions(g);

			max_nodes_of_level = 0;
			for (i=0; i<maxlevel; i++) {
				max_nodes_of_level = maximum (nodes_of_level[i], max_nodes_of_level);
			}
			if (max_nodes_of_level > SIZE) {
				error ("Graph is too wide\n");
				return FALSE;
			}

			reduce_crossings(g);
			improve_positions(g);
			set_horizontal_positions(g, horizontal_distance);
			set_vertical_positions(g, vertical_distance);
			remove_dummies(g);
			
			return TRUE;
		}
	} else {
		return	FALSE;
	}
}

/************************************************************************/
/*									*/
/*			    sugiyama layout				*/
/*									*/
/************************************************************************/


char			*sugiyama_layout (info)
Sgraph_proc_info	info;
{
	Sgraph	sgraph;
	int	successful;

	sgraph = info->sgraph;
	
	if (sgraph == empty_sgraph || sgraph->nodes == empty_node) {
	
		
	} else if (!sgraph->directed) {
	
		error ("Graph is not directed\n");
		
	} else {
		
		Snode	n;
		Sedge	e;

		info->recompute   = TRUE;
		info->no_changes  = TRUE;
		info->recenter    = TRUE;

		successful = sugiyama (sgraph,
			sugiyama_settings.horizontal_distance,
			sugiyama_settings.vertical_distance);

		if (successful) {
			
			int		*pos, i;
			Edgeline	el;
			int		successful;

			for_all_nodes (sgraph, n) {
	
				for_sourcelist (n, e) {
		
					el = (Edgeline)NULL;
					pos = (attr_data_of_type(e,int *));
					for (i=0; pos[i] != 0; i += 2)
						el = add_to_edgeline (el, pos[i], pos[i+1]);
					edge_set (graphed_edge(e), ONLY_SET, EDGE_LINE,
						el->suc, 0); /* el->suc is the start */
					myfree(attr_data(e));
			
	   			} end_for_sourcelist (n, e);
	   	 
				node_set (graphed_node(n), ONLY_SET, NODE_POSITION,
					n->x, n->y, 0);
			
			} end_for_all_nodes (sgraph, n);

		} else {
			test_find_cycle_in_directed_graph (graphed_graph(sgraph));
		}
	}
}



char		*menu_sugiyama_layout (menu, menu_item)
Menu		*menu;		/* The menu from which it is called	*/
Menu_item	*menu_item;	/* The menu item from ...		*/
{
	extern	char	*show_sugiyama_subframe ();
	
	if (event_ctrl_is_down ((Event*)menu_get (menu, MENU_FIRST_EVENT))) {
		show_sugiyama_subframe ();
	} else if (event_meta_is_down ((Event*)menu_get (menu, MENU_FIRST_EVENT))) {
		;
	} else {
		save_sugiyama_settings ();
		return call_sgraph_proc (sugiyama_layout);
	}
}



/*********** E n d e   S U G I . C ************************************************/


