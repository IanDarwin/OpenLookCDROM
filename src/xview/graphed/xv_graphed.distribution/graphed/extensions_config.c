/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/

#include "extensions_config.h"

#include "sgraph/std.h"
#include "sgraph/sgraph.h"
#include "sgraph/slist.h"
#include "sgraph/graphed.h"
#include "sgraph/algorithms.h"
#include "graphed_sgraph_interface.h"

#include <xview/xview.h>

static	void	null_proc (menu, menu_item)
Menu		menu;		/* The menu from which it is called	*/
Menu_item	menu_item;	/* The menu item from ...		*/
{
	Event	*event;
	event = (Event*)menu_get (menu, MENU_FIRST_EVENT);
	if (event_shift_is_down (event))
		message ("event_shift_is_down\n");
}

static	void	null_null_proc (menu, menu_item)
Menu		menu;		/* The menu from which it is called	*/
Menu_item	menu_item;	/* The menu item from ...		*/
{
	int	x = 223;
	int	y = 177;
	int 	z = 8;
	int	i;
	
	Pixrect	*pr = mem_create (x,y,z);
	
	for (i=0; i<1000; i++) {
		paint_nodetype_on_pr (
			get_current_nodetype(),
			pr,
			x,y,
			3);
	}
	
	pr_destroy (pr);
}

void	init_extra_menu ()
{
	extern	char	*menu_layout_reingold_tilford   ();
	extern	char	*menu_sugiyama_layout           ();
	extern	char	*menu_check_connectivity        ();
	extern	char	*menu_check_biconnectivity      ();
	extern	char	*menu_max_clique                ();

	extern	char	*tree_layout_walker_menu_callback_proc();

	extern	char	*termgraph_menu_callback_proc_1();

	extern  char	*planarity_test_call();
	extern  char	*Dual_aufruf();
	extern  char	*embedding_test_call();
	extern	char	*nature_menu_callback_proc();
	extern	char	*fast_nature_menu_callback_proc();
	
	extern	char	*call_menu_check_connectivity ();
	extern	char	*call_menu_check_strong_biconnectivity ();
	extern	char	*call_menu_check_biconnectivity ();
	extern	char	*call_menu_check_strong_connectivity ();

	extern	char	*cp_window_callback_proc();	
	extern	char	*DrawConvexStructur_menu_callback_proc();	
	extern	char	*DrawConvexEditable_menu_callback_proc();
	extern	char	*convex_draw_menu_callback_proc ();
	extern	char	*planar_menu_test_proc();
	extern	char	*planar_menu_draw_proc();
	extern	char	*planar_menu_local_proc();
	extern	char	*woods_menu_callback_proc ();
	extern	char	*bends_menu_callback_proc ();
	
	extern	char	*menu_test_graph_is_drawn_planar();
	extern	char	*menu_test_find_non_straight_line_edge();
	extern	char	*menu_test_find_cycle_in_directed_graph();
	extern	char	*menu_remove_all_self_loops_in_graph();
	extern	char	*menu_remove_all_multiple_edges_in_graph();

	extern	char	*Springembed();

	extern	char	*set_on_grid_points();
	extern	char	*fit_node_to_text();
	
#ifdef MAX_CLIQUE_ALGORITHM
	add_to_tools_menu ("maximal clique", menu_max_clique);
#endif
#ifdef HT_PLANARITY_TEST_ALGORITHM
	add_to_tools_menu ("HT planarity test",planarity_test_call);
#endif
#ifdef CONNECTIVITY_TEST_ALGORITHMS
	add_to_tools_menu ("check (weak) connectivity", call_menu_check_connectivity);
	add_to_tools_menu ("check (weak) biconnectivity", call_menu_check_biconnectivity);
	add_to_tools_menu ("check strong connectivity", call_menu_check_strong_connectivity);
	add_to_tools_menu ("check strong biconnectivity", call_menu_check_strong_biconnectivity);
	add_to_tools_menu ("---------------------------", null_proc);
#endif
#ifdef PQ_PLANARITY_TEST_ALGORITHM
	add_to_tools_menu ("PQ planarity test",planar_menu_test_proc);
#endif
#ifdef TREE_LAYOUT_ALGORITHM
        add_to_layout_menu ("tree layout", tree_layout_walker_menu_callback_proc); 
#endif
#ifdef SUGIYAMA_LAYOUT_ALGORITHM
	add_to_layout_menu ("sugiyma layout", menu_sugiyama_layout);
#endif
#ifdef SPRING_EMBEDDER_DRAWING_ALGORITHM
	add_to_layout_menu ("Spring Embedder", nature_menu_callback_proc);
#endif
#ifdef WOODS_DRAWING_ALGORITHM
	add_to_layout_menu ("Woods", woods_menu_callback_proc);
#endif
#ifdef DRAW_CONVEX_ALGORITHM
	add_to_layout_menu ("DrawConvex",convex_draw_menu_callback_proc) ;
#endif
#ifdef BENDS_DRAWING_ALGORITHM
	add_to_layout_menu ("Bends", bends_menu_callback_proc);
#endif
#ifdef CHROBAK_PAYNE_DRAWING_ALGORITHM
	add_to_layout_menu ("Chrobak-Payne", cp_window_callback_proc);
#endif
	add_to_layout_menu ("-----------------------", null_proc);
#ifdef SPRINGEMBEDDER_KAMADA
	add_to_layout_menu ("Springembedder (Kamada)", Springembed);
#endif
	add_to_layout_menu ("-----------------------", null_proc);
#ifdef SCHNIEDERS_DRAWING_ALGORITHMS
	add_to_layout_menu ("Schnieders global",planar_menu_draw_proc);
	add_to_layout_menu ("Schnieders local",planar_menu_local_proc);
#endif
#ifdef RT_TREE_DRAWING_ALGORITHM
	add_to_layout_menu ("tree layout - RT", menu_layout_reingold_tilford);
#endif
#ifdef HT_DUALGRAPH_ALGORITHM
	add_to_tools_menu ("HT dualgraph",Dual_aufruf);
#endif
#ifdef OLD_CONNECTIVITY_ALGORITHMS
	add_to_tools_menu ("check connectivity", menu_check_connectivity);
	add_to_tools_menu ("check biconnectivity", menu_check_biconnectivity);
#endif
	add_to_goodies_menu ("set on grid points", set_on_grid_points);
	add_to_goodies_menu ("fit node to text", fit_node_to_text);
	add_to_goodies_menu ("-------------------------", null_proc);
	add_to_goodies_menu ("check planar drawing", menu_test_graph_is_drawn_planar);
	add_to_goodies_menu ("check straight line edges", menu_test_find_non_straight_line_edge);
	add_to_goodies_menu ("find a directed cycle", menu_test_find_cycle_in_directed_graph);
	add_to_goodies_menu ("remove all self loops", menu_remove_all_self_loops_in_graph);
	add_to_goodies_menu ("remove all multiple edges", menu_remove_all_multiple_edges_in_graph);
	add_to_goodies_menu ("-------------------------", null_proc);
#ifdef TERMGRAPH
	add_to_goodies_menu ("Termgraph", termgraph_menu_callback_proc_1);
#endif
	/* synopsis :          <menu string>   , <procedure to call>	*/
	/* Add extra commands here					*/
}
