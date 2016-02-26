/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************************

	File :	main.c
	Date :  29.05.90

	This file is the main file of the planarity test

	Usuage:	planar <fname> 
		

*****************************************************************************/


/*	includes	*****************************************************/
#include <std.h>
#include <sgraph.h>
#include <slist.h>
#include <graphed.h>

#include <xview/xview.h>


extern void   make_Work_sgraph();
extern Sgraph Work_sgraph;


void             woods (Work_sgraph, info)
Sgraph           Work_sgraph;
Sgraph_proc_info info;
{
	Snode        s, t;
	Sedge        e;

	if (info->sgraph) {
	
		s = first_node_in_graph(Work_sgraph);
		e = s -> slist;
		if (!e) {
			e = s -> tlist;
			t = e -> snode;
		} else
			t = e -> tnode;
			
		if(!undirected_st_number (Work_sgraph,s,t )) {
			error ("cannot do an ST-numbering.\n");
			return;
		}


		if (planar(Work_sgraph)) {
/*			message("The graph is planar.\n"); */
			from_uwe_to_helmut(Work_sgraph,info->sgraph);
			woods_drawing(info->sgraph);
			message("Computation finished\n");
		} else {
			message("The graph is not planar.\n");
		}
	} else {
		; /* ist schon gut */
	}
}

void		woods_draw (info)
Sgraph_proc_info info;
{
	if (info->sgraph == empty_sgraph || info->sgraph->nodes == empty_node) {
	
		warning ("empty graph\n");
		
	} else if (info->sgraph->nodes == info->sgraph->nodes->suc) {
	
		; /* only one node -- currently handled incorrect by planarity test */
	
	} else if (!test_sgraph_biconnected (info->sgraph)) {
	
		error ("Graph is not biconnected\n"); 
		return;
		
	} else {
	
		make_Work_sgraph(info);
		woods(Work_sgraph,info);
		remove_Work_sgraph();
	
		info->repaint   = TRUE;
		info->recenter  = TRUE;
		info->recompute = TRUE;
	}
}



char *woods_menu_callback_proc(menu,menu_item)
char *menu, *menu_item;
{
	Event	*event;
	
	save_woods_settings ();
	
	event = (Event*)menu_get (menu, MENU_FIRST_EVENT);
	if (event_ctrl_is_down (event)) {
		show_woods_subframe ();
	} else {
		return call_sgraph_proc (woods_draw);
	}
	
	return(0);
}

/*
void init_user_menu()
{ add_to_user_menu("Planar Drawing - Woods",woods_menu_callback_proc);
}
*/


