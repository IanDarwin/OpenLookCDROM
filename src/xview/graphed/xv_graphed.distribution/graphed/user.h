/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	USER_HEADER
#define	USER_HEADER
/************************************************************************/
/*									*/
/*			BENUTZERINTERFACE				*/
/*									*/
/************************************************************************/

#include "dispatch_commands.h"


extern	void		working_area_event_proc      ();
extern	char		*dispatch_user_action        ();

extern	int		node_is_picked  ();
extern	int		edge_is_picked  ();
extern	int		graph_is_picked ();

extern	Node		get_picked_node   ();
extern	Edge		get_picked_edge   ();
extern	Graph		get_picked_graph  ();
extern	Graph		get_picked_or_only_existent_graph ();

extern	Rect		compute_rect_around_selection ();
extern	Rect		compute_rect_around_current_selection ();
extern	Rect		compute_rect_around_graph_of_current_selection ();

extern	Node		get_last_node  ();
extern	Edge		get_last_edge  ();
extern	Graph		get_last_graph ();

extern	void		set_last_node  ();
extern	void		set_last_edge  ();
extern	void		set_last_graph ();

extern	void		lock_user_interface   ();
extern	void		unlock_user_interface ();
extern	int		test_user_interface_locked ();

extern	void		init_extra_menu ();
extern	void		init_user_menu ();

extern	void		add_to_extra_menu ();
extern	void		add_to_user_menu ();
extern  int      	user_interface_check_destroy_buffer(); /*eingefuegt*/
extern  void    	init_user_interface();                 /*eingefuegt*/
extern  int	        set_user_event_proc();                 /*eingefuegt*/
extern  int     	remove_user_event_proc();              /*eingefuegt*/    
 
typedef	enum	{
	MENU_CALLED_FROM_CANVAS,
	MENU_CALLED_FROM_MENUBAR,
	MENU_CALLED_FROM_NOWHERE
}
	Menu_called_from;

extern	Menu_called_from menu_called_from;

#endif
