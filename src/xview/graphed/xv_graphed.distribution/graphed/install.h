/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	INSTALL_HEADER
#define	INSTALL_HEADER

extern	void	install_fontlist_in_node_subframe      ();
extern	void	install_nodetypelist_in_node_subframe  ();
extern	void	install_fontlist_in_edge_subframe      ();
extern	void	install_edgetypelist_in_edge_subframe  ();

extern	void	install_fontlist_in_nodefont_selection ();
extern	void	install_fontlist_in_edgefont_selection ();
extern	void	install_current_nodefont_in_nodefont_selection ();
extern	void	install_current_edgefont_in_edgefont_selection ();

extern	void	install_nodetypelist_in_nodetype_selection ();
extern	void	install_edgetypelist_in_edgetype_selection ();
extern	void	install_current_nodetype_in_nodetype_selection ();
extern	void	install_current_edgetype_in_edgetype_selection ();

extern	void	install_node_edge_interface_in_menu ();
extern	void	install_nodelabel_placement_in_menu ();
extern	void	install_nodesize_in_menu            ();

extern	void	update_nodelabel_visibility_in_node_subframe ();

#endif
