/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	MENU_HEADER
#define	MENU_HEADER

extern	Menu	main_menu;
extern	void	create_working_area_menu            ();
extern	void	set_menu_selection                  ();
extern	void	activate_menu_item                  ();
extern	void	inactivate_menu_item                ();
extern	void	set_menu_string                     ();
extern  void	add_to_user_menu                    ();  /*eingefuegt*/
extern  void	add_to_extra_menu                   ();  /*eingefuegt*/
extern  void	install_gragra_type_in_menu         ();  /*eingefuegt*/
extern  void	install_directedness_in_menu        ();  /*eingefuegt*/
extern  void	install_group_labelling_operation_in_menu  (); /*eingefuegt*/
extern  void	install_constrained_in_menu                (); /*eingefuegt*/ 
extern  void	install_grid_in_menu                       (); /*eingefuegt*/
extern  void	install_edgelabel_visibility_in_menu       (); /*eingefuegt*/
extern  void	install_nodelabel_visibility_in_menu       (); /*eingefuegt*/
extern  void	install_arrowangle_in_menu                 (); /*eingefuegt*/
extern  void	install_arrowlength_in_menu                (); /*eingefuegt*/
extern  void	install_edgelabelsize_in_menu              (); /*eingefuegt*/
extern  void	install_nodesize_in_menu                   (); /*eingefuegt*/
extern  void	install_nodelabel_placement_in_menu        (); /*eingefuegt*/
extern  void	install_node_edge_interface_in_menu        (); /*eingefuegt*/

extern	Menu	create_submenu;
extern	Menu	edit_submenu;
extern	Menu	gragra_submenu;
extern	Menu	file_submenu;
extern	Menu	misc_submenu;
extern	Menu	tools_submenu;
extern	Menu	layout_submenu;
extern	Menu	goodies_submenu;
extern	Menu	user_submenu;
extern	Menu	about_submenu;

extern	Pixrect	*menu_create_separator ();

#endif
