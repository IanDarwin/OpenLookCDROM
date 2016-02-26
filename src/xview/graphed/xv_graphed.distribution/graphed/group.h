/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	GROUP_HEADER
#define GROUP_HEADER
	
extern  int	group_nodes_are_all_of_same_graph ();

extern	Group	new_group		 ();
extern	Group	add_to_group		 ();
extern	Group	add_immediately_to_group ();
extern	Group	subtract_from_group	 ();
extern	Group	subtract_immediately_from_group ();
extern	Group	add_groups		 ();
extern	Group	add_groups_disjoint	 ();
extern	Group	subtract_groups		 ();
extern	void	free_group		 ();
extern	Group	copy_group		 ();

extern	void	group_set ();


extern	int	group_contains_exactly_one_node	();
extern	Group	contains_group_node		();
extern	int	contains_group_graph		();
extern	int	intersects_group_graph		();
extern	int	group_intersects_group		();

extern	Rect	compute_rect_around_group       ();
extern	int	size_of_group			();


extern	Group	make_group_of_graph		();
extern	Group	make_group_of_all		();
extern	Group	copy_group_to_graph		();
extern	Group	make_group_of_sourcelist	();
extern	Group	make_group_of_targetlist	();

extern	Graph	make_graph_of_group		();

extern   void	move_group                      ();
#endif
