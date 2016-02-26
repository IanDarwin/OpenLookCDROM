/* (C) Universitaet Passau 1986-1991 */

typedef	struct {
	int	vertical_separation;
	int	siblingseparation;
	int	subtreeseparation;
	int	size_defaults_x_sibling,
		size_defaults_x_subtree,
		size_defaults_y;
}
	Tree_settings;

extern	Tree_settings	tree_settings;
extern	void		save_tree_settings ();

extern	void	tree_layout_walker ();



