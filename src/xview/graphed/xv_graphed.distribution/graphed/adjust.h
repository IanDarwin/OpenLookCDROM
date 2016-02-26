/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef ADJUST_HEADER
#define	ADJUST_HEADER


extern	void	adjust_line_to_node           ();
extern	void	adjust_edgeline_to_node       ();
extern	void	adjust_to_box_node            ();
extern	void	adjust_to_elliptical_node     ();
extern	void	adjust_to_diamond_node        ();

extern	void	adjust_nodelabel_position     ();
extern	void	adjust_edgelabel_position     ();
extern	void	adjust_nodelabel_text_to_draw ();
extern	void	adjust_edgelabel_text_to_draw ();

extern	void	adjust_arrow_to_edge          ();

extern	void	adjust_edge_box               ();

extern	void	adjust_edge_head              ();
extern	void	adjust_edge_tail              ();
extern	void	adjust_all_edges              ();
extern  void    adjust_graph_box              ();

extern	struct	pr_subregion	compute_lines_subregion_size ();

typedef	enum {
	ADJUST_EDGELINE_HEAD,
	ADJUST_EDGELINE_TAIL,
	ADJUST_EDGELINE_HEAD_AND_TAIL
}
	Nei_adjust_mode;
	/* fuer adjust_line_to_node / adjust_edgeline_to_node : welche	*/
	/* Enden sollen angepasst werden ?				*/


extern	void	adjust_boxes_in_graph  ();
extern	void	adjust_boxes_in_group  ();

#endif


