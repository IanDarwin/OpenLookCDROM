/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef DRAW_HEADER
#define DRAW_HEADER

extern	void	draw_node		();
extern	void	erase_node		();

extern	void	draw_nodelabel		();
extern	void	erase_nodelabel		();

extern	void	erase_and_delete_node	();

extern	void	draw_virtual_node	();
extern	void	erase_virtual_node	();

extern	void	do_mark_node		();
extern	void	do_unmark_node		();


extern	void	draw_edge		();
extern	void	erase_edge		();

extern	void	draw_edgelines		();
extern	void	erase_edgelines		();
extern	void	draw_single_edgeline	();
extern	void	erase_single_edgeline	();

extern	void	draw_edgelabel		();
extern	void	erase_edgelabel		();

extern	void	draw_arrow		();
extern	void	erase_arrow		();

extern	void	erase_and_delete_edge	();

extern	void	draw_edge_sourcelist	();
extern	void	draw_edge_targetlist	();
extern	void	erase_edge_sourcelist	();
extern	void	erase_edge_targetlist	();

extern	void	draw_virtual_line	();
extern	void	erase_virtual_line	();

extern	void	do_mark_edge		();
extern	void	do_unmark_edge		();

extern  void	draw_edge_head          (); 
extern  void	draw_edge_tail          (); 
extern  void	erase_edge_head         ();
extern  void	erase_edge_tail         ();  
extern  void	erase_edges_at_node     ();     
extern  void	draw_edges_at_node      ();
   
extern	void	draw_group		();
extern	void	erase_group		();
extern	void	erase_and_delete_group	();
extern	void	draw_virtual_group	();
extern	void	erase_virtual_group	();
extern	void	draw_virtual_group_box	();
extern	void	erase_virtual_group_box	();

extern	void	show_grid		();
extern	int	get_gridwidth		();

/*	Prozeduren aus repaint.c	*/

extern	void	force_repainting	();
extern	void	redraw_all		();

#endif
