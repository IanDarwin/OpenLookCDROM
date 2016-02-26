/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef REPAINT_HEADER
#define REPAINT_HEADER

extern	Rectlist	global_repaint_rectlists [N_BUFFERS];
extern	Rectlist	global_erase_rectlists   [N_BUFFERS];
			/* in anderem Modul initialisiert	*/

extern	int		painting_enabled;

extern	void		repaint_canvas   ();
extern  void    	force_repainting ();  /*eingefuegt*/
extern  void     	redraw_all       ();  /*eingefuegt*/ 

extern	Pixrect 	*paint_graph_in_rect ();
extern	Pixrect		*get_pixrect_of_visible_window ();

extern	Pixrect		*background_pixrect;

#endif

