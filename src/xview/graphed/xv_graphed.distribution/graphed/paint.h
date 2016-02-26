/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	PAINT_HEADER
#define	PAINT_HEADER

extern	Rect	*set_clip_region               ();
extern	void	paint_node_internal            ();
extern	void	paint_edgelabel_internal       ();
extern	void	paint_single_edgeline_internal ();
extern  void	paint_dot_internal             (); /*eingefuegt*/ 
extern  void    paint_edgetype_on_pr           (); /*eingefuegt*/ 
extern  void    PR_line                        (); /*eingefuegt*/   
extern	void	paint_rectangle                ();
extern	void	paint_virtual_node             ();
extern	void	paint_line                     ();
extern	void	paint_marker_rect              ();
extern	void	paint_marker_square            ();
extern	void	paint_marker_rectangle         ();

extern	void	paint_nodetype_on_pr           ();
extern	void	pr_paint_box_node              ();
extern	void	pr_paint_elliptical_node       ();
extern	void	pr_paint_diamond_node          ();
extern	void	pw_paint_box_node              ();
extern	void	pw_paint_elliptical_node       ();
extern	void	pw_paint_diamond_node          ();

#define PIX_PAINT (PIX_SRC | PIX_DST)
#define PIX_ERASE (PIX_NOT(PIX_SRC) & PIX_DST)
#define PIX_XOR   (PIX_SRC ^ PIX_DST)

#define WHITE 0	/* WARNING : MUST BE CONSISTENT WITH	*/
#define BLACK 7	/* THE COLORMAP IN CANVAS.c !		*/

extern	Pixrect	*paint_pr;
#endif
