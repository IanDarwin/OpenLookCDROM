/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	FONT_HEADER
#define	FONT_HEADER

extern	int		insert_font ();
extern	int		add_font    ();
extern	int		delete_font ();

extern	Graphed_font	get_current_nodefont ();
extern	Graphed_font	get_current_edgefont ();

extern	Graphed_font	use_font       ();
extern	void		unuse_font     ();
extern	int		get_font_index ();
extern	int		find_font      ();

extern	void		init_fonts  ();
extern	void		write_fonts ();

#endif
