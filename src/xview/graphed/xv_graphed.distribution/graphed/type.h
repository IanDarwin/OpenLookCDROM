/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	TYPE_HEADER
#define	TYPE_HEADER

extern	int		insert_nodetype      ();
extern	int		insert_edgetype      ();
extern	int		delete_nodetype      ();
extern	int		delete_edgetype      ();

extern	Nodetype	get_current_nodetype ();
extern	Edgetype	get_current_edgetype ();

extern	Nodetype	use_nodetype        ();
extern	Edgetype	use_edgetype        ();
extern	void		unuse_nodetype      ();
extern	void		unuse_edgetype      ();
extern	Nodetypeimage	use_nodetypeimage   ();
extern	void		unuse_nodetypeimage ();

extern	int		get_nodetype_index  ();
extern	int		get_edgetype_index  ();
extern	int		find_nodetype       ();
extern	int		find_edgetype       ();
extern	Nodetype	get_nodetype	    ();
extern  int	        write_edgetypes     ();
extern  void	        install_current_edgetype    ();
extern  void    	install_current_nodetype    ();
extern  int     	add_edgetype                ();
extern  int	        add_nodetype                ();
 
extern	void		init_types ();
extern	int		save_types ();
#endif
