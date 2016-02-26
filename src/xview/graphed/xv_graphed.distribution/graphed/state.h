/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				state.h					*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*				Graph_state				*/
/*									*/
/*		Aktuelle Defaultwerte der Graphenattribute		*/
/*									*/
/************************************************************************/


typedef	struct	{
	Node_attributes		*node;
	Edge_attributes		*edge;
	
	int		max_edgelabel_width;
	int		max_edgelabel_height;
	int		directedness;
	
	Gragra_type	gragra_type;
	char		*gragra_terminals;
	char		*gragra_nonterminals;
	int		gragra_always_match_empty;
	
	int		always_pretty_print_productions;
	char		*global_embedding_name;
	unsigned	embed_match_attributes;
}
	Graph_state;


extern	Graph_state	graph_state;

/* Compatibility Macros */

#define	current_node_edge_interface  (get_current_node_edge_interface())
#define	current_nodelabel_placement  (get_current_nodelabel_placement())
#define	current_node_width           (get_current_node_width())
#define	current_node_height          (get_current_node_height())
#define	current_nodetype_index       (get_current_nodetype_index())
#define	current_edgetype_index       (get_current_edgetype_index())
#define	current_nodefont_index       (get_current_nodefont_index())
#define	current_edgefont_index       (get_current_edgefont_index())
#define	current_edgelabel_width      (get_current_edgelabel_width())
#define	current_edgelabel_height     (get_current_edgelabel_height())
#define	current_nodelabel_visibility (get_current_nodelabel_visibility())
#define	current_edgelabel_visibility (get_current_edgelabel_visibility())
#define current_arrow_length         (get_current_arrow_length())
#define current_arrow_angle          (get_current_arrow_angle())
#define	current_nodecolor            (get_current_nodecolor())
#define	current_edgecolor            (get_current_edgecolor())
#define current_directedness         (get_current_directedness())
#define current_gragra_type          (get_current_gragra_type())
#define current_gragra_terminals     (get_current_gragra_terminals())
#define current_gragra_nonterminals  (get_current_gragra_nonterminals())


extern	void		set_current_node_edge_interface  ();
extern	void		set_current_nodelabel_placement  ();
extern	void		set_current_nodesize             ();
extern	void		set_current_nodetype             ();
extern	void		set_current_edgetype             ();
extern	void		set_current_nodefont             ();
extern	void		set_current_edgefont             ();
extern	void		set_current_edgelabelsize        ();
extern	void		set_current_nodelabel_visibility ();
extern	void		set_current_edgelabel_visibility ();
extern	void		set_current_arrowlength          ();
extern	void		set_current_arrowangle           ();
extern	void		set_current_nodecolor            ();
extern	void		set_current_edgecolor            ();
extern	void		set_current_directedness         ();

extern	void		set_current_gragra_type          ();
extern	void		set_current_gragra_terminals     ();
extern	void		set_current_gragra_nonterminals  ();
extern	void		set_always_pretty_print_productions ();
extern	void		set_global_embedding_name        ();
extern	void		set_embed_match_attributes       ();
extern	void		set_gragra_always_match_empty    ();


extern	Node_edge_interface	get_current_node_edge_interface ();
extern	Nodelabel_placement	get_current_nodelabel_placement ();
extern	float			get_current_arrow_angle ();
extern	int			get_current_arrow_length ();
extern	int			get_current_node_width ();
extern	int			get_current_node_height ();
extern	int			get_current_nodefont_index ();
extern	int			get_current_edgefont_index ();
extern	int			get_current_nodetype_index ();
extern	int			get_current_edgetype_index ();
extern	int			get_current_nodelabel_visibility ();
extern	int			get_current_edgelabel_visibility ();
extern	int			get_current_nodecolor ();
extern	int			get_current_edgecolor ();
extern	int			get_current_directedness ();

extern	Gragra_type		get_current_gragra_type ();
extern	char			*get_current_gragra_terminals ();
extern	char			*get_current_gragra_nonterminals ();
extern	int			get_always_pretty_print_produtions ();
extern	char			*get_global_embedding_name ();
extern	unsigned		get_embed_match_attributes ();
extern	int			get_gragra_always_match_empty ();

/************************************************************************/
/*									*/
/*			NODE- AND EDGESTYLES				*/
/*									*/
/************************************************************************/

#define	NORMAL_NODE_STYLE	0
#define	LEFT_SIDE_NODE_STYLE	1
#define	EMBED_NODE_STYLE	2
#define	NUMBER_OF_NODE_STYLES	3
	
#define	NORMAL_EDGE_STYLE	0
#define	EMBED_EDGE_STYLE	1
#define	NUMBER_OF_EDGE_STYLES	2


extern	void		set_node_style ();
extern	void		set_edge_style ();

extern	Node_attributes	get_node_style ();
extern	Edge_attributes	get_edge_style ();


/************************************************************************/
/*			GRAPHED_STATE					*/
/************************************************************************/


typedef struct {
	int	startup;
	int	shutdown;
	int	loading;
	
	int	default_working_area_canvas_width,
		default_working_area_canvas_height,
		default_working_area_window_width,
		default_working_area_window_height;
	
	int	colorscreen;
}
	Graphed_state;

extern	Graphed_state	graphed_state;

