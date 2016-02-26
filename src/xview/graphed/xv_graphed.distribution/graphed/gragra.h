/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				Embeddings				*/
/*									*/
/************************************************************************/


typedef	struct	embed	{

	Node	right_side;
	Group	embed;
	
}
	*Embedding;


typedef	enum {
	NCE_1,
	NLC,
	BNLC,
	ENCE_1,
	NUMBER_OF_GRAGRA_TYPES
}
	Gragra_type;

int	gragra_type_to_int ();
int	int_to_gragra_type ();

extern	char	*gragra_type_strings[];
extern	char	*gragra_type_strings_for_cycle[];

typedef	struct	nce1_gragra	{

	Group		right_side;
	Group		left_side;
	Embedding	embed_in, embed_out;
}
	Nce_1_gragra;


typedef	struct	gragra	{
	
	Gragra_type	type;
	
	union {
		Nce_1_gragra	nce1;
	} gra;
}
	Gragra_prod;

#define	empty_embedding	((Embedding)NULL)


extern	int	compile_production ();
extern	int	size_of_embedding  ();
extern	void	free_embedding     ();

extern	int	node_is_nonterminal();
extern	int	node_is_terminal   ();
extern	int 	graph_is_global_embedding_to();

extern	struct picklist	*compile_production_error_list;
extern	char		*compile_production_error_message;

extern	Group	apply_production ();
