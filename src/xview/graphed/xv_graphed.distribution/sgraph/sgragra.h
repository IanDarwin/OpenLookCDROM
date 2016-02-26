/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : sgragra.h                                      Version 1.0 *
 * Aufgabe : Datenstruktur fuer Graphgrammatiken, aufbauend auf der	*
 *           Standard-Datenstruktur Sgraph fuer Graphen.                *
 * Autor   : Torsten Bachmann						*
 * Datum   : 18.12.90							*
 ************************************************************************/

#ifndef SGRAGRA_HEADER
#define SGRAGRA_HEADER



/************************************************************************
 *									*
 *			    alphabet-utilities				*
 *									*
 ************************************************************************/

extern	Slist	S_alphabet_append();
extern	Slist	S_alphabet_test  ();
extern	void	S_alphabet_remove();

#define Snode_label_is_terminal(sgragra, label) (S_alphabet_test((sgragra)->tv, (label)) != empty_slist)
#define Snode_label_is_nonterminal(sgragra, label) (S_alphabet_test((sgragra)->nv, (label)) != empty_slist)
#define Sedge_label_is_terminal(sgragra, label) (S_alphabet_test((sgragra)->te, (label)) != empty_slist)
#define Sedge_label_is_nonterminal(sgragra, label) (S_alphabet_test((sgragra)->ne, (label)) != empty_slist)


/************************************************************************
 *									*
 *				Sembed					*
 *									*
 ************************************************************************/

#define S_out	0
#define S_in	1

typedef struct sembed
{
	struct sembed	*pre, *suc;	/* Liste aller Regeln		*/
	struct sembed	*npre, *nsuc;	/* Einbettungen der Knoten	*/
	struct sprod	*prod;		/* "Vatergrammatik"		*/

	Snode		node_right;	/* Knoten der rechten Seite	*/
	char		*node_embed;	/* Markierung des Knotens aus	*/
					/* der Einbettung		*/

	char		*oldedge;	/* Markierung der alten Kante	*/
	int             olddir;		/* Richtung der alten Kanten	*/

	char		*newedge;	/* Markierung der neuen Kante	*/
	int		newdir;		/* Richtung der neuen Kante	*/

	Attributes	attrs;
}
	*Sembed;



#define	empty_sembed	((Sembed)NULL)

#define	for_all_sembeds(prod, emb) \
	{ if (((emb) = (prod)->embedding) != empty_sembed) do {
#define	end_for_all_sembeds(prod, emb) \
	} while (((emb) = (emb)->suc) != (prod)->embedding); }

#define	for_all_snode_sembeds(node, emb) \
	{ if (((emb) = (Sembed) node->embedding) \
	  != empty_sembed) do {
#define	end_for_all_snode_sembeds(node, emb) \
	} while (((emb) = (emb)->nsuc) != (Sembed) node->embedding); }
	
	
extern	Sembed	make_sembed	();
extern	void	remove_sembed	();


/************************************************************************
 *									*
 *			      Sglobalembed				*
 *									*
 ************************************************************************/

typedef struct sglobalembed
{	
	struct sglobalembed	*pre, *suc;	/* Liste aller Regeln	*/
	struct sgragra		*gragra;	/* "Vaterproduktion"	*/

	char		*node_right;
	char		*node_embed;

	char		*oldedge;
	int             olddir;	
	
	char		*newedge;
	int		newdir;

	Attributes	attrs;
}
	*Sglobalembed;



#define	empty_sglobalembed	((Sglobalembed)NULL)

#define	for_all_sglobalembeds(gragra, gemb) \
	{ if (((gemb) = (gragra)->global_embeddings) != empty_sglobalembed) do {
#define	end_for_all_sglobalembeds(gragra, gemb) \
	} while (((gemb) = (gemb)->suc) != (gragra)->global_embeddings); }

	
extern	Sglobalembed	make_sglobalembed	();
extern	void		remove_sglobalembed	();


/************************************************************************
 *									*
 *				Sprod					*
 *									*
 ************************************************************************/

typedef struct sprod
{	struct sprod	*pre, *suc;	/* Liste der Produktionen	*/
	struct sgragra	*gragra;        /* "Vatergrammatik"		*/

	char		*left;		/* linke Seite der Produktion	*/
	Sgraph		right;		/* rechte Seite			*/
	Sembed		embedding;	/* Einbettungsvorschrift	*/
	char		*label;
	Attributes	attrs;
}
	*Sprod;

#define	empty_sprod	((Sprod)NULL)

#define	for_all_sprods(gg, prod) \
	{ if (((prod) = (gg)->productions) != empty_sprod) do {
#define	end_for_all_sprods(gg, prod) \
	} while (((prod) = (prod)->suc) != (gg)->productions); }

extern	Sprod	make_sprod             ();
extern	void	remove_sprod           ();
extern	void	set_sprodlabel         ();


/************************************************************************
 *									*
 *				Sgragra					*
 *									*
 ************************************************************************/

typedef enum 
{	
	S_UNDEFINED		= -1,

	S_GG			=  0,	/* the lowest bit determines       */
	S_GG_UNDIRECTED		=  1,	/* the directedness of the grammar */
	
	S_NCE_1			=  2,
	S_NCE_1_UNDIRECTED	=  3,
	
	S_NLC			=  4,
	S_NLC_UNDIRECTED	=  5,
	
	S_BNLC			=  6,
	S_BNLC_UNDIRECTED	=  7,
	
	S_ENCE_1		=  8,
	S_ENCE_1_UNDIRECTED     =  9
	
}
	Sgragra_type;

typedef struct sgragra
{	Sgragra_type	class;		/* Grammatikklasse		*/
	Slist		tv, te;		/* Terminalzeichen		*/
	Slist		nv, ne;		/* Nichtterminalzeichen		*/
	Sprod		productions;	/* Liste der Produktionen	*/
	Sglobalembed	global_embeddings; 
	Snode		startnode;	/* Startknoten aus nv		*/
	char 		*label;
	Attributes	attrs;
}
	*Sgragra;

#define	first_prod_in_sgragra(g)	((g)->productions)
#define	last_prod_in_sgragra(g)		((g)->productions->pre)
#define	empty_sgragra			((Sgragra)NULL)

extern	Sgragra	make_sgragra     ();
extern	void	remove_sgragra   ();
extern	void	set_sgragralabel ();




#endif
