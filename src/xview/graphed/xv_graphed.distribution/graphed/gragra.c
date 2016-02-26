/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				gragra.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	In diesem Modul befinden Sicht die Verwaltungsfunktionen	*/
/*	fuer die Graphgrammatik-spezifischen Teile der Graph-		*/
/*	Datensruktur.							*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "group.h"
#include "find.h"
#include <ctype.h>

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	compile_production	(prod);				*/
/*	int	apply_production	(prod, node);			*/
/*									*/
/*	void	free_embedding		(embed);			*/
/*	int	size_of_embedding	(embed);			*/
/*									*/
/*	int 	graph_is_global_embedding_to( embed, prod )		*/
/*	int	node_is_nonterminal( node )				*/
/*	int	node_is_terminal   ( node )				*/
/*	int	embed_match_node (node, match)				*/
/*									*/
/*	int 	production_is_nlc( prod )				*/
/*	int	additional_bnlc_test( prod )				*/
/*									*/
/*	void 	make_global_embedding (type, node, use_prod, embed_prod)*/
/*	void 	make_local_embedding  (type, node, use_prod )		*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/

struct picklist	*compile_production_error_list,
		*NLC_test_error_list;
char		*compile_production_error_message,
		*NLC_test_error_message;


/************************************************************************

		GRAPHED'S GRAPH GRAMMAR DATA STRUCTURE

GraphEd has (currently) no special data structure for graph grammars.
Instead, some graphs are marked as graph grammar productions. These graphs
DESCRIBE graph grammar productions. One node is the left side, some nodes
are the right side (drawn inside the left side), all the other nodes describe
the embedding (thus called the embedding nodes).

Each such graph has a filed gra of type Gragra_prod. Gragra_prod is defined as

typedef	struct	gragra	{
	Gragra_type	type;
	union {
		Nce_1_gragra	nce1;
	} gra;
}
	Gragra_prod;

where type is

typedef	enum {
	NCE_1,
	NLC,
	BNLC,
	ENCE_1,
	NUMBER_OF_GRAGRA_TYPES
}
	Gragra_type;


NCE1 graph grammars are defined as follows :

typedef	struct	nce1_gragra	{

	Group		right_side;
	Group		left_side;	(contains only a single node)
	Embedding	embed_in, embed_out;
}
	Nce_1_gragra;

typedef	struct	embed	{
	Node	right_side;	(the node in the right side)
	Group	embed;		(all embedding nodes connected to the node <<right_side>>)
}
	*Embedding;

embed_in and embed_out are pointers to arrays describing the embedding.
The last entry in these arrays has right_side == empty_node (there is a
function size_of_embedding compute the length of the embedding list).
embeddings in embed_in describe mbeddings going from the embedding nodes
into the right side, embed_out describe embeddings that go from the
right side to the embedding nodes.

************************************************************************/


/************************************************************************/
/*									*/
/*	some procedures to handle with variables of type Embedding	*/
/*									*/
/************************************************************************/


int		size_of_embedding (embed)
Embedding	embed;
{
	int	i=0;
	
	if (embed == empty_embedding) {
		return 0;
	} else {
		for (i=0; embed[i].right_side != empty_node; i++);
		return i;
	}
}


void		free_embedding (embed)
Embedding	embed;
{
	int	i;
	
	if (embed == empty_embedding)
		return;
	
	for (i=0; embed[i].right_side != empty_node; i++) {
		free_group (embed[i].embed);
	}
	myfree (embed);
	
	return;
}
/************************************************************************/
/*									*/
/*	functions to determine properties of nodes (by their labels)	*/
/*									*/
/************************************************************************/

int	node_is_nonterminal( node )
Node	node;
/* 	recently TRUE if first letter of the node's label is upper case	*/
{
	if( (node->label.text != NULL ) && isupper( node->label.text[0] ) ){
		return TRUE ;
	} else {
		return FALSE ;
	}
}


int	node_is_terminal( node )
Node	node;
/* 	recently TRUE if first letter of the node's label is lower case	*/
{
	if( (node->label.text != NULL ) && isupper( node->label.text[0] ) ){
		return FALSE ;
	} else {
		return TRUE ;
	}
}


int	embed_match_strings (in_production, match)
char	*in_production;
char	*match;
{
	int	result;
	
	if (in_production != NULL  && match != NULL && !strcmp (in_production, match)) {
		result = TRUE;
	} else if (in_production == NULL && match == NULL) {
		result = TRUE;
	} else if (get_gragra_always_match_empty()) {
		if (in_production == NULL || !strcmp (in_production, "")) {
			result = TRUE;
		} else {
			result = FALSE;
		}
	} else {
		result = FALSE;
	}
	
	return result;
}


int	embed_match_node (in_production, match)
Node	in_production, match;
{
	int	result;
	
	result = embed_match_strings (in_production->label.text, match->label.text);
	
	if (result) {
		unsigned embed_match_attribute = get_embed_match_attributes();
		if (embed_match_attribute & NODE_TYPE)
			result = result && in_production->type == match->type;
		if (embed_match_attribute & NODE_COLOR)
			result = result && in_production->color == match->color;
	}
	
	return result;
}


int	embed_match_edge (in_production, match)
Edge	in_production, match;
{
	int	result;
	
	if (in_production->label.text != NULL) {
		char	*relabel;
		char	*redirect;
		char	save;
		if ((relabel = strchr (in_production->label.text, '>')) != NULL) {
			save = *relabel; *relabel = '\0';
			result = embed_match_strings (in_production->label.text, match->label.text);
			*relabel = save;
		} else if ((redirect = strchr (in_production->label.text, '<')) != NULL) {
			save = *redirect; *redirect = '\0';
			result = embed_match_strings (in_production->label.text, match->label.text);
			*redirect = save;
		} else {
			result = embed_match_strings (in_production->label.text, match->label.text);
		}
	} else {
		result = embed_match_strings (in_production->label.text, match->label.text);
	}

	if (result) {
		unsigned embed_match_attribute = get_embed_match_attributes();
		if (embed_match_attribute & EDGE_TYPE)
			result = result && in_production->type == match->type;
		if (embed_match_attribute & EDGE_COLOR)
			result = result && in_production->color == match->color;
	}
		
	return result;
}


/************************************************************************/
/*									*/
/*	int 	graph_is_global_embedding_to( embed, prod )		*/
/*	Graph	embed, prod;						*/
/*									*/
/************************************************************************/
/*									*/
/*	returns TRUE if production 'embed' represents a global 		*/
/*	embedding of same type as prod 					*/
/*	(graph grammar type and directednes)				*/
/*	when called with (embed,embed) as parameters, the function	*/
/*	returns TRUE if embed is an embedding at all.			*/
/*	( no matter what kind of type it is )				*/
/*									*/
/************************************************************************/

int 	graph_is_global_embedding_to( embed, prod )
Graph	embed, prod;
{
	if( 	embed->is_production 					 &&
		prod->is_production					 &&
		(prod->gra.type == embed->gra.type) 			 &&
		(prod->directed == embed->directed)			 &&
		(embed->gra.gra.nce1.left_side->node->label.text != NULL) &&
		!strncmp (embed->gra.gra.nce1.left_side->node->label.text, "embedding", strlen("embedding") )){

	   	return TRUE ;
	} else {
		return FALSE ;
	}
}

/************************************************************************/
/*									*/
/*	int	production_is_correct_global_embedding( prod )		*/
/*	Graph	prod;							*/
/*									*/
/*	returns TRUE if there are no edges betweeen two			*/
/*	right side nodes.						*/
/*	(normally used after 'graph_is_global_embedding_to(prod,prod)')	*/
/*									*/
/*									*/
/*	REMEMBER : production must be compiled before testing !		*/
/*									*/
/************************************************************************/

int	production_is_correct_global_embedding( prod )
Graph	prod;
{
	Group 	g, errorgroup = empty_group;
	Edge	e;
	int	success = TRUE;

	for_group ( prod->gra.gra.nce1.right_side, g) {
		for_edge_targetlist (g->node, e) {
			if (contains_group_node (prod->gra.gra.nce1.right_side, e->source)) {
				compile_production_error_list = new_picklist( EDGE_PICKED, e );
				success = FALSE;
				goto exit;
			}
		} end_for_edge_targetlist (g->node, e);
	} end_for_group ( prod->gra.gra.nce1.right_side, g);
exit:
	return success;
}
	

/************************************************************************/
/*									*/
/*	int 	production_is_nlc( prod )				*/
/*	Graph 	prod;							*/
/*									*/
/*	returns TRUE if production 'prod' fits nlc-rules		*/
/*	(no local embedding if prod is production)			*/
/*									*/
/*	REMEMBER : production must be compiled before testing !		*/
/*									*/
/************************************************************************/

int 	production_is_nlc( prod )
Graph	prod;
{
	int 	embed_size;
	int	success = TRUE;
	int	i;
	Group	g, errorgroup = empty_group;
	Edge	e;
	
	if( !graph_is_global_embedding_to( prod,prod ) ) {
		embed_size = size_of_embedding( prod->gra.gra.nce1.embed_in );
		for( i=0; i<embed_size; i++ ){
			if( prod->gra.gra.nce1.embed_in[i].embed != empty_group ){
				success = FALSE;
				for_group( prod->gra.gra.nce1.embed_in[i].embed, g) {
					errorgroup = add_to_group( errorgroup, g->node );
				} end_for_group( prod->gra.gra.nce1.embed_in[i].embed, g);
			}
		}
		embed_size = size_of_embedding( prod->gra.gra.nce1.embed_out );
		for( i=0; i<embed_size; i++ ){
			if( prod->gra.gra.nce1.embed_out[i].embed != empty_group ){
				success = FALSE;
				for_group( prod->gra.gra.nce1.embed_out[i].embed, g) {
					errorgroup = add_to_group( errorgroup, g->node );
				} end_for_group( prod->gra.gra.nce1.embed_out[i].embed, g);
			}
		}
	}
	if( !success ) compile_production_error_list = new_picklist( GROUP_PICKED, errorgroup );
	return success;
}

/************************************************************************/
/*									*/
/*	int	additional_bnlc_test( prod )				*/
/*	Graph	prod;							*/
/*									*/
/************************************************************************/
/*									*/
/*	This function only makes sense when being called after		*/
/*	production_is_nlc(...).						*/
/*	returns TRUE if production 'prod' fits bnlc-rules		*/
/*	( no edge between two nonterminal nodes )			*/
/*									*/
/*	REMEMBER : production must be compiled before testing !		*/
/*									*/
/************************************************************************/

int	additional_bnlc_test( prod )
Graph	prod;
{
	int 	success = TRUE;
	Group 	g, errorgroup = empty_group;
	Edge 	e;
	
	for_group ( prod->gra.gra.nce1.right_side, g) {
		for_edge_targetlist (g->node, e) {
			if ( node_is_nonterminal(e->source) &&
			     node_is_nonterminal(e->target)     ) {
				success = FALSE;
				compile_production_error_list = new_picklist( EDGE_PICKED, e );
				goto exit;
			}
		} end_for_edge_targetlist (g->node, e);
		for_edge_sourcelist (g->node, e) {
			if ( node_is_nonterminal(e->source) &&
			     node_is_nonterminal(e->target)     ) {
				success = FALSE;
				compile_production_error_list = new_picklist( EDGE_PICKED, e );
				goto exit;
			}
		} end_for_edge_sourcelist (g->node, e);
	} end_for_group ( prod->gra.gra.nce1.right_side, g);
exit:
	return success;
}

/************************************************************************/
/*									*/
/*			PRODUKTION KOMPILIEREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	compile_production (prod)				*/
/*									*/
/*	Returns FALSE and sets negative prod->compile_time upon		*/
/*	unsuccessful compilation. 					*/
/*									*/
/************************************************************************/


int	compile_production (prod)
Graph	prod;
{
	Node	node;
	Edge	edge;
	Group	g;
	int	i;
	
	Group	left_side,	/* The production's left side		*/
		right_side,	/* The production's right side		*/
		embed_rules;	/* The nodes describing the embedding	*/
	Node	left_side_node;	/* Once again the left side		*/
	Rect	left_side_rect;	/* The rectangle of left_side_node	*/
	
	int		number_of_in_embeddings, number_of_out_embeddings;
	Embedding	embed_in  = empty_embedding,
			embed_out = empty_embedding;
	Group		embed_in_group  = empty_group,
			embed_out_group = empty_group;

	int		error = FALSE;
	char		*errormsg = "";
	Picklist	errorlist;
	
	
	/* AND NOW IT BEGINS ...	*/
	
	
	if (!prod->is_production)
		return FALSE; /* Aetsch	*/
	
	
	/* At first, clean up, assuming the programmer is lazy 		*/
	if (prod->gra.gra.nce1.left_side  != empty_group) free_group (prod->gra.gra.nce1.left_side);
	if (prod->gra.gra.nce1.right_side != empty_group) free_group (prod->gra.gra.nce1.right_side);
	if (prod->gra.gra.nce1.embed_in   != empty_embedding) free_embedding (prod->gra.gra.nce1.embed_in);
	if (prod->gra.gra.nce1.embed_out  != empty_embedding) free_embedding (prod->gra.gra.nce1.embed_out);
	prod->gra.gra.nce1.left_side  = empty_group;
	prod->gra.gra.nce1.right_side = empty_group;
	prod->gra.gra.nce1.embed_in   = empty_embedding;
	prod->gra.gra.nce1.embed_out  = empty_embedding;
	
	/* Set some Variables	*/
	left_side_node = prod->firstnode;
	left_side_rect = left_side_node->box;
	right_side  = empty_group;
	left_side   = empty_group;
	embed_rules = empty_group;

	for_nodes (prod, node) {
		if ((node->label.text == NULL || !strcmp (node->label.text, "")) && !get_gragra_always_match_empty()) {
			error    = TRUE;
			errormsg = "Unlabeled Node";
			errorlist = new_picklist (NODE_PICKED, node);
			goto exit;
		}
	} end_for_nodes (prod, node);

	/* At first, we scan prod to compute left_side,	right_side	*/
	/* and embed_rules.						*/
	left_side = new_group (left_side_node);
	for_nodes (prod, node) {
		if (node == left_side_node)
			continue;
		else if (rect_includesrect (&left_side_rect, &(node->box)))
			right_side = add_to_group (right_side, node);
		else if (!rect_intersectsrect (&left_side_rect, &(node->box)))
			embed_rules = add_to_group (embed_rules, node);
		else {
			error = TRUE;
			errormsg = "Unclassifiable Node";
			errorlist = new_picklist (NODE_PICKED, node);
			goto exit;
		}
	} end_for_nodes (prod, node);
	
	number_of_in_embeddings  = size_of_group (embed_rules);
	number_of_out_embeddings = size_of_group (right_side);
	embed_in  = (Embedding)mycalloc (number_of_out_embeddings+1,  sizeof (struct embed));
	embed_out = (Embedding)mycalloc (number_of_out_embeddings+1, sizeof (struct embed));
	
	
	/* Now construct embed_in and embed_out, the tables of embedding rules	*/
	
	i=0;
	for_group (right_side, g) {
	
		embed_in[i].right_side  = g->node;
		embed_out[i].right_side = g->node;
		
		embed_in_group  = empty_group;
		embed_out_group = empty_group;
		for_edge_targetlist (g->node, edge) {
			if (edge->source == left_side_node) {
				error = TRUE;
				errormsg = "Illegal embedding into left side";
				errorlist = new_picklist (EDGE_PICKED, edge);
				goto exit;
			} else if (contains_group_node (embed_rules, edge->source)) {
				embed_in_group = add_to_group (embed_in_group, edge->source);
				if (!prod->directed) {
					embed_out_group = add_to_group( embed_out_group, edge->source );
				}
			}
		} end_for_edge_targetlist (g->node, edge);
		
		for_edge_sourcelist (g->node, edge) {
			if (edge->target == left_side_node) {
				error = TRUE;
				errormsg = "Illegal embedding into left side";
				errorlist = new_picklist (EDGE_PICKED, edge);
				goto exit;
			} else if (contains_group_node (embed_rules, edge->target)) {
				embed_out_group = add_to_group (embed_out_group, edge->target);
				if (!prod->directed) {
					embed_in_group = add_to_group( embed_in_group, edge->target);
				}
			}
		} end_for_edge_sourcelist (g->node, edge);
		
		embed_in[i].embed  = embed_in_group;
		embed_out[i].embed = embed_out_group;
		i ++;
		
	} end_for_group (right_side, g);
	embed_in[i].right_side  = empty_node;	/* like C-strings, the end	*/
	embed_in[i].embed       = empty_group;	/* is indicated by a nil	*/
	embed_out[i].right_side = empty_node;
	embed_out[i].embed      = empty_group;
	
	for_group (embed_rules, g) {
		
		for_edge_targetlist (g->node, edge) {
			if (contains_group_node (embed_rules, edge->source)) {
				error = TRUE;
				errormsg = "Illegal edge in embedding rules";
				errorlist = new_picklist (EDGE_PICKED, edge);
				goto exit;
			} else if (edge->source == left_side_node) {
				error = TRUE;
				errormsg = "Illegal embedding into left side";
				errorlist = new_picklist (EDGE_PICKED, edge);
				goto exit;
			}
		} end_for_edge_targetlist (g->node, edge);
		
		for_edge_sourcelist (g->node, edge) {
			if (contains_group_node (embed_rules, edge->target)) {
				error = TRUE;
				errormsg = "Illegal edge in embedding rules";
				errorlist = new_picklist (EDGE_PICKED, edge);
				goto exit;
			} else if (edge->target == left_side_node) {
				error = TRUE;
				errormsg = "Illegal embedding into left side";
				errorlist = new_picklist (EDGE_PICKED, edge);
				goto exit;
			}
		} end_for_edge_sourcelist (g->node, edge);
	
	} end_for_group (embed_rules, g);

	
	/* O.K., its over. We just have to update the entrys in prod.	*/
	prod->gra.gra.nce1.left_side  = left_side;
	prod->gra.gra.nce1.right_side = right_side;
	prod->gra.gra.nce1.embed_in   = embed_in;
	prod->gra.gra.nce1.embed_out  = embed_out;

	if( graph_is_global_embedding_to( prod, prod) ) {
		if( !production_is_correct_global_embedding( prod ) ) {
			error = TRUE;
			errormsg = "Edge in a global rmbedding";
			errorlist = compile_production_error_list; /* was set in 'production_is_correct...' above */
			goto exit;
		}
	} else {
		if( !node_is_nonterminal( prod->gra.gra.nce1.left_side->node) ) {
			error = TRUE;
			errormsg = "Left side not a nonterminal";
			errorlist = new_picklist( NODE_PICKED, prod->gra.gra.nce1.left_side->node);
			goto exit;
		}
	}
	if( prod->gra.type == NLC || prod->gra.type == BNLC) {
		if( !production_is_nlc( prod ) ) {
			error = TRUE;
			errormsg = "Local embedding in (B)NLC";
			errorlist = compile_production_error_list; /* was set in 'production_is_nlc' above */
			goto exit;
		}
	} 
	if( prod->gra.type == BNLC ) {
		if( !additional_bnlc_test( prod ) ) {
			error = TRUE;
			errormsg = "Connected nonterminals in BNLC";
			errorlist = compile_production_error_list; /* was set in 'additional_bnlc_test' above */
			goto exit;
		}
	}


exit:	
	free_group (embed_rules);	/* Keep your RAM's clean !	*/
	
	if ( error) {
		free_group (right_side);
		free_group (left_side);
		if (embed_in  != empty_embedding) free_embedding (embed_in);
		if (embed_out != empty_embedding) free_embedding (embed_out);
		prod->gra.gra.nce1.left_side  = empty_group;
		prod->gra.gra.nce1.right_side = empty_group;
		prod->gra.gra.nce1.embed_in   = empty_embedding;
		prod->gra.gra.nce1.embed_out  = empty_embedding;
		compile_production_error_message = errormsg;
		compile_production_error_list    = errorlist;
		prod->compile_time = -(int)ticks();	/* Error	*/
		return FALSE;
	} else {
		compile_production_error_message = (char *)NULL;
		compile_production_error_list = (Picklist)NULL;	
		prod->compile_time = (int)ticks();	/* Successful	*/
		return TRUE;
	}
}


/************************************************************************/
/*									*/
/*	void 	make_global_embedding(type, node, use_prod, embed_prod)	*/
/*									*/
/*	Compute an embedding.						*/
/*	This procedure is used when 'use_prod' is applied to 'node'	*/
/*	with a global embedding 'embed_prod'. (usually (B)NLC grammars)	*/
/*	As said above, 'embed_prod' is global. That means that the	*/
/*	specified embedding rules are NODE LABEL CONTROLLED.		*/
/*									*/
/*	REMEMBER: use only in 'apply_production' !			*/
/*									*/
/************************************************************************/


#define TARGET_IN_RIGHT_SIDE 0
#define SOURCE_IN_RIGHT_SIDE 1

static	void	make_new_edge_from_embedding (in_production, type, derive_edge, where_are_the_endpoints, source_or_target)
Edge		in_production;
Gragra_type	type;
Edge		derive_edge;
int		where_are_the_endpoints;
Node		source_or_target; /* identity depends on where_are_the_endpoints */
{
	Node		source_iso_backup,
			target_iso_backup;
	Edge		copied_edge;
	Edgeline	el;
	int		switch_direction;
	
	source_iso_backup = derive_edge->source->iso;
	target_iso_backup = derive_edge->target->iso;
	
	switch_direction =
		type == ENCE_1 &&
		in_production->label.text != NULL &&
		strchr (in_production->label.text, '<') != NULL;
	
	if (where_are_the_endpoints == TARGET_IN_RIGHT_SIDE) {
		if (!switch_direction) {
			derive_edge->source->iso = derive_edge->source;
			derive_edge->target->iso = source_or_target->iso;
		} else {
			derive_edge->target->iso = derive_edge->source;
			derive_edge->source->iso = source_or_target->iso;
		}
	} else /* SOURCE_IN_RIGHT_SIDE */ {
		if (!switch_direction) {
			derive_edge->target->iso = derive_edge->target;
			derive_edge->source->iso = source_or_target->iso;
		} else {
			derive_edge->source->iso = derive_edge->target;
			derive_edge->target->iso = source_or_target->iso;
		}
	}
	copied_edge = copy_edge_without_line (empty_graph, derive_edge);
	el = new_edgeline(node_x(derive_edge->source->iso), node_y(derive_edge->source->iso));
	(void) add_to_edgeline(el, node_x(derive_edge->target->iso), node_y(derive_edge->target->iso));
	
	if (type == ENCE_1) {
		char	*relabel, *redirect, save;
		if (in_production->label.text != NULL) {
			if ((relabel = strchr (in_production->label.text, '>')) != NULL) {
				save = *relabel; *relabel = '\0';
				edge_set (copied_edge, EDGE_LINE, el, EDGE_LABEL, strsave (relabel+1), 0);
				*relabel = save;
			} else if ((redirect = strchr (in_production->label.text, '<')) != NULL) {
				save = *redirect; *redirect = '\0';
				edge_set (copied_edge, EDGE_LINE, el, EDGE_LABEL, strsave (redirect+1), 0);
				*redirect = save;
			} else {
				edge_set (copied_edge, EDGE_LINE, el, EDGE_LABEL, strsave (in_production->label.text), 0);
			}
		} else {
			edge_set (copied_edge, EDGE_LINE, el, EDGE_LABEL, NULL, 0);
		}
	} else {
		edge_set (copied_edge, EDGE_LINE, el, 0);
	}
	
	derive_edge->source->iso = source_iso_backup;
	derive_edge->target->iso = target_iso_backup;
}


static void 	make_global_embedding (type, node, use_prod, embed_prod)
Gragra_type	type;
Node		node;
Graph		use_prod;
Graph		embed_prod;
{
	Node		right_side;
	int		number_of_in_embeddings, number_of_out_embeddings;
	int 		i;
	Edge		e, embed_edge;
	Group		g, n;
	Edgeline	el;

	/* Some variables to keep the code smaller and faster...	*/
	number_of_in_embeddings  = size_of_embedding (embed_prod->gra.gra.nce1.embed_in);
	number_of_out_embeddings = size_of_embedding (embed_prod->gra.gra.nce1.embed_out);
	
	/* Embed the edges ...	*/
	
	for_edge_targetlist( node,e ) {
	
	    for( i=0; i<number_of_in_embeddings; i++) {
		for_group (embed_prod->gra.gra.nce1.embed_in[i].embed, g) {
	      	            
		    switch (type) {
		    
			case ENCE_1 :
			    if (embed_match_node (g->node, e->source)) {
				right_side = embed_prod->gra.gra.nce1.embed_in[i].right_side;
				for_group (use_prod->gra.gra.nce1.right_side, n) {
				    if ( embed_match_node (right_side, n->node) ) {
					for_edge_targetlist (right_side, embed_edge) {
					    if (embed_match_edge (embed_edge, e) && embed_edge->source == g->node) {
						make_new_edge_from_embedding (embed_edge, type, e, TARGET_IN_RIGHT_SIDE, n->node);
					    }
					} end_for_edge_targetlist (right_side, embed_edge);
					if (!use_prod->directed) for_edge_sourcelist (right_side, embed_edge) {
					    if (embed_match_edge (embed_edge, e) && embed_edge->target == g->node) {
						make_new_edge_from_embedding (embed_edge, type, e, TARGET_IN_RIGHT_SIDE, n->node);
					    }
					} end_for_edge_sourcelist (right_side, embed_edge);
				    }
				} end_for_group (use_prod->gra.gra.nce1.right_side, n);
			    }
			    break;
			
			default :
			    if (embed_match_node (g->node, e->source)) {
				right_side = embed_prod->gra.gra.nce1.embed_in[i].right_side;
				for_group (use_prod->gra.gra.nce1.right_side, n) {
				    if( embed_match_node( right_side, n->node ) ) {
					make_new_edge_from_embedding (empty_edge, type, e, TARGET_IN_RIGHT_SIDE, n->node);
				    }
				} end_for_group( use_prod->gra.gra.nce1.right_side, n);
			    }
			    break;
	            
		    }
	         
		} end_for_group (embed_prod->gra.gra.nce1.embed_in[i].embed, g);
	    }
	   
	} end_for_edge_targetlist (node,e );


	for_edge_sourcelist( node,e ) {
	
	    for( i=0; i<number_of_out_embeddings; i++) {
		for_group (embed_prod->gra.gra.nce1.embed_out[i].embed, g) {

		    switch (type) {
		    
			case ENCE_1 :
			    if (embed_match_node (g->node, e->target)) {
				right_side = embed_prod->gra.gra.nce1.embed_in[i].right_side;
				for_group (use_prod->gra.gra.nce1.right_side, n) {
				    if (embed_match_node(right_side, n->node) ) {
					for_edge_sourcelist (right_side, embed_edge) {
					    if (embed_match_edge (embed_edge, e) && embed_edge->target == g->node) {
						make_new_edge_from_embedding (embed_edge, type, e, SOURCE_IN_RIGHT_SIDE, n->node);
					    }
					} end_for_edge_sourcelist (right_side, embed_edge);
					if (!use_prod->directed) for_edge_targetlist (right_side, embed_edge) {
					    if (embed_match_edge (embed_edge, e) && embed_edge->source == g->node) {
						make_new_edge_from_embedding (embed_edge, type, e, SOURCE_IN_RIGHT_SIDE, n->node);
					    }
					} end_for_edge_targetlist (right_side, embed_edge);
				    }
				} end_for_group (use_prod->gra.gra.nce1.right_side, n);
			    }
			    break;
			
			default :
			    if (embed_match_node (g->node, e->target)) {
				right_side = embed_prod->gra.gra.nce1.embed_out[i].right_side;
				for_group (use_prod->gra.gra.nce1.right_side, n) {
				    if( embed_match_node( right_side, n->node ) ) {
					make_new_edge_from_embedding (empty_edge, type, e, SOURCE_IN_RIGHT_SIDE, n->node);
				    }
				} end_for_group( use_prod->gra.gra.nce1.right_side, n);
			    }
			    break;
	            
		    }
		    
		}end_for_group (embed_prod->gra.gra.nce1.embed_out[i].embed, g);
	    }
	    
	} end_for_edge_sourcelist( node,e );
}

/************************************************************************/
/*									*/
/*	void 	make_local_embedding(type, node, use_prod )		*/
/*									*/
/*	Compute an embedding.						*/
/*	This procedure is used when 'use_prod' is applied to 'node'.	*/
/*	use_prod's embedding is used to compute a LOCAL, NODE CONTROL-	*/
/*	LED embedding. (usual case for NCE grammars)			*/
/*									*/
/*	REMEMBER: use only in 'apply_production' !			*/
/*									*/
/************************************************************************/


static void 	make_local_embedding(type, node, use_prod )
Gragra_type	type;
Node		node;
Graph		use_prod;
{
	int		number_of_in_embeddings, number_of_out_embeddings;
	int 		i;
	Edge		e, copied_edge, embed_edge;
	Group		g, n;

	/* Some variables to keep the code smaller and faster...	*/
	number_of_in_embeddings  = size_of_embedding (use_prod->gra.gra.nce1.embed_in);
	number_of_out_embeddings = size_of_embedding (use_prod->gra.gra.nce1.embed_out);
	
	/* Embed the edges ...	*/
	for_edge_targetlist( node,e ) {

	    for( i=0; i<number_of_in_embeddings; i++) {
		for_group (use_prod->gra.gra.nce1.embed_in[i].embed, g) {
		
		    switch (type) {
		    
			case ENCE_1 :
			    if ( embed_match_node(g->node, e->source) ) {
				for_edge_sourcelist (g->node, embed_edge) {
				    if (embed_match_edge (embed_edge, e) &&
				        embed_edge->target == use_prod->gra.gra.nce1.embed_in[i].right_side) {
				    	make_new_edge_from_embedding (embed_edge, type, e, TARGET_IN_RIGHT_SIDE, use_prod->gra.gra.nce1.embed_in[i].right_side);
				    }
				} end_for_edge_sourcelist (g->node, embed_edge);
				if (!use_prod->directed) for_edge_targetlist (g->node, embed_edge) {
				    if (embed_match_edge (embed_edge, e) &&
				        embed_edge->source == use_prod->gra.gra.nce1.embed_in[i].right_side) {
				    	make_new_edge_from_embedding (embed_edge, type, e, TARGET_IN_RIGHT_SIDE, use_prod->gra.gra.nce1.embed_in[i].right_side);
				    }
				} end_for_edge_targetlist (g->node, embed_edge);
			    }
			    break;
			
			default :
			    if (embed_match_node(g->node, e->source)) {
				make_new_edge_from_embedding (empty_edge, type, e, TARGET_IN_RIGHT_SIDE, use_prod->gra.gra.nce1.embed_in[i].right_side);
			    }
		    }
		    
		 } end_for_group (use_prod->gra.gra.nce1.embed_in[i].embed, g);
	    }
	} end_for_edge_targetlist( node,e );

	for_edge_sourcelist( node,e ) {
	
	    for (i=0; i<number_of_out_embeddings; i++) {
		for_group (use_prod->gra.gra.nce1.embed_out[i].embed, g) {
		
		    switch (type) {
		    
			case ENCE_1 :
			    if ( embed_match_node(g->node, e->target) ) {
				for_edge_targetlist (g->node, embed_edge) {
				    if (embed_match_edge (embed_edge, e) &&
				        embed_edge->source == use_prod->gra.gra.nce1.embed_in[i].right_side) {
				    	make_new_edge_from_embedding (embed_edge, type, e, SOURCE_IN_RIGHT_SIDE,use_prod->gra.gra.nce1.embed_out[i].right_side);
				    }
				} end_for_edge_targetlist (g->node, embed_edge);
				if (!use_prod->directed) for_edge_sourcelist (g->node, embed_edge) {
				    if (embed_match_edge (embed_edge, e) &&
				        embed_edge->target == use_prod->gra.gra.nce1.embed_in[i].right_side) {
				    	make_new_edge_from_embedding (embed_edge, type, e, SOURCE_IN_RIGHT_SIDE, use_prod->gra.gra.nce1.embed_out[i].right_side);
				    }
				} end_for_edge_sourcelist (g->node, embed_edge);
			    }
			    break;
			
			default :
			    if (embed_match_node(g->node, e->target)) {
				make_new_edge_from_embedding (empty_edge, type, e, SOURCE_IN_RIGHT_SIDE, use_prod->gra.gra.nce1.embed_out[i].right_side);
			    }
		    }
		    
		} end_for_group (use_prod->gra.gra.nce1.embed_out[i].embed, g);
	    }
	    
	} end_for_edge_sourcelist( node,e );
}



/************************************************************************/
/*									*/
/*			Produktion anwenden				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	apply_production (prod, node)				*/
/*									*/
/************************************************************************/


Group	apply_production (prod, node)
Graph	prod;
Node	node;
{
	Group	copy_of_right_side;
	Edge	e;
	Rect	r;
	Group	g;
	Graph	emb;
	int	i, x,y, dx,dy;
	
	
	if ( !prod->is_production                                        ||
	     (prod->compile_time < 0) /* ??? */                          ||
	     (prod->compile_time < prod->change_time)                    ||
	     
	     (prod->gra.gra.nce1.left_side == empty_group)               ||
	     !embed_match_node (prod->gra.gra.nce1.left_side->node, node)     ||
	     
	     (!node->graph->directed && prod->directed))
		return empty_group;
	
	
	
	/* At first, copy the right side into graph		*/
	copy_of_right_side = copy_group_to_graph (prod->gra.gra.nce1.right_side, node->graph);
	
	
	/* Embed the edges ...	*/
	make_local_embedding (prod->gra.type, node, prod);
	for (i=N_PASTE_BUFFERS; i<N_BUFFERS; i++) {
		for_all_graphs (i, emb) {
			if ( graph_is_global_embedding_to (emb, prod) ) {
				make_global_embedding (prod->gra.type, node, prod, emb);
			}
		} end_for_all_graphs( i, emb );
	}
	
	
	/* We are nearly ready --- the only remaining duty is to shift	*/
	/* the new nodes so that they are fitting.			*/
	
	/* compute the rect around all NODES in prod->gra.gra.nce1.right_side	*/
	/* compute_rect_around_group would include all edges, even	*/
	/* defining the embedding					*/
	r = rect_null;
	for_group (prod->gra.gra.nce1.right_side, g)
		r = rect_bounding (&r, &(g->node->box));
	end_for_group (prod->gra.gra.nce1.right_side, g);
	
	if (copy_of_right_side != empty_group) {
		x = rect_left(&r) + rect_width(&r)/2;
		y = rect_top(&r) + rect_height(&r)/2;
		dx = node_x(node) - x;
		dy = node_y(node) - y;
		group_set (copy_of_right_side, MOVE, dx,dy, 0);
	}

	
	/* Now, we can delete the old node	*/
	erase_and_delete_node (node);
	
	
	return copy_of_right_side;
}





void	pretty_print_production  (prod)
Graph	prod;
{
	Group	group, g;
	Node	node;
	Edge	edge;
	int	i, number_of_in_embeddings, number_of_out_embeddings;
	
	/* We assume that prod is successfully compiled	*/
	
	node_set (prod->gra.gra.nce1.left_side->node, ONLY_SET,
		SET_NODE_ATTRIBUTES (get_node_style (LEFT_SIDE_NODE_STYLE)),
		0);
	
	number_of_in_embeddings  = size_of_embedding (prod->gra.gra.nce1.embed_in);
	number_of_out_embeddings = size_of_embedding (prod->gra.gra.nce1.embed_out);
	
	for (i=0; i<number_of_in_embeddings; i++) {
		for_group (prod->gra.gra.nce1.embed_in[i].embed, g) {
			node_set (g->node, ONLY_SET,
				SET_NODE_ATTRIBUTES (get_node_style (EMBED_NODE_STYLE)),
				0);
			for_edge_sourcelist (g->node, edge) {
				edge_set (edge, ONLY_SET,
					SET_EDGE_ATTRIBUTES (get_edge_style (EMBED_EDGE_STYLE)),
					0);
			} end_for_edge_sourcelist (g->node, edge);
		} end_for_group (prod->gra.gra.nce1.embed_in[i].embed, g);
	}
	
	for (i=0; i<number_of_out_embeddings; i++) {
		for_group (prod->gra.gra.nce1.embed_out[i].embed, g) {
			node_set (g->node, ONLY_SET,
				SET_NODE_ATTRIBUTES (get_node_style (EMBED_NODE_STYLE)),
				0);
			for_edge_targetlist (g->node, edge) {
				edge_set (edge, ONLY_SET,
					SET_EDGE_ATTRIBUTES (get_edge_style (EMBED_EDGE_STYLE)),
					0);
			} end_for_edge_targetlist (g->node, edge);
		} end_for_group (prod->gra.gra.nce1.embed_out[i].embed, g);
	}
	
	group_set (group = make_group_of_graph (prod), RESTORE_IT, 0);
	myfree (group);
}


int		gragra_type_to_int (type)
Gragra_type	type;
{
	switch (type) {
	    case ENCE_1 : return 0;
	    case NCE_1  : return 1;
	    case NLC    : return 2;
	    case BNLC   : return 3;
	}
}


int	int_to_gragra_type (n)
int	n;
{
	switch (n) {
	    case 0 : return ENCE_1;
	    case 1 : return NCE_1;
	    case 2 : return NLC;
	    case 3 : return BNLC;
	}
}
