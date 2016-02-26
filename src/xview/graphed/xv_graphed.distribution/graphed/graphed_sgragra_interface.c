/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : graphed_sgragra_interface.c                                *
 * Aufgabe : Interface: GraphEd <-> Sgragra				*
 *									*
 * Author  : Torsten Bachmann                                           *
 * Changed : 17.12.90                                                   *
 ************************************************************************/


/*
Known bugs:

- Mehrfachaufruf von init_sgragra_from_graphed_gragra ueberpruefen (sgraph-
  pointer in graphed werden ueberschrieben).
- Pruefen, ob sgraph_node in Graphedknoten schon von Sgraph belegt (siehe oben).
- prettyprint all auf c.gg, dann user/printgrammar => core dump.

*/




#include "misc.h"
#include "graph.h"		/* includes gragra.h */
#include "group.h"
#include "user_header.h"

#include "sgraph/std.h"
#include "sgraph/slist.h"
#include "sgraph/sgraph.h"
#include "sgraph/sgragra.h"
#include "sgraph/graphed.h"


/************************************************************************/
/*			     local functions				*/
/************************************************************************/

/*
static
int	test_wac_has_a_production (canvas_buffer_number)
int	canvas_buffer_number;
	/ *
	Tests, whether there exists at least one production in the working
	area.
	* /
{
	Graph g;
	for_all_graphs(canvas_buffer_number, g)
	{	
		if (g->is_production)
			return true;
	} end_for_all_graphs(canvas_buffer_number, g);
	return false;
}
*/

static
void	clear_info (info)
Sgragra_proc_info info;
	/*
	Sets all fields from info to "undefined"
	*/
{
	info->sgragra = empty_sgragra;
	info->current_production = empty_sprod;
	info->create_mode = SGG_UNDEFINED;
}

/************************************************************************/
/*			handle right side of production			*/
/************************************************************************/

static
Sgraph	convert_group_to_sgraph(sgragra, group)
Sgragra	sgragra;
Group	group;
	/*
	Converts the graph, described through the nodes in group, into a Sgraph-
	structure. As a sideeffect labels from nodes and edges are inserted
	into the alphabetlists from sgragra. Also in GraphEd nodes (of the right
	side of the production) the pointer sgraph_node is initialized.
	The function returns the Sgraph.
	*/
	{
		Sgraph sgraph;
		Group g;
		
		/* Create the Graph */
		sgraph = make_graph(make_attr(ATTR_DATA, NULL));
		sgraph->directed = group->node->graph->directed;
	
	
		/* Append the nodes */
		for_group(group, g)
		{
			Snode sn;
			Slist *alphabet;
			sn = make_node_with_number(sgraph, make_attr(ATTR_DATA, NULL), g->node->nr);
			sn->x = g->node->x;
			sn->y = g->node->y;
			sn->graphed = (char *) g->node;
			g->node->sgraph_node = (char *) sn;
			alphabet = node_is_terminal(g->node) ? &(sgragra->tv) : &(sgragra->nv);
			set_nodelabel(sn, attr_data(S_alphabet_append(alphabet, g->node->label.text)));
		} end_for_group(group, g);
	
		
		/* Calculate the edges */
		for_group(group, g)
		{	
			Edge e;
			for_edge_sourcelist(g->node,e)
			{
				if (contains_group_node(group, e->target))
				{
					Sedge se = make_edge(g->node->sgraph_node, e->target->sgraph_node, make_attr(ATTR_DATA, NULL));
					if (e->label.text != NULL)
						set_edgelabel(se, attr_data(S_alphabet_append(&(sgragra->ne), e->label.text)));
				}
			} end_for_edge_sourcelist(g->node,e);
		} end_for_group(group, g);
		
		return sgraph;			
	}


/************************************************************************/
/*			handle normal embeddings			*/
/************************************************************************/

static
void	convert_edge_to_embedding(edge, direction, sprod)
Edge	edge;
int	direction;
Sprod	sprod;
	/*
	Converts a GraphEd-edge into a Sgragra embedding-
	rule. "edge" is the embedding-rule to be converted.
	"direction" is the direction of this rule. "sprod"
	is the Sgragra-production, where the embedding is
	appended to.
	*/
	{
		Sembed	sembed;
		Node	right_side, other_side;
		Slist	*alphabet;
		char	dummy, *dummyptr1, *dummyptr2, *label;
		right_side = direction == S_out ? edge->source : edge->target;
		other_side = direction == S_out ? edge->target : edge->source;

		sembed = make_sembed(sprod, (Snode) right_side->sgraph_node);

		alphabet = node_is_terminal(other_side) ? &(sprod->gragra->tv) : &(sprod->gragra->nv);
		sembed->node_embed = attr_data(S_alphabet_append(alphabet, other_side->label.text));

		sembed->olddir = sembed->newdir = direction;

		alphabet = &(sprod->gragra->ne);
		if ( (label=edge->label.text) != NULL)
		{	
			dummyptr1 = strchr(label, '>');
			dummyptr2 = strchr(label, '<');
			if (dummyptr1 == NULL && dummyptr2 == NULL)
			{	
				sembed->oldedge = attr_data(S_alphabet_append(alphabet, label));
				sembed->newedge = sembed->oldedge;
			}
			else if (dummyptr1 != NULL)
			{
				dummy = *dummyptr1;
				*dummyptr1 = '\0';
				sembed->oldedge = attr_data(S_alphabet_append(alphabet, label));
				sembed->newedge = attr_data(S_alphabet_append(alphabet, dummyptr1+1));
				*dummyptr1 = dummy;
			}
			else            /* if (dummyptr2 != NULL) */
			{
				dummy = *dummyptr2;
				*dummyptr2 = '\0';
				sembed->oldedge = attr_data(S_alphabet_append(alphabet, label));
				sembed->newedge = attr_data(S_alphabet_append(alphabet, dummyptr2+1));
				*dummyptr2 = dummy;
				sembed->newdir = !sembed->olddir;
			}
		}
	}

	
static
void	convert_nce1_embeddings(sprod, embedding, direction)
Sprod	sprod;
Embedding embedding;
int	direction;
	/*
	Converts the GraphEd embeddingrules into Sgragra-format.
	In GraphEd-nodes (from the right side of a production)
	the pointer sgraph_mode must have been initialized.	
	*/
	{
		if (embedding==NULL) return;
		while (embedding->right_side != NULL)
		{
			Group g;
			for_group(embedding->embed, g)
			{
				Edge edge;
				
				/* get edges between embedding->right_side and g->node.	*/
				/* These edges represent embedding-rules.		*/
				
				if (direction == S_out)
				{
					for_edge_sourcelist(embedding->right_side, edge)
					{
						if (edge->target == g->node)
						{	
							convert_edge_to_embedding (edge, S_out, sprod);
						}
					} end_for_edge_sourcelist(embedding->right_side, edge);
				}
				else
				{
					for_edge_targetlist(embedding->right_side, edge)
					{
						if (edge->source == g->node)
						{	
							convert_edge_to_embedding (edge, S_in, sprod);
						}
					} end_for_edge_targetlist(embedding->right_side, edge);
				}				
			} end_for_group(embedding->embed, g)
			embedding++;	
		}
	}	

	
static
void	convert_embeddings(sprod, graph)
Sprod	sprod;
Graph	graph;
	/*
	Converts the embedding rules, from the production "graph" into
	its representation in Sgragra's Sembed datastructure.
	*/
	{
		convert_nce1_embeddings(sprod, graph->gra.gra.nce1.embed_in,  S_in);
		convert_nce1_embeddings(sprod, graph->gra.gra.nce1.embed_out, S_out);
	}



/************************************************************************/
/*			    handle productions				*/
/************************************************************************/

static
int	append_graphed_prod_to_sgragra(sgragra, graph)
Sgragra	sgragra;
Graph	graph;
	/*
	Appends the production, given through graph to sgragra. The graph
	must not give a global embedding. To use this function, the pro-
	duction must be compiled by GraphEd.
	sgragra and graph may not be NULL and graph must give a production.
	Labels from nodes and edges become entries in the Alphabetlists, in
	the production only references into the alphabet are stored.
	The function returns true, if the conversion is correct.
	*/
{	
	Sgragra_type ggtyp;
	Sprod sprod;
	
	/*Determine grammartype of the production */
	switch (graph->gra.type)
	{	
		case ENCE_1:
			ggtyp = S_ENCE_1;
			break;
		case NCE_1:
			ggtyp = S_NCE_1;
			break;
		case NLC:
			ggtyp = S_NLC;
			break;
		case BNLC:
			ggtyp = S_BNLC;
			break;
		default:
			error("Sgragra: Can handle only 1-ENCE, 1-NCE, NLC and BNLC graphgrammars.\n");
			return false;

	}
	/* Determine directedness */
	ggtyp |= (graph->directed) ? 0 : 1;
	/* The lowest bit in ggtyp describes the undirectedness of the production */
	/* Setting this bit forces every grammarype to undirected.
		
	/* Test for same grammartypes */
	if (sgragra->class == S_UNDEFINED)
	{	
		/* 1st converted production, set grammartype */
		sgragra->class = ggtyp;
	}
	else
	{
		/* All productions of same type ? */
		if (sgragra->class != ggtyp)
		{
			error("All production must be of the same type\n");
			return false;
		}	
	}
	if (graph->gra.gra.nce1.left_side->node->label.text == NULL)
	{
		error("Sgragra: Nodelabels of left sides must have a name.\n");
		return false;
	}
	if (node_is_terminal(graph->gra.gra.nce1.left_side->node))
	{
		error("Sgragra: Nodelabels of left sides must be nonterminal.\n");
		return false;
	}
	sprod		= make_sprod(sgragra);
	sprod->label	= graph->label == NULL ? NULL : strdup(graph->label);
	sprod->left	= attr_data(S_alphabet_append(&(sgragra->nv), graph->gra.gra.nce1.left_side->node->label.text));
	sprod->right	= convert_group_to_sgraph (sgragra, graph->gra.gra.nce1.right_side);
	convert_embeddings(sprod, graph);	
	return true;
}



/************************************************************************/
/*			handle global embeddings			*/
/************************************************************************/

static
void	convert_edge_to_global_embedding(edge, direction, sgragra)
Edge	edge;
int	direction;
Sgragra	sgragra;
	/*
	Converts a GraphEd-edge into a global Sgragra embedding-
	rule. "edge" is the embedding-rule to be converted.
	"direction" is the direction of this rule. "sgragra"
	is the Sgragra-grammar, where the embedding is appended to.
	*/
	{
		Sglobalembed	sgembed;
		Node		right_side, other_side;
		Slist		*alphabet;
		char		dummy, *dummyptr1, *dummyptr2, *label;
		right_side = direction == S_out ? edge->source : edge->target;
		other_side = direction == S_out ? edge->target : edge->source;

		sgembed = make_sglobalembed(sgragra);

		alphabet = node_is_terminal(right_side) ? &(sgragra->tv) : &(sgragra->nv);
		sgembed->node_right = attr_data(S_alphabet_append(alphabet, right_side->label.text));

		alphabet = node_is_terminal(other_side) ? &(sgragra->tv) : &(sgragra->nv);
		sgembed->node_embed = attr_data(S_alphabet_append(alphabet, other_side->label.text));

		sgembed->olddir = sgembed->newdir = direction;

		alphabet = &(sgragra->ne);
		if ( (label=edge->label.text) != NULL)
		{	
			dummyptr1 = strchr(label, '>');
			dummyptr2 = strchr(label, '<');
			if (dummyptr1 == NULL && dummyptr2 == NULL)
			{	
				sgembed->oldedge = attr_data(S_alphabet_append(alphabet, label));
				sgembed->newedge = sgembed->oldedge;
			}
			else if (dummyptr1 != NULL)
			{
				dummy = *dummyptr1;
				*dummyptr1 = '\0';
				sgembed->oldedge = attr_data(S_alphabet_append(alphabet, label));
				sgembed->newedge = attr_data(S_alphabet_append(alphabet, dummyptr1+1));
				*dummyptr1 = dummy;
			}
			else        /* if (dummyptr2 != NULL) */
			{
				dummy = *dummyptr2;
				*dummyptr2 = '\0';
				sgembed->oldedge = attr_data(S_alphabet_append(alphabet, label));
				sgembed->newedge = attr_data(S_alphabet_append(alphabet, dummyptr2+1));
				*dummyptr2 = dummy;
				sgembed->newdir = !sgembed->olddir;
			}
		}
	}

	

static
void	convert_nce1_global_embeddings(sgragra, embedding, direction)
Sgragra	sgragra;
Embedding embedding;
int	direction;
	/*
	Converts the GraphEd global embeddingrules into Sgragra-format.
	*/
	{
		if (embedding==NULL) return;
		while (embedding->right_side != NULL)
		{
			Group g;
			for_group(embedding->embed, g)
			{
				Edge edge;
				
				/* get edges between embedding->right_side and g->node.	*/
				/* These edges represent embedding-rules.		*/
				
				if (direction == S_out)
				{
					for_edge_sourcelist(embedding->right_side, edge)
					{
						if (edge->target == g->node)
						{	
							convert_edge_to_global_embedding (edge, S_out, sgragra);
						}
					} end_for_edge_sourcelist(embedding->right_side, edge);
				}
				else
				{
					for_edge_targetlist(embedding->right_side, edge)
					{
						if (edge->source == g->node)
						{	
							convert_edge_to_global_embedding (edge, S_in, sgragra);
						}
					} end_for_edge_targetlist(embedding->right_side, edge);
				}				
			} end_for_group(embedding->embed, g)
			embedding++;	
		}
	}	



static
void	append_global_graphed_embeddings_to_sgragra (sgragra, graph)
Sgragra	sgragra;
Graph	graph;
{
	convert_nce1_global_embeddings(sgragra, graph->gra.gra.nce1.embed_in,  S_in);
	convert_nce1_global_embeddings(sgragra, graph->gra.gra.nce1.embed_out, S_out);
}



static
void	append_graphed_gragra_to_sgragra (info, canvas_buffer_number, autocompile)
Sgragra_proc_info info;
int	canvas_buffer_number;
int	autocompile;
	/*
	Copies all productions from Grapheds buffer with number canvas_buffer_number
	into the Sgragra datastructure. The productions must be compiled before this
	function may be called.
	Memory for the whole datastructure is mallocated. In the case of an error, the
	result is NULL.	Alphabets for terminal and nonterminal nodes and edges are
	determined from used labels in the productions.
	*/
{	
	Graph graph;
	
	if (info->sgragra == NULL)
	{	
		info->sgragra = make_sgragra();
	}
					
	for_all_graphs (canvas_buffer_number, graph)
	{	
		if (graph->is_production)
		{
			if  (	(graph->compile_time <= 0) ||
				(graph->compile_time < graph->change_time) )
			{	
				if (autocompile)
				{
					dispatch_user_action (COMPILE_PRODUCTION, graph);
					if (graph->compile_time < 0) 
					{
						remove_sgragra(info->sgragra);
						clear_info(info);
						return;
					}
				}
				else
				{
					error("Sgragra: Must compile first.\n");
					remove_sgragra(info->sgragra);
					clear_info(info);
					return;
				}	
			}
			if (graph_is_global_embedding_to(graph, graph))
			{
				append_global_graphed_embeddings_to_sgragra(info->sgragra, graph);
			}
			else 
			{
				if (!append_graphed_prod_to_sgragra(info->sgragra, graph, autocompile))
				{
					remove_sgragra(info->sgragra);
					clear_info(info);
					return;
				}
				if (graph == current_production)
				{
					/* This assignment will only be done, if conversion of	*/
					/* the production produced no error. In this case 	*/
					/* info->sgragra->productions is the current_production	*/
					info->current_production = info->sgragra->productions;
				}
			}
		}
	} end_for_all_graphs (canvas_buffer_number, graph);
	
	/* At least one production read ? */
	
	if (info->sgragra->productions == empty_sprod)
	{	
		error("Sgragra: No graphgrammar in the active window.\n");
		remove_sgragra(info->sgragra);
		clear_info(info);
	}
	
	return;
}



static
void	clean_sgragra(sgragra)
Sgragra	sgragra;
	/*
	Frees memory from the grammar and restores the Sgraph-pointers in
	GraphEd to NULL
	*/
{
	Sprod sprod;
	if (sgragra == NULL) return;
	
	/* Restore NULL in all node->sgraph_node and snode->graphed pointers */
	for_all_sprods(sgragra, sprod)
	{
		Snode snode;
		if (sprod->right != NULL)
		{
			for_all_nodes(sprod->right, snode)
			{
				if (snode->graphed != NULL)
				{
					((Node)snode->graphed)->sgraph_node = NULL;
					snode->graphed = NULL;
				}
			} end_for_all_nodes(sprod->right, snode);
		}
	
	} end_for_all_sprods(sgragra, sprod);	
	
	
	/* Now we can remove the sgragra */
	remove_sgragra(sgragra);

}


/************************************************************************/
/*			    global functions				*/
/************************************************************************/

Sgragra_proc_info 	init_sgragra_from_graphed_gragra( create_mode, auto_compile )
Sgragra_create_mode	create_mode;
bool			auto_compile;
{
	Sgragra_proc_info info;

	info = (Sgragra_proc_info) mymalloc(sizeof(struct sgragra_proc_info));
	clear_info(info);	/* Sets all fields from info to undefined. */
	info->create_mode = create_mode;
	
	switch (create_mode)
	{
		case SGG_NO_PRODUCTION:
			break;
		case SGG_CURRENT_PROD_WINDOW:
			if (current_production == empty_graph)
				error("No current production selected\n");
			else
				append_graphed_gragra_to_sgragra(info, current_production->buffer, auto_compile);
			break;
		case SGG_ACTIVE_WINDOW:
			append_graphed_gragra_to_sgragra(info, wac_buffer, auto_compile);
			break;
		case SGG_ALL_WINDOWS:
			{	int i;
				for (i=N_PASTE_BUFFERS; i<=N_BUFFERS ; i++)
				{	
					if (buffer_is_used(i))
					{
						append_graphed_gragra_to_sgragra(info, i, auto_compile);
					}
				}
			}
			break;
		default:
			error("Illegal Sgraph_sgragra_create_mode.\n");
			clear_info(info);
	}
	return info;
}


void	exit_sgragra_from_graphed_gragra( info )
Sgragra_proc_info info;
{
	clean_sgragra(info->sgragra);
	myfree(info);
}



