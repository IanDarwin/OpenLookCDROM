/* (C) Universitaet Passau 1986-1991 */
%{/************************************************************************/
/*									*/
/*				stdparser.y				*/
/*									*/
/************************************************************************/


#include "std.h"
#include "sgraph.h"

/* Stacksize for yacc	*/

#define	YYMAXDEPTH 2000

/* Macro to abort yacc	*/

#define	yacc_error(s)	{ yyerror(s); YYABORT; }


Local	Sgraph	current_graph;
Local	Snode	current_node;
Local	Sedge	current_edge;


%}
%union	{	/* stack for parser	*/
	int	nr;	/* Zahl			*/
	char	*text;	/* Label		*/
}
	
%token		GRAPH '=' ';' END GRAPHS
%token		DIRECTED UNDIRECTED
%token		BEGIN_GRAPHED_INTERNALS END_GRAPHED_INTERNALS
%token		NODEPLACE UNKNOWN_INTERNAL
%token	<nr>	NUMBER
%token	<text>	IDENTIFIER

%start		parser_start
%%
parser_start :		  graph
			| error
			{
				/* Syntax Error	*/
				if (current_graph != empty_graph) {
					remove_graph (current_graph);
					current_graph = empty_graph;
				}
				YYABORT;
			}
			;


graph :			{
				current_graph = empty_graph;
			}
			GRAPH IDENTIFIER '='
			{
				Attributes attrs;

				attrs.data = 0;
				current_graph = make_graph (attrs);
				if ($3 != nil)
					set_graphlabel (current_graph,
					                strsave ($3));
			}
			directedness
			graph_internals
			list_of_nodes
			END
			;

directedness :		  /* nothing or ... */
			{
				current_graph->directed = TRUE;
			}
			| DIRECTED
			{
				current_graph->directed = TRUE;
			}
			| UNDIRECTED
			{
				current_graph->directed = FALSE;
			}
			;

graph_internals :	/* nothing */
			|
			BEGIN_GRAPHED_INTERNALS
			numbers_and_strings ';'
			numbers_and_strings ';'
			numbers_and_strings ';'
			arbitrary_internals ';'
			arbitrary_internals
			END_GRAPHED_INTERNALS
			;


list_of_nodes :		  /* nothing or ... */
			| node list_of_nodes
			;

node :			node_nr
			node_internals
			nodelabel
			list_of_edges
			{
				current_node = empty_node;
			}
			;

node_nr :		NUMBER
			{
				Attributes attrs;

				attrs.data = 0;
				current_node = make_node_with_number (current_graph, attrs ,$1);
			}
			;

nodelabel :		IDENTIFIER
			{
				if ($1 != nil)
					set_nodelabel (current_node,
					               strsave ($1));
			}
			;

node_internals :	/* nothing */
			|
			BEGIN_GRAPHED_INTERNALS
			NUMBER NUMBER
			END_GRAPHED_INTERNALS
			{
				current_node->x = $2;
				current_node->y = $3;
			}
			|
			BEGIN_GRAPHED_INTERNALS
			any_internals
			NODEPLACE NUMBER NUMBER
			{
				current_node->x = $4;
				current_node->y = $5;
			}
			END_GRAPHED_INTERNALS
			;


list_of_edges :		 ';'
			| edge list_of_edges
			;

edge :			targetnode_nr
			edge_internals
			edgelabel
			{
				current_edge = empty_edge;
			}
			;

targetnode_nr :		NUMBER
			{
				Attributes attrs;

				attrs.data = 0;
				current_edge = make_edge (
				    current_node,
				    make_node_with_number (current_graph,attrs,$1),
				    attrs);
			}
			;

edgelabel :		IDENTIFIER
			{
				if ($1 != nil)
					set_edgelabel (current_edge,
					               strsave ($1));
			}
			;

edge_internals :	/* nothing */
			|
			BEGIN_GRAPHED_INTERNALS
			any_internals
			END_GRAPHED_INTERNALS
			;


any_internal :		UNKNOWN_INTERNAL unknown_internal
			;

any_internals :		
			| any_internal any_internals
			;

arbitrary_internal :	any_internal | NODEPLACE NUMBER NUMBER | ','
			/* a little hack for graphed 2.0 file format */
			;

arbitrary_internals :	
			| arbitrary_internal arbitrary_internals
			/* a little hack for graphed 2.0 file format */
			;

unknown_internal :	numbers_and_strings
			;

numbers_and_strings :	  /* nothing */
			| number_or_string numbers_and_strings
			;

number_or_string :	  NUMBER
			| IDENTIFIER
			;
%%
Local	int	yyerror (string)
char		*string;
{
	extern	int	yylineno;
	
	fprintf (stderr, "line %d :%s\n", yylineno, string);
}


Global	Sgraph	load_graph ()
{
	yyparse();
	
	return current_graph;
}
