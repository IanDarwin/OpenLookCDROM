/* (C) Universitaet Passau 1986-1991 */
%{/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/**********************************************************************/
/*									*/
/*				sanner.l				*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul enthaelt den Scanner fuer den Parser in parser.y.	*/
/*									*/
/*	Der eigentliche C-Sourcecode (der den Scanner yylex() enthaelt)	*/
/*	wird von dem Scannergerator lex erzeugt.			*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "load.h"

#include "graph.h"	/* Fuer y.tab.h	*/
#include "group.h"	/* ---  "   ---	*/

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	yylex()		[Nach Durchlauf von lex]		*/
/*									*/
/************************************************************************/


#include "y.tab.h"

#define LEXDEBUG
%}

digit		[0-9]
number		-?{digit}+

left_brace	\{
dollar		\$
no_dollar	[^\$]
right_brace	\}
no_right_brace	[^\}]

begin_graphed_internals	{left_brace}{dollar}
end_graphed_internals	{dollar}{right_brace}

quotation_mark			\"
no_quotation_mark		[^\"]
escaped_character		\\.
no_quotation_mark_or_backslash	[^"\\]
white_space	[ \t]
newline		"\n"

%%
{number}	{
			/* Ganze Zahl (auch negativ !) */
			

			yylval.nr = atoi (yytext);
			return NUMBER;
		}

{quotation_mark}({escaped_character}|{no_quotation_mark_or_backslash})*{quotation_mark}	{

			/* Identifier = "text", wobei ein '"' in text	*/
			/* durch ESCAPE_CHARACTER ('\') maskiert werden	*/
			/* kann						*/
			
			yylval.text = remove_escape_characters_from_text
					(&(yytext[1]), yyleng-2);
					/* yytext[1], yyleng-2, um die	*/
					/* " abzufangen			*/
					
			return IDENTIFIER;
		}

{left_brace}{no_dollar}{no_right_brace}*	{

			/* Kommentar : { irgendwas }, aber nicht {$	*/
			
			input (); /* right_brace noch mitlesen	*/
		}

{begin_graphed_internals}	{
					return BEGIN_GRAPH_INTERNALS;
				}
{end_graphed_internals}		{
					return END_GRAPH_INTERNALS;
				}

GRAPH		return GRAPH;
PROD		return PROD;
END		return END;
"="		return ('=');
";"		return (';');
","		return (',');
INIT		return INIT;
GRAPHS		return GRAPHS;

DIRECTED	return DIRECTED;
UNDIRECTED	return UNDIRECTED;

LEFT		return LEFT;
RIGHT		return RIGHT;
EMBED		return EMBED;
"<-"		return MAPSTO_IN;
"->"		return MAPSTO_OUT;

DIR		return DIR;
NSTYL		return NODESTYLE;
ESTYL		return EDGESTYLE;

NP		return NODEPLACE;
NS		return NODESIZE;
NEI		return NODE_EDGE_INTERFACE;
NFI		return NODEFONT_INDEX;
NTI		return NODETYPE_INDEX;
NLP		return NODELABEL_PLACEMENT;
NLV		return NODELABEL_VISIBILITY;

EL		return EDGELINE_POINTS;
EFI		return EDGEFONT_INDEX;
ETI		return EDGETYPE_INDEX;
ELV		return EDGELABEL_VISIBILITY;
ELS		return EDGELABEL_SIZE;
AL		return ARROWLENGTH;
AA		return ARROWANGLE;

COL		return COLOR;

GRAGRA		return GRAGRA;
GEMBED		return GLOBAL_EMBEDDING;
EMBMATCH	return EMBED_MATCH_ATTR;

WA		return WORKING_AREA_SIZE;
WAWIN		return WORKING_AREA_WINDOW;
SC		return SCROLL_OFFSET;
GR		return GRIDWIDTH;
SMP		return SELECT_IF_MULTIPLE_PICKED; /* obsolete */

[a-zA-Z]+	return UNKNOWN_INTERNAL;

{newline}	{ 
			lex_input_file_linenumber++;
			/* Ansonsten ueberlesen			*/
		}
{white_space}	{
			/* White space (tab,blank) ueberlesen	*/
		}
.		{
			return yytext[0];
			/*  = alles hier nicht aufgefuehrte	*/
		}
%%
