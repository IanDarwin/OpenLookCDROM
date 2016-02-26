/* (C) Universitaet Passau 1986-1991 */
%{/**********************************************************************/
/*									*/
/*				stdsanner.l				*/
/*									*/
/************************************************************************/

#include "std.h"
#include "y.tab.h"

#define LEXDEBUG
%}

digit		[0-9]
number		-?{digit}+

left_brace	\{
right_brace	\}
no_right_brace	[^\}]
dollar		\$
no_dollar	[^\$]

begin_graphed_internals	{left_brace}{dollar}
end_graphed_internals	{dollar}{right_brace}

quotation_mark		\"
no_quotation_mark	[^\"]

white_space	[ \t]
newline		"\n"

%%
{number}	{
			/* Ganze Zahl (auch negativ !) */
			

			yylval.nr = atoi (yytext);
			return NUMBER;
		}

{quotation_mark}{no_quotation_mark}*	{

			/* We are lazy here and assume that identifier	*/
			/* does not contain a (quoted) quotation mark	*/
			/* and is shorter than 1000 characters		*/
			
#			define IDMAXLEN	1000
			
			static	char	id[IDMAXLEN];
			int		length;
			
			input ();	/* read quotation mark at the	*/
					/* end of the identifier	*/
			
			length = minimum (yyleng, IDMAXLEN);
			strncpy (id, &yytext[1], length-1);
			id[length-1] = '\0';
			
			yylval.text = id;
			
			return IDENTIFIER;
		}

{left_brace}{no_dollar}{no_right_brace}*	{

			/* Kommentar : { irgendwas }		*/
			
			input (); /* right_brace noch mitlesen	*/
		}

{begin_graphed_internals}	{
					return BEGIN_GRAPHED_INTERNALS;
				}
{end_graphed_internals}		{
					return END_GRAPHED_INTERNALS;
				}


GRAPH		return GRAPH;
END		return END;
"="		return ('=');
";"		return (';');
GRAPHS		return GRAPHS;
DIRECTED	return DIRECTED;
UNDIRECTED	return UNDIRECTED;
NP		return NODEPLACE;
","		return (',');
[a-zA-Z]+	return UNKNOWN_INTERNAL;


{newline}	;
{white_space}	;

.		{
			return yytext[0];
			/*  = alles hier nicht aufgefuehrte	*/
		}
