%{

#ifndef hpux
#pragma ident   "@(#)lex.l 1.3     93/08/19"
#endif

#undef ECHO
#undef input
#include "ftptool.h"

#undef input
extern char *lex_string;
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):*lex_string++)==10?(yylineno++,yytchar):yytchar)==0?0:yytchar)
%}
%%
[ \t\n]+	{return(' ');}
PERMS		{return(PERMS);}
LINKS		{return(LINKS);}
USER		{return(USER);}
GROUP		{return(GROUP);}
SIZE		{return(SIZE);}
MONTH		{return(MONTH);}
DAY			{return(DAY);}
TIME		{return(TIME);}
NAME		{return(NAME);}
SKIP		{return(SKIP);}
NONUNIX		{return(NONUNIX);}
\<NAME\>		{return(LOWERNAME);}
.			{return(yytext[0]);}
%%

yywrap()
{
	return 1;
}
