L	[a-zA-Z]
D	[0-9]
I	{L}({L}|{D}|"_"|"-")*
N       ("-"|"+")?({D}|".")*
%{
#include "y.tab.h"
#include <strings.h>
#ifndef YYSTYPE
#define YYSTYPE int
#endif
extern YYSTYPE yylval;
extern char *malloc();
extern int line_no;
char *copytext();
%}
%%
"%".*           { }
"cdef"	        { ECHO; return(CDEF); }
"define"        { ECHO; return(DEFINE); }
"C"             { ECHO; return(C); }
"float"         { ECHO; return(FLOAT); }
"int"           { ECHO; return(INT); }
"string"        { ECHO; return(STRING); }
{I}		{ yylval = (YYSTYPE)copytext(); return(IDENT); }
{N}             { ECHO; yylval = (YYSTYPE)copytext(); return(NUMBER); }
"=>"            { ECHO; return(RARROW); }
"("		{ ECHO; return('('); }
","		{ ECHO; return(','); }
")"		{ ECHO; return(')'); }
"#"             { ECHO; return('#'); }
":"             { ECHO; return(':'); }
\n              { ECHO; line_no++; }
.	        { ECHO; }
%%
char *copytext()
{
    char *s;

    s = (char *)malloc((unsigned)(strlen(yytext) + 1));
    return(strcpy(s, yytext));
}





