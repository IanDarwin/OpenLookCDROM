D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
LS			(l|L)
US			(u|U)

%{
/* $Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $ */

#include <stdio.h>
#include <parsec.h>

#ifdef MYLEXDEBUG
#define LEXRETURN(x) fprintf(stderr, "Lex: %d (%s)\n", (x), yytext); return (x)
#else
#define LEXRETURN(x) return(x)
#endif

#undef YYLMAX
#define YYLMAX PC_TOKENMAX

#define yylex PC_NextToken

extern int PC_IsTypedef(), PC_IsEnumerator();

static int yywrap()
{
	return(1);
}

static int comment();
static int check_type();
static int macro();

%}

%%
"/*"			{ comment(); }
^#			{ macro(); }
"auto"			{ LEXRETURN(PC_AUTO); }
"break"			{ LEXRETURN(PC_BREAK); }
"case"			{ LEXRETURN(PC_CASE); }
"char"			{ LEXRETURN(PC_CHAR); }
"const"			{ LEXRETURN(PC_CONST); }
"continue"		{ LEXRETURN(PC_CONTINUE); }
"default"		{ LEXRETURN(PC_DEFAULT); }
"do"			{ LEXRETURN(PC_DO); }
"double"		{ LEXRETURN(PC_DOUBLE); }
"else"			{ LEXRETURN(PC_ELSE); }
"enum"			{ LEXRETURN(PC_ENUM); }
"extern"		{ LEXRETURN(PC_EXTERN); }
"float"			{ LEXRETURN(PC_FLOAT); }
"for"			{ LEXRETURN(PC_FOR); }
"goto"			{ LEXRETURN(PC_GOTO); }
"if"			{ LEXRETURN(PC_IF); }
"int"			{ LEXRETURN(PC_INT); }
"long"			{ LEXRETURN(PC_LONG); }
"register"		{ LEXRETURN(PC_REGISTER); }
"return"		{ LEXRETURN(PC_RETURN); }
"short"			{ LEXRETURN(PC_SHORT); }
"signed"		{ LEXRETURN(PC_SIGNED); }
"sizeof"		{ LEXRETURN(PC_SIZEOF); }
"static"		{ LEXRETURN(PC_STATIC); }
"struct"		{ LEXRETURN(PC_STRUCT); }
"switch"		{ LEXRETURN(PC_SWITCH); }
"typedef"		{ LEXRETURN(PC_TYPEDEF); }
"union"			{ LEXRETURN(PC_UNION); }
"unsigned"		{ LEXRETURN(PC_UNSIGNED); }
"void"			{ LEXRETURN(PC_VOID); }
"volatile"		{ LEXRETURN(PC_VOLATILE); }
"while"			{ LEXRETURN(PC_WHILE); }

{L}({L}|{D})*		{ int t; PC_FillTokenBuf(yytext); t = check_type(); LEXRETURN(t); }

0[xX]{H}+{LS}?{US}?	{ PC_FillTokenBuf(yytext); LEXRETURN(PC_INTEGER_CONSTANT); }
0[xX]{H}+{US}?{LS}?	{ PC_FillTokenBuf(yytext); LEXRETURN(PC_INTEGER_CONSTANT); }
0{D}+{LS}?{US}?		{ PC_FillTokenBuf(yytext); LEXRETURN(PC_INTEGER_CONSTANT); }
0{D}+{US}?{LS}?		{ PC_FillTokenBuf(yytext); LEXRETURN(PC_INTEGER_CONSTANT); }
{D}+{LS}?{US}?		{ PC_FillTokenBuf(yytext); LEXRETURN(PC_INTEGER_CONSTANT); }
{D}+{US}?{LS}?		{ PC_FillTokenBuf(yytext); LEXRETURN(PC_INTEGER_CONSTANT); }
'(\\.|[^\\'])+'		{ PC_FillTokenBuf(yytext); LEXRETURN(PC_CHARACTER_CONSTANT); }

{D}+{E}{LS}?		{ PC_FillTokenBuf(yytext); LEXRETURN(PC_FLOATING_CONSTANT); }
{D}*"."{D}+({E})?{LS}?	{ PC_FillTokenBuf(yytext); LEXRETURN(PC_FLOATING_CONSTANT); }
{D}+"."{D}*({E})?{LS}?	{ PC_FillTokenBuf(yytext); LEXRETURN(PC_FLOATING_CONSTANT); }

\"(\\.|[^\\"])*\"	{ PC_FillTokenBuf(yytext); LEXRETURN(PC_STRING_CONSTANT); }

">>="			{ LEXRETURN(PC_RIGHTASSIGN); }
"<<="			{ LEXRETURN(PC_LEFTASSIGN); }
"+="			{ LEXRETURN(PC_ADDASSIGN); }
"-="			{ LEXRETURN(PC_SUBASSIGN); }
"*="			{ LEXRETURN(PC_MULASSIGN); }
"/="			{ LEXRETURN(PC_DIVASSIGN); }
"%="			{ LEXRETURN(PC_MODASSIGN); }
"&="			{ LEXRETURN(PC_ANDASSIGN); }
"^="			{ LEXRETURN(PC_XORASSIGN); }
"|="			{ LEXRETURN(PC_ORASSIGN); }
">>"			{ LEXRETURN(PC_RIGHT); }
"<<"			{ LEXRETURN(PC_LEFT); }
"++"			{ LEXRETURN(PC_INCR); }
"--"			{ LEXRETURN(PC_DECR); }
"->"			{ LEXRETURN(PC_DEREF); }
"&&"			{ LEXRETURN(PC_LOGICAL_AND); }
"||"			{ LEXRETURN(PC_LOGICAL_OR); }
"<="			{ LEXRETURN(PC_LE); }
">="			{ LEXRETURN(PC_GE); }
"=="			{ LEXRETURN(PC_EQUAL); }
"!="			{ LEXRETURN(PC_NOT_EQUAL); }
";"			{ LEXRETURN(';'); }
"{"			{ LEXRETURN('{'); }
"}"			{ LEXRETURN('}'); }
","			{ LEXRETURN(','); }
":"			{ LEXRETURN(':'); }
"="			{ LEXRETURN('='); }
"("			{ LEXRETURN('('); }
")"			{ LEXRETURN(')'); }
"["			{ LEXRETURN('['); }
"]"			{ LEXRETURN(']'); }
"."			{ LEXRETURN('.'); }
"&"			{ LEXRETURN('&'); }
"!"			{ LEXRETURN('!'); }
"~"			{ LEXRETURN('~'); }
"-"			{ LEXRETURN('-'); }
"+"			{ LEXRETURN('+'); }
"*"			{ LEXRETURN('*'); }
"/"			{ LEXRETURN('/'); }
"%"			{ LEXRETURN('%'); }
"<"			{ LEXRETURN('<'); }
">"			{ LEXRETURN('>'); }
"^"			{ LEXRETURN('^'); }
"|"			{ LEXRETURN('|'); }
"?"			{ LEXRETURN('?'); }
"..."			{ LEXRETURN(PC_ELLIPSIS); }

[ \t\v\n\f]		{ }
.			{ /* ignore bad characters */ }

%%
static char *tokens_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/parsec/RCS/tokens.l,v 1.4 1992/12/16 22:27:22 rr2b R6tape $";


static comment()
{
    int sawStar = 0;

    while (1) {
	switch (input()) {
	    case 0:
	        return;
	    case '*':
	        sawStar = !sawStar;
		break;
	    case '/':
	        if (sawStar)
		    return;
		break;
	    default:
	        sawStar = 0;
		break;
	}
    }
}

static int check_type()
{
    if (PC_IsTypedef(yytext))
        return (PC_TYPEDEF_NAME);
    if (PC_IsEnumerator(yytext))
        return (PC_ENUMERATION_CONSTANT);
    return (PC_IDENTIFIER);
}

static macro()
{
    int c, sawbslash = 0;

    while (1) {
        c = input();
	switch (c) {
	    case 0:
	        return;
	    case '\\':
	        sawbslash = !sawbslash;
		break;
	    case '\n':
	        if (sawbslash)
		    sawbslash = 0;
		else {
		    unput(c);
		    return;
		}
		break;
	    default:
		sawbslash = 0;
		break;
	}
    }
}
/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
