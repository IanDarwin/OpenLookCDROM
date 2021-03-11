%{

/***********************************************************
		Copyright IBM Corporation 1991

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

***********************************************************/

/*
	$Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $
*/

/*
		Lexical analyzer for address parsing.
*/

#include <andrewos.h>
#include <parseadd.h>
#include "parsey.h"

#define NIL	0

typedef unsigned char bool;
#define FALSE	0
#define TRUE	1

extern char *StrCopy();
static bool EatComment(), GetString(), GetDomainLit();

YYSTYPE yylval;

static char *CurrentLine, *CurrentChar, *cbuffer;

/* Chain of () comments found in address */
ADDRESS_COMMENT *yycomments, *LastComment;
extern ADDRESS_COMMENT *MakeComment();

static int my_YY_input();
#undef YY_INPUT
#define YY_INPUT(buf, result, max_size) \
    (result = my_YY_input(buf, max_size))
  
#undef YYLMAX
#define YYLMAX 400

#undef yywrap
int yywrap() {  return 1;}

static int my_YY_input(buf, max_size)
    char *buf;
    int max_size;
{
      register char c;
    *buf = c = *CurrentChar;
    if(*CurrentChar) CurrentChar++;
    if (!(*buf)) return 0;
    return 1;
  }

#undef unput
%}

LWSP			[ \t]
ATOMCHARS		[!#-\'\*\+\-/-9=\?A-Z\^-~]
WORD			{ATOMCHARS}+
DWORD			({WORD}(\.{WORD})+)
ALIST			({WORD}({LWSP}+{WORD})+)
DHACK			(({WORD}\.+)|(\.+{WORD})|(\.\.+))

%%

\n{LWSP}+		|

{LWSP}+			;

\(			{
			  register ADDRESS_COMMENT *c;
			  char *s;

			  cbuffer = yytext+1;
			  if (!EatComment()) return BADTOKEN;
			  s = StrCopy(yytext);
			  if (s == NIL) return BADTOKEN;
			  c = MakeComment(s);
			  if (c == NIL) return BADTOKEN;
			  if (yycomments == NIL)
			      LastComment = yycomments = c;
			  else
			      LastComment -> Next = c;
			  LastComment = c;
			}

\"			{
			  if (GetString()) {
			      yylval.u_string = StrCopy(yytext);
			      if (yylval.u_string == NIL)
				  return BADTOKEN;
			      else
				  return QUOTEDSTRING;
			  } else
			      return BADTOKEN;
			}

\[			{
			  if (GetDomainLit()) {
			      yylval.u_string = StrCopy(yytext);
			      if (yylval.u_string == NIL)
				  return BADTOKEN;
			      else
				  return DOMAINLIT;
			  } else
			      return BADTOKEN;
			}

({WORD}{LWSP}+)*{DHACK}	{
			  yylval.u_string = StrCopy(yytext);
			  if (yylval.u_string == NIL)
			      return BADTOKEN;
			  else
			      return DOTLIST;
			}

\<			return '<';
\>			return '>';
@			return '@';
\.			return '.';
\;			return ';';
:			return ':';
\,			return ',';
\\			return '\\';

{DWORD}			{
			  yylval.u_string = StrCopy(yytext);
			  if (yylval.u_string == NIL)
			      return BADTOKEN;
			  else
			      return DOTWORD;
			}

{ALIST}			{
			  yylval.u_string = StrCopy(yytext);
			  if (yylval.u_string == NIL)
			      return BADTOKEN;
			  else
			      return ATOMLIST;
			}

{WORD}			{
		  	  yylval.u_string = StrCopy(yytext);
			  if (yylval.u_string == NIL)
			      return BADTOKEN;
			  else
			      return ATOM;
			}

.			return BADTOKEN;

%%
static char *parsel_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mail/lib/RCS/parsel.flex,v 1.1 1994/04/23 17:33:53 rr2b Exp $";

void SetNextLine(line)
    char *line;
{
    extern int ParseErrorReason;

    CurrentLine = line;
    CurrentChar = CurrentLine;
    yycomments = NIL;
    ParseErrorReason = PA_SYNTAX_ERROR;
}

#ifdef unput
#undef unput
#endif

static unput(c)
    char c;
{
    /* Only push back if not at beginning or end of line */
    if (CurrentChar > CurrentLine)
	*--CurrentChar = c;
}

int pareset_lexer()
{
  yy_init = 1;
}

static bool LWSP_char(c)
    char c;
{
    return (c == ' ' || c == '\t');
}

/* Eat a comment -- returns with closing ) as current char */

static bool EatComment()
{
    char *last = yytext + YYLMAX - 2;

    /* Just ate ( */
    for (;cbuffer < last;) {
	register char c;

	c = input();
	switch (c) {
	    case 0:		return FALSE;
	    case ')':		*cbuffer++ = c;
		                *cbuffer = '\0';
		                return(TRUE);
	    case '(':		*cbuffer++ = '(';
				if (!EatComment())
				    return FALSE;
				break;
	    case '\\':		*cbuffer++ = '\\';
				c = input();
				if (c == 0) return FALSE;
				*cbuffer++ = c;
				break;
	    case '\n':		c = input();
				if (!LWSP_char(c)) return FALSE;
				do { c = input(); } while (LWSP_char(c));
				unput(c);
				*cbuffer++ = ' ';
				break;
	    default:		*cbuffer++ = c;
				break;
	}
    }
    if (cbuffer == last) {
	*cbuffer++ = ')';
	*cbuffer = '\0';
	return(TRUE);
    }
}

/*
   Read a quoted string -- leaves closing quote as current char.
*/

static bool GetString()
{
    register char *next = yytext + 1, *last = yytext + YYLMAX - 2;

    /* Already seen " */
    for (; next < last ; ) {
	register char c;

	c = input();
	switch (c) {
	    case 0:	return FALSE;
	    case '"':	*next++ = '"';
			*next = '\0';
			return TRUE;
	    case '\\':	*next++ = '\\';
			c = input();
			if (c == 0) return FALSE;
			*next++ = c;
			break;
	    case '\n':	c = input();
			if (!LWSP_char(c)) return FALSE;
			do { c = input(); } while (LWSP_char(c));
			*next++ = ' ';
			unput(c);
			break;
	    default:	*next++ = c;
			break;
	}
    }
    if (next == last) {
	*next++ = '"';
	*next = '\0';
	return(TRUE);
    }
}

/*
   Read a domain literal leaves closing quote as current char.
*/

static bool GetDomainLit()
{
    register char *next = yytext + 1, *last = yytext + YYLMAX - 2;

    /* Already seen [ */
    for (; next < last ; ) {
	register char c;

	c = input();
	switch (c) {
	    case 0:
	    case '[':		return FALSE;
	    case ']':		*next++ = ']';
				*next = '\0';
				return TRUE;
	    case '\\':		c = input();
				if (c == 0) return FALSE;
				*next++ = c;
				break;
	    case '\n':		c = input();
				if (!LWSP_char(c)) return FALSE;
				do { c = input(); } while (LWSP_char(c));
				*next++ = ' ';
				unput(c);
				break;
	    default:		*next++ = c;
				break;
	}
    }
    if (next == last) {
	*next++ = ']';
	*next = '\0';
	return TRUE;
    }
}
