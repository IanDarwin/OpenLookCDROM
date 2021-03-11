/* See end for RCS and copyright info */

%{
#include <eli.h>
#include <eliy.h>

#ifndef _IBMR2
extern char *realloc();
#endif
static DoString();
%}

WS	    [ \t]
NL	    \n
DIGIT	    [0-9]
MAGIC	    [()'`,"; \t\n]
NONMAGIC    [^()'`,"; \t\n]

%%

\(			    return (LPAREN);
\)			    return (RPAREN);
(\+|-)?{DIGIT}+/{MAGIC}	    {
				if (EliProcessInfo.yparsebuflen < yyleng+1)
				    EliProcessInfo.yparsebuf = (char *) realloc(EliProcessInfo.yparsebuf,
					    EliProcessInfo.yparsebuflen = yyleng+1);
				strcpy( EliProcessInfo.yparsebuf, yytext );
				return (INTEGER);
			    }
'			    return (SQUOTE);
\"	    		    {
				DoString();
				return (STRING);
			    }
{NONMAGIC}+		    {
				if (EliProcessInfo.yparsebuflen < yyleng + 1)
				    EliProcessInfo.yparsebuf = (char *) realloc(EliProcessInfo.yparsebuf,
					    EliProcessInfo.yparsebuflen = yyleng + 1);
				strcpy( EliProcessInfo.yparsebuf, yytext ); 
				EliUpCaseStr( EliProcessInfo.yparsebuf );
				return (SYMBOL);
			    }
;.*			    ;	    /* Ignore comments */
({WS}|{NL})+		    ;	    /* Discard whitespace */

%%

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
******************************************************************/
#ifndef NORCSID
static char elil[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/elil.lex,v 1.3 1993/12/01 19:10:46 gk5g Exp $";
#endif

#undef input

static int  input()		/* Replaces the lex macro */
{
    int             result;

    switch (EliProcessInfo.u_source) {	/* Where is the input coming from? */
	case e_source_stdin:
	    result = ((yytchar = yysptr > yysbuf ?
		    U(*--yysptr) : getc(yyin)) == 10 ?
		    (yylineno++, yytchar) : yytchar) == EOF ?
		    0 : yytchar;
	    /*
	     * This ugly expression is what comes with lex
	     */
	    break;
	case e_source_string:
	    result = (int) *(EliProcessInfo.u_sourcestring++);	/* No need to test for
							 * end-of-string here
							 * because if it's the
							 * end of string, zero
							 * will get returned
							 * anyway. */
	    break;
	case e_source_file:
	    if (EOF == (result = fgetc(EliProcessInfo.u_inputfp)))
		result = 0;
	    break;
	default:
	    result = 0;
	    break;
    }
    return (result);
}


#undef unput

static unput(c)		/* Replaces the lex macro */
    int  c;
{
    switch (EliProcessInfo.u_source)
    {
	case e_source_stdin:
	    yytchar = c;
	    if (yytchar == '\n')
		yylineno--;
	    *yysptr++ = yytchar;
	    break;
	case e_source_string:
	    --EliProcessInfo.u_sourcestring;
	    break;
	case e_source_file:
	    ungetc(c, EliProcessInfo.u_inputfp);
	    break;
    }
}

static void AddToYParseBuf(c, grow)
int c, grow;
{
    int buflen = EliProcessInfo.yparsebuflen, len;

    if (!grow) {	/* Start a string from scratch */
        if (buflen >= 2) {
	    EliProcessInfo.yparsebuf[0] = c;
	    EliProcessInfo.yparsebuf[1] = '\0';
	    return;
	}
	EliProcessInfo.yparsebuf = (char *)realloc(EliProcessInfo.yparsebuf,
					   EliProcessInfo.yparsebuflen = 20);
	EliProcessInfo.yparsebuf[0] = c;
	EliProcessInfo.yparsebuf[1] = '\0';
	return;
    }
    len = strlen(EliProcessInfo.yparsebuf);
    if (len < (buflen - 1)) {
        EliProcessInfo.yparsebuf[len] = c;
	EliProcessInfo.yparsebuf[len + 1] = '\0';
        return;
    }
    EliProcessInfo.yparsebuf = (char *)realloc(EliProcessInfo.yparsebuf,
					EliProcessInfo.yparsebuflen =
					len + 20);
    EliProcessInfo.yparsebuf[len] = c;
    EliProcessInfo.yparsebuf[len + 1] = '\0';
}

static DoString()
{
    int sawbslash = 0, c;

    AddToYParseBuf('"', 0);
    while (1) {
        c = input();
	switch (c) {
	    case 0:
	        return;
            case '\\':
                sawbslash = !sawbslash;
		AddToYParseBuf(c, 1);
		break;
	    case '"':
		AddToYParseBuf(c, 1);
	        if (!sawbslash)
		    return;
		sawbslash = 0;
		break;
	    default:
	        AddToYParseBuf(c, 1);
		sawbslash = 0;
		break;
	}
    }
}

int reset_lexer() {}

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

