/* See end for RCS and copyright info */

%{
#include <eli.h>
#include <eliy.h>

#if !defined(_IBMR2) && !defined(bsdi)
extern char *realloc();
#endif
static DoString();
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
      (result = my_yy_input(buf, max_size, yyin))

int my_yy_input(char *buf, int max_size, FILE *yyin)
{
    int result;
    switch (EliProcessInfo.u_source) {	/* Where is the input coming from? */
	case e_source_stdin: {
	    int c = getc(yyin);
	    result = (c == EOF) ? YY_NULL : (buf[0] = c, 1);
	    break;
	    }
	case e_source_string: 
	    buf[0] = *(EliProcessInfo.u_sourcestring++);	/* No need to test for
							 * end-of-string here
							 * because if it's the
							 * end of string, zero
							 * will get returned
							 * anyway. */
		result = buf[0]?1:YY_NULL;
	    break;
	case e_source_file:
	    buf[0] = result = fgetc(EliProcessInfo.u_inputfp);
	    result = ((result == EOF)?YY_NULL: 1);
	    break;
	default:
	    result = YY_NULL;
	    break;
    }
    return (result);
}

#undef unput
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
static char elil[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/elil.flex,v 1.3 1994/05/09 21:21:08 rr2b Exp $";
#endif

#ifdef unput
#undef unput
#endif

static unput(c)		/* Replaces the lex macro */
    YY_CHAR  c;
{
    switch (EliProcessInfo.u_source)
    {
	case e_source_stdin:
	    yyunput(c, (YY_CHAR *) yytext);
	    break;
	case e_source_string:
	    --EliProcessInfo.u_sourcestring;
	    break;
	case e_source_file:
	    ungetc((int) c, EliProcessInfo.u_inputfp);
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

int reset_lexer()
{
  yy_init = 1;
}

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

