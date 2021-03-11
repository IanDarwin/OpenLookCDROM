/* -*-C-*-
********************************************************************************
*
* File:         xlio.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlio.c,v 2.4 1994/06/06 15:59:15 npm Exp $
* Description:  xlisp i/o routines
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:28 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Enterprise Integration
* Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
* Betz make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
* COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlio.c,v 2.4 1994/06/06 15:59:15 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include "xlisp.h"

/* external variables */
extern LVAL s_stdout,s_stderr,s_debugio,s_traceout;
extern int xlfsize;

/* xlgetc - get a character from a file or stream */
int xlgetc(fptr)
  LVAL fptr;
{
    LVAL lptr,cptr;
    FILEP fp;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
	if ((lptr = gethead(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
		xlfail("bad stream");
	    sethead(fptr,lptr = cdr(lptr));
	    if (lptr == NIL)
		settail(fptr,NIL);
	    ch = getchcode(cptr);
	}
    }

    /* otherwise, check for a buffered character */
    else if ((ch = getsavech(fptr)) != 0)
	setsavech(fptr,'\0');

    /* otherwise, check for terminal input or file input */
    else {
	fp = getfile(fptr);
	if (fp == CLOSED)   /* TAA MOD -- give error */
	    xlfail("can't read closed stream");
	else if (fp == CONSOLE)
	    /* TAA MOD -- revamped for redirecting */
	    ch = ostgetc();
	else {
#ifdef BETTERIO
	    if ((fptr->n_sflags & S_FORREADING) == 0)
		xlerror("can't read write-only file stream", fptr);
	    if ((fptr->n_sflags & S_READING) == 0) {
		/* possible direction change*/
		if (fptr->n_sflags & S_WRITING) {
		    OSSEEKCUR(fp,0L);
		}
		fptr->n_sflags |= S_READING;
		fptr->n_sflags &= ~S_WRITING;
	    }
#endif
	    ch = OSGETC(fp);
	}
    }

    /* return the character */
    return (ch);
}

/* xlungetc - unget a character */
VOID xlungetc(fptr,ch)
  LVAL fptr; int ch;
{
    LVAL lptr;

    /* check for ungetc from nil */
    if (fptr == NIL)
	;

    /* otherwise, check for ungetc to a stream */
    else if (ustreamp(fptr)) {
	if (ch != EOF) {
	    lptr = cons(cvchar(ch),gethead(fptr));
	    if (gethead(fptr) == NIL)
		settail(fptr,lptr);
	    sethead(fptr,lptr);
	}
    }

    /* otherwise, it must be a file */
    else
	setsavech(fptr,ch);
}

/* xlpeek - peek at a character from a file or stream */
int xlpeek(fptr)
  LVAL fptr;
{
    LVAL lptr,cptr;
    int ch;

    /* check for input from nil */
    if (fptr == NIL)
	ch = EOF;

    /* otherwise, check for input from a stream */
    else if (ustreamp(fptr)) {
	if ((lptr = gethead(fptr)) == NIL)
	    ch = EOF;
	else {
	    if (!consp(lptr) || (cptr = car(lptr)) == NIL || !charp(cptr))
		xlfail("bad stream");
	    ch = getchcode(cptr);
	}
    }

    /* otherwise, get the next file character and save it */
    else {
	ch = xlgetc(fptr);
	if (ch != EOF) setsavech(fptr,ch);  /* TAA MOD -- don't save EOF! */
    }

    /* return the character */
    return (ch);
}

/* xlputc - put a character to a file or stream */
VOID xlputc(fptr,ch)
  LVAL fptr; int ch;
{
    LVAL lptr;
    FILEP fp;

    /* count the character */
    ++xlfsize;

    /* check for output to nil */
    if (fptr == NIL)
	;

    /* otherwise, check for output to an unnamed stream */
    else if (ntype(fptr) == USTREAM) {	/* TAA MOD, was ustreamp() */
	lptr = consa(cvchar(ch));
	if (gettail(fptr)!=NIL)
	    rplacd(gettail(fptr),lptr);
	else
	    sethead(fptr,lptr);
	settail(fptr,lptr);
    }

    /* otherwise, check for terminal output or file output */
    else {
	fp = getfile(fptr);
	if (fp == CLOSED)   /* TAA MOD -- give error */
	    xlfail("can't write closed stream");
	if (fp == CONSOLE)  /* TAA MOD -- for redirecting */
	    ostputc(ch);
	else {
#ifdef BETTERIO
	    if ((fptr->n_sflags & S_FORWRITING) == 0)
		xlerror("can't write read-only file stream", fptr);
	    if ((fptr->n_sflags & S_WRITING) == 0) {
		/* possible direction change*/
		if (fptr->n_sflags & S_READING) {
		    OSSEEKCUR(fp,
			(getsavech(fptr)?(setsavech(fptr,'\0'),-1L):0L));
		}
		fptr->n_sflags |= S_WRITING;
		fptr->n_sflags &= ~S_READING;
		fptr->n_cpos = 0;   /* best guess */
	    }
	    if (ch == '\n') fptr->n_cpos = 0;
	    else fptr->n_cpos++;
#endif
	    if (OSPUTC(ch,fp)==EOF) /* TAA MOD to check for write to RO file*/
		xlerror("write failed", fptr);
	}
    }
}

/* xlflush - flush the input buffer */
VOID xlflush()
{
    osflush();
}

/* stdprint - print to *standard-output* */
VOID stdprint(expr)
  LVAL expr;
{
    xlprint(getvalue(s_stdout),expr,TRUE);
    xlterpri(getvalue(s_stdout));
}

/* stdputstr - print a string to *standard-output* */
VOID stdputstr(str)
  char *str;
{
    xlputstr(getvalue(s_stdout),str);
}

/* errprint - print to *error-output* */
VOID errprint(expr)
  LVAL expr;
{
    xlprint(getvalue(s_stderr),expr,TRUE);
    xlterpri(getvalue(s_stderr));
}

/* errputstr - print a string to *error-output* */
VOID errputstr(str)
  char *str;
{
    xlputstr(getvalue(s_stderr),str);
}

/* dbgprint - print to *debug-io* */
VOID dbgprint(expr)
  LVAL expr;
{
    xlprint(getvalue(s_debugio),expr,TRUE);
    xlterpri(getvalue(s_debugio));
}

/* dbgputstr - print a string to *debug-io* */
VOID dbgputstr(str)
  char *str;
{
    xlputstr(getvalue(s_debugio),str);
}

/* trcprin1 - print to *trace-output* */
VOID trcprin1(expr)
  LVAL expr;
{
    xlprint(getvalue(s_traceout),expr,TRUE);
}

/* trcputstr - print a string to *trace-output* */
VOID trcputstr(str)
  char *str;
{
    xlputstr(getvalue(s_traceout),str);
}
