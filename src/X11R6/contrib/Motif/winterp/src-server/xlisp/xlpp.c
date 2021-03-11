/* -*-C-*-
********************************************************************************
*
* File:         xlpp.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlpp.c,v 2.4 1994/06/06 15:59:22 npm Exp $
* Description:  xlisp pretty printer
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:56 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlpp.c,v 2.4 1994/06/06 15:59:22 npm Exp $";

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
extern LVAL s_stdout;
extern int xlfsize;

/* local variables */
static int pplevel,ppmargin,ppmaxlen;
static LVAL ppfile;

/* forward declarations */
#ifdef ANSI
LOCAL void NEAR pp(LVAL expr);	/* NPM: changed this to LOCAL */
LOCAL void NEAR pplist(LVAL expr); /* NPM: changed this to LOCAL */
LOCAL void NEAR ppexpr(LVAL expr); /* NPM: changed this to LOCAL */
LOCAL void NEAR ppputc(int ch);	/* NPM: changed this to LOCAL */
LOCAL void NEAR ppterpri(void);	/* NPM: changed this to LOCAL */
LOCAL int  NEAR ppflatsize(LVAL expr); /* NPM: changed this to LOCAL */
#else
LOCAL FORWARD VOID pp();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID pplist();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID ppexpr();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID ppputc();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID ppterpri();	/* NPM: changed this to LOCAL */
#endif

#ifdef PRINDEPTH
extern LVAL s_printlevel, s_printlength;    /*modified for depth/length ctrl*/
extern int plevel, plength;
#define xlprint xlprintl
#endif

/* xpp - pretty-print an expression */
LVAL xpp()
{
    LVAL expr;

#ifdef PRINDEPTH

    /* get printlevel and depth values */
    expr = getvalue(s_printlevel);
    if (fixp(expr) && getfixnum(expr) <= 32767 && getfixnum(expr) >= 0) {
	plevel = (int)getfixnum(expr);
    }
    else {
	plevel = 32767;
    }
    expr = getvalue(s_printlength);
    if (fixp(expr) && getfixnum(expr) <= 32767 && getfixnum(expr) >= 0) {
	plength = (int)getfixnum(expr);
    }
    else
	plength = 32767;
#endif

    /* get expression to print and file pointer */
    expr = xlgetarg();
#ifdef BETTERIO
    ppfile = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    ppfile = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* pretty print the expression */
    pplevel = ppmargin = 0; ppmaxlen = 40;
    pp(expr); ppterpri();

    /* return nil */
    return (NIL);
}

/* pp - pretty print an expression */
LOCAL VOID NEAR pp(expr)
  LVAL expr;
{
    if (consp(expr))
	pplist(expr);
    else
	ppexpr(expr);
}

/* pplist - pretty print a list */
LOCAL VOID NEAR pplist(expr)
  LVAL expr;
{
    int n;

    /* if the expression will fit on one line, print it on one */
    if ((n = ppflatsize(expr)) < ppmaxlen) {
	xlprint(ppfile,expr,TRUE);
	pplevel += n;
    }

    /* otherwise print it on several lines */
    else {
#ifdef PRINDEPTH
	int llength = plength;

	if (plevel-- == 0) {
	    ppputc('#');
	    plevel++;
	    return;
	}
#endif

	n = ppmargin;
	ppputc('(');
	if (atom(car(expr))) {
	    ppexpr(car(expr));
	    ppputc(' ');
	    ppmargin = pplevel;
	    expr = cdr(expr);
	}
	else
	    ppmargin = pplevel;
	for (; consp(expr); expr = cdr(expr)) {
#ifdef PRINDEPTH
	    if (llength-- == 0) {
		xlputstr(ppfile,"... )");
		pplevel += 5;
		ppmargin =n;
		plevel++;
		return;
	    }
#endif
	    pp(car(expr));
	    if (consp(cdr(expr)))
		ppterpri();
	}
	if (expr != NIL) {
	    ppputc(' '); ppputc('.'); ppputc(' ');
	    ppexpr(expr);
	}
	ppputc(')');
	ppmargin = n;
#ifdef PRINDEPTH
	plevel++;
#endif
    }
}

/* ppexpr - print an expression and update the indent level */
LOCAL VOID NEAR ppexpr(expr)
  LVAL expr;
{
    xlprint(ppfile,expr,TRUE);
    pplevel += ppflatsize(expr);
}

/* ppputc - output a character and update the indent level */
LOCAL VOID NEAR ppputc(ch)
  int ch;
{
    xlputc(ppfile,ch);
    pplevel++;
}

/* ppterpri - terminate the print line and indent */
LOCAL VOID NEAR ppterpri()
{
    xlterpri(ppfile);
    for (pplevel = 0; pplevel < ppmargin; pplevel++)
	xlputc(ppfile,' ');
}

/* ppflatsize - compute the flat size of an expression */
LOCAL int NEAR ppflatsize(expr)
  LVAL expr;
{
    xlfsize = 0;
    xlprint(NIL,expr,TRUE);
    return (xlfsize);
}
