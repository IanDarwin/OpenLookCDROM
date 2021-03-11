/* -*-C-*-
********************************************************************************
*
* File:         xldebug.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xldbug.c,v 2.5 1994/06/06 15:59:12 npm Exp $
* Description:  xlisp debugging support
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:41 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xldbug.c,v 2.5 1994/06/06 15:59:12 npm Exp $";

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
extern int xldebug;
extern int xlsample;
extern LVAL s_debugio,s_unbound,s_stderr;
extern LVAL s_tracenable,s_tlimit,s_breakenable;
extern LVAL true;

/* forward declarations */
#ifdef ANSI
LOCAL void NEAR breakloop(char *hdr, char FAR *cmsg, char FAR *emsg, LVAL arg,  int cflag);
#else
LOCAL FORWARD VOID breakloop();
#endif


/* xlabort - xlisp serious error handler */
VOID xlabort(emsg)
  char *emsg;
{
    xlsignal(emsg,s_unbound);
    xlerrprint("error",NULL,emsg,s_unbound);
    xlbrklevel();
}

/* xlbreak - enter a break loop */
VOID xlbreak(emsg,arg)
  char FAR *emsg; LVAL arg;
{
    breakloop("break","return from BREAK",emsg,arg,TRUE);
}

/* xlfail - xlisp error handler */
VOID xlfail(emsg)
  char *emsg;
{
    xlerror(emsg,s_unbound);
}

/* xlerror - handle a fatal error */
LVAL xlerror(emsg,arg)
  char FAR *emsg; LVAL arg;
{
    if (!null(getvalue(s_breakenable)))
	breakloop("error",NULL,emsg,arg,FALSE);
    else {
	xlsignal(emsg,arg);
	xlerrprint("error",NULL,emsg,arg);
	xlbrklevel();
    }
	return NIL;	/* actually doesn't return */
}

/* xlcerror - handle a recoverable error */
VOID xlcerror(cmsg,emsg,arg)
  char FAR *cmsg, FAR *emsg; LVAL arg;
{
    if (!null(getvalue(s_breakenable)))
	breakloop("error",cmsg,emsg,arg,TRUE);
    else {
	xlsignal(emsg,arg);
	xlerrprint("error",NULL,emsg,arg);
	xlbrklevel();
    }
}


#ifdef WINTERP
/******************************************************************************
 * LVAL Winterp_Call_Error_Hook();
 * 
 * If *ERRHOOK* exists, call it, returning value of last expr evald. If
 * if no *ERRHOOK* exists, returns NIL.
 *
 * the error hook is defined with
 * (setq *errhook*
 *       (lambda (hdr cmsg emsg &optional (arg NIL arg-supplied-p)) ...)
 * hdr  -- first arg  -- STRING        -- the kind of error, e.g. "error", "break", etc.
 * cmsg -- second arg -- STRING or NIL -- continuable error message (NIL if not continuable)
 * emsg -- third arg  -- STRING        -- the error message
 * arg  -- fourth arg -- <LVAL>        -- value assoc'd with error, or s_unbound if none.
 ******************************************************************************/
LVAL Winterp_Call_Error_Hook(hdr, cmsg, emsg, arg)
     char* hdr;
     char* cmsg;
     char* emsg;
     LVAL arg;
{
  extern LVAL s_errhook;
  extern LVAL xldenv;
  FRAMEP newfp;
  LVAL tmp;
  LVAL fun;
  LVAL val;

  /* call the *ERRHOOK* if necessary */
  if ((s_errhook != NULL)
      && ((fun = getvalue(s_errhook)) != NIL)
      && closurep(fun)) {

    /* temporarily rebind error hook function to NIL */
    tmp = xldenv;
    xldbind(s_errhook, NIL);

    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    if (arg == s_unbound) {
      pusharg(cvfixnum((FIXTYPE)3));
      pusharg(cvstring(hdr));	/* hdr  -- first arg */
      pusharg((cmsg) ? cvstring(cmsg) : NIL); /* cmsg -- second arg */
      pusharg(cvstring(emsg));	/* emsg -- third arg */
      xlfp = newfp;
      val = xlapply(3);
    }
    else {
      pusharg(cvfixnum((FIXTYPE)4));
      pusharg(cvstring(hdr));	/* hdr  -- first arg */
      pusharg((cmsg) ? cvstring(cmsg) : NIL); /* cmsg -- second arg */
      pusharg(cvstring(emsg));	/* emsg -- third arg */
      pusharg(arg);		/* arg  -- fourth arg */
      xlfp = newfp;
      val = xlapply(4);
    }

    /* unbind temporarily bound error hook function  */
    xlunbind(tmp);
    return (val);		/* return value from last expr in hook */
  }
  else
    return (NIL);		/* return NIL if no hook defined */
}
#endif /* WINTERP */

/* xlerrprint - print an error message */
VOID xlerrprint(hdr,cmsg,emsg,arg)
  char *hdr, FAR *cmsg, FAR *emsg; LVAL arg;
{
/* TAA MOD -- start error message on a fresh line */
#ifdef BETTERIO
    xlfreshline(getvalue(s_stderr));
#else
    errputstr("\n");
#endif

#ifdef WINTERP
    /* print normal xlisp msg only if no error hook, or error hook returns NIL */
    if (Winterp_Call_Error_Hook(hdr,cmsg,emsg,arg) == NIL)
    {
#endif /* WINTERP */

    /* print the error message */
#ifdef MEDMEM
    sprintf(buf,"%s: %Fs",hdr,emsg);
#else
    sprintf(buf,"%s: %s",hdr,emsg);
#endif
    errputstr(buf);

    /* print the argument */
    if (arg != s_unbound) {
	errputstr(" - ");
	errprint(arg);
    }

    /* no argument, just end the line */
    else
	errputstr("\n");

    /* print the continuation message */
    if (cmsg != NULL) {
#ifdef MEDMEM
	sprintf(buf,"if continued: %Fs\n",cmsg);
#else
	sprintf(buf,"if continued: %s\n",cmsg);
#endif
	errputstr(buf);
    }

#ifdef WINTERP
    }				/* end of if...{ added in prev WINTERP ifdef */
#endif /* WINTERP */
}

/* breakloop - the debug read-eval-print loop */
LOCAL VOID NEAR breakloop(hdr,cmsg,emsg,arg,cflag)
  char *hdr, FAR *cmsg, FAR *emsg; LVAL arg; int cflag;
{
    LVAL expr,val;
    CONTEXT cntxt;
    int type;

    /* print the error message */
    xlerrprint(hdr,cmsg,emsg,arg);

    /* flush the input buffer */
    xlflush();

    /* do the back trace */
    if (!null(getvalue(s_tracenable))) {
	val = getvalue(s_tlimit);
	xlbaktrace(fixp(val) ? (int)getfixnum(val) : -1);
    }

    /* protect some pointers */
    xlsave1(expr);

    /* increment the debug level */
    ++xldebug;

    /* debug command processing loop */
    xlbegin(&cntxt,CF_BRKLEVEL|CF_CLEANUP|CF_CONTINUE,true);
    for (type = 0; type == 0; ) {

	/* setup the continue trap */
	if ((type = setjmp(cntxt.c_jmpbuf)) != 0)
	    switch (type) {
	    case CF_CLEANUP:	/* NPM: last call had 'clean-up' or EOF input  */
		continue;	/*       exit loop, --xldebug, and xlbrklevel()*/
	    case CF_BRKLEVEL:	/* NPM: last call returned thru xlbrklevel(),  */
		type = 0;       /*       do read/eval/print at prev brklevel   */
		break;
	    case CF_CONTINUE:	/* NPM: last call had 'continue' input         */
		if (cflag) {	/*      if err cont'able,                      */
		    dbgputstr("[ continue from break loop ]\n");
		    continue;   /*      exit loop, --xldebug, and xlbrklevel() */
		}
		else xlabort("this error can't be continued");
	    }

	/* print a prompt */
	sprintf(buf,"%d> ",xldebug);
	dbgputstr(buf);

	/* read an expression and check for eof */
	if (!xlread(getvalue(s_debugio),&expr)) {
	    type = CF_CLEANUP;
	    break;
	}

	/* save the input expression */
	xlrdsave(expr);

	/* evaluate the expression */
	expr = xleval(expr);

	/* save the result */
	xlevsave(expr);

	/* Show result on a new line -- TAA MOD to improve display */
#ifdef BETTERIO
	xlfreshline(getvalue(s_debugio));
#endif

	/* print it */
	dbgprint(expr);
    }
    xlend(&cntxt);

    /* decrement the debug level */
    --xldebug;

    /* restore the stack */
    xlpop();

    /* check for aborting to the previous level */
    if (type == CF_CLEANUP)
	xlbrklevel();
}

/* baktrace - do a back trace */
VOID xlbaktrace(n)
  int n;
{
    FRAMEP fp, p;
    int argc;
    for (fp = xlfp; (n < 0 || n--) && !null(*fp); fp = fp - (int)getfixnum(*fp)) {
	p = fp + 1;
	errputstr("Function: ");
	errprint(*p++);
	if ((argc = (int)getfixnum(*p++)) != 0)
	    errputstr("Arguments:\n");
	while (--argc >= 0) {
	    errputstr("	 ");
	    errprint(*p++);
	}
    }
}

/* xldinit - debug initialization routine */
VOID xldinit()
{
    xlsample = 0;
    xldebug = 0;
}

