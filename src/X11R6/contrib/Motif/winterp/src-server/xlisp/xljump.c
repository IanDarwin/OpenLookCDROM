/* -*-C-*-
********************************************************************************
*
* File:         xljump.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xljump.c,v 2.4 1994/06/06 15:59:19 npm Exp $
* Description:  execution context routines
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:10 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xljump.c,v 2.4 1994/06/06 15:59:19 npm Exp $";

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
extern CONTEXT *xlcontext,*xltarget;
extern LVAL xlvalue,xlenv,xlfenv,xldenv;
extern int xlmask;

/* forward declarations */
#ifdef ANSI
LOCAL VOID NEAR findandjump(int mask, char *error); /* NPM: changed this to LOCAL */
#else /* !defined(ANSI) */
LOCAL FORWARD VOID findandjump(); /* NPM: changed this to LOCAL */
#endif /* ANSI */

/* xlbegin - beginning of an execution context */
VOID xlbegin(cptr,flags,expr)
  CONTEXT *cptr; int flags; LVAL expr;
{
    cptr->c_flags = flags;
    cptr->c_expr = expr;
    cptr->c_xlstack = xlstack;
    cptr->c_xlenv = xlenv;
    cptr->c_xlfenv = xlfenv;
    cptr->c_xldenv = xldenv;
    cptr->c_xlcontext = xlcontext;
    cptr->c_xlargv = xlargv;
    cptr->c_xlargc = xlargc;
    cptr->c_xlfp = xlfp;
    cptr->c_xlsp = xlsp;
    xlcontext = cptr;
}

/* xlend - end of an execution context */
VOID xlend(cptr)
  CONTEXT *cptr;
{
    xlcontext = cptr->c_xlcontext;
}

/* xlgo - go to a label */
VOID xlgo(label)
  LVAL label;
{
    CONTEXT *cptr;
    FRAMEP argv;
    int argc;

    /* find a tagbody context */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_GO) {
	    argc = cptr->c_xlargc;
	    argv = cptr->c_xlargv;
	    while (--argc >= 0)
		if (*argv++ == label) {
		    cptr->c_xlargc = argc;
		    cptr->c_xlargv = argv;
		    xljump(cptr,CF_GO,NIL);
		}
	}
    xlfail("no target for GO");
}

/* xlreturn - return from a block */
VOID xlreturn(name,val)
  LVAL name,val;
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_RETURN && cptr->c_expr == name)
	    xljump(cptr,CF_RETURN,val);
    xlfail("no target for RETURN");
}

/* xlthrow - throw to a catch */
VOID xlthrow(tag,val)
  LVAL tag,val;
{
    CONTEXT *cptr;

    /* find a catch context */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if ((cptr->c_flags & CF_THROW) && cptr->c_expr == tag)
	    xljump(cptr,CF_THROW,val);
    xlfail("no target for THROW");
}

/* xlsignal - signal an error */
VOID xlsignal(emsg,arg)
  char FAR *emsg; LVAL arg;
{
    CONTEXT *cptr;

    /* find an error catcher */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & CF_ERROR) {
	    if (!null(cptr->c_expr) && emsg != NULL)
		xlerrprint("error",NULL,emsg,arg);
	    xljump(cptr,CF_ERROR,NIL);
	}
}

/* xltoplevel - go back to the top level */
VOID xltoplevel()
{
    dbgputstr("[ back to top level ]\n");   /* TAA MOD -- was std */
    findandjump(CF_TOPLEVEL,"no top level");
}

/* xlbrklevel - go back to the previous break level */
VOID xlbrklevel()
{
    findandjump(CF_BRKLEVEL,"no previous break level");
}

/* xlcleanup - clean-up after an error */
VOID xlcleanup()
{
    dbgputstr("[ back to previous break level ]\n");	/* TAA MOD -- was std */
    findandjump(CF_CLEANUP,"not in a break loop");
}

/* xlcontinue - continue from an error */
VOID xlcontinue()
{
    findandjump(CF_CONTINUE,"not in a break loop");
}

/* xljump - jump to a saved execution context */
VOID xljump(target,mask,val)
  CONTEXT *target; int mask; LVAL val;
{
    /* unwind the execution stack */
    for (; xlcontext != target; xlcontext = xlcontext->c_xlcontext)

	/* check for an UNWIND-PROTECT */
	if ((xlcontext->c_flags & CF_UNWIND)) {
	    xltarget = target;
	    xlmask = mask;
	    break;
	}

    /* restore the state */
    xlstack = xlcontext->c_xlstack;
    xlenv = xlcontext->c_xlenv;
    xlfenv = xlcontext->c_xlfenv;
    xlunbind(xlcontext->c_xldenv);
    xlargv = xlcontext->c_xlargv;
    xlargc = xlcontext->c_xlargc;
    xlfp = xlcontext->c_xlfp;
    xlsp = xlcontext->c_xlsp;
    xlvalue = val;

    /* call the handler */
    longjmp(xlcontext->c_jmpbuf,mask);
}

/* findandjump - find a target context frame and jump to it */
LOCAL VOID NEAR findandjump(mask,error)
  int mask; char *error;
{
    CONTEXT *cptr;

    /* find a block context */
    for (cptr = xlcontext; cptr != NULL; cptr = cptr->c_xlcontext)
	if (cptr->c_flags & mask)
	    xljump(cptr,mask,NIL);
    xlabort(error);
}

