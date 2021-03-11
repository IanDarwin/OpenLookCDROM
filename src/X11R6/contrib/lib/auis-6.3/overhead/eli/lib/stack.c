/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
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
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/stack.c,v 2.7 1992/12/15 21:02:39 rr2b R6tape $";
#endif

#include  <stack.h>

/*
 * Functions to handle manipulation of the global stack 
 */

/*
 * Evaluates the items in arglist and binds them to the symbols named in
 * varlist.  Bound symbols are placed on stack s.  Note: Each var name gives
 * rise to a new symbol with the same name, just in case the original points
 * to a global symbol (which we don't want to bind to). 
 */

int             eliEvalAndBind(st, s, varlist, arglist)
EliState_t     *st;
eliEvalStack_t *s;
EliCons_t      *varlist, *arglist;
{
    int             pushed = 0, varlen = EliListLen(varlist), argsempty = !EliListLen(arglist), i;
    EliCons_t      *varptr = varlist, *argptr = arglist;
    EliSexp_t      *curvar, *curarg = NULL, *resbuf, *tmp;
    EliSym_t       *symtmp;

    if (argsempty) {
	if (!(curarg = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	    return (pushed);
	EliSexp_SetSym(st, curarg, EliNilSym(st));
    }
    for (i = 0; i < varlen; ++i) {
	curvar = EliCons_GetCar(varptr);
	if (EliSexp_GetType(curvar) != e_data_symbol) {
	    EliError(st, ELI_ERR_BAD_PARAM, curvar, "INTERNAL [eliEvalAndBind (non-symbol in var list)]", 0);
	    return (pushed);
	}
	if (!argsempty)
	    curarg = EliCons_GetCar(argptr);
	if (!(resbuf = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	    return (pushed);
	eliEval(st, curarg, resbuf);
	if (EliErr_ErrP(st))
	    return (pushed);
	if (!(symtmp = eliSym_GetNew_trace(st, EliTraceStk(st), EliSym_GetName(EliSexp_GetSym(curvar)))))
	    return (pushed);
	EliSym_BindSexp(st, symtmp, resbuf);
        if (!eliEvalStk_Push(st, s, symtmp)) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliEvalAndBind (pushing symbol)]", 0);
            return (pushed);
        }
	++pushed;
	if (i < varlen - 1) {
	    tmp = EliCons_GetCdr(varptr);
	    varptr = EliSexp_GetCons(tmp);
	    if (!argsempty) {
		tmp = EliCons_GetCdr(argptr);
		if (EliSexp_GetType(tmp) == e_data_none) {
		    argsempty = TRUE;
		    if (!(curarg = eliSexp_GetNew_trace(st, EliTraceStk(st))))
			return (pushed);
		    EliSexp_SetSym(st, curarg, EliNilSym(st));
		}
		else
		    argptr = EliSexp_GetCons(tmp);
	    }
	}
    }
    return (pushed);
}

int             eliBind(st, s, varlist, arglist)
EliState_t     *st;
eliEvalStack_t *s;
EliCons_t      *varlist, *arglist;
{
    int             pushed = 0, varlen = EliListLen(varlist), argsempty = !EliListLen(arglist), i;
    EliCons_t      *varptr = varlist, *argptr = arglist;
    EliSexp_t      *curvar, *curarg = NULL, *tmp;
    EliSym_t       *symtmp;

    if (argsempty) {
	if (!(curarg = eliSexp_GetNew_trace(st, EliTraceStk(st))))
	    return (pushed);
	EliSexp_SetSym(st, curarg, EliNilSym(st));
    }
    for (i = 0; i < varlen; ++i) {
	curvar = EliCons_GetCar(varptr);
	if (EliSexp_GetType(curvar) != e_data_symbol) {
	    EliError(st, ELI_ERR_BAD_PARAM, curvar, "INTERNAL [eliBind (non-symbol in var list)]", 0);
	    return (pushed);
	}
	if (!argsempty)
	    curarg = EliCons_GetCar(argptr);
	if (!(symtmp = eliSym_GetNew_trace(st, EliTraceStk(st), EliSym_GetName(EliSexp_GetSym(curvar)))))
	    return (pushed);
	EliSym_BindSexp(st, symtmp, curarg);
        if (!eliEvalStk_Push(st, s, symtmp)) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliBind (pushing symbol)]", 0);
            return (pushed);
        }
	++pushed;
	if (i < varlen - 1) {
	    tmp = EliCons_GetCdr(varptr);
	    varptr = EliSexp_GetCons(tmp);
	    if (!argsempty) {
		tmp = EliCons_GetCdr(argptr);
		if (EliSexp_GetType(tmp) == e_data_none) {
		    argsempty = TRUE;
		    if (!(curarg = eliSexp_GetNew_trace(st, EliTraceStk(st))))
			return (pushed);
		    EliSexp_SetSym(st, curarg, EliNilSym(st));
		}
		else
		    argptr = EliSexp_GetCons(tmp);
	    }
	}
    }
    return (pushed);
}

/* pop n items off the stack */

void            eliEvalStk_PopN(st, s, n)
EliState_t     *st;
eliEvalStack_t *s;
int             n;
{
    int             i;

    for (i = 0; i < n; ++i)
	eliEvalStk_Pop(st, s);
}
