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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/prims2.c,v 1.7 1993/02/04 15:32:52 rr2b Exp $";
#endif

#include  <prmtives.h>
#include <sys/errno.h>

extern char *sys_errlist[];

void            Prim_SETQ(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *argptr1, *argptr2, *tmp, *tmperr, *tmp2;
    EliSym_t       *symtmp, *symtmp2;

    EliDebug(20, "Entering primitive SETQ", st, FALSE);
    if (2 != EliListLen(arglist)) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [SETQ (checking arglist size)]", 0);
        return;
    }
    argptr1 = EliCons_GetCar(arglist);
    if (EliSexp_GetType(argptr1) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, argptr1, "ELI-PRIMITIVE [SETQ (1st arg not a symbol)]", 0);
        return;
    }
    symtmp = EliSexp_GetSym(argptr1);
    tmp = EliCons_GetCdr(arglist);
    if (!(argptr2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, EliCons_GetCar(EliSexp_GetCons(tmp)), argptr2);
    if (EliErr_ErrP(st))
        return;
    if (symtmp2 = EliEvalStk_FindSym(st, EliStr_GetString(EliSym_GetName(symtmp)))) {
        EliSym_BindSexp(st, symtmp2, argptr2);
        EliSexp_SetSexp(st, resbuf, argptr2);
    }
    else {
        EliSym_BindSexp(st, symtmp, argptr2);
        if (eliSym_GetScope(symtmp) == e_sym_known) {
            if (!(tmp2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
            eliSym_SetScope(symtmp, e_sym_global);
            EliSexp_SetSym(st, tmp2, symtmp);
            eliHT_Insert(st, EliSymbolTable(st), tmp2, EliStr_GetString(EliSym_GetName(symtmp)));
            eliHT_Delete(st, EliTempSymTable(st), EliStr_GetString(EliSym_GetName(symtmp)));
        }
        switch (EliSexp_GetType(argptr2)) {
            case e_data_integer:
                EliSexp_SetInt(st, resbuf, EliSexp_GetInt(argptr2));
                break;
            case e_data_string:
                EliSexp_SetStr(st, resbuf, EliSexp_GetStr(argptr2));
                break;
            case e_data_symbol:
                EliSexp_SetSym(st, resbuf, EliSexp_GetSym(argptr2));
                break;
            case e_data_list:
                EliSexp_SetCons(st, resbuf, EliSexp_GetCons(argptr2));
                break;
            case e_data_fn:
                EliSexp_SetFn(st, resbuf, EliSexp_GetFn(argptr2));
                break;
        }
    }
}


/* Here is the definition for "Prim_PLUS", used above */

void            Prim_PLUS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliCons_t      *argptr = arglist;
    EliSexp_t      *curarg, *evalarg, *tmp, *tmperr;
    int             args = EliListLen(arglist), i;
    long            result = 0L;

    EliDebug(20, "Entering primitive PLUS", st, FALSE);
    if (!args) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [PLUS (checking arglist size)]", 0);
        return;
    }
    for (i = 0; i < args; ++i) {
        curarg = EliCons_GetCar(argptr);
        if (!(evalarg = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        eliEval(st, curarg, evalarg);
        if (EliErr_ErrP(st))
            return;
        if (EliSexp_GetType(evalarg) != e_data_integer) {
            EliError(st, ELI_ERR_BAD_ARGS, evalarg, "ELI-PRIMITIVE [PLUS (an arg is not an int)]", 0);
            return;
        }
        result += EliSexp_GetInt(evalarg);
        if (i < args - 1) {
            tmp = EliCons_GetCdr(argptr);
            argptr = EliSexp_GetCons(tmp);
        }
    }
    EliSexp_SetInt(st, resbuf, result);
}

void            Prim_DEFUN(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *namearg, *argsarg, *bodyarg, *nodetmp, *node[2], *tmperr, *tmp2;
    EliCons_t      *tmp, *cell[3];
    EliSym_t       *symtmp;
    EliFn_t        *fn;
    int             i;

    EliDebug(20, "Entering primitive DEFUN", st, FALSE);
    if (3 != EliListLen(arglist)) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [DEFUN (checking arglist size)]", 0);
        return;
    }
    namearg = EliCons_GetCar(arglist);
    nodetmp = EliCons_GetCdr(arglist);
    argsarg = EliCons_GetCar(tmp = EliSexp_GetCons(nodetmp));
    nodetmp = EliCons_GetCdr(tmp);
    bodyarg = EliCons_GetCar(EliSexp_GetCons(nodetmp));
    if (EliSexp_GetType(namearg) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, namearg, "ELI-PRIMITIVE [DEFUN (1st arg not a symbol)]", 0);
        return;
    }
    if ((EliSexp_GetType(argsarg) != e_data_list) && !EliNilP(st, argsarg)) {
        EliError(st, ELI_ERR_BAD_ARGS, argsarg, "ELI-PRIMITIVE [DEFUN (2nd arg not a list)]", 0);
        return;
    }
    for (i = 0; i < 3; ++i) {
        if (!(cell[i] = eliCons_GetNew_trace(st, EliTraceStk(st))))
            return;
        if (i > 0) {
            if (!(node[i - 1] = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
            EliSexp_SetCons(st, node[i - 1], cell[i]);
            EliCons_BindCdr(st, cell[i - 1], node[i - 1]);
        }
    }
    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    EliSexp_SetSym(st, nodetmp, EliLambdaSym(st));
    EliCons_BindCar(st, cell[0], nodetmp);
    EliCons_BindCar(st, cell[1], argsarg);
    EliCons_BindCar(st, cell[2], bodyarg);
    if (!(fn = eliFn_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliFn_SetCons(st, fn, cell[0]);
    symtmp = EliSexp_GetSym(namearg);
    if (EliErr_ErrP(st))
        return;
    EliSym_BindFn(st, symtmp, fn);
    if (eliSym_GetScope(symtmp) == e_sym_known) {
        if (!(tmp2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        eliSym_SetScope(symtmp, e_sym_global);
        EliSexp_SetSym(st, tmp2, symtmp);
        eliHT_Insert(st, EliSymbolTable(st), tmp2, EliStr_GetString(EliSym_GetName(symtmp)));
        eliHT_Delete(st, EliTempSymTable(st), EliStr_GetString(EliSym_GetName(symtmp)));
    }
    EliSexp_SetSym(st, resbuf, symtmp);
}

void            Prim_DEFUNQ(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *namearg, *argsarg, *bodyarg, *nodetmp, *node[2], *tmperr, *tmp2;
    EliCons_t      *tmp, *cell[3];
    EliSym_t       *symtmp;
    EliFn_t        *fn;
    int             i;

    EliDebug(20, "Entering primitive DEFUNQ", st, FALSE);
    if (3 != EliListLen(arglist)) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [DEFUNQ (checking arglist size)]", 0);
        return;
    }
    namearg = EliCons_GetCar(arglist);
    nodetmp = EliCons_GetCdr(arglist);
    argsarg = EliCons_GetCar(tmp = EliSexp_GetCons(nodetmp));
    nodetmp = EliCons_GetCdr(tmp);
    bodyarg = EliCons_GetCar(EliSexp_GetCons(nodetmp));
    if (EliSexp_GetType(namearg) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, namearg, "ELI-PRIMITIVE [DEFUNQ (1st arg not a symbol)]", 0);
        return;
    }
    if ((EliSexp_GetType(argsarg) != e_data_list) && !EliNilP(st, argsarg)) {
        EliError(st, ELI_ERR_BAD_ARGS, argsarg, "ELI-PRIMITIVE [DEFUNQ (2nd arg not a list)]", 0);
        return;
    }
    for (i = 0; i < 3; ++i) {
        if (!(cell[i] = eliCons_GetNew_trace(st, EliTraceStk(st))))
            return;
        if (i > 0) {
            if (!(node[i - 1] = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
            EliSexp_SetCons(st, node[i - 1], cell[i]);
            EliCons_BindCdr(st, cell[i - 1], node[i - 1]);
        }
    }
    if (!(nodetmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    EliSexp_SetSym(st, nodetmp, EliLambdaqSym(st));
    EliCons_BindCar(st, cell[0], nodetmp);
    EliCons_BindCar(st, cell[1], argsarg);
    EliCons_BindCar(st, cell[2], bodyarg);
    if (!(fn = eliFn_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliFn_SetCons(st, fn, cell[0]);
    symtmp = EliSexp_GetSym(namearg);
    if (EliErr_ErrP(st))
        return;
    EliSym_BindFn(st, symtmp, fn);
    if (eliSym_GetScope(symtmp) == e_sym_known) {
        if (!(tmp2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        eliSym_SetScope(symtmp, e_sym_global);
        EliSexp_SetSym(st, tmp2, symtmp);
        eliHT_Insert(st, EliSymbolTable(st), tmp2, EliStr_GetString(EliSym_GetName(symtmp)));
        eliHT_Delete(st, EliTempSymTable(st), EliStr_GetString(EliSym_GetName(symtmp)));
    }
    eliSym_SetScope(symtmp, e_sym_global);
    EliSexp_SetSym(st, resbuf, symtmp);
}

void            Prim_CONS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *arg1, *arg2, *evalarg1, *evalarg2, *nodetmp, *tmperr;
    EliCons_t      *tmp;

    EliDebug(20, "Entering primitive CONS", st, FALSE);
    if (2 != EliListLen(arglist)) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [CONS (checking arglist size)]", 0);
        return;
    }
    arg1 = EliCons_GetCar(arglist);
    nodetmp = EliCons_GetCdr(arglist);
    arg2 = EliCons_GetCar(EliSexp_GetCons(nodetmp));
    if (!(evalarg1 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    if (!(evalarg2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, arg1, evalarg1);
    if (EliErr_ErrP(st))
        return;
    eliEval(st, arg2, evalarg2);
    if (EliErr_ErrP(st))
        return;
    if ((EliSexp_GetType(evalarg2) != e_data_list) && !EliNilP(st, evalarg2)) {
        EliError(st, ELI_ERR_BAD_ARGS, evalarg2, "ELI-PRIMITIVE [CONS (2nd arg not a list)]", 0);
        return;
    }
    if (!(tmp = eliCons_GetNew_trace(st, EliTraceStk(st))))
        return;
    EliCons_BindCar(st, tmp, evalarg1);
    if (!EliNilP(st, evalarg2))
        EliCons_BindCdr(st, tmp, evalarg2);
    EliSexp_SetCons(st, resbuf, tmp);
}


/*
 * Definition of PROGN, which evaluates each of its arbitrarily-many
 * arguments in turn, and returns the value of the last one evaluated
 */

void            Prim_PROGN(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmp, *evaltmp;
    EliCons_t      *argptr = arglist;
    int             i, len = EliListLen(arglist);

    EliDebug(20, "Entering primitive PROGN", st, FALSE);
    for (i = 0; i < len - 1; ++i) {
        if (!(evaltmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        eliEval(st, EliCons_GetCar(argptr), evaltmp);
        if (EliErr_ErrP(st))
            return;
        tmp = EliCons_GetCdr(argptr);
        argptr = EliSexp_GetCons(tmp);
    }
    eliEval(st, EliCons_GetCar(argptr), resbuf);
    if (EliErr_ErrP(st))
        return;                        /* Of course, you realize how
                                        * ridiculous this is */
}

/* You know what this one does */

void            Prim_EVAL(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmp, *tmperr;

    EliDebug(20, "Entering primitive EVAL", st, FALSE);
    if (EliListLen(arglist) != 1) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [EVAL (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, EliCons_GetCar(arglist), tmp);
    if (EliErr_ErrP(st))
        return;
    eliEval(st, tmp, resbuf);
    if (EliErr_ErrP(st))
        return;
}


void            Prim_CAR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmp, *tmp2, *tmperr;

    EliDebug(20, "Entering primitive CAR", st, FALSE);
    if (EliListLen(arglist) != 1) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [CAR (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, EliCons_GetCar(arglist), tmp);
    if (EliErr_ErrP(st))
        return;
    if ((EliSexp_GetType(tmp) != e_data_list) && !EliNilP(st, tmp)) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [CAR (arg not a list)]", 0);
        return;
    }
    if (EliSexp_GetType(tmp) == e_data_list) {
        tmp2 = EliCons_GetCar(EliSexp_GetCons(tmp));
        switch (EliSexp_GetType(tmp2)) {
            case e_data_integer:
                EliSexp_SetInt(st, resbuf, EliSexp_GetInt(tmp2));
                break;
            case e_data_symbol:
                EliSexp_SetSym(st, resbuf, EliSexp_GetSym(tmp2));
                break;
            case e_data_string:
                EliSexp_SetStr(st, resbuf, EliSexp_GetStr(tmp2));
                break;
            case e_data_list:
                EliSexp_SetCons(st, resbuf, EliSexp_GetCons(tmp2));
                break;
            case e_data_fn:
                EliSexp_SetFn(st, resbuf, EliSexp_GetFn(tmp2));
                break;
            case e_data_none:
                EliSexp_SetSym(st, resbuf, EliNilSym(st));
                break;
        }
    }
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_CDR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmp, *tmp2, *tmperr;

    EliDebug(20, "Entering primitive CDR", st, FALSE);
    if (EliListLen(arglist) != 1) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [CDR (checking arglist size)]", 0);
        return;
    }
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, EliCons_GetCar(arglist), tmp);
    if (EliErr_ErrP(st))
        return;
    if ((EliSexp_GetType(tmp) != e_data_list) && !EliNilP(st, tmp)) {
        EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [CDR (arg not a list)]", 0);
        return;
    }
    if (EliSexp_GetType(tmp) == e_data_list) {
        tmp2 = EliCons_GetCdr(EliSexp_GetCons(tmp));
        switch (EliSexp_GetType(tmp2)) {
            case e_data_integer:
                EliSexp_SetInt(st, resbuf, EliSexp_GetInt(tmp2));
                break;
            case e_data_symbol:
                EliSexp_SetSym(st, resbuf, EliSexp_GetSym(tmp2));
                break;
            case e_data_string:
                EliSexp_SetStr(st, resbuf, EliSexp_GetStr(tmp2));
                break;
            case e_data_list:
                EliSexp_SetCons(st, resbuf, EliSexp_GetCons(tmp2));
                break;
            case e_data_fn:
                EliSexp_SetFn(st, resbuf, EliSexp_GetFn(tmp2));
                break;
            case e_data_none:
                EliSexp_SetSym(st, resbuf, EliNilSym(st));
                break;
        }
    }
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}


/* Creates a list from its arbitrarily-many arguments */

void            Prim_LIST(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmp, *tmp2;
    EliCons_t      *argptr = arglist, *prevcell, *curcell, *result;
    int             len = EliListLen(arglist), i, resultlen = 0;

    EliDebug(20, "Entering primitive LIST", st, FALSE);
    if (!(prevcell = result = eliCons_GetNew_trace(st, EliTraceStk(st))))
        return;
    for (i = 0; i < len; ++i) {
        if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        eliEval(st, EliCons_GetCar(argptr), tmp);
        if (EliErr_ErrP(st))
            return;
        if (!(resultlen++))
            EliCons_BindCar(st, result, tmp);
        else {
            if (!(curcell = eliCons_GetNew_trace(st, EliTraceStk(st))))
                return;
            EliCons_BindCar(st, curcell, tmp);
            if (!(tmp2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
            EliSexp_SetCons(st, tmp2, curcell);
            EliCons_BindCdr(st, prevcell, tmp2);
            prevcell = curcell;
        }
        if (i < len - 1) {
            tmp = EliCons_GetCdr(argptr);
            argptr = EliSexp_GetCons(tmp);
        }
    }
    EliSexp_SetCons(st, resbuf, result);
}

void            Prim_COND(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    int             i, len = EliListLen(arglist), untrue = TRUE;
    EliCons_t      *argptr = arglist;
    EliSexp_t      *tmp, *tmp2, *testnode, *actnode;

    EliDebug(20, "Entering primitive COND", st, FALSE);
    EliSexp_SetSym(st, resbuf, EliNilSym(st));  /* Default if no case gets
                                                 * EliEval'd */
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    for (i = 0; (i < len) && untrue; ++i) {
        testnode = EliCons_GetCar(argptr);
        if (EliSexp_GetType(testnode) != e_data_list) {
            EliError(st, ELI_ERR_BAD_ARGS, testnode, "ELI-PRIMITIVE [COND (an arg is not a list)]", 0);
            return;
        }
        if (2 != EliListLen(EliSexp_GetCons(testnode))) {
            EliError(st, ELI_ERR_BAD_ARGS, testnode, "ELI-PRIMITIVE [COND (an arg is not a 2-element list)]", 0);
            return;
        }
        eliEval(st, EliCons_GetCar(EliSexp_GetCons(testnode)), tmp);
        if (EliErr_ErrP(st))
            return;
        if (!EliNilP(st, tmp)) {
            untrue = FALSE;
            actnode = EliCons_GetCdr(EliSexp_GetCons(testnode));
            eliEval(st, EliCons_GetCar(EliSexp_GetCons(actnode)), resbuf);
            if (EliErr_ErrP(st))
                return;
        }
        else {
            if (i < len - 1) {
                tmp2 = EliCons_GetCdr(argptr);
                argptr = EliSexp_GetCons(tmp2);
            }
        }
    }
}

void            Prim_PRINT(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmp;

    EliDebug(20, "Entering primitive PRINT", st, FALSE);
    if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, EliCons_GetCar(arglist), tmp);
    if (EliErr_ErrP(st))
        return;
    switch (EliSexp_GetType(tmp)) {
        case e_data_integer:
            EliSexp_SetInt(st, resbuf, EliSexp_GetInt(tmp));
            break;
        case e_data_string:
            EliSexp_SetStr(st, resbuf, EliSexp_GetStr(tmp));
            break;
        case e_data_symbol:
            EliSexp_SetSym(st, resbuf, EliSexp_GetSym(tmp));
            break;
        case e_data_list:
            EliSexp_SetCons(st, resbuf, EliSexp_GetCons(tmp));
            break;
        case e_data_fn:
            EliSexp_SetFn(st, resbuf, EliSexp_GetFn(tmp));
            break;
    }
    EliDisplaySexp(tmp);
}

void            Prim_TERPRI(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliDebug(20, "Entering primitive TERPRI", st, FALSE);
    EliSexp_SetSym(st, resbuf, EliTSym(st));    /* Always return true */
    putchar('\n');
}

void            Prim_EQ(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmperr, *tmpnode, *node1, *node2, *evalnode1, *evalnode2;

    EliDebug(20, "Entering primitive EQ", st, FALSE);
    if (EliListLen(arglist) != 2) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [EQ (checking arglist size)]", 0);
        return;
    }
    node1 = EliCons_GetCar(arglist);
    tmpnode = EliCons_GetCdr(arglist);
    node2 = EliCons_GetCar(EliSexp_GetCons(tmpnode));
    if (!(evalnode1 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, node1, evalnode1);
    if (EliErr_ErrP(st))
        return;
    if (!(evalnode2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, node2, evalnode2);
    if (EliErr_ErrP(st))
        return;
    if (EliSexpEq(evalnode1, evalnode2))
        EliSexp_SetSym(st, resbuf, EliTSym(st));
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_STRCONTAINS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *err, *evalNode, *args[3];
    char           *str1, *str2;
    int             ignoreCase = FALSE, numArgs;

    EliDebug(20, "Entering primitive STRCONTAINS", st, FALSE);
    if ((numArgs = EliGetListCars(arglist, args, 3)) < 2) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [STRCONTAINS (checking arglist size)]", 0);
        return;
    }
    if (!(evalNode = EliEval(st, args[0])))
        return;
    if (EliSexp_GetType(evalNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, evalNode, "ELI-PRIMITIVE [STRCONTAINS (1st arg not a string)]", 0);
        return;
    }
    str1 = EliStr_GetString(EliSexp_GetStr(evalNode));

    if (!(evalNode = EliEval(st, args[1])))
        return;
    if (EliSexp_GetType(evalNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, evalNode, "ELI-PRIMITIVE [STRCONTAINS (2nd arg not a string)]", 0);
        return;
    }
    str2 = EliStr_GetString(EliSexp_GetStr(evalNode));

    if (numArgs == 3) {
        if (!(evalNode = EliEval(st, args[2])))
            return;
        ignoreCase = !EliNilP(st, evalNode);
    }

    if (eliFindPrefix(str1, str2, ignoreCase))
        EliSexp_SetSym(st, resbuf, EliTSym(st));
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_ASSOC(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmperr, *tmpnode, *node1, *node2, *evalnode1, *evalnode2;
    int             found, i, len;
    EliCons_t      *consptr, *assoclist;

    EliDebug(20, "Entering primitive ASSOC", st, FALSE);
    EliSexp_SetSym(st, resbuf, EliNilSym(st));  /* Default */
    if (EliListLen(arglist) != 2) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [ASSOC (checking arglist size)]", 0);
        return;
    }
    node1 = EliCons_GetCar(arglist);
    tmpnode = EliCons_GetCdr(arglist);
    node2 = EliCons_GetCar(EliSexp_GetCons(tmpnode));
    if (!(evalnode1 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, node1, evalnode1);
    if (EliErr_ErrP(st))
        return;
    if (!(evalnode2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, node2, evalnode2);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(evalnode2) != e_data_list) {
        EliError(st, ELI_ERR_BAD_ARGS, evalnode2, "ELI-PRIMITIVE [ASSOC (2nd arg not a list)]", 0);
        return;
    }

    /*
     * Now we have some node evalnode1, and a list evalnode2.  What are you
     * gonna do about it?
     */

    consptr = assoclist = EliSexp_GetCons(evalnode2);
    len = EliListLen(assoclist);
    found = FALSE;
    for (i = 0; (i < len - 1) && !found; ++i) {
        tmpnode = EliCons_GetCar(consptr);
        if (EliSexp_GetType(tmpnode) == e_data_list)
            if (EliSexpEqual(st, evalnode1, EliCons_GetCar(EliSexp_GetCons(tmpnode))))
                found = TRUE;
        if (!found) {
            tmpnode = EliCons_GetCdr(consptr);
            consptr = EliSexp_GetCons(tmpnode);
        }
    }
    if (!found) {
        tmpnode = EliCons_GetCar(consptr);
        if (EliSexp_GetType(tmpnode) == e_data_list)
            if (EliSexpEqual(st, evalnode1, EliCons_GetCar(EliSexp_GetCons(tmpnode))))
                found = TRUE;
    }
    if (found)
        EliSexp_SetCons(st, resbuf, EliSexp_GetCons(tmpnode));
}

void            Prim_STRSTARTS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmperr, *tmpnode, *node1, *node2, *evalnode1, *evalnode2;
    char           *str1, *str2;
    int             len;

    EliDebug(20, "Entering primitive STRSTARTS", st, FALSE);
    if (EliListLen(arglist) != 2) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [STRSTARTS (checking arglist size)]", 0);
        return;
    }
    node1 = EliCons_GetCar(arglist);
    tmpnode = EliCons_GetCdr(arglist);
    node2 = EliCons_GetCar(EliSexp_GetCons(tmpnode));
    if (!(evalnode1 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, node1, evalnode1);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(evalnode1) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, evalnode1, "ELI-PRIMITIVE [STRSTARTS (1st arg not a string)]", 0);
        return;
    }
    if (!(evalnode2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, node2, evalnode2);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(evalnode2) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, evalnode2, "ELI-PRIMITIVE [STRSTARTS (2nd arg not a string)]", 0);
        return;
    }

    /*
     * Now we have evalnode1 and evalnode2 both pointing to strnodes.  Is
     * evalnode1 the start of evalnode2?
     */

    str1 = EliStr_GetString(EliSexp_GetStr(evalnode1));
    str2 = EliStr_GetString(EliSexp_GetStr(evalnode2));
    len = strlen(str1);
    if (strncmp(str1, str2, len))
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    else
        EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            Prim_LETSTAR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliCons_t      *bindings, *bindingsptr, *bindval, *thisbinding, *constmp;
    EliSexp_t      *nodetmp, *expr, *evalresult, *tmperr;
    EliSym_t       *varname, *thevar;
    int             numbindings, i, numargs;

    EliDebug(20, "Entering primitive LET*", st, FALSE);
    numargs = EliListLen(arglist);
    if ((numargs < 1) || (numargs > 2)) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [LET* (checking arglist size)]", 0);
        return;
    }
    nodetmp = EliCons_GetCar(arglist);
    if (EliSexp_GetType(nodetmp) != e_data_list) {
        EliError(st, ELI_ERR_BAD_ARGS, nodetmp, "ELI-PRIMITIVE [LET* (1st arg not a list)]", 0);
        return;
    }
    bindingsptr = bindings = EliSexp_GetCons(nodetmp);
    numbindings = EliListLen(bindings);
    for (i = 0; i < numbindings; ++i) {
        nodetmp = EliCons_GetCar(bindingsptr);
        if (EliSexp_GetType(nodetmp) != e_data_list) {
            EliError(st, ELI_ERR_BAD_ARGS, nodetmp, "ELI-PRIMITIVE [LET* (1st arg contains a non-list member)]", 0);
            eliEvalStk_PopN(st, EliEvalStack(st), i);
            return;
        }
        thisbinding = EliSexp_GetCons(nodetmp);
        if (EliListLen(thisbinding) != 2) {
            EliError(st, ELI_ERR_BAD_ARGS, nodetmp, "ELI-PRIMITIVE [LET* (1st arg contains a non-2-element list)]", 0);
            eliEvalStk_PopN(st, EliEvalStack(st), i);
            return;
        }
        nodetmp = EliCons_GetCar(thisbinding);
        if (EliSexp_GetType(nodetmp) != e_data_symbol) {
            EliError(st, ELI_ERR_BAD_ARGS, nodetmp, "ELI-PRIMITIVE [LET* (1st arg contains a binding to a non-symbol)]", 0);
            eliEvalStk_PopN(st, EliEvalStack(st), i);
            return;
        }
        varname = EliSexp_GetSym(nodetmp);
        if (!(thevar = eliSym_GetNew_trace(st, EliTraceStk(st), EliSym_GetName(varname)))) {
            eliEvalStk_PopN(st, EliEvalStack(st), i);
            return;
        }
        nodetmp = EliCons_GetCdr(thisbinding);
        bindval = EliSexp_GetCons(nodetmp);
        nodetmp = EliCons_GetCar(bindval);
        if (!(evalresult = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
            eliEvalStk_PopN(st, EliEvalStack(st), i);
            return;
        }
        eliEval(st, nodetmp, evalresult);
        if (EliErr_ErrP(st)) {
            eliEvalStk_PopN(st, EliEvalStack(st), i);
            return;
        }
        EliSym_BindSexp(st, thevar, evalresult);
        if (!eliEvalStk_Push(st, EliEvalStack(st), thevar)) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [LET* (pushing symbol)]", 0);
            eliEvalStk_PopN(st, EliEvalStack(st), i);
            return;
        }
        if (i < numbindings - 1) {
            nodetmp = EliCons_GetCdr(bindingsptr);
            bindingsptr = EliSexp_GetCons(nodetmp);
        }
    }
    if (numargs == 2) {                /* If there's a body to evaluate... */
        nodetmp = EliCons_GetCdr(arglist);
        constmp = EliSexp_GetCons(nodetmp);
        expr = EliCons_GetCar(constmp);
        eliEval(st, expr, resbuf);     /* No need to check errp here, since
                                        * nothing different would happen in
                                        * handling the error anyway */
    }
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    eliEvalStk_PopN(st, EliEvalStack(st), numbindings);
}

void            Prim_AND(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    int             len = EliListLen(arglist), i;
    EliSexp_t      *restmp, *tmp;
    EliCons_t      *constmp = arglist;

    EliDebug(20, "Entering primitive AND", st, FALSE);
    if (!(restmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    for (i = 0; i < len; ++i) {
        tmp = EliCons_GetCar(constmp);
        eliEval(st, tmp, restmp);
        if (EliErr_ErrP(st))
            return;
        if (EliNilP(st, restmp))
            i = len;
        if (i < len - 1) {
            tmp = EliCons_GetCdr(constmp);
            constmp = EliSexp_GetCons(tmp);
        }
    }
    EliSexp_SetSexp(st, resbuf, restmp);
}

void            Prim_OR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    int             len = EliListLen(arglist), i;
    EliSexp_t      *restmp, *tmp;
    EliCons_t      *constmp = arglist;

    EliDebug(20, "Entering primitive OR", st, FALSE);
    if (!(restmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    for (i = 0; i < len; ++i) {
        tmp = EliCons_GetCar(constmp);
        eliEval(st, tmp, restmp);
        if (EliErr_ErrP(st))
            return;
        if (!EliNilP(st, restmp))
            i = len;
        if (i < len - 1) {
            tmp = EliCons_GetCdr(constmp);
            constmp = EliSexp_GetCons(tmp);
        }
    }
    EliSexp_SetSexp(st, resbuf, restmp);
}

void            Prim_NOT(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *tmperr, *tmp, *restmp;

    EliDebug(20, "Entering primitive NOT", st, FALSE);
    if (1 != EliListLen(arglist)) {
        if (!(tmperr = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, tmperr, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, tmperr, "ELI-PRIMITIVE [NOT (checking arglist size)]", 0);
    }
    if (!(restmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    tmp = EliCons_GetCar(arglist);
    eliEval(st, tmp, restmp);
    if (EliErr_ErrP(st))
        return;
    EliSexp_SetSym(st, resbuf, EliNilP(st, restmp) ? EliTSym(st) : EliNilSym(st));
}

void            Prim_RE_STRDECOMPOSEPLUS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[3], *err, *patNode, *refNode, *strs[3], *cdrs[2], *aNode;
    int             numargs, rxpResult, i;
    char           *pat, *ref, tempChar, *hold;
    EliStr_t       *strNodes[3], *aStr;
    EliCons_t      *consCells[3], *subListHead, *subListPtr, *aCell;
    regexp         *rptr, *reg_comp();

    EliDebug(20, "Entering primitive RE-STRDECOMPOSE+", st, FALSE);
    numargs = EliGetListCars(arglist, args, 2);
    if (numargs < 2) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [RE-STRDECOMPOSE+ (checking arglist size)]", 0);
        return;
    }
    if (!(patNode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[0], patNode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(patNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, patNode, "ELI-PRIMITIVE [RE-STRDECOMPOSE+ (1st arg not a string)]", 0);
        return;
    }
    pat = EliStr_GetString(EliSexp_GetStr(patNode));
    if (!(refNode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliEval(st, args[1], refNode);
    if (EliErr_ErrP(st))
        return;
    if (EliSexp_GetType(refNode) != e_data_string) {
        EliError(st, ELI_ERR_BAD_ARGS, refNode, "ELI-PRIMITIVE [RE-STRDECOMPOSE+ (2nd arg not a string)]", 0);
        return;
    }
    ref = EliStr_GetString(EliSexp_GetStr(refNode));
    if (!(rptr = reg_comp(pat))) {
        EliError(st, ELI_ERR_BAD_ARGS, patNode, "ELI-PRIMITIVE [RE-STRDECOMPOSE+ (compiling regular expression)]", 0);
        return;
    }
    rxpResult = reg_exec(rptr, ref);
    if (rxpResult) {
        tempChar = *(rptr->startp[0]);
        *(rptr->startp[0]) = '\0';
        if (!(hold = EliSaveString(ref))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [RE-STRDECOMPOSE+ (allocating space for 1st result string)]", 0);
            free(rptr);
            return;
        }
        *(rptr->startp[0]) = tempChar;
        strNodes[0] = eliStringTable_FindOrMake(st, EliStringTable(st), hold);
        free(hold);
        if (!(strNodes[0])) {
            free(rptr);
            return;
        }
        tempChar = *(rptr->endp[0]);
        *(rptr->endp[0]) = '\0';
        if (!(hold = EliSaveString(rptr->startp[0]))) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [RE-STRDECOMPOSE+ (allocating space for 2nd result string)]", 0);
            free(rptr);
            return;
        }
        *(rptr->endp[0]) = tempChar;
        strNodes[1] = eliStringTable_FindOrMake(st, EliStringTable(st), hold);
        free(hold);
        if (!(strNodes[1])) {
            free(rptr);
            return;
        }
        if (!(strNodes[2] = eliStringTable_FindOrMake(st, EliStringTable(st), rptr->endp[0]))) {
            free(rptr);
            return;
        }

        if (!(subListPtr = subListHead = eliCons_GetNew_trace(st, EliTraceStk(st)))) {
            free(rptr);
            return;
        }
        if (!(aNode = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
            free(rptr);
            return;
        }
        EliSexp_SetStr(st, aNode, strNodes[1]);
        EliCons_BindCar(st, subListHead, aNode);
        for (i = 1; (i < 10) && rptr->startp[i]; ++i) {
            if (!(aCell = eliCons_GetNew_trace(st, EliTraceStk(st)))) {
                free(rptr);
                return;
            }
            if (!(aNode = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
                free(rptr);
                return;
            }
            EliSexp_SetCons(st, aNode, aCell);
            EliCons_BindCdr(st, subListPtr, aNode);
            subListPtr = aCell;
            tempChar = *(rptr->endp[i]);
            *(rptr->endp[i]) = '\0';
            if (!(hold = EliSaveString(rptr->startp[i]))) {
                EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [RE-STRDECOMPOSE+ (allocating space for a submatch)]", 0);
                free(rptr);
                return;
            }
            *(rptr->endp[i]) = tempChar;
            aStr = eliStringTable_FindOrMake(st, EliStringTable(st), hold);
            free(hold);
            if (!aStr) {
                free(rptr);
                return;
            }
            if (!(aNode = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
                free(rptr);
                return;
            }
            EliSexp_SetStr(st, aNode, aStr);
            EliCons_BindCar(st, aCell, aNode);
        }
        free(rptr);

        if ((!(strs[0] = eliSexp_GetNew_trace(st, EliTraceStk(st)))) || (!(strs[1] = eliSexp_GetNew_trace(st, EliTraceStk(st)))) || (!(strs[2] = eliSexp_GetNew_trace(st, EliTraceStk(st)))))
            return;

        EliSexp_SetStr(st, strs[0], strNodes[0]);
        EliSexp_SetCons(st, strs[1], subListHead);
        EliSexp_SetStr(st, strs[2], strNodes[2]);

        if ((!(consCells[0] = eliCons_GetNew_trace(st, EliTraceStk(st)))) || (!(consCells[1] = eliCons_GetNew_trace(st, EliTraceStk(st)))) || (!(consCells[2] = eliCons_GetNew_trace(st, EliTraceStk(st)))))
            return;

        if ((!(cdrs[0] = eliSexp_GetNew_trace(st, EliTraceStk(st)))) || (!(cdrs[1] = eliSexp_GetNew_trace(st, EliTraceStk(st)))))
            return;

        EliSexp_SetCons(st, cdrs[0], consCells[1]);
        EliSexp_SetCons(st, cdrs[1], consCells[2]);
        EliCons_BindCar(st, consCells[0], strs[0]);
        EliCons_BindCdr(st, consCells[0], cdrs[0]);
        EliCons_BindCar(st, consCells[1], strs[1]);
        EliCons_BindCdr(st, consCells[1], cdrs[1]);
        EliCons_BindCar(st, consCells[2], strs[2]);
        EliSexp_SetCons(st, resbuf, consCells[0]);
    }
    else {
        free(rptr);
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    }
}

void            Prim_LET(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *err = NULL, *tmp, *args[2], *bindingDudes[2];
    EliCons_t      *varList = NULL, *valList = NULL, *bindingsList, *bindingsPtr, *thisBinding, *evalVals;
    int             numBindings, i, bound, numargs;
    EliSym_t       *sym;

    EliDebug(20, "Entering primitive LET", st, FALSE);
    if ((numargs = EliGetListCars(arglist, args, 2)) < 1) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [LET (checking arglist size)]", 0);
        return;
    }
    if (EliSexp_GetType(args[0]) != e_data_list) {
        EliError(st, ELI_ERR_BAD_ARGS, args[0], "ELI-PRIMITIVE [LET (1st arg not a list)]", 0);
        return;
    }
    bindingsPtr = bindingsList = EliSexp_GetCons(args[0]);
    if (!(numBindings = EliListLen(bindingsList))) {
        EliError(st, ELI_ERR_BAD_ARGS, args[0], "ELI-PRIMITIVE [LET (1st arg empty)]", 0);
        return;
    }
    for (i = 0; i < numBindings; ++i) {
        if (EliSexp_GetType(tmp = EliCons_GetCar(bindingsPtr)) != e_data_list) {
            EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [LET (non-list in bindings list)]", 0);
            return;
        }
        thisBinding = EliSexp_GetCons(tmp);
        if (EliGetListCars(thisBinding, bindingDudes, 2) != 2) {
            if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
            EliSexp_SetCons(st, err, thisBinding);
            EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [LET (a binding in bindings list does not have 2 elements)]", 0);
            return;
        }
        if (EliSexp_GetType(bindingDudes[0]) != e_data_symbol) {
            EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [LET (bindings list contains a binding to a non-symbol)]", 0);
            return;
        }
        if (!(sym = eliSym_GetNew_trace(st, EliTraceStk(st), EliSym_GetName(EliSexp_GetSym(bindingDudes[0])))))
            return;
        if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetSym(st, tmp, sym);
        if (!(varList = EliAddToList(st, varList, tmp)))
            return;
        if (!(valList = EliAddToList(st, valList, bindingDudes[1])))
            return;
        bindingsPtr = EliGetNextCell(bindingsPtr);
    }

    /*
     * Now varList contains the var names, and valList contains the values.
     * Let's EliEval the values into a new list, then eliBind the results.
     */

    if (!(evalVals = EliEvalListToList(st, valList)))
        return;

    bound = eliBind(st, EliEvalStack(st), varList, evalVals);
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), bound);
        return;
    }

    if (numargs == 2)
        eliEval(st, args[1], resbuf);  /* We don't check for error here since
                                        * in handling one we wouldn't do
                                        * anything different anyway */
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    eliEvalStk_PopN(st, EliEvalStack(st), bound);
}

void            Prim_DO(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *err = NULL, *tmp, *args[3], *bindingDudes[3], *endTest, *returnVal, *tmp2;
    EliCons_t      *varList = NULL, *initList = NULL, *updateList = NULL, *bindingsList, *bindingsPtr, *thisBinding, *initVals, *endAndReturn, *updatePtr, *varPtr, *updateVals;
    int             numBindings, i, bound, numArgs, thisBindingLen, loopy = TRUE;
    EliSym_t       *sym;

    EliDebug(20, "Entering primitive DO", st, FALSE);
    if ((numArgs = EliGetListCars(arglist, args, 3)) < 2) {
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [DO (checking arglist size)]", 0);
        return;
    }
    if (EliSexp_GetType(args[0]) != e_data_list) {
        EliError(st, ELI_ERR_BAD_ARGS, args[0], "ELI-PRIMITIVE [DO (1st arg not a list)]", 0);
        return;
    }
    if (EliSexp_GetType(args[1]) != e_data_list) {
        EliError(st, ELI_ERR_BAD_ARGS, args[1], "ELI-PRIMITIVE [DO (2nd arg not a list)]", 0);
        return;
    }
    endAndReturn = EliSexp_GetCons(args[1]);
    if (EliListLen(endAndReturn) != 2) {
        EliError(st, ELI_ERR_BAD_ARGS, args[1], "ELI-PRIMITIVE [DO (2nd arg does not have 2 elements)]", 0);
        return;
    }
    endTest = EliCons_GetCar(endAndReturn);
    returnVal = EliCons_GetCar(EliSexp_GetCons(EliCons_GetCdr(endAndReturn)));
    bindingsPtr = bindingsList = EliSexp_GetCons(args[0]);
    if (!(numBindings = EliListLen(bindingsList))) {
        EliError(st, ELI_ERR_BAD_ARGS, args[0], "ELI-PRIMITIVE [DO (1st arg empty)]", 0);
        return;
    }
    for (i = 0; i < numBindings; ++i) {
        if (EliSexp_GetType(tmp = EliCons_GetCar(bindingsPtr)) != e_data_list) {
            EliError(st, ELI_ERR_BAD_ARGS, tmp, "ELI-PRIMITIVE [DO (non-list in bindings list)]", 0);
            return;
        }
        thisBinding = EliSexp_GetCons(tmp);
        if ((thisBindingLen = EliGetListCars(thisBinding, bindingDudes, 3)) < 2) {
            if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
            EliSexp_SetCons(st, err, thisBinding);
            EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [DO (a binding in bindings list does not have 2 or 3 elements)]", 0);
            return;
        }
        if (EliSexp_GetType(bindingDudes[0]) != e_data_symbol) {
            EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [DO (bindings list contains a binding to a non-symbol)]", 0);
            return;
        }
        if (!(sym = eliSym_GetNew_trace(st, EliTraceStk(st), EliSym_GetName(EliSexp_GetSym(bindingDudes[0])))))
            return;
        if (!(tmp = eliSexp_GetNew_trace(st, EliTraceStk(st))))
            return;
        EliSexp_SetSym(st, tmp, sym);
        if (!(varList = EliAddToList(st, varList, tmp)))
            return;
        if (!(initList = EliAddToList(st, initList, bindingDudes[1])))
            return;
        if (thisBindingLen < 3)
            if (!(bindingDudes[2] = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                return;
        if (!(updateList = EliAddToList(st, updateList, bindingDudes[2])))
            return;
        bindingsPtr = EliGetNextCell(bindingsPtr);
    }

    if (!(initVals = EliEvalListToList(st, initList)))
        return;

    bound = eliBind(st, EliEvalStack(st), varList, initVals);
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), bound);
        return;
    }

    while (loopy) {
        if (!(tmp = EliEval(st, endTest))) {
            eliEvalStk_PopN(st, EliEvalStack(st), bound);
            return;
        }
        if (EliNilP(st, tmp)) {
            if (numArgs == 3) {
                if (!(tmp = EliEval(st, args[2]))) {
                    eliEvalStk_PopN(st, EliEvalStack(st), bound);
                    return;
                }
            }
            updateVals = NULL;
            updatePtr = updateList;
            do {
                tmp = EliCons_GetCar(updatePtr);
                if (EliSexp_GetType(tmp) != e_data_none) {
                    if (!(tmp2 = EliEval(st, tmp))) {
                        eliEvalStk_PopN(st, EliEvalStack(st), bound);
                        return;
                    }
                    if (!(updateVals = EliAddToList(st, updateVals, tmp2))) {
                        eliEvalStk_PopN(st, EliEvalStack(st), bound);
                        return;
                    }
                }
                else
                    if (!(updateVals = EliAddToList(st, updateVals, tmp))) {
                        eliEvalStk_PopN(st, EliEvalStack(st), bound);
                        return;
                    }
            } while (updatePtr = EliGetNextCell(updatePtr));

            updatePtr = updateVals;
            varPtr = varList;
            do {
                tmp = EliCons_GetCar(updatePtr);
                if (EliSexp_GetType(tmp) != e_data_none) {
                    sym = EliFindSym(st, EliStr_GetString(EliSym_GetName(EliSexp_GetSym(EliCons_GetCar(varPtr)))));

                    /*
                     * I'm pretty sure this is the same sym node as is on the
                     * stack
                     */
                    EliSym_BindSexp(st, sym, tmp);
                }
                varPtr = EliGetNextCell(varPtr);
            } while (updatePtr = EliGetNextCell(updatePtr));
        }
        else
            loopy = FALSE;
    }
    eliEval(st, returnVal, resbuf);
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), bound);
        return;
    }
    eliEvalStk_PopN(st, EliEvalStack(st), bound);
}

void            Prim_SYM_TO_STR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    eliDataTypes_t  argV[1];
    EliSexp_t      *args[1], *err;
    int             processResult, evalV[1];

    EliDebug(20, "Entering primitive SYM-TO-STR", st, FALSE);
    argV[0] = e_data_symbol;
    evalV[0] = TRUE;
    processResult = EliProcessList(st, arglist, 1, 1, args, &err, argV, evalV);
    if ((processResult == -1) || (processResult == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [SYM-TO-STR (checking arglist size)]", 0);
        return;
    }
    if (processResult == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [SYM-TO-STR (arg is not a symbol)]", 0);
        return;
    }
    if (processResult < 0)
        return;
    EliSexp_SetStr(st, resbuf,
                    EliSym_GetName(EliNilP(st, args[0]) ?
                                    EliNilSym(st) :
                                    EliSexp_GetSym(args[0])));
}

void            Prim_STR_TO_INT(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    long            atol();
    eliDataTypes_t  typeV[1];
    EliSexp_t      *args[1], *err;
    int             processResult, evalV[1];

    EliDebug(20, "Entering primitive STR-TO-INT", st, FALSE);
    typeV[0] = e_data_string;
    evalV[0] = TRUE;
    processResult = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((processResult == -1) || (processResult == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [STR-TO-INT (checking arglist size)]", 0);
        return;
    }
    if (processResult == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [STR-TO-INT (arg is not a string)]", 0);
        return;
    }
    if (processResult < 0)
        return;
    EliSexp_SetInt(st, resbuf, atol(EliStr_GetString(EliSexp_GetStr(args[0]))));
}

void            Prim_INT_TO_STR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char            buf[32];           /* Should be big enough for any int
                                        * we'd want to print */
    eliDataTypes_t  typeV[1];
    EliSexp_t      *args[1], *err;
    int             processResult, evalV[1];
    EliStr_t       *strNode;

    EliDebug(20, "Entering primitive INT-TO-STR", st, FALSE);
    typeV[0] = e_data_integer;
    evalV[0] = TRUE;
    processResult = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((processResult == -1) || (processResult == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [INT-TO-STR (checking arglist size)]", 0);
        return;
    }
    if (processResult == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [INT-TO-STR (arg is not an integer)]", 0);
        return;
    }
    if (processResult < 0)
        return;
    sprintf(buf, "%ld", EliSexp_GetInt(args[0]));
    if (!(strNode = eliStringTable_FindOrMake(st, EliStringTable(st), buf)))
        return;
    EliSexp_SetStr(st, resbuf, strNode);
}

/* This one works like printf in C, but it's very primitive.
 * Output goes to stdout,
 * and the return value is T.  Format controls are:
 * %d for integers
 * %s for strings (does NOT unparse the string)
 * %S for any sexp (DOES unparse strings)
 * %% outputs a % char.
 * No modifiers or anything like that.
 */

void            Prim_PRINTF(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *thisArg, *err, *formatSexp;
    EliCons_t      *argPtr = arglist;
    char           *formatPtr, c;
    int             state = 0;         /* 0 is normal, 1 is when we've just
                                        * seen a % char */

    EliDebug(20, "Entering primitive PRINTF", st, FALSE);
    if (!(thisArg = EliEval(st, EliCons_GetCar(argPtr))))
        return;
    if (e_data_string != EliSexp_GetType(thisArg)) {
        EliError(st, ELI_ERR_BAD_ARGS, thisArg, "ELI-PRIMITIVE [PRINTF (1st arg not a string)]", 0);
        return;
    }
    formatPtr = EliStr_GetString(EliSexp_GetStr(formatSexp = thisArg));
    argPtr = EliGetNextCell(argPtr);
    while (c = *(formatPtr++)) {
        if (state) {
            state = 0;
            switch (c) {
                case 'd':
                    if (!argPtr) {
                        if (!(err = EliSexp_GetNew(st)))
                            return;
                        EliSexp_SetCons(st, err, arglist);
                        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [PRINTF (format requires more args than were supplied)]", 0);
                        return;
                    }
                    if (!(thisArg = EliEval(st, EliCons_GetCar(argPtr))))
                        return;
                    if (e_data_integer != EliSexp_GetType(thisArg)) {
                        EliError(st, ELI_ERR_BAD_ARGS, thisArg, "ELI-PRIMITIVE [PRINTF (format requires an integer)]", 0);
                        return;
                    }
                    argPtr = EliGetNextCell(argPtr);
                    printf("%ld", EliSexp_GetInt(thisArg));
                    break;
                case 's':
                    if (!argPtr) {
                        if (!(err = EliSexp_GetNew(st)))
                            return;
                        EliSexp_SetCons(st, err, arglist);
                        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [PRINTF (format requires more args than were supplied)]", 0);
                        return;
                    }
                    if (!(thisArg = EliEval(st, EliCons_GetCar(argPtr))))
                        return;
                    if (e_data_string != EliSexp_GetType(thisArg)) {
                        EliError(st, ELI_ERR_BAD_ARGS, thisArg, "ELI-PRIMITIVE [PRINTF (format requires a string)]", 0);
                        return;
                    }
                    argPtr = EliGetNextCell(argPtr);
                    printf("%s", EliStr_GetString(EliSexp_GetStr(thisArg)));
                    break;
                case 'S':
                    if (!argPtr) {
                        if (!(err = EliSexp_GetNew(st)))
                            return;
                        EliSexp_SetCons(st, err, arglist);
                        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [PRINTF (format requires more args than were supplied)]", 0);
                        return;
                    }
                    if (!(thisArg = EliEval(st, EliCons_GetCar(argPtr))))
                        return;
                    argPtr = EliGetNextCell(argPtr);
                    EliDisplaySexp(thisArg);
                    break;
                case '%':
                    putchar('%');
                    break;
                default:
                    EliError(st, ELI_ERR_BAD_ARGS, formatSexp, "ELI-PRIMITIVE [PRINTF (bad % directive in format)]", 0);
                    return;
            }
        }
        else {
            if (c == '%')
                state = 1;
            else
                putchar(c);
        }
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            Prim_PUTS(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err;
    eliDataTypes_t  typeV[1];
    int             evalV[1], paramStat;
    EliStr_t       *strTmp;

    EliDebug(20, "Entering primitive PUTS", st, FALSE);
    typeV[0] = e_data_string;
    evalV[0] = TRUE;
    paramStat = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [PUTS (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [PUTS (arg is not a string)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    printf("%s", EliStr_GetString(strTmp = EliSexp_GetStr(args[0])));
    EliSexp_SetStr(st, resbuf, strTmp);
}

void            Prim_SYSTEM(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err;
    int             paramStat, evalV[1];
    eliDataTypes_t  typeV[1];

    EliDebug(20, "Entering primitive SYSTEM", st, FALSE);
    evalV[0] = TRUE;
    typeV[0] = e_data_string;
    paramStat = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [SYSTEM (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [SYSTEM (arg is not a string)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    EliSexp_SetInt(st, resbuf, (long) system(EliStr_GetString(EliSexp_GetStr(args[0]))));
}

void            Prim_GETENV(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err;
    int             paramStat, evalV[1];
    eliDataTypes_t  typeV[1];
    char           *val;
    EliStr_t       *strTmp;

    EliDebug(20, "Entering primitive GETENV", st, FALSE);
    evalV[0] = TRUE;
    typeV[0] = e_data_string;
    paramStat = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [GETENV (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [GETENV (arg is not a string)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    if (val = getenv(EliStr_GetString(EliSexp_GetStr(args[0])))) {
        if (!(strTmp = eliStringTable_FindOrMake(st, EliStringTable(st), val)))
            return;
        EliSexp_SetStr(st, resbuf, strTmp);
    }
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            Prim_DEBUG(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *err;
    eliDataTypes_t  typeV[2];
    int             evalV[2], paramStat, newlevel;
    static long     histNum = 0;
    long            tmp;
    char           *msg, *str;

    EliDebug(20, "Entering primitive DEBUG", st, FALSE);
    typeV[0] = e_data_integer;
    evalV[0] = TRUE;
    typeV[1] = e_data_string;
    evalV[1] = TRUE;
    paramStat = EliProcessList(st, arglist, 0, 2, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [DEBUG (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [DEBUG (1st arg is not an int)]", 0);
        return;
    }
    if (paramStat == -1001) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [DEBUG (2nd arg is not a string)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    if (paramStat == 1) {
        EliProcessInfo.debugStuff.curDebugLevel = (newlevel = (int) EliSexp_GetInt(args[0]));
        EliSexp_SetInt(st, resbuf, (long) newlevel);
    }
    else {
        if (paramStat == 2) {
            if ((tmp = EliSexp_GetInt(args[0])) < 1L) {
                EliError(st, ELI_ERR_BAD_ARGS, args[0], "ELI-PRIMITIVE [DEBUG (debug level must be 1 or greater)]", 0);
                return;
            }
            if (!(msg = malloc(1 + strlen(str = EliStr_GetString(EliSexp_GetStr(args[1])))))) {
                EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [DEBUG (allocating debugging message string)]", 0);
                return;
            }
            strcpy(msg, str);
            EliDebug(tmp, msg, st, TRUE);
            EliSexp_SetSym(st, resbuf, EliTSym(st));
        }
        else {
            histNum = ((tmp = EliDebugFPrintSince(stdout, histNum)) ? tmp : histNum);
            EliSexp_SetInt(st, resbuf, tmp);
        }
    }
}

void            Prim_EQUAL(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *err;
    int             evalV[2], paramStat;

    EliDebug(20, "Entering primitive EQUAL", st, FALSE);
    evalV[0] = evalV[1] = TRUE;
    paramStat = EliProcessList(st, arglist, 2, 2, args, &err, NULL, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI_PRIMITIVE [EQUAL (checking arglist size)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    EliSexp_SetSym(st, resbuf, EliSexpEqual(st, args[0], args[1]) ? EliTSym(st) : EliNilSym(st));
}

void            Prim_UCSTRING(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err;
    eliDataTypes_t  typeV[1];
    int             evalV[1], paramStat;
    char           *newstr, *oldstr, *p;
    EliStr_t       *strTmp;

    EliDebug(20, "Entering primitive UCSTRING", st, FALSE);
    typeV[0] = e_data_string;
    evalV[0] = TRUE;
    paramStat = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [UCSTRING (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [UCSTRING (arg is not a string)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    if (!(newstr = EliStringOpBuf(strlen(oldstr = EliStr_GetString(EliSexp_GetStr(args[0]))) + 1))) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "ELI-PRIMITIVE [UCSTRING (allocating result string)]", 0);
        return;
    }
    strcpy(newstr, oldstr);
    for (p = newstr; *p; ++p)
        if (isascii(*p) && islower(*p))
            *p = toupper(*p);
    if (!(strTmp = eliStringTable_FindOrMake(st, EliStringTable(st), newstr)))
        return;
    EliSexp_SetStr(st, resbuf, strTmp);
}

/* Evaluates a single argument to a symbol name; unbinds that
 * symbol's function value.  Does this by binding a new,
 * empty fn node to the symbol.
 */
void            Prim_UNBINDFN(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err;
    eliDataTypes_t  typeV[1];
    int             evalV[1], paramStat;
    EliFn_t        *fnTmp;
    EliSym_t       *symTmp;

    EliDebug(20, "Entering primitive UNBINDFN", st, FALSE);
    typeV[0] = e_data_symbol;
    evalV[0] = TRUE;
    paramStat = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [UNBINDFN (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [UNBINDFN (arg is not a symbol)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    if (!(fnTmp = eliFn_GetNew_trace(st, EliTraceStk(st))))
        return;
    EliSym_BindFn(st, symTmp = EliSexp_GetSym(args[0]), fnTmp);
    EliSexp_SetSym(st, resbuf, symTmp);
}

void            Prim_UNBIND(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err, *sexpTmp;
    eliDataTypes_t  typeV[1];
    int             evalV[1], paramStat;
    EliSym_t       *symTmp;

    EliDebug(20, "Entering primitive UNBIND", st, FALSE);
    typeV[0] = e_data_symbol;
    evalV[0] = TRUE;
    paramStat = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [UNBIND (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [UNBIND (arg is not a symbol)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    if (!(sexpTmp = EliSexp_GetNew(st)))
        return;
    EliSym_BindSexp(st, symTmp = EliSexp_GetSym(args[0]), sexpTmp);
    EliSexp_SetSym(st, resbuf, symTmp);
}

void            Prim_DISCARD(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[1], *err;
    eliDataTypes_t  typeV[1];
    int             evalV[1], paramStat;
    EliSym_t       *symTmp;

    EliDebug(20, "Entering primitive DISCARD", st, FALSE);
    typeV[0] = e_data_symbol;
    evalV[0] = TRUE;
    paramStat = EliProcessList(st, arglist, 1, 1, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [DISCARD (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [DISCARD (arg is not a symbol)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    EliSexp_SetSym(st, resbuf, symTmp = EliSexp_GetSym(args[0]));
    eliHT_Delete(st, EliSymbolTable(st), EliStr_GetString(EliSym_GetName(symTmp)));
}

void            Prim_ERROR(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *args[2], *err;
    eliDataTypes_t  typeV[2];
    int             evalV[2], paramStat;

    EliDebug(20, "Entering primitive ERROR", st, FALSE);
    typeV[0] = e_data_string;
    typeV[1] = e_data_none;
    evalV[0] = evalV[1] = TRUE;
    paramStat = EliProcessList(st, arglist, 1, 2, args, &err, typeV, evalV);
    if ((paramStat == -1) || (paramStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [ERROR (checking arglist size)]", 0);
        return;
    }
    if (paramStat == -1000) {
        EliError(st, ELI_ERR_BAD_ARGS, err, "ELI-PRIMITIVE [ERROR (1st arg not a string)]", 0);
        return;
    }
    if (paramStat < 0)
        return;
    EliError(st, ELI_ERR_USERERROR, (paramStat == 2) ? args[1] : NULL, EliStr_GetString(EliSexp_GetStr(args[0])), 0);
}

void            Prim_DEFUNV(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *listElts[3], *err, *lambdavSexp, *symSexp;
    EliCons_t      *resultList = NULL, *argArg;
    EliSym_t       *theSym;
    EliFn_t        *fnNode;
    char           *name;

    EliDebug(20, "Entering primitive DEFUNV", st, FALSE);
    if (3 != EliGetListCars(arglist, listElts, 3)) {
        if (!(err = EliSexp_GetNew(st)))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [DEFUNV (checking arglist size)]", 0);
        return;
    }
    if (e_data_symbol != EliSexp_GetType(listElts[0])) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[0], "ELI-PRIMITIVE [DEFUNV (1st arg not a symbol)]", 0);
        return;
    }
    if (e_data_list != EliSexp_GetType(listElts[1])) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[1], "ELI-PRIMITIVE [DEFUNV (2nd arg not a list)]", 0);
        return;
    }
    if (1 != EliListLen(argArg = EliSexp_GetCons(listElts[1]))) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[1], "ELI-PRIMITIVE [DEFUNV (2nd arg must have exactly one element)]", 0);
        return;
    }
    if (EliSexp_GetType(EliCons_GetCar(argArg)) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[1], "ELI-PRIMITIVE [DEFUNV (non-symbol in parameter list)]", 0);
        return;
    }
    if (!(lambdavSexp = EliSexp_GetNew(st)))
        return;
    EliSexp_SetSym(st, lambdavSexp, EliLambdavSym(st));
    if (!(resultList = EliAddToList(st, resultList, lambdavSexp)))
        return;
    if (!(resultList = EliAddToList(st, resultList, listElts[1])))
        return;
    if (!(resultList = EliAddToList(st, resultList, listElts[2])))
        return;
    if (!(fnNode = eliFn_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliFn_SetCons(st, fnNode, resultList);
    EliSym_BindFn(st, theSym = EliSexp_GetSym(listElts[0]), fnNode);
    if (eliSym_GetScope(theSym) == e_sym_known) {
        if (!(symSexp = EliSexp_GetNew(st)))
            return;
        eliSym_SetScope(theSym, e_sym_global);
        EliSexp_SetSym(st, symSexp, theSym);
        eliHT_Insert(st, EliSymbolTable(st), symSexp, name = EliStr_GetString(EliSym_GetName(theSym)));
        eliHT_Delete(st, EliTempSymTable(st), name);
    }
    EliSexp_SetSym(st, resbuf, theSym);
}

void            Prim_DEFUNVQ(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *listElts[3], *err, *lambdavqSexp, *symSexp;
    EliCons_t      *resultList = NULL, *argArg;
    EliSym_t       *theSym;
    EliFn_t        *fnNode;
    char           *name;

    EliDebug(20, "Entering primitive DEFUNVQ", st, FALSE);
    if (3 != EliGetListCars(arglist, listElts, 3)) {
        if (!(err = EliSexp_GetNew(st)))
            return;
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "ELI-PRIMITIVE [DEFUNVQ (checking arglist size)]", 0);
        return;
    }
    if (e_data_symbol != EliSexp_GetType(listElts[0])) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[0], "ELI-PRIMITIVE [DEFUNVQ (1st arg not a symbol)]", 0);
        return;
    }
    if (e_data_list != EliSexp_GetType(listElts[1])) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[1], "ELI-PRIMITIVE [DEFUNVQ (2nd arg not a list)]", 0);
        return;
    }
    if (1 != EliListLen(argArg = EliSexp_GetCons(listElts[1]))) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[1], "ELI-PRIMITIVE [DEFUNVQ (2nd arg must have exactly one element)]", 0);
        return;
    }
    if (EliSexp_GetType(EliCons_GetCar(argArg)) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, listElts[1], "ELI-PRIMITIVE [DEFUNVQ (non-symbol in parameter list)]", 0);
        return;
    }
    if (!(lambdavqSexp = EliSexp_GetNew(st)))
        return;
    EliSexp_SetSym(st, lambdavqSexp, EliLambdavqSym(st));
    if (!(resultList = EliAddToList(st, resultList, lambdavqSexp)))
        return;
    if (!(resultList = EliAddToList(st, resultList, listElts[1])))
        return;
    if (!(resultList = EliAddToList(st, resultList, listElts[2])))
        return;
    if (!(fnNode = eliFn_GetNew_trace(st, EliTraceStk(st))))
        return;
    eliFn_SetCons(st, fnNode, resultList);
    EliSym_BindFn(st, theSym = EliSexp_GetSym(listElts[0]), fnNode);
    if (eliSym_GetScope(theSym) == e_sym_known) {
        if (!(symSexp = EliSexp_GetNew(st)))
            return;
        eliSym_SetScope(theSym, e_sym_global);
        EliSexp_SetSym(st, symSexp, theSym);
        eliHT_Insert(st, EliSymbolTable(st), symSexp, name = EliStr_GetString(EliSym_GetName(theSym)));
        eliHT_Delete(st, EliTempSymTable(st), name);
    }
    EliSexp_SetSym(st, resbuf, theSym);
}

void            Prim_VERSION(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    int             majr, minr;
    EliSexp_t      *vv[2];
    EliCons_t      *result;

    EliDebug(20, "Entering primitive VERSION", st, FALSE);
    if (!(vv[0] = EliSexp_GetNew(st)))
        return;
    if (!(vv[1] = EliSexp_GetNew(st)))
        return;
    EliVersion(&majr, &minr);
    EliSexp_SetInt(st, vv[0], (long) majr);
    EliSexp_SetInt(st, vv[1], (long) minr);
    if (!(result = EliListFromCars(st, vv, 2)))
        return;
    EliSexp_SetCons(st, resbuf, result);
}

void            Prim_TRACE(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *arg, *evalArg;

    EliDebug(20, "Entering primitive TRACE", st, FALSE);

    if (EliListLen(arglist) > 0) {
        arg = EliCons_GetCar(arglist);
        if (!(evalArg = EliEval(st, arg)))
            return;
        st->tracep = EliNilP(st, evalArg) ? FALSE : TRUE;
    }
    else
        st->tracep = !(st->tracep);
    if (!(st->tracep))
        st->indentTrace = 0;
    EliSexp_SetSym(st, resbuf, st->tracep ? EliTSym(st) : EliNilSym(st));
}

void Prim_CATCHERR(st, arglist, resbuf)
EliState_t *st;
EliCons_t *arglist;
EliSexp_t *resbuf;
{
    EliSexp_t *val[1], *err, *badsexp;
    int evalv[1], paramstat, unixerr;
    EliCons_t *resultlist = NULL;
    EliSexp_t *tmp;
    EliStr_t *strtmp;
    char *errstr, *errdesc;

    evalv[0] = 1;
    paramstat = EliProcessList(st, arglist, 1, 1,
                                val, &err, NULL, evalv);
    if ((paramstat == -1) || (paramstat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, err,
                  "ELI-PRIMITIVE [CATCHERR (checking arglist size)]", 0);
        return;
    }
    if (paramstat == -2000) {   /* Error! */
        errstr = EliErrStr(EliErr_ErrCode(st));
        errdesc = EliErr_ErrLoc(st);
        unixerr = EliErr_UnixErr(st);
        badsexp = EliErr_BadSexp(st);
        EliClearErr(st);
        if (!(tmp = EliSexp_GetNew(st)))
            return;
        EliSexp_SetSym(st, tmp, EliNilSym(st));
        if (!(resultlist = EliAddToList(st, NULL, tmp)))
            return;
        if (!(strtmp = EliStringTable_FindOrMake(st, errstr)))
            return;
        if (!(tmp = EliSexp_GetNew(st)))
            return;
        EliSexp_SetStr(st, tmp, strtmp);
        if (!(resultlist = EliAddToList(st, resultlist, tmp)))
            return;
        if (!(strtmp = EliStringTable_FindOrMake(st, errdesc)))
            return;
        if (!(tmp = EliSexp_GetNew(st)))
            return;
        EliSexp_SetStr(st, tmp, strtmp);
        if (!(resultlist = EliAddToList(st, resultlist, tmp)))
            return;
        if (unixerr) {
            if (!(strtmp =
                   EliStringTable_FindOrMake(st, sys_errlist[unixerr])))
                return;
            if (!(tmp = EliSexp_GetNew(st)))
                return;
            EliSexp_SetStr(st, tmp, strtmp);
            if (!(resultlist = EliAddToList(st, resultlist, tmp)))
                return;
        }
        if (badsexp) {
            if (!(resultlist = EliAddToList(st, resultlist,
                                             badsexp)))
                return;
        }
        EliSexp_SetCons(st, resbuf, resultlist);
        return;
    }
    if (!(tmp = EliSexp_GetNew(st)))
        return;
    EliSexp_SetSym(st, tmp, EliTSym(st));
    if (!(resultlist = EliAddToList(st, NULL, tmp)))
        return;
    if (!(resultlist = EliAddToList(st, resultlist, val[0])))
        return;
    EliSexp_SetCons(st, resbuf, resultlist);
}
