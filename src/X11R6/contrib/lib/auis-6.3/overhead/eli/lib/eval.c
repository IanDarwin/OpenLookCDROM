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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/eval.c,v 2.13 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include  <eval.h>

/* evaluation routines */

void            eliEval(st, expr, resbuf)
EliState_t     *st;
EliSexp_t      *expr, *resbuf;
{
    EliSym_t       *tmp;
    EliSexp_t      *nodetmp;
    int             depth;

    switch (EliSexp_GetType(expr)) {
        case e_data_none:
            EliSexp_SetSym(st, resbuf, EliNilSym(st));
            break;
        case e_data_integer:
            EliSexp_SetInt(st, resbuf, EliSexp_GetInt(expr));
            break;
        case e_data_string:
            EliSexp_SetStr(st, resbuf, EliSexp_GetStr(expr));
            break;
        case e_data_symbol:
            if (!(tmp = EliFindSym(st, EliStr_GetString(EliSym_GetName(EliSexp_GetSym(expr)))))) {
                EliError(st, ELI_ERR_NOSYM, expr, "INTERNAL [eliEval (looking for symbol)]", 0);
                return;
            }
            nodetmp = EliSym_GetSexp(tmp);
            if (EliSexp_GetType(nodetmp) == e_data_none) {
                EliError(st, ELI_ERR_UNBOUND, expr, "INTERNAL [eliEval (returning symbol value)]", 0);
                return;
            }
            switch (EliSexp_GetType(nodetmp)) {
                case e_data_integer:
                    EliSexp_SetInt(st, resbuf, EliSexp_GetInt(nodetmp));
                    break;
                case e_data_symbol:
                    EliSexp_SetSym(st, resbuf, EliSexp_GetSym(nodetmp));
                    break;
                case e_data_string:
                    EliSexp_SetStr(st, resbuf, EliSexp_GetStr(nodetmp));
                    break;
                case e_data_list:
                    EliSexp_SetCons(st, resbuf, EliSexp_GetCons(nodetmp));
                    break;
                case e_data_fn:
                    EliSexp_SetFn(st, resbuf, EliSexp_GetFn(nodetmp));
                    break;
            }
            break;
        case e_data_list:
            depth = st->numErrStkNodes;
            eliEvalList(st, EliCons_GetCar(EliSexp_GetCons(expr)), EliCons_GetCdr(EliSexp_GetCons(expr)), resbuf);
            if (EliErr_ErrP(st)) {
                st->g_errflag = FALSE;
                st->g_err->backtrace = EliAddToList(st, st->g_err->backtrace, expr);
                st->g_errflag = TRUE;
                return;
            }
            eliTraceStk_PurgeN(st, EliTraceStk(st),
                               st->numErrStkNodes - depth);     /* Brand new attempt to
                                                                 * actually do something
                                                                 * resembling garbage
                                                                 * collection */
            break;
        case e_data_fn:
            EliSexp_SetFn(st, resbuf, EliSexp_GetFn(expr));     /* Is this the Right
                                                                 * Thing? */
            break;
    }
}


void            eliEvalList(st, lcar, lcdr, resbuf)
EliState_t     *st;
EliSexp_t      *lcar, *lcdr, *resbuf;
{
    EliFn_t        *fnval;
    EliSexp_t      *tmpnode, *tmpnode2;
    EliCons_t      *tmpcons, *anotherCons;
    char           *fnname;
    int             i, wasTrace;
    void (*fn)();

    switch (EliSexp_GetType(lcar)) {
        case e_data_integer:
        case e_data_string:
            EliError(st, ELI_ERR_FN_UNDEF, lcar, "INTERNAL [eliEvalList (bad atom type at list car)]", 0);
            return;
        case e_data_symbol:
            fnval = EliSym_GetFn(EliSexp_GetSym(lcar));
            switch (eliFn_GetType(fnval)) {
                case e_fn_list:
                    EliDebug(40, fnname = EliStr_GetString(EliSym_GetName(EliSexp_GetSym(lcar))), st, FALSE);
                    if (!(tmpnode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                        return;
                    EliSexp_SetCons(st, tmpnode, eliFn_GetCons(fnval));
                    if (wasTrace = st->tracep) {
                        for (i = 0; i < st->indentTrace; ++i)
                            putchar(' ');
                        puts(fnname);
                        ++st->indentTrace;
                    }
                    eliEvalList(st, tmpnode, lcdr, resbuf);
                    if (wasTrace && st->tracep)
                        --st->indentTrace;
                    if (EliErr_ErrP(st))
                        return;
                    break;
                case e_fn_compiled:
                    if (EliSexp_GetType(lcdr) != e_data_list) { /* Then it better be
                                                                 * e_data_none */
                        if (!(tmpcons = eliCons_GetNew_trace(st, EliTraceStk(st))))
                            return;
                    }
                    else
                        tmpcons = EliSexp_GetCons(lcdr);
                    fnname = EliStr_GetString(EliSym_GetName(EliSexp_GetSym(lcar)));
                    if (wasTrace = st->tracep) {
                        for (i = 0; i < st->indentTrace; ++i)
                            putchar(' ');
                        puts(fnname);
                        ++st->indentTrace;
                    }
                    fn = eliFn_GetCompiled(fnval);
                    (*fn)(st, tmpcons, resbuf);
                    if (wasTrace && st->tracep)
                        --st->indentTrace;
                    if (EliErr_ErrP(st))
                        return;        /* Dumb.  What *else* is it going to
                                        * do? */
                    break;
                case e_fn_none:
                    EliError(st, ELI_ERR_FN_UNDEF, lcar,
                             "INTERNAL [eliEvalList (functionless symbol at list car)]",
                             0);
                    return;
            }
            break;
        case e_data_list:
            tmpnode = EliCons_GetCar(EliSexp_GetCons(lcar));
            if (EliSexp_GetType(tmpnode) == e_data_symbol) {
                if (EliSexp_GetSym(tmpnode) == EliLambdaSym(st)) {
                    if (EliSexp_GetType(lcdr) != e_data_list) {
                        if (!(tmpcons = eliCons_GetNew_trace(st, EliTraceStk(st))))
                            return;
                    }
                    else
                        tmpcons = EliSexp_GetCons(lcdr);
                    eliEvalLambda(st, EliSexp_GetCons(lcar), tmpcons, resbuf);
                    if (EliErr_ErrP(st))
                        return;
                }
                else {
                    if (EliSexp_GetSym(tmpnode) == EliLambdaqSym(st)) {
                        if (EliSexp_GetType(lcdr) != e_data_list) {
                            if (!(tmpcons = eliCons_GetNew_trace(st, EliTraceStk(st))))
                                return;
                        }
                        else
                            tmpcons = EliSexp_GetCons(lcdr);
                        eliEvalLambdaq(st, EliSexp_GetCons(lcar), tmpcons, resbuf);
                        if (EliErr_ErrP(st))
                            return;
                    }
                    else {
                        if (EliSexp_GetSym(tmpnode) == EliLambdavSym(st)) {
                            if (EliSexp_GetType(lcdr) != e_data_list) {
                                if (!(tmpcons = eliCons_GetNew_trace(st, EliTraceStk(st))))
                                    return;
                            }
                            else
                                tmpcons = EliSexp_GetCons(lcdr);
                            eliEvalLambdav(st, EliSexp_GetCons(lcar), tmpcons, resbuf);
                            if (EliErr_ErrP(st))
                                return;
                        }
                        else {
                            if (EliSexp_GetSym(tmpnode) == EliLambdavqSym(st)) {
                                if (EliSexp_GetType(lcdr) != e_data_list) {
                                    if (!(tmpcons = eliCons_GetNew_trace(st, EliTraceStk(st))))
                                        return;
                                }
                                else
                                    tmpcons = EliSexp_GetCons(lcdr);
                                eliEvalLambdavq(st, EliSexp_GetCons(lcar), tmpcons, resbuf);
                                if (EliErr_ErrP(st))
                                    return;
                            }
                            else {
                                if (!(tmpnode2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                                    return;
                                eliEvalList(st, tmpnode, EliCons_GetCdr(EliSexp_GetCons(lcar)),
                                            tmpnode2);
                                if (EliErr_ErrP(st))
                                    return;
                                eliEvalList(st, tmpnode2, lcdr, resbuf);
                                if (EliErr_ErrP(st))
                                    return;
                            }
                        }
                    }
                }
            }
            else {
                if (!(tmpnode2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                    return;
                eliEvalList(st, tmpnode, EliCons_GetCdr(EliSexp_GetCons(lcar)), tmpnode2);
                if (EliErr_ErrP(st))
                    return;
                eliEvalList(st, tmpnode2, lcdr, resbuf);
                if (EliErr_ErrP(st))
                    return;
            }
            break;
        case e_data_fn:        /* This whole section needs to be
                                        * double-checked */
            fnval = EliSexp_GetFn(lcar);
            switch (eliFn_GetType(fnval)) {
                case e_fn_list:
                    if (!(tmpnode = eliSexp_GetNew_trace(st, EliTraceStk(st))))
                        return;
                    EliSexp_SetCons(st, tmpnode, eliFn_GetCons(fnval));
                    eliEvalList(st, tmpnode, lcdr, resbuf);
                    if (EliErr_ErrP(st))
                        return;
                    break;
                case e_fn_compiled:
                    if (EliSexp_GetType(lcdr) != e_data_list) {
                        /* Then it better be e_data_none */
                        if (!(anotherCons = eliCons_GetNew_trace(st, EliTraceStk(st))))
                            return;
                    }
                    else
                        anotherCons = EliSexp_GetCons(lcdr);
                    fn = eliFn_GetCompiled(fnval);
                    (*fn)(st, anotherCons, resbuf);
                    if (EliErr_ErrP(st))
                        return;        /* Dumb.  What *else* is it going to
                                        * do? */
                    break;
                case e_fn_none:
                    EliError(st, ELI_ERR_FN_UNDEF, lcar,
                    "INTERNAL [eliEvalList (empty functionnode at list car)]",
                             0);
                    return;
            }
            break;
        case e_data_none:
            EliSexp_SetSym(st, resbuf, EliNilSym(st));
            break;
    }
}



/* This function ASSUMES that the car of the lambda list is indeed lambda */

void            eliEvalLambda(st, lambdalist, arglist, resbuf)
EliState_t     *st;
EliCons_t      *lambdalist, *arglist;
EliSexp_t      *resbuf;
{
    int             numvars;           /* Holds number of items pushed onto
                                        * the stack */
    EliCons_t      *varlist,           /* The varlist is the 2nd element in
                                        * the lambda list, to wit: (lambda (a
                                        * b c) (foo you)) (car (cdr '(lambda
                                        * (a b c) (foo you)))) returns this
                                        * list [(a b c)]. */
                   *llistcdr,          /* Will contain the cdr of the lambda
                                        * list */
                   *llistcdrcdr,       /* Contains the cdr of llistcdr */
                   *evaledArgs;
    EliSexp_t      *tmp;

    tmp = EliCons_GetCdr(lambdalist);
    llistcdr = EliSexp_GetCons(tmp);
    tmp = EliCons_GetCar(llistcdr);
    varlist = EliNilP(st, tmp) ? (EliCons_t *) 0 : EliSexp_GetCons(tmp);
    if (!(evaledArgs = EliEvalListToList(st, arglist)))
        return;
    numvars = varlist ? eliBind(st, EliEvalStack(st), varlist, evaledArgs) : 0;
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), numvars);
        return;
    }
    tmp = EliCons_GetCdr(llistcdr);
    llistcdrcdr = EliSexp_GetCons(tmp);
    eliEval(st, EliCons_GetCar(llistcdrcdr), resbuf);
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), numvars);
        return;
    }
    eliEvalStk_PopN(st, EliEvalStack(st), numvars);
}


/* This function ASSUMES that the car of the lambdaq list is indeed lambdaq */

void            eliEvalLambdaq(st, lambdaqlist, arglist, resbuf)
EliState_t     *st;
EliCons_t      *lambdaqlist, *arglist;
EliSexp_t      *resbuf;
{
    int             numvars;           /* Holds number of items pushed onto
                                        * the stack */
    EliCons_t      *varlist,           /* The varlist is the 2nd element in
                                        * the lambdaq list, to wit: (lambdaq
                                        * (a b c) (foo you)) (car (cdr
                                        * '(lambdaq (a b c) (foo you))))
                                        * returns this list [(a b c)]. */
                   *llistcdr,          /* Will contain the cdr of the lambdaq
                                        * list */
                   *llistcdrcdr;       /* Contains the cdr of llistcdr */
    EliSexp_t      *tmp;

    tmp = EliCons_GetCdr(lambdaqlist);
    llistcdr = EliSexp_GetCons(tmp);
    tmp = EliCons_GetCar(llistcdr);
    varlist = EliNilP(st, tmp) ? (EliCons_t *) 0 : EliSexp_GetCons(tmp);
    numvars = varlist ? eliBind(st, EliEvalStack(st), varlist, arglist) : 0;
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), numvars);
        return;
    }
    tmp = EliCons_GetCdr(llistcdr);
    llistcdrcdr = EliSexp_GetCons(tmp);
    eliEval(st, EliCons_GetCar(llistcdrcdr), resbuf);
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), numvars);
        return;
    }
    eliEvalStk_PopN(st, EliEvalStack(st), numvars);
}

void            eliEvalLambdav(st, lambdavlist, arglist, resbuf)
EliState_t     *st;
EliCons_t      *lambdavlist, *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *listElts[3], *err, *paramSexp, *evaledArgsSexp;
    EliCons_t      *paramList, *evaledArgs;
    EliSym_t       *pushSym;

    if (3 != EliGetListCars(lambdavlist, listElts, 3)) {
        if (!(err = EliSexp_GetNew(st)))
            return;
        EliSexp_SetCons(st, err, lambdavlist);
        EliError(st, ELI_ERR_BAD_PARAM, err, "INTERNAL [eliEvalLambdav (lambdav list has bad format)]", 0);
        return;
    }
    if (e_data_list != EliSexp_GetType(listElts[1])) {
        EliError(st, ELI_ERR_BAD_PARAM, listElts[1], "INTERNAL [eliEvalLambdav (lambdav requires parameter list)]", 0);
        return;
    }
    if (EliListLen(paramList = EliSexp_GetCons(listElts[1])) != 1) {
        EliError(st, ELI_ERR_BAD_PARAM, listElts[1], "INTERNAL [eliEvalLambdav (parameter list requires exactly 1 element)]", 0);
        return;
    }
    if (EliSexp_GetType(paramSexp = EliCons_GetCar(paramList)) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_PARAM, listElts[1], "INTERNAL [eliEvalLambdav (non-symbol in parameter list)]", 0);
        return;
    }
    if (!(evaledArgs = EliEvalListToList(st, arglist)))
        return;
    if (!(pushSym = EliSym_GetNew_StrNode(st, EliSym_GetName(EliSexp_GetSym(paramSexp)))))
        return;
    if (!(evaledArgsSexp = EliSexp_GetNew(st)))
        return;
    EliSexp_SetCons(st, evaledArgsSexp, evaledArgs);
    EliSym_BindSexp(st, pushSym, evaledArgsSexp);
    if (!eliEvalStk_Push(st, EliEvalStack(st), pushSym)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliEvalLambdav (pushing symbol)]", 0);
        return;
    }
    eliEval(st, listElts[2], resbuf);
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), 1);
        return;
    }
    eliEvalStk_PopN(st, EliEvalStack(st), 1);
}

void            eliEvalLambdavq(st, lambdavqlist, arglist, resbuf)
EliState_t     *st;
EliCons_t      *lambdavqlist, *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *listElts[3], *err, *paramSexp, *argsSexp;
    EliCons_t      *paramList;
    EliSym_t       *pushSym;

    if (3 != EliGetListCars(lambdavqlist, listElts, 3)) {
        if (!(err = EliSexp_GetNew(st)))
            return;
        EliSexp_SetCons(st, err, lambdavqlist);
        EliError(st, ELI_ERR_BAD_PARAM, err, "INTERNAL [eliEvalLambdavq (lambdavq list has bad format)]", 0);
        return;
    }
    if (e_data_list != EliSexp_GetType(listElts[1])) {
        EliError(st, ELI_ERR_BAD_PARAM, listElts[1], "INTERNAL [eliEvalLambdavq (lambdavq requires parameter list)]", 0);
        return;
    }
    if (EliListLen(paramList = EliSexp_GetCons(listElts[1])) != 1) {
        EliError(st, ELI_ERR_BAD_PARAM, listElts[1], "INTERNAL [eliEvalLambdavq (parameter list requires exactly 1 element)]", 0);
        return;
    }
    if (EliSexp_GetType(paramSexp = EliCons_GetCar(paramList)) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_PARAM, listElts[1], "INTERNAL [eliEvalLambdavq (non-symbol in parameter list)]", 0);
        return;
    }
    if (!(pushSym = EliSym_GetNew_StrNode(st, EliSym_GetName(EliSexp_GetSym(paramSexp)))))
        return;
    if (!(argsSexp = EliSexp_GetNew(st)))
        return;
    EliSexp_SetCons(st, argsSexp, arglist);
    EliSym_BindSexp(st, pushSym, argsSexp);
    if (!eliEvalStk_Push(st, EliEvalStack(st), pushSym)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliEvalLambdavq (pushing symbol)]", 0);
        return;
    }
    eliEval(st, listElts[2], resbuf);
    if (EliErr_ErrP(st)) {
        eliEvalStk_PopN(st, EliEvalStack(st), 1);
        return;
    }
    eliEvalStk_PopN(st, EliEvalStack(st), 1);
}
