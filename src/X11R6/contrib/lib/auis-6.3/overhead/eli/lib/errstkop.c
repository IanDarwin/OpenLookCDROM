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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/errstkop.c,v 2.8 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include <errstkop.h>

void            eliTraceStk_Purge(st, s)	/* Pop stuff off until empty */
EliState_t     *st;
eliTraceStack_t *s;
{
    while (eliTraceStk_Top(s))
	eliTraceStk_Pop(st, s);
}

int             eliTraceStk_PurgeN(st, s, n)	/* Pop n items off stack,
						 * returns # popped */
EliState_t     *st;
eliTraceStack_t *s;
int n;
{
    int             popped = 0, i = n;

    while (eliTraceStk_Top(s) && i--) {
	eliTraceStk_Pop(st, s);
	++popped;
    }
    return (popped);
}

EliStr_t       *eliStr_GetNew_trace(st, s, string)
EliState_t     *st;
eliTraceStack_t *s;
char           *string;
{
    EliStr_t       *tmp;

    tmp = eliStr_GetNew(st, string);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushStr(st, s, tmp)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliStr_GetNew_trace (tracing allocation)]", 0);
        eliStr_DecrRefcount(st, tmp);
        return (NULL);
    }
    return (tmp);
}

EliSym_t       *eliSym_GetNew_trace(st, s, strnode)
EliState_t     *st;
eliTraceStack_t *s;
EliStr_t       *strnode;
{
    EliSym_t       *tmp;

    tmp = eliSym_GetNew(st, strnode);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushSym(st, s, tmp)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliSym_GetNew_trace (tracing allocation)]", 0);
        eliSym_DecrRefcount(st, tmp);
        return (NULL);
    }
    return (tmp);
}

EliCons_t      *eliCons_GetNew_trace(st, s)
EliState_t     *st;
eliTraceStack_t *s;
{
    EliCons_t      *tmp;

    tmp = eliCons_GetNew(st);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushCons(st, s, tmp)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliCons_GetNew_trace (tracing allocation)]", 0);
        eliCons_DecrRefcount(st, tmp);
        return (NULL);
    }
    return (tmp);
}

EliSexp_t      *eliSexp_GetNew_trace(st, s)
EliState_t     *st;
eliTraceStack_t *s;
{
    EliSexp_t      *tmp;

    tmp = eliSexp_GetNew(st);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushSexp(st, s, tmp)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliSexp_GetNew_trace (tracing allocation)]", 0);
        eliSexp_DecrRefcount(st, tmp);
        return (NULL);
    }
    return (tmp);
}

EliFn_t        *eliFn_GetNew_trace(st, s)
EliState_t     *st;
eliTraceStack_t *s;
{
    EliFn_t        *tmp;

    tmp = eliFn_GetNew(st);
    if (EliErr_ErrP(st))
        return (NULL);
    if (!eliTraceStk_PushFn(st, s, tmp)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliFn_GetNew_trace (tracing allocation)]", 0);
        eliFn_DecrRefcount(st, tmp);
        return (NULL);
    }
    return (tmp);
}

eliBucketNode_t *eliBucketNode_GetNew_trace(st, s, datum, key)
EliState_t     *st;
eliTraceStack_t *s;
EliSexp_t      *datum;
char           *key;
{
    eliBucketNode_t *tmp;

    tmp = eliBucketNode_GetNew(st, datum, key);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushBucketNode(st, s, tmp)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliBucketNode_GetNew_trace (tracing allocation)]", 0);
        eliBucketNode_DecrRefcount(st, tmp);
        return (NULL);
    }
    return (tmp);
}

EliSexp_t      *eliGetSexp_trace(st, s)
EliState_t     *st;
eliTraceStack_t *s;
{
    EliSexp_t      *result;

    result = eliGetSexp(st);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushSexp(st, s, result)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliGetSexp_trace (tracing allocation)]", 0);
        eliSexp_DecrRefcount(st, result);
        return (NULL);
    }
    return (result);
}

EliSexp_t      *eliFGetSexp_trace(st, s, fp)
EliState_t     *st;
eliTraceStack_t *s;
FILE           *fp;
{
    EliSexp_t      *result;

    result = eliFGetSexp(st, fp);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushSexp(st, s, result)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliFGetSexp_trace (tracing allocation)]", 0);
        eliSexp_DecrRefcount(st, result);
        return (NULL);
    }
    return (result);
}

EliSexp_t      *eliSGetSexp_trace(st, s, string)
EliState_t     *st;
eliTraceStack_t *s;
char           *string;
{
    EliSexp_t      *result;

    result = eliSGetSexp(st, string);
    if (EliErr_ErrP(st))
	return (NULL);
    if (!eliTraceStk_PushSexp(st, s, result)) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliSGetSexp_trace (tracing allocation)]", 0);
        eliSexp_DecrRefcount(st, result);
        return (NULL);
    }
    return (result);
}
