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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/errstk.c,v 2.7 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include <errstk.h>

static int eliTraceStk_Grow(s)
eliTraceStack_t *s;
{
    eliTraceStackNode_t *tmp = (s->stack) ? (eliTraceStackNode_t *) realloc(s->stack, (s->size + GROW_STACK_SIZE) * sizeof (eliTraceStackNode_t)) : (eliTraceStackNode_t *) malloc(GROW_STACK_SIZE * sizeof (eliTraceStackNode_t));

    if (tmp) {
        s->stack = tmp;
        s->size += GROW_STACK_SIZE;
        return (TRUE);
    }
    return (FALSE);
}

void            eliTraceStk_Init(s)
eliTraceStack_t *s;
{
    s->top = 0;
    s->size = 0;
    s->stack = NULL;
}

int             eliTraceStk_PushStr(st, s, node)
EliState_t *st;
eliTraceStack_t *s;
EliStr_t *node;
{
    if (s->top == s->size) {
        if (!eliTraceStk_Grow(s))
            return (FALSE);
        else
            st->numTotalErrStkNodes += GROW_STACK_SIZE;
    }
    s->stack[s->top].type = e_types_string;
    s->stack[(s->top)++].datum.strval = node;
    eliStr_IncrRefcount(node);
    ++(st->numErrStkNodes);
    return (TRUE);
}

int             eliTraceStk_PushSym(st, s, node)
EliState_t *st;
eliTraceStack_t *s;
EliSym_t *node;
{
    if (s->top == s->size) {
        if (!eliTraceStk_Grow(s))
            return (FALSE);
        else
            st->numTotalErrStkNodes += GROW_STACK_SIZE;
    }
    s->stack[s->top].type = e_types_symbol;
    s->stack[(s->top)++].datum.symval = node;
    eliSym_IncrRefcount(node);
    ++(st->numErrStkNodes);
    return (TRUE);
}

int             eliTraceStk_PushCons(st, s, node)
EliState_t *st;
eliTraceStack_t *s;
EliCons_t *node;
{
    if (s->top == s->size) {
        if (!eliTraceStk_Grow(s))
            return (FALSE);
        else
            st->numTotalErrStkNodes += GROW_STACK_SIZE;
    }
    s->stack[s->top].type = e_types_list;
    s->stack[(s->top)++].datum.consval = node;
    eliCons_IncrRefcount(node);
    ++(st->numErrStkNodes);
    return (TRUE);
}

int             eliTraceStk_PushSexp(st, s, node)
EliState_t *st;
eliTraceStack_t *s;
EliSexp_t *node;
{
    if (s->top == s->size) {
        if (!eliTraceStk_Grow(s))
            return (FALSE);
        else
            st->numTotalErrStkNodes += GROW_STACK_SIZE;
    }
    s->stack[s->top].type = e_types_Node;
    s->stack[(s->top)++].datum.Nodeval = node;
    eliSexp_IncrRefcount(node);
    ++(st->numErrStkNodes);
    return (TRUE);
}

int             eliTraceStk_PushFn(st, s, node)
EliState_t *st;
eliTraceStack_t *s;
EliFn_t *node;
{
    if (s->top == s->size) {
        if (!eliTraceStk_Grow(s))
            return (FALSE);
        else
            st->numTotalErrStkNodes += GROW_STACK_SIZE;
    }
    s->stack[s->top].type = e_types_FnNode;
    s->stack[(s->top)++].datum.FnNodeval = node;
    eliFn_IncrRefcount(node);
    ++(st->numErrStkNodes);
    return (TRUE);
}

int             eliTraceStk_PushBucketNode(st, s, node)
EliState_t *st;
eliTraceStack_t *s;
eliBucketNode_t *node;
{
    if (s->top == s->size) {
        if (!eliTraceStk_Grow(s))
            return (FALSE);
        else
            st->numTotalErrStkNodes += GROW_STACK_SIZE;
    }
    s->stack[s->top].type = e_types_bucketnode;
    s->stack[(s->top)++].datum.bucketnodeval = node;
    eliBucketNode_IncrRefcount(node);
    ++(st->numErrStkNodes);
    return (TRUE);
}

void            eliTraceStk_Pop(st, s)
EliState_t     *st;
eliTraceStack_t *s;
{
    switch (s->stack[s->top - 1].type) {
        case e_types_string:
            eliStr_DecrRefcount(st, s->stack[s->top - 1].datum.strval);
            break;
        case e_types_symbol:
            eliSym_DecrRefcount(st, s->stack[s->top - 1].datum.symval);
            break;
        case e_types_list:
            eliCons_DecrRefcount(st, s->stack[s->top - 1].datum.consval);
            break;
        case e_types_Node:
            eliSexp_DecrRefcount(st, s->stack[s->top - 1].datum.Nodeval);
            break;
        case e_types_FnNode:
            eliFn_DecrRefcount(st, s->stack[s->top - 1].datum.FnNodeval);
            break;
        case e_types_bucketnode:
            eliBucketNode_DecrRefcount(st, s->stack[s->top - 1].datum.bucketnodeval);
            break;
    }
    --(s->top);
    --(st->numErrStkNodes);
}

eliTraceStackNode_t *eliTraceStk_Top(s)
eliTraceStack_t *s;
{
    if (s->top)
        return (&(s->stack[s->top - 1]));
    return (NULL);
}

eliObjTypes_t   eliTraceStk_TopType(s)
eliTraceStack_t *s;
{
    return (s->stack[s->top - 1].type);
}

