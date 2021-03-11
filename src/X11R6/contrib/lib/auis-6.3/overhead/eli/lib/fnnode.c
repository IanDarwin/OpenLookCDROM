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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/fnnode.c,v 2.8 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include  <fnnode.h>

EliFn_t        *eliFn_GetNew(st)
EliState_t     *st;
{
    EliFn_t        *tmp = NULL;

    switch (st->whichScheme) {
	case e_mem_malloc:
	    tmp = (EliFn_t *) malloc(sizeof(EliFn_t));
	    break;
	case e_mem_pool:
	    if (!(st->g_FnNode_freelist)) {
		if (!(st->g_FnNode_freelist = eliFn_GetNewBlock(FNNODE_BLOCKSIZE))) {
		    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliFn_GetNew (allocating block)]", 0);
		    return (NULL);
		}
		else
		    st->numTotalFnNodes += FNNODE_BLOCKSIZE;
	    }
	    tmp = st->g_FnNode_freelist;
	    st->g_FnNode_freelist = tmp->freelink;
	    break;
    }
    if (tmp) {
	tmp->data.refcount = 0;
	tmp->data.type = e_fn_none;
	++(st->numFnNodes);
    }
    return (tmp);
}

EliFn_t        *eliFn_GetNewBlock(numcells)
int             numcells;
{
    EliFn_t        *result;
    int             i;

    result = (EliFn_t *) malloc(numcells * sizeof(EliFn_t));
    if (result) {
	for (i = 0; i < (numcells - 1); ++i)
	    (result + i)->freelink = result + i + 1;
	(result + numcells - 1)->freelink = NULL;
    }
    return (result);
}

void            eliFn_IncrRefcount(node)
EliFn_t        *node;
{
    ++(node->data.refcount);
}

int             eliFn_DecrRefcount(st, node)
EliState_t     *st;
EliFn_t        *node;
{
    int             result;

    if (1 > (result = (--(node->data.refcount)))) {
	--(st->numFnNodes);
	if (node->data.type == e_fn_list)
	    eliCons_DecrRefcount(st, node->data.fn.consval);
	switch (st->whichScheme) {
	    case e_mem_malloc:
		free(node);
		break;
	    case e_mem_pool:
		node->freelink = st->g_FnNode_freelist;
		st->g_FnNode_freelist = node;
		break;
	}
    }
    return (result);
}

void            eliFn_SetCons(st, node, fn)
EliState_t     *st;
EliFn_t        *node;
EliCons_t      *fn;
{
    EliCons_t      *tmp;

    if (node->data.type == e_fn_list)
	tmp = node->data.fn.consval;
    else
	tmp = NULL;
    node->data.type = e_fn_list;
    node->data.fn.consval = fn;
    eliCons_IncrRefcount(fn);
    if (tmp)
	eliCons_DecrRefcount(st, tmp);
}

void            eliFn_SetCompiled(st, node, fn)
EliState_t     *st;
EliFn_t        *node;
void            (*fn) ();

{
    if (node->data.type == e_fn_list)
	eliCons_DecrRefcount(st, node->data.fn.consval);
    node->data.type = e_fn_compiled;
    node->data.fn.compiled = fn;
}

eliFnTypes_t    eliFn_GetType(node)
EliFn_t        *node;
{
    return (node->data.type);
}

EliCons_t      *eliFn_GetCons(node)
EliFn_t        *node;
{
    return (node->data.fn.consval);
}

void            (*eliFn_GetCompiled(node)) ()
EliFn_t        *node;
{
    return (node->data.fn.compiled);
}
