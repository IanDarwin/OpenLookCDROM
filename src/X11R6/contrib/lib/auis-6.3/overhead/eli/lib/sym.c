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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/sym.c,v 2.7 1992/12/15 21:02:39 rr2b R6tape $";
#endif

#include  <sym.h>

/*
 * Returns the first symnode in the free area.  If one does not exist, a new
 * block of symnodes is allocated and linked into the freelist, and a symnode
 * is returned. If the allocation fails, an eli internal error occurs. 
 *
 * NOTE: Initialization is performed on the returned node.  Its refcount is set
 * to zero and its val and fnval fields are set to NULL (unbound). Also, its
 * name field is set to point to the given strnode, whose ref count is
 * incremented. The freelink field is also eradicated unnecessarily. 
 */

EliSym_t       *eliSym_GetNew(st, strnode)
EliState_t     *st;
EliStr_t       *strnode;
{
    EliSym_t       *tmp = NULL;

    switch (st->whichScheme) {
	case e_mem_malloc:
	    tmp = (EliSym_t *) malloc(sizeof(EliSym_t));
	    break;
	case e_mem_pool:
	    if (!(st->g_sym_freelist)) {
		if (!(st->g_sym_freelist = eliSym_GetNewBlock(SYM_BLOCKSIZE))) {
		    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliSym_GetNew (allocating block)]", 0);
		    return (NULL);
		}
		else
		    st->numTotalSymNodes += SYM_BLOCKSIZE;
	    }
	    tmp = st->g_sym_freelist;
	    st->g_sym_freelist = tmp->freelink;
	    break;
    }
    if (tmp) {
	tmp->data.refcount = 0;
	if (!(tmp->data.val = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
	    switch (st->whichScheme) {
		case e_mem_malloc:
		    free(tmp);
		    break;
		case e_mem_pool:
		    tmp->freelink = st->g_sym_freelist;
		    st->g_sym_freelist = tmp;
		    break;
	    }
	    return (NULL);
	}
	eliSexp_IncrRefcount(tmp->data.val);
	if (!(tmp->data.fnval = eliFn_GetNew_trace(st, EliTraceStk(st)))) {
	    eliSexp_DecrRefcount(st, tmp->data.val);
	    switch (st->whichScheme) {
		case e_mem_malloc:
		    free(tmp);
		    break;
		case e_mem_pool:
		    tmp->freelink = st->g_sym_freelist;
		    st->g_sym_freelist = tmp;
		    break;
	    }
	    return (NULL);
	}
	eliFn_IncrRefcount(tmp->data.fnval);
	tmp->data.name = strnode;
	eliStr_IncrRefcount(strnode);
	tmp->data.type = e_sym_unknown;
	++(st->numSymNodes);
    }
    return (tmp);
}

/*
  * Allocates a block of symnodes (the number of nodes to allocate is given as
				    * the argument).  Sets up freelinks within the block and returns a pointer
  * to the first cell in the block, which should also be the first node in
  * this block's freelist. 
*/

EliSym_t       *eliSym_GetNewBlock(numnodes)
int             numnodes;
{
    EliSym_t       *result;
    int             i;

    result = (EliSym_t *) malloc(numnodes * sizeof(EliSym_t));
    if (result) {
	for (i = 0; i < (numnodes - 1); ++i)
	    (result + i)->freelink = result + i + 1;
	(result + numnodes - 1)->freelink = NULL;
    }
    return (result);
}

/*
  * Sets the val field of the symnode to the value given. Performs all
  * necessary administration; namely, calls "decr_refcount" on the val field's
  * old referent; calls "incr_refcount" on the new referent. Of course, this
  * assumes that the val-field is already bound, which it need not be. 
  */
/* NOTE: This is really tangled and should be rewritten */
void            EliSym_BindSexp(st, node, val)
EliState_t     *st;
EliSym_t       *node;
EliSexp_t      *val;
{
    int             decr = TRUE, incr = TRUE;

    if (node->data.val->data.type == e_data_symbol)
	if (node->data.val->data.datum.symval == node)
	    decr = FALSE;
    if (val->data.type == e_data_symbol)
	if (val->data.datum.symval == node)
	    incr = FALSE;
    if (val == node->data.val)
	incr = decr = FALSE;
    if (decr)
	eliSexp_DecrRefcount(st, node->data.val);
    node->data.val = val;
    if (incr)
	eliSexp_IncrRefcount(node->data.val);
}

/* Slightly tangled */
int             eliSym_DecrRefcount(st, node)
EliState_t     *st;
EliSym_t       *node;
{
    int             result, decr = TRUE;

    if (1 > (result = --(node->data.refcount))) {
	--(st->numSymNodes);
	if (EliSexp_GetType(node->data.val) == e_data_symbol)
	    if (node->data.val->data.datum.symval == node)
		decr = FALSE;
	eliFn_DecrRefcount(st, node->data.fnval);
	eliStr_DecrRefcount(st, node->data.name);
	if (decr)
	    eliSexp_DecrRefcount(st, node->data.val);
	switch (st->whichScheme) {
	    case e_mem_malloc:
		free(node);
		break;
	    case e_mem_pool:
		node->freelink = st->g_sym_freelist;
		st->g_sym_freelist = node;
		break;
	}
    }
    else if ((1 == result) && (eliSym_GetScope(node) == e_sym_known))
	eliHT_Delete(st, EliTempSymTable(st), EliStr_GetString(EliSym_GetName(node)));
    return (result);
}

void            eliSym_IncrRefcount(node)
EliSym_t       *node;
{
    ++(node->data.refcount);
}

EliSexp_t      *EliSym_GetSexp(node)
EliSym_t       *node;
{
    return (node->data.val);
}

/* Bind a function node to the sym */

void            EliSym_BindFn(st, node, val)
EliState_t     *st;
EliSym_t       *node;
EliFn_t        *val;
{
    int             decr = TRUE;

    if (node->data.fnval == val)
	decr = FALSE;
    if (decr)
	eliFn_DecrRefcount(st, node->data.fnval);
    node->data.fnval = val;
    if (decr)
	eliFn_IncrRefcount(val);
}

/* Return the functional value of a symbol */

EliFn_t        *EliSym_GetFn(node)
EliSym_t       *node;
{
    return (node->data.fnval);
}

/* Return the strnode containing the symnode's name */

EliStr_t       *EliSym_GetName(node)
EliSym_t       *node;
{
    return (node->data.name);
}

eliSymScopes_t  eliSym_GetScope(node)
EliSym_t       *node;
{
    return (node->data.type);
}

void            eliSym_SetScope(node, val)
EliSym_t       *node;
eliSymScopes_t  val;
{
    node->data.type = val;
}

int             eliSym_GetRefcount(node)
EliSym_t       *node;
{
    return (node->data.refcount);
}
