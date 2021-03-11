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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/node.c,v 2.8 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include  <node.h>

EliSexp_t      *eliSexp_GetNew(st)
EliState_t     *st;
{
    EliSexp_t      *tmp = NULL;

    switch (st->whichScheme) {
	case e_mem_malloc:
	    tmp = (EliSexp_t *) malloc(sizeof(EliSexp_t));
	    break;
	case e_mem_pool:
	    if (!(st->g_Node_freelist)) {
		if (!(st->g_Node_freelist = eliSexp_GetNewBlock(NODE_BLOCKSIZE))) {
		    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliSexp_GetNew (allocating block)]", 0);
		    return (NULL);
		}
		else
		    st->numTotalNodes += NODE_BLOCKSIZE;
	    }
	    tmp = st->g_Node_freelist;
	    st->g_Node_freelist = tmp->freelink;
	    break;
    }
    if (tmp) {
	tmp->data.refcount = 0;
	tmp->data.type = e_data_none;
	++(st->numNodes);
    }
    return (tmp);
}

EliSexp_t      *eliSexp_GetNewBlock(numcells)
int             numcells;
{
    EliSexp_t      *result;
    int             i;

    result = (EliSexp_t *) malloc(numcells * sizeof(EliSexp_t));
    if (result) {
	for (i = 0; i < (numcells - 1); ++i)
	    (result + i)->freelink = result + i + 1;
	(result + numcells - 1)->freelink = NULL;
    }
    return (result);
}

void            eliSexp_IncrRefcount(node)
EliSexp_t      *node;
{
    ++(node->data.refcount);
}

void            eliSexp_DecrRefcount(st, node)
EliState_t     *st;
EliSexp_t      *node;
{
    if (1 > (--(node->data.refcount))) {
	--(st->numNodes);
	switch (node->data.type) {
	    case e_data_string:
		eliStr_DecrRefcount(st, node->data.datum.strval);
		break;
	    case e_data_symbol:
		eliSym_DecrRefcount(st, node->data.datum.symval);
		break;
	    case e_data_list:
		eliCons_DecrRefcount(st, node->data.datum.consval);
		break;
	    case e_data_fn:
		eliFn_DecrRefcount(st, node->data.datum.fnval);
		break;
	}
	switch (st->whichScheme) {
	    case e_mem_malloc:
		free(node);
		break;
	    case e_mem_pool:
		node->freelink = st->g_Node_freelist;
		st->g_Node_freelist = node;
		break;
	}
    }
}

void            EliSexp_SetInt(st, node, val)
EliState_t     *st;
EliSexp_t      *node;
long            val;
{
    eliDecrRefcount_SexpRef(st, node);
    node->data.type = e_data_integer;
    node->data.datum.intval = val;
}

void            EliSexp_SetSym(st, node, val)
EliState_t     *st;
EliSexp_t      *node;
EliSym_t       *val;
{
    if ((node->data.type == e_data_symbol) ? (node->data.datum.symval != val) : TRUE) {
	eliDecrRefcount_SexpRef(st, node);
	node->data.type = e_data_symbol;
	node->data.datum.symval = val;
	eliSym_IncrRefcount(val);
    }
}

void            EliSexp_SetStr(st, node, val)
EliState_t     *st;
EliSexp_t      *node;
EliStr_t       *val;
{
    if ((node->data.type == e_data_string) ? (node->data.datum.strval != val) : TRUE) {
	eliDecrRefcount_SexpRef(st, node);
	node->data.type = e_data_string;
	node->data.datum.strval = val;
	eliStr_IncrRefcount(val);
    }
}

void            EliSexp_SetCons(st, node, val)
EliState_t     *st;
EliSexp_t      *node;
EliCons_t      *val;
{
    if ((node->data.type == e_data_list) ? (node->data.datum.consval != val) : TRUE) {
	eliDecrRefcount_SexpRef(st, node);
	node->data.type = e_data_list;
	node->data.datum.consval = val;
	eliCons_IncrRefcount(val);
    }
}

void            EliSexp_SetFn(st, node, val)
EliState_t     *st;
EliSexp_t      *node;
EliFn_t        *val;
{
    if ((node->data.type == e_data_fn) ? (node->data.datum.fnval != val) : TRUE) {
	eliDecrRefcount_SexpRef(st, node);
	node->data.type = e_data_fn;
	node->data.datum.fnval = val;
	eliFn_IncrRefcount(val);
    }
}

eliDataTypes_t  EliSexp_GetType(node)
EliSexp_t      *node;
{
    return (node->data.type);
}

/*
 * Don't use this function. It's used in eliDecrRefcount_SexpRef to set the type
 * of a node to e_data_none when its datum has been deallocated 
 */

void            eliSexp_SetType(node, type)
EliSexp_t      *node;
eliDataTypes_t  type;
{
    node->data.type = type;
}

EliCons_t      *EliSexp_GetCons(n)
EliSexp_t      *n;
{
    return (n->data.datum.consval);
}

EliSym_t       *EliSexp_GetSym(n)
EliSexp_t      *n;
{
    return (n->data.datum.symval);
}

EliStr_t       *EliSexp_GetStr(n)
EliSexp_t      *n;
{
    return (n->data.datum.strval);
}

long            EliSexp_GetInt(n)
EliSexp_t      *n;
{
    return (n->data.datum.intval);
}

EliFn_t        *EliSexp_GetFn(n)
EliSexp_t      *n;
{
    return (n->data.datum.fnval);
}

void EliSexp_SetSexp(st, node1, node2)
EliState_t *st;
EliSexp_t *node1, *node2;
{
    switch (EliSexp_GetType(node2)) {
        case e_data_integer:
            EliSexp_SetInt(st, node1, EliSexp_GetInt(node2));
            break;
        case e_data_string:
            EliSexp_SetStr(st, node1, EliSexp_GetStr(node2));
            break;
        case e_data_symbol:
            EliSexp_SetSym(st, node1, EliSexp_GetSym(node2));
            break;
        case e_data_list:
            EliSexp_SetCons(st, node1, EliSexp_GetCons(node2));
            break;
        case e_data_fn:
            EliSexp_SetFn(st, node1, EliSexp_GetFn(node2));
            break;
    }
}
