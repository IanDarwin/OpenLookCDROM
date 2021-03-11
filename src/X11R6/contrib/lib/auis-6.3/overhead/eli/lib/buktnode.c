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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/buktnode.c,v 2.8 1992/12/15 21:00:33 rr2b R6tape $";
#endif

#include  <buktnode.h>

/*
 * Returns the first bucketnode in the free area.  If one does not exist, a
 * new block of bucketnodes is allocated and linked into the freelist, and a
 * bucketnode is returned. If the allocation fails, an eli internal error
 * occurs. 
 *
 * NOTE: Initialization is performed on the returned node.  Its refcount is set
 * to zero and its datum, type and key fields are set accordingly. The datum
 * field referent has its refcount incr'd. 
 */

eliBucketNode_t *eliBucketNode_GetNew(st, datum, key)
EliState_t     *st;
EliSexp_t      *datum;
char           *key;
{
    eliBucketNode_t *tmp = NULL;

    switch (st->whichScheme) {
	case e_mem_malloc:
	    tmp = (eliBucketNode_t *) malloc(sizeof(eliBucketNode_t));
	    break;
	case e_mem_pool:
	    if (!(st->g_bucketnode_freelist)) {
		if (!(st->g_bucketnode_freelist =
		      eliBucketNode_GetNewBlock(BUCKETNODE_BLOCKSIZE))) {
		    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliBucketNode_GetNew (allocating block)]", 0);
		    return (NULL);
		}
		else
		    st->numTotalBucketNodes += BUCKETNODE_BLOCKSIZE;
	    }
	    tmp = st->g_bucketnode_freelist;
	    st->g_bucketnode_freelist = tmp->freelink;
	    break;
    }
    if (tmp) {
	tmp->data.refcount = 0;
	tmp->data.datum = datum;
	tmp->data.key = (EliSexp_GetType(datum) == e_data_string) ? datum->data.datum.strval : eliStringTable_FindOrMake(st, EliStringTable(st), key);
	if (EliErr_ErrP(st))
	    return (NULL);
	eliStr_IncrRefcount(tmp->data.key);
	eliSexp_IncrRefcount(datum);
	++(st->numBucketNodes);
    }
    return (tmp);
}

/*
  * Allocates a block of bucketnodes (the number of nodes to allocate is given
				       * as the argument).  Sets up freelinks within the block and returns a
  * pointer to the first cell in the block, which should also be the first
  * node in this block's freelist. 
  */

eliBucketNode_t *eliBucketNode_GetNewBlock(numnodes)
int             numnodes;
{
    eliBucketNode_t *result;
    int             i;

    result = (eliBucketNode_t *) malloc(numnodes * sizeof(eliBucketNode_t));
    if (result) {
	for (i = 0; i < (numnodes - 1); ++i)
	    (result + i)->freelink = result + i + 1;
	(result + numnodes - 1)->freelink = NULL;
    }
    return (result);
}

/* Set the "prev" field */

void            eliBucketNode_SetPrev(bnode, val)
eliBucketNode_t *bnode, *val;
{
    bnode->data.prev = val;
}

/* Set the "next" field */

void            eliBucketNode_SetNext(bnode, val)
eliBucketNode_t *bnode, *val;
{
    bnode->data.next = val;
}

/* Return the "prev" field */

eliBucketNode_t *eliBucketNode_GetPrev(bnode)
eliBucketNode_t *bnode;
{
    return (bnode->data.prev);
}

/* Return the "next" field */

eliBucketNode_t *eliBucketNode_GetNext(bnode)
eliBucketNode_t *bnode;
{
    return (bnode->data.next);
}

/* Return the key field */

EliStr_t       *eliBucketNode_GetKey(bnode)
eliBucketNode_t *bnode;
{
    return (bnode->data.key);
}

/* Return the datum field */

EliSexp_t      *eliBucketNode_GetSexp(bnode)
eliBucketNode_t *bnode;
{
    return (bnode->data.datum);
}

/* Increment node's refcount */

void            eliBucketNode_IncrRefcount(bnode)
eliBucketNode_t *bnode;
{
    ++(bnode->data.refcount);
}

/* Decrement node's refcount */

void            eliBucketNode_DecrRefcount(st, bnode)
EliState_t     *st;
eliBucketNode_t *bnode;
{
    if (1 > (--(bnode->data.refcount))) {
	--(st->numBucketNodes);
	eliSexp_DecrRefcount(st, bnode->data.datum);
	eliStr_DecrRefcount(st, bnode->data.key);
	switch (st->whichScheme) {
	    case e_mem_malloc:
		free(bnode);
		break;
	    case e_mem_pool:
		bnode->freelink = st->g_bucketnode_freelist;
		st->g_bucketnode_freelist = bnode;
		break;
	}
    }
}
