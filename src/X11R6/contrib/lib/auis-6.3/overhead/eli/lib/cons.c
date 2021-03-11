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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/cons.c,v 2.7 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include  <cons.h>

/*
 * Returns the first cons cell in the free area.  If one does not exist, a
 * new block of cons cells is allocated and linked into the freelist, and a
 * cons cell is returned. If the allocation fails, an eli internal error
 * occurs. 
 *
 * NOTE: Initialization is performed on the returned cell.  Its refcount is set
 * to zero and its car and cdr are set to point to nil. The freelink field is
 * also eradicated unnecessarily. 
 */

EliCons_t      *eliCons_GetNew(st)
EliState_t     *st;
{
    EliCons_t      *tmp = NULL;

    switch (st->whichScheme) {
	case e_mem_malloc:
	    tmp = (EliCons_t *) malloc(sizeof(EliCons_t));
	    break;
	case e_mem_pool:
	    if (!(st->g_cons_freelist)) {
		if (!(st->g_cons_freelist = eliCons_GetNewBlock(CONS_BLOCKSIZE))) {
		    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "INTERNAL [eliCons_GetNew (allocating block)]", 0);
		    return (NULL);
		}
		else
		    st->numTotalConsCells += CONS_BLOCKSIZE;
	    }
	    tmp = st->g_cons_freelist;
	    st->g_cons_freelist = tmp->freelink;
	    break;
    }
    if (tmp) {
	tmp->data.refcount = 0;
	if (!(tmp->data.car = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
	    switch (st->whichScheme) {
		case e_mem_malloc:
		    free(tmp);
		    break;
		case e_mem_pool:
		    tmp->freelink = st->g_cons_freelist;
		    st->g_cons_freelist = tmp;
		    break;
	    }
	    return (NULL);
	}
	eliSexp_IncrRefcount(tmp->data.car);
	if (!(tmp->data.cdr = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
	    eliSexp_DecrRefcount(st, tmp->data.car);
	    switch (st->whichScheme) {
		case e_mem_malloc:
		    free(tmp);
		    break;
		case e_mem_pool:
		    tmp->freelink = st->g_cons_freelist;
		    st->g_cons_freelist = tmp;
		    break;
	    }
	    return (NULL);
	}
	eliSexp_IncrRefcount(tmp->data.cdr);
	++(st->numConsCells);
    }
    return (tmp);
}

/*
 * Allocates a block of cons cells (the number of cells to allocate is given
 * as the argument).  Sets up freelinks within the block and returns a
 * pointer to the first cell in the block, which should also be the first
 * cell in this block's freelist. 
 */

EliCons_t      *eliCons_GetNewBlock(numcells)
int             numcells;
{
    EliCons_t      *result;
    int             i;

    result = (EliCons_t *) malloc(numcells * sizeof(EliCons_t));
    if (result) {
	for (i = 0; i < (numcells - 1); ++i)
	    (result + i)->freelink = result + i + 1;
	(result + numcells - 1)->freelink = NULL;
    }
    return (result);
}

/*
 * Sets the car field of the cons cell given to the value given, which is of
 * the given type. Performs all necessary administration; namely, calls
 * "decr_refcount" on the car's old referent; calls "incr_refcount" on the
 * new referent. Of course, this entails that the car is already bound, which
 * it should be if the user properly called eliCons_GetNew() and didn't screw
 * with the cell's innards. 
 */

void            EliCons_BindCar(st, cell, val)
EliState_t     *st;
EliCons_t      *cell;
EliSexp_t      *val;
{
    eliSexp_IncrRefcount(val);
    eliSexp_DecrRefcount(st, cell->data.car);
    cell->data.car = val;
}

/*
 * Sets the cdr field of the cons cell given to the value given, which is of
 * the given type. Performs all necessary administration; namely, calls
 * "decr_refcount" on the cdr's old referent; calls "incr_refcount" on the
 * new referent. Of course, this entails that the cdr is already bound, which
 * it should be if the user properly called eliCons_GetNew() and didn't screw
 * with the cell's innards. 
 */

void            EliCons_BindCdr(st, cell, val)
EliState_t     *st;
EliCons_t      *cell;
EliSexp_t      *val;
{
    eliSexp_IncrRefcount(val);
    eliSexp_DecrRefcount(st, cell->data.cdr);
    cell->data.cdr = val;
}

int             eliCons_DecrRefcount(st, cell)
EliState_t     *st;
EliCons_t      *cell;
{
    int             result;

    if (1 > (result = (--(cell->data.refcount)))) {
	--(st->numConsCells);
	eliSexp_DecrRefcount(st, cell->data.car);
	eliSexp_DecrRefcount(st, cell->data.cdr);
	switch (st->whichScheme) {
	    case e_mem_malloc:
		free(cell);
		break;
	    case e_mem_pool:
		cell->freelink = st->g_cons_freelist;
		st->g_cons_freelist = cell;
		break;
	}
    }
    return (result);
}

void            eliCons_IncrRefcount(cell)
EliCons_t      *cell;
{
    ++(cell->data.refcount);
}

/*
 * Traverses the top level of the list whose head is the cons cell given. The
 * last cons cell in the top level is returned. 
 */

EliCons_t      *EliLastCell(cell)
EliCons_t      *cell;
{
    EliCons_t      *result;

    for (result = cell; result->data.cdr->data.type == e_data_list; result = result->data.cdr->data.datum.consval);
    return (result);
}

EliSexp_t      *EliCons_GetCar(cell)
EliCons_t      *cell;
{
    return (cell->data.car);
}

EliSexp_t      *EliCons_GetCdr(cell)
EliCons_t      *cell;
{
    return (cell->data.cdr);
}
