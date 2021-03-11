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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/index/RCS/rcordset.c,v 2.8 1993/09/23 20:11:23 gk5g Exp $";
#endif


 

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <stdio.h>

#include "index.h"

/*
 * Internal routine: return a new, empty hash list.  Empty hash lists are not represented
 * by null pointers, but rather a hash list with a zero entry count.
 */
struct hashList *index_NewHL()
{
    register struct hashList *th;
    th = (struct hashList *) malloc (sizeof (struct hashList));
    th->next = (struct hashList *) 0;
    th->nentries = 0;
    return th;
}

/*
  * Internal routine: given a hash list and a hash value, return true if the value is
      * contained in the list, and false otherwise.
      */
index_HashPresent(alist, ahash)
register struct hashList *alist;
register long ahash;
{
    register int i;
    for(;alist;alist=alist->next) {
	for(i=0;i<alist->nentries;i++) {
	    if (ahash == alist->entries[i]) return 1;
	}
    }
    return 0;
}

/*
  * Internal routine: given a hash list and a hash value, delete the value from
  * the hash list.
  */
index_HashRemove(alist, ahash)
register struct hashList *alist;
register long ahash;
{
    register int i;
    for(;alist;alist=alist->next) {
	for(i=0;i<alist->nentries;i++) {
	    if (ahash == alist->entries[i]) alist->entries[i] = 0xffff;
	}
    }
    return 0;
}

/*
  * Internal routine: given a hash list and a hash value, add the value to the list.
  */
index_HashAdd(alist, ahash)
register struct hashList *alist;
register long ahash;
{
    register struct hashList *tlist;
    for(tlist=alist;tlist;tlist=tlist->next) {
	if (tlist->nentries < MAXHL) {
	    tlist->entries[tlist->nentries++] = ahash;
	    return;
	}
    }
    tlist = (struct hashList *) malloc(sizeof(struct hashList *));
    tlist->nentries = 1;
    tlist->next = alist->next;
    alist->next = tlist;
    tlist->entries[0] = ahash;
}

/*
  * Internal routine: given a hash list, free it.
  */
index_FreeHL(alist)
register struct hashList *alist;
{
    register struct hashList *next;
    for(;alist;alist=next) {
	next = alist->next;
	free(alist);
    }
}

/*
  * Create a new, empty record set.
  */
struct recordSet *recordset_New(asize)
{
    register struct recordSet *tr;
    if (asize <= 0) asize = 1;
    tr = (struct recordSet *) malloc(sizeof(struct recordSet));
    tr->count = 0;
    tr->acount = asize;
    tr->data = (struct recordID *) malloc(asize * sizeof(struct recordID));
    return tr;
}

/*
  * Free a record set.
  */
recordset_Free(aset)
register struct recordSet *aset;
{
    free(aset->data);
    free(aset);
}

/*
  * Add a record id to a record set.
  */
recordset_Add(aset, arid)
register struct recordSet *aset;
register struct recordID *arid;
{
    register long c;
    register struct recordID *tid;
    long newSize;
    for(tid=aset->data,c=0; c<aset->count; tid++,c++) {
	if (req(*tid, *arid)) return;
    }
    c = aset->count;
    if (c>= aset->acount) {
	newSize = 2*c;
	aset->data = (struct recordID *) realloc(aset->data, newSize*sizeof(struct recordID));
	aset->acount = newSize;
    }
    rset(aset->data[c], *arid);
    aset->count = c+1;
}

/* more recordset operations to come as they become necessary */
