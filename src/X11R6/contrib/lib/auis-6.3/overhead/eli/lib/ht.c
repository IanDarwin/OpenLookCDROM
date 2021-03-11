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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/eli/lib/RCS/ht.c,v 2.6 1992/12/15 21:01:25 rr2b R6tape $";
#endif

#include  <ht.h>

/* Default hashing function */

int             eliHT_Hash(key)
char           *key;
{
    int             result = 0;
    char           *p = key;

    do
	result += *p;
    while (*(p++));
    return (result % NUMBUCKETS);
}

/* Initialize a hash table */

void            eliHT_Init(h)
eliHashTable_t *h;
{
    int             i;

    h->hash = eliHT_Hash;
    for (i = 0; i < NUMBUCKETS; ++i)
	eliBucket_Init(&(h->buckets[i]));
}

/*
 * Create a bucketnode to hold datum, and insert into the hash table with the
 * given key 
 */

void            eliHT_Insert(st, h, datum, key)
EliState_t     *st;
eliHashTable_t *h;
EliSexp_t      *datum;
char           *key;
{
    eliBucketNode_t *tmp;

    if (!(tmp = eliBucketNode_GetNew_trace(st, EliTraceStk(st), datum, key)))
	return;
    eliBucket_Insert(&(h->buckets[(*(h->hash)) (key)]), tmp);
}

/* Return the datum within the proper bucketnode whose key is key */

EliSexp_t      *eliHT_Find(h, key)
eliHashTable_t *h;
char           *key;
{
    return (eliBucket_Find(&(h->buckets[(*(h->hash)) (key)]), key));
}

/* Remove the bucketnode whose key is key */

void            eliHT_Delete(st, h, key)
EliState_t     *st;
eliHashTable_t *h;
char           *key;
{
    eliBucket_Delete(st, &(h->buckets[(*(h->hash)) (key)]), key);
}

/* Some may never wish to use this */

void            eliHT_SetHashFn(h, fn)
eliHashTable_t *h;
int             (*fn) ();

{
    h->hash = fn;
}
