/*

Copyright 1986, 1990 by the Massachusetts Institute of Technology

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/
/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#define NEED_REPLIES
#include "Alibint.h"

#define TABLESIZE 64

typedef struct _Entry {
    unsigned long sig;
    AAtom atom;
} EntryRec, *Entry;

#define EntryName(e) ((char *)(e+1))

typedef struct _ADisplayAtoms {
    Entry table[TABLESIZE];
} AtomTable;

#define HASH(sig) ((sig) & (TABLESIZE-1))
#define REHASHVAL(sig) ((((sig) % (TABLESIZE-3)) + 2) | 1)
#define REHASH(idx,rehash) ((idx + rehash) & (TABLESIZE-1))

static void
_AFreeAtomTable(AFAudioConn *aud)
{
    register Entry *table;
    register int i;
    register Entry e;

    if (table = aud->atoms->table) {
	for (i = TABLESIZE; --i >= 0; ) {
	    if (e = *table++)
		Xfree((char *)e);
	}
	Xfree((char *)aud->atoms);
    }
}

AAtom AFInternAtom (
    AFAudioConn *aud,
    const char *name,
    ABool onlyIfExists)
{
    register AtomTable *atoms;
    register char *s1, c, *s2;
    register unsigned long sig;
    register int idx, i;
    Entry e, oe;
    int n, firstidx, rehash;
    aInternAtomReply rep;
    aInternAtomReq *req;

    if (!name)
	name = "";
    LockConnection(aud);
    /* look in the cache first */
    if (!(atoms = aud->atoms)) {
	aud->atoms = atoms = (AtomTable *)Xcalloc(1, sizeof(AtomTable));
	aud->free_funcs->atoms = _AFreeAtomTable;
    }
    sig = 0;
    for (s1 = (char *)name; c = *s1++; )
	sig = (sig << 1) + c;
    n = s1 - (char *)name - 1;
    if (atoms) {
	firstidx = idx = HASH(sig);
	while (e = atoms->table[idx]) {
	    if (e->sig == sig) {
	    	for (i = n, s1 = (char *)name, s2 = EntryName(e); --i >= 0; ) {
		    if (*s1++ != *s2++)
		    	goto nomatch;
	    	}
	    	if (!*s2) {
		    rep.atom = e->atom;
		    UnlockConnection(aud);
		    return rep.atom;
	    	}
	    }
nomatch:    if (idx == firstidx)
		rehash = REHASHVAL(sig);
	    idx = REHASH(idx, rehash);
	    if (idx == firstidx)
		break;
	}
    }
    /* not found, go to the server */
    GetReq(InternAtom, req);
    req->nbytes = n;
    req->onlyIfExists = onlyIfExists;
    req->length += (n+3)>>2;
    _ASend (aud, (char *)name, n);
    	/* use _ASend instead of Data, since the following _AReply
           will always flush the buffer anyway */
    if(_AReply (aud, (aReply *)&rep, 0, aTrue) == 0) {
	rep.atom = ANone;
    } else if (rep.atom && atoms) {
	/* store it in the cache */
	e = (Entry)Xmalloc(sizeof(EntryRec) + n + 1);
	if (e) {
	    e->sig = sig;
	    e->atom = rep.atom;
	    strcpy(EntryName(e), name);
	    if (oe = atoms->table[idx])
		Xfree((char *)oe);
	    atoms->table[idx] = e;
	}
    }
    UnlockConnection(aud);
    SyncHandle();
    return (rep.atom);
}
