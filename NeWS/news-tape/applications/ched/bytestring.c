/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef lint
static char sccsid[] = "%Z%%M% %I% %E% Copyright 1985 Sun Micro";
#endif

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

/*-
	Routines for supporting bytestrings

	bytestring.c, Sun Apr 19 11:52:26 1987

		James Gosling,
		Sun Microsystems
 */

#include "document.h"

struct bytestring *
bs_create()
{
    register struct bytestring *b;
    b = (struct bytestring *) malloc(sizeof(struct bytestring));
    b->bufsize = 0;
    b->bytes = 0;
    b->size = 0;
    b->firstsize = 0;
    b->secondpart = b->bytes;
    return b;
}

bs_destroy(b)
    register struct bytestring *b;
{
    if (b->bytes)
	free(b->bytes);
    free(b);
}

/* Move the gap to byte position n; ie. make the first part be n bytes long */
bs_movegap(b, n)
    register struct bytestring *b;
    register    n;
{
    if (b->firstsize == n)
	return;
    if (b->firstsize < n)
	bcopy(b->bytes + b->bufsize - b->size + b->firstsize,
	      b->bytes + b->firstsize,
	      n - b->firstsize);
    else
	bcopy(b->bytes + n,
	      b->bytes + b->bufsize - b->size + n,
	      b->firstsize - n);
    b->firstsize = n;
    assert(b->secondpart = b->bytes + b->bufsize - b->size);
}

/* insert n bytes from s into bytestring b starting at byte pos */
bs_insertbytes(b, pos, s, n)
    register struct bytestring *b;
{
    if (n <= 0)
	return;
    if (pos <= 0)
	pos = 0;
    if (pos >= b->size)
	pos = b->size;
    bs_movegap(b, pos);
    if (b->bufsize - b->size < n) {	/* No more room in buffer */
	register    newsize = (b->size + n) * 3 >> 1;
	if (b->bytes)
	    b->bytes = (char *) realloc(b->bytes, newsize);
	else
	    b->bytes = (char *) malloc(newsize);
	bcopy(b->bytes + b->bufsize - b->size + b->firstsize,
	      b->bytes + newsize - b->size + b->firstsize,
	      b->size - b->firstsize);
	b->bufsize = newsize;
    }
    bcopy(s, b->bytes + b->firstsize, n);
    b->firstsize += n;
    b->size += n;
    b->secondpart = b->bytes + b->bufsize - b->size;
}

/*
 * delete n bytes from bytestring b starting at byte pos.  Copes with -ve
 * values of n and regions that go outside the buffer
 */
bs_deletebytes(b, pos, n)
    register struct bytestring *b;
{
    if (n < 0)
	pos += n, n = -n;
    if (pos < 0)
	n += pos, pos = 0;
    if (pos + n > b->size)
	n = b->size - pos;
    if (n <= 0)
	return;
    if (pos + n == b->firstsize)
	b->firstsize -= n;
    else
	bs_movegap(b, pos);
    b->size -= n;
    b->secondpart = b->bytes + b->bufsize - b->size;
}

struct marker *
mk_create(d, pos, span)
    register struct document *d;
{
    register struct marker *m;
    m = (struct marker *) malloc(sizeof(struct marker));
    m->document = d;
    mk_movemark(m, pos, span);
    m->prev = 0;
    m->next = d->marks;
    d->marks = m;
    m->modified = 0;
    m->posmodified = 0;
    m->rightside = 0;
    return m;
}

mk_destroy(m)
    register struct marker *m;
{
    if (m->next)
	m->next->prev = m->prev;
    if (m->prev)
	m->prev->next = m->next;
    else {
	assert(m->document->marks == m);
	m->document->marks - m->next;
    }
    free(m);
}
