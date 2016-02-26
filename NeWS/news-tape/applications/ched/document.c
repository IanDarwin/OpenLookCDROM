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
	Routines for supporting the basic document operations

	document.c, Sun Apr 19 13:28:11 1987

		James Gosling,
		Sun Microsystems
 */

#include "document.h"

struct document *
d_create()
{
    register struct document *d;
    d = (struct document *) malloc(sizeof(struct document));
    d->data = bs_create();
    d->stylerefs = 0;
    d->nstylerefs = 0;
    d->marks = 0;
    d->views = 0;
    d->modified = 0;
    d->caret = mk_create(d, 0, 0);
    d->caret->rightside = 1;
    return d;
}

d_destroy(d)
    register struct document *d;
{
    bs_destroy(d->data);
    if (d->stylerefs)
	bs_destroy(d->stylerefs);
    mk_destroy(d->caret);
    while (d->marks)
	mk_destroy(d->marks);
    while (d->views)
	vw_destroy(d->views);
}

/*
 * insert n bytes from s into document d starting at pos
 */
d_insertbytes(d, pos, s, n)
    register struct document *d;
    char       *s;
{
    {
	register struct marker *m;
	for (m = d->marks; m; m = m->next)
	    if (m->pos > pos || m->rightside && m->pos
		    == pos) {
		m->pos += n;
		m->posmodified = 1;
	    }
	    else if (m->pos + m->span > pos) {	/* m->pos <= pos */
		m->span += n;
		m->modified = 1;
	    }
    }
    if (d->nstylerefs > 0) {
	assert(0);
    }
    d->modified = 1;
    {
	register struct docview *v;
	for (v = d->views; v; v = v->docnext)
	    v->modified = 1;
    }
    bs_insertbytes(d->data, pos, s, n);
}

d_deletebytes(d, pos, n)
    register struct document *d;
{
    if (n < 0)
	pos += n, n = -n;
    if (pos < 0)
	n += pos, pos = 0;
    if (pos + n > d->data->size)
	n = d->data->size - pos;
    if (n <= 0)
	return;
    bs_deletebytes(d->data, pos, n);
    d->modified++;
    {
	register struct docview *v;
	for (v = d->views; v; v = v->docnext)
	    v->modified = 1;
    }
    {
	register struct marker *m;
	for (m = d->marks; m; m = m->next)
	    if (m->pos + m->span > pos)
		if (m->pos >= pos + n) {
		    m->pos -= n;
		    m->posmodified = 1;
		}
		else {		/* m->pos < pos + n */
		    if (m->pos >= pos) {
			/* pos <= m->pos < pos+n */
			m->span -= n - pos + m->pos;
			if (m->span <= 0)
			    m->span = 0;
			m->pos = pos;
		    }
		    else {	/* m->pos < pos && m->pos + m->span > pos */
			if (m->pos + m->span > pos + n)
			    m->span -= n;
			else
			    m->span = pos - m->pos;
		    }
		    m->modified = 1;
		}
    }
    if (d->nstylerefs > 0) {
	assert(0);
    }
}

d_readfile(d, fd)
    register struct document *d;
{
    char        buf[8000];
    register    n;
    if (fd < 0)
	return;
    while ((n = read(fd, buf, sizeof buf)) > 0)
	d_insertbytes(d, d_caret(d), buf, n);
}
