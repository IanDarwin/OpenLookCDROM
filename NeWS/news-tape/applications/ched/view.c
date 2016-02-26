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
	Routines for supporting the basic document view operations

	view.c, Mon Apr 20 09:40:01 1987

		James Gosling,
		Sun Microsystems
 */

#include "document.h"
#include "psinter.h"

struct docview *
vw_create(d)
    register struct document *d;
{
    register struct docview *v;
    v = (struct docview *) malloc(sizeof(struct docview));
    v->mapped = 0;
    v->modified = 0;
    v->doc = d;
    if (v->docnext = d->views)
	v->docnext->docprev = v;
    d->views = v;
    v->docprev = 0;
    v->nlines = 0;
    v->lineinfo = 0;
    v->caretup = 0;;
    v->allprev = 0;
    if (v->allnext = viewroot)
	v->allnext->docprev = v;
    viewroot = v;
    v->top = mk_create(d, 0, 0);
    v->size.x = v->size.y = 0;
    return v;
}

vw_destroy(v)
    register struct docview *v;
{
    vw_unmap(v);
    if (v->docnext)
	v->docnext->docprev = v->docprev;
    if (v->docprev)
	v->docprev->docnext = v->docnext;
    else {
	assert(v->doc->views == v);
	v->doc->views = v->docnext;
    }
    if (v->allnext)
	v->allnext->allprev = v->allprev;
    if (v->allprev)
	v->allprev->allnext = v->allnext;
    else {
	assert(viewroot == v);
	viewroot = v->allnext;
    }
    mk_destroy(v->top);
    free(v);
}

vw_unmap(v)
    register struct docview *v;
{
    register    i;
    if (!v->mapped)
	return;
    ps_destroywindow(v->viewport);
    for (i = v->nlines; --i >= 0;)
	mk_destroy(v->lineinfo[i].text);
    free(v->lineinfo);
    v->lineinfo = 0;
    v->nlines = 0;
    v->mapped = 0;
}

vw_createwindowforview(v, label)
    register struct docview *v;
    char       *label;
{
    if (v->mapped)
	vw_unmap(v);
    v->viewport = ds_makewindow(label);
    viewports[v->viewport] = v;
    v->mapped = 1;
    v->modified = 0;
}
