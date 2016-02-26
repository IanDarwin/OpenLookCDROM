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
static char sccsid[] = "%Z%%M% %I% %E% Copyright 1988 Sun Micro";
#endif

/*
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 */

/*-
	Deal with document styles

	styles.c, Sun Sep  4 17:16:44 1988

		James Gosling,
		Sun Microsystems
 */

#include <stdio.h>
#include "document.h"

style_applyshift(d, limit)	/* apply the style position shift to the
				 * positions in the document's style list up
				 * to the limit */
    register struct document *d;
    register    limit;
{
    if (d->shiftdistance > 0) {
	register    n;
	if (limit >= d->nstylerefs)
	    limit = d->nstylerefs - 1;
	for (n = d->firstshifted; n <= limit; n++) {
	    d_styleref(d, n)->pos += d->shiftdistance;
	    d->firstshifted++;
	}
	if (n >= d->nstylerefs) {
	    d->firstshifted = d->nstylerefs;
	    d->shiftdistance = 0;
	}
    }
    d->shiftdistance = 0;
}

style_locate(d, pos)		/* Locates the first style whose pos is >=pos.
				 * There is always one style whose pos is just
				 * beyond the end of the document */
    register struct document *d;
{
    register    lo,
                mid,
                hi;
    register struct styleref *s;
    lo = 0;
    hi = d->nstylerefs - 1;
    if (d->shiftdistance != 0)
	while (1) {
	    if (d->firstshifted < d->nstylerefs) {
		s = d_styleref(d, d->firstshifted);
		d->firstshifted++;
		s->pos += d->shiftdistance;
		if (s->pos >= pos) {
		    hi = d->firstshifted - 1;
		    break;
		}
	    }
	    else {
		d->shiftdistance = 0;
		break;
	    }
	}
    while (lo < hi) {
	mid = (lo + hi) >> 1;
	s = d_styleref(d, mid);
	if (s->pos >= pos)
	    hi = mid;
	else
	    lo = mid + 1;
    }
    assert(d_styleref(d, lo)->pos >= pos);
    assert(lo == 0 || d_styleref(d, lo - 1)->pos < pos);
    return lo;
}

style_apply(d, pos, span, style)
    register struct document *d;
    struct style *style;
{
    short       slot;
    short       parent;
    register struct styleref *s;
    struct styleref new;
    if (span <= 0 || style == 0)
	return;
    if (d->stylerefs == 0) {
	struct styleref new;
	d->stylerefs = bs_create();
	new.style = 0;
	new.pos = d->data->size;
	new.span = 0;
	new.parent = -1;
	bs_insertbytes(d->stylerefs, 0, &new, sizeof new);
    }
    slot = style_locate(d, pos);
    new.style = style;
    new.pos = pos;
    new.parent = -1;
    parent = slot - 1;
    while (parent >= 0) {
	s = d_styleref(d, parent);
	assert(s->pos < pos);
	if (s->pos + s->span > pos) {
	    new.parent = parent;
	    if (s->pos + s->span < pos + span)
		span = s->pos + s->span - pos;
	    break;
	}
	assert(s->parent < parent);
	parent = s->parent;
    }
    new.span = span;
    bs_insertbytes(d->stylerefs, slot * sizeof(struct styleref),
		   &new, sizeof new);
    d->nstylerefs++;
}

style_delete(d, slot)
    register struct document *d;
{
    register struct styleref *s = d_styleref(d, slot);
    register    limit;
    register    child;
    short       parent;
    if (slot < 0 || slot >= d->nstylerefs)
	return;
    limit = s->pos + s->span;
    parent = s->parent;
    child = slot + 1;
    while (child < d->nstylerefs) {
	s = d_styleref(d, child);
	if (s->pos >= limit)
	    break;
	if (s->parent == slot)
	    s->parent = parent;
    }
    bs_deletebytes(d->stylerefs, slot * sizeof(struct styleref),
		   sizeof(struct styleref));
}

print_styles(d)
    register struct document *d;
{
    register    slot;
    if (d->nstylerefs <= 0) {
	printf("no styles\n");
	return;
    }
    printf("%d styles", d->nstylerefs);
    if (d->shiftdistance)
	printf(", shift %d starting at %d",
	       d->shiftdistance, d->firstshifted);
    printf("\n");
    printf("  #   pos  span  parent what\n");
    for (slot = 0; slot < d->nstylerefs; slot++) {
	register struct styleref *s = d_styleref(d, slot);
	char       *comment = "?";
	if (s->style == 0)
	    comment = "NULL";
	else if (s->style == &style_bold)
	    comment = "bold";
	else if (s->style == &style_italic)
	    comment = "italic";
	else if (s->style == &style_superscript)
	    comment = "superscript";
	else if (s->style == &style_subscript)
	    comment = "subscript";
	else if (s->style == &style_smaller)
	    comment = "smaller";
	else if (s->style == &style_larger)
	    comment = "larger";
	printf("%3d%6d%6d%8d  %s\n", slot, s->pos, s->span, s->parent,
	       comment);
    }
}
