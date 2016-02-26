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
	Routines having to do with scrolling

	scrolling.c, Sat Sep 24 18:57:10 1988

		James Gosling,
		Sun Microsystems
 */

#include "document.h"
#include "psinter.h"

thumb(v, abspos)
    register struct docview *v;
    fixed       abspos;
{
    scroll_to(v, (abspos * v->doc->data->size) >> 16, 0);
}

scroll(v, relpos)
    register struct docview *v;
    fixed       relpos;
{
    if (relpos > 0) {		/* Scrolling forward */
	register    ln;
	for (ln = 0; ln < v->nlines && v->lineinfo[ln].fs.y > relpos; ln++);
	if (ln >= v->nlines)
	    ln = v->nlines - 1;
	while (ln > 0 && v->lineinfo[ln].text == 0)
	    ln--;
	if (ln > 0 && v->lineinfo[ln].text)
	    settoppos(v, v->lineinfo[ln].text->pos);
    }
    else			/* Scrolling backward */
	scroll_to(v, v->top->pos, fixedi(v->size.y) + relpos);
}

settoppos(v, pos)
    register struct docview *v;
{
    if (pos != v->top->pos) {
	v->top->pos = pos;
	ps_setscroll(v->viewport, fixedi(pos) / v->doc->data->size);
	v->modified = 1;
    }
}
