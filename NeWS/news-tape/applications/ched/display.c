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
	Display handling routines

	display.c, Mon Apr 20 10:30:57 1987

		James Gosling,
		Sun Microsystems
 */

#include "document.h"
#include "psinter.h"

static struct font *fontroot;

/*
 * Create a font structure given a name and point size.  If a structure for
 * the named font already exists, return it. Otherwise, send the name and size
 * off the the server and build a structure out of the returned information
 */
struct font *
ft_create(name, size)
    char       *name;
{
    register struct font *f;
    register    index;
    int         length,
                bbheight,
                descent;
    for (f = fontroot; f; f = f->next)
	if (f->size == size && f->name[0] == name[0] && f->name[1] == name[1]
		&& strcmp(name, f->name) == 0)
	    return f;
    index = ps_next_user_token++;
    ps_defstr(name, size, index, &length, &bbheight, &descent);
    f = (struct font *) malloc(sizeof(struct font) + (length >> 1) * sizeof f->width[0] + strlen(name));
    f->next = fontroot;
    fontroot = f;
    f->size = size;
    f->fontindex = index;
    f->nchars = length >> 1;
    f->name = (char *) &f->width[f->nchars];
    f->bbheight = bbheight;
    f->descent = descent < 0 ? -descent : descent;
    strcpy(f->name, name);
    for (index = f->nchars; --index >= 0;)
	ps_getint(&f->width[index]);
    return f;
}

/* Make a window and return the index that refers to it */
ds_makewindow(label)
    char       *label;
{
    register    index = ps_next_user_token++;
    ps_makewindow(index, label);
    return index;
}
