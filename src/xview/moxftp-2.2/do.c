/*
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: do.c,v 1.2 1994/05/20 20:13:56 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/do.c,v $\n";
#endif

/* $Log: do.c,v $
 * Revision 1.2  1994/05/20  20:13:56  jones
 * Only add register function once!
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"

struct _do_func {
        int      flags;
	void     (*func)();
	struct _do_func *next;
};

static struct _do_func *first = NULL;

void
add_func_do(func, flags)
void (*func)();
int   flags;
{
    struct _do_func *next, *new;

    next = first;
    while (next) {
	if (next->func == func && next->flags == flags) return;
	next = next->next;
    }

    new = (struct _do_func *)XtMalloc(sizeof(struct _do_func));
    ZERO((char *)new, sizeof(struct _do_func));

    if (first == NULL) {
	first = new;
    }  else {
	next = first;
	while (next->next) next = next->next;
	next->next = new;
    }
    new->func = func;
    new->flags = flags;
}

	
void
do_all(flag)
int flag;
{
    struct _do_func *next = first;
    struct _do_func *prev = NULL;
    struct _do_func *temp = NULL;

    while (next) {
	if (next->flags & flag) {
	    next->func(flag);
	    if (next->flags & DO_ONCE) {
		if (prev != NULL) prev->next = next->next;
		else 		  first = next->next; 
		temp = next;
	        next = next->next;
	    }
	}
	if (temp) { 
	    XtFree((char *)temp);
	    temp = NULL;
	}
	prev = next;
	next = next->next;
    }
}

void
remove_func_do(func, flags)
void (*func)();
int   flags;
{
    struct _do_func *next = first;
    struct _do_func *prev = NULL;
    struct _do_func *temp = NULL;

    while (next) {
	if (next->func == func && next->flags == flags) {
	    if (prev != NULL) prev->next = next->next;
	    else 	      first = next->next; 
	    temp = next;
	    next = next->next;
	    XtFree((char *)temp);
	    return;
	}
	prev = next;
	next = next->next;
    }
}



