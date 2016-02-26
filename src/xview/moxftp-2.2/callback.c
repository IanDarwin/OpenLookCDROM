/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
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
"$Id: callback.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/callback.c,v $\n";
#endif

/* $Log: callback.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"

struct _callback *
link_callback(old, callback, data, cb)
struct _callback *old;
void (*callback)();
DATA   data;
struct _callback *cb;
{
    struct _callback *new;
    struct _callback *xx = old;

    if (callback == NULL && old == NULL) return NULL;
    if (xx == NULL) {
	xx = (struct _callback *)XtMalloc(sizeof(struct _callback));
	ZERO((char *)xx, sizeof(struct _callback));
	xx->last = NULL;
	xx->next = NULL;
	new = xx;
    } else {
	new = (struct _callback *)XtMalloc(sizeof(struct _callback));
	ZERO((char *)new, sizeof(struct _callback));
	if (xx->last == NULL) {
	    xx->last = new;
	    xx->next = new;
	} else {
	    xx->last->next = new;
	    xx->last= new;
	}	
    }
    new->callback = callback;
    new->data     = data;
    new->cb       = cb;
    return new;
}


void
do_callbacks(code,  cbp)
int code;
struct _callback *cbp;
{
    struct _callback *next;
    struct _callback *prev;

    if (cbp == NULL) return;

    next = prev = cbp;

    while (next) {
	next->callback(next->data, code, next->cb);
	next = next->next;
	XtFree((char *)prev);
	prev = next;
    }
}
