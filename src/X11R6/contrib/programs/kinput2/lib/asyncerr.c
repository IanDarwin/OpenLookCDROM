#ifndef lint
static char *rcsid = "$Id: asyncerr.c,v 1.5 1994/05/31 06:40:12 ishisone Rel $";
#endif
/*
 * Copyright (C) 1992, 1994  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

/*
 *	X asyncronous error handler
 */

#include <X11/Xlib.h>
#include <X11/Xfuncproto.h>
#include "AsyncErr.h"

#ifdef __STDC__
#include <stdlib.h>
#else
extern char *malloc();
#endif

#undef XSetErrorHandler

typedef struct fe_errdesc_ {
    struct fe_errdesc_ *prev;
    struct fe_errdesc_ *next;
    Display *dpy;
    unsigned long from;		/* range of the sequence numbers */
    unsigned long to;		/* which this handler covers */
    int (*handler)();		/* async handler */
    void (*destroy)();
    XPointer client_data;
} ErrDesc;

static ErrDesc esentinel = {
    &esentinel,
    &esentinel,
};

#define EHEAD	(esentinel.next)
#define ETAIL	(esentinel.prev)
#define EEND(p)	((p) == &esentinel)

static int (*original_handler)();

/*
 * Some useful handlers
 */

/* ARGSUSED */
static int
ignoreErrors(dpy, eev, cldata)
Display *dpy;
XErrorEvent *eev;
XPointer cldata;
{
    /*
     * Just ignore any errors.
     */
    return 0;
}

/* ARGSUSED */
static int
recordErrors(dpy, eev, cldata)
Display *dpy;
XErrorEvent *eev;
XPointer cldata;
{
    /*
     * Record the type of the error.
     * It assumes that the client data is a pointer to
     * an unsigned long int.  If error occurs, it does:
     *   + if the error code is less than 32, set the 'error code'th bit.
     *   + otherwise (i.e. extension defined error) set the bit 0.
     */
    unsigned long *errorbitsp = (unsigned long *)cldata;

    if (eev->error_code < 32) {
	*errorbitsp |= (1 << eev->error_code);
    } else {
	/* other errors */
	*errorbitsp |= 1;
    }

    /* don't invoke global handler */
    return 0;
}

static ErrDesc *
newErrDesc()
{
    return (ErrDesc *)malloc(sizeof(ErrDesc));
}

static void
eremove(edp)
ErrDesc *edp;
{
    edp->prev->next = edp->next;
    edp->next->prev = edp->prev;
}

static void
eappend(edp)
ErrDesc *edp;
{
    edp->prev = esentinel.prev;
    edp->next = &esentinel;
    (esentinel.prev)->next = edp;
    esentinel.prev = edp;
}

static void
removeHandler(edp)
ErrDesc *edp;
{
    if (edp->destroy != NULL) (*edp->destroy)(edp->dpy, edp->client_data);
    eremove(edp);
    (void)free((char *)edp);
}

static int
callHandlers(dpy, eev)
Display *dpy;
XErrorEvent *eev;
{
    ErrDesc *edp = EHEAD;
    int found = 0;
    int call_original = 0;

    while (!EEND(edp)) {
	if (edp->dpy == dpy && edp->from <= eev->serial &&
	    (eev->serial < edp->to || edp->from == edp->to)) {
	    found = 1;
	    if ((*edp->handler)(dpy, eev, edp->client_data)) call_original = 1;
	}
	edp = edp->next;
    }
    return !found || call_original;
}

static void
removeHandlers(dpy)
Display *dpy;
{
    /*
     * Remove obsolete (out of date) handlers.
     */
    ErrDesc *edp = EHEAD;
    unsigned long last = LastKnownRequestProcessed(dpy);

    while (!EEND(edp)) {
	ErrDesc *next = edp->next;

	if (edp->dpy == dpy && edp->to <= last && edp->from != edp->to) {
	    removeHandler(edp);
	}
	edp = next;
    }
}

/*
 * public functions
 */

int
XAEHandler(dpy, eev)
Display *dpy;
XErrorEvent *eev;
{
    if (callHandlers(dpy, eev) && original_handler != NULL) {
	(void)original_handler(dpy, eev);
    }
    removeHandlers(dpy);
    return 0;	/* for lint */
}

void
XAEInit()
{
    int (*oldhandler)() = XSetErrorHandler(XAEHandler);

    if (oldhandler != XAEHandler) original_handler = oldhandler;
}

XErrorHandler
XAESetErrorHandler(handler)
XErrorHandler handler;
{
    int (*oldhandler)();

    oldhandler = original_handler;
    original_handler = handler;
    return oldhandler;
}

XAEHandle
XAESet(dpy, handler, destroy, client_data)
Display *dpy;
int (*handler)();
void (*destroy)();
XPointer client_data;
{
    ErrDesc *e;

    /*
     * First, remove out-of-date handlers.
     */
    removeHandlers(dpy);

    /*
     * Allocate new ErrDesc structure.
     */
    e = newErrDesc();
    if (e == NULL) return NULL;

    e->dpy = dpy;
    e->from = NextRequest(dpy);
    e->to = e->from;
    e->handler = handler;
    e->destroy = destroy;
    e->client_data = client_data;

    eappend(e);

    return e;
}

void
XAEUnset(handle)
XAEHandle handle;
{
    Display *dpy = handle->dpy;

    if (handle != NULL && handle->from == handle->to) {
	handle->to = NextRequest(dpy);
	if (handle->to <= handle->from) removeHandler(handle);
    }
    removeHandlers(dpy);
}

XAEHandle
XAESetIgnoreErrors(dpy)
Display *dpy;
{
    return XAESet(dpy, ignoreErrors, (void (*)())NULL, (XPointer)NULL);
}

XAEHandle
XAESetRecordErrors(dpy, errorbitsp)
Display *dpy;
unsigned long *errorbitsp;
{
    *errorbitsp = 0L;		/* clear all bits */
    return XAESet(dpy, recordErrors,
		  (void (*)())NULL, (XPointer)errorbitsp);
}
