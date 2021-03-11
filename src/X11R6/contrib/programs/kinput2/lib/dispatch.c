#ifndef lint
static char *rcsid = "$Id: dispatch.c,v 1.14 1994/05/31 07:48:42 ishisone Rel $";
#endif

/*
 * a very simple event dispatch library for non-widget windows
 *
 *	'non-widget window' means windows that have no associated widget,
 *	e.g. windows created by Xlib directly.
 */

/*
 * Copyright (c) 1990  Software Research Associates, Inc.
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

#include <X11/Intrinsic.h>
#include "MyDispatch.h"
#include "AsyncErr.h"

#define DEBUG_VAR debug_dispatch
#include "DebugPrint.h"

typedef struct _handler_ {
    int			type;		/* event type */
    unsigned long	mask;		/* event mask */
    void		(*handler)();
    XtPointer		data;
    struct _handler_	*next;
} HandlerRec;

typedef struct {
    Boolean		dispatching;	/* now dispatching */
    Boolean		toberemoved;	/* this list is to be removed later */
    unsigned long	mask;		/* event mask */
    HandlerRec		*handlers;
} WindowRec;

static int Initialized = 0;
static XContext Context;

static void
initialize()
{
    Context = XUniqueContext();
    Initialized = 1;
}

static void
resetEventMask(dpy, window, wp)
Display *dpy;
Window window;
WindowRec *wp;
{
    register HandlerRec *hp = wp->handlers;
    register unsigned long mask = 0L;

    while (hp != NULL) {
	mask |= hp->mask;
	hp = hp->next;
    }

    if (mask != wp->mask) {
	XAEHandle h = XAESetIgnoreErrors(dpy);	/* keep the operation safe */
	XSelectInput(dpy, window, mask);
	XAEUnset(h);
	wp->mask = mask;
    }
}

static void
removeAll(dpy, window, wp)
Display *dpy;
Window window;
WindowRec *wp;
{
    register HandlerRec *hp = wp->handlers;

    while (hp != NULL) {
	register HandlerRec *hp0 = hp;
	hp = hp->next;
	XtFree((char *)hp0);
    }

    if (wp->mask != 0L) {
	XAEHandle h = XAESetIgnoreErrors(dpy);

	/* keep it safe (because the window might not exist any more) */
	XSelectInput(dpy, window, 0L);
	XAEUnset(h);
    }

    XtFree((char *)wp);
    (void)XDeleteContext(dpy, window, Context);
}

static void
doDispatch(event, list)
XEvent *event;
register HandlerRec *list;
{
    void (*handler)();
    XtPointer data;
    register int type = event->type;

    /*
     * we must be careful here. the invoked handler might remove
     * itself, or remove other handler to be invoked next.
     * so we use this somewhat strange recursive call.
     */
    while (list != NULL) {
	if (list->type == type) {
	    handler = list->handler;
	    data = list->data;
	    doDispatch(event, list->next);
	    (*handler)(event, data);
	    return;
	}
	list = list->next;
    }
}

void
MyDispatchEvent(event)
XEvent *event;
{
    caddr_t data;

    if (!Initialized) initialize();

    if (!XFindContext(event->xany.display, event->xany.window,
		      Context, &data)) {
	WindowRec *wrec = (WindowRec *)data;

	wrec->dispatching = True;
	wrec->toberemoved = False;

	doDispatch(event, wrec->handlers);

	wrec->dispatching = False;
	if (wrec->toberemoved) {
	    removeAll(event->xany.display, event->xany.window, wrec);
	}
    }
}

void
MyAddEventHandler(dpy, window, type, mask, func, data)
Display *dpy;
Window window;
int type;
unsigned long mask;
void (*func)();
XtPointer data;
{
    WindowRec *wp;
    HandlerRec *hp;
    caddr_t cdata;

    TRACE(("MyAddEventHandler(window=%08lx,type=%d)\n", window, type));
    if (!Initialized) initialize();

    hp = XtNew(HandlerRec);
    hp->type = type;
    hp->mask = mask;
    hp->handler = func;
    hp->data = data;
    hp->next = NULL;

    if (!XFindContext(dpy, window, Context, &cdata)) {
	wp = (WindowRec *)cdata;
	hp->next = wp->handlers;
	wp->handlers = hp;
    } else {
	wp = XtNew(WindowRec);
	wp->mask = 0L;
	wp->dispatching = False;
	wp->handlers = hp;
	(void)XSaveContext(dpy, window, Context, (caddr_t)wp);
    }
    resetEventMask(dpy, window, wp);
}

void
MyRemoveEventHandler(dpy, window, type, func, data)
Display *dpy;
Window window;
int type;
void (*func)();
XtPointer data;
{
    caddr_t cdata;
    WindowRec *wp;
    HandlerRec *hp, *hp0;

    TRACE(("MyRemoveEventHandler(window=%08lx,type=%d)\n", window, type));
    if (!Initialized) initialize();
    if (XFindContext(dpy, window, Context, &cdata) || cdata == NULL) return;

    wp = (WindowRec *)cdata; 
    hp0 = NULL;
    hp = wp->handlers;

    while (hp != NULL) {
	if (hp->type == type && hp->handler == func && hp->data == data) {
	    HandlerRec *tmp = hp;

	    hp = hp->next;
	    if (hp0 == NULL) {
		wp->handlers = hp;
	    } else {
		hp0->next = hp;
	    }
	    XtFree((char *)tmp);
	} else {
	    hp0 = hp;
	    hp = hp->next;
	}
    }

    resetEventMask(dpy, window, wp);
    
    if (wp->handlers == NULL) {
	if (wp->dispatching) {
	    /* now dispatching. we just mark it to be removed later. */
	    wp->toberemoved = True;
	} else {
	    XtFree((char *)wp);
	    (void)XDeleteContext(dpy, window, Context);
	}
    }
}

void
MyRemoveAllEventHandler(dpy, window)
Display *dpy;
Window window;
{
    caddr_t cdata;
    WindowRec *wp;

    TRACE(("MyRemoveAllEventHandler(window=%08lx)\n", window));
    if (!Initialized) initialize();

    if (XFindContext(dpy, window, Context, &cdata) || cdata == NULL) return;

    wp = (WindowRec *)cdata;

    if (wp->dispatching) {
	/* now dispatching. we just mark it to be removed later. */
	wp->toberemoved = True;
	return;
    } else {
	removeAll(dpy, window, wp);
    }
}
