/**
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $NCDId: @(#)Xtutil.c,v 1.8 1994/02/28 22:26:32 greg Exp $
 *
 * This file contains utilities for writing Xt programs.
 */

#include <X11/Intrinsic.h>
#include <audio/Alibint.h>
#include "Xtutil.h"

typedef struct
{
    AuServer       *aud;
    XtAppContext    app_context;
    AuBool          syncWorkProcActive,
                    eventEnqWorkProcActive;
    XtWorkProcId    syncWorkProcId,
                    eventEnqWorkProcId;
}               PrivData;


/* ARGSUSED */
void
AuXtHandleAudioEvents(closure, pfd, pinputid)
XtPointer       closure;
int            *pfd;
XtInputId      *pinputid;
{
    AuServer       *aud = (AuServer *) closure;

    AuHandleEvents(aud);
}

static Boolean
_au_xt_syncWorkProc(data)
XtPointer       data;
{
    PrivData       *priv = (PrivData *) data;

    AuFlush(priv->aud);
    priv->syncWorkProcActive = AuFalse;
    return True;
}

static Boolean
_au_xt_eventEnqWorkProc(data)
XtPointer       data;
{
    PrivData       *priv = (PrivData *) data;

    AuHandleEvents(priv->aud);
    priv->eventEnqWorkProcActive = AuFalse;
    return True;
}

/* ARGSUSED */
static void
_au_xt_synchandler(aud, handler, data)
AuServer       *aud;
AuSyncHandlerRec *handler;
XtPointer       data;
{
    PrivData       *priv = (PrivData *) data;

    if (!priv->syncWorkProcActive)
    {
	priv->syncWorkProcId =
	    XtAppAddWorkProc(priv->app_context, _au_xt_syncWorkProc, priv);
	priv->syncWorkProcActive = AuTrue;
    }
}

/* ARGSUSED */
static void
_au_xt_eventenqhandler(aud, handler, event, data)
AuServer       *aud;
AuEventEnqHandlerRec *handler;
AuEvent        *event;
XtPointer       data;
{
    PrivData       *priv = (PrivData *) data;

    if (!priv->eventEnqWorkProcActive)
    {
	priv->eventEnqWorkProcId =
	    XtAppAddWorkProc(priv->app_context, _au_xt_eventEnqWorkProc, priv);
	priv->eventEnqWorkProcActive = AuTrue;
    }
}

typedef struct _HandlerRec
{
    AuSyncHandlerRec *sync;
    AuEventEnqHandlerRec *eenq;
    PrivData       *priv;
    XtInputId       id;
    struct _HandlerRec *next;
}               HandlerRec, *HandlerPtr;

static HandlerPtr handlerListHead,
                handlerListTail;

XtInputId
AuXtAppAddAudioHandler(app_context, aud)
XtAppContext    app_context;
AuServer       *aud;
{
    PrivData       *priv;
    HandlerPtr      h;

    if (!(h = (HandlerPtr) Aumalloc(sizeof(HandlerRec))))
	return 0;

    if (!(priv = (PrivData *) Aumalloc(sizeof(PrivData))))
    {
	Aufree(h);
	return 0;
    }

    priv->app_context = app_context;
    priv->aud = aud;
    priv->syncWorkProcActive = AuFalse;
    priv->eventEnqWorkProcActive = AuFalse;

    if (!(h->sync =
	  AuRegisterSyncHandler(aud, _au_xt_synchandler, (AuPointer) priv)))
    {
	Aufree(h);
	Aufree(priv);
	return 0;
    }

    if (!(h->eenq =
	  AuRegisterEventEnqHandler(aud, AuEventEnqueuedByReply,
				 _au_xt_eventenqhandler, (AuPointer) priv)))
    {
	AuUnregisterSyncHandler(aud, h->sync);
	Aufree(h);
	Aufree(priv);
	return 0;
    }

    if (!(h->id = XtAppAddInput(app_context, AuServerConnectionNumber(aud),
				(XtPointer) XtInputReadMask,
				AuXtHandleAudioEvents, (XtPointer) aud)))
    {
	AuUnregisterEventEnqHandler(aud, h->eenq);
	AuUnregisterSyncHandler(aud, h->sync);
	Aufree(h);
	Aufree(priv);
	return 0;
    }

    h->priv = priv;
    h->next = NULL;

    if (!handlerListTail)
	handlerListHead = handlerListTail = h;
    else
    {
	handlerListTail->next = h;
	handlerListTail = h;
    }

    return h->id;
}

void
AuXtAppRemoveAudioHandler(aud, id)
AuServer       *aud;
XtInputId       id;
{
    HandlerPtr      h = handlerListHead,
                    p = NULL;

    while (h)
    {
	if (h->priv->aud == aud && h->id == id)
	    break;

	p = h;
	h = h->next;
    }

    if (!h)
	return;

    if (p)
	p->next = h->next;
    else
	handlerListHead = h->next;

    if (!h->next)
	handlerListTail = p;

    if (h->priv->eventEnqWorkProcActive)
	XtRemoveWorkProc(h->priv->eventEnqWorkProcId);

    if (h->priv->syncWorkProcActive)
	XtRemoveWorkProc(h->priv->syncWorkProcId);

    XtRemoveInput(id);
    AuUnregisterEventEnqHandler(aud, h->eenq);
    AuUnregisterSyncHandler(aud, h->sync);
    Aufree(h->priv);
    Aufree(h);
}
