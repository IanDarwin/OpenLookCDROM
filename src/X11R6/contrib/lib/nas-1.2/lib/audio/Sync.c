/*
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
 * $NCDId: @(#)Sync.c,v 1.6 1993/01/26 05:13:49 greg Exp $
 */

/* Portions derived from */
/* $XConsortium: XSync.c,v 11.15 91/01/06 11:48:24 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	 */

/*
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 */

#include "Alibint.h"

/* Synchronize with errors and events, optionally discarding pending events */

void 
AuSync(aud, discard)
register AuServer *aud;
AuBool          discard;
{
    auGetCloseDownModeReply rep;
    register auReq *req;

    _AuLockServer(aud);

    _AuGetEmptyReq(GetCloseDownMode, req, aud);
    (void) _AuReply(aud, (auReply *) & rep, 0, auTrue, (AuStatus *) NULL);

    if (discard && aud->head)
    {
	((_AuQEvent *) aud->tail)->next = aud->qfree;
	aud->qfree = (_AuQEvent *) aud->head;
	aud->head = aud->tail = NULL;
	aud->qlen = 0;
    }

    _AuUnlockServer(aud);
}
