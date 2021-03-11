/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)ScanTEvent.c,v 1.3 1993/01/29 20:47:30 lemke Exp $
 */

#include "Alibint.h"

/* ARGSUSED */
static AuBool
_AuTypedEventPredicate (aud, event, arg)
    register AuServer   *aud;
    register AuEvent    *event;
    register AuPointer	arg;
{
    return (event->type == *(int *) arg);
}



AuBool AuScanForTypedEvent (aud, mode, dequeue, type, event)
    register AuServer   *aud;
    register int	mode;
    register AuBool	dequeue;
    register int	type;
    register AuEvent    *event;
{
    int tmp = type;
    return AuScanEvents (aud, mode, dequeue, _AuTypedEventPredicate,
			 (AuPointer) &tmp, event);
}
