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
 * $NCDId: @(#)GetElState.c,v 1.1 1993/05/06 23:21:50 greg Exp $
 */

#include "Alibint.h"

AuElementState *
AuGetElementStates(aud, num_states, states, ret_status)
AuServer       *aud;
int            *num_states;
AuElementState *states;
AuStatus       *ret_status;
{
    register auGetElementStatesReq *req;
    auGetElementStatesReply rep;
    auElementState  s;
    int             i;

    if (ret_status)
	*ret_status = AuSuccess;

    _AuLockServer(aud);
    _AuGetReq(GetElementStates, req, aud);

    req->numStates = *num_states;
    req->length += (*num_states * SIZEOF(auElementState)) >> 2;

    for (i = 0; i < *num_states; i++, states++)
    {
#undef xfer
#define xfer(x) s.x = states->x
	xfer(flow);
	xfer(element_num);

	_AuData32(aud, &s, SIZEOF(auElementState));
    }

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    *num_states = rep.numStates;
    states = NULL;

    if (rep.numStates)
    {
	AuElementState *st;

	if (!(states = st = (AuElementState *)
	      Aumalloc(sizeof(AuElementState) * rep.numStates)))
	{
	    _AuUnlockServer(aud);
	    _AuSyncHandle(aud);
	    return NULL;
	}

	while (rep.numStates--)
	{
	    _AuReadPad(aud, (char *) &s, SIZEOF(auElementState));
#undef xfer
#define xfer(x) st->x = s.x
	    xfer(flow);
	    xfer(element_num);
	    xfer(state);

	    st++;
	}
    }

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    return states;
}

/* ARGSUSED */
void
AuFreeElementStates(aud, num, states)
AuServer       *aud;
int             num;
AuElementState *states;
{
    Aufree(states);
}
