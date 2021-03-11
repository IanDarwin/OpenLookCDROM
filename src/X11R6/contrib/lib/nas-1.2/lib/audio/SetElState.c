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
 * $NCDId: @(#)SetElState.c,v 1.2 1993/01/26 05:13:45 greg Exp $
 */

#include "Alibint.h"

void
AuSetElementStates(aud, num_states, states, ret_status)
AuServer *aud;
int num_states;
AuElementState *states;
AuStatus       *ret_status;
{
    register auSetElementStatesReq *req;
    auElementState s;
    int i;

    if (ret_status)
	*ret_status = AuSuccess;

    _AuLockServer(aud);
    _AuGetReq(SetElementStates, req, aud);

    req->numStates = num_states;
    req->length += (num_states * SIZEOF(auElementState)) >> 2;

    for (i = 0; i < num_states; i++, states++)
    {
#undef xfer
#define xfer(x) s.x = states->x
	xfer(flow);
	xfer(element_num);
	xfer(state);

	_AuData32(aud, &s, SIZEOF(auElementState));
    }

    (void) _AuIfRoundTrip(aud, ret_status);
    _AuUnlockServer(aud);
    _AuSyncHandle(aud);
}

