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
 * $NCDId: @(#)ReadEl.c,v 1.3 1993/08/13 20:51:31 greg Exp $
 */

#include "Alibint.h"

AuUint32
AuReadElement(aud, flow, element_num, num_bytes, data, ret_status)
AuServer       *aud;
AuFlowID        flow;
int             element_num;
AuUint32   num_bytes;
AuPointer       data;
AuStatus       *ret_status;
{
    register auReadElementReq *req;
    auReadElementReply rep;

    if (ret_status)
	*ret_status = AuSuccess;

    _AuLockServer(aud);
    _AuGetReq(ReadElement, req, aud);

#undef xfer
#define xfer(x) req->x = x

    xfer(flow);
    xfer(element_num);
    xfer(num_bytes);

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    _AuReadPad(aud, data, rep.num_bytes);
    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    return rep.num_bytes;
}
