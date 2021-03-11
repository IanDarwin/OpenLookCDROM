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
 * $NCDId: @(#)WriteEl.c,v 1.5 1994/04/07 20:34:11 greg Exp $
 */

#include "Alibint.h"

void
_AuWriteElement(aud, flow, element_num, num_bytes, data, state, ret_status)
AuServer       *aud;
AuFlowID        flow;
int             element_num,
                state;
AuUint32   num_bytes;
AuPointer       data;
AuStatus       *ret_status;
{
    register auWriteElementReq *req;

    _AuLockServer(aud);
    _AuGetReq(WriteElement, req, aud);

#undef xfer
#define xfer(x) req->x = x

    xfer(flow);
    xfer(element_num);
    xfer(num_bytes);
    xfer(state);

    req->length += PAD4(num_bytes) >> 2;

    _AuData(aud, data, num_bytes);

    (void) _AuIfRoundTrip(aud, ret_status);
    _AuUnlockServer(aud);
    _AuSyncHandle(aud);
}

void
AuWriteElement(aud, flow, element_num, num_bytes, data, end_of_data, ret_status)
AuServer       *aud;
AuFlowID        flow;
int             element_num;
AuUint32   num_bytes;
AuPointer       data;
AuBool          end_of_data;
AuStatus       *ret_status;
{
    AuUint32   maxBytes,
                    bytes,
                    finalState;
    AuStatus        status = AuSuccess,
                   *pstatus;

    if (ret_status)
    {
	*ret_status = AuSuccess;
	pstatus = ret_status;
    }
    else
	pstatus = &status;

    finalState = end_of_data ? AuTransferStateEnd : AuTransferStateReady;
    maxBytes = (aud->max_request_size) - SIZEOF(auWriteElementReq);

    do
    {
	bytes = num_bytes > maxBytes ? maxBytes : num_bytes;
	num_bytes -= bytes;
	_AuWriteElement(aud, flow, element_num, bytes, data,
			num_bytes ? AuTransferStatePending : finalState,
			ret_status);
	data = (AuPointer) ((unsigned char *) data + bytes);
    } while (*pstatus == AuSuccess && num_bytes);
}
