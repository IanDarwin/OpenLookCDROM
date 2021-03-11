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
 * $NCDId: @(#)SetDevAttr.c,v 1.2 1993/01/29 20:47:36 lemke Exp $
 */

#include "Alibint.h"

void
AuSetDeviceAttributes(aud, device, mask, attr, ret_status)
AuServer       *aud;
AuDeviceID      device;
AuMask		mask;
AuDeviceAttributes *attr;
AuStatus       *ret_status;
{
    register auResourceReq *req;
    auDeviceAttributes a;
    int             stringLen;
    AuMask	    savemask = AuDeviceValueMask(attr);

    AuDeviceValueMask(attr) = mask;
    if (ret_status)
	*ret_status = AuSuccess;

    _AuLockServer(aud);
    _AuGetResReq(SetDeviceAttributes, device, req, aud);

    stringLen = (AuDeviceValueMask(attr) & AuCompCommonDescriptionMask) ?
	AuDeviceDescription(attr)->len : 0;

    req->length += (SIZEOF(auDeviceAttributes) + PAD4(stringLen)) >> 2;

    _xferDeviceAttributes(attr, a);

    _AuData(aud, (char *) &a, SIZEOF(auDeviceAttributes));

    if (stringLen)
	_AuData(aud, AuDeviceDescription(attr)->data, stringLen);

    AuDeviceValueMask(attr) = savemask;
    (void) _AuIfRoundTrip(aud, ret_status);
    _AuUnlockServer(aud);
    _AuSyncHandle(aud);
}
