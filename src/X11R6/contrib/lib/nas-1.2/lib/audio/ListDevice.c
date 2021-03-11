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
 * $NCDId: @(#)ListDevice.c,v 1.3 1994/04/07 20:41:22 greg Exp $
 */

#include "Alibint.h"

AuDeviceAttributes *
AuListDevices(aud, mask, attr, num_devices, ret_status)
AuServer       *aud;
AuMask          mask;
AuDeviceAttributes *attr;
int            *num_devices;
AuStatus       *ret_status;
{
    register auReq *req;
    auListDevicesReply rep;
    auDeviceAttributes a;
    AuDeviceAttributes *list = NULL,
                   *l,
                    tmp;
    int             stringLen, childLen;
    AuMask          savemask;

    if (attr)
	savemask = AuDeviceValueMask(attr);
    else
	attr = &tmp;

    AuDeviceValueMask(attr) = mask;

    if (ret_status)
	*ret_status = AuSuccess;

    _AuLockServer(aud);
    _AuGetEmptyReq(ListDevices, req, aud);

    stringLen = (AuDeviceValueMask(attr) & AuCompCommonDescriptionMask) ?
	AuDeviceDescription(attr)->len : 0;
    childLen = (AuDeviceValueMask(attr) & AuCompDeviceChildrenMask) ?
	AuDeviceNumChildren(attr) * sizeof(AuDeviceID) : 0;

    req->length += (SIZEOF(auDeviceAttributes) + PAD4(stringLen) +
		    childLen) >> 2;

    _xferDeviceAttributes(attr, a);

    _AuData(aud, (char *) &a, SIZEOF(auDeviceAttributes));

    if (stringLen)
	_AuData(aud, AuDeviceDescription(attr)->data, stringLen);

    if (childLen)
	_AuData(aud, (char *) AuDeviceChildren(attr), childLen);

    AuDeviceValueMask(attr) = savemask;

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    *num_devices = rep.num_devices;

    if (rep.num_devices)
    {
	if (!(list = l = (AuDeviceAttributes *)
	      Aucalloc(1, rep.num_devices * sizeof(AuDeviceAttributes))))
	{
	    _AuUnlockServer(aud);
	    _AuSyncHandle(aud);
	    return NULL;
	}

	while (rep.num_devices--)
	{
	    _AuReadPad(aud, (char *) &a, SIZEOF(auDeviceAttributes));

	    _xferDeviceAttributes(&a, *l);

	    if ((AuDeviceValueMask(l) & AuCompCommonDescriptionMask) &&
		AuDeviceDescription(l)->len)
	    {
		if (!(AuDeviceDescription(l)->data = (char *)
		      Aumalloc(AuDeviceDescription(l)->len + 1)))
		{
		    AuFreeDeviceAttributes(aud, *num_devices, list);
		    _AuUnlockServer(aud);
		    _AuSyncHandle(aud);
		    return NULL;
		}

		_AuReadPad(aud, AuDeviceDescription(l)->data,
			   AuDeviceDescription(l)->len);

		AuDeviceDescription(l)->data[AuDeviceDescription(l)->len] = 0;
	    }

	    if ((AuDeviceValueMask(l) & AuCompDeviceChildrenMask) &&
		AuDeviceNumChildren(l))
	    {
		if (!(AuDeviceChildren(l) = (AuDeviceID *)
		      Aumalloc(AuDeviceNumChildren(l) * sizeof(AuDeviceID))))
		{
		    AuFreeDeviceAttributes(aud, *num_devices, list);
		    _AuUnlockServer(aud);
		    _AuSyncHandle(aud);
		    return NULL;
		}

		_AuReadPad(aud, (char *) AuDeviceChildren(l),
			   AuDeviceNumChildren(l) * sizeof(AuDeviceID));
	    }

	    l++;
	}
    }

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    return list;
}
