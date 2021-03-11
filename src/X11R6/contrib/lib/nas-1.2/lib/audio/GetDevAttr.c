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
 * $NCDId: @(#)GetDevAttr.c,v 1.6 1994/04/07 20:41:53 greg Exp $
 */

#include "Alibint.h"

AuDeviceAttributes *
AuGetDeviceAttributes(aud, device, ret_status)
AuServer       *aud;
AuDeviceID      device;
AuStatus       *ret_status;
{
    register auResourceReq *req;
    auGetDeviceAttributesReply rep;
    auDeviceAttributes a;
    AuDeviceAttributes *attr;

    if (ret_status)
	*ret_status = AuSuccess;

    _AuLockServer(aud);
    _AuGetResReq(GetDeviceAttributes, device, req, aud);

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    _AuReadPad(aud, (char *) &a, SIZEOF(auDeviceAttributes));

    if (!(attr = (AuDeviceAttributes *)
	  Aucalloc(1, sizeof(AuDeviceAttributes))))
    {
	_AuUnlockServer(aud);
	_AuSyncHandle(aud);
	return NULL;
    }

    _xferDeviceAttributes(&a, *attr);

    if ((AuDeviceValueMask(attr) & AuCompCommonDescriptionMask) &&
	AuDeviceDescription(attr)->len)
    {
	if (!(AuDeviceDescription(attr)->data = (char *)
	      Aumalloc(AuDeviceDescription(attr)->len + 1)))
	{
	    AuFreeDeviceAttributes(aud, 1, attr);
	    _AuUnlockServer(aud);
	    _AuSyncHandle(aud);
	    return NULL;
	}

	_AuReadPad(aud, AuDeviceDescription(attr)->data,
		   AuDeviceDescription(attr)->len);

	AuDeviceDescription(attr)->data[AuDeviceDescription(attr)->len] = 0;
    }

    if ((AuDeviceValueMask(attr) & AuCompDeviceChildrenMask) &&
	AuDeviceNumChildren(attr))
    {
	if (!(AuDeviceChildren(attr) = (AuDeviceID *)
	      Aumalloc(AuDeviceNumChildren(attr) * sizeof(AuDeviceID))))
	{
	    AuFreeDeviceAttributes(aud, 1, attr);
	    _AuUnlockServer(aud);
	    _AuSyncHandle(aud);
	    return NULL;
	}

	_AuReadPad(aud, (char *) AuDeviceChildren(attr),
		   AuDeviceNumChildren(attr) * sizeof(AuDeviceID));
    }

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    return attr;
}

/* ARGSUSED */
void
AuFreeDeviceAttributes(aud, num, attr)
AuServer       *aud;
int             num;
AuDeviceAttributes *attr;
{
    AuDeviceAttributes *p = attr;

    while (num--)
    {
	if (AuDeviceDescription(p)->data)
	    Aufree(AuDeviceDescription(p)->data);

	if (AuDeviceChildren(p))
	    Aufree(AuDeviceChildren(p));

	p++;
    }

    Aufree(attr);
}
