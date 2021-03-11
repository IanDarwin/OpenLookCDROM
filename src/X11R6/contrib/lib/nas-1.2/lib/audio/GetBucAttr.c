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
 * $NCDId: @(#)GetBucAttr.c,v 1.8 1994/03/09 19:25:30 greg Exp $
 */

#include "Alibint.h"

AuBucketAttributes *
AuGetBucketAttributes(aud, bucket, ret_status)
AuServer       *aud;
AuBucketID      bucket;
AuStatus       *ret_status;
{
    register auResourceReq *req;
    auGetBucketAttributesReply rep;
    auBucketAttributes a;
    AuBucketAttributes *attr;

    if (ret_status)
	*ret_status = AuSuccess;

    if (attr = _AuLookupBucketInCache(aud, bucket))
	return attr;

    _AuLockServer(aud);
    _AuGetResReq(GetBucketAttributes, bucket, req, aud);

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    _AuReadPad(aud, (char *) &a, SIZEOF(auBucketAttributes));

    if (!(attr = (AuBucketAttributes *)
	  Aucalloc(1, sizeof(AuBucketAttributes))))
    {
	_AuUnlockServer(aud);
	_AuSyncHandle(aud);
	return NULL;
    }

    _xferBucketAttributes(&a, *attr);

    if ((AuBucketValueMask(attr) & AuCompCommonDescriptionMask) &&
	AuBucketDescription(attr)->len)
    {
	if (!(AuBucketDescription(attr)->data = (char *)
	      Aumalloc(AuBucketDescription(attr)->len + 1)))
	{
	    AuFreeBucketAttributes(aud, 1, attr);
	    _AuUnlockServer(aud);
	    _AuSyncHandle(aud);
	    return NULL;
	}

	_AuReadPad(aud, AuBucketDescription(attr)->data,
		   AuBucketDescription(attr)->len);

	AuBucketDescription(attr)->data[AuBucketDescription(attr)->len] = 0;
    }

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    _AuAddToBucketCache(aud, attr);
    return attr;
}

/* ARGSUSED */
void
AuFreeBucketAttributes(aud, num, attr)
AuServer       *aud;
int             num;
AuBucketAttributes *attr;
{
    AuBucketAttributes *p = attr;

    if (!num)
	return;

    while (num--)
    {
	if (AuBucketDescription(p)->data)
	    Aufree(AuBucketDescription(p)->data);

	p++;
    }

    Aufree(attr);
}
