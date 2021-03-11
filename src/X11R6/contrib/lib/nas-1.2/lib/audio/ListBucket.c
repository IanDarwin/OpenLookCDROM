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
 * $NCDId: @(#)ListBucket.c,v 1.8 1994/03/09 19:25:59 greg Exp $
 */

#include "Alibint.h"

AuBucketAttributes *
AuListBuckets(aud, mask, attr, num_buckets, ret_status)
AuServer       *aud;
AuMask          mask;
AuBucketAttributes *attr;
int            *num_buckets;
AuStatus       *ret_status;
{
    register auReq *req;
    auListBucketsReply rep;
    auBucketAttributes a;
    AuBucketAttributes *list = NULL,
                   *l,
                    tmp;
    int             stringLen,
                    i;
    AuMask          savemask;

    if (attr)
	savemask = AuBucketValueMask(attr);
    else
	attr = &tmp;

    AuBucketValueMask(attr) = mask;

    if (ret_status)
	*ret_status = AuSuccess;

    _AuLockServer(aud);
    _AuGetEmptyReq(ListBuckets, req, aud);

    stringLen = (AuBucketValueMask(attr) & AuCompCommonDescriptionMask) ?
	AuBucketDescription(attr)->len : 0;

    req->length += (SIZEOF(auBucketAttributes) + PAD4(stringLen)) >> 2;

    _xferBucketAttributes(attr, a);

    _AuData(aud, (char *) &a, SIZEOF(auBucketAttributes));

    if (stringLen)
	_AuData(aud, AuBucketDescription(attr)->data, stringLen);

    AuBucketValueMask(attr) = savemask;

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    *num_buckets = rep.num_buckets;

    if (rep.num_buckets)
    {
	if (!(list = l = (AuBucketAttributes *)
	      Aucalloc(1, sizeof(AuBucketAttributes) * rep.num_buckets)))
	{
	    _AuUnlockServer(aud);
	    _AuSyncHandle(aud);
	    return NULL;
	}

	while (rep.num_buckets--)
	{
	    _AuReadPad(aud, (char *) &a, SIZEOF(auBucketAttributes));

	    _xferBucketAttributes(&a, *l);

	    if (AuBucketValueMask(l) & AuCompCommonDescriptionMask)
	    {
		if (!(AuBucketDescription(l)->data = (char *)
		      Aumalloc(AuBucketDescription(l)->len + 1)))
		{
		    AuFreeBucketAttributes(aud, *num_buckets, list);
		    _AuUnlockServer(aud);
		    _AuSyncHandle(aud);
		    return NULL;
		}

		if (AuBucketDescription(l)->len)
		    _AuReadPad(aud, AuBucketDescription(l)->data,
			       AuBucketDescription(l)->len);

		AuBucketDescription(l)->data[AuBucketDescription(l)->len] = 0;
	    }

	    l++;
	}
    }

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    for (i = 0; i < *num_buckets; i++)
	_AuAddToBucketCache(aud, &list[i]);

    return list;
}
