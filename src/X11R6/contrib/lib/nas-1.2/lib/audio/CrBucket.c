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
 * $NCDId: @(#)CrBucket.c,v 1.3 1993/08/13 20:51:21 greg Exp $
 */

#include "Alibint.h"

AuBucketID
AuCreateBucket(aud, format, num_tracks, access, sample_rate, num_samples,
	       description, ret_status)
AuServer       *aud;
AuUint32   format,
                num_tracks,
                access,
                sample_rate,
                num_samples;
AuString       *description;
AuStatus       *ret_status;
{
    register auResourceReq *req;
    auBucketAttributes b;
    AuBucketID      bucket = AuAllocID(aud);

    if (ret_status)
	*ret_status = AuSuccess;

    b.common.value_mask = AuCompBucketAllMasks &
	~(AuCompCommonKindMask | AuCompCommonUseMask);
    b.common.id = bucket;
    b.common.format = format;
    b.common.num_tracks = num_tracks;
    b.common.access = access;

    if (description)
    {
	b.common.description.type = description->type;
	b.common.description.len = description->len;
    }
    else
    {
	b.common.description.type = AuStringLatin1;
	b.common.description.len = 0;
    }

    b.bucket.sample_rate = sample_rate;
    b.bucket.num_samples = num_samples;

    _AuLockServer(aud);
    _AuGetResReq(CreateBucket, bucket, req, aud);

    req->length += (SIZEOF(auBucketAttributes) +
		    PAD4(b.common.description.len)) >> 2;

    _AuData(aud, (char *) &b, SIZEOF(auBucketAttributes));

    if (b.common.description.len)
	_AuData(aud, description->data, b.common.description.len);

    if (!_AuIfRoundTrip(aud, ret_status))
	bucket = AuNone;

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    return bucket;
}
