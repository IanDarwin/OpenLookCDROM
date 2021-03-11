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
 * $NCDId: @(#)bcache.c,v 1.2 1994/03/09 19:24:25 greg Exp $
 */

#include "Alibint.h"

typedef struct _AttrRec
{
    AuBucketAttributes *attr;
    struct _AttrRec *next;
}               AttrRec, *AttrPtr;

typedef struct _ServerRec
{
    AuServer       *aud;
    AttrPtr         cache;
    struct _ServerRec *next;
}               ServerRec, *ServerPtr;

static ServerPtr servers;

static AuBucketAttributes *
copyBucketAttributes(attr)
AuBucketAttributes *attr;
{
    AuBucketAttributes *a;

    if (!(a = (AuBucketAttributes *) Aumalloc(sizeof(AuBucketAttributes))))
	return NULL;

    bcopy(attr, a, sizeof(AuBucketAttributes));

    if (AuBucketDescription(attr)->data)
	if (!(AuBucketDescription(a)->data =
	      (char *) Aumalloc(AuBucketDescription(attr)->len + 1)))
	{
	    Aufree(a);
	    return NULL;
	}
	else
	    bcopy(AuBucketDescription(attr)->data,
		  AuBucketDescription(a)->data,
		  AuBucketDescription(attr)->len + 1);

    return a;
}

void
_AuAddToBucketCache(aud, attr)
AuServer       *aud;
AuBucketAttributes *attr;
{
    AttrPtr         e;
    ServerPtr       s = servers;

    while (s && s->aud != aud)
	s = s->next;

    if (!s)
	if (!(s = (ServerPtr) Aumalloc(sizeof(ServerRec))))
	    return;
	else
	{
	    s->cache = NULL;
	    s->aud = aud;
	    s->next = servers;
	    servers = s;
	}

    e = s->cache;

    while (e && (AuBucketIdentifier(e->attr) != AuBucketIdentifier(attr)))
	e = e->next;

    if (e)
	return;				       /* bucket is already in the
					        * cache */

    if (!(e = (AttrPtr) Aumalloc(sizeof(AttrRec))))
	return;

    if (!(e->attr = copyBucketAttributes(attr)))
    {
	Aufree(e);
	return;
    }

    e->next = s->cache;
    s->cache = e;
}

AuBucketAttributes *
_AuLookupBucketInCache(aud, bucket)
AuServer       *aud;
AuBucketID      bucket;
{
    AttrPtr         e;
    ServerPtr       s = servers;

    while (s && s->aud != aud)
	s = s->next;

    if (!s)
	return NULL;

    e = s->cache;

    while (e && (AuBucketIdentifier(e->attr) != bucket))
	e = e->next;

    if (!e)
	return NULL;

    return copyBucketAttributes(e->attr);
}

void
_AuRemoveFromBucketCache(aud, bucket)
AuServer       *aud;
AuBucketID      bucket;
{
    AttrPtr         e,
                    p = NULL;
    ServerPtr       s = servers;

    while (s && s->aud != aud)
	s = s->next;

    if (!s)
	return;

    e = s->cache;

    while (e && (AuBucketIdentifier(e->attr) != bucket))
    {
	p = e;
	e = e->next;
    }

    if (!e)
	return;

    if (p)
	p->next = e->next;
    else
	s->cache = e->next;

    AuFreeBucketAttributes(aud, 1, e->attr);
    AuFree(e);
}

/* remove all the entries for the server from the cache */
void
_AuFreeBucketCache(aud)
AuServer       *aud;
{
    AttrPtr         e,
                    next;
    ServerPtr       s = servers,
                    p = NULL;

    while (s && s->aud != aud)
    {
	p = s;
	s = s->next;
    }

    if (!s)
	return;

    if (p)
	p->next = s->next;
    else
	servers = s->next;

    e = s->cache;

    while (e)
    {
	next = e->next;
	AuFreeBucketAttributes(aud, 1, e->attr);
	Aufree(e);
	e = next;
    }

    Aufree(s);
}
