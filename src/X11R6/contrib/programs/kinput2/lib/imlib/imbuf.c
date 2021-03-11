#ifndef lint
static char *rcsid = "$Id: imbuf.c,v 1.7 1994/05/30 10:26:28 ishisone Exp $";
#endif
/*
 * Copyright (c) 1994  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#include <X11/Intrinsic.h>
#include <X11/Xfuncs.h>
#include "imbuf.h"

static void
allocIMBuf(ibp, len)
IMBuffer *ibp;
int len;
{
    int newsize;

    if (ibp->size >= len) return;
    newsize = ibp->size * 2;
    if (newsize < len) newsize = len;
    if (ibp->buf == ibp->internal) {
	ibp->buf = XtMalloc(newsize);
	bcopy(ibp->internal, ibp->buf, ibp->size);
    } else {
	ibp->buf = XtRealloc(ibp->buf, newsize);
    }
    ibp->size = newsize;
}

void
IMBufInit(ibp)
IMBuffer *ibp;
{
    ibp->buf = ibp->internal;
    IMBufClear(ibp);
}

void
IMBufClear(ibp)
IMBuffer *ibp;
{
    if (ibp->buf != NULL && ibp->buf != ibp->internal) XtFree(ibp->buf);
    ibp->buf = ibp->internal;
    ibp->size = sizeof(ibp->internal);
    ibp->start = ibp->end = 0;
}

void
IMBufAdd(ibp, data, len)
IMBuffer *ibp;
char *data;
int len;
{
    allocIMBuf(ibp, ibp->end + len);
    (void)bcopy(data, ibp->buf + ibp->end, len);
    ibp->end += len;
}

void
IMBufOverwrite(ibp, offset, data, len)
IMBuffer *ibp;
int offset;
char *data;
int len;
{
    int dend;

    dend = ibp->start + offset + len;
    allocIMBuf(ibp, dend);
    (void)bcopy(data, ibp->buf + ibp->start + offset, len);
    if (ibp->end < dend) ibp->end = dend;
}

char *
IMBufAlloc(ibp, len)
IMBuffer *ibp;
int len;
{
    char *p;

    allocIMBuf(ibp, ibp->end + len);
    p = ibp->buf + ibp->end;
    ibp->end += len;
    return p;
}

void
IMBufDiscard(ibp, len)
IMBuffer *ibp;
int len;
{
    if (len > 0) {
	/* discard top of the data */
	ibp->start += len;
    } else {
	/* discard end of the data */
	ibp->end += len;
    }
    if (ibp->start >= ibp->end) IMBufClear(ibp);
}

void
IMBufDiscardNUL(ibp)
IMBuffer *ibp;
{
    while (ibp->start < ibp->end) {
	if ((ibp->buf)[ibp->start] != 0) break;
	ibp->start++;
    }
    if (ibp->start >= ibp->end) IMBufClear(ibp);
}

void
IMBufCompact(ibp)
IMBuffer *ibp;
{

    if (ibp->buf != ibp->internal) {
	int length = IMBUFLEN(ibp);

	if (length <= sizeof(ibp->internal)) {
	    bcopy(ibp->buf + ibp->start, ibp->internal, length);
	    XtFree(ibp->buf);
	    ibp->buf = ibp->internal;
	    ibp->size = sizeof(ibp->internal);
	} else {
	    bcopy(ibp->buf + ibp->start, ibp->buf, length);
	}
	ibp->start = 0;
	ibp->end = length;
    } else if (ibp->start != 0) {
	int length = IMBUFLEN(ibp);

	bcopy(ibp->buf + ibp->start, ibp->buf, length);
	ibp->start = 0;
	ibp->end = length;
    }
}
