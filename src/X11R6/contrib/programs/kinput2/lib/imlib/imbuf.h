/* $Id: imbuf.h,v 1.4 1994/05/12 09:01:05 ishisone Exp $ */
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

#ifndef _imbuf_h
#define _imbuf_h

#include "imprtype.h"

/*
 * IMBuffer -- variable length buffer
 */

#define IMBUF_INT_SIZE	256
typedef struct {
    char *buf;			/* pointer to the buffer */
    int size;			/* size of the buffer */
    int start;			/* start offset of the data */
    int end;			/* end offset of the data */
    char internal[IMBUF_INT_SIZE]; /* internal data buffer */
} IMBuffer;

#define IMBUFLEN(ibp)	((ibp)->end - (ibp)->start)
#define IMBUFDATA(ibp)	((ibp)->buf + (ibp)->start)

extern void IMBufInit _Pt_((IMBuffer *ibp));
extern void IMBufClear _Pt_((IMBuffer *ibp));
extern void IMBufAdd _Pt_((IMBuffer *ibp, char *data, int len));
extern void IMBufOverwrite _Pt_((IMBuffer *ibp, int offset,
				 char *data, int len));
extern char *IMBufAlloc _Pt_((IMBuffer *ibp, int len));
extern void IMBufDiscard _Pt_((IMBuffer *ibp, int len));
extern void IMBufDiscardNUL _Pt_((IMBuffer *ibp));
extern void IMBufCompact _Pt_((IMBuffer *ibp));

#endif /* _imbuf_h */
