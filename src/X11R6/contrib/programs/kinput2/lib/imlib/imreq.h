/* $Id: imreq.h,v 1.1 1994/05/16 02:45:36 ishisone Exp $ */
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

#ifndef _imreq_h
#define _imreq_h

typedef struct _im_request_ {
    char *name;			/* name of request. 
				 * for extension requests, this is the name of
				 * the extension.
				 */
    int major;			/* major opcode */
    int minor;			/* minor opcode */
    void (*proc)();		/* request handler proc.
				 * NULL means clients don't send this
				 * request to servers
				 * (i.e. server->client request).
				 */
    IMExtensionMask mask;	/* required extension mask */
    struct _im_request_ *next;	/* for dispatch table */
} IMRequest;

extern IMRequest *IMMainDispatchTable[];

#endif /* _imreq_h */
