/* $Id: imxport.h,v 1.3 1994/05/13 08:34:17 ishisone Exp $ */
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

#ifndef _imxport_h
#define _imxport_h

#include "imprtype.h"

/*
 * Supported tranport definitions
 */
#ifndef NO_UNIX_TRANSPORT
#define IM_UNIX_TRANSPORT
#endif

#ifndef NO_TCP_TRANSPORT
#define IM_TCP_TRANSPORT
#endif

#ifndef NO_X_TRANSPORT
#define IM_X_TRANSPORT
#endif


/*
 * IMTransport -- transport information
 */

typedef struct _im_connection_ _tmp_IMConnection;
typedef struct {
    int (*flush) _Pt_((_tmp_IMConnection *conn));
    void (*shutdown) _Pt_((_tmp_IMConnection *conn));
} IMTransportOps;

typedef union {
    struct {
	int fd;			/* socket file descriptor */
	XtInputId id;		/* input ID */
    } sock;
    struct {
	Window server;		/* server communication window */
	Window client;		/* client window */
    } x;
} IMTransportPriv;

typedef struct {
    IMTransportOps *ops;
    IMTransportPriv priv;
} IMTransport;

#endif /* _imxport_h */
